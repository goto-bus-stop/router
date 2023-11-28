//! Authorization plugin

// To remove once the commented code is out
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(clippy::needless_borrow)]
#![allow(clippy::extra_unused_lifetimes)]

use std::collections::HashMap;
use std::error::Error;
use std::ops::ControlFlow;
use std::ops::Deref;
use std::sync::Arc;

use apollo_compiler::schema::Directive;
use apollo_compiler::schema::ExtendedType;
use apollo_compiler::schema::Value;
use apollo_compiler::Node;
use futures::Future;
use futures::StreamExt;
use http::uri::*;
use http::Uri;
use hyper_rustls::ConfigBuilderExt;
use regex::Regex;
use tower::Layer;
use tower::ServiceBuilder;
use tower::ServiceExt;

use super::directives::HTTPSourceAPI;
use super::directives::SourceAPI;
use super::directives::SOURCE_API_DIRECTIVE_NAME;
use super::Connector;
use crate::error::ConnectorDirectiveError;
use crate::error::FetchError;
use crate::layers::ServiceBuilderExt;
use crate::services::layers::query_analysis::QueryAnalysisLayer;
use crate::services::new_service::ServiceFactory;
use crate::services::subgraph;
use crate::services::trust_dns_connector::new_async_http_connector;
use crate::services::MakeSubgraphService;
use crate::services::SupergraphCreator;
use crate::services::SupergraphRequest;
use crate::spec::Query;
use crate::spec::Schema;
use crate::spec::Selection;
use crate::Configuration;

#[derive(Clone)]
pub(crate) struct SubgraphConnector {
    creator: SupergraphCreator,
    query_analysis_layer: QueryAnalysisLayer,
}

impl SubgraphConnector {
    pub(crate) fn for_schema(
        schema: Arc<Schema>,
        configuration: Arc<Configuration>,
        creator: SupergraphCreator,
    ) -> Result<Self, ConnectorDirectiveError> {
        let query_analysis_layer = QueryAnalysisLayer::new(schema.clone(), configuration);
        Ok(Self {
            creator,
            query_analysis_layer,
        })
    }
}

use std::task::Poll;

use futures::future::BoxFuture;
use tower::BoxError;
use tower::Service;

use crate::services::SubgraphRequest;
use crate::services::SubgraphResponse;

/// OUTER SUBGRAPH SERVICE

impl tower::Service<SubgraphRequest> for SubgraphConnector {
    type Response = SubgraphResponse;
    type Error = BoxError;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, _cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, request: SubgraphRequest) -> Self::Future {
        let supergraph_request = SupergraphRequest {
            supergraph_request: request.subgraph_request,
            context: request.context,
        };

        let query_analysis_layer = self.query_analysis_layer.clone();

        let service = self.creator.make_connector();

        Box::pin(async move {
            let res = match query_analysis_layer
                .supergraph_request(supergraph_request)
                .await
            {
                Ok(req) => service.oneshot(req).await?,
                Err(res) => res,
            };

            let (parts, mut body) = res.response.into_parts();

            // todo: multipart support in connectors ? :D
            let response = http::Response::from_parts(
                parts,
                body.next().await.ok_or("connector: empty response body")?,
            );

            Ok(SubgraphResponse {
                response,
                context: res.context,
            })
        })
    }
}

/// INNER (CONNECTOR) SUBGRAPH SERVICE

#[derive(Clone)]
pub(crate) struct HTTPConnector {
    connector: Connector,
}

impl HTTPConnector {
    pub(crate) fn new(connector: Connector) -> Self {
        Self { connector }
    }
}

impl<S> Layer<S> for HTTPConnector
where
    S: Clone,
{
    type Service = HTTPConnectorService<S>;

    fn layer(&self, inner: S) -> Self::Service {
        HTTPConnectorService {
            inner,
            connector: self.connector.clone(),
        }
    }
}

#[derive(Clone)]
pub(crate) struct HTTPConnectorService<S> {
    inner: S,
    connector: Connector,
}

impl<S> tower::Service<SubgraphRequest> for HTTPConnectorService<S>
where
    S: Service<SubgraphRequest>,
    S::Future: Future<Output = Result<SubgraphResponse, BoxError>> + Send + 'static,
{
    type Response = SubgraphResponse;
    type Error = BoxError;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner
            .poll_ready(cx)
            .map_err(|_| BoxError::from("http service is not ready"))
    }

    fn call(&mut self, request: SubgraphRequest) -> Self::Future {
        dbg!(&request.subgraph_request, &request.subgraph_name);
        let connector = self.connector.clone();
        dbg!(&connector);

        // TODO: this is where actual connectors will be wired up!
        Box::pin(async move {
            // left as an exercise for the reader (@geal :p)
            let (context, request_to_make) = connector.create_request(request)?;

            // TODO: in the hot path ? REALLY ?! :D
            let mut http_connector = new_async_http_connector()?;
            http_connector.set_nodelay(true);
            http_connector.set_keepalive(Some(std::time::Duration::from_secs(60)));
            http_connector.enforce_http(false);

            let tls_config = rustls::ClientConfig::builder()
                .with_safe_defaults()
                .with_native_roots()
                .with_no_client_auth();

            let http_connector = hyper_rustls::HttpsConnectorBuilder::new()
                .with_tls_config(tls_config)
                .https_or_http()
                .enable_http1()
                .enable_http2()
                .wrap_connector(http_connector);

            let response = hyper::Client::builder()
                .build(http_connector)
                // TODO: tls, client builder etc.
                .call(request_to_make)
                .await
                .map_err(|e| format!("connector http call failed {}", e))?;

            // left as an exercise for the reader (@geal :p)
            let subgraph_response = connector.map_http_response(response, context).await?;
            dbg!(&subgraph_response);

            Ok(subgraph_response)

            // TODO: consider removing inner,
            // unless we have a nice subgraph HTTP service sometimes
            //
            // let fut = self.inner.call(request);
            // fut.await.map_err(BoxError::from)
        })
    }
}

#[cfg(test)]
mod tests {
    use std::net::{SocketAddr, TcpListener};
    use std::sync::Arc;

    use super::*;
    use crate::{router_factory::YamlRouterFactory, services::supergraph, TestHarness};

    const SCHEMA: &str = include_str!("../../../../examples/connectors/starstuff.graphql");

    #[tokio::test]
    async fn nullability_formatting() {
        /*let subgraphs = MockedSubgraphs([
        ("user", MockSubgraph::builder().with_json(
                serde_json::json!{{"query":"{currentUser{activeOrganization{__typename id}}}"}},
                serde_json::json!{{"data": {"currentUser": { "activeOrganization": null }}}}
            ).build()),
        ("orga", MockSubgraph::default())
        ].into_iter().collect());*/
        let listener = TcpListener::bind(SocketAddr::from(([127, 0, 0, 1], 0))).unwrap();
        let address = listener.local_addr().unwrap();

        let schema = SCHEMA.replace(
            "https://ipinfo.io/",
            &format!("http://127.0.0.1:{}/", address.port()),
        );

        // we cannot use Testharness because the subgraph connectors are actually extracted in YamlRouterFactory
        let mut factory = YamlRouterFactory;
        use crate::router_factory::RouterSuperServiceFactory;
        let router_creator = factory
            .create(
                Arc::new(
                    serde_json::from_value(serde_json::json!({
                        "include_subgraph_errors": { "all": true }
                    }))
                    .unwrap(),
                ),
                schema,
                None,
                None,
            )
            .await
            .unwrap();
        let service = router_creator.create();

        let request = supergraph::Request::fake_builder()
            .query("query { serverNetworkInfo { ip city country } }")
            // Request building here
            .build()
            .unwrap()
            .try_into()
            .unwrap();
        let response = service
            .oneshot(request)
            .await
            .unwrap()
            .next_response()
            .await
            .unwrap()
            .unwrap();
        let response: serde_json::Value = serde_json::from_slice(&response).unwrap();

        insta::assert_json_snapshot!(response);
    }
}
