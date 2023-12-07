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
use hyper::client::HttpConnector;
use hyper_rustls::ConfigBuilderExt;
use hyper_rustls::HttpsConnector;
use regex::Regex;
use tower::Layer;
use tower::ServiceBuilder;
use tower::ServiceExt;

use super::directives::HTTPSourceAPI;
use super::directives::SourceAPI;
use super::outer::map_request;
use super::outer::map_response;
use super::request_response::HackEntityResponseKey;
use super::Connector;
use crate::error::ConnectorDirectiveError;
use crate::error::FetchError;
use crate::layers::ServiceBuilderExt;
use crate::services::layers::query_analysis::QueryAnalysisLayer;
use crate::services::new_service::ServiceFactory;
use crate::services::subgraph;
use crate::services::trust_dns_connector::new_async_http_connector;
use crate::services::trust_dns_connector::AsyncHyperResolver;
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

/// "OUTER" SUBGRAPH -> "INNER" SUPERGRAPH SERVICE

impl tower::Service<SubgraphRequest> for SubgraphConnector {
    type Response = SubgraphResponse;
    type Error = BoxError;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, _cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, request: SubgraphRequest) -> Self::Future {
        let context = request.context.clone();
        let inner = match map_request(request) {
            Ok(inner) => inner,
            Err(e) => return Box::pin(async move { Err(BoxError::from(e)) }),
        };

        let query_analysis_layer = self.query_analysis_layer.clone();
        let service = self.creator.make_connector();

        Box::pin(async move {
            let res = match query_analysis_layer.supergraph_request(inner).await {
                Ok(req) => service.oneshot(req).await?,
                Err(res) => res,
            };

            map_response(res, &context).await.map_err(BoxError::from)
        })
    }
}

/// INNER (CONNECTOR) SUBGRAPH SERVICE

#[derive(Clone)]
pub(crate) struct HTTPConnector {
    schema: Arc<Schema>,
    connector: Connector,
    client: hyper::Client<HttpsConnector<HttpConnector<AsyncHyperResolver>>>,
}

impl HTTPConnector {
    pub(crate) fn new(schema: Arc<Schema>, connector: Connector) -> Result<Self, BoxError> {
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
        //TODO: add decompression
        let client = hyper::Client::builder().build(http_connector);

        Ok(Self {
            schema,
            connector,
            client,
        })
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
            schema: self.schema.clone(),
            connector: self.connector.clone(),
            client: self.client.clone(),
        }
    }
}

#[derive(Clone)]
pub(crate) struct HTTPConnectorService<S> {
    inner: S,
    schema: Arc<Schema>,
    connector: Connector,
    client: hyper::Client<HttpsConnector<HttpConnector<AsyncHyperResolver>>>,
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
        // dbg!(&request.subgraph_request, &request.subgraph_name);
        let schema = self.schema.clone();
        let connector = self.connector.clone();
        let context = request.context.clone();
        // dbg!(&connector);

        let client = self.client.clone();

        Box::pin(async move {
            let requests =
                connector.create_requests(request, Arc::from(schema.definitions.clone()))?;

            let tasks = requests.into_iter().map(|(req, res_params)| async {
                println!("HTTP {} {}", &req.method(), &req.uri());
                let mut res = client.request(req).await?;
                res.extensions_mut().insert(res_params);
                Ok::<_, BoxError>(res)
            });

            let results = futures::future::try_join_all(tasks).await;

            let responses = match results {
                Ok(responses) => responses,
                Err(e) => return Err(BoxError::from(e)),
            };

            let subgraph_response = connector.map_http_responses(responses, context).await?;
            dbg!(&subgraph_response.response.body().data);

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
    use std::convert::Infallible;
    use std::net::SocketAddr;
    use std::net::TcpListener;
    use std::sync::Arc;

    use http::header::CONTENT_TYPE;
    use http::Method;
    use http::StatusCode;
    use hyper::service::make_service_fn;
    use hyper::service::service_fn;
    use hyper::Body;
    use hyper::Server;
    use mime::APPLICATION_JSON;

    use super::*;
    use crate::router_factory::YamlRouterFactory;
    use crate::services::supergraph;
    use crate::TestHarness;

    const SCHEMA: &str = include_str!("../../../../examples/connectors/starstuff.graphql");

    async fn emulate_rest_connector(listener: TcpListener) {
        async fn handle(
            request: http::Request<Body>,
        ) -> Result<http::Response<String>, Infallible> {
            /*
                        type IP
              @join__type(graph: NETWORK, key: "ip")
              @sourceType(
                graph: "network"
                api: "ipinfo"
                http: { GET: "/json" }
                selection: "ip hostname city region country loc org postal timezone readme"
              ) {
              ip: ID!
              hostname: String
              city: String
              region: String
              country: String
              loc: String
              org: String
              postal: String
              timezone: String
              readme: String
            }
                         */

            let res = if request.method() == Method::GET
            /*&& request.uri().path() == "/json"*/
            {
                let value = serde_json::json! {{
                    "ip": "1.2.3.4",
                    "hostname": "hello",
                    "city": "Paris",
                    "region": "Ile de France",
                    "country": "France",
                    "loc": "1",
                    "org": "a",
                    "postal": "75000",
                    "timezone": "CEST",
                    "readme": "readme"
                }};
                Ok(http::Response::builder()
                    .header(CONTENT_TYPE, APPLICATION_JSON.essence_str())
                    .status(StatusCode::OK)
                    .body(serde_json::to_string(&value).expect("always valid"))
                    .unwrap())
            } else {
                Ok(http::Response::builder()
                    .header(CONTENT_TYPE, APPLICATION_JSON.essence_str())
                    .status(StatusCode::BAD_REQUEST)
                    .body(String::new())
                    .unwrap())
            };
            println!("generated service response: {res:?}");
            res
        }

        let make_svc = make_service_fn(|_conn| async { Ok::<_, Infallible>(service_fn(handle)) });
        let server = Server::from_tcp(listener).unwrap().serve(make_svc);
        server.await.unwrap();
    }

    #[tokio::test]
    async fn nullability_formatting() {
        let listener = TcpListener::bind(SocketAddr::from(([127, 0, 0, 1], 0))).unwrap();
        let address = listener.local_addr().unwrap();
        let _spawned_task = tokio::task::spawn(emulate_rest_connector(listener));

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
