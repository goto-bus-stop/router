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
use regex::Regex;
use tower::Layer;
use tower::ServiceBuilder;
use tower::ServiceExt;

use super::directives::HTTPSourceAPI;
use super::directives::SourceAPI;
use super::directives::SOURCE_API_DIRECTIVE_NAME;
use crate::error::ConnectorDirectiveError;
use crate::error::FetchError;
use crate::layers::ServiceBuilderExt;
use crate::services::layers::query_analysis::QueryAnalysisLayer;
use crate::services::new_service::ServiceFactory;
use crate::services::subgraph;
use crate::services::MakeSubgraphService;
use crate::services::SupergraphCreator;
use crate::services::SupergraphRequest;
use crate::spec::Query;
use crate::spec::Schema;
use crate::spec::Selection;
use crate::Configuration;

#[derive(Clone)]
pub(crate) struct SubgraphConnector {
    source_apis: Arc<HashMap<String, SourceAPI>>,
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
            source_apis: Arc::new(SourceAPI::from_schema(&schema.definitions)?),
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

#[derive(Clone)]
pub(crate) struct HTTPConnector {}

impl<S> Layer<S> for HTTPConnector
where
    S: Clone,
{
    type Service = HTTPConnectorService<S>;

    fn layer(&self, inner: S) -> Self::Service {
        HTTPConnectorService { inner }
    }
}

#[derive(Clone)]
pub(crate) struct HTTPConnectorService<S> {
    inner: S,
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

        let fut = self.inner.call(request);
        // TODO: this is where actual connectors will be wired up!
        Box::pin(async move { fut.await.map_err(BoxError::from) })
    }
}

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
