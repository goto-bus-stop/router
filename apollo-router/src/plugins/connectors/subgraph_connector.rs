//! Authorization plugin

// To remove once the commented code is out
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(clippy::needless_borrow)]
#![allow(clippy::extra_unused_lifetimes)]

use std::collections::HashMap;
use std::ops::ControlFlow;
use std::ops::Deref;
use std::sync::Arc;

use apollo_compiler::schema::Directive;
use apollo_compiler::schema::ExtendedType;
use apollo_compiler::schema::Value;
use apollo_compiler::Node;
use futures::StreamExt;
use http::uri::*;
use http::Uri;
use regex::Regex;
use tower::ServiceBuilder;
use tower::ServiceExt;

use super::directives::HTTPSourceAPI;
use super::directives::SourceAPI;
use super::directives::SOURCE_API_DIRECTIVE_NAME;
use crate::error::ConnectorDirectiveError;
use crate::error::FetchError;
use crate::layers::ServiceBuilderExt;
use crate::services::new_service::ServiceFactory;
use crate::services::subgraph;
use crate::services::MakeSubgraphService;
use crate::services::SupergraphCreator;
use crate::services::SupergraphRequest;
use crate::spec::Query;
use crate::spec::Schema;
use crate::spec::Selection;

#[derive(Clone)]
pub(crate) struct SubgraphConnector {
    source_apis: Arc<HashMap<String, SourceAPI>>,
    creator: SupergraphCreator,
}

impl SubgraphConnector {
    pub(crate) fn for_schema(
        schema: Arc<Schema>,
        creator: SupergraphCreator,
    ) -> Result<Self, ConnectorDirectiveError> {
        Ok(Self {
            source_apis: Arc::new(SourceAPI::from_schema(&schema.definitions)?),
            creator,
        })
    }

    pub(crate) fn subgraph_service(
        &self,
        subgraph_name: &str,
        service: subgraph::BoxService,
    ) -> subgraph::BoxService {
        let subgraph_name = Arc::new(subgraph_name.to_string());
        let s2 = subgraph_name.clone();
        ServiceBuilder::new()
            .map_request(move |req| {
                dbg!(&s2);
                req
            })
            .service(service)
            .boxed()
    }
}

use crate::services::SubgraphRequest;
use crate::services::SubgraphResponse;
use futures::future::BoxFuture;
use std::task::Poll;
use tower::BoxError;
use tower::Service;

impl tower::Service<SubgraphRequest> for SubgraphConnector {
    type Response = SubgraphResponse;
    type Error = BoxError;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, request: SubgraphRequest) -> Self::Future {
        let supergraph_request = SupergraphRequest {
            supergraph_request: request.subgraph_request,
            context: request.context,
        };

        let service = self.creator.make_connector();

        Box::pin(async {
            let res = service.oneshot(supergraph_request).await?;

            let (parts, mut body) = res.response.into_parts();

            // todo: multipart support in connectors ? :D
            let response = http::Response::from_parts(
                parts,
                body.next()
                    .await
                    .ok_or_else(|| "connector: empty response body")?,
            );

            let subgraph_response = SubgraphResponse {
                response,
                context: res.context,
            };

            Ok(subgraph_response)
        })
    }
}
