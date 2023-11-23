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
use http::uri::*;
use http::Uri;
use regex::Regex;
use tower::ServiceBuilder;
use tower::ServiceExt;

use super::directives::HTTPSourceAPI;
use super::directives::SourceAPI;
use super::directives::SOURCE_API_DIRECTIVE_NAME;
use crate::error::ConnectorDirectiveError;
use crate::layers::ServiceBuilderExt;
use crate::services::subgraph;
use crate::spec::Query;
use crate::spec::Schema;
use crate::spec::Selection;

#[derive(Clone)]
pub(crate) struct SubgraphConnector {
    source_apis: Arc<HashMap<String, SourceAPI>>,
}

impl SubgraphConnector {
    pub(crate) fn for_schema(schema: Arc<Schema>) -> Result<Self, ConnectorDirectiveError> {
        Ok(Self {
            source_apis: Arc::new(SourceAPI::from_schema(&schema.definitions)?),
        })
    }

    pub(crate) fn subgraph_service(
        &self,
        _subgraph_name: &str,
        service: subgraph::BoxService,
    ) -> subgraph::BoxService {
        service
    }
}
