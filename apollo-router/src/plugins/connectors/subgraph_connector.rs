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
use super::directives::SOURCE_API_ENUM_NAME;
use crate::error::ConnectorDirectiveError;
use crate::layers::ServiceBuilderExt;
use crate::services::subgraph;
// use crate::spec::hir_type_name;
use crate::spec::Query;
use crate::spec::Schema;
use crate::spec::Selection;

pub(crate) const HTTP_RESOURCE_DIRECTIVE_NAME: &str = "http_resource";
pub(crate) const HTTP_LIST_RESOURCE_DIRECTIVE_NAME: &str = "http_list_resource";
pub(crate) const HTTP_FIELD_DIRECTIVE_NAME: &str = "http_field";

#[derive(Clone)]
pub(crate) struct SubgraphConnector {
    source_apis: Arc<HashMap<String, SourceAPI>>,
}

impl SubgraphConnector {
    pub(crate) fn for_schema(schema: Arc<Schema>) -> Result<Self, ConnectorDirectiveError> {
        Ok(Self {
            source_apis: Arc::new(source_apis_from_root_enum(&schema)?),
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

// Given a valid schema with a SOURCE_API enum,
// returns `SourceApi` directive parameters for each of the relevant subgraphs.
fn source_apis_from_root_enum(
    schema: &Schema,
) -> Result<HashMap<String, SourceAPI>, ConnectorDirectiveError> {
    // SOURCE_API is an enum available at the root,
    // it contains variants, that have @source_api metadata attached to them
    Ok(schema
        .definitions
        .get_enum(SOURCE_API_ENUM_NAME)
        .map(|source_api_enum| {
            // for each of the variants, let's get the name, and create a SourceApi item.
            source_api_enum
                .values
                .iter()
                .map(|(node, value)| {
                    // the node contains the name,
                    // let's craft a SourceApi from the directive metadata
                    SourceAPI::from_root_enum(value)
                        .map(|source_api| (node.to_string().to_string(), source_api))
                })
                .collect::<Result<HashMap<_, _>, _>>()
        })
        .transpose()?
        .unwrap_or_default())
}

// Given a valid schema with a @source_api directive applied to the SCHEMA section,
// returns `SourceApi` directive parameters for each of the relevant subgraphs.
fn source_apis_from_schema_directive(
    schema: &Schema,
) -> Result<HashMap<String, SourceAPI>, ConnectorDirectiveError> {
    // `source_api` is an directive that applies to schema
    Ok(schema
        .definitions
        .schema_definition
        .directives
        .iter()
        .filter(|d| d.name == SOURCE_API_DIRECTIVE_NAME)
        .map(|source_api_directive| {
            let connector_name = source_api_directive
                .argument_by_name("name")
                .as_ref()
                .map(|name| {
                    Ok(name
                        .as_str()
                        .ok_or_else(|| {
                            ConnectorDirectiveError::InvalidTypeForAttribute(
                                "String".to_string(),
                                "name".to_string(),
                            )
                        })?
                        .to_string())
                })
                .ok_or_else(|| {
                    ConnectorDirectiveError::MissingAttributeForType(
                        "name".to_string(),
                        SOURCE_API_DIRECTIVE_NAME.to_string(),
                    )
                })??;
            // for each of the applied directives, let's get the name, and create a SourceApi item.
            SourceAPI::from_schema_directive(source_api_directive)
                .map(|source_api| (connector_name, source_api))
        })
        .collect::<Result<HashMap<_, _>, _>>()
        .unwrap_or_default())
}

#[cfg(test)]
mod tests {
    use insta::assert_json_snapshot;
    use insta::with_settings;

    use super::*;
    use crate::Configuration;

    const SCHEMA_DIRECTIVE: &str = include_str!("./test_supergraph_schema_directive.graphql");
    const SCHEMA_ENUM: &str = include_str!("./test_supergraph_root_enum.graphql");

    #[test]
    fn test_schema_directive_has_no_errors() {
        let schema = Schema::parse(
            SCHEMA_DIRECTIVE,
            &Configuration::fake_builder().build().unwrap(),
        )
        .unwrap();

        assert!(!schema.has_errors());
    }
    #[test]
    fn test_source_api_directive() {
        let schema = Schema::parse(
            SCHEMA_DIRECTIVE,
            &Configuration::fake_builder().build().unwrap(),
        )
        .unwrap();

        let source_apis_from_schema = source_apis_from_schema_directive(&schema).unwrap();

        insta::with_settings!({sort_maps => true}, {
            assert_json_snapshot!(source_apis_from_schema);
        });
    }

    #[test]
    fn test_schema_enum_has_no_errors() {
        let schema =
            Schema::parse(SCHEMA_ENUM, &Configuration::fake_builder().build().unwrap()).unwrap();

        assert!(!schema.has_errors());
    }

    #[test]
    fn test_source_api_enum() {
        let schema =
            Schema::parse(SCHEMA_ENUM, &Configuration::fake_builder().build().unwrap()).unwrap();

        let source_apis_from_schema = source_apis_from_root_enum(&schema).unwrap();
        insta::with_settings!({sort_maps => true}, {
            assert_json_snapshot!(source_apis_from_schema);
        });
    }
}
