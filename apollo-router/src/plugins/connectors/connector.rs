use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use apollo_compiler::ast::Selection as GraphQLSelection;
use apollo_compiler::schema::Name;
use apollo_compiler::validation::Valid;
use apollo_compiler::Schema;
use tower::BoxError;

use super::directives::SourceAPI;
use super::directives::SourceField;
use super::directives::SourceType;
use super::http_json_transport::HttpJsonTransport;
use super::join_spec_helpers::Key;
use super::request_response::handle_responses;
use super::request_response::make_requests;
use super::request_response::ResponseParams;
use crate::services::SubgraphRequest;
use crate::services::SubgraphResponse;
use crate::Context;

/// A connector wraps the API and type/field connector metadata and has
/// a unique name used to construct a "subgraph" in the inner supergraph
#[derive(Clone, Debug)]
pub(crate) struct Connector {
    /// Internal name used to construct "subgraphs" in the inner supergraph
    pub(super) name: String,
    pub(crate) origin_subgraph: String,
    pub(super) kind: ConnectorKind,
    pub(super) transport: ConnectorTransport,
    pub(super) output_selection: Vec<GraphQLSelection>,
    pub(super) input_selection: Vec<GraphQLSelection>,
}

#[derive(Clone, Debug)]
pub(super) enum ConnectorKind {
    RootField {
        parent_type_name: Name,
        field_name: Name,
        output_type_name: Name,
    },
    Entity {
        type_name: Name,
        key: Key,
    },
    EntityField {
        type_name: Name,
        field_name: Name,
        output_type_name: Name,
        key: Key,
    },
}

#[derive(Clone, Debug)]
pub(super) enum ConnectorTransport {
    HttpJson(HttpJsonTransport),
}

/// The list of the subgraph names that should use the inner query planner
/// instead of making a normal subgraph request.
pub(crate) fn connector_subgraph_names(connectors: &HashMap<String, Connector>) -> HashSet<String> {
    connectors
        .values()
        .map(|c| c.origin_subgraph.clone())
        .collect()
}

impl Connector {
    pub(super) fn new_from_source_type(
        name: String,
        api: SourceAPI,
        directive: SourceType,
    ) -> Result<Self, BoxError> {
        let (input_selection, key_string, transport) = if let Some(ref http) = directive.http {
            let (input_selection, key_string) =
                HttpJsonTransport::input_selection_from_http_source(http);

            let transport = ConnectorTransport::HttpJson(HttpJsonTransport::from_source_type(
                &api, &directive,
            )?);

            (input_selection, key_string, transport)
        } else {
            return Err("Only HTTP sources are supported".into());
        };

        let output_selection = directive.selection.clone().into();

        let kind = ConnectorKind::Entity {
            type_name: directive.type_name.clone(),
            key: Key::Resolvable(key_string),
        };

        Ok(Connector {
            name,
            origin_subgraph: directive.graph.clone(),
            kind,
            transport,
            output_selection,
            input_selection,
        })
    }

    pub(super) fn new_from_source_field(
        name: String,
        api: SourceAPI,
        directive: SourceField,
    ) -> Result<Self, BoxError> {
        let (input_selection, key_string, transport) = if let Some(ref http) = directive.http {
            let (input_selection, key_string) =
                HttpJsonTransport::input_selection_from_http_source(http);

            let transport = ConnectorTransport::HttpJson(HttpJsonTransport::from_source_field(
                &api, &directive,
            )?);

            (input_selection, key_string, transport)
        } else {
            return Err("Only HTTP sources are supported".into());
        };

        let output_selection = directive.selection.clone().into();

        let kind =
            if directive.parent_type_name == "Query" || directive.parent_type_name == "Mutation" {
                ConnectorKind::RootField {
                    parent_type_name: directive.parent_type_name.clone(),
                    field_name: directive.field_name.clone(),
                    output_type_name: directive.output_type_name.clone(),
                }
            } else {
                ConnectorKind::EntityField {
                    type_name: directive.parent_type_name.clone(),
                    field_name: directive.field_name.clone(),
                    output_type_name: directive.output_type_name.clone(),
                    key: Key::Resolvable(key_string),
                }
            };

        Ok(Connector {
            name,
            origin_subgraph: directive.graph.clone(),
            kind,
            transport,
            output_selection,
            input_selection,
        })
    }

    /// Generate a map of connectors with unique names
    pub(crate) fn from_schema(schema: &Schema) -> Result<HashMap<String, Self>, BoxError> {
        let apis = SourceAPI::from_schema(schema)?;
        let types = SourceType::from_schema(schema)?;
        let fields = SourceField::from_schema(schema)?;

        if apis.is_empty() || (types.is_empty() && fields.is_empty()) {
            return Ok(Default::default());
        }

        let default_api = apis
            .values()
            .find(|api| api.is_default())
            .unwrap_or(apis.values().next().ok_or("No APIs defined")?);

        let mut connectors = HashMap::new();

        for (i, directive) in types.into_iter().enumerate() {
            let connector_name = format!("CONNECTOR_{}_{}", directive.type_name, i).to_uppercase();
            let api = apis.get(&directive.api_name()).unwrap_or(default_api);

            connectors.insert(
                connector_name.clone(),
                Connector::new_from_source_type(connector_name, api.clone(), directive)?,
            );
        }

        for (i, directive) in fields.into_iter().enumerate() {
            let connector_name = format!(
                "CONNECTOR_{}_{}_{}",
                directive.parent_type_name, directive.field_name, i
            )
            .to_uppercase();

            let api = apis.get(&directive.api_name()).unwrap_or(default_api);

            connectors.insert(
                connector_name.clone(),
                Connector::new_from_source_field(connector_name, api.clone(), directive)?,
            );
        }

        Ok(connectors)
    }

    pub(crate) fn create_requests(
        &self,
        subgraph_request: SubgraphRequest,
        schema: Arc<Valid<Schema>>,
    ) -> Result<Vec<(http::Request<hyper::Body>, ResponseParams)>, BoxError> {
        make_requests(subgraph_request, self, schema.clone())
    }

    pub(super) async fn map_http_responses(
        &self,
        responses: Vec<http::Response<hyper::Body>>,
        context: Context,
    ) -> Result<SubgraphResponse, BoxError> {
        handle_responses(context, self, responses).await
    }
}

#[cfg(test)]
mod tests {
    use apollo_compiler::name;

    use super::*;
    use crate::plugins::connectors::directives::HTTPSource;
    use crate::plugins::connectors::directives::HTTPSourceAPI;
    use crate::plugins::connectors::selection_parser::Selection as JSONSelection;
    use crate::plugins::connectors::url_path_parser::URLPathTemplate;
    use crate::services::subgraph;

    #[test]
    fn request() {
        let subgraph_request = subgraph::Request::fake_builder()
            .subgraph_request(
                http::Request::builder()
                    .body(crate::graphql::Request::builder().query("{field}").build())
                    .unwrap(),
            )
            .build();

        let api = SourceAPI {
            graph: "B".to_string(),
            name: "API".to_string(),
            http: Some(HTTPSourceAPI {
                base_url: "http://localhost/api".to_string(),
                default: true,
                headers: vec![],
            }),
        };

        let directive = SourceField {
            graph: "B".to_string(),
            parent_type_name: name!(Query),
            field_name: name!(field),
            output_type_name: name!(String),
            api: "API".to_string(),
            http: Some(HTTPSource {
                method: http::Method::GET,
                path_template: URLPathTemplate::parse("/path").unwrap(),
                body: None,
                headers: vec![],
            }),
            selection: JSONSelection::parse(".data").unwrap().1,
        };

        let connector =
            Connector::new_from_source_field("CONNECTOR_QUERY_FIELDB".to_string(), api, directive)
                .unwrap();

        let requests_and_params = connector
            .create_requests(
                subgraph_request,
                Arc::new(
                    Schema::parse_and_validate(
                        "type Query { field: String }".to_string(),
                        "schema.graphql",
                    )
                    .unwrap(),
                ),
            )
            .unwrap();
        insta::assert_debug_snapshot!(requests_and_params);
    }
}
