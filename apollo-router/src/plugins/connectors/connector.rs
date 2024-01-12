use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Display;

use apollo_compiler::ast::Selection as GraphQLSelection;
use apollo_compiler::schema::Name;
use apollo_compiler::Schema;
use tower::BoxError;

use super::directives::graph_enum_map;
use super::directives::KeyTypeMap;
use super::directives::SourceAPI;
use super::directives::SourceField;
use super::directives::SourceType;
use super::http_json_transport::HttpJsonTransport;

/// A connector wraps the API and type/field connector metadata and has
/// a unique name used to construct a "subgraph" in the inner supergraph
#[derive(Clone, Debug)]
pub(crate) struct Connector {
    /// Internal name used to construct "subgraphs" in the inner supergraph
    pub(super) name: String,
    /// The api name, as defined in the `sourceAPI` directive
    api: String,
    pub(crate) origin_subgraph: String,
    pub(super) kind: ConnectorKind,
    pub(super) transport: ConnectorTransport,
    pub(super) output_selection: Vec<GraphQLSelection>,
    pub(super) input_selection: Vec<GraphQLSelection>,
    pub(super) key_type_map: Option<KeyTypeMap>,
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
        key: String,
        is_interface_object: bool,
    },
    EntityField {
        type_name: Name,
        field_name: Name,
        output_type_name: Name,
        key: String,
        on_interface_object: bool,
    },
}

#[derive(Clone, Debug)]
pub(super) enum ConnectorTransport {
    HttpJson(HttpJsonTransport),
}

impl ConnectorTransport {
    pub(super) fn source_api_name(&self) -> Cow<'static, str> {
        match self {
            Self::HttpJson(transport) => transport.source_api_name.clone(),
        }
    }
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
        let (input_selection, key, transport) = if let Some(ref http) = directive.http {
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
            key,
            is_interface_object: directive.is_interface_object,
        };

        Ok(Connector {
            name,
            api: directive.api.clone(),
            origin_subgraph: directive.graph.clone(),
            kind,
            transport,
            output_selection,
            input_selection,
            key_type_map: directive.key_type_map,
        })
    }

    pub(super) fn new_from_source_field(
        name: String,
        api: SourceAPI,
        directive: SourceField,
    ) -> Result<Self, BoxError> {
        let (input_selection, key, transport) = if let Some(ref http) = directive.http {
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
                    key,
                    on_interface_object: directive.on_interface_object,
                }
            };

        Ok(Connector {
            name,
            api: directive.api.clone(),
            origin_subgraph: directive.graph.clone(),
            kind,
            transport,
            output_selection,
            input_selection,
            key_type_map: None,
        })
    }

    /// Generate a map of connectors with unique names
    pub(crate) fn from_schema(schema: &Schema) -> Result<HashMap<String, Self>, BoxError> {
        // NOTE: crate::spec::Schema::parse might be called with an API schema, which doesn't have a join__Graph
        // TODO: we can extract this map once and pass it to the ::from_schema functions instead of generating it for each connector type
        if graph_enum_map(schema).is_none() {
            return Ok(Default::default());
        }

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

    pub(super) fn finder_field_name(&self) -> Option<serde_json_bytes::ByteString> {
        match &self.kind {
            ConnectorKind::RootField { .. } => None,
            ConnectorKind::Entity { type_name, .. } => {
                Some(format!("_{}_finder", type_name).into())
            }
            ConnectorKind::EntityField { type_name, .. } => {
                Some(format!("_{}_finder", type_name).into())
            }
        }
    }
}

impl Display for Connector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ConnectorTransport::HttpJson(tr) = &self.transport;
        write!(
            f,
            "{}.{}: {} {}",
            self.origin_subgraph, self.api, tr.method, tr.path_template
        )
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use apollo_compiler::name;

    use super::*;
    use crate::plugins::connectors::directives::HTTPSource;
    use crate::plugins::connectors::directives::HTTPSourceAPI;
    use crate::plugins::connectors::request_response::make_requests;
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
            on_interface_object: false,
        };

        let connector =
            Connector::new_from_source_field("CONNECTOR_QUERY_FIELDB".to_string(), api, directive)
                .unwrap();

        let schema = Arc::new(
            Schema::parse_and_validate(
                "type Query { field: String }".to_string(),
                "schema.graphql",
            )
            .unwrap(),
        );

        let requests_and_params =
            make_requests(subgraph_request, &connector, schema.clone()).unwrap();
        insta::assert_debug_snapshot!(requests_and_params);
    }
}
