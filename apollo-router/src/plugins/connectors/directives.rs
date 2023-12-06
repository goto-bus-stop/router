#![allow(dead_code)]

use std::collections::HashMap;

use apollo_compiler::ast::Selection;
use apollo_compiler::schema::Component;
use apollo_compiler::schema::Directive;
use apollo_compiler::schema::ExtendedType;
use apollo_compiler::schema::FieldDefinition;
use apollo_compiler::schema::Value;
use apollo_compiler::Node;
use apollo_compiler::NodeStr;
use apollo_compiler::Schema;
use indexmap::IndexMap;
use serde::Serialize;

use super::selection_parser::Selection as JSONSelection;
use super::url_path_parser::URLPathTemplate;
use crate::error::ConnectorDirectiveError;

const SOURCE_API_DIRECTIVE_NAME: &str = "sourceAPI";
const HTTP_ARGUMENT_NAME: &str = "http";
const SOURCE_TYPE_DIRECTIVE_NAME: &str = "sourceType";
const SOURCE_FIELD_DIRECTIVE_NAME: &str = "sourceField";

const JOIN_SCHEMA_DIRECTIVE_NAME: &str = "join__schema";
const JOIN_TYPE_DIRECTIVE_NAME: &str = "join__type";
const JOIN_FIELD_DIRECTIVE_NAME: &str = "join__field";
const JOIN_GRAPH_ENUM_NAME: &str = "join__Graph";
const JOIN_GRAPH_DIRECTIVE_NAME: &str = "join__graph";

// --- @join__* ----------------------------------------------------------------

fn graph_enum_map(schema: &apollo_compiler::Schema) -> Option<HashMap<String, String>> {
    schema.get_enum(JOIN_GRAPH_ENUM_NAME).map(|e| {
        e.values
            .iter()
            .map(|(name, node)| {
                (
                    name.to_string(),
                    node.directives
                        .iter()
                        .find_map(|d| {
                            if d.name == JOIN_GRAPH_DIRECTIVE_NAME {
                                d.argument_by_name("name")
                                    .as_ref()
                                    .and_then(|name| name.as_str())
                                    .map(|app_name| app_name.to_string())
                            } else {
                                None
                            }
                        })
                        .unwrap_or_default(),
                )
            })
            .collect::<HashMap<_, _>>()
    })
}

#[derive(Debug)]
struct JoinWithDirectives {
    graph: String,
    directives: Vec<DirectiveAsObject>,
}

impl JoinWithDirectives {
    fn from_directive(
        directive: &Node<Directive>,
    ) -> Result<Option<Self>, ConnectorDirectiveError> {
        let directives = if let Some(directives) = directive.argument_by_name("directives") {
            directives
                .as_list()
                .map(|directives| {
                    directives
                        .iter()
                        .map(|directive| directive.try_into())
                        .collect::<Result<Vec<_>, _>>()
                })
                .ok_or_else(|| {
                    ConnectorDirectiveError::InvalidJoinDirective(
                        "Expected directives to be a list".to_string(),
                    )
                })??
        } else {
            return Ok(None);
        };

        let graph = directive
            .argument_by_name("graph")
            .as_ref()
            .and_then(|graph| graph.as_enum())
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidJoinDirective("graph: must be an enum".to_string())
            })?
            .to_string();

        Ok(Some(Self { graph, directives }))
    }
}

#[derive(Debug)]
struct DirectiveAsObject {
    name: String,
    args: HashMap<NodeStr, Node<apollo_compiler::ast::Value>>,
}

impl TryFrom<&Node<apollo_compiler::ast::Value>> for DirectiveAsObject {
    type Error = ConnectorDirectiveError;

    fn try_from(directive: &Node<apollo_compiler::ast::Value>) -> Result<Self, Self::Error> {
        let directive = directive
            .as_object()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidJoinDirective(
                    "Expected directive to be an object".to_string(),
                )
            })?
            .iter()
            .map(|(name, value)| (name.clone(), value.clone()))
            .collect::<HashMap<_, _>>();

        let name = directive
            .get(&NodeStr::new("name"))
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidJoinDirective(
                    "Expected directive to have a name".to_string(),
                )
            })?
            .as_str()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidJoinDirective(
                    "Expected name to be a string".to_string(),
                )
            })?
            .to_string();

        let args = directive
            .get(&NodeStr::new("args"))
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidJoinDirective(
                    "Expected directive to have args".to_string(),
                )
            })?
            .as_object()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidJoinDirective(
                    "Expected args to be an object".to_string(),
                )
            })?
            .iter()
            .map(|(name, value)| (name.clone(), value.clone()))
            .collect::<HashMap<_, _>>();

        Ok(Self { name, args })
    }
}

// --- @sourceAPI --------------------------------------------------------------

#[derive(Clone, Debug, Serialize)]
pub(super) struct SourceAPI {
    pub(crate) graph: String,
    pub(crate) name: String,
    pub(crate) http: Option<HTTPSourceAPI>,
}

impl SourceAPI {
    pub(super) fn from_schema(
        schema: &Schema,
    ) -> Result<HashMap<String, Self>, ConnectorDirectiveError> {
        let graph_names = graph_enum_map(schema).ok_or_else(|| {
            ConnectorDirectiveError::InvalidJoinDirective("Missing join__Graph enum".to_string())
        })?;

        let mut result = HashMap::new();

        let directives = schema
            .schema_definition
            .directives
            .iter()
            .filter(|d| d.name == JOIN_SCHEMA_DIRECTIVE_NAME)
            .filter_map(|join_schema| {
                JoinWithDirectives::from_directive(&join_schema.node).transpose()
            })
            .collect::<Result<Vec<_>, _>>()?;

        let source_apis = directives
            .into_iter()
            .flat_map(|obj| {
                obj.directives
                    .into_iter()
                    .filter(|d| d.name == SOURCE_API_DIRECTIVE_NAME)
                    .map(move |d| (obj.graph.clone(), d.args))
            })
            .collect::<Vec<_>>();

        for (graph, args) in source_apis {
            let graph_name = graph_names.get(&graph).ok_or_else(|| {
                ConnectorDirectiveError::InvalidJoinDirective(
                    format!("Missing graph {} in join__Graph enum", graph).to_string(),
                )
            })?;
            let source_api = Self::from_schema_directive(graph_name.clone(), args)?;
            let name = format!("{}_{}", source_api.graph, source_api.name);
            result.insert(name, source_api);
        }

        Ok(result)
    }

    pub(super) fn from_schema_directive(
        graph: String,
        args: HashMap<NodeStr, Node<apollo_compiler::ast::Value>>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let name = args
            .get(&NodeStr::new("name"))
            .ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    "name".to_string(),
                    SOURCE_API_DIRECTIVE_NAME.to_string(),
                )
            })?
            .as_str()
            .ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    "name".to_string(),
                    SOURCE_API_DIRECTIVE_NAME.to_string(),
                )
            })?
            .to_string();

        let http = args
            .get(&NodeStr::new("http"))
            .map(HTTPSourceAPI::from_directive)
            .transpose()?;

        Ok(Self { graph, name, http })
    }
}

#[derive(Clone, Debug, Serialize)]
pub(super) struct HTTPSourceAPI {
    pub(crate) base_url: String,
    pub(crate) default: bool,
    pub(crate) headers: Vec<HTTPHeaderMapping>,
}

impl HTTPSourceAPI {
    pub(super) fn from_directive(
        directive: &Node<apollo_compiler::ast::Value>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let directive = directive
            .as_object()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidTypeForAttribute(
                    "object".to_string(),
                    "http".to_string(),
                )
            })?
            .iter()
            .map(|(name, value)| (name.clone(), value.clone()))
            .collect::<HashMap<_, _>>();

        let base_url = directive
            .get(&NodeStr::new("baseURL"))
            .ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    "baseURL".to_string(),
                    "http".to_string(),
                )
            })?
            .as_str()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidTypeForAttribute(
                    "string".to_string(),
                    "baseURL".to_string(),
                )
            })?
            .to_string();

        let default = directive
            .get(&NodeStr::new("default"))
            .and_then(|v| v.to_bool())
            .unwrap_or_default();

        let headers = directive
            .get(&NodeStr::new("headers"))
            .map(HTTPHeaderMapping::from_header_arguments)
            .transpose()?
            .unwrap_or_default();

        Ok(Self {
            base_url,
            default,
            headers,
        })
    }
}

#[derive(Clone, Debug, Serialize)]
pub(super) struct HTTPHeaderMapping {
    pub(crate) name: String,
    //TODO: how to translate?
    pub(crate) r#as: Option<String>,
    pub(crate) value: Option<String>,
}

impl HTTPHeaderMapping {
    pub(super) fn from_header_arguments(
        argument: &Node<Value>,
    ) -> Result<Vec<Self>, ConnectorDirectiveError> {
        Ok(argument
            .as_list()
            .map(|arguments| {
                arguments
                    .iter()
                    .map(Self::from_value)
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?
            .unwrap_or_default())
    }
    fn from_value(argument: &Node<Value>) -> Result<Self, ConnectorDirectiveError> {
        let header_arguments = argument.as_object().ok_or_else(|| {
            ConnectorDirectiveError::InvalidTypeForAttribute(
                "Object".to_string(),
                "headers".to_string(),
            )
        })?;
        let mut name = None;
        let mut r#as = Default::default();
        let mut value = Default::default();

        for (node_name, arg) in header_arguments.iter() {
            let as_string = arg.as_str().map(|s| s.to_string());
            match node_name.as_str() {
                "name" => {
                    name = as_string;
                }
                "as" => r#as = as_string,
                "value" => value = as_string,
                other => {
                    return Err(ConnectorDirectiveError::UnknownAttributeForType(
                        other.to_string(),
                        "header".to_string(),
                    ));
                }
            }
        }

        Ok(Self {
            name: name.ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    "name".to_string(),
                    "header".to_string(),
                )
            })?,
            r#as,
            value,
        })
    }
}

// --- @sourceType -------------------------------------------------------------

#[derive(Debug, Serialize)]
pub(super) struct SourceType {
    pub(super) graph: String,
    pub(super) type_name: String,
    pub(crate) api: String,
    pub(crate) http: Option<HTTPSourceType>,
    pub(crate) selection: JSONSelection,
    pub(crate) key_type_map: Option<KeyTypeMap>,
}

impl SourceType {
    pub(super) fn from_schema(
        schema: &Schema,
    ) -> Result<HashMap<String, Vec<Self>>, ConnectorDirectiveError> {
        let graph_names = graph_enum_map(schema).ok_or_else(|| {
            ConnectorDirectiveError::InvalidJoinDirective("Missing join__Graph enum".to_string())
        })?;

        let mut result: HashMap<String, Vec<Self>> = HashMap::new();

        for (name, ty) in &schema.types {
            let directives = ty
                .directives()
                .iter()
                .filter(|d| d.name == JOIN_TYPE_DIRECTIVE_NAME)
                .filter_map(|join_type| {
                    JoinWithDirectives::from_directive(&join_type.node).transpose()
                })
                .collect::<Result<Vec<_>, _>>()?;

            let source_types = directives
                .into_iter()
                .flat_map(|obj| {
                    obj.directives
                        .into_iter()
                        .filter(|d| d.name == SOURCE_TYPE_DIRECTIVE_NAME)
                        .map(move |d| (obj.graph.clone(), d.args))
                })
                .collect::<Vec<_>>();

            if !source_types.is_empty() {
                result.insert(
                    name.to_string(),
                    source_types
                        .iter()
                        .map(|(graph, args)| {
                            let graph_name = graph_names.get(graph).ok_or_else(|| {
                                ConnectorDirectiveError::InvalidJoinDirective(
                                    format!("Missing graph {} in join__Graph enum", graph)
                                        .to_string(),
                                )
                            })?;
                            Self::from_directive(graph_name.clone(), name.to_string(), args)
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                );
            }
        }

        Ok(result)
    }

    pub(super) fn from_directive(
        graph: String,
        type_name: String,
        directive: &HashMap<NodeStr, Node<apollo_compiler::ast::Value>>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let api = directive
            .get(&NodeStr::new("api"))
            .ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    "api".to_string(),
                    SOURCE_TYPE_DIRECTIVE_NAME.to_string(),
                )
            })?
            .as_str()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidTypeForAttribute(
                    "string".to_string(),
                    "api".to_string(),
                )
            })?
            .to_string();

        let selection = directive
            .get(&NodeStr::new("selection"))
            .ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    "selection".to_string(),
                    SOURCE_TYPE_DIRECTIVE_NAME.to_string(),
                )
            })?
            .as_str()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidTypeForAttribute(
                    "string".to_string(),
                    "selection".to_string(),
                )
            })
            .map(|v| {
                JSONSelection::parse(v).map_err(|_| {
                    ConnectorDirectiveError::ParseError(
                        "Failed to parse selection".to_string(),
                        "selection".to_string(),
                    )
                })
            })??
            .1;

        let http = directive
            .get(&NodeStr::new("http"))
            .map(HTTPSourceType::from_argument)
            .transpose()?;

        let key_type_map = directive
            .get(&NodeStr::new("keyTypeMap"))
            .map(KeyTypeMap::from_argument)
            .transpose()?;

        Ok(Self {
            graph,
            type_name,
            api,
            http,
            selection,
            key_type_map,
        })
    }

    pub(super) fn api_name(&self) -> String {
        format!("{}_{}", self.graph, self.api)
    }

    pub(super) fn selections(&self) -> Vec<Selection> {
        self.selection.clone().into()
    }

    pub(super) fn path_required_parameters(&self) -> Vec<String> {
        match &self.http {
            Some(http) => http.path_template.required_parameters(),
            None => vec![],
        }
    }
}

#[derive(Debug, Serialize)]
pub(super) struct HTTPSourceType {
    pub(crate) path_template: URLPathTemplate,
    #[serde(with = "http_serde::method")]
    pub(crate) method: http::Method,
    pub(crate) headers: Vec<HTTPHeaderMapping>,
    pub(crate) body: Option<JSONSelection>,
}

impl HTTPSourceType {
    pub(super) fn from_argument(argument: &Node<Value>) -> Result<Self, ConnectorDirectiveError> {
        let argument = argument
            .as_object()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidTypeForAttribute(
                    "object".to_string(),
                    "http".to_string(),
                )
            })?
            .iter()
            .map(|(name, value)| (name, value))
            .collect::<HashMap<_, _>>();

        fn parse_template(
            value: &Node<Value>,
            name: &str,
        ) -> Result<URLPathTemplate, ConnectorDirectiveError> {
            URLPathTemplate::parse(value.as_str().ok_or_else(|| {
                ConnectorDirectiveError::InvalidTypeForAttribute(
                    name.to_string(),
                    "String".to_string(),
                )
            })?)
            .map_err(|e| ConnectorDirectiveError::ParseError(name.to_string(), e))
        }

        let (path_template, method) = if let Some(get) = argument.get(&NodeStr::new("GET")) {
            (parse_template(get, "GET")?, http::Method::GET)
        } else if let Some(post) = argument.get(&NodeStr::new("POST")) {
            (parse_template(post, "POST")?, http::Method::POST)
        } else {
            return Err(ConnectorDirectiveError::RequiresExactlyOne(
                "GET, POST".to_string(),
                "HTTPSourceType".to_string(),
            ));
        };

        let headers = argument
            .get(&NodeStr::new("headers"))
            .map(|v| HTTPHeaderMapping::from_header_arguments(v))
            .transpose()?
            .unwrap_or_default();

        let body = argument
            .get(&NodeStr::new("body"))
            .map(|v| {
                let v = v.as_str().ok_or_else(|| {
                    ConnectorDirectiveError::InvalidTypeForAttribute(
                        "string".to_string(),
                        "body".to_string(),
                    )
                })?;

                Ok(JSONSelection::parse(v)
                    .map_err(|_| {
                        ConnectorDirectiveError::ParseError(
                            "Failed to parse selection".to_string(),
                            "body".to_string(),
                        )
                    })?
                    .1)
            })
            .transpose()?;

        Ok(Self {
            path_template,
            method,
            headers,
            body,
        })
    }
}

#[derive(Debug, Serialize)]
pub(super) struct KeyTypeMap {
    key: String,
    // Dictionary mapping possible __typename strings to values of the JSON
    // property named by key.
    type_map: HashMap<String, String>, // TODO: is this accurate?
}

impl KeyTypeMap {
    pub(super) fn from_argument(_arguments: &Node<Value>) -> Result<Self, ConnectorDirectiveError> {
        Ok(Self {
            key: Default::default(),
            type_map: Default::default(),
        })
    }
}

// --- @sourceField ------------------------------------------------------------

#[derive(Debug, Serialize)]
pub(super) struct SourceField {
    pub(super) graph: String,
    pub(super) parent_type_name: String,
    pub(super) field_name: String,
    pub(super) output_type_name: String,
    pub(super) api: String,
    pub(super) http: Option<HTTPSourceField>,
    pub(super) selection: JSONSelection,
}

impl SourceField {
    pub(super) fn from_schema(schema: &Schema) -> Result<Vec<Self>, ConnectorDirectiveError> {
        let graph_names = graph_enum_map(schema).ok_or_else(|| {
            ConnectorDirectiveError::InvalidJoinDirective("Missing join__Graph enum".to_string())
        })?;

        let mut source_fields = vec![];
        for (parent_type_name, ty) in schema.types.iter() {
            source_fields.extend(Self::from_type(
                &graph_names,
                parent_type_name.to_string(),
                ty,
            )?);
        }
        Ok(source_fields)
    }

    fn from_type(
        graph_names: &HashMap<String, String>,
        parent_type_name: String,
        ty: &ExtendedType,
    ) -> Result<Vec<Self>, ConnectorDirectiveError> {
        Ok(match ty {
            ExtendedType::Object(ty) => {
                Self::from_fields(graph_names, parent_type_name, &ty.fields)?
            }
            ExtendedType::Interface(ty) => {
                Self::from_fields(graph_names, parent_type_name, &ty.fields)?
            }
            _ => vec![],
        })
    }

    fn from_fields(
        graph_names: &HashMap<String, String>,
        parent_type_name: String,
        fields: &IndexMap<NodeStr, Component<FieldDefinition>>,
    ) -> Result<Vec<Self>, ConnectorDirectiveError> {
        let mut result: Vec<Self> = vec![];

        for (field_name, field_def) in fields {
            let directives = field_def
                .directives
                .iter()
                .filter(|d| d.name == JOIN_FIELD_DIRECTIVE_NAME)
                .filter_map(|d| JoinWithDirectives::from_directive(d).transpose())
                .collect::<Result<Vec<_>, _>>()?;

            let source_fields = directives
                .into_iter()
                .flat_map(|obj| {
                    obj.directives
                        .into_iter()
                        .filter(|d| d.name == SOURCE_FIELD_DIRECTIVE_NAME)
                        .map(move |d| (obj.graph.clone(), d.args))
                })
                .collect::<Vec<_>>();

            result.extend(
                source_fields
                    .iter()
                    .map(|(graph, args)| {
                        let graph_name = graph_names.get(graph).ok_or_else(|| {
                            ConnectorDirectiveError::InvalidJoinDirective(
                                format!("Missing graph {} in join__Graph enum", graph).to_string(),
                            )
                        })?;
                        Self::from_directive(
                            graph_name.clone(),
                            parent_type_name.clone(),
                            field_name.to_string(),
                            field_def.ty.inner_named_type().to_string(),
                            args,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            );
        }

        Ok(result)
    }

    pub(super) fn from_directive(
        graph: String,
        parent_type_name: String,
        field_name: String,
        output_type_name: String,
        directive: &HashMap<NodeStr, Node<apollo_compiler::ast::Value>>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let api = directive
            .get(&NodeStr::new("api"))
            .ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    "api".to_string(),
                    SOURCE_FIELD_DIRECTIVE_NAME.to_string(),
                )
            })?
            .as_str()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidTypeForAttribute(
                    "string".to_string(),
                    "api".to_string(),
                )
            })?
            .to_string();

        let selection = directive
            .get(&NodeStr::new("selection"))
            .ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    "selection".to_string(),
                    SOURCE_FIELD_DIRECTIVE_NAME.to_string(),
                )
            })?
            .as_str()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidTypeForAttribute(
                    "string".to_string(),
                    "selection".to_string(),
                )
            })
            .map(|v| {
                JSONSelection::parse(v).map_err(|_| {
                    ConnectorDirectiveError::ParseError(
                        "Failed to parse selection".to_string(),
                        "selection".to_string(),
                    )
                })
            })??
            .1;

        let http = directive
            .get(&NodeStr::new("http"))
            .map(HTTPSourceField::from_argument)
            .transpose()?;

        Ok(Self {
            graph,
            parent_type_name,
            field_name,
            output_type_name,
            api,
            http,
            selection,
        })
    }

    pub(super) fn api_name(&self) -> String {
        format!("{}_{}", self.graph, self.api)
    }

    pub(super) fn selections(&self) -> Vec<Selection> {
        self.selection.clone().into()
    }

    pub(super) fn path_required_parameters(&self) -> Vec<String> {
        match &self.http {
            Some(http) => http.path_template.required_parameters(),
            None => vec![],
        }
    }
}

#[derive(Debug, Serialize)]
pub(super) struct HTTPSourceField {
    path_template: URLPathTemplate,
    #[serde(with = "http_serde::method")]
    method: http::Method,
    body: Option<JSONSelection>,
}

impl HTTPSourceField {
    fn from_argument(argument: &Node<Value>) -> Result<Self, ConnectorDirectiveError> {
        let argument = argument
            .as_object()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidTypeForAttribute(
                    "object".to_string(),
                    HTTP_ARGUMENT_NAME.to_string(),
                )
            })?
            .iter()
            .map(|(name, value)| (name, value))
            .collect::<HashMap<_, _>>();

        fn parse_template(
            value: &Node<Value>,
            name: &str,
        ) -> Result<URLPathTemplate, ConnectorDirectiveError> {
            URLPathTemplate::parse(value.as_str().ok_or_else(|| {
                ConnectorDirectiveError::InvalidTypeForAttribute(
                    name.to_string(),
                    "String".to_string(),
                )
            })?)
            .map_err(|e| ConnectorDirectiveError::ParseError(name.to_string(), e))
        }

        let (path_template, method) = if let Some(get) = argument.get(&NodeStr::new("GET")) {
            (parse_template(get, "GET")?, http::Method::GET)
        } else if let Some(post) = argument.get(&NodeStr::new("POST")) {
            (parse_template(post, "POST")?, http::Method::POST)
        } else if let Some(patch) = argument.get(&NodeStr::new("PATCH")) {
            (parse_template(patch, "PATCH")?, http::Method::PATCH)
        } else if let Some(put) = argument.get(&NodeStr::new("PUT")) {
            (parse_template(put, "PUT")?, http::Method::PUT)
        } else if let Some(delete) = argument.get(&NodeStr::new("DELETE")) {
            (parse_template(delete, "DELETE")?, http::Method::DELETE)
        } else {
            return Err(ConnectorDirectiveError::RequiresExactlyOne(
                "GET, PATCH, POST, PUT, DELETE".to_string(),
                "HTTPSourceField".to_string(),
            ));
        };

        let body = argument
            .get(&NodeStr::new("body"))
            .map(|v| {
                let v = v.as_str().ok_or_else(|| {
                    ConnectorDirectiveError::InvalidTypeForAttribute(
                        "string".to_string(),
                        "body".to_string(),
                    )
                })?;

                Ok(JSONSelection::parse(v)
                    .map_err(|_| {
                        ConnectorDirectiveError::ParseError(
                            "Failed to parse selection".to_string(),
                            "body".to_string(),
                        )
                    })?
                    .1)
            })
            .transpose()?;

        Ok(Self {
            path_template,
            method,
            body,
        })
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_json_snapshot;

    use super::*;

    #[test]
    fn test_source_api_directive_has_no_errors() {
        let partial_sdl = r#"
            enum join__Graph {
              CONTACTS @join__graph(name: "contacts")
            }

            schema
              @join__schema(
                graph: CONTACTS
                directives: [
                  {
                    name: "sourceAPI"
                    args: {
                      name: "rest_contacts"
                      http: {
                          baseURL: "http://localhost:4002/contacts/"
                          default: true
                          headers: [
                              { name: "x-test", value: "test1234" }
                              { name: "x-before-rename-test", as: "x-after-rename-test" }
                              {
                                  name: "x-before-rename-and-with-value-test",
                                  as: "x-after-rename-and-with-value-test",
                                  value: "test5678"
                              }
                          ]
                      }
                    }
                  }
                  {
                    name: "sourceAPI"
                    args: {
                      name: "rest_notes"
                      http: { baseURL: "http://localhost:4002/notes/" }
                    }
                  }
                  {
                    name: "sourceAPI"
                    args: {
                      name: "legacy_contacts"
                      http: { baseURL: "http://localhost:4002/legacy/contacts/" }
                    }
                  }
                ]
              )
            {
                query: Query
            }
            "#;
        let partial_schema = apollo_compiler::Schema::parse(partial_sdl, "schema.graphql");

        let all_source_apis = SourceAPI::from_schema(&partial_schema).unwrap();

        insta::with_settings!({sort_maps => true}, {
            assert_json_snapshot!(all_source_apis);
        });
    }

    #[test]
    fn test_source_api_directive_missing_mandatory_fields() {
        let partial_sdl = r#"
            enum join__Graph {
              CONTACTS @join__graph(name: "contacts")
            }

            schema
              @join__schema(
                graph: CONTACTS
                directives: [
                  {
                    name: "sourceAPI"
                    args: {
                      http: {
                        baseURL: "http://localhost:4002/contacts/"
                      }
                    }
                  }
                ]
              )
            { query: Query }
            "#;

        let partial_schema = apollo_compiler::Schema::parse(partial_sdl, "schema.graphql");
        let missing_name_error = SourceAPI::from_schema(&partial_schema).unwrap_err();
        assert_eq!(
            ConnectorDirectiveError::MissingAttributeForType(
                "name".to_string(),
                "sourceAPI".to_string()
            ),
            missing_name_error
        );

        let partial_sdl = r#"
            enum join__Graph {
              CONTACTS @join__graph(name: "contacts")
            }

            schema
              @join__schema(
                graph: CONTACTS
                directives: [
                  {
                    name: "sourceAPI"
                    args: {
                      name: "missing_base_url"
                      http: {
                          default: true
                      }
                    }
                  }
                ]
              )
            { query: Query }
            "#;

        let partial_schema = apollo_compiler::Schema::parse(partial_sdl, "schema.graphql");
        let missing_base_url_error = SourceAPI::from_schema(&partial_schema).unwrap_err();
        assert_eq!(
            ConnectorDirectiveError::MissingAttributeForType(
                "baseURL".to_string(),
                "http".to_string()
            ),
            missing_base_url_error
        );

        let partial_sdl = r#"
            enum join__Graph {
              CONTACTS @join__graph(name: "contacts")
            }

            schema
              @join__schema(
                graph: CONTACTS
                directives: [
                  {
                    name: "sourceAPI"
                    args: {
                      name: "missing_header_name"
                      http: {
                          baseURL: "http://localhost:4002/contacts/"
                          headers: [{ as: "missing mandatory name field" }]
                      }
                    }
                  }
                ]
              )
            { query: Query }
            "#;

        let partial_schema = apollo_compiler::Schema::parse(partial_sdl, "schema.graphql");
        let missing_header_name_error = SourceAPI::from_schema(&partial_schema).unwrap_err();
        assert_eq!(
            ConnectorDirectiveError::MissingAttributeForType(
                "name".to_string(),
                "header".to_string()
            ),
            missing_header_name_error
        );
    }

    #[test]
    fn test_valid_source_types() {
        let partial_sdl = r#"
        enum join__Graph {
          CONTACTS @join__graph(name: "contacts")
        }

        type ValidSourceType
          @join__type(
            graph: CONTACTS
            key: "contactId"
            directives: [
              {
                name: "sourceType"
                args: { api: "contacts", http: { GET: "/contacts/{contactId}" }, selection: "a" }
              }
            ]
          )
        {
            id: ID!
            name: String
        }

        type ValidSourceTypeDefaultHttp
          @join__type(
            graph: CONTACTS
            key: "contactId"
            directives: [
              {
                name: "sourceType"
                args: { api: "contacts", http: { GET: "/contacts/{contactId}" }, selection: "a" }
              }
            ]
          )
        {
            id: ID!
            name: String
        }
        "#;

        let partial_schema = apollo_compiler::Schema::parse(partial_sdl, "schema.graphql");
        let source_types = SourceType::from_schema(&partial_schema).unwrap();

        insta::with_settings!({sort_maps => true}, {
            assert_json_snapshot!(source_types);
        });
    }

    #[test]
    fn test_valid_source_field() {
        let partial_sdl = r#"
        enum join__Graph {
          CONTACTS @join__graph(name: "contacts")
        }

        type Query {
          field: String
            @join__field(
              graph: CONTACTS
              directives: [
                {
                  name: "sourceField"
                  args: {
                    api: "contacts"
                    http: { GET: "/contacts/{contactId}" }
                    selection: "id name"
                  }
                }
              ]
            )
        }
        "#;

        let partial_schema = apollo_compiler::Schema::parse(partial_sdl, "schema.graphql");
        let source_fields = SourceField::from_schema(&partial_schema).unwrap();

        insta::with_settings!({sort_maps => true}, {
            assert_json_snapshot!(source_fields, @r###"
            [
              {
                "graph": "contacts",
                "parent_type_name": "Query",
                "field_name": "field",
                "output_type_name": "String",
                "api": "contacts",
                "http": {
                  "path_template": "/contacts/{contactId!}",
                  "method": "GET",
                  "body": null
                },
                "selection": {
                  "Named": {
                    "selections": [
                      {
                        "Field": [
                          null,
                          "id",
                          null
                        ]
                      },
                      {
                        "Field": [
                          null,
                          "name",
                          null
                        ]
                      }
                    ],
                    "star": null
                  }
                }
              }
            ]
            "###);
        });

        let partial_sdl = r#"
        enum join__Graph {
          CONTACTS @join__graph(name: "contacts")
        }

        type Query {
          field: String
            @join__field(
              graph: CONTACTS
              directives: [
                {
                  name: "sourceField"
                  args: {
                    api: "contacts"
                    http: { body: "id name" }
                    selection: "id name"
                  }
                }
              ]
            )
        }
        "#;

        let partial_schema = apollo_compiler::Schema::parse(partial_sdl, "schema.graphql");

        assert_eq!(
            SourceField::from_schema(&partial_schema).unwrap_err(),
            ConnectorDirectiveError::RequiresExactlyOne(
                "GET, PATCH, POST, PUT, DELETE".to_string(),
                "HTTPSourceField".to_string(),
            ),
        );
    }
}
