#![allow(dead_code)]

use std::collections::HashMap;

use apollo_compiler::schema::Component;
use apollo_compiler::schema::Directive;
use apollo_compiler::schema::EnumValueDefinition;
use apollo_compiler::schema::Value;
use apollo_compiler::Node;
use serde::Serialize;

use super::selection_parser::Selection as JSONSelection;
use super::url_path_parser::Template as URLPathTemplate;
use crate::error::ConnectorDirectiveError;

pub(super) const SOURCE_API_DIRECTIVE_NAME: &str = "source_api";
const HTTP_ARGUMENT_NAME: &str = "http";

#[derive(Debug, Serialize)]
pub(super) struct SourceAPI {
    name: String,
    http: Option<HTTPSourceAPI>,
}

// TODO: remove one of both once we land on the directive position
impl SourceAPI {
    pub(super) fn from_directive(
        name: String,
        component: &Component<EnumValueDefinition>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let http = component
            .directives
            .0
            .iter()
            .find(|d| d.name == SOURCE_API_DIRECTIVE_NAME)
            .map(HTTPSourceAPI::from_directive)
            .transpose()?;
        Ok(Self { name, http })
    }

    pub(super) fn from_schema_directive(
        schema_directive: &Component<Directive>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let name = schema_directive
            .argument_by_name("name")
            .as_ref()
            .map(|name| name.as_str().unwrap().to_string())
            .unwrap_or_default();

        let http = Some(HTTPSourceAPI::from_directive(schema_directive)?);

        Ok(Self { name, http })
    }
}

#[derive(Debug, Serialize)]
pub(super) struct HTTPSourceAPI {
    base_url: String,
    default: Option<bool>,
    headers: Vec<HTTPHeaderMapping>,
}

impl HTTPSourceAPI {
    pub(super) fn from_directive(
        directive: &Node<Directive>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let mut base_url = Default::default();
        let mut default = Default::default();
        let mut headers = Default::default();

        for (name, node) in directive
            .arguments
            .iter()
            .find(|argument| argument.name == HTTP_ARGUMENT_NAME)
            .ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    HTTP_ARGUMENT_NAME.to_string(),
                    SOURCE_API_DIRECTIVE_NAME.to_string(),
                )
            })?
            .value
            .as_object()
            .ok_or_else(|| {
                ConnectorDirectiveError::InvalidTypeForAttribute(
                    "object".to_string(),
                    HTTP_ARGUMENT_NAME.to_string(),
                )
            })?
            .iter()
        {
            match name.as_str() {
                // TODO: error handling plz ^^'
                "base_url" => {
                    base_url = node
                        .as_str()
                        .ok_or_else(|| {
                            ConnectorDirectiveError::InvalidTypeForAttribute(
                                "String".to_string(),
                                "base_url".to_string(),
                            )
                        })?
                        .to_string()
                }
                "default" => {
                    default = Some(node.to_bool().ok_or_else(|| {
                        ConnectorDirectiveError::InvalidTypeForAttribute(
                            "boolean".to_string(),
                            "default".to_string(),
                        )
                    })?)
                }
                "headers" => headers = HTTPHeaderMapping::from_header_arguments(node)?,
                other => {
                    return Err(ConnectorDirectiveError::UnknownAttributeForType(
                        other.to_string(),
                        HTTP_ARGUMENT_NAME.to_string(),
                    ))
                }
            }
        }

        Ok(Self {
            base_url,
            default,
            headers,
        })
    }
}

#[derive(Debug, Serialize)]
pub(super) struct HTTPHeaderMapping {
    name: String,
    r#as: Option<String>,
    value: Option<String>,
}

impl HTTPHeaderMapping {
    // TODO: maybe a result?
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
    // TODO: 100% a result, the name is mandatory!
    fn from_value(argument: &Node<Value>) -> Result<Self, ConnectorDirectiveError> {
        let header_arguments = argument.as_object().unwrap();
        let mut name = Default::default();
        let mut r#as = Default::default();
        let mut value = Default::default();

        for (node_name, arg) in header_arguments.iter() {
            let as_string = arg.as_str().map(|s| s.to_string());
            match node_name.as_str() {
                "name" => {
                    name = as_string.ok_or_else(|| {
                        ConnectorDirectiveError::MissingAttributeForType(
                            "name".to_string(),
                            "header".to_string(),
                        )
                    })?;
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

        Ok(Self { name, r#as, value })
    }
}

#[derive(Debug, Serialize)]
pub(super) struct SourceType {
    api: String,
    http: Option<HTTPSourceType>,
    selection: Option<JSONSelection>,
    key_type_map: Option<KeyTypeMap>,
}

// TODO: impl tryfrom with XOR validation on methods
#[derive(Debug, Serialize)]
pub(super) struct HTTPSourceType {
    get: Option<URLPathTemplate>,
    post: Option<URLPathTemplate>,
    headers: Vec<HTTPHeaderMapping>,
    body: Option<JSONSelection>,
}

#[derive(Debug, Serialize)]
pub(super) struct KeyTypeMap {
    key: String,
    // Dictionary mapping possible __typename strings to values of the JSON
    // property named by key.
    type_map: HashMap<String, String>, // TODO: is this accurate?
}

#[derive(Debug, Serialize)]
pub(super) struct SourceField {
    api: Option<String>,
    http: Option<HTTPSourceField>,
    selection: Option<JSONSelection>,
}

// TODO: impl tryfrom with XOR validation on methods
#[derive(Debug, Serialize)]
pub(super) struct HTTPSourceField {
    get: Option<URLPathTemplate>,
    post: Option<URLPathTemplate>,
    put: Option<URLPathTemplate>,
    patch: Option<URLPathTemplate>,
    delete: Option<URLPathTemplate>,
    headers: Vec<HTTPHeaderMapping>,
    body: Option<JSONSelection>,
}
