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
pub(crate) const SOURCE_API_ENUM_NAME: &str = "SOURCE_API";

const SOURCE_TYPE_DIRECTIVE_NAME: &str = "source_type";

#[derive(Debug, Serialize)]
pub(super) struct SourceAPI {
    name: String,
    http: Option<HTTPSourceAPI>,
}

// TODO: remove one of both once we land on the directive position
impl SourceAPI {
    pub(super) fn from_root_enum(
        component: &Component<EnumValueDefinition>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let (name, http) = component
            .directives
            .0
            .iter()
            .find(|d| d.name == SOURCE_API_DIRECTIVE_NAME)
            .map(|directive| {
                let name = directive
                    .argument_by_name("name")
                    .ok_or_else(|| {
                        ConnectorDirectiveError::MissingAttributeForType(
                            "name".to_string(),
                            SOURCE_API_DIRECTIVE_NAME.to_string(),
                        )
                    })?
                    .as_str()
                    .ok_or_else(|| {
                        ConnectorDirectiveError::InvalidTypeForAttribute(
                            "String!".to_string(),
                            "name".to_string(),
                        )
                    })?
                    .to_string();

                Ok((name, HTTPSourceAPI::from_directive(directive)?))
            })
            .transpose()?
            .ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    SOURCE_API_DIRECTIVE_NAME.to_string(),
                    SOURCE_API_ENUM_NAME.to_string(),
                )
            })?;

        Ok(Self {
            name,
            http: Some(http),
        })
    }

    pub(super) fn from_schema_directive(
        schema_directive: &Component<Directive>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let name = schema_directive
            .argument_by_name("name")
            .as_ref()
            .and_then(|name| name.as_str())
            .ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    "name".to_string(),
                    SOURCE_API_DIRECTIVE_NAME.to_string(),
                )
            })?
            .to_string();

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
                "base_url" => base_url = node.as_str(),
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
            base_url: base_url
                .ok_or_else(|| {
                    ConnectorDirectiveError::MissingAttributeForType(
                        "base_url".to_string(),
                        HTTP_ARGUMENT_NAME.to_string(),
                    )
                })?
                .to_string(),
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

#[derive(Debug, Serialize)]
pub(super) struct SourceType {
    api: String,
    http: Option<HTTPSourceType>,
    selection: Option<JSONSelection>,
    key_type_map: Option<KeyTypeMap>,
}

impl SourceType {
    pub(super) fn from_arguments(arguments: &Node<Value>) -> Result<Self, ConnectorDirectiveError> {
        let args = arguments.as_object().ok_or_else(|| {
            ConnectorDirectiveError::InvalidTypeForAttribute(
                "Object".to_string(),
                "source_type".to_string(),
            )
        })?;

        let mut api = Default::default();
        let mut http = Default::default();
        let mut selection = Default::default();
        let mut key_type_map = Default::default();

        for (name, node) in args.iter() {
            match name.as_str() {
                "api" => {
                    api = node
                        .as_str()
                        .ok_or_else(|| {
                            ConnectorDirectiveError::InvalidTypeForAttribute(
                                "string".to_string(),
                                "api".to_string(),
                            )
                        })?
                        .to_string()
                }
                "http" => http = Some(HTTPSourceType::from_arguments(node)?),
                "selection" => {
                    selection = Some(
                        JSONSelection::parse(node.as_str().ok_or_else(|| {
                            ConnectorDirectiveError::InvalidTypeForAttribute(
                                "string".to_string(),
                                "selection".to_string(),
                            )
                        })?)
                        .map_err(|e| {
                            ConnectorDirectiveError::ParseError(
                                e.to_string(),
                                "selection".to_string(),
                            )
                        })?
                        .1,
                    )
                }
                "keyTypeMap" => key_type_map = Some(KeyTypeMap::from_arguments(node)?),
                other => {
                    return Err(ConnectorDirectiveError::UnknownAttributeForType(
                        other.to_string(),
                        SOURCE_TYPE_DIRECTIVE_NAME.to_string(),
                    ))
                }
            }
        }

        Ok(Self {
            api,
            http,
            selection,
            key_type_map,
        })
    }
}

// TODO: impl tryfrom with XOR validation on methods
#[derive(Debug, Serialize)]
pub(super) struct HTTPSourceType {
    get: Option<URLPathTemplate>,
    post: Option<URLPathTemplate>,
    headers: Vec<HTTPHeaderMapping>,
    body: Option<JSONSelection>,
}

impl HTTPSourceType {
    pub(super) fn from_arguments(arguments: &Node<Value>) -> Result<Self, ConnectorDirectiveError> {
        Ok(Self {
            get: Default::default(),
            post: Default::default(),
            headers: Default::default(),
            body: Default::default(),
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
    pub(super) fn from_arguments(arguments: &Node<Value>) -> Result<Self, ConnectorDirectiveError> {
        Ok(Self {
            key: Default::default(),
            type_map: Default::default(),
        })
    }
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

#[cfg(test)]
mod tests {
    use insta::assert_json_snapshot;

    use super::*;
    use crate::spec::Schema;
    use crate::Configuration;

    #[test]
    fn test_enum_directive_has_no_errors() {
        let partial_sdl = r#"  
            enum SOURCE_API {
                CONTACTS
                @source_api(
                    name: "rest_contacts"
                    http: {
                        base_url: "http://localhost:4002/contacts/"
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
                )
                NOTES
                @source_api(
                    name: "rest_notes"
                    http: { base_url: "http://localhost:4002/notes/" }
                )
                LEGACY_CONTACTS
                @source_api(
                    name: "legacy_contacts"
                    http: { base_url: "http://localhost:4002/legacy/contacts/" }
                )
            }"#;
        let partial_schema =
            Schema::parse(partial_sdl, &Configuration::fake_builder().build().unwrap()).unwrap();

        let root_enum = partial_schema
            .definitions
            .get_enum(SOURCE_API_ENUM_NAME)
            .unwrap();

        // for each of the variants, let's get the name, and create a SourceApi item.
        let all_source_apis = root_enum
            .values
            .iter()
            .map(|(node, value)| {
                // the node contains the name,
                // let's craft a SourceApi from the directive metadata
                SourceAPI::from_root_enum(value)
                    .map(|source_api| (node.as_str().to_string(), source_api))
                    .unwrap()
            })
            .collect::<HashMap<_, _>>();

        insta::with_settings!({sort_maps => true}, {
            assert_json_snapshot!(all_source_apis);
        });
    }

    #[test]
    fn test_enum_directive_missing_mandatory_fields() {
        let partial_sdl = r#"
            directive @source_api(name: String!, http: HTTPSourceAPI) on ENUM_VALUE

            input HTTPSourceAPI {
            base_url: String!
            default: Boolean
            headers: [HTTPHeaderMapping!]
            }

            input HTTPHeaderMapping {
            name: String!
            as: String
            value: String
            }

            enum SOURCE_API {
                MISSING_NAME @source_api(
                    http: {
                        base_url: "http://localhost:4002/contacts/"
                    }
                )
                MISSING_BASE_URL @source_api(
                    name: "missing_base_url"
                    http: {
                    }
                )
                MISSING_HEADER_NAME @source_api(
                    name: "missing_header_name"
                    http: {
                        base_url: "http://localhost:4002/contacts/"
                        headers: [{ }]
                    }
                )
            }"#;

        let partial_schema =
            Schema::parse(partial_sdl, &Configuration::fake_builder().build().unwrap()).unwrap();

        let root_enum = partial_schema
            .definitions
            .get_enum(SOURCE_API_ENUM_NAME)
            .unwrap();

        // for each of the variants, let's get the name, and create a SourceApi item.
        let mut all_source_apis = root_enum
            .values
            .iter()
            .map(|(node, value)| {
                // the node contains the name,
                // let's craft a SourceApi from the directive metadata
                (node.as_str().to_string(), SourceAPI::from_root_enum(value))
            })
            .collect::<HashMap<_, _>>();

        let missing_name_error = all_source_apis.remove("MISSING_NAME").unwrap().unwrap_err();
        assert_eq!(
            ConnectorDirectiveError::MissingAttributeForType(
                "name".to_string(),
                "source_api".to_string()
            ),
            missing_name_error
        );

        let missing_base_url_error = all_source_apis
            .remove("MISSING_BASE_URL")
            .unwrap()
            .unwrap_err();
        assert_eq!(
            ConnectorDirectiveError::MissingAttributeForType(
                "base_url".to_string(),
                "http".to_string()
            ),
            missing_base_url_error
        );

        let missing_header_name_error = all_source_apis
            .remove("MISSING_HEADER_NAME")
            .unwrap()
            .unwrap_err();
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
        type ValidSourceType
            @source_type(api: CONTACTS, http: { GET: "/contacts/{contactId}" }) {
            id: ID!
            name: String
        }
        type ValidSourceTypeDefaultHttp
            @source_type(api: CONTACTS) {
                id: ID!
                name: String
            }
        "#;

        let partial_schema =
            Schema::parse(partial_sdl, &Configuration::fake_builder().build().unwrap()).unwrap();

        let valid_source_type = SourceType::from_arguments(
            partial_schema
                .definitions
                .get_object("ValidSourceType")
                .unwrap()
                .directives
                .get(SOURCE_TYPE_DIRECTIVE_NAME)
                .unwrap()
                .arguments,
        )
        .unwrap();

        insta::with_settings!({sort_maps => true}, {
            assert_json_snapshot!(valid_source_type);
        });

        let valid_source_type_default_http = SourceType::from_arguments(
            partial_schema
                .definitions
                .get_object("ValidSourceTypeDefaultHttp")
                .unwrap()
                .directives
                .get(SOURCE_TYPE_DIRECTIVE_NAME)
                .unwrap()
                .arguments,
        )
        .unwrap();

        insta::with_settings!({sort_maps => true}, {
            assert_json_snapshot!(valid_source_type_default_http);
        });
    }
}
