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

pub(super) const SOURCE_API_DIRECTIVE_NAME: &str = "sourceAPI";
const HTTP_ARGUMENT_NAME: &str = "http";

const SOURCE_TYPE_DIRECTIVE_NAME: &str = "sourceType";
const SOURCE_FIELD_DIRECTIVE_NAME: &str = "sourceField";

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
        schema
            .schema_definition
            .directives
            .iter()
            .filter(|d| d.name == SOURCE_API_DIRECTIVE_NAME)
            .map(|source_api_directive| {
                let source_api = Self::from_schema_directive(source_api_directive)?;
                Ok::<_, ConnectorDirectiveError>((
                    format!("{}_{}", source_api.graph, source_api.name),
                    source_api,
                ))
            })
            .collect::<Result<HashMap<_, _>, _>>()
    }

    pub(super) fn from_schema_directive(
        schema_directive: &Component<Directive>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let graph = schema_directive
            .argument_by_name("graph")
            .as_ref()
            .and_then(|graph| graph.as_str())
            .ok_or_else(|| {
                ConnectorDirectiveError::MissingAttributeForType(
                    "graph".to_string(),
                    SOURCE_API_DIRECTIVE_NAME.to_string(),
                )
            })?
            .to_string();

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

        Ok(Self { graph, name, http })
    }
}

#[derive(Clone, Debug, Serialize)]
pub(super) struct HTTPSourceAPI {
    pub(crate) base_url: String,
    pub(crate) default: Option<bool>,
    pub(crate) headers: Vec<HTTPHeaderMapping>,
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
                "baseURL" => base_url = node.as_str(),
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
                        "baseURL".to_string(),
                        HTTP_ARGUMENT_NAME.to_string(),
                    )
                })?
                .to_string(),
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

#[derive(Debug, Serialize)]
pub(super) struct SourceType {
    pub(super) graph: String,
    pub(super) type_name: String,
    pub(crate) api: String,
    pub(crate) http: Option<HTTPSourceType>,
    pub(crate) selection: Option<JSONSelection>,
    pub(crate) key_type_map: Option<KeyTypeMap>,
}

impl SourceType {
    pub(super) fn from_schema(
        schema: &Schema,
    ) -> Result<HashMap<String, Self>, ConnectorDirectiveError> {
        Ok(schema
            .types
            .iter()
            .flat_map(|(name, object)| {
                object
                    .directives()
                    .get(SOURCE_TYPE_DIRECTIVE_NAME)
                    .map(|directive| {
                        let source_type = Self::from_directive(name.to_string(), directive)?;

                        Ok::<_, ConnectorDirectiveError>((name.to_string(), source_type))
                    })
            })
            .collect::<Result<HashMap<_, _>, _>>()
            .unwrap_or_default())
    }

    pub(super) fn from_directive(
        type_name: String,
        directive: &Component<Directive>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let mut graph = Default::default();
        let mut api = Default::default();
        let mut http = Default::default();
        let mut selection = Default::default();
        let mut key_type_map = Default::default();

        for argument in directive.arguments.iter() {
            match argument.name.as_str() {
                "graph" => {
                    graph = argument
                        .value
                        .as_str()
                        .ok_or_else(|| {
                            ConnectorDirectiveError::InvalidTypeForAttribute(
                                "string".to_string(),
                                "graph".to_string(),
                            )
                        })?
                        .to_string()
                }
                "api" => {
                    api = argument
                        .value
                        .as_str()
                        .ok_or_else(|| {
                            ConnectorDirectiveError::InvalidTypeForAttribute(
                                "string".to_string(),
                                "api".to_string(),
                            )
                        })?
                        .to_string()
                }
                "http" => http = Some(HTTPSourceType::from_argument(&argument.value)?),
                "selection" => {
                    selection = Some(
                        JSONSelection::parse(argument.value.as_str().ok_or_else(|| {
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
                "keyTypeMap" => key_type_map = Some(KeyTypeMap::from_arguments(&argument.value)?),
                other => {
                    return Err(ConnectorDirectiveError::UnknownAttributeForType(
                        other.to_string(),
                        SOURCE_TYPE_DIRECTIVE_NAME.to_string(),
                    ))
                }
            }
        }

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
        match &self.selection {
            Some(selection) => selection.clone().into(),
            None => vec![],
        }
    }

    pub(super) fn path_required_parameters(&self) -> Vec<String> {
        match &self.http {
            Some(http) => {
                if let Some(get) = &http.get {
                    get.required_parameters()
                } else if let Some(post) = &http.post {
                    post.required_parameters()
                } else {
                    vec![]
                }
            }
            None => vec![],
        }
    }
}

// TODO: impl tryfrom with XOR validation on methods
#[derive(Debug, Serialize)]
pub(super) struct HTTPSourceType {
    pub(crate) get: Option<URLPathTemplate>,
    pub(crate) post: Option<URLPathTemplate>,
    pub(crate) headers: Vec<HTTPHeaderMapping>,
    pub(crate) body: Option<JSONSelection>,
}

impl HTTPSourceType {
    pub(super) fn from_argument(argument: &Node<Value>) -> Result<Self, ConnectorDirectiveError> {
        let mut get = Default::default();
        let mut post = Default::default();
        let mut body = Default::default();

        let value = argument.as_object().ok_or_else(|| {
            ConnectorDirectiveError::InvalidTypeForAttribute(
                "object".to_string(),
                HTTP_ARGUMENT_NAME.to_string(),
            )
        })?;

        let mut methods_set = 0;

        for (name, value) in value.iter() {
            match name.as_str() {
                "GET" => {
                    get = Some(
                        URLPathTemplate::parse(value.as_str().expect("must be a string")).map_err(
                            |_| {
                                ConnectorDirectiveError::ParseError(
                                    "GET".to_string(),
                                    "URLPathTemplate".to_string(),
                                )
                            },
                        )?,
                    );
                    methods_set += 1;
                }
                "POST" => {
                    post = Some(
                        URLPathTemplate::parse(value.as_str().expect("must be a string")).map_err(
                            |_| {
                                ConnectorDirectiveError::ParseError(
                                    "POST".to_string(),
                                    "URLPathTemplate".to_string(),
                                )
                            },
                        )?,
                    );
                    methods_set += 1;
                }

                "body" => {
                    body = Some(
                        JSONSelection::parse(value.as_str().ok_or_else(|| {
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
                other => {
                    return Err(ConnectorDirectiveError::UnknownAttributeForType(
                        other.to_string(),
                        HTTP_ARGUMENT_NAME.to_string(),
                    ))
                }
            }
        }

        if methods_set != 1 {
            return Err(ConnectorDirectiveError::RequiresExactlyOne(
                "HTTPSourceField".to_string(),
                "GET, POST".to_string(),
            ));
        }

        Ok(Self {
            get,
            post,
            headers: Default::default(),
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
    pub(super) fn from_arguments(
        _arguments: &Node<Value>,
    ) -> Result<Self, ConnectorDirectiveError> {
        Ok(Self {
            key: Default::default(),
            type_map: Default::default(),
        })
    }
}

#[derive(Debug, Serialize)]
pub(super) struct SourceField {
    pub(super) graph: String,
    pub(super) parent_type_name: String,
    pub(super) field_name: String,
    pub(super) output_type_name: String,
    api: String,
    http: Option<HTTPSourceField>,
    selection: Option<JSONSelection>,
}

impl SourceField {
    pub(super) fn from_schema(schema: &Schema) -> Result<Vec<Self>, ConnectorDirectiveError> {
        let mut source_fields = vec![];
        for (parent_type_name, ty) in schema.types.iter() {
            source_fields.extend(Self::from_type(parent_type_name.to_string(), ty)?);
        }
        Ok(source_fields)
    }

    fn from_type(
        parent_type_name: String,
        ty: &ExtendedType,
    ) -> Result<Vec<Self>, ConnectorDirectiveError> {
        Ok(match ty {
            ExtendedType::Object(ty) => Self::from_fields(parent_type_name, &ty.fields)?,
            ExtendedType::Interface(ty) => Self::from_fields(parent_type_name, &ty.fields)?,
            _ => vec![],
        })
    }

    fn from_fields(
        parent_type_name: String,
        fields: &IndexMap<NodeStr, Component<FieldDefinition>>,
    ) -> Result<Vec<Self>, ConnectorDirectiveError> {
        let parent_type_name = parent_type_name.clone();
        fields
            .iter()
            .flat_map(|(name, field)| {
                let parent_type_name = parent_type_name.clone();
                field
                    .directives
                    .iter()
                    .filter(|d| d.name == SOURCE_FIELD_DIRECTIVE_NAME)
                    .map(move |directive| {
                        let source_field = Self::from_directive(
                            parent_type_name.clone(),
                            name.to_string(),
                            field.ty.inner_named_type().to_string(),
                            directive,
                        )?;
                        Ok(source_field)
                    })
            })
            .collect()
    }

    pub(super) fn from_directive(
        parent_type_name: String,
        field_name: String,
        output_type_name: String,
        directive: &Node<Directive>,
    ) -> Result<Self, ConnectorDirectiveError> {
        let mut graph = Default::default();
        let mut api = Default::default();
        let mut http = Default::default();
        let mut selection = Default::default();

        for argument in directive.arguments.iter() {
            match argument.name.as_str() {
                "graph" => {
                    graph = argument
                        .value
                        .as_str()
                        .ok_or_else(|| {
                            ConnectorDirectiveError::InvalidTypeForAttribute(
                                "string".to_string(),
                                "graph".to_string(),
                            )
                        })?
                        .to_string()
                }
                "api" => {
                    api = argument
                        .value
                        .as_str()
                        .ok_or_else(|| {
                            ConnectorDirectiveError::InvalidTypeForAttribute(
                                "string".to_string(),
                                "api".to_string(),
                            )
                        })?
                        .to_string()
                }
                "http" => http = Some(HTTPSourceField::from_argument(&argument.value)?),
                "selection" => {
                    selection = Some(
                        JSONSelection::parse(argument.value.as_str().ok_or_else(|| {
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
                other => {
                    return Err(ConnectorDirectiveError::UnknownAttributeForType(
                        other.to_string(),
                        SOURCE_TYPE_DIRECTIVE_NAME.to_string(),
                    ))
                }
            }
        }

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
        match &self.selection {
            Some(selection) => selection.clone().into(),
            None => vec![],
        }
    }

    pub(super) fn path_required_parameters(&self) -> Vec<String> {
        match &self.http {
            Some(http) => {
                if let Some(get) = &http.get {
                    get.required_parameters()
                } else if let Some(post) = &http.post {
                    post.required_parameters()
                } else if let Some(put) = &http.put {
                    put.required_parameters()
                } else if let Some(patch) = &http.patch {
                    patch.required_parameters()
                } else if let Some(delete) = &http.delete {
                    delete.required_parameters()
                } else {
                    vec![]
                }
            }
            None => vec![],
        }
    }
}

// TODO: impl tryfrom with XOR validation on methods
#[derive(Debug, Serialize)]
pub(super) struct HTTPSourceField {
    get: Option<URLPathTemplate>,
    post: Option<URLPathTemplate>,
    put: Option<URLPathTemplate>,
    patch: Option<URLPathTemplate>,
    delete: Option<URLPathTemplate>,
    body: Option<JSONSelection>,
}

impl HTTPSourceField {
    fn from_argument(argument: &Node<Value>) -> Result<Self, ConnectorDirectiveError> {
        let mut get = Default::default();
        let mut post = Default::default();
        let mut put = Default::default();
        let mut patch = Default::default();
        let mut delete = Default::default();
        let mut body = Default::default();

        let value = argument.as_object().ok_or_else(|| {
            ConnectorDirectiveError::InvalidTypeForAttribute(
                "object".to_string(),
                HTTP_ARGUMENT_NAME.to_string(),
            )
        })?;

        let mut methods_set = 0;

        for (name, value) in value.iter() {
            match name.as_str() {
                "GET" => {
                    get = Some(
                        URLPathTemplate::parse(value.as_str().expect("must be a string")).map_err(
                            |_| {
                                ConnectorDirectiveError::ParseError(
                                    "GET".to_string(),
                                    "URLPathTemplate".to_string(),
                                )
                            },
                        )?,
                    );
                    methods_set += 1;
                }
                "POST" => {
                    post = Some(
                        URLPathTemplate::parse(value.as_str().expect("must be a string")).map_err(
                            |_| {
                                ConnectorDirectiveError::ParseError(
                                    "POST".to_string(),
                                    "URLPathTemplate".to_string(),
                                )
                            },
                        )?,
                    );
                    methods_set += 1;
                }
                "PUT" => {
                    put = Some(
                        URLPathTemplate::parse(value.as_str().expect("must be a string")).map_err(
                            |_| {
                                ConnectorDirectiveError::ParseError(
                                    "PUT".to_string(),
                                    "URLPathTemplate".to_string(),
                                )
                            },
                        )?,
                    );
                    methods_set += 1;
                }
                "PATCH" => {
                    patch = Some(
                        URLPathTemplate::parse(value.as_str().expect("must be a string")).map_err(
                            |_| {
                                ConnectorDirectiveError::ParseError(
                                    "PATCH".to_string(),
                                    "URLPathTemplate".to_string(),
                                )
                            },
                        )?,
                    );
                    methods_set += 1;
                }
                "DELETE" => {
                    delete = Some(
                        URLPathTemplate::parse(value.as_str().expect("must be a string")).map_err(
                            |_| {
                                ConnectorDirectiveError::ParseError(
                                    "DELETE".to_string(),
                                    "URLPathTemplate".to_string(),
                                )
                            },
                        )?,
                    );
                    methods_set += 1;
                }
                "body" => {
                    body = Some(
                        JSONSelection::parse(value.as_str().ok_or_else(|| {
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
                other => {
                    return Err(ConnectorDirectiveError::UnknownAttributeForType(
                        other.to_string(),
                        HTTP_ARGUMENT_NAME.to_string(),
                    ))
                }
            }
        }

        if methods_set != 1 {
            return Err(ConnectorDirectiveError::RequiresExactlyOne(
                "HTTPSourceField".to_string(),
                "GET, POST, PUT, PATCH, DELETE".to_string(),
            ));
        }

        Ok(Self {
            get,
            post,
            put,
            patch,
            delete,
            body,
        })
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_json_snapshot;

    use super::*;
    use crate::spec::Schema;
    use crate::Configuration;

    #[test]
    fn test_source_api_directive_has_no_errors() {
        let partial_sdl = r#"
            schema
                @sourceAPI(
                    graph: "contacts"
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
                )

                @sourceAPI(
                    graph: "contacts"
                    name: "rest_notes"
                    http: { baseURL: "http://localhost:4002/notes/" }
                )

                @sourceAPI(
                    graph: "contacts"
                    name: "legacy_contacts"
                    http: { baseURL: "http://localhost:4002/legacy/contacts/" }
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
            directive @sourceAPI(name: String!, http: HTTPSourceAPI) on SCHEMA

            input HTTPSourceAPI {
                baseURL: String!
                default: Boolean
                headers: [HTTPHeaderMapping!]
            }

            input HTTPHeaderMapping {
                name: String!
                as: String
                value: String
            }

            schema
                @sourceAPI(
                    http: {
                        baseURL: "http://localhost:4002/contacts/"
                    }
                )
                @sourceAPI(
                    graph: "contacts"
                    name: "missing_base_url"
                    http: {
                        default: true
                    }
                )
                @sourceAPI(
                    graph: "contacts"
                    name: "missing_header_name"
                    http: {
                        baseURL: "http://localhost:4002/contacts/"
                        headers: [{ as: "missing mandatory name field" }]
                    }
                )
            { query: Query }
            "#;

        let partial_schema =
            Schema::parse(partial_sdl, &Configuration::fake_builder().build().unwrap()).unwrap();

        let schema_directives = partial_schema
            .definitions
            .schema_definition
            .directives
            .clone();

        // relies on source order
        let mut all_source_apis = schema_directives
            .iter()
            .map(SourceAPI::from_schema_directive)
            .collect::<Vec<_>>();

        let missing_name_error = all_source_apis.remove(0).unwrap_err();
        assert_eq!(
            ConnectorDirectiveError::MissingAttributeForType(
                "graph".to_string(),
                "sourceAPI".to_string()
            ),
            missing_name_error
        );

        let missing_base_url_error = all_source_apis.remove(0).unwrap_err();
        assert_eq!(
            ConnectorDirectiveError::MissingAttributeForType(
                "baseURL".to_string(),
                "http".to_string()
            ),
            missing_base_url_error
        );

        let missing_header_name_error = all_source_apis.remove(0).unwrap_err();
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
            @sourceType(graph: "contacts", api: "contacts", http: { GET: "/contacts/{contactId}" })
        {
            id: ID!
            name: String
        }
        type ValidSourceTypeDefaultHttp
            @sourceType(graph: "contacts", api: "contacts")
        {
            id: ID!
            name: String
        }
        "#;

        let partial_schema =
            Schema::parse(partial_sdl, &Configuration::fake_builder().build().unwrap()).unwrap();

        let valid_source_type = SourceType::from_directive(
            "ValidSourceType".to_string(),
            partial_schema
                .definitions
                .get_object("ValidSourceType")
                .unwrap()
                .directives
                .get(SOURCE_TYPE_DIRECTIVE_NAME)
                .unwrap(),
        )
        .unwrap();

        insta::with_settings!({sort_maps => true}, {
            assert_json_snapshot!(valid_source_type);
        });

        let valid_source_type_default_http = SourceType::from_directive(
            "ValidSourceTypeDefaultHttp".to_string(),
            partial_schema
                .definitions
                .get_object("ValidSourceTypeDefaultHttp")
                .unwrap()
                .directives
                .get(SOURCE_TYPE_DIRECTIVE_NAME)
                .unwrap(),
        )
        .unwrap();

        insta::with_settings!({sort_maps => true}, {
            assert_json_snapshot!(valid_source_type_default_http);
        });
    }

    #[test]
    fn test_valid_source_field() {
        let partial_sdl = r#"
        type Query {
            field: String
              @sourceField(
                graph: "contacts"s
                api: "contacts"
                http: { GET: "/contacts/{contactId}" }
                selection: "id name"
              )
        }
        "#;

        let partial_schema =
            Schema::parse(partial_sdl, &Configuration::fake_builder().build().unwrap()).unwrap();

        let valid_source_type = SourceField::from_directive(
            "Query".to_string(),
            "field".to_string(),
            "String".to_string(),
            partial_schema
                .definitions
                .get_object("Query")
                .unwrap()
                .fields
                .iter()
                .find(|(name, _)| *name == "field")
                .map(|(_, field)| field)
                .unwrap()
                .directives
                .get(SOURCE_FIELD_DIRECTIVE_NAME)
                .unwrap(),
        )
        .unwrap();

        insta::with_settings!({sort_maps => true}, {
            assert_json_snapshot!(valid_source_type, @r###"
            {
              "graph": "contacts",
              "parent_type_name": "Query",
              "field_name": "field",
              "output_type_name": "String",
              "api": "contacts",
              "http": {
                "get": "/contacts/{contactId}",
                "post": null,
                "put": null,
                "patch": null,
                "delete": null,
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
            "###);
        });

        let partial_sdl = r#"
        type Query {
            field: String
              @sourceField(
                graph: "contacts"
                api: "contacts"
                http: { GET: "/contacts/{contactId}", POST: "/x" }
                selection: "id name"
              )
        }
        "#;

        let partial_schema =
            Schema::parse(partial_sdl, &Configuration::fake_builder().build().unwrap()).unwrap();

        assert_eq!(
            SourceField::from_directive(
                "Query".to_string(),
                "field".to_string(),
                "String".to_string(),
                partial_schema
                    .definitions
                    .get_object("Query")
                    .unwrap()
                    .fields
                    .iter()
                    .find(|(name, _)| *name == "field")
                    .map(|(_, field)| field)
                    .unwrap()
                    .directives
                    .get(SOURCE_FIELD_DIRECTIVE_NAME)
                    .unwrap(),
            )
            .err(),
            Some(ConnectorDirectiveError::RequiresExactlyOne(
                "HTTPSourceField".to_string(),
                "GET, POST, PUT, PATCH, DELETE".to_string(),
            )),
        );

        let partial_sdl = r#"
        type Query {
            field: String
              @sourceField(
                graph: "contacts"
                api: "contacts"
                http: { body: "id name" }
                selection: "id name"
              )
        }
        "#;

        let partial_schema =
            Schema::parse(partial_sdl, &Configuration::fake_builder().build().unwrap()).unwrap();

        assert_eq!(
            SourceField::from_directive(
                "Query".to_string(),
                "field".to_string(),
                "String".to_string(),
                partial_schema
                    .definitions
                    .get_object("Query")
                    .unwrap()
                    .fields
                    .iter()
                    .find(|(name, _)| *name == "field")
                    .map(|(_, field)| field)
                    .unwrap()
                    .directives
                    .get(SOURCE_FIELD_DIRECTIVE_NAME)
                    .unwrap(),
            )
            .err(),
            Some(ConnectorDirectiveError::RequiresExactlyOne(
                "HTTPSourceField".to_string(),
                "GET, POST, PUT, PATCH, DELETE".to_string(),
            )),
        );
    }
}
