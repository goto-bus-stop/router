#![allow(dead_code)]

use std::collections::HashMap;

use apollo_compiler::schema::{Component, Directive, EnumValueDefinition, Value};
use apollo_compiler::Node;

use super::selection_parser::Selection as JSONSelection;
use super::url_path_parser::Template as URLPathTemplate;

pub(super) const SOURCE_API_DIRECTIVE_NAME: &str = "source_api";
const HTTP_ARGUMENT_NAME: &str = "http";

#[derive(Debug)]
pub(super) struct SourceAPI {
    name: String,
    http: Option<HTTPSourceAPI>,
}

impl SourceAPI {
    pub(super) fn from_directive(name: String, component: &Component<EnumValueDefinition>) -> Self {
        let http = component
            .directives
            .0
            .iter()
            .find(|d| d.name == SOURCE_API_DIRECTIVE_NAME)
            .map(|directive| HTTPSourceAPI::from_directive(directive));
        Self { name, http }
    }
}

#[derive(Debug)]
pub(super) struct HTTPSourceAPI {
    base_url: String,
    default: Option<bool>,
    headers: Vec<HTTPHeaderMapping>,
}

impl HTTPSourceAPI {
    // todo: probably a result instead of unwraps ^^
    pub(super) fn from_directive(directive: &Node<Directive>) -> Self {
        let mut base_url = Default::default();
        let mut default = Default::default();
        let mut headers = Default::default();

        directive
            .arguments
            .iter()
            .find(|argument| argument.name == HTTP_ARGUMENT_NAME)
            // TODO: error handling plz ^^'
            .unwrap()
            .value
            .as_object()
            // TODO: error handling plz ^^'
            .unwrap()
            .iter()
            .for_each(|(name, node)| match name.as_str() {
                // TODO: error handling plz ^^'
                "base_url" => base_url = node.as_str().unwrap().to_string(),
                "default" => default = node.to_bool(),
                "headers" => headers = HTTPHeaderMapping::from_header_arguments(node),
                other => todo!("graceful error handling {other}"),
            });

        Self {
            base_url: base_url,
            default,
            headers,
        }
    }
}

#[derive(Debug)]
pub(super) struct HTTPHeaderMapping {
    name: String,
    r#as: Option<bool>,
    value: Option<String>,
}

impl HTTPHeaderMapping {
    // TODO: maybe a result?
    pub(super) fn from_header_arguments(argument: &Node<Value>) -> Vec<Self> {
        vec![]
    }
}

#[derive(Debug)]
pub(super) struct SourceType {
    api: String,
    http: Option<HTTPSourceType>,
    selection: Option<JSONSelection>,
    key_type_map: Option<KeyTypeMap>,
}

// TODO: impl tryfrom with XOR validation on methods
#[derive(Debug)]
pub(super) struct HTTPSourceType {
    get: Option<URLPathTemplate>,
    post: Option<URLPathTemplate>,
    headers: Vec<HTTPHeaderMapping>,
    body: Option<JSONSelection>,
}

#[derive(Debug)]
pub(super) struct KeyTypeMap {
    key: String,
    // Dictionary mapping possible __typename strings to values of the JSON
    // property named by key.
    type_map: HashMap<String, String>, // TODO: is this accurate?
}

#[derive(Debug)]
pub(super) struct SourceField {
    api: Option<String>,
    http: Option<HTTPSourceField>,
    selection: Option<JSONSelection>,
}

// TODO: impl tryfrom with XOR validation on methods
#[derive(Debug)]
pub(super) struct HTTPSourceField {
    get: Option<URLPathTemplate>,
    post: Option<URLPathTemplate>,
    put: Option<URLPathTemplate>,
    patch: Option<URLPathTemplate>,
    delete: Option<URLPathTemplate>,
    headers: Vec<HTTPHeaderMapping>,
    body: Option<JSONSelection>,
}
