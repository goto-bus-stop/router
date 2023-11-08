#![allow(dead_code)]

use std::collections::HashMap;

use super::selection_parser::Selection as JSONSelection;
use super::url_path_parser::Template as URLPathTemplate;

#[derive(Debug)]
pub(super) struct SourceApi {
    name: String,
    http: HTTPSourceAPI,
}

impl SourceApi {
    pub(super) fn new(name: String) -> Self {
        Self {
            name,
            http: HTTPSourceAPI {
                base_url: "".to_string(),
                default: None,
                headers: vec![],
            },
        }
    }
}

#[derive(Debug)]
pub(super) struct HTTPSourceAPI {
    base_url: String,
    default: Option<bool>,
    headers: Vec<HTTPHeaderMapping>,
}

#[derive(Debug)]
pub(super) struct HTTPHeaderMapping {
    name: String,
    r#as: Option<bool>,
    value: Option<String>,
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
