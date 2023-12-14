use apollo_compiler::ast::Selection as GraphQLSelection;
use displaydoc::Display;
use serde_json_bytes::Value;
use thiserror::Error;

use super::directives::HTTPHeaderMapping;
use super::directives::HTTPSource;
use super::directives::SourceAPI;
use super::directives::SourceField;
use super::directives::SourceType;
use super::join_spec_helpers::parameters_to_selection_set;
use super::join_spec_helpers::selection_set_to_string;
use super::selection_parser::ApplyTo;
use super::selection_parser::Selection as JSONSelection;
use super::url_path_parser::URLPathTemplate;

#[derive(Clone, Debug)]
pub(super) struct HttpJsonTransport {
    pub(super) base_uri: url::Url,
    pub(super) method: http::Method,
    #[allow(dead_code)]
    pub(super) headers: Vec<HttpHeader>,

    pub(super) path_template: URLPathTemplate,
    pub(super) response_mapper: JSONSelection,
    pub(super) body_mapper: Option<JSONSelection>,
}

impl HttpJsonTransport {
    pub(super) fn from_source_type(
        api: &SourceAPI,
        directive: &SourceType,
    ) -> Result<Self, HttpJsonTransportError> {
        let api_http = api
            .http
            .as_ref()
            .ok_or(HttpJsonTransportError::MissingHttp)?;
        let http = directive
            .http
            .as_ref()
            .ok_or(HttpJsonTransportError::MissingHttp)?;

        Ok(Self {
            base_uri: api_http
                .base_url
                .parse()
                .map_err(HttpJsonTransportError::InvalidBaseUri)?,
            method: http.method.clone(),
            headers: HttpHeader::from_directive(&http.headers)?,
            path_template: http.path_template.clone(),
            response_mapper: directive.selection.clone(),
            body_mapper: http.body.clone(),
        })
    }

    pub(super) fn from_source_field(
        api: &SourceAPI,
        directive: &SourceField,
    ) -> Result<Self, HttpJsonTransportError> {
        let api_http = api
            .http
            .as_ref()
            .ok_or(HttpJsonTransportError::MissingHttp)?;
        let http = directive
            .http
            .as_ref()
            .ok_or(HttpJsonTransportError::MissingHttp)?;

        Ok(Self {
            base_uri: api_http
                .base_url
                .parse()
                .map_err(HttpJsonTransportError::InvalidBaseUri)?,
            method: http.method.clone(),
            headers: vec![], // TODO HttpHeader::from_directive(&http.headers)?,
            path_template: http.path_template.clone(),
            response_mapper: directive.selection.clone(),
            body_mapper: http.body.clone(),
        })
    }

    pub(super) fn make_request(
        &self,
        inputs: Value,
    ) -> Result<http::Request<hyper::Body>, HttpJsonTransportError> {
        let body = if let Some(ref sel) = self.body_mapper {
            let (body, _todo) = sel.apply_to(&inputs);
            hyper::Body::from(
                serde_json::to_vec(&body).map_err(HttpJsonTransportError::BodySerialization)?,
            )
        } else {
            hyper::Body::empty()
        };

        let request = http::Request::builder()
            .method(self.method.clone())
            .uri(self.make_uri(&inputs)?)
            .header("content-type", "application/json")
            .body(body)
            .map_err(HttpJsonTransportError::InvalidNewRequest)?;

        // TODO: Add headers

        Ok(request)
    }

    fn make_uri(&self, inputs: &Value) -> Result<http::Uri, HttpJsonTransportError> {
        let path: http::Uri = self
            .path_template
            .generate_path(inputs)
            .map_err(HttpJsonTransportError::PathGenerationError)?
            .parse()
            .map_err(|_| HttpJsonTransportError::NewUriError(None))?;

        let path_and_query = path
            .into_parts()
            .path_and_query
            .ok_or(HttpJsonTransportError::NewUriError(None))?;

        let mut url = self.base_uri.clone();

        let base_path = self.base_uri.path().trim_end_matches('/');
        let path = path_and_query.path().trim_start_matches('/');
        url.set_path([base_path, path].join("/").as_str());

        let query = path_and_query.query();
        url.set_query(query);

        url.to_string()
            .parse()
            .map_err(|e| HttpJsonTransportError::NewUriError(Some(e)))
    }

    pub(super) fn map_response(&self, response: Value) -> Result<Value, HttpJsonTransportError> {
        let (mapped, _todo) = self.response_mapper.apply_to(&response);
        Ok(mapped.unwrap_or(Value::Null))
    }

    // TODO incorporate body selection too?
    pub(super) fn input_selection_from_http_source(
        http: &HTTPSource,
    ) -> (Vec<GraphQLSelection>, String) {
        let required = http.path_template.required_parameters();
        let selection_set = parameters_to_selection_set(&required);
        let selection_set_string = selection_set_to_string(&selection_set);
        (selection_set, selection_set_string)
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub(super) enum HttpHeader {
    Rename {
        original_name: String,
        new_name: String,
    },
    Inject {
        name: String,
        value: String,
    },
}

impl HttpHeader {
    fn from_directive(
        header_map: &[HTTPHeaderMapping],
    ) -> Result<Vec<Self>, HttpJsonTransportError> {
        header_map
            .iter()
            .map(|mapping| {
                if let Some(new_name) = &mapping.r#as {
                    Ok(Self::Rename {
                        original_name: mapping.name.clone(),
                        new_name: new_name.clone(),
                    })
                } else if let Some(value) = &mapping.value {
                    Ok(Self::Inject {
                        name: mapping.name.clone(),
                        value: value.clone(),
                    })
                } else {
                    Err(HttpJsonTransportError::InvalidHeaderMapping)
                }
            })
            .collect::<Result<Vec<_>, _>>()
    }
}

#[derive(Error, Display, Debug)]
pub(super) enum HttpJsonTransportError {
    /// HTTP parameters missing
    MissingHttp,
    /// Invalid Base URI on API
    InvalidBaseUri(#[from] url::ParseError),
    /// Invalid HTTP header mapping
    InvalidHeaderMapping,
    /// Error building URI
    NewUriError(#[from] Option<http::uri::InvalidUri>),
    /// Could not generate path from inputs
    PathGenerationError(String),
    /// Could not generate HTTP request
    InvalidNewRequest(#[source] http::Error),
    /// Could not serialize body
    BodySerialization(#[source] serde_json::Error),
}
