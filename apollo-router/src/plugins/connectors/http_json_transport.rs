use apollo_compiler::ast::Selection as GraphQLSelection;
use displaydoc::Display;
use serde_json_bytes::Value;
use thiserror::Error;
use url::Url;

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
            .uri(self.make_uri(&inputs)?.as_str())
            .header("content-type", "application/json")
            .body(body)
            .map_err(HttpJsonTransportError::InvalidNewRequest)?;

        // TODO: Add headers

        Ok(request)
    }

    fn make_uri(&self, inputs: &Value) -> Result<url::Url, HttpJsonTransportError> {
        let path = self
            .path_template
            .generate_path(inputs)
            .map_err(HttpJsonTransportError::PathGenerationError)?;
        append_path(self.base_uri.clone(), &path)
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

/// Append a path and query to a URI. Uses the path from base URI (but will discard the query).
fn append_path(base_uri: Url, path: &str) -> Result<Url, HttpJsonTransportError> {
    // we will need to work on path segments, and on query parameters.
    // the first thing we need to do is parse the path so we have APIs to reason with both:
    let path_uri: Url = Url::options()
        .base_url(Some(&base_uri))
        .parse(path)
        .map_err(HttpJsonTransportError::InvalidPath)?;
    // get query parameters from both base_uri and path
    let base_uri_query_pairs =
        (!base_uri.query().unwrap_or_default().is_empty()).then(|| base_uri.query_pairs());
    let path_uri_query_pairs =
        (!path_uri.query().unwrap_or_default().is_empty()).then(|| path_uri.query_pairs());

    let mut res = base_uri.clone();

    // append segments
    {
        // Path segments being none indicates the base_uri cannot be a base URL.
        // This means the schema is invalid.
        let segments = base_uri.path_segments().ok_or_else(|| {
            HttpJsonTransportError::InvalidBaseUri(
                url::ParseError::RelativeUrlWithCannotBeABaseBase,
            )
        })?;

        // Ok this one is a bit tricky.
        // Here we're trying to only append segments that are not empty, to avoid `//`
        let mut res_segments = res.path_segments_mut().map_err(|_| {
            HttpJsonTransportError::InvalidBaseUri(
                url::ParseError::RelativeUrlWithCannotBeABaseBase,
            )
        })?;
        res_segments
            .clear()
            .extend(segments.filter(|segment| !segment.is_empty()))
            .extend(
                path_uri
                    .path_segments()
                    .ok_or_else(|| {
                        HttpJsonTransportError::InvalidPath(
                            url::ParseError::RelativeUrlWithCannotBeABaseBase,
                        )
                    })?
                    .filter(|segment| !segment.is_empty()),
            );
    }
    // Calling clear on query_pairs will cause a `?` to be appended.
    // We only want to do it if necessary
    if base_uri_query_pairs.is_some() || path_uri_query_pairs.is_some() {
        res.query_pairs_mut().clear();
    }
    if let Some(pairs) = base_uri_query_pairs {
        res.query_pairs_mut().extend_pairs(pairs);
    }
    if let Some(pairs) = path_uri_query_pairs {
        res.query_pairs_mut().extend_pairs(pairs);
    }

    Ok(res)
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
    /// Invalid Path for directive
    InvalidPath(url::ParseError),
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

#[cfg(test)]
mod tests {
    #[test]
    fn append_path_test() {
        assert_eq!(
            super::append_path("https://localhost:8080/v1".parse().unwrap(), "/hello/42")
                .unwrap()
                .as_str(),
            "https://localhost:8080/v1/hello/42"
        );
    }

    #[test]
    fn append_path_test_with_trailing_slash() {
        assert_eq!(
            super::append_path("https://localhost:8080/".parse().unwrap(), "/hello/42")
                .unwrap()
                .as_str(),
            "https://localhost:8080/hello/42"
        );
    }

    #[test]
    fn append_path_test_with_trailing_slash_and_base_path() {
        assert_eq!(
            super::append_path("https://localhost:8080/v1/".parse().unwrap(), "/hello/42")
                .unwrap()
                .as_str(),
            "https://localhost:8080/v1/hello/42"
        );
    }
    #[test]
    fn append_path_test_with_and_base_path_and_params() {
        assert_eq!(
            super::append_path(
                "https://localhost:8080/v1?foo=bar".parse().unwrap(),
                "/hello/42"
            )
            .unwrap()
            .as_str(),
            "https://localhost:8080/v1/hello/42?foo=bar"
        );
    }
    #[test]
    fn append_path_test_with_and_base_path_and_trailing_slash_and_params() {
        assert_eq!(
            super::append_path(
                "https://localhost:8080/v1/?foo=bar".parse().unwrap(),
                "/hello/42"
            )
            .unwrap()
            .as_str(),
            "https://localhost:8080/v1/hello/42?foo=bar"
        );
    }
}
