use apollo_compiler::ast;
use apollo_compiler::ast::Definition;
use apollo_compiler::ast::Selection;
use apollo_compiler::Node;
use displaydoc::Display;
use futures::StreamExt;
use serde::Serialize;
use serde_json_bytes::ByteString;
use serde_json_bytes::Map;
use serde_json_bytes::Value;
use thiserror::Error;

use super::request_response::HackEntityResponseKey;
use crate::graphql;
use crate::services::SubgraphRequest;
use crate::services::SubgraphResponse;
use crate::services::SupergraphRequest;

const ENTITIES: &str = "_entities";
const REPRESENTATIONS: &str = "representations";
const TYPENAME: &str = "__typename";

pub(super) fn map_request(
    mut outer: SubgraphRequest,
) -> Result<(SupergraphRequest, Option<HackEntityResponseKey>), OuterRequestError> {
    if let Some((body, hack_entity_response_key)) =
        convert_entities_field_to_magic_finder(outer.subgraph_request.body_mut())?
    {
        let mut supergraph_request = http::Request::builder()
            .method(outer.subgraph_request.method())
            .uri(outer.subgraph_request.uri())
            .version(outer.subgraph_request.version())
            .extension(hack_entity_response_key.clone()) // 2.
            .body(body)
            .map_err(|_todo| OuterRequestError::NewRequestBodyInvalid)?;
        *supergraph_request.headers_mut() = outer.subgraph_request.headers().clone();

        Ok((
            SupergraphRequest {
                supergraph_request,
                context: outer.context,
            },
            Some(hack_entity_response_key),
        ))
    } else {
        Ok((
            SupergraphRequest {
                supergraph_request: outer.subgraph_request,
                context: outer.context,
            },
            None,
        ))
    }
}

fn convert_entities_field_to_magic_finder(
    req: &mut graphql::Request,
) -> Result<Option<(graphql::Request, HackEntityResponseKey)>, OuterRequestError> {
    let query = req.query.clone().ok_or(OuterRequestError::QueryMissing)?;

    if let Some(entities_field) = find_entities_field(&query)? {
        let type_conditions = collect_type_conditions(&entities_field.selection_set);
        let typenames = collect_typenames_from_representations(&req.variables)?;

        // TODO check that all typenames are in type_conditions
        // TODO handle abstract types
        debug_assert!(type_conditions
            .iter()
            .all(|t| typenames.contains(&t.as_str())));

        let magic_finder_field = ast::Name::new(format!(
            "_{}_finder",
            typenames
                .first()
                .map(|s| (*s).to_string())
                .unwrap_or_default()
        ))
        .map_err(|_todo| OuterRequestError::InvalidMagicFinderFieldName)?;

        req.query = Some(query.clone().replace(ENTITIES, &magic_finder_field));

        Ok(Some((
            req.to_owned(),
            HackEntityResponseKey(magic_finder_field.to_string()),
        )))
    } else {
        Ok(None)
    }
}

fn find_entities_field(query: &str) -> Result<Option<Node<ast::Field>>, OuterRequestError> {
    let doc = apollo_compiler::ast::Document::parse(query.to_owned(), "op.graphql")
        .map_err(|_todo| OuterRequestError::QueryParseError)?;

    // Because this operation comes from the query planner, we can assume a single operation
    let op = doc
        .definitions
        .iter()
        .find_map(|def| match def {
            Definition::OperationDefinition(op) => Some(op),
            _ => None,
        })
        .ok_or(OuterRequestError::QueryMissing)?;

    Ok(op
        .selection_set
        .first()
        .and_then(|s| match s {
            Selection::Field(f) => {
                if f.name == ENTITIES {
                    Some(f)
                } else {
                    None
                }
            }
            _ => None,
        })
        .cloned())
}

fn collect_type_conditions(selection_set: &[Selection]) -> Vec<&ast::Name> {
    selection_set
        .iter()
        .filter_map(|s| match s {
            Selection::InlineFragment(f) => f.type_condition.as_ref(),
            _ => None,
        })
        .collect()
}

fn collect_typenames_from_representations(
    variables: &Map<ByteString, Value>,
) -> Result<Vec<&str>, OuterRequestError> {
    variables
        .get(REPRESENTATIONS)
        .ok_or(OuterRequestError::RepresentationsMissing)?
        .as_array()
        .ok_or(OuterRequestError::RepresentationsNotArray)?
        .iter()
        .map(|v| {
            v.as_object()
                .ok_or(OuterRequestError::ReperesentationNotObject)?
                .get(TYPENAME)
                .ok_or(OuterRequestError::TypenameMissing)?
                .as_str()
                .ok_or(OuterRequestError::TypenameNotString)
        })
        .collect::<Result<Vec<_>, _>>()
}

pub(super) async fn map_response(
    inner: crate::services::supergraph::Response,
    context: &crate::Context,
    hack_entity_response_key: Option<HackEntityResponseKey>,
) -> Result<SubgraphResponse, OuterResponseError> {
    if let Some(hack_entity_response_key) = hack_entity_response_key {
        let (parts, mut body) = inner.response.into_parts();

        // TODO multipart responses from connector subgraphs?
        let mut body = body
            .next()
            .await
            .ok_or(OuterResponseError::BadResponseBody)?;

        if let Some(ref mut data) = body.data {
            let data = data
                .as_object_mut()
                .ok_or(OuterResponseError::DataNotObject)?;

            if let Some(value) = data.remove(hack_entity_response_key.0.as_str()) {
                data.insert(ENTITIES, value);
            }
        }

        Ok(SubgraphResponse {
            response: http::Response::from_parts(parts, body),
            context: context.clone(),
        })
    } else {
        // TODO do I have to do this if I don't need to touch the response?
        let (parts, mut body) = inner.response.into_parts();
        let body = body
            .next()
            .await
            .ok_or(OuterResponseError::BadResponseBody)?;
        Ok(SubgraphResponse {
            response: http::Response::from_parts(parts, body),
            context: context.clone(),
        })
    }
}

#[derive(Error, Display, Debug, Clone, Serialize, Eq, PartialEq)]
pub(super) enum OuterRequestError {
    /// Query is missing
    QueryMissing,
    /// Cannot parse query
    QueryParseError,
    /// Representations variable missing
    RepresentationsMissing,
    /// Respresentations is not an array
    RepresentationsNotArray,
    /// Representation is not an object
    ReperesentationNotObject,
    /// Typename missing
    TypenameMissing,
    /// Typename is not a string
    TypenameNotString,
    /// Invalid magic finder field name
    InvalidMagicFinderFieldName,
    /// Could not construct new GraphQL request body
    NewRequestBodyInvalid,
}

#[derive(Error, Display, Debug, Clone, Serialize, Eq, PartialEq)]
pub(super) enum OuterResponseError {
    /// Could not read response body
    BadResponseBody,
    /// Data is not an object
    DataNotObject,
}
