use std::sync::Arc;

use apollo_compiler::ast::Definition;
use apollo_compiler::executable::Selection;
use apollo_compiler::validation::Valid;
use apollo_compiler::ExecutableDocument;
use apollo_compiler::Schema;
use serde_json_bytes::ByteString;
use serde_json_bytes::Map;
use serde_json_bytes::Value;
use tower::BoxError;

use super::connector::ConnectorKind;
use super::connector::ConnectorTransport;
use super::Connector;
use crate::json_ext::Object;
use crate::services::SubgraphRequest;
use crate::services::SubgraphResponse;
use crate::Context;

const REPRESENTATIONS_VAR: &str = "representations";
const ENTITIES: &str = "_entities";

#[derive(Clone)]
pub(super) struct HackEntityResponseKey(pub(super) String);

#[derive(Debug)]
pub(crate) struct ResponseParams {
    key: ResponseKey,
}

#[derive(Debug, Default)]
struct RequestInputs {
    arguments: Map<ByteString, Value>,
    parent: Map<ByteString, Value>,
}

impl RequestInputs {
    fn merge(&self) -> Value {
        let mut new = Map::new();
        new.extend(self.parent.clone());
        new.extend(self.arguments.clone());
        // if parent types are shadowed by arguments, we can use `this.` to access them
        new.insert("this", Value::Object(self.parent.clone()));
        Value::Object(new)
    }
}

#[derive(Clone, Debug)]
enum ResponseKey {
    RootField {
        name: String,
        typename: ResponseTypeName,
    },
    Entity {
        index: usize,
        typename: ResponseTypeName,
    },
    EntityField {
        index: usize,
        field_name: String,
        typename: ResponseTypeName,
    },
}

#[derive(Clone, Debug)]
enum ResponseTypeName {
    Concrete(String), // TODO Abstract(Vec<String>) with discriminator?
}

pub(super) fn make_requests(
    request: SubgraphRequest,
    connector: &Connector,
    schema: Arc<Valid<Schema>>,
) -> Result<Vec<(http::Request<hyper::Body>, ResponseParams)>, BoxError> {
    match connector.kind {
        ConnectorKind::RootField { .. } => {
            let parts = root_fields(&request, schema)?;
            Ok(request_params_to_requests(connector, parts)?)
        }
        ConnectorKind::Entity { .. } => {
            let parts = entities_from_request(&request, schema)?;
            Ok(request_params_to_requests(connector, parts)?)
        }
        ConnectorKind::EntityField { .. } => {
            let parts = entities_with_fields_from_request(&request, schema, connector)?;
            Ok(request_params_to_requests(connector, parts)?)
        }
    }
}

fn request_params_to_requests(
    connector: &Connector,
    from_request: Vec<(ResponseKey, RequestInputs)>,
) -> Result<Vec<(http::Request<hyper::Body>, ResponseParams)>, BoxError> {
    from_request
        .into_iter()
        .map(|(response_key, inputs)| {
            let inputs = inputs.merge();

            let request = match connector.transport {
                ConnectorTransport::HttpJson(ref transport) => transport.make_request(inputs)?,
            };

            let response_params = ResponseParams { key: response_key };

            Ok((request, response_params))
        })
        .collect::<Result<Vec<_>, _>>()
}

// --- ROOT FIELDS -------------------------------------------------------------

/// Given a query, find the root fields and return a list of requests.
/// The connector subgraph must have only a single root field, but it could be
/// used multiple times with aliases.
fn root_fields(
    request: &SubgraphRequest,
    schema: Arc<Valid<Schema>>,
) -> Result<Vec<(ResponseKey, RequestInputs)>, BoxError> {
    let query = request
        .subgraph_request
        .body()
        .query
        .clone()
        .ok_or_else(|| BoxError::from("missing query"))?;

    let doc = ExecutableDocument::parse(&schema, query, "op.graphql")
        .map_err(|_| "cannot parse operation document")?;

    let op = doc
        .get_operation(request.subgraph_request.body().operation_name.as_deref())
        .map_err(|_| anyhow::anyhow!("invalid operation"))?;

    op.selection_set
        .selections
        .iter()
        .map(|s| match s {
            Selection::Field(field) => {
                let response_key = ResponseKey::RootField {
                    name: field.response_key().to_string(),
                    typename: ResponseTypeName::Concrete(field.ty().inner_named_type().to_string()),
                };

                let arguments = graphql_utils::field_arguments_map(
                    field,
                    &request.subgraph_request.body().variables,
                )?;

                let request_inputs = RequestInputs {
                    arguments,
                    parent: Default::default(),
                };

                Ok((response_key, request_inputs))
            }

            // TODO if the client operation uses fragments, we'll probably need to handle that here
            Selection::FragmentSpread(_) => {
                Err(BoxError::from("root field fragment spread not supported"))
            }
            Selection::InlineFragment(_) => {
                Err(BoxError::from("root field inline fragment not supported"))
            }
        })
        .collect::<Result<Vec<_>, BoxError>>()
}

// --- ENTITIES ----------------------------------------------------------------

/// Given entity representations:
///
/// variables: { representations: [{ __typename: "User", id: "1" }] }
///
/// Return a list of requests to make, as well as the response key (index in list) for each.
fn entities_from_request(
    request: &SubgraphRequest,
    _schema: Arc<Valid<Schema>>,
) -> Result<Vec<(ResponseKey, RequestInputs)>, BoxError> {
    request
        .subgraph_request
        .body()
        .variables
        .get(REPRESENTATIONS_VAR)
        .ok_or_else(|| BoxError::from("missing representations variable"))?
        .as_array()
        .ok_or_else(|| BoxError::from("representations is not an array"))?
        .iter()
        .enumerate()
        .map(|(i, rep)| {
            // TODO abstract types?
            let typename = rep
                .as_object()
                .ok_or_else(|| BoxError::from("representation is not an object"))?
                .get("__typename")
                .ok_or_else(|| BoxError::from("representation is missing __typename"))?
                .as_str()
                .ok_or_else(|| BoxError::from("__typename is not a string"))?
                .to_string();
            Ok((
                ResponseKey::Entity {
                    index: i,
                    typename: ResponseTypeName::Concrete(typename),
                },
                RequestInputs {
                    arguments: Default::default(),
                    parent: rep
                        .as_object()
                        .ok_or_else(|| BoxError::from("representation is not an object"))?
                        .clone(),
                },
            ))
        })
        .collect::<Result<Vec<_>, _>>()
}

// --- ENTITY FIELDS -----------------------------------------------------------

/// Given an entities query and variables:
///
/// query: "{ _entities(representations: $representations) { ... on User { name } } }"
/// variables: { representations: [{ __typename: "User", id: "1" }] }
///
/// Return a list of requests to make, as well as the response key (index in list and name/alias of field) for each.
fn entities_with_fields_from_request(
    request: &SubgraphRequest,
    _schema: Arc<Valid<Schema>>,
    connector: &Connector,
) -> Result<Vec<(ResponseKey, RequestInputs)>, BoxError> {
    // TODO this is the fallback when using the magic finder field, which means
    // we won't have a type condition in the query
    let typename = match connector.kind {
        ConnectorKind::EntityField { ref type_name, .. } => type_name,
        _ => unreachable!(),
    };

    let query = request
        .subgraph_request
        .body()
        .query
        .clone()
        .ok_or_else(|| BoxError::from("missing query"))?;

    // Use the AST because the `_entities` field is not actually present in the supergraph
    let doc = apollo_compiler::ast::Document::parse(query, "op.graphql")
        .map_err(|_| "cannot parse operation document")?;

    // Assume a single operation (because this is from a query plan)
    let op = doc
        .definitions
        .iter()
        .find(|d| matches!(d, Definition::OperationDefinition(_)))
        .ok_or_else(|| BoxError::from("missing operation"))?;
    let entities_field = match op {
        apollo_compiler::ast::Definition::OperationDefinition(op) => {
            let selection = op
                .selection_set
                .first()
                .ok_or_else(|| BoxError::from("missing entities field"))?;
            match selection {
                apollo_compiler::ast::Selection::Field(f) => Ok(f),
                _ => Err(BoxError::from("must be a field")),
            }
        }
        _ => unreachable!(),
    }?;

    let types_and_fields = entities_field
        .selection_set
        .iter()
        .map(|selection| match selection {
            apollo_compiler::ast::Selection::Field(f) => {
                // allow __typename outside of the type condition
                if f.name == "__typename" {
                    Ok(vec![])
                } else {
                    // if we're using the magic finder field, the query planner doesn't use an inline fragment
                    // (because the output type in not an interface)
                    Ok(vec![(typename.to_string(), f)])
                }
            }

            apollo_compiler::ast::Selection::FragmentSpread(_) => Err(BoxError::from(
                "_entities selection can't be a named fragment",
            )),

            apollo_compiler::ast::Selection::InlineFragment(frag) => {
                let type_name = frag
                    .type_condition
                    .as_ref()
                    .ok_or_else(|| BoxError::from("missing type condition"))?;
                Ok(frag
                    .selection_set
                    .iter()
                    .map(|sel| {
                        let field = match sel {
                            apollo_compiler::ast::Selection::Field(f) => f,
                            apollo_compiler::ast::Selection::FragmentSpread(_) => todo!(),
                            apollo_compiler::ast::Selection::InlineFragment(_) => todo!(),
                        };
                        (type_name.to_string(), field)
                    })
                    .collect::<Vec<_>>())
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    let representations = request
        .subgraph_request
        .body()
        .variables
        .get(REPRESENTATIONS_VAR)
        .ok_or_else(|| BoxError::from("missing representations variable"))?
        .as_array()
        .ok_or_else(|| BoxError::from("representations is not an array"))?
        .iter()
        .enumerate()
        .collect::<Vec<_>>();

    // if we have multiple fields (because of aliases, we'll flatten that list)
    // and generate requests for each field/representation pair
    types_and_fields
        .into_iter()
        .flatten()
        .flat_map(|(typename, field)| {
            representations.iter().map(move |(i, representation)| {
                let arguments = graphql_utils::ast_field_arguments_map(
                    field,
                    &request.subgraph_request.body().variables,
                )?;

                Ok::<_, BoxError>((
                    ResponseKey::EntityField {
                        index: *i,
                        field_name: field.response_name().to_string(),
                        typename: ResponseTypeName::Concrete(typename.to_string()),
                    },
                    RequestInputs {
                        arguments,
                        parent: representation
                            .as_object()
                            .ok_or_else(|| BoxError::from("representation is not an object"))?
                            .clone(),
                    },
                ))
            })
        })
        .collect::<Result<Vec<_>, _>>()
}

// --- RESPONSES ---------------------------------------------------------------

pub(super) async fn handle_responses(
    context: Context,
    connector: &Connector,
    responses: Vec<http::Response<hyper::Body>>,
    hack_entity_response_key: Option<HackEntityResponseKey>,
) -> Result<SubgraphResponse, BoxError> {
    let mut data = serde_json_bytes::Map::new();
    let mut errors = Vec::new();

    let entity_response_key = hack_entity_response_key
        .map(|e| e.0.clone())
        .unwrap_or(ENTITIES.to_string());

    for response in responses {
        let (parts, body) = response.into_parts();

        let response_params = parts
            .extensions
            .get::<ResponseParams>()
            .ok_or_else(|| BoxError::from("missing response params"))?;

        match parts.status {
            hyper::StatusCode::OK => {
                let json_data: Value = serde_json::from_slice(
                    &hyper::body::to_bytes(body)
                        .await
                        .map_err(|_| "couldn't retrieve http response body")?,
                )
                .map_err(|_| "couldn't deserialize response body")?;

                let mut res_data = match connector.transport {
                    ConnectorTransport::HttpJson(ref transport) => {
                        transport.map_response(json_data)?
                    }
                };

                // TODO __typename injection
                // TODO alias handling

                match response_params.key {
                    // add the response to the "data" using the root field name or alias
                    ResponseKey::RootField {
                        ref name,
                        ref typename,
                    } => {
                        let ResponseTypeName::Concrete(typename) = typename;
                        inject_typename(&mut res_data, typename);

                        data.insert(name.clone(), res_data);
                    }

                    // add the response to the "_entities" array at the right index
                    ResponseKey::Entity {
                        index,
                        ref typename,
                    } => {
                        let ResponseTypeName::Concrete(typename) = typename;
                        inject_typename(&mut res_data, typename);

                        let entities = data
                            .entry(entity_response_key.clone())
                            .or_insert(Value::Array(vec![]));
                        entities
                            .as_array_mut()
                            .ok_or_else(|| BoxError::from("entities is not an array"))?
                            .insert(index, res_data);
                    }

                    // make an entity object and assign the response to the appropriate field or aliased field,
                    // then add the object to the _entities array at the right index (or add the field to an existing object)
                    ResponseKey::EntityField {
                        index,
                        ref field_name,
                        ref typename,
                    } => {
                        let ResponseTypeName::Concrete(typename) = typename;

                        let entities = data
                            .entry(entity_response_key.clone())
                            .or_insert(Value::Array(vec![]))
                            .as_array_mut()
                            .ok_or_else(|| BoxError::from("entities is not an array"))?;

                        match entities.get_mut(index) {
                            Some(Value::Object(entity)) => {
                                entity.insert(field_name.clone(), res_data);
                            }
                            _ => {
                                let mut entity = serde_json_bytes::Map::new();
                                entity.insert("__typename", Value::String(typename.clone().into()));
                                entity.insert(field_name.clone(), res_data);
                                entities.insert(index, Value::Object(entity));
                            }
                        };
                    }
                }
            }
            _ => {
                errors.push(
                    crate::graphql::Error::builder()
                        .message(format!("http error: {}", parts.status))
                        // todo path: ["_entities", i, "???"]
                        .extension_code(format!("{}", parts.status))
                        .build(),
                );
            }
        }
    }

    let response = SubgraphResponse::builder()
        .data(Value::Object(data))
        .errors(errors)
        .context(context)
        // .headers(parts.headers)
        .extensions(Object::default())
        .build();

    Ok(response)
}

fn inject_typename(data: &mut Value, typename: &str) {
    if let Value::Object(data) = data {
        data.insert(
            ByteString::from("__typename"),
            Value::String(ByteString::from(typename)),
        );
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use apollo_compiler::name;
    use apollo_compiler::Schema;
    use insta::assert_debug_snapshot;
    use tower::BoxError;

    use crate::plugins::connectors::connector::Connector;
    use crate::plugins::connectors::directives::HTTPSource;
    use crate::plugins::connectors::directives::HTTPSourceAPI;
    use crate::plugins::connectors::directives::SourceAPI;
    use crate::plugins::connectors::directives::SourceField;
    use crate::plugins::connectors::selection_parser::Selection as JSONSelection;
    use crate::plugins::connectors::url_path_parser::URLPathTemplate;
    use crate::Context;

    #[test]
    fn root_fields() -> anyhow::Result<()> {
        let schema = Arc::new(Schema::parse_and_validate(
            r#"
            scalar JSON
            type Query {
              a: String
              b(var: String): String
              c(var1: Int, var2: Boolean, var3: Float, var4: ID, var5: JSON, var6: [String], var7: String): String
            }
          "#,
            "test.graphql",
        ).unwrap());

        let req = crate::services::SubgraphRequest::fake_builder()
            .subgraph_request(
                http::Request::builder()
                    .body(
                        crate::graphql::Request::builder()
                            .query("query { a a2: a }")
                            .build(),
                    )
                    .expect("request builder"),
            )
            .build();

        assert_debug_snapshot!(super::root_fields(&req, schema.clone()), @r###"
        Ok(
            [
                (
                    RootField {
                        name: "a",
                        typename: Concrete(
                            "String",
                        ),
                    },
                    RequestInputs {
                        arguments: {},
                        parent: {},
                    },
                ),
                (
                    RootField {
                        name: "a2",
                        typename: Concrete(
                            "String",
                        ),
                    },
                    RequestInputs {
                        arguments: {},
                        parent: {},
                    },
                ),
            ],
        )
        "###);

        let req = crate::services::SubgraphRequest::fake_builder()
            .subgraph_request(
                http::Request::builder()
                    .body(
                        crate::graphql::Request::builder()
                            .query("query($var: String) { b(var: \"inline\") b2: b(var: $var) }")
                            .variables(
                                serde_json_bytes::json!({ "var": "variable" })
                                    .as_object()
                                    .unwrap()
                                    .clone(),
                            )
                            .build(),
                    )
                    .expect("request builder"),
            )
            .build();

        assert_debug_snapshot!(super::root_fields(&req, schema.clone()), @r###"
        Ok(
            [
                (
                    RootField {
                        name: "b",
                        typename: Concrete(
                            "String",
                        ),
                    },
                    RequestInputs {
                        arguments: {
                            "var": String(
                                "inline",
                            ),
                        },
                        parent: {},
                    },
                ),
                (
                    RootField {
                        name: "b2",
                        typename: Concrete(
                            "String",
                        ),
                    },
                    RequestInputs {
                        arguments: {
                            "var": String(
                                "variable",
                            ),
                        },
                        parent: {},
                    },
                ),
            ],
        )
        "###);

        let req = crate::services::SubgraphRequest::fake_builder()
            .subgraph_request(
                http::Request::builder()
                    .body(
                        crate::graphql::Request::builder()
                            .query(r#"
                              query(
                                $var1: Int, $var2: Bool, $var3: Float, $var4: ID, $var5: JSON, $var6: [String], $var7: String
                              ) {
                                c(var1: $var1, var2: $var2, var3: $var3, var4: $var4, var5: $var5, var6: $var6, var7: $var7)
                                c2: c(
                                  var1: 1,
                                  var2: true,
                                  var3: 0.9,
                                  var4: "123",
                                  var5: { a: 42 },
                                  var6: ["item"],
                                  var7: null
                                )
                              }
                            "#)
                            .variables(
                                serde_json_bytes::json!({
                                  "var1": 1, "var2": true, "var3": 0.9,
                                  "var4": "123", "var5": { "a": 42 }, "var6": ["item"],
                                  "var7": null
                                })
                                .as_object()
                                .unwrap()
                                .clone(),
                            )
                            .build(),
                    )
                    .expect("request builder"),
            )
            .build();

        assert_debug_snapshot!(super::root_fields(&req, schema.clone()), @r###"
        Ok(
            [
                (
                    RootField {
                        name: "c",
                        typename: Concrete(
                            "String",
                        ),
                    },
                    RequestInputs {
                        arguments: {
                            "var1": Number(1),
                            "var2": Bool(
                                true,
                            ),
                            "var3": Number(0.9),
                            "var4": String(
                                "123",
                            ),
                            "var5": Object({
                                "a": Number(42),
                            }),
                            "var6": Array([
                                String(
                                    "item",
                                ),
                            ]),
                            "var7": Null,
                        },
                        parent: {},
                    },
                ),
                (
                    RootField {
                        name: "c2",
                        typename: Concrete(
                            "String",
                        ),
                    },
                    RequestInputs {
                        arguments: {
                            "var1": Number(1),
                            "var2": Bool(
                                true,
                            ),
                            "var3": Number(0.9),
                            "var4": String(
                                "123",
                            ),
                            "var5": Object({
                                "a": Number(42),
                            }),
                            "var6": Array([
                                String(
                                    "item",
                                ),
                            ]),
                            "var7": Null,
                        },
                        parent: {},
                    },
                ),
            ],
        )
        "###);
        Ok(())
    }

    #[test]
    fn entities_with_fields_from_request() -> anyhow::Result<()> {
        let partial_sdl = r#"
        type Query {
          field: String
        }

        type Entity {
          field: String
        }
        "#;
        let schema = Arc::new(Schema::parse_and_validate(partial_sdl, "test.graphql").unwrap());

        let req = crate::services::SubgraphRequest::fake_builder()
            .subgraph_request(
                http::Request::builder()
                    .body(
                        crate::graphql::Request::builder()
                            .query(
                                r#"
                              query($representations: [_Any!]!) {
                                _entities(representations: $representations) {
                                  __typename
                                  ... on Entity {
                                    field
                                    alias: field
                                  }
                                }
                              }
                            "#,
                            )
                            .variables(
                                serde_json_bytes::json!({
                                  "representations": [
                                      { "__typename": "User", "id": "1" },
                                      { "__typename": "User", "id": "2" },
                                  ]
                                })
                                .as_object()
                                .unwrap()
                                .clone(),
                            )
                            .build(),
                    )
                    .expect("request builder"),
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
            parent_type_name: name!("Entity"),
            field_name: name!("field"),
            output_type_name: name!("String"),
            api: "API".to_string(),
            http: Some(HTTPSource {
                method: http::Method::GET,
                path_template: URLPathTemplate::parse("/path").unwrap(),
                body: None,
                headers: vec![],
            }),
            selection: JSONSelection::parse(".data").unwrap().1,
        };

        let connector =
            Connector::new_from_source_field("CONNECTOR_QUERY_FIELDB".to_string(), api, directive)
                .unwrap();

        assert_debug_snapshot!(super::entities_with_fields_from_request(&req, schema.clone(), &connector).unwrap(), @r###"
        [
            (
                EntityField {
                    index: 0,
                    field_name: "field",
                    typename: Concrete(
                        "Entity",
                    ),
                },
                RequestInputs {
                    arguments: {},
                    parent: {
                        "__typename": String(
                            "User",
                        ),
                        "id": String(
                            "1",
                        ),
                    },
                },
            ),
            (
                EntityField {
                    index: 1,
                    field_name: "field",
                    typename: Concrete(
                        "Entity",
                    ),
                },
                RequestInputs {
                    arguments: {},
                    parent: {
                        "__typename": String(
                            "User",
                        ),
                        "id": String(
                            "2",
                        ),
                    },
                },
            ),
            (
                EntityField {
                    index: 0,
                    field_name: "alias",
                    typename: Concrete(
                        "Entity",
                    ),
                },
                RequestInputs {
                    arguments: {},
                    parent: {
                        "__typename": String(
                            "User",
                        ),
                        "id": String(
                            "1",
                        ),
                    },
                },
            ),
            (
                EntityField {
                    index: 1,
                    field_name: "alias",
                    typename: Concrete(
                        "Entity",
                    ),
                },
                RequestInputs {
                    arguments: {},
                    parent: {
                        "__typename": String(
                            "User",
                        ),
                        "id": String(
                            "2",
                        ),
                    },
                },
            ),
        ]
        "###);
        Ok(())
    }

    #[test]
    fn make_requests() -> anyhow::Result<()> {
        let req = crate::services::SubgraphRequest::fake_builder()
            .subgraph_name("CONNECTOR_0")
            .subgraph_request(
                http::Request::builder()
                    .body(
                        crate::graphql::Request::builder()
                            .query("query { a: hello }")
                            .build(),
                    )
                    .expect("request builder"),
            )
            .build();

        let schema = Schema::parse_and_validate(
            r#"
              type Query {
                hello: String
              }
            "#,
            "test.graphql",
        )
        .unwrap();

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
            parent_type_name: name!("Query"),
            field_name: name!("field"),
            output_type_name: name!("String"),
            api: "API".to_string(),
            http: Some(HTTPSource {
                method: http::Method::GET,
                path_template: URLPathTemplate::parse("/path").unwrap(),
                body: None,
                headers: vec![],
            }),
            selection: JSONSelection::parse(".data").unwrap().1,
        };

        let connector =
            Connector::new_from_source_field("CONNECTOR_0".to_string(), api, directive).unwrap();

        let requests = super::make_requests(req, &connector, Arc::new(schema)).unwrap();

        assert_debug_snapshot!(requests, @r###"
        [
            (
                Request {
                    method: GET,
                    uri: http://localhost/api/path,
                    version: HTTP/1.1,
                    headers: {
                        "content-type": "application/json",
                    },
                    body: Body(
                        Empty,
                    ),
                },
                ResponseParams {
                    key: RootField {
                        name: "a",
                        typename: Concrete(
                            "String",
                        ),
                    },
                },
            ),
        ]
        "###);

        assert_debug_snapshot!(requests.first().unwrap().1, @r###"
        ResponseParams {
            key: RootField {
                name: "a",
                typename: Concrete(
                    "String",
                ),
            },
        }
        "###);
        Ok(())
    }

    #[tokio::test]
    async fn handle_requests() -> Result<(), BoxError> {
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
            parent_type_name: name!("Query"),
            field_name: name!("field"),
            output_type_name: name!("String"),
            api: "API".to_string(),
            http: Some(HTTPSource {
                method: http::Method::GET,
                path_template: URLPathTemplate::parse("/path").unwrap(),
                body: None,
                headers: vec![],
            }),
            selection: JSONSelection::parse(".data").unwrap().1,
        };

        let connector =
            Connector::new_from_source_field("CONNECTOR_QUERY_FIELDB".to_string(), api, directive)
                .unwrap();

        let response1 = http::Response::builder()
            .extension(super::ResponseParams {
                key: super::ResponseKey::RootField {
                    name: "hello".to_string(),
                    typename: super::ResponseTypeName::Concrete("String".to_string()),
                },
            })
            .body(hyper::Body::from(r#"{"data":"world"}"#))
            .expect("response builder");

        let response2 = http::Response::builder()
            .extension(super::ResponseParams {
                key: super::ResponseKey::RootField {
                    name: "hello2".to_string(),
                    typename: super::ResponseTypeName::Concrete("String".to_string()),
                },
            })
            .body(hyper::Body::from(r#"{"data":"world"}"#))
            .expect("response builder");

        let res = super::handle_responses(
            Context::default(),
            &connector,
            vec![response1, response2],
            None,
        )
        .await?;

        assert_debug_snapshot!(res.response.body(), @r###"
        Response {
            label: None,
            data: Some(
                Object({
                    "hello": String(
                        "world",
                    ),
                    "hello2": String(
                        "world",
                    ),
                }),
            ),
            path: None,
            errors: [],
            extensions: {},
            has_next: None,
            subscribed: None,
            created_at: None,
            incremental: [],
        }
        "###);
        Ok(())
    }
}

mod graphql_utils {
    use apollo_compiler::executable::Field;
    use apollo_compiler::schema::Value;
    use apollo_compiler::Node;
    use serde_json::Number;
    use serde_json_bytes::ByteString;
    use serde_json_bytes::Map;
    use serde_json_bytes::Value as JSONValue;
    use tower::BoxError;

    pub(super) fn field_arguments_map(
        field: &Node<Field>,
        variables: &Map<ByteString, JSONValue>,
    ) -> Result<Map<ByteString, JSONValue>, BoxError> {
        let mut arguments = Map::new();
        for argument in field.arguments.iter() {
            match &*argument.value {
                apollo_compiler::schema::Value::Variable(name) => {
                    arguments.insert(
                        argument.name.as_str(),
                        variables
                            .get(name.as_str())
                            .unwrap_or(&JSONValue::Null)
                            .clone(),
                    );
                }
                _ => {
                    arguments.insert(
                        argument.name.as_str(),
                        argument_value_to_json(&argument.value)?,
                    );
                }
            }
        }
        Ok(arguments)
    }

    pub(super) fn ast_field_arguments_map(
        field: &apollo_compiler::Node<apollo_compiler::ast::Field>,
        variables: &Map<ByteString, JSONValue>,
    ) -> Result<Map<ByteString, JSONValue>, BoxError> {
        let mut arguments = Map::new();
        for argument in field.arguments.iter() {
            match &*argument.value {
                apollo_compiler::schema::Value::Variable(name) => {
                    arguments.insert(
                        argument.name.as_str(),
                        variables
                            .get(name.as_str())
                            .unwrap_or(&JSONValue::Null)
                            .clone(),
                    );
                }
                _ => {
                    arguments.insert(
                        argument.name.as_str(),
                        argument_value_to_json(&argument.value)?,
                    );
                }
            }
        }
        Ok(arguments)
    }

    pub(super) fn argument_value_to_json(
        value: &apollo_compiler::ast::Value,
    ) -> Result<JSONValue, BoxError> {
        match value {
            Value::Null => Ok(JSONValue::Null),
            Value::Enum(e) => Ok(JSONValue::String(e.as_str().into())),
            Value::Variable(_) => Err(BoxError::from("variables not supported")),
            Value::String(s) => Ok(JSONValue::String(s.as_str().into())),
            Value::Float(f) => Ok(JSONValue::Number(
                Number::from_f64(
                    f.try_to_f64()
                        .map_err(|_| BoxError::from("try_to_f64 failed"))?,
                )
                .ok_or_else(|| BoxError::from("Number::from_f64 failed"))?,
            )),
            Value::Int(i) => Ok(JSONValue::Number(Number::from(
                i.try_to_i32().map_err(|_| "invalid int")?,
            ))),
            Value::Boolean(b) => Ok(JSONValue::Bool(*b)),
            Value::List(l) => Ok(JSONValue::Array(
                l.iter()
                    .map(|v| argument_value_to_json(v))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Value::Object(o) => Ok(JSONValue::Object(
                o.iter()
                    .map(|(k, v)| argument_value_to_json(v).map(|v| (k.as_str().into(), v)))
                    .collect::<Result<Map<_, _>, _>>()?,
            )),
        }
    }
}
