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

use super::selection_parser::ApplyTo;
use super::selection_parser::Selection as JSONSelection;
use super::url_path_parser::URLPathTemplate;
use super::Connector;
use crate::json_ext::Object;
use crate::plugins::connectors::connector::ConnectorType;
use crate::services::SubgraphRequest;
use crate::services::SubgraphResponse;
use crate::Context;

const REPRESENTATIONS_VAR: &str = "representations";
const ENTITIES: &str = "_entities";

#[derive(Debug)]
struct RequestParams {
    base_uri: http::Uri,
    path_template: URLPathTemplate,
    method: http::Method,
    body: Option<JSONSelection>,
    inputs: RequestInputs,
}

impl TryFrom<RequestParams> for http::Request<hyper::Body> {
    type Error = BoxError;

    fn try_from(params: RequestParams) -> Result<Self, Self::Error> {
        let inputs = params.inputs.merge();

        let uri = params.base_uri.clone();
        let path = params
            .path_template
            .generate_path(&inputs)
            .map_err(BoxError::from)?;
        let uri = append_path(&uri, &path)?;

        // TODO construct headers if necessary

        let body = if let Some(sel) = params.body {
            let (body, _) = sel.apply_to(&inputs);
            hyper::Body::from(serde_json::to_vec(&body)?)
        } else {
            hyper::Body::empty()
        };

        http::Request::builder()
            .method(params.method)
            .uri(uri)
            .header("content-type", "application/json") // TODO
            .body(body)
            .map_err(BoxError::from)
    }
}

/// Append a path and query to a URI. Uses the path from base URI (but will discard the query).
fn append_path(base_uri: &http::Uri, path: &str) -> anyhow::Result<http::Uri> {
    let parts = base_uri.clone().into_parts();
    let path_and_query = parts.path_and_query.clone();
    let new_path = format!(
        "{}{}",
        path_and_query
            .clone()
            .map(|p| p.path().to_string().clone())
            .unwrap_or_default(),
        path
    );
    let uri = http::Uri::builder()
        .authority(
            parts
                .authority
                .ok_or_else(|| anyhow::anyhow!("missing authority"))?
                .clone(),
        )
        .scheme(
            parts
                .scheme
                .ok_or_else(|| anyhow::anyhow!("missing scheme"))?
                .clone(),
        )
        .path_and_query(new_path)
        .build()?;
    Ok(uri)
}

#[derive(Debug)]
pub(crate) struct ResponseParams {
    key: ResponseKey,
    selection: JSONSelection,
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
    match connector.ty.as_ref() {
        ConnectorType::RootField(..) => {
            let parts = root_fields(&request, schema)?;
            Ok(request_params_to_requests(connector, parts)?)
        }
        ConnectorType::Entity(..) => {
            let parts = entities_from_request(&request, schema)?;
            Ok(request_params_to_requests(connector, parts)?)
        }
        ConnectorType::EntityField(..) => {
            let parts = entities_with_fields_from_request(&request, schema)?;
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
            let request = RequestParams {
                base_uri: connector.base_uri()?,
                path_template: connector.path_template().clone(),
                method: connector.method(),
                body: connector.body(),
                inputs,
            }
            .try_into()
            .map_err(|_| BoxError::from("invalid request"))?;

            let response_params = ResponseParams {
                key: response_key,
                selection: connector.json_selection(),
            };

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
) -> Result<Vec<(ResponseKey, RequestInputs)>, BoxError> {
    let query = request
        .subgraph_request
        .body()
        .query
        .clone()
        .ok_or_else(|| BoxError::from("missing query"))?;

    // Use the AST because the `_entities` field is not actually present in the supergraph
    let doc = apollo_compiler::ast::Document::parse(query, "op.graphql")
        .map_err(|_| "cannot parse operation document")?;

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
    debug_assert!(entities_field.name.as_str() == ENTITIES);

    let types_and_fields = entities_field
        .selection_set
        .iter()
        .map(|selection| match selection {
            apollo_compiler::ast::Selection::Field(f) => {
                // allow __typename outside of the type condition
                if f.response_name().to_string().starts_with("__") {
                    Ok(vec![])
                } else {
                    Err(BoxError::from("_entities selection can't be a field"))
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
    _connector: &Connector,
    responses: Vec<http::Response<hyper::Body>>,
) -> Result<SubgraphResponse, BoxError> {
    let mut data = serde_json_bytes::Map::new();
    let mut errors = Vec::new();

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

                let (res_data, _) = response_params.selection.apply_to(&json_data);

                // TODO __typename injection
                // TODO alias handling

                match response_params.key {
                    // add the response to the "data" using the root field name or alias
                    ResponseKey::RootField {
                        ref name,
                        ref typename,
                    } => {
                        let ResponseTypeName::Concrete(typename) = typename;

                        let res_data = match res_data.unwrap_or_default() {
                            Value::Object(mut res_data) => {
                                res_data.insert(
                                    "__typename".to_string(),
                                    Value::String(typename.clone().into()),
                                );
                                Some(Value::Object(res_data.clone()))
                            }
                            d => Some(d),
                        };

                        data.insert(name.clone(), res_data.unwrap_or_default());
                    }

                    // add the response to the "_entities" array at the right index
                    ResponseKey::Entity {
                        index,
                        ref typename,
                    } => {
                        let ResponseTypeName::Concrete(typename) = typename;

                        let res_data = match res_data.unwrap_or_default() {
                            Value::Object(mut res_data) => {
                                res_data.insert(
                                    "__typename".to_string(),
                                    Value::String(typename.clone().into()),
                                );
                                Some(Value::Object(res_data.clone()))
                            }
                            d => Some(d),
                        };

                        let entities = data.entry(ENTITIES).or_insert(Value::Array(vec![]));
                        entities
                            .as_array_mut()
                            .ok_or_else(|| BoxError::from("entities is not an array"))?
                            .insert(index, res_data.unwrap_or_default());
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
                            .entry(ENTITIES)
                            .or_insert(Value::Array(vec![]))
                            .as_array_mut()
                            .ok_or_else(|| BoxError::from("entities is not an array"))?;

                        match entities.get_mut(index) {
                            Some(Value::Object(entity)) => {
                                entity.insert(field_name.clone(), res_data.unwrap_or_default());
                            }
                            _ => {
                                let mut entity = serde_json_bytes::Map::new();
                                entity.insert("__typename", Value::String(typename.clone().into()));
                                entity.insert(field_name.clone(), res_data.unwrap_or_default());
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use apollo_compiler::Schema;
    use insta::assert_debug_snapshot;
    use tower::BoxError;

    use crate::plugins::connectors::connector::Connector;
    use crate::plugins::connectors::connector::ConnectorType;
    use crate::plugins::connectors::directives::SourceAPI;
    use crate::plugins::connectors::directives::SourceField;
    use crate::plugins::connectors::selection_parser::Selection as JSONSelection;
    use crate::Context;

    #[test]
    fn append_path_test() -> anyhow::Result<()> {
        assert_eq!(
            super::append_path(
                &http::Uri::builder()
                    .scheme("https")
                    .authority("localhost:8080")
                    .path_and_query("/v1")
                    .build()?,
                "/hello/42"
            )?,
            "https://localhost:8080/v1/hello/42"
        );

        Ok(())
    }

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
        let schema = Arc::new(
            Schema::parse_and_validate(r#"type Query { hello: String }"#, "test.graphql").unwrap(),
        );

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

        assert_debug_snapshot!(super::entities_with_fields_from_request(&req, schema.clone()).unwrap(), @r###"
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
              directive @join__graph(name: String) on ENUM_VALUE
              directive @join__schema(
                graph: join__Graph!
                directives: [join__Directive!] = []
              ) on SCHEMA
              directive @join__field(
                graph: join__Graph!
                directives: [join__Directive!] = []
              ) on FIELD_DEFINITION
              scalar join__Directive

              enum join__Graph { SUBGRAPH @join__graph(name: "subgraph") }
              schema @join__schema(
                graph: SUBGRAPH
                directives: [{ name: "sourceAPI" args: { name: "api", http: { baseURL: "https://api/v1" } }}]
              ) { query: Query }
              type Query {
                hello: String
                  @join__field(
                    graph: SUBGRAPH,
                    directives: [{ name: "sourceField", args: { api: "api", http: { GET: "/hello" }, selection: ".data" } }]
                  )
              }
            "#,
            "test.graphql",
        ).unwrap();

        let apis = SourceAPI::from_schema(&schema)?;
        let mut fields = SourceField::from_schema(&schema)?;
        let ty = ConnectorType::RootField(fields.swap_remove(0));

        let connector = Connector::new(
            "CONNECTOR_0".to_string(),
            apis.get("subgraph_api").expect("api exists").clone(),
            ty,
        );

        let requests = super::make_requests(req, &connector, Arc::new(schema)).unwrap();

        assert_debug_snapshot!(requests, @r###"
        [
            (
                Request {
                    method: GET,
                    uri: https://api/v1/hello,
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
                    selection: Path(
                        Path(
                            Field(
                                "data",
                            ),
                            Empty,
                        ),
                    ),
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
            selection: Path(
                Path(
                    Field(
                        "data",
                    ),
                    Empty,
                ),
            ),
        }
        "###);
        Ok(())
    }

    #[tokio::test]
    async fn handle_requests() -> Result<(), BoxError> {
        let schema = Schema::parse_and_validate(
            r#"
              directive @join__graph(name: String) on ENUM_VALUE
              directive @join__schema(
                graph: join__Graph!
                directives: [join__Directive!] = []
              ) on SCHEMA
              directive @join__field(
                graph: join__Graph!
                directives: [join__Directive!] = []
              ) on FIELD_DEFINITION
              scalar join__Directive

              enum join__Graph { SUBGRAPH @join__graph(name: "subgraph") }
              schema @join__schema(
                graph: SUBGRAPH
                directives: [{ name: "sourceAPI" args: { name: "api", http: { baseURL: "https://api/v1" } }}]
              ) { query: Query }
              type Query {
                hello: String
                  @join__field(
                    graph: SUBGRAPH,
                    directives: [{ name: "sourceField", args: { api: "api", http: { GET: "/hello" }, selection: ".data" } }]
                  )
              }
            "#,
            "test.graphql",
        ).unwrap();

        let apis = SourceAPI::from_schema(&schema)?;
        let mut fields = SourceField::from_schema(&schema)?;
        let ty = ConnectorType::RootField(fields.swap_remove(0));

        let connector = Connector::new(
            "CONNECTOR_0".to_string(),
            apis.get("subgraph_api").expect("api exists").clone(),
            ty,
        );

        let response1 = http::Response::builder()
            .extension(super::ResponseParams {
                key: super::ResponseKey::RootField {
                    name: "hello".to_string(),
                    typename: super::ResponseTypeName::Concrete("String".to_string()),
                },
                selection: JSONSelection::parse(".data").unwrap().1,
            })
            .body(hyper::Body::from(r#"{"data":"world"}"#))
            .expect("response builder");

        let response2 = http::Response::builder()
            .extension(super::ResponseParams {
                key: super::ResponseKey::RootField {
                    name: "hello2".to_string(),
                    typename: super::ResponseTypeName::Concrete("String".to_string()),
                },
                selection: JSONSelection::parse(".data").unwrap().1,
            })
            .body(hyper::Body::from(r#"{"data":"world"}"#))
            .expect("response builder");

        let res =
            super::handle_responses(Context::default(), &connector, vec![response1, response2])
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
