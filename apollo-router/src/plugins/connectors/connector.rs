use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use anyhow::anyhow;
use anyhow::bail;
use apollo_compiler::ast;
use apollo_compiler::ast::Selection;
use apollo_compiler::schema::ExtendedType;
use apollo_compiler::schema::FieldDefinition;
use apollo_compiler::schema::InputValueDefinition;
use apollo_compiler::schema::Name;
use apollo_compiler::validation::Valid;
use apollo_compiler::Node;
use apollo_compiler::Schema;
use tower::BoxError;

use super::directives::SourceAPI;
use super::directives::SourceField;
use super::directives::SourceType;
use super::join_spec_helpers::add_entities_field;
use super::join_spec_helpers::add_input_join_field_directive;
use super::join_spec_helpers::add_join_field_directive;
use super::join_spec_helpers::add_join_type_directive;
use super::join_spec_helpers::make_any_scalar;
use super::join_spec_helpers::parameters_to_selection_set;
use super::join_spec_helpers::selection_set_to_string;
use super::join_spec_helpers::Key;
use super::request_response::handle_responses;
use super::request_response::make_requests;
use super::request_response::ResponseParams;
use super::selection_parser::Selection as JSONSelection;
use super::url_path_parser::URLPathTemplate;
use crate::services::SubgraphRequest;
use crate::services::SubgraphResponse;
use crate::Context;

/// A connector wraps the API and type/field connector metadata and has
/// a unique name used to construct a "subgraph" in the inner supergraph
#[derive(Clone, Debug)]
pub(crate) struct Connector {
    /// Internal name used to construct "subgraphs" in the inner supergraph
    pub(super) name: String,
    api: Arc<SourceAPI>,
    pub(super) ty: Arc<ConnectorType>,
}

#[derive(Debug)]
pub(super) enum ConnectorType {
    Entity(SourceType),
    RootField(SourceField),
    EntityField(SourceField),
}

/// The list of the subgraph names that should use the inner query planner
/// instead of making a normal subgraph request.
pub(crate) fn connector_subgraph_names(connectors: &HashMap<String, Connector>) -> HashSet<String> {
    connectors
        .values()
        .map(|c| c.outer_subgraph_name())
        .collect()
}

impl Connector {
    pub(super) fn new(name: String, api: SourceAPI, ty: ConnectorType) -> Self {
        Self {
            name,
            api: Arc::new(api),
            ty: Arc::new(ty),
        }
    }

    /// Generate a map of connectors with unique names
    pub(crate) fn from_schema(schema: &Schema) -> anyhow::Result<HashMap<String, Self>> {
        let apis = SourceAPI::from_schema(schema)?;
        let types = SourceType::from_schema(schema)?;
        let fields = SourceField::from_schema(schema)?;

        let mut connectors = HashMap::new();

        for (i, (type_name, directives)) in types.into_iter().enumerate() {
            let connector_name = format!("CONNECTOR_{}_{}", type_name, i).to_uppercase();

            for directive in directives {
                connectors.insert(
                    connector_name.clone(),
                    Connector {
                        name: connector_name.clone(),
                        api: Arc::new(
                            apis.get(&directive.api_name())
                                .ok_or(anyhow!("missing API {}", directive.api_name()))? // TODO support default
                                .clone(),
                        ),
                        ty: Arc::new(ConnectorType::Entity(directive)),
                    },
                );
            }
        }

        for (i, directive) in fields.into_iter().enumerate() {
            let connector_name = format!(
                "CONNECTOR_{}_{}_{}",
                directive.parent_type_name, directive.field_name, i
            )
            .to_uppercase();

            let api_name = directive.api_name().clone();

            // TODO better way to determine parent is Query or Mutation
            let ty = if directive.parent_type_name == "Query"
                || directive.parent_type_name == "Mutation"
            {
                ConnectorType::RootField(directive)
            } else {
                ConnectorType::EntityField(directive)
            };

            connectors.insert(
                connector_name.clone(),
                Connector {
                    name: connector_name,
                    api: Arc::new(
                        apis.get(&api_name)
                            .ok_or(anyhow!("missing API {}", api_name))? // TODO support default
                            .clone(),
                    ),
                    ty: Arc::new(ty),
                },
            );
        }

        Ok(connectors)
    }

    /// Generate a list of changes to apply to the new schame
    pub(super) fn changes(&self, schema: &Schema) -> anyhow::Result<Vec<Change>> {
        let graph = self.name.clone();
        match &*self.ty {
            // Root fields: add the parent type and the field, then recursively
            // add the selections
            ConnectorType::RootField(field) => {
                let mut changes = vec![
                    Change::Type {
                        name: ast::Name::new(field.parent_type_name.clone())?,
                        graph: graph.clone(),
                        key: Key::None,
                    },
                    Change::Field {
                        type_name: ast::Name::new(field.parent_type_name.clone())?,
                        field_name: ast::Name::new(field.field_name.clone())?,
                        graph: graph.clone(),
                    },
                ];

                changes.extend(recurse_selection(
                    graph.clone(),
                    schema,
                    field.output_type_name.clone(),
                    schema
                        .types
                        .get(field.output_type_name.as_str())
                        .ok_or(anyhow!("missing type"))?,
                    &field.selections(),
                )?);

                let field_def = schema
                    .type_field(&field.parent_type_name, &field.field_name)
                    .map_err(|_| anyhow::anyhow!("missing field"))?;

                for arg in field_def.arguments.iter() {
                    changes.extend(recurse_inputs(graph.clone(), schema, arg)?);
                }

                Ok(changes)
            }
            // Entity: add the type with appropriate keys, add a finder field,
            // recursively add the selections, and recursively add the key fields if necessary
            ConnectorType::Entity(ty) => {
                let keys = ty.path_required_parameters();
                let key_selection = parameters_to_selection_set(&keys);
                let key_string = selection_set_to_string(&key_selection);

                let mut changes = vec![
                    Change::Type {
                        name: ast::Name::new(ty.type_name.clone())?,
                        graph: graph.clone(),
                        key: Key::Resolvable(key_string),
                    },
                    Change::MagicFinder {
                        type_name: ast::Name::new(ty.type_name.clone())?,
                        graph: graph.clone(),
                    },
                ];

                changes.extend(recurse_selection(
                    graph.clone(),
                    schema,
                    ty.type_name.clone(),
                    schema
                        .types
                        .get(ty.type_name.as_str())
                        .ok_or(anyhow!("missing type"))?,
                    &ty.selections(),
                )?);

                // TODO need a test with a nested composite key
                // TODO mark key fields as external if necessary
                changes.extend(recurse_selection(
                    graph,
                    schema,
                    ty.type_name.clone(),
                    schema
                        .types
                        .get(ty.type_name.as_str())
                        .ok_or(anyhow!("missing type"))?,
                    &key_selection,
                )?);

                Ok(changes)
            }
            // Entity field: add the parent entity type with appropriate keys,
            // add the field itself, add a finder field, recursively add the
            // selections, and recursively add the key fields if necessary
            ConnectorType::EntityField(field) => {
                let keys = field.path_required_parameters();
                let key_selection = parameters_to_selection_set(&keys);
                let key_string = selection_set_to_string(&key_selection);

                let mut changes = vec![
                    Change::Type {
                        name: ast::Name::new(field.parent_type_name.clone())?,
                        graph: graph.clone(),
                        key: Key::Resolvable(key_string),
                    },
                    Change::Field {
                        type_name: ast::Name::new(field.parent_type_name.clone())?,
                        field_name: ast::Name::new(field.field_name.clone())?,
                        graph: graph.clone(),
                    },
                    Change::MagicFinder {
                        type_name: ast::Name::new(field.parent_type_name.clone())?,
                        graph: graph.clone(),
                    },
                ];

                changes.extend(recurse_selection(
                    graph.clone(),
                    schema,
                    field.output_type_name.clone(),
                    schema
                        .types
                        .get(field.output_type_name.as_str())
                        .ok_or(anyhow!("missing type"))?,
                    &field.selections(),
                )?);

                // TODO need a test with a nested composite key
                // TODO mark key fields as external if necessary
                changes.extend(recurse_selection(
                    graph.clone(),
                    schema,
                    field.parent_type_name.clone(), // key fields are on the parent type, not the output type
                    schema
                        .types
                        .get(field.parent_type_name.as_str())
                        .ok_or(anyhow!("missing type"))?,
                    &key_selection,
                )?);

                let field_def = schema
                    .type_field(&field.parent_type_name, &field.field_name)
                    .map_err(|_| anyhow::anyhow!("missing field"))?;

                for arg in field_def.arguments.iter() {
                    changes.extend(recurse_inputs(graph.clone(), schema, arg)?);
                }

                Ok(changes)
            }
        }
    }

    pub(super) fn outer_subgraph_name(&self) -> String {
        match &*self.ty {
            ConnectorType::Entity(ref ty) => ty.graph.clone(),
            ConnectorType::RootField(ref field) => field.graph.clone(),
            ConnectorType::EntityField(ref field) => field.graph.clone(),
        }
    }

    pub(crate) fn name(&self) -> &str {
        self.name.as_str()
    }

    pub(super) fn base_uri(&self) -> Result<http::Uri, http::uri::InvalidUri> {
        self.api.base_uri()
    }

    pub(super) fn path_template(&self) -> &URLPathTemplate {
        match self.ty.as_ref() {
            ConnectorType::Entity(source_type) => source_type.path_template(),
            ConnectorType::RootField(source_field) => source_field.path_template(),
            ConnectorType::EntityField(source_field) => source_field.path_template(),
        }
    }

    pub(super) fn method(&self) -> http::Method {
        match self.ty.as_ref() {
            ConnectorType::Entity(source_type) => source_type.method().clone(),
            ConnectorType::RootField(source_field) => source_field.method().clone(),
            ConnectorType::EntityField(source_field) => source_field.method().clone(),
        }
    }

    pub(super) fn json_selection(&self) -> JSONSelection {
        match self.ty.as_ref() {
            ConnectorType::Entity(source_type) => source_type.selection.clone(),
            ConnectorType::RootField(source_field) => source_field.selection.clone(),
            ConnectorType::EntityField(source_field) => source_field.selection.clone(),
        }
    }

    pub(super) fn body(&self) -> Option<JSONSelection> {
        match self.ty.as_ref() {
            ConnectorType::Entity(source_type) => source_type.body(),
            ConnectorType::RootField(source_field) => source_field.body(),
            ConnectorType::EntityField(source_field) => source_field.body(),
        }
    }

    pub(crate) fn create_requests(
        &self,
        subgraph_request: SubgraphRequest,
        schema: Arc<Valid<Schema>>,
    ) -> Result<Vec<(http::Request<hyper::Body>, ResponseParams)>, BoxError> {
        make_requests(subgraph_request, self, schema.clone())
    }

    pub(crate) async fn map_http_responses(
        &self,
        responses: Vec<http::Response<hyper::Body>>,
        context: Context,
    ) -> Result<SubgraphResponse, BoxError> {
        handle_responses(context, self, responses).await
    }
}

/// A "change" is a unit of work that can be applied to a schema. Each connector
/// produces a set of changes to include types, fields, and applies
/// `@join__` directives appropriately so that the query planner can extract
/// subgraphs for each connector.
#[derive(Debug)]
pub(super) enum Change {
    /// Include a type in the schema and add the `@join__type` directive
    Type { name: Name, graph: String, key: Key },
    /// Include a field on a type in the schema and add the `@join__field` directive
    /// TODO: currently assumes that the type already exists (order matters!)
    Field {
        type_name: Name,
        field_name: Name,
        graph: String,
    },
    InputField {
        type_name: Name,
        field_name: Name,
        graph: String,
    },
    /// Add a special field to Query that we can use instead of `_entities`
    MagicFinder { type_name: Name, graph: String },
}

impl Change {
    /// Apply this change to a schema, generating or modifying types and fields
    pub(super) fn apply_to(
        &self,
        original_schema: &Schema,
        schema: &mut Schema,
    ) -> anyhow::Result<()> {
        match self {
            Change::Type { name, graph, key } => {
                let ty = upsert_type(original_schema, schema, name)?;
                add_join_type_directive(ty, graph, key);
            }
            Change::Field {
                type_name,
                field_name,
                graph,
            } => {
                let field = upsert_field(original_schema, schema, type_name, field_name)?;
                add_join_field_directive(field, graph)?;
            }
            Change::InputField {
                type_name,
                field_name,
                graph,
            } => {
                let field = upsert_input_field(original_schema, schema, type_name, field_name)?;
                add_input_join_field_directive(field, graph)?;
            }
            Change::MagicFinder { type_name, graph } => {
                {
                    let arg_ty = add_type(schema, "_Any", make_any_scalar())?;
                    add_join_type_directive(arg_ty, graph, &Key::None);
                }

                let ty = upsert_type(original_schema, schema, "Query")?;
                add_join_type_directive(ty, graph, &Key::None);

                add_entities_field(
                    ty,
                    graph,
                    format!("_{}_finder", type_name).as_str(),
                    type_name,
                )?;
            }
        }
        Ok(())
    }
}

fn upsert_field<'a>(
    source: &Schema,
    dest: &'a mut Schema,
    type_name: &Name,
    field_name: &Name,
) -> anyhow::Result<&'a mut FieldDefinition> {
    let new_ty = dest
        .types
        .get_mut(type_name)
        .ok_or(anyhow!("Cannot copy field to type that does not exist"))?;
    let field = source
        .type_field(type_name, field_name)
        .map_err(|_| anyhow!("Missing field {}.{}", type_name, field_name))?;

    let new_field = match new_ty {
        ExtendedType::Object(ref mut ty) => ty
            .make_mut()
            .fields
            .entry(field_name.clone())
            .or_insert_with(|| clean_copy_of_field(field).into()),
        ExtendedType::Interface(ref mut ty) => ty
            .make_mut()
            .fields
            .entry(field_name.clone())
            .or_insert_with(|| clean_copy_of_field(field).into()),
        _ => bail!("Cannot copy field into non-composite type"),
    };

    Ok(new_field.make_mut())
}

fn upsert_input_field<'a>(
    source: &Schema,
    dest: &'a mut Schema,
    type_name: &Name,
    field_name: &Name,
) -> anyhow::Result<&'a mut InputValueDefinition> {
    let new_ty = dest
        .types
        .get_mut(type_name)
        .ok_or(anyhow!("Cannot copy field to type that does not exist"))?;

    let ty = source.get_input_object(type_name).ok_or_else(|| {
        anyhow!(
            "Cannot copy field to type that does not exist: {}",
            type_name
        )
    })?;

    let field = ty
        .fields
        .get(field_name)
        .ok_or_else(|| anyhow!("Missing field {}.{}", type_name, field_name))?;

    let new_field = match new_ty {
        ExtendedType::InputObject(ref mut ty) => ty
            .make_mut()
            .fields
            .entry(field_name.clone())
            .or_insert_with(|| clean_copy_of_input_field(field).into()),
        _ => bail!("Cannot copy field into non-composite type"),
    };

    Ok(new_field.make_mut())
}

fn upsert_type<'a>(
    source: &Schema,
    dest: &'a mut Schema,
    name: &str,
) -> anyhow::Result<&'a mut ExtendedType> {
    let original = source
        .types
        .get(name)
        .ok_or(anyhow!("Cannot copy type that does not exist"))?;

    if source
        .root_operation(apollo_compiler::executable::OperationType::Query)
        .map(|op| op.as_str() == name)
        .unwrap_or(false)
    {
        dest.schema_definition.make_mut().query = Some(ast::Name::new(name)?.into());
    }

    if source
        .root_operation(apollo_compiler::executable::OperationType::Mutation)
        .map(|op| op.as_str() == name)
        .unwrap_or(false)
    {
        dest.schema_definition.make_mut().mutation = Some(ast::Name::new(name)?.into());
    }

    let ty = dest
        .types
        .entry(ast::Name::new(name)?)
        .or_insert_with(|| clean_copy_of_type(original));

    Ok(ty)
}

fn add_type<'a>(
    dest: &'a mut Schema,
    name: &str,
    ty: ExtendedType,
) -> anyhow::Result<&'a mut ExtendedType> {
    Ok(dest
        .types
        .entry(ast::Name::new(name)?)
        .or_insert_with(|| ty))
}

fn clean_copy_of_field(f: &FieldDefinition) -> FieldDefinition {
    let mut f = f.clone();
    f.directives.clear();
    f
}

fn clean_copy_of_input_field(f: &InputValueDefinition) -> InputValueDefinition {
    let mut f = f.clone();
    f.directives.clear();
    f
}

fn clean_copy_of_type(ty: &ExtendedType) -> ExtendedType {
    match ty.clone() {
        ExtendedType::Object(mut ty) => {
            let ty = ty.make_mut();
            ty.directives.clear();
            ty.fields.clear();
            ExtendedType::Object(ty.clone().into())
        }
        ExtendedType::Interface(mut ty) => {
            let ty = ty.make_mut();
            ty.directives.clear();
            ty.fields.clear();
            ExtendedType::Interface(ty.clone().into())
        }
        ExtendedType::Union(mut ty) => {
            let ty = ty.make_mut();
            ty.directives.clear();
            ty.members.clear();
            ExtendedType::Union(ty.clone().into())
        }
        ExtendedType::Enum(mut ty) => {
            let ty = ty.make_mut();
            ty.directives.clear();
            ty.values.clear();
            ExtendedType::Enum(ty.clone().into())
        }
        ExtendedType::Scalar(mut ty) => {
            let ty = ty.make_mut();
            ty.directives.clear();
            ExtendedType::Scalar(ty.clone().into())
        }
        ExtendedType::InputObject(mut ty) => {
            let ty = ty.make_mut();
            ty.directives.clear();
            ty.fields.clear();
            ExtendedType::InputObject(ty.clone().into())
        }
    }
}

fn recurse_selection(
    graph: String,
    schema: &Schema,
    type_name: String,
    ty: &ExtendedType,
    selections: &Vec<Selection>,
) -> anyhow::Result<Vec<Change>> {
    let mut mutations = Vec::new();

    mutations.push(Change::Type {
        name: ast::Name::new(type_name.clone())?,
        graph: graph.clone(),
        key: Key::None,
    });

    match ty {
        ExtendedType::Object(obj) => {
            for selection in selections {
                match selection {
                    Selection::Field(selection) => {
                        let field = obj.fields.get(&selection.name).ok_or(anyhow!(
                            "missing field {} for type {}",
                            selection.name.to_string().as_str(),
                            type_name
                        ))?;

                        let field_type_name = field.ty.inner_named_type();

                        mutations.push(Change::Field {
                            type_name: ast::Name::new(&type_name.clone())?,
                            field_name: selection.name.clone(),
                            graph: graph.clone(),
                        });

                        if !selection.selection_set.is_empty() {
                            let field_type = schema
                                .types
                                .get(field_type_name)
                                .ok_or(anyhow!("missing type {}", field_type_name))?;

                            mutations.extend(recurse_selection(
                                graph.clone(),
                                schema,
                                field_type_name.to_string(),
                                field_type,
                                &selection.selection_set,
                            )?);
                        }
                    }
                    Selection::FragmentSpread(_) => todo!(),
                    Selection::InlineFragment(_) => todo!(),
                }
            }
        }
        ExtendedType::Interface(_) => todo!(),
        ExtendedType::InputObject(_) => todo!(),
        _ => {} // hit a scalar and we're done
    }

    Ok(mutations)
}

fn recurse_inputs(
    graph: String,
    schema: &Schema,
    input_value_def: &Node<InputValueDefinition>,
) -> anyhow::Result<Vec<Change>> {
    let mut changes = Vec::new();

    let output_type_name = input_value_def.ty.inner_named_type();

    let ty = schema
        .types
        .get(output_type_name.as_str())
        .ok_or(anyhow!("missing type {}", output_type_name))?;

    if !ty.is_built_in() {
        changes.push(Change::Type {
            name: output_type_name.clone(),
            graph: graph.clone(),
            key: Key::None,
        });
    }

    if let ExtendedType::InputObject(obj) = ty {
        for field in obj.fields.values() {
            changes.push(Change::InputField {
                type_name: output_type_name.clone(),
                field_name: field.name.clone(),
                graph: graph.clone(),
            });
            changes.extend(recurse_inputs(graph.clone(), schema, &field.node)?);
        }
    }

    Ok(changes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plugins::connectors::directives::HTTPSourceAPI;
    use crate::plugins::connectors::directives::HTTPSourceField;
    use crate::plugins::connectors::selection_parser::Selection as JSONSelection;
    use crate::plugins::connectors::url_path_parser::URLPathTemplate;
    use crate::services::subgraph;

    #[test]
    fn request() {
        let subgraph_request = subgraph::Request::fake_builder()
            .subgraph_request(
                http::Request::builder()
                    .body(crate::graphql::Request::builder().query("{field}").build())
                    .unwrap(),
            )
            .build();
        let connector = Connector {
            name: "CONNECTOR_QUERY_FIELDB".to_string(),
            api: Arc::new(SourceAPI {
                graph: "B".to_string(),
                name: "API".to_string(),
                http: Some(HTTPSourceAPI {
                    base_url: "http://localhost/api".to_string(),
                    default: true,
                    headers: vec![],
                }),
            }),
            ty: Arc::new(ConnectorType::RootField(SourceField {
                graph: "B".to_string(),
                parent_type_name: "Query".to_string(),
                field_name: "field".to_string(),
                output_type_name: "String".to_string(),
                api: "API".to_string(),
                http: Some(HTTPSourceField {
                    method: http::Method::GET,
                    path_template: URLPathTemplate::parse("/path").unwrap(),
                    body: None,
                }),
                selection: JSONSelection::parse(".data").unwrap().1,
            })),
        };

        let requests_and_params = connector
            .create_requests(
                subgraph_request,
                Arc::new(
                    Schema::parse_and_validate(
                        "type Query { field: String }".to_string(),
                        "schema.graphql",
                    )
                    .unwrap(),
                ),
            )
            .unwrap();
        insta::assert_debug_snapshot!(requests_and_params);
    }
}
