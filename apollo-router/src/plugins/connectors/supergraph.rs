use std::collections::HashMap;

use anyhow::anyhow;
use anyhow::bail;
use apollo_compiler::ast;
use apollo_compiler::ast::Selection;
use apollo_compiler::name;
use apollo_compiler::schema::EnumValueDefinition;
use apollo_compiler::schema::ExtendedType;
use apollo_compiler::schema::FieldDefinition;
use apollo_compiler::schema::InputValueDefinition;
use apollo_compiler::schema::Name;
use apollo_compiler::Node;
use apollo_compiler::Schema;
use itertools::Itertools;
use tower::BoxError;

use super::connector::Connector;
use super::connector::ConnectorKind;
use super::directives::graph_enum_map;
use super::join_spec_helpers::add_entities_field;
use super::join_spec_helpers::add_input_join_field_directive;
use super::join_spec_helpers::add_join_enum_value_directive;
use super::join_spec_helpers::add_join_field_directive;
use super::join_spec_helpers::add_join_implements;
use super::join_spec_helpers::add_join_type_directive;
use super::join_spec_helpers::copy_definitions;
use super::join_spec_helpers::join_graph_enum;
use super::join_spec_helpers::make_any_scalar;

/// Generates a new supergraph schema with one subgraph per connector. Copies
/// types and fields from the original schema and adds directives to associate
/// them with the appropriate connector.
pub(crate) fn generate_connector_supergraph(
    schema: &Schema,
    connectors: &HashMap<String, Connector>,
) -> Result<Schema, BoxError> {
    let mut new_schema = Schema::new();
    copy_definitions(schema, &mut new_schema);

    /* enum name -> subgraph name  */
    let origin_subgraph_map = graph_enum_map(schema)
        .ok_or("missing join__Graph enum")?
        .into_iter()
        .map(|(k, v)| (v, k))
        .collect::<HashMap<_, _>>();

    let mut changes = Vec::new();
    // sorted for stable SDL generation
    for connector in connectors.values().sorted_by_key(|c| c.name.clone()) {
        changes.extend(make_changes(connector, schema, &origin_subgraph_map)?);
    }

    for change in changes {
        change.apply_to(schema, &mut new_schema)?;
    }

    let connector_graph_names = connectors
        .values()
        // sorted for stable SDL generation
        .sorted_by_key(|c| c.name.clone())
        .map(|c| c.name.as_str())
        .collect::<Vec<_>>();
    new_schema.types.insert(
        name!("join__Graph"),
        join_graph_enum(&connector_graph_names),
    );

    Ok(new_schema)
}

/// Generate a list of changes to apply to the new schame
pub(super) fn make_changes(
    connector: &Connector,
    schema: &Schema,
    subgraph_enum_map: &HashMap<String, String>,
) -> anyhow::Result<Vec<Change>> {
    let graph = connector.name.clone();
    let origin_subgraph_name = subgraph_enum_map
        .get(connector.origin_subgraph.as_str())
        .ok_or(anyhow!("missing origin subgraph"))?;

    match &connector.kind {
        // Root fields: add the parent type and the field, then recursively
        // add the selections
        ConnectorKind::RootField {
            field_name,
            output_type_name,
            parent_type_name,
        } => {
            let mut changes = vec![
                Change::Type {
                    name: parent_type_name.clone(),
                    graph: graph.clone(),
                    key: None,
                    is_interface_object: false,
                    implements: None,
                },
                Change::Field {
                    type_name: parent_type_name.clone(),
                    field_name: field_name.clone(),
                    graph: graph.clone(),
                },
            ];

            changes.extend(recurse_selection(
                origin_subgraph_name,
                graph.clone(),
                schema,
                output_type_name,
                schema
                    .types
                    .get(output_type_name)
                    .ok_or(anyhow!("missing type"))?,
                &connector.output_selection,
            )?);

            let field_def = schema
                .type_field(parent_type_name, field_name)
                .map_err(|_| anyhow::anyhow!("missing field"))?;

            for arg in field_def.arguments.iter() {
                changes.extend(recurse_inputs(graph.clone(), schema, arg)?);
            }

            Ok(changes)
        }
        // Entity: add the type with appropriate keys, add a finder field,
        // recursively add the selections, and recursively add the key fields if necessary
        ConnectorKind::Entity {
            type_name,
            key,
            is_interface_object,
        } => {
            let mut changes = vec![
                Change::Type {
                    name: type_name.clone(),
                    graph: graph.clone(),
                    key: Some(key.clone()),
                    is_interface_object: *is_interface_object,
                    implements: None,
                },
                Change::MagicFinder {
                    type_name: type_name.clone(),
                    graph: graph.clone(),
                },
            ];

            changes.extend(recurse_selection(
                origin_subgraph_name,
                graph.clone(),
                schema,
                type_name,
                schema.types.get(type_name).ok_or(anyhow!("missing type"))?,
                &connector.output_selection,
            )?);

            // TODO need a test with a nested composite key
            // TODO mark key fields as external if necessary
            changes.extend(recurse_selection(
                origin_subgraph_name,
                graph,
                schema,
                type_name,
                schema.types.get(type_name).ok_or(anyhow!("missing type"))?,
                &connector.input_selection,
            )?);

            Ok(changes)
        }
        // Entity field: add the parent entity type with appropriate keys,
        // add the field itself, add a finder field, recursively add the
        // selections, and recursively add the key fields if necessary
        ConnectorKind::EntityField {
            field_name,
            type_name,
            output_type_name,
            key,
            on_interface_object,
        } => {
            let mut changes = vec![
                Change::Type {
                    name: type_name.clone(),
                    graph: graph.clone(),
                    key: Some(key.clone()),
                    is_interface_object: *on_interface_object,
                    implements: None,
                },
                Change::Field {
                    type_name: type_name.clone(),
                    field_name: field_name.clone(),
                    graph: graph.clone(),
                },
                Change::MagicFinder {
                    type_name: type_name.clone(),
                    graph: graph.clone(),
                },
            ];

            changes.extend(recurse_selection(
                origin_subgraph_name,
                graph.clone(),
                schema,
                output_type_name,
                schema
                    .types
                    .get(output_type_name)
                    .ok_or(anyhow!("missing type"))?,
                &connector.output_selection,
            )?);

            // TODO need a test with a nested composite key
            // TODO mark key fields as external if necessary
            changes.extend(recurse_selection(
                origin_subgraph_name,
                graph.clone(),
                schema,
                type_name, // key fields are on the entity type, not the output type
                schema
                    .types
                    .get(type_name.as_str())
                    .ok_or(anyhow!("missing type"))?,
                &connector.input_selection,
            )?);

            let field_def = schema
                .type_field(type_name, field_name)
                .map_err(|_| anyhow::anyhow!("missing field"))?;

            for arg in field_def.arguments.iter() {
                changes.extend(recurse_inputs(graph.clone(), schema, arg)?);
            }

            Ok(changes)
        }
    }
}

/// A "change" is a unit of work that can be applied to a schema. Each connector
/// produces a set of changes to include types, fields, and applies
/// `@join__` directives appropriately so that the query planner can extract
/// subgraphs for each connector.
#[derive(Debug)]
pub(super) enum Change {
    /// Include a type in the schema and add the `@join__type` directive
    Type {
        name: Name,
        graph: String,
        key: Option<String>,
        is_interface_object: bool,
        implements: Option<Name>,
    },
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
    /// Add an enum value
    EnumValue {
        enum_name: Name,
        value_name: Name,
        graph: String,
    },
}

impl Change {
    /// Apply this change to a schema, generating or modifying types and fields
    pub(super) fn apply_to(
        &self,
        original_schema: &Schema,
        schema: &mut Schema,
    ) -> anyhow::Result<()> {
        match self {
            Change::Type {
                name,
                graph,
                key,
                is_interface_object,
                implements,
            } => {
                let ty = upsert_type(original_schema, schema, name)?;
                add_join_type_directive(ty, graph, key.clone(), Some(*is_interface_object));
                if let Some(implements) = implements {
                    add_join_implements(ty, graph, implements)?;
                }
            }

            Change::Field {
                type_name,
                field_name,
                graph,
            } => {
                if let Some(field) = upsert_field(original_schema, schema, type_name, field_name)? {
                    add_join_field_directive(field, graph)?;
                }
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
                    add_join_type_directive(arg_ty, graph, None, None);
                }

                let ty = upsert_type(original_schema, schema, "Query")?;
                add_join_type_directive(ty, graph, None, None);

                add_entities_field(
                    ty,
                    graph,
                    format!("_{}_finder", type_name).as_str(),
                    type_name,
                )?;
            }

            Change::EnumValue {
                enum_name,
                value_name,
                graph,
            } => {
                let ty = upsert_type(original_schema, schema, enum_name)?;
                add_join_type_directive(ty, graph, None, None);
                if let ExtendedType::Enum(enm) = ty {
                    let value = enm.make_mut().values.entry(value_name.clone()).or_insert(
                        EnumValueDefinition {
                            description: Default::default(),
                            value: value_name.clone(),
                            directives: Default::default(),
                        }
                        .into(),
                    );
                    let value = value.make_mut();
                    add_join_enum_value_directive(value, graph);
                }
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
) -> anyhow::Result<Option<&'a mut FieldDefinition>> {
    let new_ty = dest
        .types
        .get_mut(type_name)
        .ok_or(anyhow!("Cannot copy field to type that does not exist"))?;

    if let Ok(field) = source.type_field(type_name, field_name) {
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

        Ok(Some(new_field.make_mut()))
    } else {
        Ok(None)
    }
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
    origin_graph: &str,
    graph: String,
    schema: &Schema,
    type_name: &Name,
    ty: &ExtendedType,
    selections: &Vec<Selection>,
) -> anyhow::Result<Vec<Change>> {
    let mut mutations = Vec::new();

    mutations.push(Change::Type {
        name: type_name.clone(),
        graph: graph.clone(),
        key: None,
        is_interface_object: false,
        implements: None,
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
                            type_name: type_name.clone(),
                            field_name: selection.name.clone(),
                            graph: graph.clone(),
                        });

                        let field_type = schema
                            .types
                            .get(field_type_name)
                            .ok_or(anyhow!("missing type {}", field_type_name))?;

                        if field_type.is_enum() {
                            mutations.extend(enum_values_for_graph(
                                field_type,
                                origin_graph,
                                &graph,
                            ));
                        }

                        if !selection.selection_set.is_empty() {
                            mutations.extend(recurse_selection(
                                origin_graph,
                                graph.clone(),
                                schema,
                                field_type_name,
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
        ExtendedType::Interface(obj) => {
            let implementors_map = schema.implementers_map();
            let possible_types = implementors_map
                .get(type_name)
                .ok_or(anyhow!("get possible types for {} failed", type_name))?
                .iter()
                .flat_map(|name| schema.types.get(name))
                .filter(|ty| {
                    ty.directives().iter().any(|d| {
                        d.name == "join__type"
                            && d.argument_by_name("graph")
                                .and_then(|a| a.as_enum())
                                .map(|e| *e == origin_graph)
                                .unwrap_or(false)
                    })
                })
                .sorted_by_key(|ty| ty.name().to_string())
                .collect::<Vec<_>>();

            for selection in selections {
                match selection {
                    Selection::Field(selection) => {
                        if let Some(field) = obj.fields.get(&selection.name) {
                            let field_type_name = field.ty.inner_named_type();

                            mutations.push(Change::Field {
                                type_name: type_name.clone(),
                                field_name: selection.name.clone(),
                                graph: graph.clone(),
                            });

                            if !selection.selection_set.is_empty() {
                                let field_type = schema
                                    .types
                                    .get(field_type_name)
                                    .ok_or(anyhow!("missing type {}", field_type_name))?;

                                mutations.extend(recurse_selection(
                                    origin_graph,
                                    graph.clone(),
                                    schema,
                                    field_type_name,
                                    field_type,
                                    &selection.selection_set,
                                )?);
                            }
                        }

                        for possible_type in possible_types.iter() {
                            match possible_type {
                                ExtendedType::Object(obj) => {
                                    if let Some(field) = obj.fields.get(&selection.name) {
                                        {
                                            mutations.push(Change::Type {
                                                name: possible_type.name().clone(),
                                                graph: graph.clone(),
                                                key: None,
                                                is_interface_object: false,
                                                implements: Some(type_name.clone()),
                                            });

                                            let field_type_name = field.ty.inner_named_type();

                                            mutations.push(Change::Field {
                                                type_name: possible_type.name().clone(),
                                                field_name: selection.name.clone(),
                                                graph: graph.clone(),
                                            });

                                            let field_type =
                                                schema.types.get(field_type_name).ok_or(
                                                    anyhow!("missing type {}", field_type_name),
                                                )?;

                                            if field_type.is_enum() {
                                                mutations.extend(enum_values_for_graph(
                                                    field_type,
                                                    origin_graph,
                                                    &graph,
                                                ));
                                            }

                                            if !selection.selection_set.is_empty() {
                                                mutations.extend(recurse_selection(
                                                    origin_graph,
                                                    graph.clone(),
                                                    schema,
                                                    field_type_name,
                                                    field_type,
                                                    &selection.selection_set,
                                                )?);
                                            }
                                        }
                                    }
                                }
                                ExtendedType::Interface(_) => todo!(),
                                _ => {}
                            }
                        }
                    }

                    Selection::FragmentSpread(_) => todo!(),
                    Selection::InlineFragment(_) => todo!(),
                }
            }
        }
        ExtendedType::InputObject(_) => return Err(anyhow!("input object in selection")),
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
            key: None,
            is_interface_object: false,
            implements: None,
        });
    }

    match ty {
        ExtendedType::InputObject(obj) => {
            for field in obj.fields.values() {
                changes.push(Change::InputField {
                    type_name: output_type_name.clone(),
                    field_name: field.name.clone(),
                    graph: graph.clone(),
                });
                changes.extend(recurse_inputs(graph.clone(), schema, &field.node)?);
            }
        }
        ExtendedType::Enum(enm) => {
            for value in enm.values.values() {
                changes.push(Change::EnumValue {
                    enum_name: ty.name().clone(),
                    value_name: value.value.clone(),
                    graph: graph.to_string(),
                });
            }
        }
        _ => {}
    }

    Ok(changes)
}

/// Given an enum definition, find all the values that are associated with the origin
/// subgraph. Return a list of enum value inclusions for the connector subgraph.
fn enum_values_for_graph(ty: &ExtendedType, origin_graph: &str, graph: &str) -> Vec<Change> {
    let mut results = Vec::new();

    if let ExtendedType::Enum(enm) = ty {
        for value in enm.values.values() {
            let has_join = value.directives.iter().any(|d| {
                d.name == "join__enumValue"
                    && d.argument_by_name("graph")
                        .and_then(|a| a.as_enum())
                        .map(|e| *e == origin_graph)
                        .unwrap_or(false)
            });
            if has_join {
                results.push(Change::EnumValue {
                    enum_name: ty.name().clone(),
                    value_name: value.value.clone(),
                    graph: graph.to_string(),
                });
            }
        }
    }

    results
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use apollo_compiler::Schema;
    use insta::assert_snapshot;
    use itertools::Itertools;

    use super::generate_connector_supergraph;
    use crate::plugins::connectors::connector::Connector;
    use crate::spec::Schema as RouterSchema;
    use crate::Configuration;

    const SCHEMA: &str = include_str!("./test_supergraph.graphql");

    #[test]
    fn it_works() -> anyhow::Result<()> {
        let schema = Schema::parse_and_validate(SCHEMA, "outer.graphql").unwrap();

        let connectors = Arc::from(Connector::from_schema(&schema).unwrap());
        let inner = generate_connector_supergraph(&schema, &connectors).unwrap();

        // new supergraph can be parsed into subgraphs
        let result = RouterSchema::parse(
            inner.serialize().to_string().as_str(),
            &Configuration::fake_builder().build().unwrap(),
        )?;

        assert_eq!(
            result
                .subgraph_definition_and_names
                .values()
                .sorted()
                .cloned()
                .collect::<Vec<_>>(),
            vec![
                "CONNECTOR_ENTITYACROSSBOTH_0".to_string(),
                "CONNECTOR_ENTITYACROSSBOTH_E_0".to_string(),
                "CONNECTOR_HELLO_1".to_string(),
                "CONNECTOR_HELLO_WORLD_1".to_string(),
                "CONNECTOR_MUTATION_MUTATION_2".to_string(),
                "CONNECTOR_QUERY_HELLO_3".to_string(),
                "CONNECTOR_QUERY_INTERFACES_5".to_string(),
                "CONNECTOR_QUERY_WITHARGUMENTS_4".to_string(),
                "CONNECTOR_TESTINGINTERFACEOBJECT_2".to_string(),
                "CONNECTOR_TESTINGINTERFACEOBJECT_D_6".to_string()
            ]
        );

        assert_snapshot!(inner.serialize().to_string());
        Ok(())
    }
}
