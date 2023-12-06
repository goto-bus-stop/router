use anyhow::bail;
use apollo_compiler::ast::DirectiveList;
use apollo_compiler::executable::Argument;
use apollo_compiler::name;
use apollo_compiler::schema::Directive;
use apollo_compiler::schema::EnumType;
use apollo_compiler::schema::EnumValueDefinition;
use apollo_compiler::schema::ExtendedType;
use apollo_compiler::schema::FieldDefinition;
use apollo_compiler::schema::InputValueDefinition;
use apollo_compiler::schema::ScalarType;
use apollo_compiler::schema::Type;
use apollo_compiler::schema::Value;
use apollo_compiler::ty;
use apollo_compiler::NodeStr;
use apollo_compiler::Schema;
use indexmap::IndexMap;
use itertools::Itertools;

pub(super) fn copy_definitions(schema: &Schema, new_schema: &mut Schema) {
    // @link
    let schema_definition = new_schema.schema_definition.make_mut();
    schema
        .schema_definition
        .directives
        .iter()
        .filter(|directive| directive.name == "link")
        .for_each(|directive| {
            schema_definition.directives.push(directive.clone());
        });

    // join__ directive definitions
    schema
        .directive_definitions
        .iter()
        .filter(|(name, _)| name.to_string().starts_with("join__") || *name == "link")
        .for_each(|(name, d)| {
            new_schema
                .directive_definitions
                .insert(name.clone(), d.clone());
        });

    // join__ and link__ scalars
    schema
        .types
        .iter()
        .filter(|(name, t)| {
            matches!(t, ExtendedType::Scalar(_))
                && (name.to_string().starts_with("join__")
                    || name.to_string().starts_with("link__"))
        })
        .for_each(|(name, d)| {
            new_schema.types.insert(name.clone(), d.clone());
        });

    // link__ enum
    schema
        .types
        .iter()
        .filter(|(name, t)| {
            matches!(t, ExtendedType::Enum(_)) && (name.to_string().starts_with("link__"))
        })
        .for_each(|(name, d)| {
            new_schema.types.insert(name.clone(), d.clone());
        });
}

pub(super) fn join_graph_enum(names: &[&str]) -> ExtendedType {
    let values: IndexMap<_, _> = names
        .iter()
        .map(|name| {
            (
                name!(name),
                EnumValueDefinition {
                    value: name!(name),
                    directives: DirectiveList(vec![
                        join_graph_directive(name, "http://unused").into()
                    ]),
                    description: None,
                }
                .into(),
            )
        })
        .collect();

    ExtendedType::Enum(
        EnumType {
            name: name!("join__Graph"),
            description: None,
            directives: Default::default(),
            values,
        }
        .into(),
    )
}

// directive @join__graph(name: String!, url: String!) on ENUM_VALUE

fn join_graph_directive(name: &str, url: &str) -> Directive {
    Directive {
        name: name!("join__graph"),
        arguments: vec![
            Argument {
                name: name!("name"),
                value: Value::String(name.into()).into(),
            }
            .into(),
            Argument {
                name: name!("url"),
                value: Value::String(url.into()).into(),
            }
            .into(),
        ],
    }
}

/*
directive @join__type(
  graph: join__Graph!
  key: join__FieldSet
  extension: Boolean! = false         # TODO
  resolvable: Boolean! = true
  isInterfaceObject: Boolean! = false # TODO
) repeatable on OBJECT | INTERFACE | UNION | ENUM | INPUT_OBJECT | SCALAR
*/

#[derive(Debug)]
pub(super) enum Key {
    None,
    Resolvable(String),
    NonResolvable(String),
}

fn join_type_directive(_graph: &str, key: &Key) -> Directive {
    let mut arguments = vec![Argument {
        name: name!("graph"),
        value: Value::Enum(name!(graph)).into(),
    }
    .into()];

    match key {
        Key::Resolvable(fields) => {
            arguments.push(
                Argument {
                    name: name!("key"),
                    value: Value::String(NodeStr::new(fields.as_str())).into(),
                }
                .into(),
            );
        }
        Key::NonResolvable(fields) => {
            arguments.push(
                Argument {
                    name: name!("key"),
                    value: Value::String(NodeStr::new(fields.as_str())).into(),
                }
                .into(),
            );

            arguments.push(
                Argument {
                    name: name!("resolveable"),
                    value: Value::Boolean(false).into(),
                }
                .into(),
            );
        }
        _ => {}
    }

    Directive {
        name: name!("join__type"),
        arguments,
    }
}

pub(super) fn add_join_type_directive(ty: &mut ExtendedType, graph: &str, key: &Key) {
    // TODO instead of adding the directive more than once, we should "upgrade" existing
    // directives. (e.g. without key -> add key, or resolvable: false -> resolvable: true)
    let exists = ty.directives().iter().any(|d| {
        d.name == "join__type"
            && d.argument_by_name("graph")
                .and_then(|val| val.as_enum())
                .map(|val| val.as_str() == graph)
                .unwrap_or(false)
            && d.argument_by_name("key")
                .and_then(|val| val.as_str())
                .map(|val| match key {
                    Key::None => false,
                    Key::Resolvable(f) => val == f,
                    Key::NonResolvable(f) => val == f,
                })
                .unwrap_or(false)
            && d.argument_by_name("resolvable")
                .and_then(|val| val.to_bool())
                .map(|val| match key {
                    Key::None => false,
                    Key::Resolvable(_) => !val,
                    Key::NonResolvable(_) => val,
                })
                .unwrap_or(false)
    });

    if exists {
        return;
    }

    match ty {
        ExtendedType::Object(ref mut ty) => {
            let ty = ty.make_mut();
            ty.directives.push(join_type_directive(graph, key).into());
        }
        ExtendedType::Interface(ty) => {
            let ty = ty.make_mut();
            ty.directives.push(join_type_directive(graph, key).into());
        }
        ExtendedType::Union(ty) => {
            let ty = ty.make_mut();
            ty.directives.push(join_type_directive(graph, key).into());
        }
        ExtendedType::Enum(ty) => {
            let ty = ty.make_mut();
            ty.directives.push(join_type_directive(graph, key).into());
        }
        ExtendedType::InputObject(ty) => {
            let ty = ty.make_mut();
            ty.directives.push(join_type_directive(graph, key).into());
        }
        ExtendedType::Scalar(ty) => {
            let ty = ty.make_mut();
            ty.directives.push(join_type_directive(graph, key).into());
        }
    }
}

/*
directive @join__field(
  graph: join__Graph
  requires: join__FieldSet # TODO
  provides: join__FieldSet # TODO
  type: String             # TODO
  external: Boolean        # TODO
  override: String         # TODO
  usedOverridden: Boolean  # TODO
) repeatable on FIELD_DEFINITION | INPUT_FIELD_DEFINITION
*/

fn join_field_directive(_graph: &str) -> Directive {
    Directive {
        name: name!("join__field"),
        arguments: vec![Argument {
            name: name!("graph"),
            value: Value::Enum(name!(graph)).into(),
        }
        .into()],
    }
}

pub(super) fn add_join_field_directive(
    field: &mut FieldDefinition,
    graph: &str,
) -> anyhow::Result<()> {
    let exists = field.directives.iter().any(|d| {
        d.name == "join__field"
            && d.argument_by_name("graph")
                .and_then(|val| val.as_enum())
                .map(|val| val.as_str() == graph)
                .unwrap_or(false)
    });

    if exists {
        return Ok(());
    }

    field.directives.push(join_field_directive(graph).into());

    Ok(())
}

/*
TODO:

directive @join__enumValue(graph: join__Graph!) repeatable on ENUM_VALUE

directive @join__implements(
  graph: join__Graph!
  interface: String!
) repeatable on OBJECT | INTERFACE

directive @join__unionMember(
  graph: join__Graph!
  member: String!
) repeatable on UNION

*/

pub(super) fn add_entities_field(
    ty: &mut ExtendedType,
    graph: &str,
    _name: &str,
    _entity_name: &str,
) -> anyhow::Result<()> {
    match ty {
        ExtendedType::Object(ref mut ty) => {
            let ty = ty.make_mut();
            ty.fields
                .entry(name!(name))
                .and_modify(|f| {
                    f.make_mut()
                        .directives
                        .push(join_field_directive(graph).into());
                })
                .or_insert_with(|| {
                    FieldDefinition {
                        name: name!(name),
                        arguments: vec![InputValueDefinition {
                            description: Default::default(),
                            directives: Default::default(),
                            default_value: Default::default(),
                            name: name!("representations"),
                            ty: ty!([_Any!]!).into(),
                        }
                        .into()],
                        directives: DirectiveList(vec![join_field_directive(graph).into()]),
                        description: None,
                        ty: Type::Named(name!(entity_name)).non_null().list().non_null(),
                    }
                    .into()
                });
        }
        _ => bail!("Cannot add entities field to non-object type"),
    }

    Ok(())
}

pub(super) fn make_any_scalar() -> ExtendedType {
    ExtendedType::Scalar(
        ScalarType {
            name: name!("_Any"),
            description: None,
            directives: apollo_compiler::schema::DirectiveList(vec![Directive {
                arguments: vec![Argument {
                    name: name!("url"),
                    // just to avoid validation warnings
                    value: Value::String(NodeStr::new("https://whatever")).into(),
                }
                .into()],
                name: name!("specifiedBy"),
            }
            .into()]),
        }
        .into(),
    )
}

// GraphQL Selection Sets for @key fields --------------------------------------

use apollo_compiler::ast::Selection as GraphQLSelection;

fn new_field(_name: String, selection: Option<Vec<GraphQLSelection>>) -> GraphQLSelection {
    GraphQLSelection::Field(
        apollo_compiler::ast::Field {
            alias: None,
            name: name!(name),
            arguments: Default::default(),
            directives: Default::default(),
            selection_set: selection.unwrap_or_default(),
        }
        .into(),
    )
}

// key fields are typically a single line
pub(super) fn selection_set_to_string(selection_set: &[apollo_compiler::ast::Selection]) -> String {
    selection_set
        .iter()
        .map(|s| s.serialize().no_indent().to_string())
        .join(" ")
}

pub(super) fn parameters_to_selection_set(paths: &Vec<String>) -> Vec<GraphQLSelection> {
    let mut root = Vec::new();

    for path in paths {
        let mut parts: Vec<&str> = path.split('.').collect();
        // "this" is an alias, so we can ignore it
        if parts.first() == Some(&"this") {
            parts = parts[1..].to_vec();
        }

        let mut current_node = &mut root;

        for part in parts {
            let existing_node_index =
                current_node
                    .iter()
                    .position(|n: &GraphQLSelection| match n {
                        GraphQLSelection::Field(n) => n.name == part,
                        GraphQLSelection::FragmentSpread(_) => false, // TODO
                        GraphQLSelection::InlineFragment(_) => false, // TODO
                    });

            match existing_node_index {
                Some(index) => {
                    current_node = match &mut current_node[index] {
                        GraphQLSelection::Field(n) => &mut n.make_mut().selection_set,
                        GraphQLSelection::FragmentSpread(_) => todo!(),
                        GraphQLSelection::InlineFragment(_) => todo!(),
                    };
                }
                None => {
                    let new_node = new_field(part.to_string(), Some(Vec::new()));
                    current_node.push(new_node);
                    let new_node_index = current_node.len() - 1;
                    current_node = match &mut current_node[new_node_index] {
                        GraphQLSelection::Field(n) => &mut n.make_mut().selection_set,
                        GraphQLSelection::FragmentSpread(_) => todo!(),
                        GraphQLSelection::InlineFragment(_) => todo!(),
                    };
                }
            }
        }
    }

    root
}

#[cfg(test)]
mod tests {
    use super::parameters_to_selection_set;
    use super::selection_set_to_string;

    #[test]
    fn test_parameters_to_selection_set() {
        assert_eq!(
            selection_set_to_string(&parameters_to_selection_set(&vec![
                "id".to_string(),
                "b.c".to_string(),
                "b.d.e".to_string(),
                "b.d.f".to_string(),
                "this.g".to_string(),
                "this.h.i".to_string()
            ])),
            "id b { c d { e f } } g h { i }"
        )
    }
}
