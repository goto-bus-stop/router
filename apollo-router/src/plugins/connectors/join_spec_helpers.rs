use anyhow::bail;
use apollo_compiler::executable::Argument;
use apollo_compiler::executable::Directives;
use apollo_compiler::schema::Directive;
use apollo_compiler::schema::EnumType;
use apollo_compiler::schema::EnumValueDefinition;
use apollo_compiler::schema::ExtendedType;
use apollo_compiler::schema::FieldDefinition;
use apollo_compiler::schema::InputValueDefinition;
use apollo_compiler::schema::ScalarType;
use apollo_compiler::schema::Type;
use apollo_compiler::schema::Value;
use apollo_compiler::NodeStr;
use apollo_compiler::Schema;
use indexmap::IndexMap;

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
                NodeStr::new(name),
                EnumValueDefinition {
                    value: NodeStr::new(name),
                    directives: Directives(
                        vec![join_graph_directive(name, "http://unused").into()],
                    ),
                    description: None,
                }
                .into(),
            )
        })
        .collect();

    ExtendedType::Enum(
        EnumType {
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
        name: "join__graph".into(),
        arguments: vec![
            Argument {
                name: "name".into(),
                value: Value::String(name.into()).into(),
            }
            .into(),
            Argument {
                name: "url".into(),
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

fn join_type_directive(graph: &str, key: Option<&str>, resolvable: bool) -> Directive {
    let mut arguments = vec![Argument {
        name: NodeStr::new("graph"),
        value: Value::Enum(NodeStr::new(graph)).into(),
    }
    .into()];

    if let Some(key) = key {
        arguments.push(
            Argument {
                name: NodeStr::new("key"),
                value: Value::String(NodeStr::new(key)).into(),
            }
            .into(),
        );

        if !resolvable {
            arguments.push(
                Argument {
                    name: NodeStr::new("resolveable"),
                    value: Value::Boolean(false).into(),
                }
                .into(),
            );
        }
    }

    Directive {
        name: NodeStr::new("join__type"),
        arguments,
    }
}

pub(super) fn add_join_type_directive(
    ty: &mut ExtendedType,
    graph: &str,
    key: Option<&str>,
    resolvable: bool,
) {
    let exists = ty.directives().iter().any(|d| {
        d.name == "join__type"
            && d.argument_by_name("graph")
                .and_then(|val| val.as_str())
                .map(|val| val == graph)
                .unwrap_or(false)
            && d.argument_by_name("key")
                .and_then(|val| val.as_str())
                .map(|val| val == key.unwrap_or(""))
                .unwrap_or(false)
            && d.argument_by_name("resolvable")
                .and_then(|val| val.to_bool())
                .map(|val| val == resolvable)
                .unwrap_or(false)
    });

    if exists {
        return;
    }

    match ty {
        ExtendedType::Object(ref mut ty) => {
            let ty = ty.make_mut();
            ty.directives
                .push(join_type_directive(graph, key, resolvable).into());
        }
        ExtendedType::Interface(ty) => {
            let ty = ty.make_mut();
            ty.directives
                .push(join_type_directive(graph, key, resolvable).into());
        }
        ExtendedType::Union(ty) => {
            let ty = ty.make_mut();
            ty.directives
                .push(join_type_directive(graph, key, resolvable).into());
        }
        ExtendedType::Enum(ty) => {
            let ty = ty.make_mut();
            ty.directives
                .push(join_type_directive(graph, key, resolvable).into());
        }
        ExtendedType::InputObject(ty) => {
            let ty = ty.make_mut();
            ty.directives
                .push(join_type_directive(graph, key, resolvable).into());
        }
        ExtendedType::Scalar(ty) => {
            let ty = ty.make_mut();
            ty.directives
                .push(join_type_directive(graph, key, resolvable).into());
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

fn join_field_directive(graph: &str) -> Directive {
    Directive {
        name: NodeStr::new("join__field"),
        arguments: vec![Argument {
            name: NodeStr::new("graph"),
            value: Value::Enum(NodeStr::new(graph)).into(),
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
                .and_then(|val| val.as_str())
                .map(|val| val == graph)
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
    name: &str,
    entity_name: &str,
) -> anyhow::Result<()> {
    let field = FieldDefinition {
        name: NodeStr::new(name),
        arguments: vec![InputValueDefinition {
            description: Default::default(),
            directives: Default::default(),
            default_value: Default::default(),
            name: NodeStr::new("representations"),
            ty: Type::non_null(Type::list(Type::non_null(Type::new_named("_Any")))).into(),
        }
        .into()],
        directives: Directives(vec![join_field_directive(graph).into()]),
        description: None,
        ty: Type::non_null(Type::list(Type::non_null(Type::new_named(entity_name)))),
    };

    match ty {
        ExtendedType::Object(ref mut ty) => {
            let ty = ty.make_mut();
            ty.fields.insert(NodeStr::new(name), field.into());
        }
        _ => bail!("Cannot add entities field to non-object type"),
    }

    Ok(())
}

pub(super) fn make_any_scalar() -> ExtendedType {
    ExtendedType::Scalar(
        ScalarType {
            description: None,
            directives: apollo_compiler::schema::Directives(vec![Directive {
                arguments: vec![Argument {
                    name: NodeStr::new("url"),
                    // just to avoid validation warnings
                    value: Value::String(NodeStr::new("https://whatever")).into(),
                }
                .into()],
                name: NodeStr::new("specifiedBy"),
            }
            .into()]),
        }
        .into(),
    )
}
