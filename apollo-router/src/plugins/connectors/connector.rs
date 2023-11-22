use std::collections::HashMap;

use anyhow::anyhow;
use anyhow::bail;
use apollo_compiler::ast::Selection;
use apollo_compiler::schema::ExtendedType;
use apollo_compiler::schema::FieldDefinition;
use apollo_compiler::NodeStr;
use apollo_compiler::Schema;

use super::directives::SourceAPI;
use super::directives::SourceField;
use super::directives::SourceType;
use super::join_spec_helpers::add_entities_field;
use super::join_spec_helpers::add_join_field_directive;
use super::join_spec_helpers::add_join_type_directive;
use super::join_spec_helpers::make_any_scalar;

/// A connector wraps the API and type/field connector metadata and has
/// a unique name used to construct a "subgraph" in the inner supergraph
pub(crate) struct Connector {
    /// Internal name used to construct "subgraphs" in the inner supergraph
    pub(crate) name: String,
    api: SourceAPI,
    ty: ConnectorType,
}

pub(super) enum ConnectorType {
    Type(SourceType),
    Field(SourceField),
}

/// The list of the subgraph names that should use the inner query planner
/// instead of making a normal subgraph request.
pub(crate) fn connector_subgraph_names(connectors: HashMap<String, Connector>) -> Vec<String> {
    connectors
        .values()
        .map(|c| c.outer_subgraph_name())
        .collect()
}

impl Connector {
    /// Generate a map of connectors with unique names
    pub(super) fn from_schema(schema: &Schema) -> anyhow::Result<HashMap<String, Self>> {
        let apis = SourceAPI::from_schema(schema)?;
        let types = SourceType::from_schema(schema)?;
        let fields = SourceField::from_schema(schema)?;

        let mut connectors = HashMap::new();

        for (i, (type_name, directive)) in types.into_iter().enumerate() {
            let connector_name = format!("CONNECTOR_{}_{}", type_name, i).to_uppercase();

            connectors.insert(
                connector_name.clone(),
                Connector {
                    name: connector_name,
                    api: apis
                        .get(&directive.api_name())
                        .ok_or(anyhow!("missing API {}", directive.api_name()))? // TODO support default
                        .clone(),
                    ty: ConnectorType::Type(directive),
                },
            );
        }

        for (i, directive) in fields.into_iter().enumerate() {
            let connector_name = format!(
                "CONNECTOR_{}_{}_{}",
                directive.parent_type_name, directive.field_name, i
            )
            .to_uppercase();

            connectors.insert(
                connector_name.clone(),
                Connector {
                    name: connector_name,
                    api: apis
                        .get(&directive.api_name())
                        .ok_or(anyhow!("missing API {}", directive.api_name()))? // TODO support default
                        .clone(),
                    ty: ConnectorType::Field(directive),
                },
            );
        }

        Ok(connectors)
    }

    /// Generate a list of changes to apply to the new schame
    pub(super) fn changes(&self, schema: &Schema) -> anyhow::Result<Vec<Change>> {
        let graph = self.name.clone();
        match &self.ty {
            ConnectorType::Type(ty) => {
                let mut changes = vec![
                    Change::Type {
                        name: ty.type_name.clone().into(),
                        graph: graph.clone(),
                        key: None, // TODO
                    },
                    Change::MagicFinder {
                        type_name: ty.type_name.clone().into(),
                        graph: graph.clone(),
                    },
                ];

                changes.extend(recurse_selection(
                    graph,
                    schema,
                    ty.type_name.clone(),
                    schema
                        .types
                        .get(ty.type_name.as_str())
                        .ok_or(anyhow!("missing type"))?,
                    &ty.selections(),
                )?);

                Ok(changes)
            }
            ConnectorType::Field(field) => {
                let mut changes = vec![
                    Change::Type {
                        name: field.parent_type_name.clone().into(),
                        graph: graph.clone(),
                        key: None, // TODO,
                    },
                    Change::Field {
                        type_name: field.parent_type_name.clone().into(),
                        field_name: field.field_name.clone().into(),
                        graph: graph.clone(),
                    },
                ];

                changes.extend(recurse_selection(
                    graph,
                    schema,
                    field.output_type_name.clone(),
                    schema
                        .types
                        .get(field.output_type_name.as_str())
                        .ok_or(anyhow!("missing type"))?,
                    &field.selections(),
                )?);

                // TODO if field is on an entity type, add a magic finder for parent type

                Ok(changes)
            }
        }
    }

    pub(super) fn outer_subgraph_name(&self) -> String {
        match self.ty {
            ConnectorType::Type(ref ty) => ty.graph.clone(),
            ConnectorType::Field(ref field) => field.graph.clone(),
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
        name: NodeStr,
        graph: String,
        key: Option<String>, // TODO
    },
    /// Include a field on a type in the schema and add the `@join__field` directive
    /// TODO: currently assumes that the type already exists
    Field {
        type_name: NodeStr,
        field_name: NodeStr,
        graph: String,
    },
    /// Add a special field to Query that we can use instead of `_entities`
    MagicFinder { type_name: NodeStr, graph: String },
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
                add_join_type_directive(ty, graph, key.as_deref(), false);
            }
            Change::Field {
                type_name,
                field_name,
                graph,
            } => {
                let field = upsert_field(original_schema, schema, type_name, field_name)?;
                add_join_field_directive(field, graph)?;
            }
            Change::MagicFinder { type_name, graph } => {
                {
                    let arg_ty = add_type(schema, "_Any", make_any_scalar())?;
                    add_join_type_directive(arg_ty, graph, None, false);
                }

                let ty = upsert_type(original_schema, schema, "Query")?;
                add_join_type_directive(ty, graph, None, false);

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
    type_name: &NodeStr,
    field_name: &NodeStr,
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
        dest.schema_definition.make_mut().query = Some(name.into());
    }

    if source
        .root_operation(apollo_compiler::executable::OperationType::Mutation)
        .map(|op| op.as_str() == name)
        .unwrap_or(false)
    {
        dest.schema_definition.make_mut().mutation = Some(name.into());
    }

    let ty = dest
        .types
        .entry(name.into())
        .or_insert_with(|| clean_copy_of_type(original));

    Ok(ty)
}

fn add_type<'a>(
    dest: &'a mut Schema,
    name: &str,
    ty: ExtendedType,
) -> anyhow::Result<&'a mut ExtendedType> {
    Ok(dest.types.entry(NodeStr::new(name)).or_insert_with(|| ty))
}

fn clean_copy_of_field(f: &FieldDefinition) -> FieldDefinition {
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
        name: NodeStr::new(&type_name.clone()),
        graph: graph.clone(),
        key: None, // TODO
    });

    match ty {
        ExtendedType::Object(obj) => {
            for selection in selections {
                match selection {
                    Selection::Field(selection) => {
                        let field = obj
                            .fields
                            .get(&NodeStr::new(selection.name.to_string().as_str()))
                            .ok_or(anyhow!("missing field"))?;

                        let field_type_name = field.ty.inner_named_type();

                        mutations.push(Change::Field {
                            type_name: NodeStr::new(&type_name.clone()),
                            field_name: selection.name.clone(),
                            graph: graph.clone(),
                        });

                        if !selection.selection_set.is_empty() {
                            let field_type = schema
                                .types
                                .get(field_type_name)
                                .ok_or(anyhow!("missing type"))?;

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
        _ => bail!("composite types only"),
    }

    Ok(mutations)
}
