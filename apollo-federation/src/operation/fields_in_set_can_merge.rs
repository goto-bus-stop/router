//! Adapted from https://github.com/apollographql/apollo-rs/blob/main/crates/apollo-compiler/src/validation/selection.rs
//!
/// This uses the [validation algorithm described by XING][0] ([archived][1]), which
/// scales much better with larger selection sets that may have many overlapping fields,
/// and with widespread use of fragments.
///
/// The functionality here is a bit different than in the apollo-compiler source. We are interested
/// in checking if two selection sets can be merged with each other. We can assume that the input
/// selection sets are correct, because they were already validated before getting to the query
/// planner. The XING algorithm works by intelligently grouping selections together. We can still
/// do that once per selection set, and then concatenate the groups and run the rest of the
/// algorithm for each selection set that we attempt to merge in.
///
/// [0]: https://tech.new-work.se/graphql-overlapping-fields-can-be-merged-fast-ea6e92e0a01
/// [1]: https://web.archive.org/web/20240208084612/https://tech.new-work.se/graphql-overlapping-fields-can-be-merged-fast-ea6e92e0a01
use std::cell::OnceCell;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::rc::Rc;
use std::sync::Arc;

use apollo_compiler::executable;
use apollo_compiler::Name;
use apollo_compiler::Node;
use indexmap::IndexMap;

use super::FieldSelection;
use super::Selection;
use super::SelectionSet;
use crate::error::FederationError;
use crate::schema::position::FieldDefinitionPosition;
use crate::schema::position::TypeDefinitionPosition;
use crate::schema::ValidFederationSchema;

/// Expand one or more selection sets to a list of all fields selected.
fn expand_selections<'doc>(
    selection_sets: impl Iterator<Item = &'doc SelectionSet>,
) -> Vec<Arc<FieldSelection>> {
    let mut selections = vec![];
    let mut queue: VecDeque<&SelectionSet> = selection_sets.collect();

    while let Some(next_set) = queue.pop_front() {
        for selection in next_set.selections.values() {
            match selection {
                Selection::Field(field) => selections.push(Arc::clone(field)),
                Selection::InlineFragment(spread) => queue.push_back(&spread.selection_set),
                Selection::FragmentSpread(_) => {
                    unreachable!()
                }
            }
        }
    }

    selections
}

fn is_composite(ty: TypeDefinitionPosition) -> bool {
    use TypeDefinitionPosition::*;
    matches!(ty, Object(_) | Interface(_) | Union(_))
}

/// A temporary index for frequent argument lookups by name, using a hash map if there are many
/// arguments.
enum ArgumentLookup<'a> {
    Map(HashMap<&'a Name, &'a Node<executable::Argument>>),
    List(&'a [Node<executable::Argument>]),
}
impl<'a> ArgumentLookup<'a> {
    fn new(list: &'a [Node<executable::Argument>]) -> Self {
        if list.len() > 20 {
            Self::Map(list.iter().map(|arg| (&arg.name, arg)).collect())
        } else {
            Self::List(list)
        }
    }

    fn by_name(&self, name: &Name) -> Option<&'a Node<executable::Argument>> {
        match self {
            Self::Map(map) => map.get(name).copied(),
            Self::List(list) => list.iter().find(|arg| arg.name == *name),
        }
    }
}

/// Check if two field selections from the overlapping types are the same, so the fields can be merged.
fn same_name_and_arguments(
    field_a: &FieldSelection,
    field_b: &FieldSelection,
) -> Result<bool, FederationError> {
    // 2bi. fieldA and fieldB must have identical field names.
    if field_a.field.name() != field_b.field.name() {
        return Ok(false);
    }

    // Check if fieldB provides the same argument names and values as fieldA (order-independent).
    let self_args = ArgumentLookup::new(&field_a.field.arguments);
    let other_args = ArgumentLookup::new(&field_b.field.arguments);
    for arg in field_a.field.arguments.iter() {
        let Some(other_arg) = other_args.by_name(&arg.name) else {
            return Ok(false);
        };

        if !same_value(&other_arg.value, &arg.value) {
            return Ok(false);
        }
    }
    // Check if fieldB provides any arguments that fieldA does not provide.
    for arg in field_b.field.arguments.iter() {
        if self_args.by_name(&arg.name).is_none() {
            return Ok(false);
        };
    }

    Ok(true)
}

/// Compare two input values, with two special cases for objects: assuming no duplicate keys,
/// and order-independence.
fn same_value(left: &executable::Value, right: &executable::Value) -> bool {
    match (left, right) {
        (executable::Value::Null, executable::Value::Null) => true,
        (executable::Value::Enum(left), executable::Value::Enum(right)) => left == right,
        (executable::Value::Variable(left), executable::Value::Variable(right)) => left == right,
        (executable::Value::String(left), executable::Value::String(right)) => left == right,
        (executable::Value::Float(left), executable::Value::Float(right)) => left == right,
        (executable::Value::Int(left), executable::Value::Int(right)) => left == right,
        (executable::Value::Boolean(left), executable::Value::Boolean(right)) => left == right,
        (executable::Value::List(left), executable::Value::List(right)) => left
            .iter()
            .zip(right.iter())
            .all(|(left, right)| same_value(left, right)),
        (executable::Value::Object(left), executable::Value::Object(right))
            if left.len() == right.len() =>
        {
            // This check could miss out on keys that exist in `right`, but not in `left`, if `left` contains duplicate keys.
            // We assume that that doesn't happen. GraphQL does not support duplicate keys and
            // that is checked elsewhere in validation.
            left.iter().all(|(key, value)| {
                right
                    .iter()
                    .find(|(other_key, _)| key == other_key)
                    .is_some_and(|(_, other_value)| same_value(value, other_value))
            })
        }
        _ => false,
    }
}

fn same_output_type_shape(
    schema: &ValidFederationSchema,
    selection_a: &FieldSelection,
    selection_b: &FieldSelection,
) -> Result<bool, FederationError> {
    let field_a = selection_a.field.field_position.get(schema.schema())?;
    let field_b = selection_b.field.field_position.get(schema.schema())?;

    let mut type_a = &field_a.ty;
    let mut type_b = &field_b.ty;

    // Steps 3 and 4 of the spec text unwrap both types simultaneously down to the named type.
    // The apollo-rs representation of NonNull and Lists makes it tricky to follow the steps
    // exactly.
    //
    // Instead we unwrap lists and non-null lists first, which leaves just a named type or a
    // non-null named type...
    while !type_a.is_named() || !type_b.is_named() {
        // 4. If typeA or typeB is List.
        // 4a. If typeA or typeB is not List, return false.
        // 4b. Let typeA be the item type of typeA
        // 4c. Let typeB be the item type of typeB
        (type_a, type_b) = match (type_a, type_b) {
            (executable::Type::List(type_a), executable::Type::List(type_b))
            | (executable::Type::NonNullList(type_a), executable::Type::NonNullList(type_b)) => {
                (type_a.as_ref(), type_b.as_ref())
            }
            (executable::Type::List(_), _)
            | (_, executable::Type::List(_))
            | (executable::Type::NonNullList(_), _)
            | (_, executable::Type::NonNullList(_)) => return Ok(false),
            // Now it's a named type.
            (type_a, type_b) => (type_a, type_b),
        };
    }

    // Now we are down to two named types, we can check that they have the same nullability...
    let (type_a, type_b) = match (type_a, type_b) {
        (executable::Type::NonNullNamed(a), executable::Type::NonNullNamed(b)) => (a, b),
        (executable::Type::Named(a), executable::Type::Named(b)) => (a, b),
        _ => return Ok(false),
    };

    let def_a = schema.get_type(type_a.clone())?;
    let def_b = schema.get_type(type_b.clone())?;

    // 5. If typeA or typeB is Scalar or Enum.
    if let (
        TypeDefinitionPosition::Scalar(_) | TypeDefinitionPosition::Enum(_),
        TypeDefinitionPosition::Scalar(_) | TypeDefinitionPosition::Enum(_),
    ) = (&def_a, &def_b)
    {
        // 5a. If typeA and typeB are the same type return true, otherwise return false.
        return Ok(def_a == def_b);
    }

    // 6. If typeA or typeB is not a composite type, return false.
    Ok(is_composite(def_a) && is_composite(def_b))
}

/// A boolean that turns on after the first check.
struct OnceBool(std::cell::Cell<bool>);
impl OnceBool {
    fn new() -> Self {
        Self(false.into())
    }

    /// Returns `false` the first time it is called, then returns `true` forever.
    fn already_done(&self) -> bool {
        self.0.replace(true)
    }
}

/// Represents a merged field set that may or may not be valid.
struct MergedFieldSet {
    selections: Vec<Arc<FieldSelection>>,
    grouped_by_output_names: OnceCell<IndexMap<Name, Vec<Arc<FieldSelection>>>>,
    grouped_by_common_parents: OnceCell<Vec<Vec<Arc<FieldSelection>>>>,
    same_response_shape_guard: OnceBool,
    same_for_common_parents_guard: OnceBool,
}

impl MergedFieldSet {
    fn new(selections: Vec<Arc<FieldSelection>>) -> Self {
        Self {
            selections,
            grouped_by_output_names: Default::default(),
            grouped_by_common_parents: Default::default(),
            same_response_shape_guard: OnceBool::new(),
            same_for_common_parents_guard: OnceBool::new(),
        }
    }

    /// Given a set of fields, do all the fields that contribute to 1 output name have the same
    /// shape?
    ///
    /// This prevents leaf output fields from having an inconsistent type.
    fn same_response_shape_by_name(
        &self,
        validator: &mut FieldsInSetCanMerge<'_>,
    ) -> Result<bool, FederationError> {
        // No need to do this if this field set has been checked before.
        if self.same_response_shape_guard.already_done() {
            return Ok(true);
        }

        for fields_for_name in self.group_by_output_name().values() {
            let Some((field_a, rest)) = fields_for_name.split_first() else {
                // If there is only one field with a given output name, there is nothing to check.
                continue;
            };
            for field_b in rest {
                // Covers steps 3-5 of the spec algorithm.
                if !same_output_type_shape(validator.schema, field_a, field_b)? {
                    return Ok(false);
                }
            }

            let mut nested_selection_sets = fields_for_name
                .iter()
                .filter_map(|selection| selection.selection_set.as_ref())
                .peekable();
            if nested_selection_sets.peek().is_some() {
                let merged_set = expand_selections(nested_selection_sets);
                if !validator.same_response_shape_by_name(merged_set)? {
                    return Ok(false);
                }
            }
        }

        Ok(true)
    }

    /// Given a set of fields, do all the fields selecting from potentially overlapping types
    /// select from the same thing?
    ///
    /// This prevents selecting two different fields from the same type into the same name. That
    /// would be a contradiction because there would be no way to know which field takes precedence.
    fn same_for_common_parents_by_name(
        &self,
        validator: &mut FieldsInSetCanMerge<'_>,
    ) -> Result<bool, FederationError> {
        // No need to do this if this field set has been checked before.
        if self.same_for_common_parents_guard.already_done() {
            return Ok(true);
        }

        for fields_for_name in self.group_by_output_name().values() {
            let selection_for_name = validator.lookup(fields_for_name.clone());
            for fields_for_parents in selection_for_name.group_by_common_parents() {
                // 2bi. fieldA and fieldB must have identical field names.
                // 2bii. fieldA and fieldB must have identical sets of arguments.
                // The same arguments check is reflexive so we don't need to check all
                // combinations.
                let Some((field_a, rest)) = fields_for_parents.split_first() else {
                    continue;
                };
                for field_b in rest {
                    if !same_name_and_arguments(field_a, field_b)? {
                        return Ok(false);
                    }
                }

                let mut nested_selection_sets = fields_for_parents
                    .iter()
                    .filter_map(|selection| selection.selection_set.as_ref())
                    .peekable();
                if nested_selection_sets.peek().is_some() {
                    let merged_set = expand_selections(nested_selection_sets);
                    if !validator.same_for_common_parents_by_name(merged_set)? {
                        return Ok(false);
                    }
                }
            }
        }
        Ok(true)
    }

    fn group_by_output_name(&self) -> &IndexMap<Name, Vec<Arc<FieldSelection>>> {
        self.grouped_by_output_names.get_or_init(|| {
            let mut map = IndexMap::<_, Vec<_>>::new();
            for selection in &self.selections {
                map.entry(selection.field.response_name().clone())
                    .or_default()
                    .push(Arc::clone(selection));
            }
            map
        })
    }

    /// Returns potentially overlapping groups of fields. Fields overlap if they are selected from
    /// the same concrete type or if they are selected from an abstract type (future schema changes
    /// can make any abstract type overlap with any other type).
    fn group_by_common_parents(&self) -> &Vec<Vec<Arc<FieldSelection>>> {
        self.grouped_by_common_parents.get_or_init(|| {
            let mut abstract_parents = vec![];
            let mut concrete_parents = IndexMap::<_, Vec<_>>::new();
            for selection in self.selections.iter().cloned() {
                match &selection.field.field_position {
                    FieldDefinitionPosition::Object(object) => {
                        concrete_parents
                            .entry(object.type_name.clone())
                            .or_default()
                            .push(selection);
                    }
                    FieldDefinitionPosition::Interface(_) | FieldDefinitionPosition::Union(_) => {
                        abstract_parents.push(selection);
                    }
                }
            }

            if concrete_parents.is_empty() {
                vec![abstract_parents]
            } else {
                concrete_parents
                    .into_values()
                    .map(|mut group| {
                        group.extend(abstract_parents.iter().cloned());
                        group
                    })
                    .collect()
            }
        })
    }
}

/// For use as a hash map key, avoiding a clone of a potentially large array into the key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct FieldSelectionsId(u64);

impl FieldSelectionsId {
    fn new(selections: &[impl AsRef<FieldSelection>]) -> Self {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::Hash;
        use std::hash::Hasher;

        // We can use the unseeded default hasher because the output will be
        // hashed again with a randomly seeded hasher and still lead to unpredictable output.
        let mut hasher = DefaultHasher::new();
        selections.len().hash(&mut hasher);
        for selection in selections {
            selection.as_ref().field.hash(&mut hasher);
        }
        Self(hasher.finish())
    }
}

/// Implements the `FieldsInSetCanMerge()` validation.
/// https://spec.graphql.org/draft/#sec-Field-Selection-Merging
///
/// This uses the [validation algorithm described by XING][0] ([archived][1]), which
/// scales much better with larger selection sets that may have many overlapping fields,
/// and with widespread use of fragments.
///
/// [0]: https://tech.new-work.se/graphql-overlapping-fields-can-be-merged-fast-ea6e92e0a01
/// [1]: https://web.archive.org/web/20240208084612/https://tech.new-work.se/graphql-overlapping-fields-can-be-merged-fast-ea6e92e0a01
pub(super) struct FieldsInSetCanMerge<'s> {
    schema: &'s ValidFederationSchema,
    /// Stores merged field sets.
    ///
    /// The value is an Rc because it needs to have an independent lifetime from `self`,
    /// so the cache can be updated while a field set is borrowed.
    cache: HashMap<FieldSelectionsId, Rc<MergedFieldSet>>,
}

impl<'s> FieldsInSetCanMerge<'s> {
    pub(super) fn new(schema: &'s ValidFederationSchema) -> Self {
        Self {
            schema,
            cache: Default::default(),
        }
    }

    pub(super) fn validate_selection_set(
        &mut self,
        selection_set: &SelectionSet,
    ) -> Result<bool, FederationError> {
        let fields = expand_selections(std::iter::once(selection_set));
        let set = self.lookup(fields);
        Ok(set.same_response_shape_by_name(self)? && set.same_for_common_parents_by_name(self)?)
    }

    fn lookup(&mut self, selections: Vec<Arc<FieldSelection>>) -> Rc<MergedFieldSet> {
        let id = FieldSelectionsId::new(&selections);
        self.cache
            .entry(id)
            .or_insert_with(|| Rc::new(MergedFieldSet::new(selections)))
            .clone()
    }

    fn same_for_common_parents_by_name(
        &mut self,
        selections: Vec<Arc<FieldSelection>>,
    ) -> Result<bool, FederationError> {
        self.lookup(selections)
            .same_for_common_parents_by_name(self)
    }

    fn same_response_shape_by_name(
        &mut self,
        selections: Vec<Arc<FieldSelection>>,
    ) -> Result<bool, FederationError> {
        self.lookup(selections).same_response_shape_by_name(self)
    }
}
