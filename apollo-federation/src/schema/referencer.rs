use apollo_compiler::schema::Name;
use indexmap::IndexMap;
use indexmap::IndexSet;

use crate::error::FederationError;
use crate::error::SingleFederationError;
use crate::schema::position::DirectiveArgumentPosition;
use crate::schema::position::EnumPosition;
use crate::schema::position::EnumValuePosition;
use crate::schema::position::InputObjectFieldPosition;
use crate::schema::position::InputObjectPosition;
use crate::schema::position::InterfaceFieldArgumentPosition;
use crate::schema::position::InterfaceFieldPosition;
use crate::schema::position::InterfacePosition;
use crate::schema::position::ObjectFieldArgumentPosition;
use crate::schema::position::ObjectFieldPosition;
use crate::schema::position::ObjectPosition;
use crate::schema::position::ScalarPosition;
use crate::schema::position::SchemaDefinitionPosition;
use crate::schema::position::SchemaRootPosition;
use crate::schema::position::UnionPosition;
use crate::schema::position::UnionTypenamePosition;

#[derive(Debug, Clone, Default)]
pub(crate) struct Referencers {
    pub(crate) scalar_types: IndexMap<Name, ScalarTypeReferencers>,
    pub(crate) object_types: IndexMap<Name, ObjectTypeReferencers>,
    pub(crate) interface_types: IndexMap<Name, InterfaceTypeReferencers>,
    pub(crate) union_types: IndexMap<Name, UnionTypeReferencers>,
    pub(crate) enum_types: IndexMap<Name, EnumTypeReferencers>,
    pub(crate) input_object_types: IndexMap<Name, InputObjectTypeReferencers>,
    pub(crate) directives: IndexMap<Name, DirectiveReferencers>,
}

impl Referencers {
    pub(crate) fn contains_type_name(&self, name: &str) -> bool {
        self.scalar_types.contains_key(name)
            || self.object_types.contains_key(name)
            || self.interface_types.contains_key(name)
            || self.union_types.contains_key(name)
            || self.enum_types.contains_key(name)
            || self.input_object_types.contains_key(name)
    }

    pub(crate) fn get_scalar_type(
        &self,
        name: &str,
    ) -> Result<&ScalarTypeReferencers, FederationError> {
        self.scalar_types.get(name).ok_or_else(|| {
            SingleFederationError::Internal {
                message: "Scalar type referencers unexpectedly missing type".to_owned(),
            }
            .into()
        })
    }

    pub(crate) fn get_object_type(
        &self,
        name: &str,
    ) -> Result<&ObjectTypeReferencers, FederationError> {
        self.object_types.get(name).ok_or_else(|| {
            SingleFederationError::Internal {
                message: "Object type referencers unexpectedly missing type".to_owned(),
            }
            .into()
        })
    }

    pub(crate) fn get_interface_type(
        &self,
        name: &str,
    ) -> Result<&InterfaceTypeReferencers, FederationError> {
        self.interface_types.get(name).ok_or_else(|| {
            SingleFederationError::Internal {
                message: "Interface type referencers unexpectedly missing type".to_owned(),
            }
            .into()
        })
    }

    pub(crate) fn get_union_type(
        &self,
        name: &str,
    ) -> Result<&UnionTypeReferencers, FederationError> {
        self.union_types.get(name).ok_or_else(|| {
            SingleFederationError::Internal {
                message: "Union type referencers unexpectedly missing type".to_owned(),
            }
            .into()
        })
    }

    pub(crate) fn get_enum_type(
        &self,
        name: &str,
    ) -> Result<&EnumTypeReferencers, FederationError> {
        self.enum_types.get(name).ok_or_else(|| {
            SingleFederationError::Internal {
                message: "Enum type referencers unexpectedly missing type".to_owned(),
            }
            .into()
        })
    }

    pub(crate) fn get_input_object_type(
        &self,
        name: &str,
    ) -> Result<&InputObjectTypeReferencers, FederationError> {
        self.input_object_types.get(name).ok_or_else(|| {
            SingleFederationError::Internal {
                message: "Input object type referencers unexpectedly missing type".to_owned(),
            }
            .into()
        })
    }

    pub(crate) fn get_directive(
        &self,
        name: &str,
    ) -> Result<&DirectiveReferencers, FederationError> {
        self.directives.get(name).ok_or_else(|| {
            SingleFederationError::Internal {
                message: "Directive referencers unexpectedly missing directive".to_owned(),
            }
            .into()
        })
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ScalarTypeReferencers {
    pub(crate) object_fields: IndexSet<ObjectFieldPosition>,
    pub(crate) object_field_arguments: IndexSet<ObjectFieldArgumentPosition>,
    pub(crate) interface_fields: IndexSet<InterfaceFieldPosition>,
    pub(crate) interface_field_arguments: IndexSet<InterfaceFieldArgumentPosition>,
    pub(crate) union_fields: IndexSet<UnionTypenamePosition>,
    pub(crate) input_object_fields: IndexSet<InputObjectFieldPosition>,
    pub(crate) directive_arguments: IndexSet<DirectiveArgumentPosition>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ObjectTypeReferencers {
    pub(crate) schema_roots: IndexSet<SchemaRootPosition>,
    pub(crate) object_fields: IndexSet<ObjectFieldPosition>,
    pub(crate) interface_fields: IndexSet<InterfaceFieldPosition>,
    pub(crate) union_types: IndexSet<UnionPosition>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct InterfaceTypeReferencers {
    pub(crate) object_types: IndexSet<ObjectPosition>,
    pub(crate) object_fields: IndexSet<ObjectFieldPosition>,
    pub(crate) interface_types: IndexSet<InterfacePosition>,
    pub(crate) interface_fields: IndexSet<InterfaceFieldPosition>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct UnionTypeReferencers {
    pub(crate) object_fields: IndexSet<ObjectFieldPosition>,
    pub(crate) interface_fields: IndexSet<InterfaceFieldPosition>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct EnumTypeReferencers {
    pub(crate) object_fields: IndexSet<ObjectFieldPosition>,
    pub(crate) object_field_arguments: IndexSet<ObjectFieldArgumentPosition>,
    pub(crate) interface_fields: IndexSet<InterfaceFieldPosition>,
    pub(crate) interface_field_arguments: IndexSet<InterfaceFieldArgumentPosition>,
    pub(crate) input_object_fields: IndexSet<InputObjectFieldPosition>,
    pub(crate) directive_arguments: IndexSet<DirectiveArgumentPosition>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct InputObjectTypeReferencers {
    pub(crate) object_field_arguments: IndexSet<ObjectFieldArgumentPosition>,
    pub(crate) interface_field_arguments: IndexSet<InterfaceFieldArgumentPosition>,
    pub(crate) input_object_fields: IndexSet<InputObjectFieldPosition>,
    pub(crate) directive_arguments: IndexSet<DirectiveArgumentPosition>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct DirectiveReferencers {
    pub(crate) schema: Option<SchemaDefinitionPosition>,
    pub(crate) scalar_types: IndexSet<ScalarPosition>,
    pub(crate) object_types: IndexSet<ObjectPosition>,
    pub(crate) object_fields: IndexSet<ObjectFieldPosition>,
    pub(crate) object_field_arguments: IndexSet<ObjectFieldArgumentPosition>,
    pub(crate) interface_types: IndexSet<InterfacePosition>,
    pub(crate) interface_fields: IndexSet<InterfaceFieldPosition>,
    pub(crate) interface_field_arguments: IndexSet<InterfaceFieldArgumentPosition>,
    pub(crate) union_types: IndexSet<UnionPosition>,
    pub(crate) enum_types: IndexSet<EnumPosition>,
    pub(crate) enum_values: IndexSet<EnumValuePosition>,
    pub(crate) input_object_types: IndexSet<InputObjectPosition>,
    pub(crate) input_object_fields: IndexSet<InputObjectFieldPosition>,
    pub(crate) directive_arguments: IndexSet<DirectiveArgumentPosition>,
}
