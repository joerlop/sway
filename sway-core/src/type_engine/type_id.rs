use std::fmt;
use sway_types::{Span, Spanned, TypeApplication, TypeDeclaration};

use crate::types::*;

use super::*;

/// A identifier to uniquely refer to our type terms
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeId(usize);

impl std::ops::Deref for TypeId {
    type Target = usize;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&look_up_type_id(*self).to_string())
    }
}

impl fmt::Debug for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&look_up_type_id(*self).to_string())
    }
}

impl From<usize> for TypeId {
    fn from(o: usize) -> Self {
        TypeId(o)
    }
}

impl UnresolvedTypeCheck for TypeId {
    fn check_for_unresolved_types(&self) -> Vec<CompileError> {
        use TypeInfo::*;
        let span_override = if let TypeInfo::Ref(_, span) = look_up_type_id_raw(*self) {
            Some(span)
        } else {
            None
        };
        match look_up_type_id(*self) {
            UnknownGeneric { name } => vec![CompileError::UnableToInferGeneric {
                ty: name.as_str().to_string(),
                span: span_override.unwrap_or_else(|| name.span()),
            }],
            _ => vec![],
        }
    }
}

impl JsonAbiString for TypeId {
    fn json_abi_str(&self) -> String {
        look_up_type_id(*self).json_abi_str()
    }
}

impl ToJsonAbi for TypeId {
    type Output = Option<Vec<Property>>;

    fn generate_json_abi(&self) -> Self::Output {
        match look_up_type_id(*self) {
            TypeInfo::Array(type_id, _) => Some(vec![Property {
                name: "__array_element".to_string(),
                type_field: type_id.json_abi_str(),
                components: type_id.generate_json_abi(),
                type_arguments: type_id
                    .get_type_parameters()
                    .map(|v| v.iter().map(TypeParameter::generate_json_abi).collect()),
            }]),
            TypeInfo::Enum { variant_types, .. } => Some(
                variant_types
                    .iter()
                    .map(|x| x.generate_json_abi())
                    .collect(),
            ),
            TypeInfo::Struct { fields, .. } => {
                Some(fields.iter().map(|x| x.generate_json_abi()).collect())
            }
            TypeInfo::Tuple(fields) => Some(fields.iter().map(|x| x.generate_json_abi()).collect()),
            _ => None,
        }
    }
}

impl TypeId {
    pub(crate) fn generate_json_abi_flat(
        &self,
        types: &mut Vec<TypeDeclaration>,
        resolved_custom: TypeId,
    ) -> Option<Vec<TypeApplication>> {
        match look_up_type_id(*self) {
            TypeInfo::Struct { fields, .. } => {
                let new_types = fields
                    .iter()
                    .map(|x| TypeDeclaration {
                        type_id: *x.generic_type_id,
                        type_field: x.generic_type_id.json_abi_str(),
                        components: x.generic_type_id.generate_json_abi_flat(types, x.type_id),
                        type_parameters: None,
                    })
                    .collect::<Vec<_>>();
                types.extend(new_types);

                Some(
                    fields
                        .iter()
                        .map(|x| TypeApplication {
                            name: x.name.to_string(),
                            type_field: *x.generic_type_id,
                            type_arguments: x.type_id.get_type_parameters().map(|v| {
                                v.iter()
                                    .map(|v| v.generate_json_abi_flat(types))
                                    .collect::<Vec<_>>()
                            }),
                        })
                        .collect(),
                )
            }
            TypeInfo::Array(type_id, _) => {
                let element_type = TypeDeclaration {
                    type_id: *type_id,
                    type_field: type_id.json_abi_str(),
                    components: type_id.generate_json_abi_flat(types, type_id),
                    type_parameters: None, /*type_id.get_type_parameters().map(|v| {
                                               v.iter()
                                                   .map(|v| v.generate_json_abi_flat(types))
                                                   .collect::<Vec<_>>()
                                           })*/
                };
                types.push(element_type);
                Some(vec![TypeApplication {
                    name: "__array_element".to_string(),
                    type_field: *type_id,
                    type_arguments: None,
                }])
            }
            TypeInfo::Custom {
                name,
                type_arguments,
            } => {
                match look_up_type_id(resolved_custom) {
                    TypeInfo::Struct { fields, .. } => {
                        let new_types = fields
                            .iter()
                            .map(|x| TypeDeclaration {
                                type_id: *x.generic_type_id,
                                type_field: x.generic_type_id.json_abi_str(),
                                components: x
                                    .generic_type_id
                                    .generate_json_abi_flat(types, x.type_id),
                                type_parameters: None,
                            })
                            .collect::<Vec<_>>();
                        types.extend(new_types);

                        Some(
                            fields
                                .iter()
                                .map(|x| TypeApplication {
                                    name: x.name.to_string(),
                                    type_field: *x.generic_type_id,
                                    type_arguments: x.type_id.get_type_parameters().map(|v| {
                                        v.iter()
                                            .map(|v| v.generate_json_abi_flat(types))
                                            .collect::<Vec<_>>()
                                    }),
                                })
                                .collect(),
                        )
                    }
                    _ => None,
                }

                //                dbg!(name);
                //                dbg!(type_arguments);
                //                dbg!(self);
                //                dbg!(resolved_custom);
                //                None
            }
            _ => None,
        }
    }
}

impl ReplaceSelfType for TypeId {
    fn replace_self_type(&mut self, self_type: TypeId) {
        match look_up_type_id(*self) {
            TypeInfo::SelfType => {
                *self = self_type;
            }
            TypeInfo::Enum {
                mut type_parameters,
                mut variant_types,
                ..
            } => {
                for type_parameter in type_parameters.iter_mut() {
                    type_parameter.replace_self_type(self_type);
                }
                for variant_type in variant_types.iter_mut() {
                    variant_type.replace_self_type(self_type);
                }
            }
            TypeInfo::Struct {
                mut type_parameters,
                mut fields,
                ..
            } => {
                for type_parameter in type_parameters.iter_mut() {
                    type_parameter.replace_self_type(self_type);
                }
                for field in fields.iter_mut() {
                    field.replace_self_type(self_type);
                }
            }
            TypeInfo::Ref(mut type_id, _) => {
                type_id.replace_self_type(self_type);
            }
            TypeInfo::Tuple(mut type_arguments) => {
                for type_argument in type_arguments.iter_mut() {
                    type_argument.replace_self_type(self_type);
                }
            }
            TypeInfo::Custom { type_arguments, .. } => {
                if let Some(mut type_arguments) = type_arguments {
                    for type_argument in type_arguments.iter_mut() {
                        type_argument.replace_self_type(self_type);
                    }
                }
            }
            TypeInfo::Array(mut type_id, _) => {
                type_id.replace_self_type(self_type);
            }
            TypeInfo::Storage { mut fields } => {
                for field in fields.iter_mut() {
                    field.replace_self_type(self_type);
                }
            }
            TypeInfo::Unknown
            | TypeInfo::UnknownGeneric { .. }
            | TypeInfo::Str(_)
            | TypeInfo::UnsignedInteger(_)
            | TypeInfo::Boolean
            | TypeInfo::ContractCaller { .. }
            | TypeInfo::Byte
            | TypeInfo::B256
            | TypeInfo::Numeric
            | TypeInfo::Contract
            | TypeInfo::ErrorRecovery => {}
        }
    }
}

impl TypeId {
    pub(crate) fn update_type(&mut self, type_mapping: &TypeMapping, span: &Span) {
        *self = match look_up_type_id(*self).matches_type_parameter(type_mapping) {
            Some(matching_id) => insert_type(TypeInfo::Ref(matching_id, span.clone())),
            None => {
                let ty = TypeInfo::Ref(insert_type(look_up_type_id_raw(*self)), span.clone());
                insert_type(ty)
            }
        };
    }

    pub(crate) fn get_type_parameters(&self) -> Option<Vec<TypeParameter>> {
        match look_up_type_id(*self) {
            TypeInfo::Enum {
                type_parameters, ..
            } => (!type_parameters.is_empty()).then(|| type_parameters),
            TypeInfo::Struct {
                type_parameters, ..
            } => (!type_parameters.is_empty()).then(|| type_parameters),
            _ => None,
        }
    }
}
