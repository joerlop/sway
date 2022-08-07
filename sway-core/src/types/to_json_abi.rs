use sway_types::TypeDeclaration;

pub trait ToJsonAbi {
    type Output;

    fn generate_json_abi(&self) -> Self::Output;
}

pub trait ToJsonAbiFlat {
    type FlatOutput;

    fn generate_json_abi_flat(&self, types: &mut Vec<TypeDeclaration>) -> Self::FlatOutput;
}
