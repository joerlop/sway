use super::{TypeEngine, TypeMapping};

pub(crate) trait CopyTypes {
    fn copy_types(&mut self, type_engine: &mut TypeEngine, type_mapping: &TypeMapping);
}
