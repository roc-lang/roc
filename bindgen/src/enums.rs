use crate::types::RocTagUnion;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, Default)]
struct EnumId(u64);

impl EnumId {
    pub fn to_name(self) -> String {
        format!("U{}", self.0)
    }
}

/// Whenever we register a new tag union type,
/// give it a unique and short name (e.g. U1, U2, U3...)
/// and then from then on, whenever we ask for that
/// same record type, return the same name.
#[derive(Default)]
pub struct Enums {
    by_tag_union: HashMap<RocTagUnion, EnumId>,
    by_id: HashMap<EnumId, RocTagUnion>,
    next_id: EnumId,
}

impl Enums {
    pub fn get_name(&mut self, rec_type: &RocTagUnion) -> String {
        match self.by_tag_union.get(rec_type) {
            Some(struct_id) => struct_id.to_name(),
            None => self.next_id().to_name(),
        }
    }

    fn next_id(&mut self) -> EnumId {
        self.next_id.0 += 1;

        self.next_id
    }
}
