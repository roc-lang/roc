use crate::types::{RocRecord, RocType};
use std::collections::HashMap;

#[derive(Copy, Clone, Debug)]
struct StructId(u64);

impl StructId {
    pub fn to_name(self) -> String {
        format!("S{}", self.0)
    }
}

/// Whenever we register a new record type,
/// give it a unique and short name (e.g. S1, S2, S3...)
/// and then from then on, whenever we ask for that
/// same record type, return the same name.
pub struct Structs {
    by_record: HashMap<RocRecord, StructId>,
    next_id: StructId,
}

impl Structs {
    pub fn get_name(&mut self, rec_type: &RocRecord) -> String {
        match self.by_record.get(rec_type) {
            Some(struct_id) => struct_id.to_name(),
            None => self.next_id().to_name(),
        }
    }

    fn next_id(&mut self) -> StructId {
        self.next_id.0 += 1;

        self.next_id
    }
}
