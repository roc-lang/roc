use core::mem;
use roc_std::{RocDec, RocList, RocStr};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RocType {
    Str,
    Bool,
    List(Box<RocType>),
    RocBox(Box<RocType>),
    TagUnion(RocTagUnion),
    Record(RocRecord),
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    U128,
    F32,
    F64,
    Dec,
}

impl RocType {
    pub fn alignment(&self, ptr_alignment: usize) -> usize {
        match self {
            RocType::Str | RocType::List(_) | RocType::RocBox(_) => ptr_alignment,
            RocType::Dec => mem::align_of::<RocDec>(),
            RocType::Bool => mem::align_of::<bool>(),
            RocType::TagUnion(tag_union) => tag_union.alignment(ptr_alignment),
            RocType::Record(record) => record.alignment(ptr_alignment),
            RocType::I8 => mem::align_of::<i8>(),
            RocType::U8 => mem::align_of::<u8>(),
            RocType::I16 => mem::align_of::<i16>(),
            RocType::U16 => mem::align_of::<u16>(),
            RocType::I32 => mem::align_of::<i32>(),
            RocType::U32 => mem::align_of::<u32>(),
            RocType::I64 => mem::align_of::<i64>(),
            RocType::U64 => mem::align_of::<u64>(),
            RocType::I128 => mem::align_of::<i128>(),
            RocType::U128 => mem::align_of::<u128>(),
            RocType::F32 => mem::align_of::<f32>(),
            RocType::F64 => mem::align_of::<f64>(),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct RocRecord {
    fields: Vec<(String, Box<RocType>)>,
}

impl RocRecord {
    pub fn new(fields: Vec<(String, Box<RocType>)>) -> Self {
        Self { fields }
    }

    pub fn into_fields(self) -> Vec<(String, Box<RocType>)> {
        self.fields
    }

    pub fn field_names(&self) -> Vec<&str> {
        self.fields
            .iter()
            .map(|(field, _)| field.as_str())
            .collect()
    }

    pub fn alignment(&self, ptr_alignment: usize) -> usize {
        let mut align = 0;

        for (_, field_type) in self.fields.iter() {
            align = align.max(field_type.alignment(ptr_alignment))
        }

        align
    }

    /// Use struct ordering, taking into account alignment and alphabetization.
    pub fn use_struct_ordering(&mut self, ptr_alignment: usize) {
        self.fields.sort_by(|(field1, type1), (field2, type2)| {
            let align1 = type1.alignment(ptr_alignment);
            let align2 = type2.alignment(ptr_alignment);

            if align1 == align2 {
                field1.cmp(field2)
            } else {
                // Sort by *descending* alignment; highest alignments should go first!
                align2.cmp(&align1)
            }
        });
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct RocTagUnion {
    tags: Vec<(String, Vec<RocType>)>,
}

impl RocTagUnion {
    pub fn new(tags: Vec<(String, Vec<RocType>)>) -> Self {
        Self { tags }
    }

    pub fn into_tags(self) -> Vec<(String, Vec<RocType>)> {
        self.tags
    }

    pub fn alignment(&self, ptr_alignment: usize) -> usize {
        let mut align = 0;

        for (_, args) in self.tags.iter() {
            for arg in args {
                align = align.max(arg.alignment(ptr_alignment));
            }
        }

        align
    }
}

#[test]
fn field_order_str() {
    use RocType::*;

    // These all have the same alignment, so they should be sorted alphabetically.

    let mut rec = RocRecord::new(vec![
        ("second".to_string(), Box::new(Str)),
        ("first".to_string(), Box::new(Str)),
        ("third".to_string(), Box::new(Str)),
    ]);

    rec.use_struct_ordering(mem::align_of::<String>());

    assert_eq!(vec!["first", "second", "third"], rec.field_names());
}

#[test]
fn field_order_diff_align() {
    use RocType::*;

    // These have different alignments, and alignment takes precedence over field names.

    let mut rec = RocRecord::new(vec![
        ("first".to_string(), Box::new(U8)),
        ("second".to_string(), Box::new(I32)),
        ("third".to_string(), Box::new(Str)),
    ]);

    rec.use_struct_ordering(mem::align_of::<String>());

    assert_eq!(vec!["third", "second", "first"], rec.field_names());
}
