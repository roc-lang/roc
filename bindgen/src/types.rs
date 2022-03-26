#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RocType {
    Str,
    Bool,
    List(Box<RocType>),
    TagUnion(Vec<(String, Vec<RocType>)>),
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
}
