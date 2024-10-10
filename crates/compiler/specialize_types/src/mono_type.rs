use core::fmt;

/// A slice into the Vec<T> of MonoTypes
///
/// The starting position is a u32 which should be plenty
/// We limit slices to u16::MAX = 65535 elements
pub struct MonoSlice<T> {
    pub start: u32,
    pub length: u16,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Copy for MonoSlice<T> {}

impl<T> Clone for MonoSlice<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> std::fmt::Debug for MonoSlice<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "MonoSlice {{ start: {}, length: {} }}",
            self.start, self.length
        )
    }
}

#[derive(Clone, Copy, Debug)]
struct Symbol {
    inner: u64,
}

#[derive(Clone, Copy, Debug)]
struct MonoTypeId {
    inner: u32,
}

#[derive(Clone, Copy, Debug)]
pub enum MonoType {
    Apply(Symbol, MonoSlice<MonoTypeId>),
    Func {
        args: MonoSlice<MonoTypeId>,
        ret: MonoTypeId,
    },
    Record(RecordFields),
    Tuple(TupleElems),
    TagUnion(UnionTags),
    EmptyRecord,
    EmptyTuple,
    EmptyTagUnion,
}

#[derive(Clone, Copy, Debug)]
pub struct RecordFields {
    pub length: u16,
    pub field_names_start: u32,
    pub field_type_ids_start: u32,
    pub field_types_start: u32,
}

#[derive(Clone, Copy, Debug)]
pub struct TupleElems {
    pub length: u16,
    pub elem_index_start: u32,
    pub type_ids_start: u32,
}

#[derive(Clone, Copy, Debug)]
pub struct UnionTags {
    pub length: u16,
    pub labels_start: u32,
    pub values_start: u32,
}
