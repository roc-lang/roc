use crate::vec::{AsU32, Vec};
use crate::Arena;

pub type String32<'a, T> = Vec<'a, T, u32>;
pub type String16<'a, T> = Vec<'a, T, u16>;
pub type String8<'a, T> = Vec<'a, T, u8>;

pub struct String<'a, Len: AsU32> {
    vec: Vec<'a, u8, Len>,
}

impl<'a, Len: AsU32> String<'a, Len> {
    pub fn with_capacity_in(capacity: Len, arena: &'a mut Arena) -> Self {
        Self {
            vec: Vec::with_capacity_in(capacity, arena),
        }
    }

    pub fn capacity(&self) -> Len {
        self.vec.capacity()
    }

    pub fn len(&self) -> Len {
        self.vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    pub fn get_utf8_byte(&self, arena: &'a Arena<'a>, index: Len) -> Option<&'a u8> {
        self.vec.get(arena, index)
    }
}
