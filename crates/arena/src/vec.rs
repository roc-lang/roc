use crate::{Arena, ArenaRefMut};
use core::{
    alloc::Layout,
    mem::{self, align_of, size_of},
};

pub trait AsU32: Default + Copy + Sized + PartialEq + Ord {
    fn as_u32(self) -> u32;
}

impl AsU32 for u32 {
    fn as_u32(self) -> u32 {
        self
    }
}

impl AsU32 for u16 {
    fn as_u32(self) -> u32 {
        self as u32
    }
}

impl AsU32 for u8 {
    fn as_u32(self) -> u32 {
        self as u32
    }
}

pub type Vec32<'a, T> = Vec<'a, T, u32>;
pub type Vec16<'a, T> = Vec<'a, T, u16>;
pub type Vec8<'a, T> = Vec<'a, T, u8>;

pub struct Vec<'a, T, Len: AsU32> {
    start: ArenaRefMut<'a, T>,
    len: Len,
    capacity: Len,
}

impl<'a, T, Len: AsU32> Vec<'a, T, Len> {
    pub fn with_capacity_in(capacity: Len, arena: &'a mut Arena) -> Self {
        let start = unsafe {
            arena
                .alloc_layout(Layout::from_size_align_unchecked(
                    capacity.as_u32() as usize * mem::size_of::<T>(),
                    align_of::<T>(),
                ))
                .cast()
        };

        Self {
            start,
            len: Default::default(),
            capacity,
        }
    }

    pub fn capacity(&self) -> Len {
        self.capacity
    }

    pub fn len(&self) -> Len {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == Default::default()
    }

    pub fn get(&self, arena: &'a Arena<'a>, index: Len) -> Option<&'a T> {
        if index < self.len {
            #[cfg(debug_assertions)]
            {
                self.start.debug_verify_arena(arena, "Vec::get");
            }

            Some(unsafe {
                arena.get_unchecked(self.start.add_bytes(size_of::<T>() as u32 * index.as_u32()))
            })
        } else {
            None
        }
    }
}
