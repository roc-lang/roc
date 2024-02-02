use core::{
    alloc::Layout,
    marker::PhantomData,
    mem::{self, align_of, size_of},
};

use crate::{arena::Arena, ArenaRefMut};

pub struct Vec<'a, T> {
    start: ArenaRefMut<'a, T>,
    len: u32,
    capacity: u32,

    // Below this line is zero-cost in --release builds
    _phantom: PhantomData<&'a T>,
}

impl<'a, T> Vec<'a, T> {
    pub fn with_capacity_in(capacity: u32, arena: &'a mut Arena) -> Self {
        let start = arena
            .alloc_layout(unsafe {
                Layout::from_size_align_unchecked(
                    capacity as usize * mem::size_of::<T>(),
                    align_of::<T>(),
                )
            })
            .cast();

        Self {
            start,
            len: 0,
            capacity,
            _phantom: PhantomData,
        }
    }
    pub fn capacity(&self) -> u32 {
        self.capacity
    }

    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn get(&self, arena: &'a Arena<'a>, index: u32) -> Option<&'a T> {
        if index < self.len {
            #[cfg(debug_assertions)]
            {
                self.start.debug_verify_arena(arena, "Vec::get");
            }

            Some(unsafe {
                arena.get_unchecked(self.start.add_bytes(size_of::<T>() as u32 * index))
            })
        } else {
            None
        }
    }
}

#[cfg(debug_assertions)]
fn debug_verify_arena<'a>(
    self_arena: &Arena<'a>,
    other_arena: &Arena<'a>,
    operation: &'static str,
) {
    // This only does anything in debug builds. In optimized builds, we don't do it.
    if (self_arena as *const _) != (other_arena as *const _) {
        panic!("Vec::{operation} was called passing a different arena from the one this Vec was created with!");
    }
}
