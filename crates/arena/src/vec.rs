/// To conserve memory, these store only a pointer to the first type variable, and then
/// compute the others on the fly by offsetting from that one pointer by a multiple
/// of capacity. When iterating over all the entries in one lane, this doesn't really
/// matter because it only affects the initialization of the loop. However, when doing
/// individual lookups (e.g. get, get_mut, get_unchecked) it means that individual
/// lookups on the first type variable are less expensive than subsequent ones, because
/// subsequent ones require multiplication. As such, put whatever needs the most
/// individual lookups as the first type variable!
use crate::{Arena, ArenaRefMut};
use core::{
    alloc::Layout,
    mem::{self, align_of, size_of},
    ops::Add,
};

pub type Vec32<'a, T> = Vec<'a, T, u32>;
pub type Vec16<'a, T> = Vec<'a, T, u16>;
pub type Vec8<'a, T> = Vec<'a, T, u8>;

pub trait AsBool {
    fn as_bool(self) -> bool;
}

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
            Some(unsafe { self.get_unchecked(arena, index) })
        } else {
            None
        }
    }

    pub unsafe fn get_unchecked(&self, arena: &'a Arena<'a>, index: Len) -> &'a T {
        #[cfg(debug_assertions)]
        {
            self.start.debug_verify_arena(arena, "Vec::get");

            // We still verify in debug builds!
            assert!(index < self.len);
        }

        arena.get_unchecked(self.start.add_bytes(size_of::<T>() as u32 * index.as_u32()))
    }

    pub fn write<'b>(
        &self,
        self_arena: &Arena<'a>,
        dest: &mut Vec<'b, T, impl AsU32>,
        dest_arena: &mut Arena<'b>,
    ) {
        // This will fail if dest.len + self.len overflows, so we no longer need to worry about that.
        dest.reserve(self.len);

        let src_ptr = self.start.as_ref(self_arena) as *const _ as *const u8;
        let dest_ptr = dest.start.as_mut(dest_arena) as *mut _ as *mut u8;

        unsafe { core::ptr::copy_nonoverlapping(src_ptr, dest_ptr, self.len.as_u32() as usize) }
    }

    fn reserve(&self, len: impl AsU32) {
        let todo = todo!("need to check for capacity overflow!");
    }
}

pub trait AsU32:
    Default + Copy + Sized + PartialEq + Ord + From<u8> + Add<Self> + From<<Self as Add>::Output>
{
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
