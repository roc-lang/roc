use crate::arena::Arena;
use core::{alloc::Layout, ffi::c_void, fmt::Debug, mem, ptr};
use roc_error_macros::{internal_error, unrecoverable};

#[derive(Default, Debug)]
pub struct IdMap2<'a, Len, V1, V2> {
    len: Len,
    capacity: Len,
    /// Offset into the arena
    start1: usize,
    start2: usize,
}

pub struct Id<Len> {
    index: Len,

    #[cfg(debug_assertions)]
    owner: *const c_void,
}

impl<'a, Len: Into<usize> + TryFrom<usize> + Debug, V1, V2> IdMap2<'a, Len, V1, V2> {
    pub fn with_capacity(arena: &'a mut Arena, capacity: Len) -> Self {
        let capacity_usize: usize = capacity.into();
        let (layout, offset) = Layout::array::<V1>(capacity_usize)
            .and_then(|layout| layout.extend(Layout::array::<V2>(capacity_usize)))
            .unwrap_or_else(|_| internal_error!());
        let start1 = arena.alloc_layout(layout).byte_offset();
        let start2 = start1 + offset;

        Self {
            len: 0,
            capacity,
            start1,
            start2,
        }
    }

    pub fn insert(&'a mut self, arena: &'a mut Arena, v1: V1, v2: V2) -> Id<Len> {
        let answer: Id<Len> = Id {
            index: self.len,

            #[cfg(debug_assertions)]
            owner: self.as_ptr() as *const c_void,
        };
        let index: usize = self.len.into();

        unsafe {
            ptr::write(
                (arena.at_byte_offset_mut(self.start1) as *mut V1).add(index),
                v1,
            );
            ptr::write(
                (arena.at_byte_offset_mut(self.start2) as *mut V2).add(index),
                v2,
            );
        }

        match index.checked_add(1).and_then(|len| len.try_into().ok()) {
            Some(len) => {
                self.len = len;
            }
            None => {
                unrecoverable!("An internal collection overflowed its maximum capacity.");
            }
        }
    }

    pub fn get_first(&'a self, arena: &'a Arena, id: Id<Len>) -> &'a V1 {
        let index: usize = id.index.into();

        #[cfg(debug_assertions)]
        {
            self.check_owner(id);
        }

        unsafe {
            &*arena
                .at_byte_offset(self.start1 + (id.index * mem::size_of::<V1>()))
                .cast()
        }
    }

    pub fn get_first_mut(&'a self, arena: &'a mut Arena, id: Id<Len>) -> &'a mut V1 {
        let index: usize = id.index.into();

        #[cfg(debug_assertions)]
        {
            self.check_owner(id);
        }

        unsafe {
            &mut *arena
                .at_byte_offset_mut(self.start1 + (id.index * mem::size_of::<V1>()))
                .cast()
        }
    }

    pub fn get_second(&'a self, arena: &'a Arena, id: Id<Len>) -> &'a V2 {
        let index: usize = id.index.into();

        #[cfg(debug_assertions)]
        {
            self.check_owner(id);
        }

        unsafe {
            &*arena
                .at_byte_offset(self.start2 + (id.index * mem::size_of::<V2>()))
                .cast()
        }
    }

    pub fn get_second_mut(&'a self, arena: &'a mut Arena, id: Id<Len>) -> &'a mut V2 {
        let index: usize = id.index.into();

        #[cfg(debug_assertions)]
        {
            self.check_owner(id);
        }

        unsafe {
            &mut *arena
                .at_byte_offset_mut(self.start2 + (id.index * mem::size_of::<V2>()))
                .cast()
        }
    }

    #[cfg(debug_assertions)]
    fn check_owner(&self, id: Id<Len>) {
        assert_eq!(
            id.owner,
            self.as_ptr() as *const c_void,
            "Tried to get an Id of {} from a different collection than the one it came from. This check is not performed in release builds, where it would be a nasty bug!",
            id.index
        );
    }
}
