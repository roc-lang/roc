use crate::arena::Arena;
use core::{alloc::Layout, ffi::c_void, mem};

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

impl<'a, Len: Into<usize> + From<usize>, V1, V2> IdMap2<'a, Len, V1, V2> {
    pub fn with_capacity(arena: &'a mut Arena, capacity: Len) -> Self {
        let capacity_usize: usize = capacity.into();
        let (layout, offset) = Layout::array::<V1>(capacity_usize)
            .and_then(|layout| layout.extend(Layout::array::<V2>(capacity_usize)))
            .unwrap_or_else(|layout_err| panic!());
        // let elem_size = mem::size_of::<V1>() + mem::size_of::<V2>();
        // let total_size = capacity as usize * elem_size;
        // let align = mem::align_of::<V1>().max(mem::align_of::V2());
        // let layout =
        let start_index = arena.alloc_layout(layout);

        Self {
            len: 0,
            capacity,
            start_index,
        }
    }

    pub fn insert(&'a mut self, arena: &'a mut Arena, v1: V1, v2: V2) -> Id<Len> {
        let answer: Id<Len> = Id {
            index: self.len,

            #[cfg(debug_assertions)]
            owner: self.as_ptr() as *const c_void,
        };
        let index: usize = self.len.into();

        self.len = (index + 1).into();

        todo!("push v1 and v2, then return answer")
    }

    pub fn get_first(&'a self, arena: &'a Arena, id: Id<Len>) -> &'a V1 {
        let index: usize = id.index.into();

        #[cfg(debug_assertions)]
        {
            self.check_owner(id);
        }

        todo!()
    }

    pub fn get_second(&'a self, arena: &'a Arena, id: Id<Len>) -> &'a V2 {
        let index: usize = id.index.into();

        #[cfg(debug_assertions)]
        {
            self.check_owner(id);
        }

        todo!()
    }

    #[cfg(debug_assertions)]
    fn check_owner(&self, id: Id<Len>) {
        assert_eq!(
            id.owner,
            self.as_ptr() as *const c_void,
            "Tried to get an Id of {} from a different collection than the one it came from. This check is not performed in release builds, where it would be a nasty bug!",
            index
        );
    }
}
