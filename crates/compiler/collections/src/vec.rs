pub struct ExactVec<T> {
    ptr: *mut T,

    #[cfg(debug_assertions)]
    capacity: u32,
}

impl<T> ExactVec<T> {
    pub fn with_capacity_in(arena: Arena, capacity: u32) {
        Self {
            ptr: arena.aligned_alloc<T>(capacity).cast(),

            #[cfg(debug_assertions)]
            capacity,
        }
    }

    pub fn insert(&mut self, index: u32, elem: T) {
        todo!()
    }
}
