pub struct Arena<'a> {
    base: NonNull<u8>,
    capacity: u32,
    len: u32,
}

impl<'a> Arena<'a> {
    pub fn alloc<T>(len: usize, align: ) -> &'a mut T {

    }
}
