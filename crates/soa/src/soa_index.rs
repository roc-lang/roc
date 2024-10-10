use core::fmt;

use crate::soa_slice::Slice;

/// An index into an array of values, based
/// on an offset into the array rather than a pointer.
///
/// Unlike a Rust pointer, this is a u32 offset
/// rather than usize.
pub struct Index<T> {
    pub index: u32,
    pub(crate) _marker: core::marker::PhantomData<T>,
}

impl<T> fmt::Debug for Index<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Index<{}>({})", core::any::type_name::<T>(), self.index)
    }
}

// derive of copy and clone does not play well with PhantomData

impl<T> Copy for Index<T> {}

impl<T> Clone for Index<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Index<T> {
    pub const fn new(start: u32) -> Self {
        Self {
            index: start,
            _marker: core::marker::PhantomData,
        }
    }

    pub fn push_new(vector: &mut Vec<T>, value: T) -> Self {
        let index = Self::new(vector.len() as _);

        vector.push(value);

        index
    }

    pub const fn as_slice(self) -> Slice<T> {
        Slice::new(self.index, 1)
    }
}
