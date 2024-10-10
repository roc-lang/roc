use core::fmt;

use crate::soa_slice::Slice;

/// An index into an array of values, based
/// on an offset into the array rather than a pointer.
///
/// Unlike a Rust pointer, this is a u32 offset
/// rather than usize.
pub struct Index<Array, Elem> {
    pub index: u32,
    pub(crate) _marker: core::marker::PhantomData<(Array, Elem)>,
}

impl<Array, Elem> fmt::Debug for Index<Array, Elem> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Index<{}, {}>({})",
            core::any::type_name::<Array>(),
            core::any::type_name::<Elem>(),
            self.index
        )
    }
}

// derive of copy and clone does not play well with PhantomData

impl<Array, Elem> Copy for Index<Array, Elem> {}

impl<Array, Elem> Clone for Index<Array, Elem> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Array, Elem> Index<Array, Elem> {
    pub const fn new(start: u32) -> Self {
        Self {
            index: start,
            _marker: core::marker::PhantomData,
        }
    }

    pub fn push_new(vector: &mut Vec<Elem>, value: Elem) -> Self {
        let index = Self::new(vector.len() as _);

        vector.push(value);

        index
    }

    pub const fn as_slice(self) -> Slice<Array, Elem> {
        Slice::new(self.index, 1)
    }
}
