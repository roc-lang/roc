use core::{
    any,
    cmp::Ordering,
    fmt::{self, Formatter},
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use crate::soa_slice::Slice;

/// An index into an array of values, based
/// on an offset into the array rather than a pointer.
///
/// Unlike a Rust pointer, this is a u32 offset
/// rather than usize.
pub struct Index<T> {
    pub index: u32,
    pub _marker: PhantomData<T>,
}

impl<T> PartialEq for Index<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for Index<T> {}

impl<T> PartialOrd for Index<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Index<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}

impl<T> fmt::Debug for Index<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Index<{}>({})", any::type_name::<T>(), self.index)
    }
}

// derive of copy and clone does not play well with PhantomData

impl<T> Copy for Index<T> {}

impl<T> Clone for Index<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Hash for Index<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl<T> Index<T> {
    pub const fn new(start: u32) -> Self {
        Self {
            index: start,
            _marker: PhantomData,
        }
    }

    pub const fn as_slice(self) -> Slice<T> {
        Slice {
            start: self.index,
            length: 1,
            _marker: PhantomData,
        }
    }

    pub const fn index(self) -> usize {
        self.index as usize
    }
}

impl<T> core::ops::Index<Index<T>> for [T] {
    type Output = T;

    fn index(&self, index: Index<T>) -> &Self::Output {
        &self[index.index()]
    }
}

impl<T> core::ops::IndexMut<Index<T>> for [T] {
    fn index_mut(&mut self, index: Index<T>) -> &mut Self::Output {
        &mut self[index.index()]
    }
}
