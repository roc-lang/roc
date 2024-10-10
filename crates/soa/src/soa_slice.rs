use core::fmt;
use core::iter::Map;

use crate::soa_index::Index;

/// A slice into an array of values, based
/// on an offset into the array rather than a pointer.
///
/// Unlike a Rust slice, this is a u32 offset
/// rather than a pointer, and the length is u16.
pub struct Slice<T> {
    pub start: u32,
    pub length: u16,
    _marker: core::marker::PhantomData<T>,
}

impl<T> fmt::Debug for Slice<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Slice {{ start: {}, length: {} }}",
            self.start, self.length
        )
    }
}

// derive of copy and clone does not play well with PhantomData

impl<T> Copy for Slice<T> {}

impl<T> Clone for Slice<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Default for Slice<T> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<T> Slice<T> {
    pub fn empty() -> Self {
        Self {
            start: 0,
            length: 0,
            _marker: Default::default(),
        }
    }

    pub fn get_slice<'a>(&self, slice: &'a [T]) -> &'a [T] {
        &slice[self.indices()]
    }

    pub fn get_slice_mut<'a>(&self, slice: &'a mut [T]) -> &'a mut [T] {
        &mut slice[self.indices()]
    }

    #[inline(always)]
    pub fn indices(&self) -> core::ops::Range<usize> {
        self.start as usize..(self.start as usize + self.length as usize)
    }

    pub const fn len(&self) -> usize {
        self.length as usize
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub const fn new(start: u32, length: u16) -> Self {
        Self {
            start,
            length,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn extend_new(vec: &mut Vec<T>, it: impl IntoIterator<Item = T>) -> Self {
        let start = vec.len();

        vec.extend(it);

        let end = vec.len();

        Self::new(start as u32, (end - start) as u16)
    }
}

impl<T> IntoIterator for Slice<T> {
    type Item = Index<T>;

    #[allow(clippy::type_complexity)]
    type IntoIter = Map<core::ops::Range<u32>, fn(u32) -> Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        (self.start..(self.start + self.length as u32)).map(u32_to_index)
    }
}

fn u32_to_index<T>(i: u32) -> Index<T> {
    Index {
        index: i,
        _marker: core::marker::PhantomData,
    }
}

pub trait GetSlice<T> {
    fn get_slice(&self, slice: Slice<T>) -> &[T];
}
