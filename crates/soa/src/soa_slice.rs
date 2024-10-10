use core::fmt;
use core::iter::Map;

use crate::soa_index::Index;

/// A slice into an array of values, based
/// on an offset into the array rather than a pointer.
///
/// Unlike a Rust slice, this is a u32 offset
/// rather than a pointer, and the length is u16.
pub struct Slice<Array, Elem> {
    pub start: u32,
    pub length: u16,
    _marker: core::marker::PhantomData<(Array, Elem)>,
}

impl<T, U> fmt::Debug for Slice<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Slice<{}, {}> {{ start: {}, length: {} }}",
            core::any::type_name::<T>(),
            core::any::type_name::<U>(),
            self.start,
            self.length
        )
    }
}

// derive of copy and clone does not play well with PhantomData

impl<T, U> Copy for Slice<T, U> {}

impl<T, U> Clone for Slice<T, U> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, U> Default for Slice<T, U> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<Array, Elem> Slice<Array, Elem> {
    pub fn empty() -> Self {
        Self {
            start: 0,
            length: 0,
            _marker: Default::default(),
        }
    }

    pub fn get_slice<'a>(&self, slice: &'a [Elem]) -> &'a [Elem] {
        &slice[self.indices()]
    }

    pub fn get_slice_mut<'a>(&self, slice: &'a mut [Elem]) -> &'a mut [Elem] {
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

    pub fn extend_new(vec: &mut Vec<Elem>, it: impl IntoIterator<Item = Elem>) -> Self {
        let start = vec.len();

        vec.extend(it);

        let end = vec.len();

        Self::new(start as u32, (end - start) as u16)
    }
}

impl<Array, Elem> IntoIterator for Slice<Array, Elem> {
    type Item = Index<Elem>;

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

pub trait GetSlice<Array, Elem> {
    fn get_slice(&self, slice: Slice<Array, Elem>) -> &[Elem];
}
