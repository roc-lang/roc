use core::{fmt, marker::PhantomData};

use crate::{soa_index::Index, soa_slice::Slice};

/// Two slices of the same length, each based on a different
/// offset into the same array (rather than a pointer).
///
/// Unlike a Rust slice, this is a u32 offset
/// rather than a pointer, and the length is u16.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Slice2<T, U> {
    pub start1: u32,
    pub start2: u32,
    pub length: u16,
    pub _marker: core::marker::PhantomData<(T, U)>,
}

impl<T, U> fmt::Debug for Slice2<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Slice2<{}, {}> {{ start1: {}, start2: {}, length: {} }}",
            core::any::type_name::<T>(),
            core::any::type_name::<U>(),
            self.start1,
            self.start2,
            self.length
        )
    }
}

// derive of copy and clone does not play well with PhantomData

impl<T, U> Copy for Slice2<T, U> {}

impl<T, U> Clone for Slice2<T, U> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, U> Default for Slice2<T, U> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<T, U> Slice2<T, U> {
    pub const fn empty() -> Self {
        Self {
            start1: 0,
            start2: 0,
            length: 0,
            _marker: PhantomData,
        }
    }

    pub const fn slice_first(self) -> Slice<T> {
        Slice {
            start: self.start1,
            length: self.length,
            _marker: PhantomData,
        }
    }

    pub const fn slice_second(self) -> Slice<U> {
        Slice {
            start: self.start2,
            length: self.length,
            _marker: PhantomData,
        }
    }

    pub const fn len(&self) -> usize {
        self.length as usize
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub const fn new(start1: u32, start2: u32, length: u16) -> Self {
        Self {
            start1,
            start2,
            length,
            _marker: PhantomData,
        }
    }
}

impl<T, U> IntoIterator for Slice2<T, U> {
    type Item = (Index<T>, Index<U>);
    type IntoIter = SliceIterator<T, U>;

    fn into_iter(self) -> Self::IntoIter {
        SliceIterator {
            slice: self,
            offset: 0,
        }
    }
}

pub struct SliceIterator<T, U> {
    slice: Slice2<T, U>,
    offset: u32,
}

impl<T, U> Iterator for SliceIterator<T, U> {
    type Item = (Index<T>, Index<U>);

    fn next(&mut self) -> Option<Self::Item> {
        let offset = self.offset;

        if offset < self.slice.length as u32 {
            let index1 = Index {
                index: self.slice.start1 + offset,
                _marker: PhantomData,
            };
            let index2 = Index {
                index: self.slice.start2 + offset,
                _marker: PhantomData,
            };

            self.offset += 1;

            Some((index1, index2))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = (self.slice.length as u32 - self.offset) as usize;
        (remaining, Some(remaining))
    }
}

impl<T, U> ExactSizeIterator for SliceIterator<T, U> {}
