use core::{fmt, marker::PhantomData, ops::Range};

use crate::soa_index::Index;

/// A slice into an array of values, based
/// on an offset into the array rather than a pointer.
///
/// Unlike a Rust slice, this is a u32 offset
/// rather than a pointer, and the length is u16.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Slice<T> {
    pub start: u32,
    pub length: u16,
    pub _marker: core::marker::PhantomData<T>,
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
    pub const fn empty() -> Self {
        Self {
            start: 0,
            length: 0,
            _marker: PhantomData,
        }
    }

    pub const fn start(self) -> u32 {
        self.start
    }

    pub fn advance(&mut self, amount: u32) {
        self.start += amount
    }

    pub fn get_slice<'a>(&self, elems: &'a [T]) -> &'a [T] {
        &elems[self.indices()]
    }

    pub fn get_slice_mut<'a>(&self, elems: &'a mut [T]) -> &'a mut [T] {
        &mut elems[self.indices()]
    }

    #[inline(always)]
    pub const fn indices(&self) -> Range<usize> {
        self.start as usize..(self.start as usize + self.length as usize)
    }

    pub const fn len(&self) -> usize {
        self.length as usize
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn at_start(&self) -> Index<T> {
        Index {
            index: self.start,
            _marker: PhantomData,
        }
    }

    pub fn at(&self, i: usize) -> Index<T> {
        Index {
            index: self.start + i as u32,
            _marker: PhantomData,
        }
    }

    pub const fn new(start: u32, length: u16) -> Self {
        Self {
            start,
            length,
            _marker: PhantomData,
        }
    }
}

impl<T> IntoIterator for Slice<T> {
    type Item = Index<T>;
    type IntoIter = SliceIterator<T>;

    fn into_iter(self) -> Self::IntoIter {
        SliceIterator {
            slice: self,
            current: self.start,
        }
    }
}

pub struct SliceIterator<T> {
    slice: Slice<T>,
    current: u32,
}

impl<T> Iterator for SliceIterator<T> {
    type Item = Index<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.slice.start + self.slice.length as u32 {
            let index = Index {
                index: self.current,
                _marker: PhantomData,
            };

            self.current += 1;

            Some(index)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = (self.slice.start + self.slice.length as u32 - self.current) as usize;
        (remaining, Some(remaining))
    }
}

impl<T> ExactSizeIterator for SliceIterator<T> {}

pub trait GetSlice<T> {
    fn get_slice(&self, slice: Slice<T>) -> &[T];
}
