use core::{fmt, marker::PhantomData, ops::Range, ptr::NonNull};

use crate::soa_index::Index;

/// A slice into an array of values, based
/// on an offset into the array rather than a pointer.
///
/// Unlike a Rust slice, this is a u32 offset
/// rather than a pointer, and the length is u16.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Slice<T> {
    pub(crate) start: u32,
    pub(crate) length: u16,
    pub(crate) _marker: core::marker::PhantomData<T>,

    #[cfg(debug_assertions)]
    pub(crate) array_start: Option<NonNull<T>>,
}

impl<T> fmt::Debug for Slice<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Slice<{}> {{ start: {}, length: {} }}",
            core::any::type_name::<T>(),
            self.start,
            self.length
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

impl<T> Slice<T> {
    pub fn empty(_array: &[T]) -> Self {
        Self {
            start: 0,
            length: 0,
            _marker: Default::default(),

            #[cfg(debug_assertions)]
            array_start: NonNull::new(_array.as_ptr() as *mut T),
        }
    }

    /// Create an empty slice that isn't associated with any particular array.
    /// This is marked as unsafe because it omits the runtime checks (in debug builds)
    /// which verify that indices made from this slice are compared with other
    /// indices into the original array.
    pub unsafe fn empty_unchecked() -> Self {
        Self {
            start: 0,
            length: 0,
            _marker: Default::default(),

            #[cfg(debug_assertions)]
            array_start: None,
        }
    }

    /// This is unsafe because it doesn't verify that the start index being returned is being used with the original
    /// slice it was created with. Self::get_in is the safe alternative to this.
    pub const unsafe fn start(self) -> usize {
        self.start as usize
    }

    pub fn get_slice<'a>(&self, slice: &'a [T]) -> &'a [T] {
        &slice[self.as_range()]
    }

    pub fn get_slice_mut<'a>(&self, slice: &'a mut [T]) -> &'a mut [T] {
        &mut slice[self.as_range()]
    }

    /// This is unsafe because it doesn't verify that the start index being returned is being used with the original
    /// slice it was created with. Self::get_in is the safe alternative to this.
    #[inline(always)]
    pub unsafe fn indices(&self) -> Range<usize> {
        self.as_range()
    }

    /// Given the original &[T] that this Slice<T> was based on,
    /// return the &[T] representation of this Slice<T>.
    #[inline(always)]
    pub fn slice_from<'a>(&self, original: &'a [T]) -> &'a [T] {
        #[cfg(debug_assertions)]
        self.verify_array(Some(original.as_ptr() as *mut T), "slice_from");

        &original[self.as_range()]
    }

    /// The pub version of this is indices() and it's marked as unsafe. This one isn't marked as unsafe,
    /// for internal convenience.
    #[inline(always)]
    fn as_range(&self) -> Range<usize> {
        self.start as usize..(self.start as usize + self.length as usize)
    }

    /// Given the original &mut [T] that this Slice<T> was based on,
    /// return the &mut [T] representation of this Slice<T>.
    #[inline(always)]
    pub fn slice_from_mut<'a>(&self, original: &'a mut [T]) -> &'a mut [T] {
        #[cfg(debug_assertions)]
        self.verify_array(Some(original.as_ptr() as *mut T), "slice_from");

        &mut original[self.start as usize..(self.start as usize + self.length as usize)]
    }

    pub const fn len(&self) -> usize {
        self.length as usize
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn at_start(&self, array: &[T]) -> Index<T> {
        self.at(0, array)
    }

    pub fn at(&self, i: usize, _array: &[T]) -> Index<T> {
        #[cfg(debug_assertions)]
        {
            if !self.matches_stored_array(_array) {
                panic!("Tried to call .at() on a slice passing an array that was different from the array it was created with!");
            }
        }

        Index {
            index: self.start + i as u32,
            _marker: PhantomData,

            #[cfg(debug_assertions)]
            array_start: self.array_start,
        }
    }

    #[cfg(debug_assertions)]
    fn matches_stored_array(&self, other_array: &[T]) -> bool {
        match self.array_start {
            Some(self_array) => other_array.as_ptr() == self_array.as_ptr(),
            None => true,
        }
    }

    pub const fn new(start: u32, length: u16, _array: &[T]) -> Self {
        Self {
            start,
            length,
            _marker: PhantomData,

            #[cfg(debug_assertions)]
            array_start: Some(
                // Safety: this is safe beccause references are never to null pointers.
                // Once NonNull::new is const on stable Rust, use that instead. Docs to check for const-ness:
                // https://doc.rust-lang.org/std/ptr/struct.NonNull.html#method.new
                unsafe { NonNull::new_unchecked(_array.as_ptr() as *mut T) },
            ),
        }
    }

    /// Create a new slice that isn't associated with any particular array.
    /// This is marked as unsafe because it omits the runtime checks (in debug builds)
    /// which verify in debug builds that indices made from this slice are compared with other
    /// indices into the original array.
    pub const unsafe fn new_unchecked(start: u32, length: u16) -> Self {
        Self {
            start,
            length,
            _marker: PhantomData,

            #[cfg(debug_assertions)]
            array_start: None,
        }
    }

    #[cfg(debug_assertions)]
    fn verify_array(&self, other: Option<*mut T>, operation: &'static str) {
        match (self.array_start, other) {
            (Some(array_start), Some(other_array_start))
                if other_array_start != array_start.as_ptr() =>
            {
                panic!(
                    "Tried to {} an index from one array to the index of a different array!",
                    operation
                );
            }
            _ => {
                // Either one of them was None, meaning this is unchecked, or they point to the same memory - as expected!
            }
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

                #[cfg(debug_assertions)]
                array_start: self.slice.array_start,
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
