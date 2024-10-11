use core::{
    any,
    cmp::Ordering,
    fmt::{self, Formatter},
    hash::{Hash, Hasher},
    marker::PhantomData,
    ptr::NonNull,
};

use crate::soa_slice::Slice;

/// An index into an array of values, based
/// on an offset into the array rather than a pointer.
///
/// Unlike a Rust pointer, this is a u32 offset
/// rather than usize.
pub struct Index<T> {
    pub(crate) index: u32,
    pub(crate) _marker: PhantomData<T>,

    #[cfg(debug_assertions)]
    pub(crate) array_start: Option<NonNull<T>>,
}

impl<T> PartialEq for Index<T> {
    fn eq(&self, other: &Self) -> bool {
        #[cfg(debug_assertions)]
        self.verify_array(other.array_start.map(NonNull::as_ptr), ".eq()");

        self.index == other.index
    }
}

impl<T> Eq for Index<T> {}

impl<T> PartialOrd for Index<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        #[cfg(debug_assertions)]
        self.verify_array(other.array_start.map(NonNull::as_ptr), ".partial_cmp()");

        Some(self.cmp(other))
    }
}

impl<T> Ord for Index<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        #[cfg(debug_assertions)]
        self.verify_array(other.array_start.map(NonNull::as_ptr), ".cmp()");

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
    pub const fn new(
        start: u32,
        _array: &[T], // only used in debug builds
    ) -> Self {
        Self {
            index: start,
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

    /// Create a new index that isn't associated with any particular array.
    /// This is marked as unsafe because it omits the runtime checks (in debug builds)
    /// which verify in debug builds that indices are dereferened into the original array.
    pub const unsafe fn new_unchecked(start: u32) -> Self {
        Self {
            index: start,
            _marker: PhantomData,

            #[cfg(debug_assertions)]
            array_start: None,
        }
    }

    pub const fn as_slice(self) -> Slice<T> {
        Slice {
            start: self.index,
            length: 1,
            _marker: PhantomData,

            #[cfg(debug_assertions)]
            array_start: self.array_start,
        }
    }

    /// Given the original slice this indexes into, return the equivalent of slice.get_unchecked(index)
    /// (except that in debug build it actually does do a bounds check, and panics if it's out of bounds,
    /// but that should never happen since this is an Index into a known slice - basically, a reference).
    pub fn get_in(self, slice: &[T]) -> &T {
        #[cfg(debug_assertions)]
        {
            self.verify_array(Some(slice.as_ptr() as *mut T), "get_in");
            slice.get(self.index as usize).expect("Accessing an Index in its original slice went out of bounds. This should never happen!")
        }

        #[cfg(not(debug_assertions))]
        unsafe {
            slice.get_unchecked(self.index as usize)
        }
    }

    /// Given the original slice this indexes into, return the equivalent of slice.get_unchecked_mut(index)
    /// (except that in debug build it actually does do a bounds check, and panics if it's out of bounds,
    /// but that should never happen since this is an Index into a known slice - basically, a reference).
    pub fn get_in_mut(self, slice: &mut [T]) -> &mut T {
        #[cfg(debug_assertions)]
        {
            self.verify_array(Some(slice.as_ptr() as *mut T), "get_in");
            slice.get_mut(self.index as usize).expect("Accessing an Index in its original slice went out of bounds. This should never happen!")
        }

        #[cfg(not(debug_assertions))]
        unsafe {
            slice.get_unchecked_mut(self.index as usize)
        }
    }

    /// This is unsafe because it doesn't verify that the index being returned is being used with the original
    /// slice it was created with. Self::get_in is the safe alternative to this.
    pub const unsafe fn index(self) -> usize {
        self.index as usize
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

impl<'a, T> core::ops::Index<Index<T>> for [T] {
    type Output = T;

    fn index(&self, index: Index<T>) -> &Self::Output {
        index.get_in(self)
    }
}

impl<'a, T> core::ops::IndexMut<Index<T>> for [T] {
    fn index_mut(&mut self, index: Index<T>) -> &mut Self::Output {
        index.get_in_mut(self)
    }
}
