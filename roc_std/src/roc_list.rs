use core::ffi::c_void;
use core::fmt;
use core::ops::{Deref, DerefMut, Drop};
use core::{mem, ptr};

use crate::{roc_alloc, roc_dealloc, roc_realloc, Storage, REFCOUNT_1};

#[repr(C)]
pub struct RocList<T> {
    elements: *mut T,
    length: usize,
}

impl<T: Clone> Clone for RocList<T> {
    fn clone(&self) -> Self {
        Self::from_slice(self.as_slice())
    }
}

impl<T> RocList<T> {
    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            Some(unsafe {
                let raw = self.elements.add(index);

                &*raw
            })
        } else {
            None
        }
    }

    pub fn storage(&self) -> Option<Storage> {
        use core::cmp::Ordering::*;

        if self.length == 0 {
            return None;
        }

        unsafe {
            let value = *self.get_storage_ptr();

            // NOTE doesn't work with elements of 16 or more bytes
            match isize::cmp(&value, &0) {
                Equal => Some(Storage::ReadOnly),
                Less => Some(Storage::Refcounted(value)),
                Greater => Some(Storage::Capacity(value as usize)),
            }
        }
    }

    fn get_storage_ptr_help(elements: *mut T) -> *mut isize {
        let ptr = elements as *mut isize;

        unsafe { ptr.offset(-1) }
    }

    fn get_storage_ptr(&self) -> *const isize {
        Self::get_storage_ptr_help(self.elements)
    }

    fn get_storage_ptr_mut(&mut self) -> *mut isize {
        self.get_storage_ptr() as *mut isize
    }

    fn set_storage_ptr(&mut self, ptr: *const isize) {
        self.elements = unsafe { ptr.offset(1) as *mut T };
    }

    fn get_element_ptr(elements: *const T) -> *const T {
        let elem_alignment = core::mem::align_of::<T>();
        let ptr = elements as *const usize;

        unsafe {
            if elem_alignment <= core::mem::align_of::<usize>() {
                ptr.add(1) as *const T
            } else {
                // If elements have an alignment bigger than usize (e.g. an i128),
                // we will have necessarily allocated two usize slots worth of
                // space for the storage value (with the first usize slot being
                // padding for alignment's sake), and we need to skip past both.
                ptr.add(2) as *const T
            }
        }
    }

    pub fn from_slice_with_capacity(slice: &[T], capacity: usize) -> Self
    where
        T: Clone,
    {
        assert!(capacity > 0);
        assert!(slice.len() <= capacity);

        let element_bytes = capacity * core::mem::size_of::<T>();

        let padding = {
            if core::mem::align_of::<T>() <= core::mem::align_of::<usize>() {
                // aligned on usize (8 bytes on 64-bit systems)
                0
            } else {
                // aligned on 2*usize (16 bytes on 64-bit systems)
                core::mem::size_of::<usize>()
            }
        };

        let num_bytes = core::mem::size_of::<usize>() + padding + element_bytes;

        let elements = unsafe {
            let raw_ptr = roc_alloc(num_bytes, core::mem::size_of::<usize>() as u32) as *mut u8;

            // pointer to the first element
            let raw_ptr = Self::get_element_ptr(raw_ptr as *mut T) as *mut T;

            // write the refcount
            let refcount_ptr = raw_ptr as *mut isize;
            *(refcount_ptr.offset(-1)) = isize::MIN;

            // Clone the elements into the new array.
            let target_ptr = raw_ptr;
            for (i, value) in slice.iter().cloned().enumerate() {
                let target_ptr = target_ptr.add(i);
                target_ptr.write(value);
            }

            raw_ptr
        };

        Self {
            length: slice.len(),
            elements,
        }
    }

    pub fn from_slice(slice: &[T]) -> Self
    where
        T: Clone,
    {
        // Avoid allocation with empty list.
        if slice.is_empty() {
            Self::default()
        } else {
            Self::from_slice_with_capacity(slice, slice.len())
        }
    }

    pub fn as_slice(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.elements, self.length) }
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { core::slice::from_raw_parts_mut(self.elements, self.length) }
    }

    /// Copy the contents of the given slice into the end of this list,
    /// reallocating and resizing as necessary.
    pub fn append_slice(&mut self, slice: &[T]) {
        let new_len = self.len() + slice.len();
        let storage_ptr = self.get_storage_ptr_mut();

        // First, ensure that there's enough storage space.
        unsafe {
            let storage_val = *storage_ptr as isize;

            // Check if this is refcounted, readonly, or has a capcacity.
            // (Capacity will be positive if it has a capacity.)
            if storage_val > 0 {
                let capacity = storage_val as usize;

                // We don't have enough capacity, so we need to get some more.
                if capacity < new_len {
                    // Double our capacity using realloc
                    let new_cap = 2 * capacity;
                    let new_ptr = roc_realloc(
                        storage_ptr as *mut c_void,
                        new_cap,
                        capacity,
                        Self::align_of_storage_ptr(),
                    ) as *mut isize;

                    // Write the new capacity into the new memory
                    *new_ptr = new_cap as isize;

                    // Copy all the existing elements into the new allocation.
                    ptr::copy_nonoverlapping(self.elements, new_ptr as *mut T, self.len());

                    // Update our storage pointer to be the new one
                    self.set_storage_ptr(new_ptr);
                }
            } else {
                // If this was reference counted, decrement the refcount!
                if storage_val < 0 {
                    let refcount = storage_val;

                    // Either deallocate or decrement.
                    if refcount == REFCOUNT_1 {
                        roc_dealloc(storage_ptr as *mut c_void, Self::align_of_storage_ptr());
                    } else {
                        *storage_ptr = refcount - 1;
                    }
                }

                // This is either refcounted or readonly; either way, we need
                // to clone the elements!

                // Double the capacity we need, in case there are future additions.
                let new_cap = new_len * 2;
                let new_ptr = roc_alloc(new_cap, Self::align_of_storage_ptr()) as *mut isize;

                // Write the new capacity into the new memory; this list is
                // now unique, and gets its own capacity!
                *new_ptr = new_cap as isize;

                // Copy all the existing elements into the new allocation.
                ptr::copy_nonoverlapping(self.elements, new_ptr as *mut T, self.len());

                // Update our storage pointer to be the new one
                self.set_storage_ptr(new_ptr);
            }

            // Since this is an append, we want to start writing new elements
            // into the memory immediately after the current last element.
            let dest = self.elements.add(self.len());

            // There's now enough storage to append the contents of the slice
            // in-place, so do that!
            ptr::copy_nonoverlapping(slice.as_ptr(), dest, self.len());
        }

        self.length = new_len;
    }

    /// The alignment we need is either the alignment of T, or else
    /// the alignment of usize, whichever is higher. That's because we need
    /// to store both T values as well as the refcount/capacity storage slot.
    fn align_of_storage_ptr() -> u32 {
        mem::align_of::<T>().max(mem::align_of::<usize>()) as u32
    }

    unsafe fn drop_pointer_to_first_argument(ptr: *mut T) {
        let storage_ptr = Self::get_storage_ptr_help(ptr);
        let storage_val = *storage_ptr;

        if storage_val == REFCOUNT_1 || storage_val > 0 {
            // If we have no more references, or if this was unique,
            // deallocate it.
            roc_dealloc(storage_ptr as *mut c_void, Self::align_of_storage_ptr());
        } else if storage_val < 0 {
            // If this still has more references, decrement one.
            *storage_ptr = storage_val - 1;
        }

        // The only remaining option is that this is in readonly memory,
        // in which case we shouldn't attempt to do anything to it.
    }
}

impl<T> Deref for RocList<T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        self.as_slice()
    }
}

impl<T> DerefMut for RocList<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        self.as_mut_slice()
    }
}

impl<'a, T> IntoIterator for &'a RocList<T> {
    type Item = &'a T;

    type IntoIter = <&'a [T] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.as_slice().iter()
    }
}

impl<T> IntoIterator for RocList<T> {
    type Item = T;

    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        let remaining = self.len();

        let buf = unsafe { NonNull::new_unchecked(self.elements as _) };
        let ptr = self.elements;

        IntoIter {
            buf,
            ptr,
            remaining,
        }
    }
}

use core::ptr::NonNull;

pub struct IntoIter<T> {
    buf: NonNull<T>,
    // pub cap: usize,
    ptr: *const T,
    remaining: usize,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        next_help(self)
    }
}

fn next_help<T>(this: &mut IntoIter<T>) -> Option<T> {
    if this.remaining == 0 {
        None
    } else if mem::size_of::<T>() == 0 {
        // purposefully don't use 'ptr.offset' because for
        // vectors with 0-size elements this would return the
        // same pointer.
        this.remaining -= 1;

        // Make up a value of this ZST.
        Some(unsafe { mem::zeroed() })
    } else {
        let old = this.ptr;
        this.ptr = unsafe { this.ptr.offset(1) };
        this.remaining -= 1;

        Some(unsafe { ptr::read(old) })
    }
}

impl<T> Drop for IntoIter<T> {
    fn drop(&mut self) {
        // drop the elements that we have not yet returned.
        while let Some(item) = next_help(self) {
            drop(item);
        }

        // deallocate the whole buffer
        unsafe {
            RocList::drop_pointer_to_first_argument(self.buf.as_mut());
        }
    }
}

impl<T> Default for RocList<T> {
    fn default() -> Self {
        Self {
            length: 0,
            elements: core::ptr::null_mut(),
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for RocList<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // RocList { storage: Refcounted(3), elements: [ 1,2,3,4] }
        f.debug_struct("RocList")
            .field("storage", &self.storage())
            .field("elements", &self.as_slice())
            .finish()
    }
}

impl<T: PartialEq> PartialEq for RocList<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.length != other.length {
            return false;
        }

        for i in 0..self.length {
            unsafe {
                if *self.elements.add(i) != *other.elements.add(i) {
                    return false;
                }
            }
        }

        true
    }
}

impl<T: Eq> Eq for RocList<T> {}

impl<T> Drop for RocList<T> {
    fn drop(&mut self) {
        if !self.is_empty() {
            let storage_ptr = self.get_storage_ptr_mut();

            unsafe {
                let storage_val = *storage_ptr;

                if storage_val == REFCOUNT_1 || storage_val > 0 {
                    // If we have no more references, or if this was unique,
                    // deallocate it.
                    roc_dealloc(storage_ptr as *mut c_void, Self::align_of_storage_ptr());
                } else if storage_val < 0 {
                    // If this still has more references, decrement one.
                    *storage_ptr = storage_val - 1;
                }

                // The only remaining option is that this is in readonly memory,
                // in which case we shouldn't attempt to do anything to it.
            }
        }
    }
}
