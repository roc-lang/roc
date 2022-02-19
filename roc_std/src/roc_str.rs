use core::ffi::c_void;
use core::fmt::{self, Display, Formatter};
use core::ops::{Deref, DerefMut, Drop};
use core::{mem, ptr, slice};

use crate::{roc_alloc, roc_dealloc, Storage, REFCOUNT_1};

#[repr(C)]
pub struct RocStr {
    elements: *mut u8,
    length: usize,
    capacity: usize,
}

impl RocStr {
    pub const SIZE: usize = core::mem::size_of::<Self>();
    pub const MASK: u8 = 0b1000_0000;

    pub const fn len(&self) -> usize {
        self.length
    }

    pub const fn capacity(&self) -> usize {
        self.capacity ^ isize::MIN as usize
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub const fn is_small_str(&self) -> bool {
        (self.capacity as isize) < 0
    }

    pub const fn empty() -> Self {
        Self {
            length: 0,
            capacity: isize::MIN as usize,
            elements: core::ptr::null_mut(),
        }
    }

    pub fn get(&self, index: usize) -> Option<&u8> {
        if index < self.len() {
            Some(unsafe {
                let raw = if self.is_small_str() {
                    self.get_small_str_ptr().add(index)
                } else {
                    self.elements.add(index)
                };

                &*raw
            })
        } else {
            None
        }
    }

    pub fn get_bytes(&self) -> *const u8 {
        if self.is_small_str() {
            self.get_small_str_ptr()
        } else {
            self.elements
        }
    }

    pub fn storage(&self) -> Option<Storage> {
        use core::cmp::Ordering::*;

        if self.is_small_str() {
            return None;
        }

        unsafe {
            let value = *self.get_storage_ptr();

            // NOTE doesn't work with elements of 16 or more bytes
            match isize::cmp(&(value as isize), &0) {
                Equal => Some(Storage::ReadOnly),
                Less => Some(Storage::Refcounted(value)),
                Greater => Some(Storage::Capacity(value as usize)),
            }
        }
    }

    fn get_storage_ptr(&self) -> *const isize {
        let ptr = self.elements as *const isize;

        unsafe { ptr.offset(-1) }
    }

    fn get_storage_ptr_mut(&mut self) -> *mut isize {
        self.get_storage_ptr() as *mut isize
    }

    fn get_element_ptr(elements: *const u8) -> *const usize {
        let elem_alignment = core::mem::align_of::<u8>();
        let ptr = elements as *const usize;

        unsafe {
            if elem_alignment <= core::mem::align_of::<usize>() {
                ptr.add(1)
            } else {
                // If elements have an alignment bigger than usize (e.g. an i128),
                // we will have necessarily allocated two usize slots worth of
                // space for the storage value (with the first usize slot being
                // padding for alignment's sake), and we need to skip past both.
                ptr.add(2)
            }
        }
    }

    fn get_small_str_ptr(&self) -> *const u8 {
        (self as *const Self).cast()
    }

    fn get_small_str_ptr_mut(&mut self) -> *mut u8 {
        (self as *mut Self).cast()
    }

    const fn from_slice_small_str(slice: &[u8]) -> Self {
        assert!(slice.len() < Self::SIZE);

        let mut array = [0u8; Self::SIZE];

        // while loop because for uses Iterator and is not available in const contexts
        let mut i = 0;
        while i < slice.len() {
            array[i] = slice[i];
            i += 1;
        }

        array[Self::SIZE - 1] = slice.len() as u8 | Self::MASK;

        unsafe { core::mem::transmute(array) }
    }

    fn from_slice_with_capacity_str(slice: &[u8], capacity: usize) -> Self {
        assert!(
            slice.len() <= capacity,
            "RocStr::from_slice_with_capacity_str length bigger than capacity {} {}",
            slice.len(),
            capacity
        );
        if capacity < core::mem::size_of::<Self>() {
            Self::from_slice_small_str(slice)
        } else {
            let ptr = slice.as_ptr();
            let element_bytes = capacity;

            let num_bytes = core::mem::size_of::<usize>() + element_bytes;

            let elements = unsafe {
                let raw_ptr = roc_alloc(num_bytes, core::mem::size_of::<usize>() as u32) as *mut u8;
                // write the capacity
                let capacity_ptr = raw_ptr as *mut usize;
                *capacity_ptr = capacity;

                let raw_ptr = Self::get_element_ptr(raw_ptr as *mut u8);

                // write the refcount
                let refcount_ptr = raw_ptr as *mut isize;
                *(refcount_ptr.offset(-1)) = isize::MIN;

                {
                    // NOTE: using a memcpy here causes weird issues
                    let target_ptr = raw_ptr as *mut u8;
                    let source_ptr = ptr as *const u8;
                    let length = slice.len();

                    for index in 0..length {
                        *target_ptr.add(index) = *source_ptr.add(index);
                    }
                }

                raw_ptr as *mut u8
            };

            Self {
                length: slice.len(),
                capacity: slice.len(),
                elements,
            }
        }
    }

    pub fn from_slice(slice: &[u8]) -> Self {
        Self::from_slice_with_capacity_str(slice, slice.len())
    }

    pub fn as_slice(&self) -> &[u8] {
        if self.is_empty() {
            &[]
        } else if self.is_small_str() {
            unsafe { core::slice::from_raw_parts(self.get_small_str_ptr(), self.len()) }
        } else {
            unsafe { core::slice::from_raw_parts(self.elements, self.length) }
        }
    }

    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        if self.is_empty() {
            &mut []
        } else if self.is_small_str() {
            unsafe { core::slice::from_raw_parts_mut(self.get_small_str_ptr_mut(), self.len()) }
        } else {
            unsafe { core::slice::from_raw_parts_mut(self.elements, self.length) }
        }
    }

    pub fn as_str(&self) -> &str {
        let slice = self.as_slice();

        unsafe { core::str::from_utf8_unchecked(slice) }
    }

    pub fn as_mut_str(&mut self) -> &mut str {
        let slice = self.as_mut_slice();

        unsafe { core::str::from_utf8_unchecked_mut(slice) }
    }

    /// Write a CStr (null-terminated) representation of this RocStr into
    /// the given buffer.
    ///
    /// # Safety
    /// This assumes the given buffer has enough space, so make sure you only
    /// pass in a pointer to an allocation that's at least as long as this Str!
    pub unsafe fn write_c_str(&self, buf: *mut char) {
        if self.is_small_str() {
            ptr::copy_nonoverlapping(self.get_small_str_ptr(), buf as *mut u8, self.len());
        } else {
            ptr::copy_nonoverlapping(self.elements, buf as *mut u8, self.len());
        }

        // null-terminate
        *(buf.add(self.len())) = '\0';
    }
}

impl Deref for RocStr {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl DerefMut for RocStr {
    fn deref_mut(&mut self) -> &mut str {
        self.as_mut_str()
    }
}

impl Default for RocStr {
    fn default() -> Self {
        Self::empty()
    }
}

impl From<&str> for RocStr {
    fn from(str: &str) -> Self {
        Self::from_slice(str.as_bytes())
    }
}

impl Display for RocStr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl fmt::Debug for RocStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // RocStr { is_small_str: false, storage: Refcounted(3), elements: [ 1,2,3,4] }

        match core::str::from_utf8(self.as_slice()) {
            Ok(string) => f
                .debug_struct("RocStr")
                .field("is_small_str", &self.is_small_str())
                .field("storage", &self.storage())
                .field("string_contents", &string)
                .finish(),
            Err(_) => f
                .debug_struct("RocStr")
                .field("is_small_str", &self.is_small_str())
                .field("storage", &self.storage())
                .field("byte_contents", &self.as_slice())
                .finish(),
        }
    }
}

impl PartialEq for RocStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl Eq for RocStr {}

impl Clone for RocStr {
    fn clone(&self) -> Self {
        if self.is_small_str() {
            Self {
                elements: self.elements,
                length: self.length,
                capacity: self.capacity,
            }
        } else {
            let capacity_size = core::mem::size_of::<usize>();
            let copy_length = self.length + capacity_size;
            let elements = unsafe {
                // We use *mut u8 here even though technically these are
                // usize-aligned (due to the refcount slot).
                // This avoids any potential edge cases around there somehow
                // being unreadable memory after the last byte, which would
                // potentially get read when reading <usize> bytes at a time.
                let raw_ptr =
                    roc_alloc(copy_length, core::mem::size_of::<usize>() as u32) as *mut u8;
                let dest_slice = slice::from_raw_parts_mut(raw_ptr, copy_length);
                let src_ptr = self.elements.offset(-(capacity_size as isize)) as *mut u8;
                let src_slice = slice::from_raw_parts(src_ptr, copy_length);

                dest_slice.copy_from_slice(src_slice);

                *(raw_ptr as *mut usize) = self.length;

                (raw_ptr as *mut u8).add(capacity_size)
            };

            Self {
                elements,
                length: self.length,
                capacity: self.capacity,
            }
        }
    }
}

impl Drop for RocStr {
    fn drop(&mut self) {
        if !self.is_small_str() {
            let storage_ptr = self.get_storage_ptr_mut();

            unsafe {
                let storage_val = *storage_ptr;

                if storage_val == REFCOUNT_1 || storage_val > 0 {
                    // If we have no more references, or if this was unique,
                    // deallocate it.
                    roc_dealloc(storage_ptr as *mut c_void, mem::align_of::<isize>() as u32);
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
