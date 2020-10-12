#![crate_type = "lib"]
#![no_std]
use core::fmt;

// A list of C functions that are being imported
extern "C" {
    pub fn printf(format: *const u8, ...) -> i32;
}

const REFCOUNT_1: usize = isize::MIN as usize;

//#[macro_export]
//macro_rules! roclist {
//    () => (
//        $crate::RocList::empty()
//    );
//    ($($x:expr),+ $(,)?) => (
//        $crate::RocList::from_slice(&[$($x),+])
//    );
//}

#[repr(C)]
pub struct RocList<T> {
    elements: *mut T,
    length: usize,
}

#[derive(Clone, Copy, Debug)]
pub enum Storage {
    ReadOnly,
    Refcounted(usize),
    Capacity(usize),
}

impl<T> RocList<T> {
    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    pub fn empty() -> Self {
        RocList {
            length: 0,
            elements: core::ptr::null_mut(),
        }
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
            match isize::cmp(&(value as isize), &0) {
                Equal => Some(Storage::ReadOnly),
                Less => Some(Storage::Refcounted(value)),
                Greater => Some(Storage::Capacity(value)),
            }
        }
    }

    fn get_storage_ptr(&self) -> *const usize {
        let ptr = self.elements as *const usize;

        unsafe { ptr.offset(-1) }
    }

    fn get_storage_ptr_mut(&mut self) -> *mut usize {
        self.get_storage_ptr() as *mut usize
    }

    fn get_element_ptr<Q>(elements: *const Q) -> *const usize {
        let elem_alignment = core::mem::align_of::<T>();
        let ptr = elements as *const usize;

        unsafe {
            if elem_alignment <= core::mem::align_of::<usize>() {
                ptr.offset(1)
            } else {
                // If elements have an alignment bigger than usize (e.g. an i128),
                // we will have necessarily allocated two usize slots worth of
                // space for the storage value (with the first usize slot being
                // padding for alignment's sake), and we need to skip past both.
                ptr.offset(2)
            }
        }
    }

    pub fn from_slice_with_capacity(slice: &[T], capacity: usize) -> RocList<T>
    where
        T: Copy,
    {
        assert!(slice.len() <= capacity);

        let ptr = slice.as_ptr();
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
            let raw_ptr = libc::malloc(num_bytes);

            // write the capacity
            let capacity_ptr = raw_ptr as *mut usize;
            *capacity_ptr = capacity;

            let raw_ptr = Self::get_element_ptr(raw_ptr as *mut T);

            {
                // NOTE: using a memcpy here causes weird issues
                let target_ptr = raw_ptr as *mut T;
                let source_ptr = ptr as *const T;
                let length = slice.len() as isize;
                for index in 0..length {
                    *target_ptr.offset(index) = *source_ptr.offset(index);
                }
            }

            raw_ptr as *mut T
        };

        RocList {
            length: slice.len(),
            elements,
        }
    }

    pub fn from_slice(slice: &[T]) -> RocList<T>
    where
        T: Copy,
    {
        Self::from_slice_with_capacity(slice, slice.len())
    }

    pub fn as_slice(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.elements, self.length) }
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

        for i in 0..(self.length as isize) {
            unsafe {
                if *self.elements.offset(i) != *other.elements.offset(i) {
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
        use Storage::*;
        match self.storage() {
            None | Some(ReadOnly) => {}
            Some(Capacity(_)) | Some(Refcounted(REFCOUNT_1)) => unsafe {
                libc::free(self.get_storage_ptr() as *mut libc::c_void);
            },
            Some(Refcounted(rc)) => {
                let sptr = self.get_storage_ptr_mut();
                unsafe {
                    *sptr = rc - 1;
                }
            }
        }
    }
}
