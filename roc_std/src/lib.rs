#![crate_type = "lib"]
#![no_std]
use core::fmt;
use libc;

// A list of C functions that are being imported
extern "C" {
    pub fn printf(format: *const u8, ...) -> i32;
}

const REFCOUNT_1: usize = usize::MAX - 1;

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
    #[no_mangle]
    pub fn len(&self) -> usize {
        self.length
    }

    #[no_mangle]
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    #[no_mangle]
    pub fn empty() -> Self {
        RocList {
            length: 0,
            elements: core::ptr::null_mut(),
        }
    }

    #[no_mangle]
    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            Some(unsafe {
                let raw = self.elements.offset(index as isize);
                let reference = core::mem::transmute::<*mut T, &T>(raw);

                reference
            })
        } else {
            None
        }
    }

    //#[no_mangle]
    pub fn storage(&self) -> Option<Storage> {
        use core::cmp::Ordering::*;

        if self.length == 0 {
            return None;
        }

        unsafe {
            let value = *self.get_storage_ptr();

            // NOTE doesn't work with elements of 16 or more bytes
            match usize::cmp(&0, &value) {
                Equal => Some(Storage::ReadOnly),
                Less => {
                    // let rc = (isize::MIN - (value as isize)) as usize;
                    // let rc = usize::MAX - value;
                    let rc = value;
                    Some(Storage::Refcounted(rc))
                }
                Greater => Some(Storage::Capacity(value)),
            }
        }
    }

    fn get_storage_ptr(&self) -> *const usize {
        let elem_alignment = core::mem::align_of::<T>();

        unsafe {
            if elem_alignment <= core::mem::align_of::<usize>() {
                let ptr = self.elements as *const usize;
                ptr.offset(-1)
            } else {
                let ptr = self.elements as *const (usize, usize);
                (ptr.offset(-1)) as *const usize
            }
        }
    }

    fn get_storage_ptr_mut(&mut self) -> *mut usize {
        self.get_storage_ptr() as *mut usize
    }

    fn get_element_ptr<Q>(elements: *const Q) -> *const usize {
        let elem_alignment = core::mem::align_of::<T>();

        unsafe {
            if elem_alignment <= core::mem::align_of::<usize>() {
                let ptr = elements as *const usize;
                ptr.offset(1)
            } else {
                let ptr = elements as *const (usize, usize);
                (ptr.offset(1)) as *const usize
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
        let num_bytes = core::mem::size_of::<usize>() + element_bytes;

        let num_bytes = num_bytes + (num_bytes % 16);

        let elements = unsafe {
            // dbg!(&num_bytes);
            printf("num bytes %d\n".as_ptr(), num_bytes);
            let raw_ptr = libc::malloc(num_bytes);

            // write the capacity
            let capacity_ptr = raw_ptr as *mut usize;
            *capacity_ptr = capacity;

            let raw_ptr = Self::get_element_ptr(raw_ptr as *mut T);

            libc::memcpy(
                raw_ptr as *mut libc::c_void,
                ptr as *mut libc::c_void,
                num_bytes,
            );

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
        return;

        use Storage::*;
        match self.storage() {
            None | Some(ReadOnly) => {}
            Some(Capacity(_)) | Some(Refcounted(REFCOUNT_1)) => unsafe {
                //libc::free(self.get_storage_ptr() as *mut libc::c_void);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sanity_check() {
        // TODO get the refcount/capacity of a list with 8-byte elements
        // TODO get the refcount/capacity of a list with 16-byte elements
    }
}
