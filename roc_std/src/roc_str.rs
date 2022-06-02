#![deny(unsafe_op_in_unsafe_fn)]

use core::{
    convert::TryFrom,
    fmt::Debug,
    hash::Hash,
    mem::{size_of, ManuallyDrop},
    ops::{Deref, DerefMut},
};

#[cfg(not(feature = "no_std"))]
use std::ffi::{CStr, CString};

use crate::{roc_alloc, roc_memcpy, RocList};

#[repr(transparent)]
pub struct RocStr(RocStrInner);

impl RocStr {
    pub const SIZE: usize = core::mem::size_of::<Self>();
    pub const MASK: u8 = 0b1000_0000;

    pub const fn empty() -> Self {
        Self(RocStrInner {
            small_string: SmallString::empty(),
        })
    }

    /// Create a string from bytes.
    ///
    /// # Safety
    ///
    /// `slice` must be valid UTF-8.
    pub unsafe fn from_slice_unchecked(slice: &[u8]) -> Self {
        if let Some(small_string) = unsafe { SmallString::try_from_utf8_bytes(slice) } {
            Self(RocStrInner { small_string })
        } else {
            let heap_allocated = RocList::from_slice(slice);
            Self(RocStrInner {
                heap_allocated: ManuallyDrop::new(heap_allocated),
            })
        }
    }

    fn is_small_str(&self) -> bool {
        unsafe { self.0.small_string.is_small_str() }
    }

    fn as_enum_ref(&self) -> RocStrInnerRef {
        if self.is_small_str() {
            unsafe { RocStrInnerRef::SmallString(&self.0.small_string) }
        } else {
            unsafe { RocStrInnerRef::HeapAllocated(&self.0.heap_allocated) }
        }
    }

    fn iter_bytes(&self) -> impl ExactSizeIterator<Item = u8> + '_ {
        match self.as_enum_ref() {
            RocStrInnerRef::HeapAllocated(roc_list) => roc_list.iter().copied(),
            RocStrInnerRef::SmallString(small_str) => small_str.bytes.iter().copied(),
        }
    }

    pub fn len(&self) -> usize {
        match self.as_enum_ref() {
            RocStrInnerRef::HeapAllocated(h) => h.len(),
            RocStrInnerRef::SmallString(s) => s.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn as_str(&self) -> &str {
        &*self
    }

    #[cfg(not(feature = "no_std"))]
    pub fn to_cstring(self) -> Option<CString> {
        if self.iter_bytes().any(|byte| byte == 0) {
            None
        } else {
            Some(unsafe { self.to_cstring_unchecked() })
        }
    }

    /// C strings must not have any \0 bytes in them. This does not check for that,
    /// and instead assumes that the RocStr has no \0 bytes in the middle.
    #[cfg(not(feature = "no_std"))]
    pub unsafe fn to_cstring_unchecked(self) -> CString {
        use crate::Storage;
        use core::{ffi::c_void, mem};

        let len;
        let alloc_ptr = match self.as_enum_ref() {
            RocStrInnerRef::HeapAllocated(roc_list) => {
                len = roc_list.len();

                // We already have an allocation that's even bigger than necessary, because
                // the refcount bytes take up more than the 1B needed for the \0 at the end.
                unsafe {
                    let alloc_ptr = roc_list.ptr_to_allocation() as *mut libc::c_void;

                    // First, copy the bytes over the original allocation - effectively scooting
                    // everything over by one `usize`. Now we no longer have a refcount (but the
                    // CString wouldn't use that anyway), but we do have a free `usize` at the end.
                    //
                    // IMPORTANT: Must use memmove instead of memcpy because the regions overlap.
                    // Passing overlapping regions to memcpy is undefined behavior!
                    libc::memmove(alloc_ptr, roc_list.ptr_to_first_elem(), len);

                    alloc_ptr
                }
            }
            RocStrInnerRef::SmallString(small_str) => {
                let align = mem::align_of::<Storage>() as u32;

                len = small_str.len();

                // Make a new allocation, then copy the bytes over and null-terminate.
                unsafe {
                    // Use len + 1 to make room for the null terminateor.
                    let alloc_ptr = roc_alloc(len + 1, align);

                    // Copy the contents of the small string into the new allocation
                    roc_memcpy(alloc_ptr, small_str.bytes_ptr() as *mut c_void, len);

                    // Null-terminate
                    *((alloc_ptr as *mut u8).add(len)) = 0;

                    alloc_ptr
                }
            }
        };

        let c_string = unsafe {
            // Null-terminate the string by writing a zero to the end, where we now
            // have a free `usize` worth of space. Don't write an entire `usize` in
            // there, because it might not be aligned properly (depending on whether
            // len() happens to be aligned to `usize`). Null-termination only needs
            // 1 byte anyway.
            *(alloc_ptr as *mut u8).add(len) = 0;

            CString::from_raw(alloc_ptr as *mut std::os::raw::c_char)
        };

        core::mem::forget(self);

        c_string
    }
}

impl Deref for RocStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self.as_enum_ref() {
            RocStrInnerRef::HeapAllocated(h) => unsafe { core::str::from_utf8_unchecked(&*h) },
            RocStrInnerRef::SmallString(s) => &*s,
        }
    }
}

#[cfg(not(feature = "no_std"))]
impl core::convert::TryFrom<&CStr> for RocStr {
    type Error = core::str::Utf8Error;

    fn try_from(c_str: &CStr) -> Result<Self, Self::Error> {
        c_str.to_str().map(RocStr::from)
    }
}

#[cfg(not(feature = "no_std"))]
impl core::convert::TryFrom<CString> for RocStr {
    type Error = core::str::Utf8Error;

    fn try_from(c_string: CString) -> Result<Self, Self::Error> {
        c_string.to_str().map(RocStr::from)
    }
}

impl Default for RocStr {
    fn default() -> Self {
        Self::empty()
    }
}

impl From<&str> for RocStr {
    fn from(s: &str) -> Self {
        unsafe { Self::from_slice_unchecked(s.as_bytes()) }
    }
}

impl PartialEq for RocStr {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl Eq for RocStr {}

impl PartialOrd for RocStr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl Ord for RocStr {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl Debug for RocStr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.deref().fmt(f)
    }
}

impl Clone for RocStr {
    fn clone(&self) -> Self {
        match self.as_enum_ref() {
            RocStrInnerRef::HeapAllocated(h) => Self(RocStrInner {
                heap_allocated: ManuallyDrop::new(h.clone()),
            }),
            RocStrInnerRef::SmallString(s) => Self(RocStrInner { small_string: *s }),
        }
    }
}

impl Drop for RocStr {
    fn drop(&mut self) {
        if !self.is_small_str() {
            unsafe {
                ManuallyDrop::drop(&mut self.0.heap_allocated);
            }
        }
    }
}

#[repr(C)]
union RocStrInner {
    heap_allocated: ManuallyDrop<RocList<u8>>,
    small_string: SmallString,
}

enum RocStrInnerRef<'a> {
    HeapAllocated(&'a RocList<u8>),
    SmallString(&'a SmallString),
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct SmallString {
    bytes: [u8; Self::CAPACITY],
    len: u8,
}

impl SmallString {
    const CAPACITY: usize = size_of::<RocList<u8>>() - 1;

    const fn empty() -> Self {
        Self {
            bytes: [0; Self::CAPACITY],
            len: RocStr::MASK,
        }
    }

    /// # Safety
    ///
    /// `slice` must be valid UTF-8.
    unsafe fn try_from_utf8_bytes(slice: &[u8]) -> Option<Self> {
        // Check the size of the slice.
        let len_as_u8 = u8::try_from(slice.len()).ok()?;
        if (len_as_u8 as usize) > Self::CAPACITY {
            return None;
        }

        // Construct the small string.
        let mut bytes = [0; Self::CAPACITY];
        bytes[..slice.len()].copy_from_slice(slice);
        Some(Self {
            bytes,
            len: len_as_u8 | RocStr::MASK,
        })
    }

    fn is_small_str(&self) -> bool {
        self.len & RocStr::MASK != 0
    }

    fn len(&self) -> usize {
        usize::from(self.len & !RocStr::MASK)
    }

    fn bytes_ptr(&self) -> *const u8 {
        self.bytes.as_ptr()
    }
}

impl Deref for SmallString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        let len = self.len();
        unsafe { core::str::from_utf8_unchecked(self.bytes.get_unchecked(..len)) }
    }
}

impl DerefMut for SmallString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let len = self.len();
        unsafe { core::str::from_utf8_unchecked_mut(self.bytes.get_unchecked_mut(..len)) }
    }
}

impl Hash for RocStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}
