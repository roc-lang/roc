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

use crate::RocList;

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

    pub fn capacity(&self) -> usize {
        match self.as_enum_ref() {
            RocStrInnerRef::HeapAllocated(roc_list) => roc_list.capacity(),
            RocStrInnerRef::SmallString(_) => SmallString::CAPACITY,
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

    /// Note that there is no way to convert directly to a String.
    ///
    /// This is because RocStr values are not allocated using the system allocator, so
    /// handing off any heap-allocated bytes to a String would not work because its Drop
    /// implementation would try to free those bytes using the wrong allocator.
    ///
    /// Instead, if you want a Rust String, you need to do a fresh allocation and copy the
    /// bytes over - in other words, calling this `as_str` method and then calling `to_string`
    /// on that.
    pub fn as_str(&self) -> &str {
        &*self
    }

    /// Returns the index of the first interior \0 byte in the string, or None if there are none.
    fn first_nul_byte(&self) -> Option<usize> {
        match self.as_enum_ref() {
            RocStrInnerRef::HeapAllocated(roc_list) => roc_list.iter().position(|byte| *byte == 0),
            RocStrInnerRef::SmallString(small_string) => small_string.first_nul_byte(),
        }
    }

    // If the string is under this many bytes, the into_temp_c_str method will allocate the
    // CStr on the stack when the RocStr is non-unique.
    const TEMP_CSTR_MAX_STACK_BYTES: usize = 64;

    /// Turn this RocStr into a &CStr and then provide access to that &CStr for the duration
    /// of a given function. This does not allocate when given a small string or a string with
    /// unique refcount, but may allocate when given a large string with non-unique refcount.
    /// (It will do a stack allocation if the string is under 64 bytes.)
    ///
    /// Because this works on an owned RocStr, it's able to overwrite the underlying bytes
    /// to null-terminate the string in-place. Small strings have an extra byte at the end
    /// where the length is stored, which can become 0 for null-termination. Heap-allocated
    /// strings can have excess capacity which can hold a null termiator, or if they have no
    /// excess capacity, all the bytes can be shifted over the refcount in order to free up
    /// a `usize` worth of free space at the end - which can easily fit a null terminator.
    ///
    /// This operation can fail because a RocStr may contain \0 characters, which a CStr cannot.
    /// Also it can fail due to the RocStr not being null-terminated, which it might
    /// coincidentally be if it has excess capacity and the first byte of excess capacity is 0.
    #[cfg(not(feature = "no_std"))]
    pub fn into_temp_c_str<T, F: Fn(&CStr) -> T>(self, func: F) -> Result<T, InteriorNulError> {
        use core::mem::MaybeUninit;

        use crate::{roc_alloc, roc_dealloc};

        if let Some(pos) = self.first_nul_byte() {
            return Err(InteriorNulError { pos, roc_str: self });
        }

        match self.as_enum_ref() {
            RocStrInnerRef::HeapAllocated(roc_list) => {
                unsafe {
                    match roc_list.elements_and_storage() {
                        Some((_, storage)) if storage.get().is_unique() => {
                            // The backing RocList was unique, so we can mutate it in-place.
                            let len = roc_list.len();

                            if len < roc_list.capacity() {
                                // We happen to have excess capacity, so write a null terminator
                                // into the first byte of excess capacity.
                                let ptr = roc_list.ptr_to_first_elem();

                                *((ptr.add(len)) as *mut u8) = 0;

                                let c_str =
                                    CStr::from_bytes_with_nul_unchecked(roc_list.as_slice());

                                Ok(func(c_str))
                            } else {
                                // We always have an allocation that's even bigger than necessary,
                                // because the refcount bytes take up more than the 1B needed for
                                // the \0 at the end. We just need to shift the bytes over on top
                                // of the refcount.
                                let alloc_ptr = roc_list.ptr_to_allocation() as *mut _;

                                // First, copy the bytes over the original allocation - effectively
                                // shifting everything over by one `usize`. Now we no longer have a
                                // refcount (but the &CStr won't use that anyway), but we do have a
                                // free `usize` at the end.
                                //
                                // IMPORTANT: Must use memmove instead of memcpy because the regions
                                // overlap. Passing overlapping regions to memcpy is undefined!
                                libc::memmove(alloc_ptr, roc_list.ptr_to_first_elem().cast(), len);

                                let bytes = std::slice::from_raw_parts(alloc_ptr.cast(), len);
                                let c_str = CStr::from_bytes_with_nul_unchecked(bytes);

                                Ok(func(c_str))
                            }
                        }
                        Some(_) => {
                            use crate::roc_memcpy;

                            // The backing list was not unique, so we can't mutate it in-place.
                            let len = roc_list.len();

                            if len < Self::TEMP_CSTR_MAX_STACK_BYTES {
                                // TODO: once https://doc.rust-lang.org/std/mem/union.MaybeUninit.html#method.uninit_array
                                // has become stabilized, use that here in order to do a precise
                                // stack allocation instead of always over-allocating to 64B.
                                let mut bytes: MaybeUninit<[u8; Self::TEMP_CSTR_MAX_STACK_BYTES]> =
                                    MaybeUninit::uninit();
                                let alloc_ptr = bytes.as_mut_ptr() as *mut u8;
                                let elem_ptr = roc_list.ptr_to_first_elem() as *mut _;

                                // memcpy the bytes into the stack allocation
                                roc_memcpy(alloc_ptr.cast(), elem_ptr, len);

                                // Null-terminate the new allocation.
                                *(alloc_ptr.add(len)) = 0;

                                // Convert the bytes to &CStr
                                let bytes = std::slice::from_raw_parts(alloc_ptr.cast(), len);
                                let c_str = CStr::from_bytes_with_nul_unchecked(bytes);

                                Ok(func(c_str))
                            } else {
                                // The string is too long to stack-allocate, so
                                // do a heap allocation and then free it afterwards.
                                let align = core::mem::align_of::<u8>() as u32;
                                let alloc_ptr = roc_alloc(len, align);
                                let elem_ptr = roc_list.ptr_to_first_elem() as *mut _;

                                // memcpy the bytes into the heap allocation
                                roc_memcpy(alloc_ptr.cast(), elem_ptr, len);

                                // Null-terminate the new allocation.
                                *((alloc_ptr as *mut u8).add(len)) = 0;

                                // Convert the bytes to &CStr
                                let bytes = std::slice::from_raw_parts(alloc_ptr.cast(), len);
                                let c_str = CStr::from_bytes_with_nul_unchecked(bytes);

                                // Pass the &CStr to the function to get the answer.
                                let answer = func(c_str);

                                // Free the heap allocation.
                                roc_dealloc(alloc_ptr, align);

                                Ok(answer)
                            }
                        }
                        None => {
                            // The backing list was empty.
                            //
                            // No need to do a heap allocation for an empty &CStr - we
                            // can just do a stack allocation that will live for the
                            // duration of the function.
                            let c_str = CStr::from_bytes_with_nul_unchecked(&[0]);

                            Ok(func(c_str))
                        }
                    }
                }
            }
            RocStrInnerRef::SmallString(small_str) => {
                // Set the length byte to 0 to guarantee null-termination.
                // We may alredy happen to be null-terminated if small_str.len() is
                // less than the maximum small string length, but we can't assume that,
                // and checking first would be more expensive than always writing the 0.
                unsafe {
                    let mut bytes = small_str.bytes;
                    let ptr = bytes.as_mut_ptr();

                    *((ptr.add(small_str.len())) as *mut u8) = 0;

                    let c_str = CStr::from_bytes_with_nul_unchecked(&bytes);

                    Ok(func(c_str))
                }
            }
        }
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

/// This can fail because a CStr may contain invalid UTF-8 characters
#[cfg(not(feature = "no_std"))]
impl TryFrom<&CStr> for RocStr {
    type Error = core::str::Utf8Error;

    fn try_from(c_str: &CStr) -> Result<Self, Self::Error> {
        c_str.to_str().map(RocStr::from)
    }
}

/// This can fail because a CString may contain invalid UTF-8 characters
#[cfg(not(feature = "no_std"))]
impl TryFrom<CString> for RocStr {
    type Error = core::str::Utf8Error;

    fn try_from(c_string: CString) -> Result<Self, Self::Error> {
        c_string.to_str().map(RocStr::from)
    }
}

#[cfg(not(feature = "no_std"))]
/// Like https://doc.rust-lang.org/std/ffi/struct.NulError.html but
/// only for interior nulls, not for missing null terminators.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InteriorNulError {
    pub pos: usize,
    pub roc_str: RocStr,
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

    /// Returns the index of the first interior \0 byte in the string, or None if there are none.
    fn first_nul_byte(&self) -> Option<usize> {
        self.bytes.iter().position(|byte| *byte == 0)
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

#[cfg(test)]
mod into_temp_c_str {
    use super::RocStr;
    use core::mem;

    #[test]
    fn empty_string() {
        let is_empty = RocStr::empty().into_temp_c_str(|c_str| c_str.to_bytes().is_empty());

        assert_eq!(Ok(true), is_empty);
    }

    #[test]
    fn small_string_all_lengths() {
        for len in 1..mem::size_of::<RocStr>() {
            let bytes: Vec<u8> = (1..=len as u8).collect();

            assert_eq!(bytes.len(), len);

            // The bytes should contain no nul characters.
            assert!(bytes.iter().all(|byte| *byte != 0));

            // e.g. "1" or "12" or "12345" etc.
            let string = String::from_utf8(bytes).unwrap();

            let answer = RocStr::from(string.as_str()).into_temp_c_str(|c_str| {
                assert_eq!(c_str.to_str(), Ok(string.as_str()));

                42
            });

            assert_eq!(Ok(42), answer);
        }
    }
}
