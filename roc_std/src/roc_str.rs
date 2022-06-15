#![deny(unsafe_op_in_unsafe_fn)]

use core::{
    cmp,
    convert::TryFrom,
    fmt,
    hash::{self, Hash},
    mem::{self, size_of, ManuallyDrop},
    ops::{Deref, DerefMut},
    ptr,
};

#[cfg(not(feature = "no_std"))]
use std::ffi::{CStr, CString};

use crate::RocList;

#[repr(transparent)]
pub struct RocStr(RocStrInner);

fn with_stack_bytes<F, E, T>(length: usize, closure: F) -> T
where
    F: FnOnce(*mut E) -> T,
{
    use crate::{roc_alloc, roc_dealloc};
    use core::mem::MaybeUninit;

    if length < RocStr::TEMP_STR_MAX_STACK_BYTES {
        // TODO: once https://doc.rust-lang.org/std/mem/union.MaybeUninit.html#method.uninit_array
        // has become stabilized, use that here in order to do a precise
        // stack allocation instead of always over-allocating to 64B.
        let mut bytes: MaybeUninit<[u8; RocStr::TEMP_STR_MAX_STACK_BYTES]> = MaybeUninit::uninit();

        closure(bytes.as_mut_ptr() as *mut E)
    } else {
        let align = core::mem::align_of::<E>() as u32;
        // The string is too long to stack-allocate, so
        // do a heap allocation and then free it afterwards.
        let ptr = unsafe { roc_alloc(length, align) } as *mut E;
        let answer = closure(ptr);

        // Free the heap allocation.
        unsafe { roc_dealloc(ptr.cast(), align) };

        answer
    }
}

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

    /// Create a string from bytes.
    ///
    /// # Safety
    ///
    /// `ptr` must point to `length` valid UTF-8 bytes.
    pub unsafe fn from_raw_parts(ptr: *const u8, length: usize, _capacity: usize) -> Self {
        let slice = unsafe { std::slice::from_raw_parts(ptr, length) };

        unsafe { Self::from_slice_unchecked(slice) }
    }

    pub fn is_small_str(&self) -> bool {
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

    /// Create an empty RocStr with enough space preallocated to store
    /// the requested number of bytes.
    pub fn with_capacity(bytes: usize) -> Self {
        if bytes <= SmallString::CAPACITY {
            RocStr(RocStrInner {
                small_string: SmallString::empty(),
            })
        } else {
            // The requested capacity won't fit in a small string; we need to go big.
            RocStr(RocStrInner {
                heap_allocated: ManuallyDrop::new(RocList::with_capacity(bytes)),
            })
        }
    }

    /// Increase a RocStr's capacity by at least the requested number of bytes (possibly more).
    ///
    /// May return a new RocStr, if the provided one was not unique.
    pub fn reserve(&mut self, bytes: usize) {
        if self.is_small_str() {
            let small_str = unsafe { self.0.small_string };
            let target_cap = small_str.len() + bytes;

            if target_cap > SmallString::CAPACITY {
                // The requested capacity won't fit in a small string; we need to go big.
                let mut roc_list = RocList::with_capacity(target_cap);

                roc_list.extend_from_slice(small_str.as_bytes());

                *self = RocStr(RocStrInner {
                    heap_allocated: ManuallyDrop::new(roc_list),
                });
            }
        } else {
            let mut roc_list = unsafe { ManuallyDrop::take(&mut self.0.heap_allocated) };

            roc_list.reserve(bytes);

            let mut updated = RocStr(RocStrInner {
                heap_allocated: ManuallyDrop::new(roc_list),
            });

            mem::swap(self, &mut updated);
            mem::forget(updated);
        }
    }

    /// Returns the index of the first interior \0 byte in the string, or None if there are none.
    fn first_nul_byte(&self) -> Option<usize> {
        match self.as_enum_ref() {
            RocStrInnerRef::HeapAllocated(roc_list) => roc_list.iter().position(|byte| *byte == 0),
            RocStrInnerRef::SmallString(small_string) => small_string.first_nul_byte(),
        }
    }

    // If the string is under this many bytes, the with_terminator family
    // of methods will allocate the terminated string on the stack when
    // the RocStr is non-unique.
    const TEMP_STR_MAX_STACK_BYTES: usize = 64;

    /// Like calling with_utf8_terminator passing \0 for the terminator,
    /// except it can fail because a RocStr may contain \0 characters,
    /// which a nul-terminated string must not.
    pub fn utf8_nul_terminated<T, F: Fn(*mut u8, usize) -> T>(
        self,
        func: F,
    ) -> Result<T, InteriorNulError> {
        if let Some(pos) = self.first_nul_byte() {
            Err(InteriorNulError { pos, roc_str: self })
        } else {
            Ok(self.with_utf8_terminator(b'\0', func))
        }
    }

    /// Turn this RocStr into a UTF-8 `*mut u8`, terminate it with the given character
    /// (commonly either `b'\n'` or b`\0`) and then provide access to that
    /// `*mut u8` (as well as its length) for the duration of a given function. This is
    /// designed to be an efficient way to turn a `RocStr` received from an application into
    /// either the nul-terminated UTF-8 `char*` needed by UNIX syscalls, or into a
    /// newline-terminated string to write to stdout or stderr (for a "println"-style effect).
    ///
    /// **NOTE:** The length passed to the function is the same value that `RocStr::len` will
    /// return; it does not count the terminator. So to convert it to a nul-terminated slice
    /// of Rust bytes (for example), call `slice::from_raw_parts` passing the given length + 1.
    ///
    /// This operation achieves efficiency by reusing allocated bytes from the RocStr itself,
    /// and sometimes allocating on the stack. It does not allocate on the heap when given a
    /// a small string or a string with unique refcount, but may allocate when given a large
    /// string with non-unique refcount. (It will do a stack allocation if the string is under
    /// 64 bytes; the stack allocation will only live for the duration of the called function.)
    ///
    /// If the given (owned) RocStr is unique, this can overwrite the underlying bytes
    /// to terminate the string in-place. Small strings have an extra byte at the end
    /// where the length is stored, which can be replaced with the terminator. Heap-allocated
    /// strings can have excess capacity which can hold a terminator, or if they have no
    /// excess capacity, all the bytes can be shifted over the refcount in order to free up
    /// a `usize` worth of free space at the end - which can easily fit a 1-byte terminator.
    pub fn with_utf8_terminator<T, F: Fn(*mut u8, usize) -> T>(self, terminator: u8, func: F) -> T {
        // Note that this function does not use with_terminator because it can be
        // more efficient than that - due to knowing that it's already in UTF-8 and always
        // has room for a 1-byte terminator in the existing allocation (either in the refcount
        // bytes, or, in a small string, in the length at the end of the string).

        let terminate = |alloc_ptr: *mut u8, len: usize| unsafe {
            *(alloc_ptr.add(len)) = terminator;

            func(alloc_ptr, len)
        };

        match self.as_enum_ref() {
            RocStrInnerRef::HeapAllocated(roc_list) => {
                unsafe {
                    match roc_list.storage() {
                        Some(storage) if storage.is_unique() => {
                            // The backing RocList was unique, so we can mutate it in-place.
                            let len = roc_list.len();
                            let ptr = if len < roc_list.capacity() {
                                // We happen to have excess capacity already, so we will be able
                                // to write the terminator into the first byte of excess capacity.
                                roc_list.ptr_to_first_elem() as *mut u8
                            } else {
                                // We always have an allocation that's even bigger than necessary,
                                // because the refcount bytes take up more than the 1B needed for
                                // the terminator. We just need to shift the bytes over on top
                                // of the refcount.
                                let alloc_ptr = roc_list.ptr_to_allocation() as *mut u8;

                                // First, copy the bytes over the original allocation - effectively
                                // shifting everything over by one `usize`. Now we no longer have a
                                // refcount (but the terminated won't use that anyway), but we do
                                // have a free `usize` at the end.
                                //
                                // IMPORTANT: Must use ptr::copy instead of ptr::copy_nonoverlapping
                                // because the regions definitely overlap!
                                ptr::copy(roc_list.ptr_to_first_elem() as *mut u8, alloc_ptr, len);

                                alloc_ptr
                            };

                            terminate(ptr, len)
                        }
                        Some(_) => {
                            let len = roc_list.len();

                            // The backing list was not unique, so we can't mutate it in-place.
                            // ask for `len + 1` to store the original string and the terminator
                            with_stack_bytes(len + 1, |alloc_ptr: *mut u8| {
                                let alloc_ptr = alloc_ptr as *mut u8;
                                let elem_ptr = roc_list.ptr_to_first_elem() as *mut u8;

                                // memcpy the bytes into the stack allocation
                                ptr::copy_nonoverlapping(elem_ptr, alloc_ptr, len);

                                terminate(alloc_ptr, len)
                            })
                        }
                        None => {
                            // The backing list was empty.
                            //
                            // No need to do a heap allocation for an empty string - we
                            // can just do a stack allocation that will live for the
                            // duration of the function.
                            func([terminator].as_mut_ptr(), 0)
                        }
                    }
                }
            }
            RocStrInnerRef::SmallString(small_str) => {
                let mut bytes = small_str.bytes;

                // Even if the small string is at capacity, there will be room to write
                // a terminator in the byte that's used to store the length.
                terminate(bytes.as_mut_ptr() as *mut u8, small_str.len())
            }
        }
    }

    /// Like calling with_utf16_terminator passing \0 for the terminator,
    /// except it can fail because a RocStr may contain \0 characters,
    /// which a nul-terminated string must not.
    pub fn utf16_nul_terminated<T, F: Fn(*mut u16, usize) -> T>(
        self,
        func: F,
    ) -> Result<T, InteriorNulError> {
        if let Some(pos) = self.first_nul_byte() {
            Err(InteriorNulError { pos, roc_str: self })
        } else {
            Ok(self.with_utf16_terminator(0, func))
        }
    }

    /// Turn this RocStr into a nul-terminated UTF-16 `*mut u16` and then provide access to
    /// that `*mut u16` (as well as its length) for the duration of a given function. This is
    /// designed to be an efficient way to turn a RocStr received from an application into
    /// the nul-terminated UTF-16 `wchar_t*` needed by Windows API calls.
    ///
    /// **NOTE:** The length passed to the function is the same value that `RocStr::len` will
    /// return; it does not count the terminator. So to convert it to a nul-terminated
    /// slice of Rust bytes, call `slice::from_raw_parts` passing the given length + 1.
    ///
    /// This operation achieves efficiency by reusing allocated bytes from the RocStr itself,
    /// and sometimes allocating on the stack. It does not allocate on the heap when given a
    /// a small string or a string with unique refcount, but may allocate when given a large
    /// string with non-unique refcount. (It will do a stack allocation if the string is under
    /// 64 bytes; the stack allocation will only live for the duration of the called function.)
    ///
    /// Because this works on an owned RocStr, it's able to overwrite the underlying bytes
    /// to nul-terminate the string in-place. Small strings have an extra byte at the end
    /// where the length is stored, which can become 0 for nul-termination. Heap-allocated
    /// strings can have excess capacity which can hold a termiator, or if they have no
    /// excess capacity, all the bytes can be shifted over the refcount in order to free up
    /// a `usize` worth of free space at the end - which can easily fit a terminator.
    ///
    /// This operation can fail because a RocStr may contain \0 characters, which a
    /// nul-terminated string must not.
    pub fn with_utf16_terminator<T, F: Fn(*mut u16, usize) -> T>(
        self,
        terminator: u16,
        func: F,
    ) -> T {
        self.with_terminator(terminator, |dest_ptr: *mut u16, str_slice: &str| {
            // Translate UTF-8 source bytes into UTF-16 and write them into the destination.
            for (index, wchar) in str_slice.encode_utf16().enumerate() {
                unsafe {
                    *(dest_ptr.add(index)) = wchar;
                }
            }

            func(dest_ptr, str_slice.len())
        })
    }

    pub fn with_windows_path<T, F: Fn(*mut u16, usize) -> T>(
        self,
        func: F,
    ) -> Result<T, InteriorNulError> {
        if let Some(pos) = self.first_nul_byte() {
            Err(InteriorNulError { pos, roc_str: self })
        } else {
            let answer = self.with_terminator(0u16, |dest_ptr: *mut u16, str_slice: &str| {
                // Translate UTF-8 source bytes into UTF-16 and write them into the destination.
                for (index, mut wchar) in str_slice.encode_utf16().enumerate() {
                    // Replace slashes with backslashes
                    if wchar == '/' as u16 {
                        wchar = '\\' as u16
                    };

                    unsafe {
                        *(dest_ptr.add(index)) = wchar;
                    }
                }

                func(dest_ptr, str_slice.len())
            });

            Ok(answer)
        }
    }

    /// Generic version of temp_c_utf8 and temp_c_utf16. The given function will be
    /// passed a pointer to elements of type E. The pointer will have enough room to hold
    /// one element for each byte of the given `&str`'s length, plus the terminator element.
    ///
    /// The terminator will be written right after the end of the space for the other elements,
    /// but all the memory in that space before the terminator will be uninitialized. This means
    /// if you want to do something like copy the contents of the `&str` into there, that will
    /// need to be done explicitly.
    ///
    /// The terminator is always written - even if there are no other elements present before it.
    /// (In such a case, the `&str` argument will be empty and the `*mut E` will point directly
    /// to the terminator).
    ///
    /// One use for this is to convert slashes to backslashes in Windows paths;
    /// this function provides the most efficient way to do that, because no extra
    /// iteration pass is necessary; the conversion can be done after each translation
    /// of a UTF-8 character to UTF-16. Here's how that would look:
    ///
    ///     use roc_std::{RocStr, InteriorNulError};
    ///
    ///     pub fn with_windows_path<T, F: Fn(*mut u16, usize) -> T>(
    ///         roc_str: RocStr,
    ///         func: F,
    ///     ) -> Result<T, InteriorNulError> {
    ///         let answer = roc_str.with_terminator(0u16, |dest_ptr: *mut u16, str_slice: &str| {
    ///             // Translate UTF-8 source bytes into UTF-16 and write them into the destination.
    ///             for (index, mut wchar) in str_slice.encode_utf16().enumerate() {
    ///                 // Replace slashes with backslashes
    ///                 if wchar == '/' as u16 {
    ///                     wchar = '\\' as u16
    ///                 };
    ///
    ///                 unsafe {
    ///                     *(dest_ptr.add(index)) = wchar;
    ///                 }
    ///             }
    ///
    ///             func(dest_ptr, str_slice.len())
    ///         });
    ///
    ///         Ok(answer)
    ///     }
    pub fn with_terminator<E: Copy, A, F: Fn(*mut E, &str) -> A>(
        self,
        terminator: E,
        func: F,
    ) -> A {
        use crate::Storage;
        use core::mem::align_of;

        let terminate = |alloc_ptr: *mut E, str_slice: &str| unsafe {
            *(alloc_ptr.add(str_slice.len())) = terminator;

            func(alloc_ptr, str_slice)
        };

        // When we don't have an existing allocation that can work, fall back on this.
        // It uses either a stack allocation, or, if that would be too big, a heap allocation.
        let fallback = |str_slice: &str| {
            // We need 1 extra elem for the terminator. It must be an elem,
            // not a byte, because we'll be providing a pointer to elems.
            let needed_bytes = (str_slice.len() + 1) * size_of::<E>();

            with_stack_bytes(needed_bytes, |alloc_ptr: *mut E| {
                terminate(alloc_ptr, str_slice)
            })
        };

        match self.as_enum_ref() {
            RocStrInnerRef::HeapAllocated(roc_list) => {
                let len = roc_list.len();

                unsafe {
                    match roc_list.storage() {
                        Some(storage) if storage.is_unique() => {
                            // The backing RocList was unique, so we can mutate it in-place.

                            // We need 1 extra elem for the terminator. It must be an elem,
                            // not a byte, because we'll be providing a pointer to elems.
                            let needed_bytes = (len + 1) * size_of::<E>();

                            // We can use not only the capacity on the heap, but also
                            // the bytes originally used for the refcount.
                            let available_bytes = roc_list.capacity() + size_of::<Storage>();

                            if needed_bytes < available_bytes {
                                debug_assert!(align_of::<Storage>() >= align_of::<E>());

                                // We happen to have sufficient excess capacity already,
                                // so we will be able to write the new elements as well as
                                // the terminator into the existing allocation.
                                let ptr = roc_list.ptr_to_allocation() as *mut E;
                                let answer = terminate(ptr, self.as_str());

                                // We cannot rely on the RocStr::drop implementation, because
                                // it tries to use the refcount - which we just overwrote
                                // with string bytes.
                                mem::forget(self);
                                crate::roc_dealloc(ptr.cast(), mem::align_of::<E>() as u32);

                                answer
                            } else {
                                // We didn't have sufficient excess capacity already,
                                // so we need to do either a new stack allocation or a new
                                // heap allocation.
                                fallback(self.as_str())
                            }
                        }
                        Some(_) => {
                            // The backing list was not unique, so we can't mutate it in-place.
                            fallback(self.as_str())
                        }
                        None => {
                            // The backing list was empty.
                            //
                            // No need to do a heap allocation for an empty string - we
                            // can just do a stack allocation that will live for the
                            // duration of the function.
                            func([terminator].as_mut_ptr() as *mut E, "")
                        }
                    }
                }
            }
            RocStrInnerRef::SmallString(small_str) => {
                let len = small_str.len();

                // We need 1 extra elem for the terminator. It must be an elem,
                // not a byte, because we'll be providing a pointer to elems.
                let needed_bytes = (len + 1) * size_of::<E>();
                let available_bytes = size_of::<SmallString>();

                if needed_bytes < available_bytes {
                    terminate(small_str.bytes.as_ptr() as *mut E, self.as_str())
                } else {
                    fallback(self.as_str())
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
/// only for interior nuls, not for missing nul terminators.
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
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl Ord for RocStr {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl fmt::Debug for RocStr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.deref().fmt(f)
    }
}

impl fmt::Display for RocStr {
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
        for (index, byte) in self.bytes[0..self.len()].iter().enumerate() {
            if *byte == 0 {
                return Some(index);
            }
        }

        None
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
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}
