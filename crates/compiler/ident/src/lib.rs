//! Implements data structures used for efficiently representing small strings, like identifiers.
#![warn(clippy::dbg_macro)]

use core::cmp::Ordering;
use core::convert::From;
use core::{fmt, mem, ptr, slice};
use std::alloc::{alloc, dealloc, Layout};
use std::os::raw::c_char;

/// A string which can store identifiers using the small string optimization.
/// It relies on the invariant that it cannot store null characters to store
/// an extra character; if the last byte is 0, that means it's a large string.
///
/// Because the msbyte of the length is always 0, this can only store up to
/// 2^56 bytes on a 64-bit target, or 2^28 bytes in a 32-bit target. That's
/// way more than enough for an identifier!
///
/// If it's a small string, that discriminant byte is used to store the length,
/// except it stores it as (255 - length) so that it will be in the range
/// 192 - 255 (all of which are invalid UTF-8 when in the final position of
/// a UTF-8 string). This design works on little-endian targets, but a different
/// design for storing length might be necessary on big-endian targets.

#[repr(C)]
pub struct IdentStr {
    elements: *const u8,
    length: usize,
}

impl IdentStr {
    // Reserve 1 byte for the discriminant
    const SMALL_STR_BYTES: usize = std::mem::size_of::<Self>() - 1;

    #[inline(always)]
    pub const fn len(&self) -> usize {
        let bytes = self.length.to_ne_bytes();
        let last_byte = bytes[mem::size_of::<usize>() - 1];

        // We always perform this subtraction so that the following
        // conditionals can all be cmov instructions.
        let small_str_variable_len = (u8::MAX - last_byte) as usize;

        // The numbers 192 - 255 (0xC0 - 0xFF) are not valid as the final
        // byte of a UTF-8 string. Hence they are unused and we can use them
        // to store the length of a small string!
        //
        // Reference: https://en.wikipedia.org/wiki/UTF-8#Codepage_layout
        if last_byte >= 0xC0 {
            small_str_variable_len
        } else if last_byte == 0 {
            // This is a big string, so return its length.
            self.length
        } else {
            // This is a valid UTF-8 character, meaning the entire struct must
            // be in use for storing characters.
            mem::size_of::<IdentStr>()
        }
    }

    pub const fn is_empty(&self) -> bool {
        self.length == 0
    }

    pub const fn is_small_str(&self) -> bool {
        let bytes = self.length.to_ne_bytes();
        let last_byte = bytes[mem::size_of::<usize>() - 1];

        last_byte != 0
    }

    pub fn get(&self, index: usize) -> Option<&u8> {
        self.as_bytes().get(index)
    }

    pub fn get_bytes(&self) -> *const u8 {
        if self.is_small_str() {
            self.get_small_str_ptr()
        } else {
            self.elements
        }
    }

    fn get_small_str_ptr(&self) -> *const u8 {
        (self as *const IdentStr).cast()
    }

    #[inline(always)]
    const fn small_str_from_bytes(slice: &[u8]) -> Self {
        assert!(slice.len() <= Self::SMALL_STR_BYTES);

        let len = slice.len();
        let mut bytes = [0; mem::size_of::<Self>()];

        // Copy the bytes from the slice into bytes.
        // while because for/Iterator does not work in const context
        let mut i = 0;
        while i < len {
            bytes[i] = slice[i];
            i += 1;
        }

        // Write length and small string bit to last byte of length.
        bytes[Self::SMALL_STR_BYTES] = u8::MAX - len as u8;

        unsafe { mem::transmute::<[u8; mem::size_of::<Self>()], Self>(bytes) }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(str: &str) -> Self {
        let slice = str.as_bytes();
        let len = slice.len();

        match len.cmp(&mem::size_of::<Self>()) {
            Ordering::Less => Self::small_str_from_bytes(slice),
            Ordering::Equal => {
                // This fits in a small string, and is exactly long enough to
                // take up the entire available struct
                let mut bytes = [0; mem::size_of::<Self>()];

                // Copy the bytes from the slice into the answer
                bytes.copy_from_slice(slice);

                unsafe { mem::transmute::<[u8; mem::size_of::<Self>()], Self>(bytes) }
            }
            Ordering::Greater => {
                // This needs a big string
                let align = mem::align_of::<u8>();
                let elements = unsafe {
                    let layout = Layout::from_size_align_unchecked(len, align);
                    alloc(layout)
                };

                // Turn the new elements into a slice, and copy the existing
                // slice's bytes into it.
                unsafe {
                    let dest_slice = slice::from_raw_parts_mut(elements, len);

                    dest_slice.copy_from_slice(slice);
                }

                Self {
                    length: len,
                    elements,
                }
            }
        }
    }

    #[inline(always)]
    pub fn as_slice(&self) -> &[u8] {
        use core::slice::from_raw_parts;

        if self.is_empty() {
            &[]
        } else if self.is_small_str() {
            unsafe { from_raw_parts(self.get_small_str_ptr(), self.len()) }
        } else {
            unsafe { from_raw_parts(self.elements, self.length) }
        }
    }

    #[inline(always)]
    pub fn as_str(&self) -> &str {
        let slice = self.as_slice();

        unsafe { core::str::from_utf8_unchecked(slice) }
    }

    /// Write a CStr (null-terminated) representation of this IdentStr into
    /// the given buffer.
    ///
    /// # Safety
    /// This assumes the given buffer has enough space, so make sure you only
    /// pass in a pointer to an allocation that's at least as long as this Str!
    pub unsafe fn write_c_str(&self, buf: *mut c_char) {
        let bytes = self.as_bytes();
        ptr::copy_nonoverlapping(bytes.as_ptr().cast(), buf, bytes.len());

        // null-terminate
        *buf.add(self.len()) = 0;
    }
}

impl Default for IdentStr {
    fn default() -> Self {
        Self {
            length: 0,
            elements: core::ptr::null_mut(),
        }
    }
}

impl std::ops::Deref for IdentStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl From<&str> for IdentStr {
    fn from(str: &str) -> Self {
        Self::from_str(str)
    }
}

impl From<IdentStr> for String {
    fn from(ident_str: IdentStr) -> Self {
        if ident_str.is_small_str() {
            // Copy it to a heap allocation
            ident_str.as_str().to_string()
        } else {
            // Reuse the existing heap allocation
            let string = unsafe {
                String::from_raw_parts(
                    ident_str.as_ptr() as *mut u8,
                    ident_str.len(),
                    ident_str.len(),
                )
            };

            // Make sure not to drop the IdentStr, since now there's
            // a String referencing its heap-allocated contents.
            std::mem::forget(ident_str);

            string
        }
    }
}

impl From<String> for IdentStr {
    fn from(string: String) -> Self {
        if string.len() <= Self::SMALL_STR_BYTES {
            Self::from_str(string.as_str())
        } else {
            // Take over the string's heap allocation
            let length = string.len();
            let elements = string.as_ptr();

            // Make sure the existing string doesn't get dropped.
            std::mem::forget(string);

            Self { elements, length }
        }
    }
}

impl fmt::Debug for IdentStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // IdentStr { is_small_str: false, storage: Refcounted(3), elements: [1,2,3,4] }
        f.debug_struct("IdentStr")
            //.field("is_small_str", &self.is_small_str())
            .field("string", &self.as_str())
            //.field("elements", &self.as_slice())
            .finish()
    }
}

impl fmt::Display for IdentStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // IdentStr { is_small_str: false, storage: Refcounted(3), elements: [1,2,3,4] }
        f.write_str(self.as_str())
    }
}

unsafe impl std::marker::Sync for IdentStr {}
unsafe impl std::marker::Send for IdentStr {}

impl PartialEq for IdentStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl Eq for IdentStr {}

impl PartialOrd for IdentStr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl Ord for IdentStr {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl std::hash::Hash for IdentStr {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.as_str().hash(hasher)
    }
}

impl Clone for IdentStr {
    fn clone(&self) -> Self {
        if self.is_empty() || self.is_small_str() {
            // we can just copy the bytes
            Self {
                elements: self.elements,
                length: self.length,
            }
        } else {
            Self::from_str(self.as_str())
        }
    }
}

impl Drop for IdentStr {
    fn drop(&mut self) {
        if !self.is_empty() && !self.is_small_str() {
            let align = mem::align_of::<u8>();
            unsafe {
                let layout = Layout::from_size_align_unchecked(self.length, align);
                dealloc(self.elements as *mut _, layout);
            }
        }
    }
}

#[test]
fn default() {
    let answer = IdentStr::default();

    assert_eq!(answer.len(), 0);
    assert_eq!(answer, answer);
    assert_eq!(answer.clone(), answer);
    assert_eq!(answer, answer);
    assert_eq!(answer.as_str(), "");
    assert_eq!(answer.as_str(), "");
}

#[test]
fn big_str() {
    for &string in &[
        "0123456789abcdefg",
        "0123456789abcdefgh",
        "0123456789abcdefghi",
    ] {
        let answer = IdentStr::from(string);

        assert_eq!(answer.len(), string.len());
        assert_eq!(answer, answer);
        assert_eq!(answer.clone(), answer);
        assert_eq!(answer.clone(), answer.clone());
        assert_eq!(answer.as_str(), string);
        assert_eq!(answer.clone().as_str(), string);
    }
}

#[cfg(target_pointer_width = "64")]
#[test]
fn small_var_length() {
    for &string in &[
        "",
        "0",
        "01",
        "012",
        "0123",
        "01234",
        "012345",
        "0123456",
        "01234567",
        "012345678",
        "0123456789",
        "0123456789a",
        "0123456789ab",
        "0123456789abc",
        "0123456789abcd",
        "0123456789abcde ",
    ] {
        let answer = IdentStr::from(string);

        assert_eq!(answer.len(), string.len());
        assert_eq!(answer, answer);
        assert_eq!(answer.clone(), answer);
        assert_eq!(answer.clone(), answer.clone());
        assert_eq!(answer.as_str(), string);
        assert_eq!(answer.clone().as_str(), string);
    }
}

#[cfg(target_pointer_width = "32")]
#[test]
fn small_var_length() {
    for &string in &[
        "", "0", "01", "012", "0123", "01234", "012345", "0123456", "01234567",
    ] {
        let answer = IdentStr::from(string);

        assert_eq!(answer.len(), string.len());
        assert_eq!(answer, answer);
        assert_eq!(answer.clone(), answer);
        assert_eq!(answer.clone(), answer.clone());
        assert_eq!(answer.as_str(), string);
        assert_eq!(answer.clone().as_str(), string);
    }
}

#[cfg(target_pointer_width = "64")]
#[test]
fn small_max_length() {
    let string = "0123456789abcdef";
    let answer = IdentStr::from(string);

    assert_eq!(answer.len(), string.len());
    assert_eq!(answer, answer);
    assert_eq!(answer.clone(), answer);
    assert_eq!(answer, answer);
    assert_eq!(answer.as_str(), string);
    assert_eq!(answer.as_str(), string);
}

#[cfg(target_pointer_width = "32")]
#[test]
fn small_max_length() {
    let string = "01234567";
    let answer = IdentStr::from(string);

    assert_eq!(answer.len(), string.len());
    assert_eq!(answer, answer);
    assert_eq!(answer.clone(), answer);
    assert_eq!(answer.clone(), answer.clone());
    assert_eq!(answer.as_str(), string);
    assert_eq!(answer.clone().as_str(), string);
}
