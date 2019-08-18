use std::mem::{self, MaybeUninit};
use std::slice;
use std::ptr;

pub struct RocStr(InnerStr);

/// Roc strings are optimized not to do heap allocations when they are between
/// 0-15 bytes in length on 64-bit little endian systems, 
/// and 0-7 bytes on systems that are 32-bit, big endian, or both. 
///
/// This optimization relies on the assumption that string lengths are always
/// less than isize::MAX as opposed to usize::MAX. It relies on this because
/// it uses the most significant bit in the most significant byte in the length 
/// as a flag for whether it is a short string or a long string. This bit is
/// unused if lengths are below isize::MAX.
///
/// Roc integers are i64, so on 64-bit systems this guarantee necessarily holds
/// from the roc side. On a 32-bit system it might not though. Rust historically
/// had this guarantee, but it might get relaxed. For more on the Rust side, see
/// https://github.com/rust-lang/unsafe-code-guidelines/issues/102
///
/// Since Roc will interpret them as i64, it's important that on 64-bit systems,
/// Rust never sends Roc any length values outsize isize::MAX because they'll
/// be interpreted as negative i64s!
///
/// Anyway, this "is this a short string?" bit is in a convenient location on 
/// 64-bit little endian systems. This is because of how Rust's &str is 
/// laid out, and memory alignment.  
///
/// Rust's &str is laid out as a slice, namely:
///
/// struct RustStr { ptr: *const [u8], length: usize }
///
/// In little endian systems, the bit for detecting short vs long length is
/// the most significant bit of the length field, which is the very last byte
/// in the struct.
///
/// This means if we detect that we are a short string, we can pass a pointer
/// to the entire struct (which is necessarily aligned already), and its first
/// contiguous N bytes represent the bytes in the string, where N is 15 on
/// 64-bit systems and 7 on 32-bit ones. The final byte is the msbyte where
/// we stored the flag, but it doesn't matter what's in that memory because the
/// str's length will be too low to encounter that anyway.
union InnerStr {
    raw: [u8; 16],
    long: LongStr,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct LongStr {
    /// It is *crucial* that we have exactly this memory layout!
    /// This is the same layout that Rust uses for string slices in memory,
    /// which lets us transmute long strings directly into them.
    ///
    /// https://pramode.in/2016/09/13/using-unsafe-tricks-in-rust/
    bytes: MaybeUninit<*const u8>,
    length: usize,
}

// The bit pattern for an empty string. (1 and then all 0s.)
// Any other bit pattern means this is not an empty string!
#[cfg(target_pointer_width = "64")]
const EMPTY_STRING: usize = 2^63;

#[cfg(target_pointer_width = "32")]
const EMPTY_STRING: usize = 2^31;

impl RocStr {
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        unsafe { self.0.long.length == EMPTY_STRING }
    }

    #[inline(always)]
    pub fn empty() -> RocStr {
        RocStr(InnerStr {
            long: LongStr {
                length: EMPTY_STRING,
                // empty strings only ever have length set.
                bytes: MaybeUninit::uninit(),
            }
        })
    }

    pub fn len(&self) -> usize {
        let len_msbyte = self.len_msbyte();

        // This is a short string iff the last bit of len_msbyte is 1.
        if len_msbyte & 1 == 1 {
            // Shift away the "is this a short string?" flag
            let length: u8 = len_msbyte >> 1;

            length as usize
        } else {
            unsafe { self.0.long.length }
        }
    }

    /// The most significant byte in the length. We use the last bit of this
    /// byte to determine if we are a short string or a long string.
    /// If this is a short string, we intentionally set that bit to 1.
    #[inline(always)]
    #[cfg(all(target_pointer_width = "64", target_endian = "little"))]
    fn len_msbyte(&self) -> u8 {
        // TODO: can we just cast this to u8? Will truncation do what we want?
        (unsafe { mem::transmute::<usize, [u8; 8]>(self.0.long.length) })[7]
    }

    #[inline(always)]
    #[cfg(all(target_pointer_width = "32", target_endian = "little"))]
    fn len_msbyte(&self) -> u8 {
        // TODO: can we just cast this to u8? Will truncation do what we want?
        (unsafe { mem::transmute::<usize, [u8; 4]>(self.long.length) })[3]
    }

    #[inline(always)]
    #[cfg(all(target_pointer_width = "64", target_endian = "big"))]
    fn len_msbyte(&self) -> u8 {
        (unsafe { mem::transmute::<usize, [u8; 8]>(self.long.length) })[0]
    }

    #[inline(always)]
    #[cfg(all(target_pointer_width = "32", target_endian = "big"))]
    fn len_msbyte(&self) -> u8 {
        (unsafe { mem::transmute::<usize, [u8; 4]>(self.long.length) })[0]
    }
}

/// We can offer to convert to a shared string slice, but not a mutable one!
impl<'a> Into<&'a str> for &'a RocStr {
    #[cfg(all(target_pointer_width = "64", target_endian = "little"))]
    fn into(self) -> &'a str {
        let len_msbyte = self.len_msbyte();

        // This is a short string iff the last bit of len_msbyte is 1.
        if len_msbyte & 1 == 1 {
            // Shift away the "is this a short string?" flag
            let length: u8 = len_msbyte >> 1;

            unsafe { 
                // These bytes are already aligned, so we can use them directly.
                let bytes_ptr = &self.0.raw as *const u8;
                let bytes_slice: &[u8] = 
                    slice::from_raw_parts(bytes_ptr, length as usize);

                // &str in Rust has the same memory layout as an &[u8] slice.
                mem::transmute::<&[u8], &str>(bytes_slice)
            }
        } else {
            // If it's a long string, we already have the exact
            // same memory layout as a Rust &str slice.
            unsafe { mem::transmute::<[u8; 16], &'a str>(self.0.raw) } 
        }
    }
}

impl<'a> From<&'a str> for RocStr {
    #[cfg(all(target_pointer_width = "64", target_endian = "little"))]
    fn from(string: &'a str) -> RocStr {
        if string.is_empty() {
            RocStr::empty()
        } else {
            let str_len = string.len();

            if str_len <= 15 {
                let mut buffer: [u8; 16] = [0; 16];

                // Copy the raw bytes from the string slice into the buffer.
                unsafe {
                    let buffer_ptr =
                        mem::transmute::<*mut [u8; 16], *mut u8>(&mut buffer);

                    // Write into the buffer's bytes
                    ptr::copy_nonoverlapping(
                        string.as_ptr(),
                        buffer_ptr,
                        str_len
                    );
                }

                // Set the last byte in the buffer to be the length (with the flag).
                buffer[15] = ((string.len() as u8) << 1) | 1;

                RocStr(InnerStr { raw: buffer })
            } else {
                let bytes_ptr = string.as_bytes().clone().as_ptr();
                let long = LongStr {
                    bytes: MaybeUninit::new(bytes_ptr),
                    length: str_len,
                };

                RocStr(InnerStr {long})
            }
        }
    }
}
