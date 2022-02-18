#![crate_type = "lib"]
#![no_std]
use core::convert::From;
use core::ffi::c_void;
use core::fmt::{self, Display, Formatter};
use core::mem::{ManuallyDrop, MaybeUninit};
use core::ops::{Deref, DerefMut, Drop};
use core::{mem, ptr, slice};

mod roc_list;

pub use roc_list::RocList;

// A list of C functions that are being imported
extern "C" {
    pub fn roc_alloc(size: usize, alignment: u32) -> *mut c_void;
    pub fn roc_realloc(
        ptr: *mut c_void,
        new_size: usize,
        old_size: usize,
        alignment: u32,
    ) -> *mut c_void;
    pub fn roc_dealloc(ptr: *mut c_void, alignment: u32);
}

const REFCOUNT_1: isize = isize::MIN;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RocOrder {
    Eq = 0,
    Gt = 1,
    Lt = 2,
}

//#[macro_export]
//macro_rules! roclist {
//    () => (
//        $crate::RocList::default()
//    );
//    ($($x:expr),+ $(,)?) => (
//        $crate::RocList::from_slice(&[$($x),+])
//    );
//}

#[derive(Clone, Copy, Debug)]
pub enum Storage {
    ReadOnly,
    Refcounted(isize),
    Capacity(usize),
}

#[repr(C)]
pub struct RocStr {
    elements: *mut u8,
    length: usize,
}

impl RocStr {
    pub fn len(&self) -> usize {
        if self.is_small_str() {
            let bytes = self.length.to_ne_bytes();
            let last_byte = bytes[mem::size_of::<usize>() - 1];

            (last_byte ^ 0b1000_0000) as usize
        } else {
            self.length
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_small_str(&self) -> bool {
        (self.length as isize) < 0
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

    fn from_slice_with_capacity_str(slice: &[u8], capacity: usize) -> Self {
        assert!(
            slice.len() <= capacity,
            "RocStr::from_slice_with_capacity_str length bigger than capacity {} {}",
            slice.len(),
            capacity
        );
        if capacity < core::mem::size_of::<Self>() {
            let mut rocstr = Self::default();
            let target_ptr = rocstr.get_small_str_ptr_mut();
            let source_ptr = slice.as_ptr() as *const u8;
            for index in 0..slice.len() {
                unsafe {
                    *target_ptr.add(index) = *source_ptr.add(index);
                }
            }
            // Write length and small string bit to last byte of length.
            let mut bytes = rocstr.length.to_ne_bytes();
            bytes[mem::size_of::<usize>() - 1] = capacity as u8 ^ 0b1000_0000;
            rocstr.length = usize::from_ne_bytes(bytes);

            rocstr
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
        Self {
            length: isize::MIN as usize,
            elements: core::ptr::null_mut(),
        }
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

/// Like a Rust `Result`, but following Roc's ABI instead of Rust's.
/// (Using Rust's `Result` instead of this will not work properly with Roc code!)
///
/// This can be converted to/from a Rust `Result` using `.into()`
#[repr(C)]
pub struct RocResult<T, E> {
    payload: RocResultPayload<T, E>,
    tag: RocResultTag,
}

impl<T, E> core::fmt::Debug for RocResult<T, E>
where
    T: core::fmt::Debug,
    E: core::fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.as_result_of_refs() {
            Ok(payload) => write!(f, "RocOk({:?})", payload),
            Err(payload) => write!(f, "RocErr({:?})", payload),
        }
    }
}

impl<T, E> PartialEq for RocResult<T, E>
where
    T: PartialEq,
    E: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.as_result_of_refs() == other.as_result_of_refs()
    }
}

impl<T, E> Clone for RocResult<T, E>
where
    T: Clone,
    E: Clone,
{
    fn clone(&self) -> Self {
        match self.as_result_of_refs() {
            Ok(payload) => RocResult::ok(ManuallyDrop::into_inner(payload.clone())),
            Err(payload) => RocResult::err(ManuallyDrop::into_inner(payload.clone())),
        }
    }
}

impl<T, E> RocResult<T, E> {
    pub fn ok(payload: T) -> Self {
        Self {
            tag: RocResultTag::RocOk,
            payload: RocResultPayload {
                ok: ManuallyDrop::new(payload),
            },
        }
    }

    pub fn err(payload: E) -> Self {
        Self {
            tag: RocResultTag::RocErr,
            payload: RocResultPayload {
                err: ManuallyDrop::new(payload),
            },
        }
    }

    pub fn is_ok(&self) -> bool {
        matches!(self.tag, RocResultTag::RocOk)
    }

    pub fn is_err(&self) -> bool {
        matches!(self.tag, RocResultTag::RocErr)
    }

    fn into_payload(mut self) -> RocResultPayload<T, E> {
        let mut value = MaybeUninit::uninit();
        let ref_mut_value = unsafe { &mut *value.as_mut_ptr() };

        // move the value into our MaybeUninit memory
        core::mem::swap(&mut self.payload, ref_mut_value);

        // don't run the destructor on self; the `payload` has been moved out
        // and replaced by uninitialized memory
        core::mem::forget(self);

        unsafe { value.assume_init() }
    }

    fn as_result_of_refs(&self) -> Result<&ManuallyDrop<T>, &ManuallyDrop<E>> {
        use RocResultTag::*;

        unsafe {
            match self.tag {
                RocOk => Ok(&self.payload.ok),
                RocErr => Err(&self.payload.err),
            }
        }
    }
}

impl<T, E> From<RocResult<T, E>> for Result<T, E> {
    fn from(roc_result: RocResult<T, E>) -> Self {
        use RocResultTag::*;

        let tag = roc_result.tag;
        let payload = roc_result.into_payload();

        unsafe {
            match tag {
                RocOk => Ok(ManuallyDrop::into_inner(payload.ok)),
                RocErr => Err(ManuallyDrop::into_inner(payload.err)),
            }
        }
    }
}

impl<T, E> From<Result<T, E>> for RocResult<T, E> {
    fn from(result: Result<T, E>) -> Self {
        match result {
            Ok(payload) => RocResult::ok(payload),
            Err(payload) => RocResult::err(payload),
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum RocResultTag {
    RocErr = 0,
    RocOk = 1,
}

#[repr(C)]
union RocResultPayload<T, E> {
    ok: ManuallyDrop<T>,
    err: ManuallyDrop<E>,
}

impl<T, E> Drop for RocResult<T, E> {
    fn drop(&mut self) {
        use RocResultTag::*;

        match self.tag {
            RocOk => unsafe { ManuallyDrop::drop(&mut self.payload.ok) },
            RocErr => unsafe { ManuallyDrop::drop(&mut self.payload.err) },
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct RocDec(pub i128);

impl RocDec {
    pub const MIN: Self = Self(i128::MIN);
    pub const MAX: Self = Self(i128::MAX);

    pub const DECIMAL_PLACES: u32 = 18;

    pub const ONE_POINT_ZERO: i128 = 10i128.pow(Self::DECIMAL_PLACES);

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(value: &str) -> Option<Self> {
        // Split the string into the parts before and after the "."
        let mut parts = value.split('.');

        let before_point = match parts.next() {
            Some(answer) => answer,
            None => {
                return None;
            }
        };

        let opt_after_point = match parts.next() {
            Some(answer) if answer.len() <= Self::DECIMAL_PLACES as usize => Some(answer),
            _ => None,
        };

        // There should have only been one "." in the string!
        if parts.next().is_some() {
            return None;
        }

        // Calculate the low digits - the ones after the decimal point.
        let lo = match opt_after_point {
            Some(after_point) => {
                match after_point.parse::<i128>() {
                    Ok(answer) => {
                        // Translate e.g. the 1 from 0.1 into 10000000000000000000
                        // by "restoring" the elided trailing zeroes to the number!
                        let trailing_zeroes = Self::DECIMAL_PLACES as usize - after_point.len();
                        let lo = answer * 10i128.pow(trailing_zeroes as u32);

                        if !before_point.starts_with('-') {
                            lo
                        } else {
                            -lo
                        }
                    }
                    Err(_) => {
                        return None;
                    }
                }
            }
            None => 0,
        };

        // Calculate the high digits - the ones before the decimal point.
        match before_point.parse::<i128>() {
            Ok(answer) => match answer.checked_mul(10i128.pow(Self::DECIMAL_PLACES)) {
                Some(hi) => hi.checked_add(lo).map(Self),
                None => None,
            },
            Err(_) => None,
        }
    }

    pub fn from_str_to_i128_unsafe(val: &str) -> i128 {
        Self::from_str(val).unwrap().0
    }
}
