#![crate_type = "lib"]
#![cfg_attr(feature = "no_std", no_std)]

use core::cmp::Ordering;
use core::ffi::c_void;
use core::fmt::{self, Debug};
use core::hash::{Hash, Hasher};
use core::mem::{ManuallyDrop, MaybeUninit};
use core::ops::Drop;
use core::str;

mod roc_list;
mod roc_str;
mod storage;

pub use roc_list::RocList;
pub use roc_str::RocStr;
pub use storage::Storage;

// A list of C functions that are being imported
#[cfg(feature = "platform")]
extern "C" {
    pub fn roc_alloc(size: usize, alignment: u32) -> *mut c_void;
    pub fn roc_realloc(
        ptr: *mut c_void,
        new_size: usize,
        old_size: usize,
        alignment: u32,
    ) -> *mut c_void;
    pub fn roc_dealloc(ptr: *mut c_void, alignment: u32);
    pub fn roc_panic(c_ptr: *mut c_void, tag_id: u32);
    pub fn roc_memcpy(dst: *mut c_void, src: *mut c_void, n: usize) -> *mut c_void;
    pub fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void;
}

/// # Safety
/// This is only marked unsafe to typecheck without warnings in the rest of the code here.
#[cfg(not(feature = "platform"))]
#[no_mangle]
pub unsafe extern "C" fn roc_alloc(_size: usize, _alignment: u32) -> *mut c_void {
    unimplemented!("It is not valid to call roc alloc from within the compiler. Please use the \"platform\" feature if this is a platform.")
}

/// # Safety
/// This is only marked unsafe to typecheck without warnings in the rest of the code here.
#[cfg(not(feature = "platform"))]
#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    _ptr: *mut c_void,
    _new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    unimplemented!("It is not valid to call roc realloc from within the compiler. Please use the \"platform\" feature if this is a platform.")
}

/// # Safety
/// This is only marked unsafe to typecheck without warnings in the rest of the code here.
#[cfg(not(feature = "platform"))]
#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(_ptr: *mut c_void, _alignment: u32) {
    unimplemented!("It is not valid to call roc dealloc from within the compiler. Please use the \"platform\" feature if this is a platform.")
}

#[cfg(not(feature = "platform"))]
#[no_mangle]
pub unsafe extern "C" fn roc_panic(c_ptr: *mut c_void, tag_id: u32) {
    unimplemented!("It is not valid to call roc panic from within the compiler. Please use the \"platform\" feature if this is a platform.")
}

/// # Safety
/// This is only marked unsafe to typecheck without warnings in the rest of the code here.
#[cfg(not(feature = "platform"))]
#[no_mangle]
pub fn roc_memcpy(_dst: *mut c_void, _src: *mut c_void, _n: usize) -> *mut c_void {
    unimplemented!("It is not valid to call roc memcpy from within the compiler. Please use the \"platform\" feature if this is a platform.")
}

/// # Safety
/// This is only marked unsafe to typecheck without warnings in the rest of the code here.
#[cfg(not(feature = "platform"))]
#[no_mangle]
pub fn roc_memset(_dst: *mut c_void, _c: i32, _n: usize) -> *mut c_void {
    unimplemented!("It is not valid to call roc memset from within the compiler. Please use the \"platform\" feature if this is a platform.")
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RocOrder {
    Eq = 0,
    Gt = 1,
    Lt = 2,
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

impl<T, E> Debug for RocResult<T, E>
where
    T: Debug,
    E: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.as_result_of_refs() {
            Ok(payload) => {
                f.write_str("RocOk(")?;
                payload.fmt(f)?;
                f.write_str(")")
            }
            Err(payload) => {
                f.write_str("RocErr(")?;
                payload.fmt(f)?;
                f.write_str(")")
            }
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
#[repr(C)]
pub struct RocDec([u8; 16]);

impl RocDec {
    pub const MIN: Self = Self(i128::MIN.to_ne_bytes());
    pub const MAX: Self = Self(i128::MAX.to_ne_bytes());

    const DECIMAL_PLACES: usize = 18;
    const ONE_POINT_ZERO: i128 = 10i128.pow(Self::DECIMAL_PLACES as u32);
    const MAX_DIGITS: usize = 39;
    const MAX_STR_LENGTH: usize = Self::MAX_DIGITS + 2; // + 2 here to account for the sign & decimal dot

    pub fn new(num: i128) -> Self {
        Self(num.to_ne_bytes())
    }

    pub fn as_bits(&self) -> (i64, u64) {
        let lower_bits = self.as_i128() as u64;
        let upper_bits = (self.as_i128() >> 64) as i64;
        (upper_bits, lower_bits)
    }

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
            Some(answer) if answer.len() <= Self::DECIMAL_PLACES => Some(answer),
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
                        let trailing_zeroes = Self::DECIMAL_PLACES - after_point.len();
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
            Ok(answer) => match answer.checked_mul(Self::ONE_POINT_ZERO) {
                Some(hi) => hi.checked_add(lo).map(|num| Self(num.to_ne_bytes())),
                None => None,
            },
            Err(_) => None,
        }
    }

    pub fn from_str_to_i128_unsafe(val: &str) -> i128 {
        Self::from_str(val).unwrap().as_i128()
    }

    /// This is private because RocDec being an i128 is an implementation detail
    #[inline(always)]
    fn as_i128(&self) -> i128 {
        i128::from_ne_bytes(self.0)
    }

    pub fn from_ne_bytes(bytes: [u8; 16]) -> Self {
        Self(bytes)
    }

    pub fn to_ne_bytes(&self) -> [u8; 16] {
        self.0
    }

    #[cfg(not(feature = "no_std"))]
    fn to_str_helper(&self, bytes: &mut [u8; Self::MAX_STR_LENGTH]) -> usize {
        // TODO there is probably some way to implement this logic without std::io::Write,
        // which in turn would make this method work with no_std.
        use std::io::Write;

        if self.as_i128() == 0 {
            write!(&mut bytes[..], "{}", "0").unwrap();

            return 1;
        }

        let is_negative = (self.as_i128() < 0) as usize;

        // The :019 in the following write! is computed as Self::DECIMAL_PLACES + 1. If you change
        // Self::DECIMAL_PLACES, this assert should remind you to change that format string as
        // well.
        static_assertions::const_assert!(Self::DECIMAL_PLACES + 1 == 19);

        // By using the :019 format, we're guaranteeing that numbers less than 1, say 0.01234
        // get their leading zeros placed in bytes for us. i.e. bytes = b"0012340000000000000"
        write!(&mut bytes[..], "{:019}", self.as_i128()).unwrap();

        // If self represents 1234.5678, then bytes is b"1234567800000000000000".
        let mut i = Self::MAX_STR_LENGTH - 1;
        // Find the last place where we have actual data.
        while bytes[i] == 0 {
            i = i - 1;
        }
        // At this point i is 21 because bytes[21] is the final '0' in b"1234567800000000000000".

        let decimal_location = i - Self::DECIMAL_PLACES + 1 + is_negative;
        // decimal_location = 4

        while bytes[i] == ('0' as u8) && i >= decimal_location {
            bytes[i] = 0;
            i = i - 1;
        }
        // Now i = 7, because bytes[7] = '8', and bytes = b"12345678"

        if i < decimal_location {
            // This means that we've removed trailing zeros and are left with an integer. Our
            // convention is to print these without a decimal point or trailing zeros, so we're done.
            return i + 1;
        }

        let ret = i + 1;
        while i >= decimal_location {
            bytes[i + 1] = bytes[i];
            i = i - 1;
        }
        bytes[i + 1] = bytes[i];
        // Now i = 4, and bytes = b"123455678"

        bytes[decimal_location] = '.' as u8;
        // Finally bytes = b"1234.5678"

        ret + 1
    }

    #[cfg(not(feature = "no_std"))] // to_str_helper currently uses std, but might not need to.
    pub fn to_str(&self) -> RocStr {
        let mut bytes = [0 as u8; Self::MAX_STR_LENGTH];
        let last_idx = self.to_str_helper(&mut bytes);
        unsafe { RocStr::from_slice_unchecked(&bytes[0..last_idx]) }
    }
}

#[cfg(not(feature = "no_std"))] // to_str_helper currently uses std, but might not need to.
impl fmt::Display for RocDec {
    fn fmt(&self, fmtr: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut bytes = [0 as u8; Self::MAX_STR_LENGTH];
        let last_idx = self.to_str_helper(&mut bytes);
        let result = unsafe { str::from_utf8_unchecked(&bytes[0..last_idx]) };
        write!(fmtr, "{}", result)
    }
}

#[repr(C, align(16))]
#[derive(Clone, Copy, Eq, Default)]
pub struct I128([u8; 16]);

impl From<i128> for I128 {
    fn from(other: i128) -> Self {
        Self(other.to_ne_bytes())
    }
}

impl From<I128> for i128 {
    fn from(other: I128) -> Self {
        unsafe { core::mem::transmute::<I128, i128>(other) }
    }
}

impl fmt::Debug for I128 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        i128::from(*self).fmt(f)
    }
}

impl fmt::Display for I128 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&i128::from(*self), f)
    }
}

impl PartialEq for I128 {
    fn eq(&self, other: &Self) -> bool {
        i128::from(*self).eq(&i128::from(*other))
    }
}

impl PartialOrd for I128 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        i128::from(*self).partial_cmp(&i128::from(*other))
    }
}

impl Ord for I128 {
    fn cmp(&self, other: &Self) -> Ordering {
        i128::from(*self).cmp(&i128::from(*other))
    }
}

impl Hash for I128 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        i128::from(*self).hash(state);
    }
}

#[repr(C, align(16))]
#[derive(Clone, Copy, Eq, Default)]
pub struct U128([u8; 16]);

impl From<u128> for U128 {
    fn from(other: u128) -> Self {
        Self(other.to_ne_bytes())
    }
}

impl From<U128> for u128 {
    fn from(other: U128) -> Self {
        unsafe { core::mem::transmute::<U128, u128>(other) }
    }
}

impl fmt::Debug for U128 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        u128::from(*self).fmt(f)
    }
}

impl fmt::Display for U128 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&u128::from(*self), f)
    }
}

impl PartialEq for U128 {
    fn eq(&self, other: &Self) -> bool {
        u128::from(*self).eq(&u128::from(*other))
    }
}

impl PartialOrd for U128 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        u128::from(*self).partial_cmp(&u128::from(*other))
    }
}

impl Ord for U128 {
    fn cmp(&self, other: &Self) -> Ordering {
        u128::from(*self).cmp(&u128::from(*other))
    }
}

impl Hash for U128 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        u128::from(*self).hash(state);
    }
}
