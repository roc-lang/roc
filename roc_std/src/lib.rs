#![crate_type = "lib"]
// #![no_std]
use core::ffi::c_void;
use core::fmt;
use core::mem::{ManuallyDrop, MaybeUninit};
use core::ops::Drop;
use core::str;
use std::hash::{Hash, Hasher};
use std::io::Write;

mod rc;
mod roc_list;
mod roc_str;
mod storage;

pub use rc::ReferenceCount;
pub use roc_list::RocList;
pub use roc_str::RocStr;

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
}

/// # Safety
/// This is only marked unsafe to typecheck without warnings in the rest of the code here.
#[cfg(not(feature = "platform"))]
pub unsafe extern "C" fn roc_alloc(_size: usize, _alignment: u32) -> *mut c_void {
    unimplemented!("It is not valid to call roc alloc from within the compiler. Please use the \"platform\" feature if this is a platform.")
}
/// # Safety
/// This is only marked unsafe to typecheck without warnings in the rest of the code here.
#[cfg(not(feature = "platform"))]
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
pub unsafe extern "C" fn roc_dealloc(_ptr: *mut c_void, _alignment: u32) {
    unimplemented!("It is not valid to call roc dealloc from within the compiler. Please use the \"platform\" feature if this is a platform.")
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
#[repr(C, align(16))]
pub struct RocDec(i128);

impl RocDec {
    pub const MIN: Self = Self(i128::MIN);
    pub const MAX: Self = Self(i128::MAX);

    const DECIMAL_PLACES: usize = 18;
    const ONE_POINT_ZERO: i128 = 10i128.pow(Self::DECIMAL_PLACES as u32);
    const MAX_DIGITS: usize = 39;
    const MAX_STR_LENGTH: usize = Self::MAX_DIGITS + 2; // + 2 here to account for the sign & decimal dot

    pub fn new(bits: i128) -> Self {
        Self(bits)
    }

    pub fn as_bits(&self) -> (i64, u64) {
        let lower_bits = self.0 as u64;
        let upper_bits = (self.0 >> 64) as i64;
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
                Some(hi) => hi.checked_add(lo).map(Self),
                None => None,
            },
            Err(_) => None,
        }
    }

    pub fn from_str_to_i128_unsafe(val: &str) -> i128 {
        Self::from_str(val).unwrap().0
    }

    fn to_str_helper(&self, bytes: &mut [u8; Self::MAX_STR_LENGTH]) -> usize {
        if self.0 == 0 {
            write!(&mut bytes[..], "{}", "0").unwrap();
            return 1;
        }

        let is_negative = (self.0 < 0) as usize;

        static_assertions::const_assert!(Self::DECIMAL_PLACES + 1 == 19);
        // The :019 in the following write! is computed as Self::DECIMAL_PLACES + 1. If you change
        // Self::DECIMAL_PLACES, this assert should remind you to change that format string as
        // well.
        //
        // By using the :019 format, we're guaranteeing that numbers less than 1, say 0.01234
        // get their leading zeros placed in bytes for us. i.e. bytes = b"0012340000000000000"
        write!(&mut bytes[..], "{:019}", self.0).unwrap();

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

    pub fn to_str(&self) -> RocStr {
        let mut bytes = [0 as u8; Self::MAX_STR_LENGTH];
        let last_idx = self.to_str_helper(&mut bytes);
        unsafe { RocStr::from_slice(&bytes[0..last_idx]) }
    }
}

impl fmt::Display for RocDec {
    fn fmt(&self, fmtr: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut bytes = [0 as u8; Self::MAX_STR_LENGTH];
        let last_idx = self.to_str_helper(&mut bytes);
        let result = unsafe { str::from_utf8_unchecked(&bytes[0..last_idx]) };
        write!(fmtr, "{}", result)
    }
}

#[repr(align(16))]
#[repr(C)]
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
        let i128: i128 = (*self).into();

        i128.fmt(f)
    }
}

impl fmt::Display for I128 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let i128: i128 = (*self).into();

        i128.fmt(f)
    }
}

impl PartialEq for I128 {
    fn eq(&self, other: &Self) -> bool {
        let i128_self: i128 = (*self).into();
        let i128_other: i128 = (*other).into();

        i128_self.eq(&i128_other)
    }
}

impl PartialOrd for I128 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let i128_self: i128 = (*self).into();
        let i128_other: i128 = (*other).into();

        i128_self.partial_cmp(&i128_other)
    }
}

impl Ord for I128 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let i128_self: i128 = (*self).into();
        let i128_other: i128 = (*other).into();

        i128_self.cmp(&i128_other)
    }
}

impl Hash for I128 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let i128: i128 = (*self).into();

        i128.hash(state);
    }
}

#[repr(align(16))]
#[repr(C)]
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
        let u128: u128 = (*self).into();

        u128.fmt(f)
    }
}

impl fmt::Display for U128 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u128: u128 = (*self).into();

        u128.fmt(f)
    }
}

impl PartialEq for U128 {
    fn eq(&self, other: &Self) -> bool {
        let u128_self: u128 = (*self).into();
        let u128_other: u128 = (*other).into();

        u128_self.eq(&u128_other)
    }
}

impl PartialOrd for U128 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let u128_self: u128 = (*self).into();
        let u128_other: u128 = (*other).into();

        u128_self.partial_cmp(&u128_other)
    }
}

impl Ord for U128 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let u128_self: u128 = (*self).into();
        let u128_other: u128 = (*other).into();

        u128_self.cmp(&u128_other)
    }
}

impl Hash for U128 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let u128: u128 = (*self).into();

        u128.hash(state);
    }
}
