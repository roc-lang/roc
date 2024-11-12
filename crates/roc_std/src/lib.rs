//! Provides Rust representations of Roc data structures.
// #![cfg_attr(not(feature = "std"), no_std)]
#![crate_type = "lib"]

use arrayvec::ArrayString;
use core::cmp::Ordering;
use core::ffi::c_void;
use core::fmt::{self, Debug};
use core::hash::{Hash, Hasher};
use core::mem::{ManuallyDrop, MaybeUninit};
use core::ops::Drop;
use core::str;
use std::convert::Infallible;

mod roc_box;
mod roc_list;
mod roc_str;
mod storage;

pub use roc_box::RocBox;
pub use roc_list::{RocList, SendSafeRocList};
pub use roc_str::{InteriorNulError, RocStr, SendSafeRocStr};
pub use storage::Storage;

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
    pub fn roc_panic(c_ptr: *mut c_void, tag_id: u32);
    pub fn roc_dbg(loc: *mut c_void, msg: *mut c_void, src: *mut c_void);
    pub fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void;
}

pub fn roc_alloc_refcounted<T>() -> *mut T {
    let size = core::mem::size_of::<T>();
    let align = core::mem::align_of::<T>();

    roc_alloc_refcounted_help(size, align) as *mut T
}

fn roc_alloc_refcounted_help(mut size: usize, mut align: usize) -> *mut u8 {
    let prefix = if align > 8 { 16 } else { 8 };
    size += prefix;
    align = align.max(core::mem::size_of::<crate::Storage>());

    unsafe {
        let allocation_ptr = roc_alloc(size, align as _) as *mut u8;
        let data_ptr = allocation_ptr.add(prefix);
        let storage_ptr = (data_ptr as *mut crate::Storage).sub(1);

        *storage_ptr = Storage::new_reference_counted();

        data_ptr
    }
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

impl<T, E> Eq for RocResult<T, E>
where
    T: Eq,
    E: Eq,
{
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

impl<T, E> Ord for RocResult<T, E>
where
    T: Ord,
    E: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_result_of_refs().cmp(&other.as_result_of_refs())
    }
}

impl<T, E> PartialOrd for RocResult<T, E>
where
    T: PartialOrd,
    E: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_result_of_refs()
            .partial_cmp(&other.as_result_of_refs())
    }
}

impl<T, E> Hash for RocResult<T, E>
where
    T: Hash,
    E: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_result_of_refs().hash(state)
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

    fn into_payload(self) -> RocResultPayload<T, E> {
        let mut value = MaybeUninit::uninit();

        // copy the value into our MaybeUninit memory
        unsafe {
            core::ptr::copy_nonoverlapping(&self.payload, value.as_mut_ptr(), 1);
        }

        // don't run the destructor on self; the `payload` briefly has two owners
        // but only `value` is allowed to drop it (after initialization)
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

impl<T, E> RocRefcounted for RocResult<T, E>
where
    T: RocRefcounted,
    E: RocRefcounted,
{
    fn inc(&mut self) {
        unsafe {
            match self.tag {
                RocResultTag::RocOk => (*self.payload.ok).inc(),
                RocResultTag::RocErr => (*self.payload.err).inc(),
            }
        }
    }
    fn dec(&mut self) {
        unsafe {
            match self.tag {
                RocResultTag::RocOk => (*self.payload.ok).dec(),
                RocResultTag::RocErr => (*self.payload.err).dec(),
            }
        }
    }
    fn is_refcounted() -> bool {
        T::is_refcounted() || E::is_refcounted()
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

#[derive(Copy, Clone, PartialEq, Eq, Hash, Default)]
#[repr(C, align(16))]
pub struct RocDec([u8; 16]);

impl Debug for RocDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("RocDec")
            .field(&self.0)
            .field(&self.to_str())
            .finish()
    }
}

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

        let opt_after_point = parts
            .next()
            .map(|answer| &answer[..Ord::min(answer.len(), Self::DECIMAL_PLACES)]);

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
        let (is_pos, digits) = match before_point.chars().next() {
            Some('+') => (true, &before_point[1..]),
            Some('-') => (false, &before_point[1..]),
            _ => (true, before_point),
        };

        let mut hi: i128 = 0;
        macro_rules! adjust_hi {
            ($op:ident) => {{
                for digit in digits.chars() {
                    if digit == '_' {
                        continue;
                    }

                    let digit = digit.to_digit(10)?;
                    hi = hi.checked_mul(10)?;
                    hi = hi.$op(digit as _)?;
                }
            }};
        }

        if is_pos {
            adjust_hi!(checked_add);
        } else {
            adjust_hi!(checked_sub);
        }

        match hi.checked_mul(Self::ONE_POINT_ZERO) {
            Some(hi) => hi.checked_add(lo).map(|num| Self(num.to_ne_bytes())),
            None => None,
        }
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

    fn to_str_helper(self, string: &mut ArrayString<{ Self::MAX_STR_LENGTH }>) -> &str {
        use core::fmt::Write;

        if self.as_i128() == 0 {
            return "0";
        }

        // The :019 in the following write! is computed as Self::DECIMAL_PLACES + 1. If you change
        // Self::DECIMAL_PLACES, this assert should remind you to change that format string as well.
        static_assertions::const_assert!(RocDec::DECIMAL_PLACES + 1 == 19);

        // By using the :019 format, we're guaranteeing that numbers less than 1, say 0.01234
        // get their leading zeros placed in bytes for us. i.e. `string = b"0012340000000000000"`
        write!(string, "{:019}", self.as_i128()).unwrap();

        let decimal_location = string.len() - Self::DECIMAL_PLACES;
        // skip trailing zeros
        let last_nonzero_byte = string.trim_end_matches('0').len();

        if last_nonzero_byte <= decimal_location {
            // This means that we've removed trailing zeros and are left with an integer. Our
            // convention is to print these without a decimal point or trailing zeros, so we're done.
            string.truncate(decimal_location);
            return string.as_str();
        }

        // otherwise, we're dealing with a fraction, and need to insert the decimal dot

        // truncate all extra zeros off
        string.truncate(last_nonzero_byte);

        // push a dummy character so we have space for the decimal dot
        string.push('$');

        // Safety: at any time, the string only contains ascii characters, so it is always valid utf8
        let bytes = unsafe { string.as_bytes_mut() };

        // shift the fractional part by one
        bytes.copy_within(decimal_location..last_nonzero_byte, decimal_location + 1);

        // and put in the decimal dot in the right place
        bytes[decimal_location] = b'.';

        string.as_str()
    }

    pub fn to_str(&self) -> RocStr {
        RocStr::from(self.to_str_helper(&mut ArrayString::new()))
    }
}

impl From<i32> for RocDec {
    fn from(value: i32) -> Self {
        RocDec::from_ne_bytes((RocDec::ONE_POINT_ZERO * value as i128).to_ne_bytes())
    }
}

impl fmt::Display for RocDec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str_helper(&mut ArrayString::new()))
    }
}

impl PartialOrd for RocDec {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RocDec {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_i128().cmp(&other.as_i128())
    }
}

#[repr(C, align(16))]
#[derive(Clone, Copy, Eq, Default, PartialEq)]
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

impl PartialOrd for I128 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
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
#[derive(Clone, Copy, Eq, Default, PartialEq)]
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

impl PartialOrd for U128 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
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

/// All Roc types that are refcounted must implement this trait.
///
/// For aggregate types, this must recurse down the structure.
pub trait RocRefcounted {
    /// Increments the refcount.
    fn inc(&mut self);

    /// Decrements the refcount potentially freeing the underlying allocation.
    fn dec(&mut self);

    /// Returns true if the type is actually refcounted by roc.
    fn is_refcounted() -> bool;
}

#[macro_export]
macro_rules! roc_refcounted_noop_impl {
    ( $( $T:tt),+ ) => {
        $(
            impl RocRefcounted for $T {
                fn inc(&mut self) {}
                fn dec(&mut self) {}
                fn is_refcounted() -> bool {
                    false
                }
            }
        )+
    };
}
roc_refcounted_noop_impl!(bool);
roc_refcounted_noop_impl!(u8, u16, u32, u64, u128, U128);
roc_refcounted_noop_impl!(i8, i16, i32, i64, i128, I128);
roc_refcounted_noop_impl!(f32, f64);
roc_refcounted_noop_impl!(RocDec);
roc_refcounted_noop_impl!(Infallible, ());

macro_rules! roc_refcounted_arr_impl {
    ( $n:tt ) => {
        impl<T> RocRefcounted for [T; $n]
        where
            T: RocRefcounted,
        {
            fn inc(&mut self) {
                self.iter_mut().for_each(|x| x.inc());
            }
            fn dec(&mut self) {
                self.iter_mut().for_each(|x| x.dec());
            }
            fn is_refcounted() -> bool {
                T::is_refcounted()
            }
        }
    };
}

roc_refcounted_arr_impl!(0);
roc_refcounted_arr_impl!(1);
roc_refcounted_arr_impl!(2);
roc_refcounted_arr_impl!(3);
roc_refcounted_arr_impl!(4);
roc_refcounted_arr_impl!(5);
roc_refcounted_arr_impl!(6);
roc_refcounted_arr_impl!(7);
roc_refcounted_arr_impl!(8);

macro_rules! roc_refcounted_tuple_impl {
    ( $( $idx:tt $T:ident),* ) => {
        impl<$($T, )+> RocRefcounted for ($($T, )*)
        where
            $($T : RocRefcounted, )*
        {
            fn inc(&mut self) {
                $(
                self.$idx.inc();
                )*
            }
            fn dec(&mut self) {
                $(
                self.$idx.dec();
                )*
            }
            fn is_refcounted() -> bool {
                $($T::is_refcounted() || )* false
            }
        }
    };
}

roc_refcounted_tuple_impl!(0 A, 1 B);
roc_refcounted_tuple_impl!(0 A, 1 B, 2 C);
roc_refcounted_tuple_impl!(0 A, 1 B, 3 C, 3 D);
roc_refcounted_tuple_impl!(0 A, 1 B, 3 C, 3 D, 4 E);
roc_refcounted_tuple_impl!(0 A, 1 B, 3 C, 3 D, 4 E, 5 F);
roc_refcounted_tuple_impl!(0 A, 1 B, 3 C, 3 D, 4 E, 5 F, 6 G);
roc_refcounted_tuple_impl!(0 A, 1 B, 3 C, 3 D, 4 E, 5 F, 6 G, 7 H);
