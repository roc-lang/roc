#![crate_type = "lib"]
#![no_std]
use core::ffi::c_void;
use core::fmt;
use core::mem::{ManuallyDrop, MaybeUninit};
use core::ops::Drop;

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
