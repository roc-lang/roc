#[cfg(unix)]
use core::ffi::CStr;

#[cfg(windows)]
use widestring::U16CStr;

#[cfg(unix)]
#[repr(transparent)]
pub struct NativePath {
    pub(crate) c_str: CStr,
}

#[cfg(windows)]
#[repr(transparent)]
pub struct NativePath {
    pub(crate) u16_c_str: U16CStr,
}

#[cfg(unix)]
impl NativePath {
    pub fn new(c_str: &CStr) -> &Self {
        // Safety: Self is repr(transparent)
        unsafe { &*(c_str as *const CStr as *const Self) }
    }
}

#[cfg(windows)]
impl NativePath {
    pub fn new(u16_c_str: &U16CStr) -> &Self {
        // Safety: Self is repr(transparent)
        unsafe { &*(u16_c_str as *const U16CStr as *const Self) }
    }
}
