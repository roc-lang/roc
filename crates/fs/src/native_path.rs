use core::{fmt, mem};

#[cfg(unix)]
use core::ffi::CStr;

#[cfg(windows)]
use widestring::U16CStr;

#[cfg(unix)]
#[repr(transparent)]
pub struct NativePath {
    pub(crate) inner: CStr,
}

#[cfg(windows)]
#[repr(transparent)]
pub struct NativePath {
    pub(crate) inner: U16CStr,
}

impl fmt::Debug for NativePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

#[cfg(unix)]
impl<'a> From<&'a CStr> for &'a NativePath {
    fn from(c_str: &'a CStr) -> Self {
        // Safety: Self is repr(transparent)
        unsafe { mem::transmute(c_str) }
    }
}

#[cfg(windows)]
impl<'a> From<&'a U16CStr> for &'a NativePath {
    fn from(u16_c_str: &'a U16CStr) -> Self {
        // Safety: Self is repr(transparent)
        unsafe { mem::transmute(u16_c_str) }
    }
}
