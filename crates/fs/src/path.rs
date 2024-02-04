#[repr(transparent)]
#[cfg(unix)]
pub struct Path(i8);

#[repr(transparent)]
#[cfg(windows)]
pub struct Path(u16);

impl Path {
    #[cfg(unix)]
    pub fn from_cstr(c_str: &core::ffi::CStr) -> &Self {
        unsafe { &*(c_str.as_ptr() as *const Self) }
    }

    #[cfg(unix)]
    pub(crate) fn as_nul_terminated_utf8(&self) -> *const i8 {
        self as *const Self as *const i8
    }

    /// Safety: the given reference must be to the first element of a nul-terminated UTF-16 string
    #[cfg(windows)]
    pub unsafe fn from_nul_terminated_utf16(c_str: &u16) -> &Self {
        unsafe { &*(c_str as *const u16 as *const Self) }
    }

    #[cfg(windows)]
    pub(crate) fn as_nul_terminated_utf16(&self) -> *const i8 {
        self as *const Self as *const i16
    }
}
