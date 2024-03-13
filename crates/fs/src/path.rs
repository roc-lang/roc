/// This is an intentionally minimal representation of native OS filesystempaths.
///
/// Specifically, a &Path is either a pointer to nul-terminated UTF-8 bytes (on UNIX) or to
/// nul-terminated UTF-16 (on Windows). Converting from any other string representation
/// to this representation is out of scope for this module; it's up to the caller to do that.
///
/// The reason for this design is to keep the fs crate focused on providing access to the raw OS
/// filesystem APIs and not around how to get things in the format they need. This allows it to be
/// decoupled from things like memory management, since converting from UTF-8 strings to the UTF-16
/// that Windows APIs require involves an intermediate buffer which must be allocated somewhere.

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
