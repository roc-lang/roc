#[cfg(unix)]
pub type Path = core::ffi::CStr;

#[cfg(windows)]
pub type Path = widestring::ucstr::U16CStr;
