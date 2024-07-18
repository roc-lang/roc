use core::{fmt, mem};

#[cfg(unix)]
use core::ffi::CStr;

#[cfg(windows)]
use widestring::U16CStr;

#[repr(transparent)]
pub struct NativePath {
    // It's impossible to construct a NativePath on the stack;
    // you can only ever have a reference to one. (That reference
    // will point to nul-terminated bytes.)
}

impl fmt::Debug for NativePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cstr;

        #[cfg(unix)]
        unsafe {
            cstr = CStr::from_ptr(self as *const Self as *const i8);
        }

        #[cfg(windows)]
        unsafe {
            cstr = U16CStr::from_ptr(self.as_ptr().cast());
        }

        cstr.fmt(f)
    }
}

#[cfg(unix)]
impl NativePath {
    /// If the last byte in the slice is 0, returns Some; otherwise, returns None.
    pub fn from_slice(slice: &[u8]) -> Option<&Self> {
        if slice.ends_with(&[0]) {
            // Safety: We just verified that the slice is nonempty and ends in a 0.
            Some(unsafe { Self::from_slice_unchecked(slice) })
        } else {
            None
        }
    }

    /// Safety: The given slice must be nonempty and must point to nul-terminated bytes.
    pub unsafe fn from_slice_unchecked(slice: &[u8]) -> &Self {
        // Safety: Self is repr(transparent); we're just doing this to get the correct lifetime
        mem::transmute(slice.as_ptr())
    }

    /// Safety: The given slice must be nonempty and must point to nul-terminated bytes.
    pub unsafe fn from_slice_unchecked_mut(slice: &mut [u8]) -> &mut Self {
        // Safety: Self is repr(transparent); we're just doing this to get the correct lifetime
        mem::transmute(slice.as_mut_ptr())
    }

    pub(crate) fn as_ptr(&self) -> *const Self {
        self as *const Self
    }
}

#[cfg(windows)]
impl NativePath {
    /// If the last element in the slice is 0, returns Some; otherwise, returns None.
    pub fn from_slice(slice: &[u16]) -> Option<&NativePath> {
        if slice.ends_with(&[0]) {
            // Safety: We just verified that the slice is nonempty and ends in a 0.
            Some(unsafe { Self::from_slice_unchecked(slice) })
        } else {
            None
        }
    }

    /// Safety: The given slice must be nonempty and must point to nul-terminated widechars.
    pub unsafe fn from_slice_unchecked(slice: &[u16]) -> &NativePath {
        // Safety: Self is repr(transparent); we're just doing this to get the correct lifetime
        mem::transmute(slice.as_ptr())
    }

    /// Safety: The given slice must be nonempty and must point to nul-terminated widechars.
    pub unsafe fn from_slice_unchecked_mut(slice: &mut [u16]) -> &mut NativePath {
        // Safety: Self is repr(transparent); we're just doing this to get the correct lifetime
        mem::transmute(slice.as_ptr())
    }
}

#[cfg(unix)]
impl<'a> From<&'a CStr> for &'a NativePath {
    fn from(c_str: &'a CStr) -> Self {
        // Safety: Self is repr(transparent); we're just doing this to get the correct lifetime
        unsafe { mem::transmute(c_str.as_ptr()) }
    }
}

#[cfg(windows)]
impl<'a> From<&'a U16CStr> for &'a NativePath {
    fn from(u16_c_str: &'a U16CStr) -> Self {
        // Safety: Self is repr(transparent)
        unsafe { mem::transmute(u16_c_str.as_ptr()) }
    }
}
