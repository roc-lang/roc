/// Like core::str, except:
/// - It's stored in the native OS string encoding (so, UTF-8 on UNIX, UTF-16 on Windows and WASM)
/// - It's guaranteed not to have NUL bytes in it, so it can be safely NUL-terminated for OS API calls
/// - Length is not usize; instead, it can be configured to be stored as u8, u16, or u32
mod native_cstr;

pub use native_cstr::*;

#[cfg(unix)]
use core::num::NonZeroU8;

#[cfg(any(windows, wasm32))]
use core::num::NonZeroU16;

#[cfg(any(windows, wasm32))]
use widestring::U16Str;

/// A string literal which expands to an S32
#[macro_export]
macro_rules! s {
    ($s:literal) => {{
        // Safety: from_bytes_unchecked verifies its safety properties in debug builds,
        // so if this literal is invalid (e.g. contains NUL bytes), we'll get a panic right away.
        unsafe {
            #[cfg(unix)]
            {
                crate::NativeStr::from_slice_unchecked($s.as_bytes())
            }

            #[cfg(any(windows, wasm32))]
            {
                $crate::NativeStr::from_slice_unchecked(widestring::u16str!($s).as_slice())
            }
        }
    }};
}

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct NativeStr {
    #[cfg(unix)]
    inner: [NonZeroU8],

    #[cfg(any(windows, wasm32))]
    inner: [NonZeroU16],
}

#[cfg(unix)]
impl<'a> From<&'a NativeStr> for &'a [NonZeroU8] {
    fn from(native: &'a NativeStr) -> Self {
        &native.inner
    }
}

#[cfg(unix)]
impl<'a> From<&'a NativeStr> for &'a str {
    fn from(native: &'a NativeStr) -> Self {
        // Safety: we know NativeStr is valid UTF-8
        unsafe {
            core::str::from_utf8_unchecked(&*(&native.inner as *const [NonZeroU8] as *const [u8]))
        }
    }
}

#[cfg(any(windows, wasm32))]
impl<'a> From<&'a NativeStr> for &'a [NonZeroU16] {
    fn from(native: &'a NativeStr) -> Self {
        &native.inner
    }
}

#[cfg(any(windows, wasm32))]
impl<'a> From<&'a NativeStr> for &'a U16str {
    fn from(native: &'a NativeStr) -> Self {
        // Safety: we know NativeStr is valid UTF-16
        unsafe { U16Str::from_slice(&*(&native.inner as *const [NonZeroU16] as *const [u16])) }
    }
}

impl NativeStr {
    #[inline(always)]
    #[must_use]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }
}

#[cfg(unix)]
impl NativeStr {
    pub fn as_slice(&self) -> &[NonZeroU8] {
        &self.inner
    }

    /// Safety: the given &str must not contain any 0 bytes.
    pub const unsafe fn from_str_unchecked(str_ref: &str) -> &Self {
        Self::from_slice_unchecked(str_ref.as_bytes())
    }

    /// Safety: the given slice must not contain any elements that are 0.
    pub const unsafe fn from_slice_unchecked(slice: &[u8]) -> &Self {
        &*(slice as *const [u8] as *const Self)
    }
}

#[cfg(any(windows, wasm32))]
impl NativeStr {
    pub fn as_slice(&self) -> &[NonZeroU8] {
        &self.inner
    }

    /// Safety: the given &U16Str must not contain any 0 characters.
    pub unsafe fn from_str_unchecked(str_ref: &(impl AsRef<U16Str> + ?Sized)) -> &Self {
        Self::from_slice_unchecked(str_ref.as_ref().as_slice())
    }

    /// Safety: the given slice must not contain any elements that are 0.
    pub unsafe fn from_slice_unchecked(slice: &(impl AsRef<[u16]> + ?Sized)) -> &Self {
        &*(slice.as_ref() as *const [u16] as *const Self)
    }
}

#[test]
fn test_native_str() {
    const TEST_LITERAL: &NativeStr = s!("this is a test");

    #[cfg(unix)]
    {
        assert_eq!(Into::<&str>::into(TEST_LITERAL), "this is a test");
    }

    #[cfg(any(windows, wasm32))]
    {
        assert_eq!(
            Into::<&U16Str>::into(TEST_LITERAL),
            widestring::u16str!("this is a test")
        );
    }
}
