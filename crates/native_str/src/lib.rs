/// A string literal which expands to a (nul-terminated) CStr on UNIX,
/// a (nul-terminated) U16CStr on Windows, and a normal Rust str on WebAssembly.
#[macro_export]
macro_rules! native_str {
    ($s:literal) => {{
        #[cfg(any(unix, wasm32))]
        {
            $s
        }

        #[cfg(windows)]
        {
            widestring::u16str!($s)
        }
    }};
}

/// A string literal which expands to a (nul-terminated) CStr on UNIX,
/// a (nul-terminated) U16CStr on Windows, and a normal Rust str on WebAssembly.
#[macro_export]
macro_rules! native_cstr {
    ($s:literal) => {{
        #[cfg(unix)]
        {
            unsafe { core::ffi::CStr::from_bytes_with_nul_unchecked(concat!($s, "\0").as_bytes()) }
        }

        #[cfg(windows)]
        {
            widestring::u16cstr!($s)
        }

        #[cfg(wasm32)]
        {
            $s
        }
    }};
}

#[cfg(any(unix, wasm32))]
pub type NativeStr = str;

#[cfg(windows)]
pub type NativeStr = widestring::U16Str;

#[cfg(unix)]
pub type NativeCStr = core::ffi::CStr;

#[cfg(windows)]
pub type NativeCStr = widestring::U16CStr;

#[cfg(wasm32)]
pub type NativeCStr = str;

#[test]
fn test_native_cstr() {
    const TEST_LITERAL: &NativeCStr = native_cstr!("this is a test");

    #[cfg(unix)]
    {
        let slice = TEST_LITERAL.to_bytes_with_nul();

        assert_eq!(slice.len(), 15); // Should be the length of the string plus a nul terminator
        assert_eq!(slice[14], 0); // Should be nul-terminated
    }

    #[cfg(windows)]
    {
        let slice = TEST_LITERAL.to_slice_with_nul();

        assert_eq!(slice.len(), 15); // Should be the length of the string plus a nul terminator
        assert_eq!(slice[14], 0); // Should be nul-terminated
    }

    #[cfg(wasm32)]
    {
        assert_eq!(TEST_LITERAL, "this is a test");
    }
}

#[test]
fn test_native_str() {
    const TEST_LITERAL: &NativeStr = native_str!("this is a test");

    #[cfg(any(unix, wasm32))]
    {
        assert_eq!(TEST_LITERAL, "this is a test");
    }

    #[cfg(windows)]
    {
        let slice = TEST_LITERAL.as_slice();

        assert_eq!(slice.len(), 14); // Should be the length of the string
        assert_eq!(slice[13], 0); // Should not be nul-terminated
    }
}
