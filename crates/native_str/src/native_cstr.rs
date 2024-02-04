/// Like core::ffi::CStr, except:
/// - It's stored in the native OS string encoding (so, UTF-8 on UNIX, UTF-16 on Windows and WASM)
/// - On wasm, it's more like core::str than CStr because it actually stores the length

/// A string literal which expands to a nul-terminated native string on UNIX,
/// and a normal Rust str on WebAssembly.
#[macro_export]
macro_rules! s0 {
    ($s:literal) => {{
        #[cfg(debug_assertions)]
        {
            // NativeStr must not contain NUL bytes, because they are often converted into NUL-terminated
            // strings to send to the OS.
            if $s.contains('\0') {
                panic!("Tried to construct a NativeCStr from a string literal which contained a NUL byte!");
            }
        }

        // Safety: we just verified there are no NUL
        unsafe {
            #[cfg(unix)]
            {
                $crate::NativeCStr::new_unchecked(concat!($s, "\0"))
            }

            #[cfg(windows)]
            {
                $crate::NativeCStr::new_unchecked(widestring::u16cstr!($s))
            }

            #[cfg(wasm32)]
            {
                $crate::NativeCStr::new_unchecked($s)
            }
        }
    }};
}

pub struct NativeCStr {
    #[cfg(any(unix, wasm32))]
    base: *mut NonZeroU8,

    #[cfg(any(windows))]
    base: *mut NonZeroU16,
}

#[cfg(any(unix, wasm32))]
impl NativeCStr {
    /// Safety: the given slice must not contain any NUL bytes ('\0'),
    /// and its length may not exceed the maximum length this NativeStr
    /// supports. (Debug builds will verify both of these.)
    pub unsafe fn new_unchecked<'a>(slice: impl Into<&'a str>) -> &'a Self {
        let slice = slice.into();
        let len: Len = From::<usize>::from(slice.len());

        #[cfg(debug_assertions)]
        {
            // NativeStr must not contain NUL bytes, because they are often converted into NUL-terminated
            // strings to send to the OS.
            if slice.contains('\0') {
                panic!("Tried to construct a NativeStr from a &str which contained a NUL byte!");
            }

            // If casting back to usize results in a smaller length, then this function's safety
            // requirements were violated!
            if Into::<usize>::into(len) < slice.len() {
                panic!("Tried to construct a NativeStr from a &str or length {:?}, which exceeds this NativeStr's maximum stored length!", slice.len());
            }
        }

        &Self {
            start: unsafe { &*slice.as_ptr().cast() },
            len,
        }
    }
}

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
