#[cfg(unix)]
pub struct IoError(i32);

#[cfg(windows)]
pub struct IoError(u32);

#[cfg(unix)]
impl IoError {
    pub fn write(&self, buf: &mut [u8]) -> Result<(), IoError> {
        extern "C" {
            // https://linux.die.net/man/3/strerror_r
            fn strerror_r(errnum: i32, buf: *mut u8, buf_len: usize) -> i32;
        }

        let error = unsafe { strerror_r(self.0, buf.as_mut_ptr(), buf.len()) };

        if error == 0 {
            Ok(())
        } else {
            Err(Self::most_recent())
        }
    }

    pub fn most_recent() -> Self {
        Self(unsafe { *errno_location() })
    }
}

#[cfg(windows)]
impl IoError {
    pub fn write(&self, buf: &mut [u16]) -> usize {
        // https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-formatmessagew
        extern "system" {
            fn FormatMessageW(
                dwFlags: u32,
                lpSource: *const c_void,
                dwMessageId: u32,
                dwLanguageId: u32,
                lpBuffer: *mut u16,
                nSize: u32,
                Arguments: *mut c_void,
            ) -> u32;
        }

        const FORMAT_MESSAGE_FROM_SYSTEM: u32 = 0x00001000;
        const FORMAT_MESSAGE_IGNORE_INSERTS: u32 = 0x00000200;

        let chars_written = unsafe {
            FormatMessageW(
                FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                core::ptr::null(),
                error_code,
                0, // Use default language
                buf.as_mut_ptr(),
                buf.len() as u32,
                core::ptr::null_mut(),
            )
        };

        if chars_written > 0 {
            Ok(chars_written as usize)
        } else {
            Err(IoError(error::last_error()))
        }
    }

    pub fn most_recent() -> Self {
        Self(unsafe { GetLastError() })
    }
}

// Adapted from https://github.com/rust-lang/rust copyright (C) Rust contributors
// Dual-licensed under MIT and Apache licenses.
// Thank you, Rust contributors.
extern "C" {
    #[cfg(not(any(target_os = "dragonfly", target_os = "vxworks")))]
    #[cfg_attr(
        any(
            target_os = "linux",
            target_os = "emscripten",
            target_os = "fuchsia",
            target_os = "l4re",
            target_os = "hurd",
        ),
        link_name = "__errno_location"
    )]
    #[cfg_attr(
        any(
            target_os = "netbsd",
            target_os = "openbsd",
            target_os = "android",
            target_os = "redox",
            target_env = "newlib"
        ),
        link_name = "__errno"
    )]
    #[cfg_attr(
        any(target_os = "solaris", target_os = "illumos"),
        link_name = "___errno"
    )]
    #[cfg_attr(target_os = "nto", link_name = "__get_errno_ptr")]
    #[cfg_attr(
        any(
            target_os = "macos",
            target_os = "ios",
            target_os = "tvos",
            target_os = "freebsd",
            target_os = "watchos"
        ),
        link_name = "__error"
    )]
    #[cfg_attr(target_os = "haiku", link_name = "_errnop")]
    #[cfg_attr(target_os = "aix", link_name = "_Errno")]
    fn errno_location() -> *mut i32;
}

#[cfg(windows)]
extern "system" {
    fn GetLastError() -> u32;
}
