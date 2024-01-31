#[derive(Debug, Clone, Copy)]
pub struct IoError(i32);

impl IoError {
    /// ENOMEM is 12 in errno.h https://en.wikipedia.org/wiki/Errno.h
    pub const NOT_ENOUGH_MEMORY: Self = Self(12);

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

        Self(unsafe { *errno_location() })
    }
}
