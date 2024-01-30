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

#[inline(always)]
#[cfg(unix)]
pub fn last_error() -> i32 {
    unsafe { *errno_location() }
}

#[cfg(windows)]
extern "system" {
    fn GetLastError() -> u32;
}

#[cfg(windows)]
#[inline(always)]
pub fn last_error() -> u32 {
    GetLastError()
}
