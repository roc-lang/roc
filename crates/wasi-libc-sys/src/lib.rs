//! Provides a Rust wrapper for the WebAssembly libc, which is used when
//! preprocessing hosts for use with the Wasm development back-end, and for the
//! mock hosts we use in our Wasm tests.
// Rust's libc crate doesn't support Wasm, so we provide an implementation from Zig
// We define Rust signatures here as we need them, rather than trying to cover all of libc
#[cfg(target_family = "wasm")]
use core::ffi::c_void;
#[cfg(target_family = "wasm")]
extern "C" {
    pub fn malloc(size: usize) -> *mut c_void;
    pub fn free(p: *mut c_void);
    pub fn realloc(p: *mut c_void, size: usize) -> *mut c_void;
    pub fn memcpy(dst: *mut c_void, src: *mut c_void, n: usize) -> *mut c_void;
    pub fn memset(dst: *mut c_void, ch: i32, n: usize) -> *mut c_void;
}

// Tell users of this crate where to find the Wasm .a file
// If a non-Wasm target is using this crate, we assume it is a build script that wants to emit Wasm
// For Wasm target, it won't ever be used, but we expose it just to keep things simple

// these variables are set by build.rs
pub const WASI_LIBC_PATH: &str = env!("WASI_LIBC_PATH");
pub const WASI_COMPILER_RT_PATH: &str = env!("WASI_COMPILER_RT_PATH");
