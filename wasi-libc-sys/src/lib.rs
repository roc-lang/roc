use core::ffi::c_void;

// Rust's libc crate doesn't support Wasm, so we provide an implementation from Zig
// We define Rust signatures here as we need them, rather than trying to cover all of libc
extern "C" {
    pub fn malloc(size: usize) -> *mut c_void;
    pub fn free(p: *mut c_void);
    pub fn realloc(p: *mut c_void, size: usize) -> *mut c_void;
    pub fn memcpy(dst: *mut c_void, src: *mut c_void, n: usize) -> *mut c_void;
    pub fn memset(dst: *mut c_void, ch: i32, n: usize) -> *mut c_void;
}
