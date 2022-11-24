use core::ffi::c_void;

/// # Safety
/// The Roc application needs this.
#[no_mangle]
pub unsafe fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    libc::malloc(size)
}

/// # Safety
/// The Roc application needs this.
#[no_mangle]
pub unsafe fn roc_memcpy(dest: *mut c_void, src: *const c_void, bytes: usize) -> *mut c_void {
    libc::memcpy(dest, src, bytes)
}

/// # Safety
/// The Roc application needs this.
#[no_mangle]
pub unsafe fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    libc::realloc(c_ptr, new_size)
}

/// # Safety
/// The Roc application needs this.
#[no_mangle]
pub unsafe fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    libc::free(c_ptr)
}
