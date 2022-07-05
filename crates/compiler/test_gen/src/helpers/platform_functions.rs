use core::ffi::c_void;

/// # Safety
/// The Roc application needs this.
#[no_mangle]
pub unsafe fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    let ptr = libc::malloc(size);
    println!("allocated: {:x?}", ptr);

    ptr
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

/// # Safety
/// The Roc application needs this.
#[no_mangle]
pub unsafe fn roc_panic(c_ptr: *mut c_void, tag_id: u32) {
    use roc_gen_llvm::llvm::build::PanicTagId;

    use std::ffi::CStr;
    use std::os::raw::c_char;

    match PanicTagId::try_from(tag_id) {
        Ok(PanicTagId::NullTerminatedString) => {
            let slice = CStr::from_ptr(c_ptr as *const c_char);
            let string = slice.to_str().unwrap();
            eprintln!("Roc hit a panic: {}", string);
            std::process::exit(1);
        }
        Err(_) => unreachable!(),
    }
}
