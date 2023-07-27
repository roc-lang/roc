use core::ffi::c_void;

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let answer = roc_app::mainForHost(
        // {{ mainForHostArgs }}
    );

    assert_eq!(
        // {{ mainForHostExpected }}
        , answer
    );

    println!("Assertion passed!");

    0
}

#[no_mangle]
pub unsafe extern "C" fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    return libc::malloc(size);
}

#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    return libc::realloc(c_ptr, new_size);
}

#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    return libc::free(c_ptr);
}
