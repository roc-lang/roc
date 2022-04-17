use core::ffi::c_void;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::alloc::{alloc, realloc, Layout};

#[no_mangle]
pub unsafe extern "C" fn roc_alloc(size: usize, alignment: u32) -> *mut c_void {
    let layout = Layout::from_size_align(size, alignment as usize).unwrap();
    let ptr = alloc(layout);

    return ptr as *mut c_void;
}

#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    old_size: usize,
    alignment: u32,
) -> *mut c_void {
    let layout = Layout::from_size_align(old_size, alignment as usize).unwrap();
    let ptr = realloc(c_ptr as *mut u8, layout, new_size);

    return ptr as *mut c_void;
}

#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    // TODO free memory for wasm. 
    #![cfg(not(target_arch="wasm32"))]
    return libc::free(c_ptr);
}

#[no_mangle]
pub unsafe extern "C" fn roc_panic(c_ptr: *mut c_void, tag_id: u32) {
    match tag_id {
        0 => {
            let slice = CStr::from_ptr(c_ptr as *const c_char);
            let string = slice.to_str().unwrap();
            eprintln!("Roc hit a panic: {}", string);
            std::process::exit(1);
        }
        _ => todo!(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn roc_memcpy(dst: *mut c_void, src: *mut c_void, n: usize) -> *mut c_void {
    let dst_bytes: *mut u8 = std::mem::transmute(dst);
    let src_bytes: *mut u8 = std::mem::transmute(src);
    let size = n as isize;
    for offset in 0..size {
        *dst_bytes.offset(offset) = *src_bytes.offset(offset);
    }
    dst
}

#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    let dst_bytes = dst as *mut u8;
    let size = n as isize;
    let c8 = c as u8;
    for offset in 0..size {
        *dst_bytes.offset(offset) = c8;
    }

    dst
}