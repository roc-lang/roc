#![allow(non_snake_case)]

use core::ffi::c_void;
use core::mem::MaybeUninit;
use libc::c_char;
use roc_std::{RocCallResult, RocStr};
use std::ffi::CStr;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed"]
    fn roc_main(output: *mut RocCallResult<RocStr>) -> ();
}

#[no_mangle]
pub unsafe fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    return libc::malloc(size);
}

#[no_mangle]
pub unsafe fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    return libc::realloc(c_ptr, new_size);
}

#[no_mangle]
pub unsafe fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    return libc::free(c_ptr);
}

// #[no_mangle]
// pub unsafe fn roc_panic(c_ptr: *mut c_void, tag_id: u32) {
//     match tag_id {
//         0 => {
//             let slice = CStr::from_ptr(c_ptr as *const c_char);
//             let string = slice.to_str().unwrap();
//             eprintln!("Roc hit a panic: {}", string);
//             std::process::exit(1);
//         }
//         _ => todo!(),
//     }
// }

#[no_mangle]
pub fn rust_main() -> isize {
    let mut call_result: MaybeUninit<RocCallResult<RocStr>> = MaybeUninit::uninit();

    unsafe {
        roc_main(call_result.as_mut_ptr());

        let output = call_result.assume_init();

        match output.into() {
            Ok(roc_str) => {
                let len = roc_str.len();
                let str_bytes = roc_str.get_bytes() as *const libc::c_void;

                if libc::write(1, str_bytes, len) < 0 {
                    panic!("Writing to stdout failed!");
                }
            }
            Err(msg) => {
                panic!("Roc failed with message: {}", msg);
            }
        }
    }

    // Exit code
    0
}
