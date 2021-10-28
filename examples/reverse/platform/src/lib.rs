#![allow(non_snake_case)]

use core::ffi::c_void;
use libc;
use roc_std::RocStr;
use roc_std::RocList;
use std::ffi::CStr;
use std::os::raw::c_char;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed"]
    fn roc_main(input: RocList<RocStr>) -> RocList<RocStr>;
}

#[no_mangle]
pub unsafe extern "C" fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    libc::malloc(size)
}

#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    libc::realloc(c_ptr, new_size)
}

#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    libc::free(c_ptr)
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
    libc::memcpy(dst, src, n)
}

#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let args = std::env::args()
        .map(|s| RocStr::from_slice(s.as_bytes()))
        .collect::<Vec<_>>();

    let args_list = RocList::from_slice(&args);
    unsafe {
        let out = roc_main(args_list);
        for arg in out.into_iter() {
            println!("{}", arg.as_str());
        }
    }

    // Exit code
    0
}

