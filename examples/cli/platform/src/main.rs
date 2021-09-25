#![allow(non_snake_case)]

use core::ffi::c_void;
use roc_std::RocStr;

fn main() {
    let mut result = host::rust_main();
    // This is stupid code that does nothing to avoid rust optimizing functions that roc needs away.
    if result == 0x1234_5678_9ABC_DEF0 {
        let roc_alloc_ptr: isize = unsafe {
            std::mem::transmute(
                host::roc_alloc as *const unsafe extern "C" fn(usize, u32) -> *mut c_void,
            )
        };
        let roc_realloc_ptr: isize = unsafe {
            std::mem::transmute(
                host::roc_realloc
                    as *const unsafe extern "C" fn(*mut c_void, usize, usize, u32) -> *mut c_void,
            )
        };
        let roc_dealloc_ptr: isize = unsafe {
            std::mem::transmute(host::roc_dealloc as *const unsafe extern "C" fn(*mut c_void, u32))
        };
        let roc_panic_ptr: isize = unsafe {
            std::mem::transmute(host::roc_panic as *const unsafe extern "C" fn(*mut c_void, u32))
        };
        let roc_memcpy_ptr: isize = unsafe {
            std::mem::transmute(
                host::roc_memcpy
                    as *const unsafe extern "C" fn(*mut c_void, *mut c_void, usize) -> *mut c_void,
            )
        };
        let roc_memset_ptr: isize = unsafe {
            std::mem::transmute(
                host::roc_memset
                    as *const unsafe extern "C" fn(*mut c_void, i32, usize) -> *mut c_void,
            )
        };
        let roc_fx_putLine_ptr: isize = unsafe {
            std::mem::transmute(host::roc_fx_putLine as *const extern "C" fn(line: RocStr) -> ())
        };
        let roc_fx_getLine_ptr: isize = unsafe {
            std::mem::transmute(host::roc_fx_getLine as *const extern "C" fn() -> RocStr)
        };
        // I really want to use the equivalent of std::hint::black_box, but it is expirimental.
        result = result ^ roc_alloc_ptr;
        result = result ^ roc_realloc_ptr;
        result = result ^ roc_dealloc_ptr;
        result = result ^ roc_panic_ptr;
        result = result ^ roc_memcpy_ptr;
        result = result ^ roc_memset_ptr;
        result = result ^ roc_fx_putLine_ptr;
        result = result ^ roc_fx_getLine_ptr;
        result = result ^ roc_alloc_ptr;
        result = result ^ roc_realloc_ptr;
        result = result ^ roc_dealloc_ptr;
        result = result ^ roc_panic_ptr;
        result = result ^ roc_memcpy_ptr;
        result = result ^ roc_memset_ptr;
        result = result ^ roc_fx_putLine_ptr;
        result = result ^ roc_fx_getLine_ptr;
    }
    std::process::exit(result as i32);
}
