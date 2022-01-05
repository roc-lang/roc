#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
// we actually want to compare against the literal float bits
#![allow(clippy::float_cmp)]

pub mod gen_compare;
pub mod gen_dict;
pub mod gen_list;
pub mod gen_num;
pub mod gen_primitives;
pub mod gen_records;
pub mod gen_refcount;
pub mod gen_result;
pub mod gen_set;
pub mod gen_str;
pub mod gen_tags;
mod helpers;
pub mod wasm_str;

use core::ffi::c_void;
#[no_mangle]
pub unsafe fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    libc::malloc(size)
}

#[no_mangle]
pub unsafe fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    libc::realloc(c_ptr, new_size)
}

#[no_mangle]
pub unsafe fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    libc::free(c_ptr)
}

#[no_mangle]
pub unsafe fn roc_panic(c_ptr: *mut c_void, tag_id: u32) {
    use roc_gen_llvm::llvm::build::PanicTagId;

    use std::convert::TryFrom;
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
