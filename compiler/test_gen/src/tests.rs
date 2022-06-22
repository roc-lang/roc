#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
// we actually want to compare against the literal float bits
#![allow(clippy::float_cmp)]

pub mod gen_abilities;
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

#[cfg(feature = "gen-wasm")]
pub mod wasm_linking;

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
