#![crate_type = "staticlib"]

use std::ffi::CStr;
use std::os::raw::c_char;

extern "C" {
    #[link_name = "main_1"]
    fn str_from_roc() -> *const c_char;
}

// This magical #[export_name] annotation seems to be the only way to convince
// LLVM to emit this as the symbol "start" rather than "_start" on macOS.
// The linker will not recognize "_start" as the program entrypoint.
//
// See https://github.com/rust-lang/rust/issues/35052#issuecomment-235420755
#[export_name = "\x01start"]
pub extern "C" fn main(_argc: isize, _argv: *const *const u8) -> isize {
    let c_str = unsafe { CStr::from_ptr(str_from_roc()) };

    println!("Roc says: \"{}\"", c_str.to_str().unwrap());

    0
}
