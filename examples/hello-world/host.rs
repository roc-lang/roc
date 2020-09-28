#![crate_type = "lib"]

use std::ffi::CStr;
use std::os::raw::c_char;

#[link(name = "roc_app")]
extern "C" {
    #[link_name = "main_1"]
    fn str_from_roc() -> *const c_char;
}

#[no_mangle]
pub extern "C" fn main() {
    let c_str = unsafe { CStr::from_ptr(str_from_roc()) };

    println!("Roc says: {}", c_str.to_str().unwrap());
}
