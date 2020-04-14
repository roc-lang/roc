use std::ffi::CStr;
use std::os::raw::c_char;

#[link(name = "roc_app", kind = "static")]
extern "C" {
    #[link_name = "$Test.main"]
    fn str_from_roc() -> *const c_char;
}

pub fn main() {
    let c_str = unsafe { CStr::from_ptr(str_from_roc()) };

    println!("Roc says: {}", c_str.to_str().unwrap());
}
