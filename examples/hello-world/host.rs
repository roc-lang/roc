use std::ffi::CStr;
use std::os::raw::c_char;

#[derive(Debug)]
#[repr(C)]
struct Triple {
    a: f64,
    b: f64,
    c: f64,
}

#[link(name = "hello_from_roc")]
extern "C" {
    #[link_name = "$Test.main"]
    fn floats() -> Triple;
}

pub fn main() {
    println!("Roc says: {:?}", unsafe { floats() });
}
