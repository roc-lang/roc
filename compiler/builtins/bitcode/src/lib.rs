#![crate_type = "lib"]
#![no_std]

// TODO replace this with a normal Inkwell build_cast call - this was just
// used as a proof of concept for getting bitcode importing working!

#[no_mangle]
pub fn i64_to_f64_(num: i64) -> f64 {
    num as f64
}
