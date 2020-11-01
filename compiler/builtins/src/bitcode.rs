use std::fs::File;
use std::io::prelude::Read;
use std::vec::Vec;

pub fn get_bytes() -> Vec<u8> {
    // In the build script for the builtins module, we compile the builtins bitcode and set
    // BUILTINS_BC to the path to the compiled output.
    let path: &'static str = env!(
        "BUILTINS_BC",
        "Env var BUILTINS_BC not found. Is there a problem with the build script?"
    );
    let mut builtins_bitcode = File::open(path).expect("Unable to find builtins bitcode source");
    let mut buffer = Vec::new();
    builtins_bitcode
        .read_to_end(&mut buffer)
        .expect("Unable to read builtins bitcode");
    buffer
}

pub const MATH_ASIN: &str = "roc_builtins.math.asin";
pub const MATH_ACOS: &str = "roc_builtins.math.acos";
pub const MATH_ATAN: &str = "roc_builtins.math.atan";
pub const MATH_IS_FINITE: &str = "roc_builtins.math.is_finite";
pub const MATH_POW_INT: &str = "roc_builtins.math.pow_int";

pub const STR_COUNT_SEGEMENTS: &str = "roc_builtins.str.count_segements";
pub const STR_STR_SPLIT_IN_PLACE: &str = "roc_builtins.str.str_split_in_place";
