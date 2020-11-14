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

pub const NUM_ASIN: &str = "roc_builtins.num.asin";
pub const NUM_ACOS: &str = "roc_builtins.num.acos";
pub const NUM_ATAN: &str = "roc_builtins.num.atan";
pub const NUM_IS_FINITE: &str = "roc_builtins.num.is_finite";
pub const NUM_POW_INT: &str = "roc_builtins.num.pow_int";

pub const STR_COUNT_SEGMENTS: &str = "roc_builtins.str.count_segments";
pub const STR_STR_SPLIT_IN_PLACE: &str = "roc_builtins.str.str_split_in_place";
pub const STR_COUNT_GRAPEHEME_CLUSTERS: &str = "roc_builtins.str.count_grapheme_clusters";
