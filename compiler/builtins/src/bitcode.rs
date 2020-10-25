use std::fs::File;
use std::io::prelude::Read;
use std::vec::Vec;

pub fn get_bytes() -> Vec<u8> {
    // In the build script for the gen module, we compile the builtins bitcode and set
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
