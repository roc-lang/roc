use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let src_path = fs::canonicalize("./../builtins/bitcode/src/lib.c")
        .expect("Failed to resolve bitcode source");
    let src = src_path.to_str().expect("Invalid src path");

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("builtins.bc");
    let dest = dest_path.to_str().expect("Invalid dest path");

    Command::new("clang")
        .args(&["-emit-llvm", "-o", dest, "-c", src])
        .status()
        .unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", src);
    println!("cargo:rustc-env=BUILTINS_BC={}", dest);
}
