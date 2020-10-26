use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let src_path = fs::canonicalize("./../builtins/bitcode/src/main.zig")
        .expect("Failed to resolve bitcode source");
    let src = src_path.to_str().expect("Invalid src path");

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_ll_path = Path::new(&out_dir).join("builtins.ll");
    let dest_ll = dest_ll_path.to_str().expect("Invalid dest ir path");
    let dest_bc_path = Path::new(&out_dir).join("builtins.bc");
    let dest_bc = dest_bc_path.to_str().expect("Invalid dest bc path");

    Command::new("zig")
        .args(&["build-obj", src, "-femit-llvm-ir=", dest_ll])
        .status()
        .unwrap();

    Command::new("llvm-as")
        .args(&[dest_ll, "-o", dest_bc])
        .status()
        .unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", src);
    println!("cargo:rustc-env=BUILTINS_BC={}", dest_bc);
}
