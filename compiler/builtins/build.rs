use std::env;
use std::io;
use std::path::Path;
use std::process::Command;
use std::str;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let out_dir_str = out_dir.to_str().unwrap();
    Command::new("mkdir")
        .args(&["-p", out_dir_str])
        .output()
        .and_then(|r| match r.status.success() {
            true => Ok(r),
            false => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                match str::from_utf8(&r.stderr) {
                    Ok(stderr) => stderr,
                    Err(_) => "Failed make out directory",
                },
            )),
        })
        .unwrap();

    // "." is relative to where "build.rs" is
    let src_path = Path::new(".").join("bitcode").join("src").join("main.zig");
    let src_path_str = src_path.to_str().expect("Invalid src path");
    println!("Building main.zig from: {}", src_path_str);

    let zig_cache_dir = Path::new(&out_dir).join("zig-cache");
    let zig_cache_dir_str = zig_cache_dir.to_str().expect("Invalid zig cache dir");
    println!("Setting zig cache to: {}", zig_cache_dir_str);

    let dest_ll_path = Path::new(&out_dir).join("builtins.ll");
    let dest_ll = dest_ll_path.to_str().expect("Invalid dest ir path");
    let emit_ir_arg = "-femit-llvm-ir=".to_owned() + dest_ll;
    println!("Compiling zig llvm-ir to: {}", dest_ll);

    let zig_output = Command::new("zig")
        .args(&[
            "build-obj",
            src_path_str,
            &emit_ir_arg,
            "-fno-emit-bin",
            "--strip",
            "-O",
            "ReleaseFast",
            "--cache-dir",
            zig_cache_dir_str,
        ])
        .output()
        .expect("Failed to start zig build-obj");
    match zig_output.status.success() {
        true => (),
        false => {
            let error_str = match str::from_utf8(&zig_output.stderr) {
                Ok(stderr) => stderr,
                Err(_) => "Failed to build zig",
            };
            panic!("zig build-obj failed: {}", error_str);
        }
    };

    let dest_bc_path = Path::new(&out_dir).join("builtins.bc");
    let dest_bc = dest_bc_path.to_str().expect("Invalid dest bc path");
    println!("Compiling bitcode to: {}", dest_bc);

    Command::new("llvm-as")
        .args(&[dest_ll, "-o", dest_bc])
        .output()
        .expect("Failed to start llvm-ar");
    match zig_output.status.success() {
        true => (),
        false => {
            let error_str = match str::from_utf8(&zig_output.stderr) {
                Ok(stderr) => stderr,
                Err(_) => "Failed to run llvm-ar",
            };
            panic!("llvm-ar failed: {}", error_str);
        }
    };

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", src_path_str);
    println!("cargo:rustc-env=BUILTINS_BC={}", dest_bc);
}
