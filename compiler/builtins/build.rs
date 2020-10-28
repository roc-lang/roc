use std::convert::AsRef;
use std::env;
use std::ffi::OsStr;
use std::path::Path;
use std::process::Command;
use std::str;

fn run_command<S, I>(command: &str, args: I)
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output_result = Command::new(OsStr::new(&command)).args(args).output();
    match output_result {
        Ok(output) => match output.status.success() {
            true => (),
            false => {
                let error_str = match str::from_utf8(&output.stderr) {
                    Ok(stderr) => stderr.to_string(),
                    Err(_) => format!("Failed to run \"{}\"", command),
                };
                panic!("{} failed: {}", command, error_str);
            }
        },
        Err(reason) => panic!("{} failed: {}", command, reason),
    }
}

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();

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

    run_command(
        "zig",
        &[
            "build-obj",
            src_path_str,
            &emit_ir_arg,
            "-fno-emit-bin",
            "--strip",
            "-O",
            "ReleaseFast",
            "--cache-dir",
            zig_cache_dir_str,
        ],
    );

    let dest_bc_path = Path::new(&out_dir).join("builtins.bc");
    let dest_bc = dest_bc_path.to_str().expect("Invalid dest bc path");
    println!("Compiling bitcode to: {}", dest_bc);

    run_command("llvm-as", &[dest_ll, "-o", dest_bc]);

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", src_path_str);
    println!("cargo:rustc-env=BUILTINS_BC={}", dest_bc);
}
