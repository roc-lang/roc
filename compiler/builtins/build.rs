use std::convert::AsRef;
use std::env;
use std::ffi::OsStr;
use std::fs::{self};
use std::io;
use std::path::Path;
use std::process::Command;
use std::str;

// TODO: Use zig build system command instead

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();

    // "." is relative to where "build.rs" is
    let build_script_dir_path = Path::new(".");

    let bitcode_path = build_script_dir_path.join("bitcode");

    let src_path = bitcode_path.join("src");

    let build_zig_path = bitcode_path.join("build.zig");
    let build_zig = build_zig_path.to_str().expect("Invalid build path");

    let dest_ir_path = build_script_dir_path.join("builtins.ll");
    let dest_ir = dest_ir_path.to_str().expect("Invalid dest ir path");
    println!("Compiling ir to: {}", dest_ir);

    run_command(
        "zig",
        &["build", "ir", "-Drelease=true", "--build-file", build_zig],
    );

    let dest_bc_path = Path::new(&out_dir).join("builtins.bc");
    let dest_bc = dest_bc_path.to_str().expect("Invalid dest bc path");
    println!("Compiling bitcode to: {}", dest_bc);

    run_command("llvm-as-10", &[dest_ir, "-o", dest_bc]);

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rustc-env=BUILTINS_BC={}", dest_bc);
    get_zig_files(src_path.as_path(), &|path| {
        let path: &Path = path;
        println!(
            "cargo:rerun-if-changed={}",
            path.to_str().expect("Failed to convert path to str")
        );
        ()
    })
    .unwrap();
}

fn run_command<S, I>(command_str: &str, args: I)
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output_result = Command::new(OsStr::new(&command_str)).args(args).output();
    match output_result {
        Ok(output) => match output.status.success() {
            true => (),
            false => {
                let error_str = match str::from_utf8(&output.stderr) {
                    Ok(stderr) => stderr.to_string(),
                    Err(_) => format!("Failed to run \"{}\"", command_str),
                };
                panic!("{} failed: {}", command_str, error_str);
            }
        },
        Err(reason) => panic!("{} failed: {}", command_str, reason),
    }
}

fn get_zig_files(dir: &Path, cb: &dyn Fn(&Path)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path_buf = entry.path();
            if path_buf.is_dir() {
                get_zig_files(&path_buf, cb).unwrap();
            } else {
                let path = path_buf.as_path();
                let path_ext = path.extension().unwrap();
                if path_ext == "zig" {
                    cb(path);
                }
            }
        }
    }
    Ok(())
}

// fn get_zig_files(dir: &Path) -> io::Result<Vec<&Path>> {
// let mut vec = Vec::new();
// if dir.is_dir() {
// for entry in fs::read_dir(dir)? {
// let entry = entry?;
// let path_buf = entry.path();
// if path_buf.is_dir() {
// match get_zig_files(&path_buf) {
// Ok(sub_files) => vec = [vec, sub_files].concat(),
// Err(_) => (),
// };
// } else {
// let path = path_buf.as_path();
// let path_ext = path.extension().unwrap();
// if path_ext == "zig" {
// vec.push(path.clone());
// }
// }
// }
// }
// Ok(vec)
// }
