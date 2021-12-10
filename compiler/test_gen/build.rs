use std::env;
use std::ffi::OsStr;
use std::path::Path;
use std::process::Command;

pub const PLATFORM_FILENAME: &str = "wasm_test_platform";
pub const OUT_DIR_VAR: &str = "TEST_GEN_OUT";
pub const LIBC_PATH_VAR: &str = "TEST_GEN_WASM_LIBC_PATH";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = env::var("OUT_DIR").unwrap();

    println!("cargo:rustc-env={}={}", OUT_DIR_VAR, out_dir);

    build_wasm_test_platform(&out_dir);
    build_wasm_libc(&out_dir);
}

fn build_wasm_test_platform(out_dir: &str) {
    println!("cargo:rerun-if-changed=src/helpers/{}.c", PLATFORM_FILENAME);

    run_command(
        Path::new("."),
        "zig",
        [
            "build-obj",
            "-target",
            "wasm32-wasi",
            "-lc",
            &format!("src/helpers/{}.c", PLATFORM_FILENAME),
            &format!("-femit-bin={}/{}.o", out_dir, PLATFORM_FILENAME),
        ],
    );
}

fn build_wasm_libc(out_dir: &str) {
    let source_path = "src/helpers/dummy_libc_program.c";
    println!("cargo:rerun-if-changed={}", source_path);
    let cwd = Path::new(".");

    run_command(
        cwd,
        "zig",
        [
            "build-exe", // must be an executable or it won't compile libc
            "-target",
            "wasm32-wasi",
            "-lc",
            source_path,
            "-femit-bin=/dev/null",
            "--global-cache-dir",
            out_dir,
        ],
    );

    let libc_path = run_command(
        cwd,
        "find",
        [
            out_dir,
            "-name",
            "libc.a"
        ],
    );

    println!("cargo:rustc-env={}={}", LIBC_PATH_VAR, libc_path);
}

fn run_command<S, I, P: AsRef<Path>>(path: P, command_str: &str, args: I) -> String
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output_result = Command::new(OsStr::new(&command_str))
        .current_dir(path)
        .args(args)
        .output();
    match output_result {
        Ok(output) => match output.status.success() {
            true => std::str::from_utf8(&output.stdout),
            false => {
                let error_str = match std::str::from_utf8(&output.stderr) {
                    Ok(stderr) => stderr.to_string(),
                    Err(_) => format!("Failed to run \"{}\"", command_str),
                };
                panic!("{} failed: {}", command_str, error_str);
            }
        },
        Err(reason) => panic!("{} failed: {}", command_str, reason),
    }
}
