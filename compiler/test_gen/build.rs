use std::env;
use std::ffi::OsStr;
use std::path::Path;
use std::process::Command;

pub const PLATFORM_FILENAME: &str = "wasm_test_platform";
pub const DIRNAME_VAR: &str = "TEST_GEN_OUT";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/helpers/{}.c", PLATFORM_FILENAME);

    let out_dir = env::var("OUT_DIR").unwrap();

    println!("cargo:rustc-env={}={}", DIRNAME_VAR, out_dir);

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

fn run_command<S, I, P: AsRef<Path>>(path: P, command_str: &str, args: I)
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
            true => (),
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
