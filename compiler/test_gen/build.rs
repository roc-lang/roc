use roc_builtins::bitcode;
use std::env;
use std::ffi::OsStr;
use std::path::Path;
use std::process::Command;

const PLATFORM_FILENAME: &str = "wasm_test_platform";
const OUT_DIR_VAR: &str = "TEST_GEN_OUT";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    if feature_is_enabled("gen-wasm") {
        build_wasm();
    }
}

fn build_wasm() {
    let out_dir = env::var("OUT_DIR").unwrap();

    println!("cargo:rustc-env={}={}", OUT_DIR_VAR, out_dir);

    build_wasm_platform_and_builtins(&out_dir);
}

fn zig_executable() -> String {
    match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    }
}

/// Create an all-in-one object file: platform + builtins + libc
fn build_wasm_platform_and_builtins(out_dir: &str) {
    println!("cargo:rerun-if-changed=src/helpers/{}.c", PLATFORM_FILENAME);

    // See discussion with Luuk de Gram (Zig contributor)
    // https://github.com/rtfeldman/roc/pull/2181#pullrequestreview-839608063
    // This builds a library file that exports everything. It has no linker data but we don't need that.
    let args = [
        "build-lib",
        "-target",
        "wasm32-wasi",
        "-lc",
        "-dynamic", // -dynamic ensures libc code goes into the binary
        bitcode::BUILTINS_WASM32_OBJ_PATH,
        &format!("src/helpers/{}.c", PLATFORM_FILENAME),
        &format!("-femit-bin={}/{}.o", out_dir, PLATFORM_FILENAME),
    ];

    let zig = zig_executable();

    // println!("{} {}", zig, args.join(" "));

    run_command(Path::new("."), &zig, args);
}

fn feature_is_enabled(feature_name: &str) -> bool {
    let cargo_env_var = format!(
        "CARGO_FEATURE_{}",
        feature_name.replace("-", "_").to_uppercase()
    );
    env::var(cargo_env_var).is_ok()
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
            true => std::str::from_utf8(&output.stdout).unwrap().to_string(),
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
