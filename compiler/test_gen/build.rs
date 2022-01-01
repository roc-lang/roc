use std::env;
use std::ffi::OsStr;
use std::path::Path;
use std::process::Command;

const PLATFORM_FILENAME: &str = "wasm_test_platform";
const OUT_DIR_VAR: &str = "TEST_GEN_OUT";
const LIBC_PATH_VAR: &str = "TEST_GEN_WASM_LIBC_PATH";
const COMPILER_RT_PATH_VAR: &str = "TEST_GEN_WASM_COMPILER_RT_PATH";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    if feature_is_enabled("gen-wasm") {
        build_wasm();
    }
}

fn build_wasm() {
    let out_dir = env::var("OUT_DIR").unwrap();

    println!("cargo:rustc-env={}={}", OUT_DIR_VAR, out_dir);

    build_wasm_test_platform(&out_dir);
    build_wasm_libc(&out_dir);
}

fn zig_executable() -> String {
    match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    }
}

fn build_wasm_test_platform(out_dir: &str) {
    println!("cargo:rerun-if-changed=src/helpers/{}.c", PLATFORM_FILENAME);

    run_command(
        Path::new("."),
        &zig_executable(),
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
    let zig_cache_dir = format!("{}/zig-cache-wasm32", out_dir);

    run_command(
        cwd,
        &zig_executable(),
        [
            "build-exe", // must be an executable or it won't compile libc
            "-target",
            "wasm32-wasi",
            "-lc",
            source_path,
            "-femit-bin=/dev/null",
            "--global-cache-dir",
            &zig_cache_dir,
        ],
    );

    let libc_path = run_command(cwd, "find", [&zig_cache_dir, "-name", "libc.a"]);
    let compiler_rt_path = run_command(cwd, "find", [&zig_cache_dir, "-name", "compiler_rt.o"]);

    println!("cargo:rustc-env={}={}", LIBC_PATH_VAR, libc_path);
    println!(
        "cargo:rustc-env={}={}",
        COMPILER_RT_PATH_VAR, compiler_rt_path
    );
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
