use roc_builtins::bitcode;
use std::env;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
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
    let source_path = format!("src/helpers/{}.c", PLATFORM_FILENAME);
    println!("cargo:rerun-if-changed={}", source_path);

    let out_dir = env::var("OUT_DIR").unwrap();
    println!("cargo:rustc-env={}={}", OUT_DIR_VAR, out_dir);

    // Zig can produce *either* an object containing relocations OR an object containing libc code
    // But we want both, so we have to compile twice with different flags, then link them

    // Create an object file with relocations
    let platform_path = build_wasm_platform(&out_dir, &source_path);

    // Compile again to get libc path
    let (libc_path, compiler_rt_path) = build_wasm_libc_compilerrt(&out_dir, &source_path);
    let mut libc_pathbuf = PathBuf::from(&libc_path);
    libc_pathbuf.pop();
    let libc_dir = libc_pathbuf.to_str().unwrap();

    let args = &[
        "wasm-ld",
        bitcode::BUILTINS_WASM32_OBJ_PATH,
        &platform_path,
        &compiler_rt_path,
        "-L",
        libc_dir,
        "-lc",
        "-o",
        &format!("{}/{}.o", out_dir, PLATFORM_FILENAME),
        "--export-all",
        "--no-entry",
        // "--emit-relocs", // TODO: resize stack by relocating __heap_base (issue #2480) here and in repl_test build
    ];

    let zig = zig_executable();

    // println!("{} {}", zig, args.join(" "));

    run_command(&zig, args);
}

fn zig_executable() -> String {
    match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    }
}

fn build_wasm_platform(out_dir: &str, source_path: &str) -> String {
    let platform_path = format!("{}/{}.o", out_dir, PLATFORM_FILENAME);

    run_command(
        &zig_executable(),
        &[
            "build-lib",
            "-target",
            "wasm32-wasi",
            "-lc",
            source_path,
            &format!("-femit-bin={}", &platform_path),
        ],
    );

    platform_path
}

fn build_wasm_libc_compilerrt(out_dir: &str, source_path: &str) -> (String, String) {
    let zig_cache_dir = format!("{}/zig-cache-wasm32", out_dir);

    run_command(
        &zig_executable(),
        &[
            "build-lib",
            "-dynamic", // ensure libc code is actually generated (not just linked against header)
            "-target",
            "wasm32-wasi",
            "-lc",
            source_path,
            "-femit-bin=/dev/null",
            "--global-cache-dir",
            &zig_cache_dir,
        ],
    );

    (
        run_command("find", &[&zig_cache_dir, "-name", "libc.a"]),
        run_command("find", &[&zig_cache_dir, "-name", "compiler_rt.o"]),
    )
}

fn feature_is_enabled(feature_name: &str) -> bool {
    let cargo_env_var = format!(
        "CARGO_FEATURE_{}",
        feature_name.replace('-', "_").to_uppercase()
    );
    env::var(cargo_env_var).is_ok()
}

fn run_command(command_str: &str, args: &[&str]) -> String {
    let output_result = Command::new(OsStr::new(&command_str))
        .current_dir(Path::new("."))
        .args(args)
        .output();

    let fail = |err: String| {
        panic!(
            "\n\nFailed command:\n\t{} {}\n\n{}",
            command_str,
            args.join(" "),
            err
        );
    };

    match output_result {
        Ok(output) => match output.status.success() {
            true => std::str::from_utf8(&output.stdout)
                .unwrap()
                .trim()
                .to_string(),
            false => {
                let error_str = match std::str::from_utf8(&output.stderr) {
                    Ok(stderr) => stderr.to_string(),
                    Err(_) => format!("Failed to run \"{}\"", command_str),
                };
                fail(error_str)
            }
        },
        Err(reason) => fail(reason.to_string()),
    }
}
