use roc_builtins::bitcode;
use std::env;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use wasi_libc_sys::{WASI_COMPILER_RT_PATH, WASI_LIBC_PATH};

const PLATFORM_FILENAME: &str = "wasm_test_platform";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    if feature_is_enabled("gen-wasm") || feature_is_enabled("gen-llvm-wasm") {
        build_wasm_test_host();
        build_wasm_linking_test_host();
    }
}

fn build_wasm_linking_test_host() {
    let host_source_path = PathBuf::from("src")
        .join("helpers")
        .join("wasm_linking_test_host.zig");

    let import_source_path = PathBuf::from("src")
        .join("helpers")
        .join("wasm_linking_host_imports.zig");

    let host_wasm_path = PathBuf::from("build").join("wasm_linking_test_host.wasm");
    let host_native_path = PathBuf::from("build").join("wasm_linking_test_host.exe");

    let host_source: &str = host_source_path.to_str().unwrap();
    let import_source: &str = import_source_path.to_str().unwrap();
    let host_wasm: &str = host_wasm_path.to_str().unwrap();
    let host_native: &str = host_native_path.to_str().unwrap();

    println!("cargo:rerun-if-changed={}", host_source);
    println!("cargo:rerun-if-changed={}", import_source);

    if !Path::new("build").exists() {
        fs::create_dir("build").unwrap();
    }

    if Path::new(host_wasm).exists() {
        fs::remove_file(host_wasm).unwrap();
    }

    run_zig(&[
        "build-obj",
        "-target",
        "wasm32-freestanding-musl",
        host_source,
        &format!("-femit-bin={}", host_wasm),
    ]);

    let host_obj_path = PathBuf::from("build").join("wasm_linking_test_host.obj");
    let host_obj = host_obj_path.to_str().unwrap();
    run_zig(&[
        "build-obj",
        host_source,
        &format!("-femit-bin={}", &host_obj),
    ]);

    let import_obj_path = PathBuf::from("build").join("wasm_linking_host_imports.obj");
    let import_obj = import_obj_path.to_str().unwrap();
    run_zig(&[
        "build-obj",
        import_source,
        &format!("-femit-bin={}", &import_obj),
    ]);

    run_zig(&[
        "build-exe",
        host_obj,
        import_obj,
        &format!("-femit-bin={}", host_native),
        "--subsystem",
        "console",
        "-lc",
        //"-fcompiler-rt",
    ]);
}

fn build_wasm_test_host() {
    let mut source_path = PathBuf::new()
        .join("src")
        .join("helpers")
        .join(PLATFORM_FILENAME);
    source_path.set_extension("c");
    println!("cargo:rerun-if-changed={}", source_path.to_str().unwrap());

    let out_dir = env::var("OUT_DIR").unwrap();

    // Create an object file with relocations
    let platform_path = build_wasm_platform(&out_dir, source_path.to_str().unwrap());

    let mut outfile = PathBuf::from(&out_dir).join(PLATFORM_FILENAME);
    outfile.set_extension("wasm");

    run_zig(&[
        "wasm-ld",
        &bitcode::get_builtins_wasm32_obj_path(),
        platform_path.to_str().unwrap(),
        WASI_COMPILER_RT_PATH,
        WASI_LIBC_PATH,
        "-o",
        outfile.to_str().unwrap(),
        "--no-entry",
        "--relocatable",
    ]);
}

fn zig_executable() -> String {
    match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    }
}

fn build_wasm_platform(out_dir: &str, source_path: &str) -> PathBuf {
    let mut outfile = PathBuf::from(out_dir).join(PLATFORM_FILENAME);
    outfile.set_extension("o");

    run_zig(&[
        "build-lib",
        "-target",
        "wasm32-wasi",
        "-lc",
        source_path,
        &format!("-femit-bin={}", outfile.to_str().unwrap()),
    ]);

    outfile
}

fn feature_is_enabled(feature_name: &str) -> bool {
    let cargo_env_var = format!(
        "CARGO_FEATURE_{}",
        feature_name.replace('-', "_").to_uppercase()
    );
    env::var(cargo_env_var).is_ok()
}

// Run cargo with -vv to see commands printed out
fn run_zig(args: &[&str]) {
    let zig = zig_executable();
    println!("{} {}", zig, args.join(" "));
    let output = Command::new(&zig).args(args).output().unwrap();

    assert!(output.status.success(), "{:#?}", output);
    assert!(output.stdout.is_empty(), "{:#?}", output);
    assert!(output.stderr.is_empty(), "{:#?}", output);
}
