use roc_builtins::bitcode;
use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

use wasi_libc_sys::{WASI_COMPILER_RT_PATH, WASI_LIBC_PATH};

const PLATFORM_FILENAME: &str = "wasm_test_platform";
const OUT_DIR_VAR: &str = "TEST_GEN_OUT";

const LINKING_TEST_HOST_SOURCE: &str = "src/helpers/wasm_linking_test_host.zig";
const LINKING_TEST_IMPORT_SOURCE: &str = "src/helpers/wasm_linking_host_imports.zig";
const LINKING_TEST_HOST_WASM: &str = "build/wasm_linking_test_host.wasm";
const LINKING_TEST_HOST_NATIVE: &str = "build/wasm_linking_test_host";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    if feature_is_enabled("gen-wasm") {
        build_wasm_test_host();
        build_wasm_linking_test_host();
    }
}

fn build_wasm_linking_test_host() {
    println!("cargo:rerun-if-changed={}", LINKING_TEST_HOST_SOURCE);
    println!("cargo:rerun-if-changed={}", LINKING_TEST_IMPORT_SOURCE);

    fs::create_dir("build").unwrap();

    if Path::new(LINKING_TEST_HOST_WASM).exists() {
        fs::remove_file(LINKING_TEST_HOST_WASM).unwrap();
    }

    Command::new("zig")
        .args([
            "build-obj",
            "-target",
            "wasm32-freestanding-musl",
            LINKING_TEST_HOST_SOURCE,
            &format!("-femit-bin={}", LINKING_TEST_HOST_WASM),
        ])
        .output()
        .unwrap();

    let host_obj = "build/wasm_linking_test_host.o";
    Command::new("zig")
        .args([
            "build-obj",
            LINKING_TEST_HOST_SOURCE,
            &format!("-femit-bin={}", host_obj),
        ])
        .output()
        .unwrap();

    let import_obj = "build/wasm_linking_host_imports.o";
    Command::new("zig")
        .args([
            "build-obj",
            LINKING_TEST_IMPORT_SOURCE,
            &format!("-femit-bin={}", import_obj),
        ])
        .output()
        .unwrap();

    Command::new("zig")
        .args([
            "build-exe",
            host_obj,
            import_obj,
            &format!("-femit-bin={}", LINKING_TEST_HOST_NATIVE),
        ])
        .output()
        .unwrap();
}

fn build_wasm_test_host() {
    let source_path = format!("src/helpers/{}.c", PLATFORM_FILENAME);
    println!("cargo:rerun-if-changed={}", source_path);

    let out_dir = env::var("OUT_DIR").unwrap();
    println!("cargo:rustc-env={}={}", OUT_DIR_VAR, out_dir);

    // Create an object file with relocations
    let platform_path = build_wasm_platform(&out_dir, &source_path);

    Command::new(&zig_executable())
        .args([
            "wasm-ld",
            bitcode::BUILTINS_WASM32_OBJ_PATH,
            &platform_path,
            WASI_COMPILER_RT_PATH,
            WASI_LIBC_PATH,
            "-o",
            &format!("{}/{}.o", out_dir, PLATFORM_FILENAME),
            "--no-entry",
            "--relocatable",
        ])
        .output()
        .unwrap();
}

fn zig_executable() -> String {
    match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    }
}

fn build_wasm_platform(out_dir: &str, source_path: &str) -> String {
    let platform_path = format!("{}/{}.o", out_dir, PLATFORM_FILENAME);

    Command::new(&zig_executable())
        .args([
            "build-lib",
            "-target",
            "wasm32-wasi",
            "-lc",
            source_path,
            &format!("-femit-bin={}", &platform_path),
        ])
        .output()
        .unwrap();

    platform_path
}

fn feature_is_enabled(feature_name: &str) -> bool {
    let cargo_env_var = format!(
        "CARGO_FEATURE_{}",
        feature_name.replace('-', "_").to_uppercase()
    );
    env::var(cargo_env_var).is_ok()
}
