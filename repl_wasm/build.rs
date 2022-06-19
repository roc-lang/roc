use std::env;
use std::path::PathBuf;
use std::process::Command;

use roc_builtins::bitcode;
use wasi_libc_sys::{WASI_COMPILER_RT_PATH, WASI_LIBC_PATH};

const PLATFORM_FILENAME: &str = "repl_platform";
const PRE_LINKED_BINARY: [&str; 2] = ["src", "pre_linked_binary.o"];

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let source_path = format!("src/{}.c", PLATFORM_FILENAME);
    println!("cargo:rerun-if-changed={}", source_path);

    // Zig can produce *either* an object containing relocations OR an object containing libc code
    // But we want both, so we have to compile twice with different flags, then link them

    // Create an object file with relocations
    let out_dir = env::var("OUT_DIR").unwrap();
    let platform_obj = build_wasm_platform(&out_dir, &source_path);

    let pre_linked_binary_path: PathBuf = PRE_LINKED_BINARY.iter().collect();

    Command::new(&zig_executable())
        .args([
            "wasm-ld",
            bitcode::BUILTINS_WASM32_OBJ_PATH,
            &platform_obj,
            WASI_COMPILER_RT_PATH,
            WASI_LIBC_PATH,
            "-o",
            pre_linked_binary_path.to_str().unwrap(),
            "--export-all",
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
    let platform_obj = format!("{}/{}.o", out_dir, PLATFORM_FILENAME);

    Command::new(&zig_executable())
        .args([
            "build-lib",
            "-target",
            "wasm32-wasi",
            "-lc",
            source_path,
            &format!("-femit-bin={}", &platform_obj),
        ])
        .output()
        .unwrap();

    platform_obj
}
