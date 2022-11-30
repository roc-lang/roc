use std::env;
use std::path::PathBuf;
use std::process::Command;

use roc_builtins::bitcode;
use wasi_libc_sys::{WASI_COMPILER_RT_PATH, WASI_LIBC_PATH};

const PLATFORM_FILENAME: &str = "repl_platform";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let source_path = format!("src/{}.c", PLATFORM_FILENAME);
    println!("cargo:rerun-if-changed={}", source_path);

    // Zig can produce *either* an object containing relocations OR an object containing libc code
    // But we want both, so we have to compile twice with different flags, then link them

    // Create an object file with relocations
    let out_dir = env::var("OUT_DIR").unwrap();
    let platform_obj = build_wasm_platform(&out_dir, &source_path);

    let mut pre_linked_binary_path = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    pre_linked_binary_path.extend(["pre_linked_binary"]);
    pre_linked_binary_path.set_extension("o");

    let builtins_host_tempfile =
        bitcode::host_wasm_tempfile().expect("failed to write host builtins object to tempfile");

    let output = Command::new(&zig_executable())
        .args([
            "wasm-ld",
            builtins_host_tempfile.path().to_str().unwrap(),
            platform_obj.to_str().unwrap(),
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

    // Extend the lifetime of the tempfile so it doesn't get dropped
    // (and thus deleted) before the Zig process is done using it!
    let _ = builtins_host_tempfile;

    assert!(output.status.success(), "{:#?}", output);
    assert!(output.stdout.is_empty(), "{:#?}", output);
    assert!(output.stderr.is_empty(), "{:#?}", output);
}

fn zig_executable() -> String {
    match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    }
}

fn build_wasm_platform(out_dir: &str, source_path: &str) -> PathBuf {
    let mut platform_obj = PathBuf::from(out_dir).join(PLATFORM_FILENAME);
    platform_obj.set_extension("o");

    Command::new(&zig_executable())
        .args([
            "build-lib",
            "-target",
            "wasm32-wasi",
            "-lc",
            source_path,
            &format!("-femit-bin={}", platform_obj.to_str().unwrap()),
        ])
        .output()
        .unwrap();

    platform_obj
}
