use roc_command_utils::zig;
use std::env;
use std::fs;
use std::path::Path;
use std::path::PathBuf;

use wasi_libc_sys::{WASI_COMPILER_RT_PATH, WASI_LIBC_PATH};

const PLATFORM_FILENAME: &str = "wasm_test_platform";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    if feature_is_enabled("gen-wasm") || feature_is_enabled("gen-llvm-wasm") {
        build_wasm_test_host();
        build_wasm_linking_test_host();
    }
}

const fn object_file_extension() -> &'static str {
    if cfg!(windows) {
        "obj"
    } else {
        "o"
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
    let host_native_path = PathBuf::from("build").join("wasm_linking_test_host");

    let host_source: &str = host_source_path.to_str().unwrap();
    let import_source: &str = import_source_path.to_str().unwrap();
    let host_wasm: &str = host_wasm_path.to_str().unwrap();
    let host_native: &str = host_native_path.to_str().unwrap();

    println!("cargo:rerun-if-changed={host_source}");
    println!("cargo:rerun-if-changed={import_source}");

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
        &format!("-femit-bin={host_wasm}"),
    ]);

    let mut import_obj_path = PathBuf::from("build").join("wasm_linking_host_imports");
    import_obj_path.set_extension(object_file_extension());
    let import_obj = import_obj_path.to_str().unwrap();
    run_zig(&[
        "build-obj",
        import_source,
        &format!("-femit-bin={}", &import_obj),
    ]);

    run_zig(&[
        "build-exe",
        host_source,
        import_obj,
        &format!("-femit-bin={host_native}"),
        #[cfg(windows)]
        "--subsystem",
        #[cfg(windows)]
        "console",
        #[cfg(windows)]
        "-lc",
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

    let builtins_host_tempfile = roc_bitcode::host_wasm_tempfile()
        .expect("failed to write host builtins object to tempfile");

    run_zig(&[
        "wasm-ld",
        builtins_host_tempfile.path().to_str().unwrap(),
        platform_path.to_str().unwrap(),
        WASI_COMPILER_RT_PATH,
        WASI_LIBC_PATH,
        "-o",
        outfile.to_str().unwrap(),
        "--no-entry",
        "--relocatable",
    ]);

    // Extend the lifetime of the tempfile so it doesn't get dropped
    // (and thus deleted) before the Zig process is done using it!
    let _ = builtins_host_tempfile;
}

fn build_wasm_platform(out_dir: &str, source_path: &str) -> PathBuf {
    let mut outfile = PathBuf::from(out_dir).join(PLATFORM_FILENAME);
    outfile.set_extension("wasm");

    run_zig(&[
        "build-lib",
        "-target",
        "wasm32-wasi-musl",
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
    let mut zig_cmd = zig();

    let full_zig_cmd = zig_cmd.args(args);
    println!("{full_zig_cmd:?}");

    let zig_cmd_output = full_zig_cmd.output().unwrap();

    if !zig_cmd_output.status.success() {
        eprintln!(
            "stdout:\n{}",
            String::from_utf8_lossy(&zig_cmd_output.stdout)
        );
        eprintln!(
            "stderr:\n{}",
            String::from_utf8_lossy(&zig_cmd_output.stderr)
        );
        panic!("zig call failed with status {:?}", zig_cmd_output.status);
    }

    assert!(zig_cmd_output.stdout.is_empty(), "{zig_cmd_output:#?}");
    assert!(zig_cmd_output.stderr.is_empty(), "{zig_cmd_output:#?}");
}
