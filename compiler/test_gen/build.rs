use roc_builtins::bitcode;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

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

    // Zig can produce *either* an object containing relocations OR an object containing libc code
    // But we want both, so we have to compile twice with different flags, then link them

    // Create an object file with relocations
    let platform_path = build_wasm_platform(&out_dir, &source_path);

    // Compile again to get libc path
    let (mut libc_pathbuf, compiler_rt_pathbuf) =
        build_wasm_libc_compilerrt(&out_dir, &source_path);
    libc_pathbuf.pop();
    let libc_dir = libc_pathbuf.to_str().unwrap();

    let args = &[
        "wasm-ld",
        bitcode::BUILTINS_WASM32_OBJ_PATH,
        &platform_path,
        compiler_rt_pathbuf.to_str().unwrap(),
        "-L",
        libc_dir,
        "-lc",
        "-o",
        &format!("{}/{}.o", out_dir, PLATFORM_FILENAME),
        "--no-entry",
        "--relocatable",
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

fn build_wasm_libc_compilerrt(out_dir: &str, source_path: &str) -> (PathBuf, PathBuf) {
    let zig_cache_dir = format!("{}/zig-cache-wasm32", out_dir);
    let zig_cache_path = PathBuf::from(&zig_cache_dir);

    Command::new(&zig_executable())
        .args([
            "build-lib",
            "-dynamic", // ensure libc code is actually generated (not just linked against header)
            "-target",
            "wasm32-wasi",
            "-lc",
            source_path,
            "-femit-bin=/dev/null",
            "--global-cache-dir",
            &zig_cache_dir,
        ])
        .output()
        .unwrap();

    let libc_path = find(&zig_cache_path, &OsString::from("libc.a"))
        .unwrap()
        .unwrap();

    let compiler_rt_path = find(&zig_cache_path, &OsString::from("compiler_rt.o"))
        .unwrap()
        .unwrap();

    (libc_path, compiler_rt_path)
}

fn find(dir: &Path, filename: &OsString) -> std::io::Result<Option<PathBuf>> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            let found = find(&path, filename)?;
            if found.is_some() {
                return Ok(found);
            }
        } else if &entry.file_name() == filename {
            return Ok(Some(path));
        }
    }
    Ok(None)
}

fn feature_is_enabled(feature_name: &str) -> bool {
    let cargo_env_var = format!(
        "CARGO_FEATURE_{}",
        feature_name.replace('-', "_").to_uppercase()
    );
    env::var(cargo_env_var).is_ok()
}
