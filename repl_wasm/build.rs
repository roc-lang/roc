use std::env;
use std::fs;
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::Command;

use roc_builtins::bitcode;

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

    // Compile again to get libc path
    let (libc_archive, compiler_rt_obj) = build_wasm_libc_compilerrt(&out_dir, &source_path);
    let mut libc_pathbuf = PathBuf::from(&libc_archive);
    libc_pathbuf.pop();
    let libc_dir = libc_pathbuf.to_str().unwrap();

    let pre_linked_binary_path: PathBuf = PRE_LINKED_BINARY.iter().collect();

    Command::new(&zig_executable())
        .args([
            "wasm-ld",
            bitcode::BUILTINS_WASM32_OBJ_PATH,
            &platform_obj,
            compiler_rt_obj.to_str().unwrap(),
            "-L",
            libc_dir,
            "-lc",
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
