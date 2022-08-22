use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/dummy.c");

    let out_dir = env::var("OUT_DIR").unwrap();
    let zig_cache_dir = PathBuf::from(&out_dir).join("zig-cache");
    let out_file = PathBuf::from(&out_dir).join("wasi-libc.a");

    // Compile a dummy C program with Zig, with our own private cache directory
    Command::new(&zig_executable())
        .args([
            "build-exe",
            "-target",
            "wasm32-wasi",
            "-lc",
            "-O",
            "ReleaseSmall",
            "--global-cache-dir",
            zig_cache_dir.to_str().unwrap(),
            "src/dummy.c",
            &format!("-femit-bin={}/dummy.wasm", out_dir),
        ])
        .output()
        .unwrap();

    let libc_path = find(&zig_cache_dir, &OsString::from("libc.a"))
        .unwrap()
        .unwrap();

    let compiler_rt_path = find(&zig_cache_dir, &OsString::from("libcompiler_rt.a"))
        .unwrap()
        .unwrap();

    // Copy libc to where Cargo expects the output of this crate
    fs::copy(&libc_path, &out_file).unwrap();

    println!(
        "cargo:rustc-env=WASI_LIBC_PATH={}",
        out_file.to_str().unwrap()
    );

    println!(
        "cargo:rustc-env=WASI_COMPILER_RT_PATH={}",
        compiler_rt_path.to_str().unwrap()
    );
}

fn zig_executable() -> String {
    match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    }
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
