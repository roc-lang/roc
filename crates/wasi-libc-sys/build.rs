use roc_command_utils::zig;
use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/dummy.c");

    // out_dir == "<project_root>/target/[<platform>/]<profile>/build/wasi_libc_sys_<hex>/"
    let out_dir = env::var("OUT_DIR").unwrap();
    let zig_cache_dir = PathBuf::from(&out_dir).join(".zig-cache");

    // out_file == "<project_root>/target/[<platform>/]<profile>/lib/wasi-libc.a"
    let mut out_file = PathBuf::from(&out_dir);
    out_file.pop();
    out_file.pop();
    out_file.pop();
    out_file.push("lib");
    out_file.push("wasi-libc.a");

    // Compile a dummy C program with Zig, with our own private cache directory
    zig()
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
            &format!("-femit-bin={out_dir}/dummy.wasm"),
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
    fs::create_dir_all(out_file.parent().unwrap()).unwrap();
    fs::copy(libc_path, &out_file).unwrap();

    println!(
        "cargo:rustc-env=WASI_LIBC_PATH={}",
        out_file.to_str().unwrap()
    );

    println!(
        "cargo:rustc-env=WASI_COMPILER_RT_PATH={}",
        compiler_rt_path.to_str().unwrap()
    );
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
