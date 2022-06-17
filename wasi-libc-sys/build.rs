use std::env;
use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use std::process::Command;

use roc_utils::{path_to_str, zig_cache_find};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/dummy.c");

    let out_dir_str = env::var("OUT_DIR").unwrap();
    let out_dir = Path::new(&out_dir_str);
    let zig_cache_dir = out_dir.join("zig-cache");
    let zig_cache_dir_o = zig_cache_dir.join("o");// the files we need are in the o subdir
    let out_file = out_dir.join("wasi-libc.a");

    // Compile a dummy C program with Zig, with our own private cache directory
    let zig = zig_executable();
    run_command(
        Path::new("."),
        &zig,
        [
            "build-exe",
            "-target",
            "wasm32-wasi",
            "-lc",
            "-O",
            "ReleaseSmall",
            "--global-cache-dir",
            path_to_str(&zig_cache_dir),
            path_to_str(&Path::new("src").join("dummy.c")),
            &format!("-femit-bin={}", path_to_str(&out_dir.join("dummy.wasm"))),
        ],
    );

    // Find the libc.a and compiler_rt.o files that Zig wrote (as a side-effect of compiling the dummy program)
    let zig_libc_path = zig_cache_find(path_to_str(&zig_cache_dir_o), "libc.a");
    let zig_crt_path = zig_cache_find(path_to_str(&zig_cache_dir_o), "compiler_rt.o");

    // Copy libc to where Cargo expects the output of this crate
    fs::copy(&zig_libc_path, &out_file).unwrap();

    // Generate some Rust code to indicate where the file is
    let generated_rust = [
        "pub const WASI_LIBC_PATH: &str =",
        &format!("    \"{}\";", path_to_str(&out_file).replace("\\", "\\\\")),
        "pub const WASI_COMPILER_RT_PATH: &str =",
        &format!("  \"{}\";", zig_crt_path.replace("\\", "\\\\")),
        "",
    ]
    .join("\n");

    fs::write("src/generated.rs", generated_rust).unwrap();
}

fn zig_executable() -> String {
    match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    }
}

fn run_command<S, I, P: AsRef<Path>>(path: P, command_str: &str, args: I) -> String
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output_result = Command::new(OsStr::new(&command_str))
        .current_dir(path)
        .args(args)
        .output();
    match output_result {
        Ok(output) => match output.status.success() {
            true => std::str::from_utf8(&output.stdout).unwrap().to_string(),
            false => {
                let error_str = match std::str::from_utf8(&output.stderr) {
                    Ok(stderr) => stderr.to_string(),
                    Err(_) => format!("Failed to run \"{}\"", command_str),
                };
                panic!("{} failed: {}", command_str, error_str);
            }
        },
        Err(reason) => panic!("{} failed: {}", command_str, reason),
    }
}
