use std::env;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;

use roc_builtins::bitcode;

const PLATFORM_FILENAME: &str = "repl_platform";
const PRE_LINKED_BINARY: &str = "data/pre_linked_binary.o";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let source_path = format!("src/{}.c", PLATFORM_FILENAME);
    println!("cargo:rerun-if-changed={}", source_path);

    std::fs::create_dir_all("./data").unwrap();

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

    let args = &[
        "wasm-ld",
        bitcode::BUILTINS_WASM32_OBJ_PATH,
        &platform_obj,
        &compiler_rt_obj,
        "-L",
        libc_dir,
        "-lc",
        "-o",
        PRE_LINKED_BINARY,
        "--export-all",
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
    let platform_obj = format!("{}/{}.o", out_dir, PLATFORM_FILENAME);

    run_command(
        &zig_executable(),
        &[
            "build-lib",
            "-target",
            "wasm32-wasi",
            "-lc",
            source_path,
            &format!("-femit-bin={}", &platform_obj),
        ],
    );

    platform_obj
}

fn build_wasm_libc_compilerrt(out_dir: &str, source_path: &str) -> (String, String) {
    let zig_cache_dir = format!("{}/zig-cache-wasm32", out_dir);

    run_command(
        &zig_executable(),
        &[
            "build-lib",
            "-dynamic", // ensure libc code is actually generated (not just linked against header)
            "-target",
            "wasm32-wasi",
            "-lc",
            source_path,
            "-femit-bin=/dev/null",
            "--global-cache-dir",
            &zig_cache_dir,
        ],
    );

    (
        run_command("find", &[&zig_cache_dir, "-name", "libc.a"]),
        run_command("find", &[&zig_cache_dir, "-name", "compiler_rt.o"]),
    )
}

fn run_command(command_str: &str, args: &[&str]) -> String {
    let output_result = Command::new(OsStr::new(&command_str))
        .current_dir(Path::new("."))
        .args(args)
        .output();

    let fail = |err: String| {
        panic!(
            "\n\nFailed command:\n\t{} {}\n\n{}",
            command_str,
            args.join(" "),
            err
        );
    };

    match output_result {
        Ok(output) => match output.status.success() {
            true => std::str::from_utf8(&output.stdout)
                .unwrap()
                .trim()
                .to_string(),
            false => {
                let error_str = match std::str::from_utf8(&output.stderr) {
                    Ok(stderr) => stderr.to_string(),
                    Err(_) => format!("Failed to run \"{}\"", command_str),
                };
                fail(error_str)
            }
        },
        Err(reason) => fail(reason.to_string()),
    }
}
