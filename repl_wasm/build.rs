use std::env;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;

use roc_builtins::bitcode;
use roc_utils::{path_to_str, zig_cache_find};

const PLATFORM_FILENAME: &str = "repl_platform";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let source_path = Path::new("src").join(format!("{}.c", PLATFORM_FILENAME));
    println!("cargo:rerun-if-changed={}", path_to_str(&source_path));

    std::fs::create_dir_all(Path::new(".").join("data")).unwrap();

    // Zig can produce *either* an object containing relocations OR an object containing libc code
    // But we want both, so we have to compile twice with different flags, then link them

    // Create an object file with relocations
    let out_dir_str = env::var("OUT_DIR").unwrap();
    let out_dir = Path::new(&out_dir_str);
    let platform_obj = build_wasm_platform(&out_dir, &source_path);

    // Compile again to get libc path
    let (libc_archive, compiler_rt_obj) = build_wasm_libc_compilerrt(&out_dir, &source_path);
    let mut libc_pathbuf = PathBuf::from(&libc_archive);
    libc_pathbuf.pop();
    let libc_dir = libc_pathbuf.to_str().unwrap();

    let pre_linked_binary_path = Path::new("data").join("pre_linked_binary.o");

    let args = &[
        "wasm-ld",
        bitcode::BUILTINS_WASM32_OBJ_PATH,
        &platform_obj,
        &compiler_rt_obj,
        "-L",
        libc_dir,
        "-lc",
        "-o",
        path_to_str(&pre_linked_binary_path),
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

fn build_wasm_platform(out_dir: &Path, source_path: &Path) -> String {
    let platform_obj_path = out_dir.join(format!("{}.o", PLATFORM_FILENAME));
    let platform_obj_path_str = path_to_str(&platform_obj_path);

    run_command(
        &zig_executable(),
        &[
            "build-lib",
            "-target",
            "wasm32-wasi",
            "-lc",
            path_to_str(source_path),
            &format!("-femit-bin={}", platform_obj_path_str),
        ],
    );

    platform_obj_path_str.to_string()
}

fn build_wasm_libc_compilerrt(out_dir: &Path, source_path: &Path) -> (String, String) {
    let zig_cache_dir = out_dir.join("zig-cache-wasm32");
    #[cfg(unix)]
    let the_void = "/dev/null";
    #[cfg(windows)]
    let the_void = "NUL";

    run_command(
        &zig_executable(),
        &[
            "build-lib",
            "-dynamic", // ensure libc code is actually generated (not just linked against header)
            "-target",
            "wasm32-wasi",
            "-lc",
            path_to_str(source_path),
            &format!("-femit-bin={}", the_void),
            "--global-cache-dir",
            &path_to_str(&zig_cache_dir),
        ],
    );

    // the files we need are in the o subdir
    let zig_cache_dir_o = zig_cache_dir.join("o");
    (
        zig_cache_find(&path_to_str(&zig_cache_dir_o), "libc.a"),
        zig_cache_find(&path_to_str(&zig_cache_dir_o), "compiler_rt.o"),
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
