use std::env;
use std::ffi::OsStr;
use std::path::Path;
use std::process::Command;

use roc_builtins::bitcode;

const PLATFORM_FILENAME: &str = "repl_platform";
const PRE_LINKED_BINARY: &str = "data/pre_linked_binary.o";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/{}.c", PLATFORM_FILENAME);

    // When we build on Netlify, zig is not installed (but also not used,
    // since all we're doing is generating docs), so we can skip the steps
    // that require having zig installed.
    if env::var_os("NO_ZIG_INSTALLED").is_some() {
        // We still need to do the other things before this point, because
        // setting the env vars is needed for other parts of the build.
        return;
    }

    std::fs::create_dir_all("./data").unwrap();

    // Build a pre-linked binary with platform, builtins and all their libc dependencies
    // This builds a library file that exports all symbols. It has no linker data but we don't need it.
    // See discussion with Luuk de Gram (Zig contributor)
    // https://github.com/rtfeldman/roc/pull/2181#pullrequestreview-839608063
    let args = [
        "build-lib",
        "-target",
        "wasm32-wasi",
        "-lc",
        "-dynamic", // -dynamic ensures libc code goes into the binary
        bitcode::BUILTINS_WASM32_OBJ_PATH,
        &format!("src/{}.c", PLATFORM_FILENAME),
        &format!("-femit-bin={}", PRE_LINKED_BINARY),
    ];

    let zig = zig_executable();

    // println!("{} {}", zig, args.join(" "));

    run_command(Path::new("."), &zig, args);
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
