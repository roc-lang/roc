use roc_command_utils::{pretty_command_string, zig};
use roc_error_macros::internal_error;
use std::fs;
use std::io;
use std::path::Path;
use std::process::Command;
use std::str;

#[cfg(target_os = "macos")]
use tempfile::tempdir;

/// To debug the zig code with debug prints, we need to disable the wasm code gen
const DEBUG: bool = false;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    // "." is relative to where "build.rs" is
    // dunce can be removed once ziglang/zig#5109 is fixed
    let bitcode_path = dunce::canonicalize(Path::new(".")).unwrap().join("..");

    // workaround for github.com/ziglang/zig/issues/20501
    #[cfg(target_os = "macos")]
    let zig_cache_dir = tempdir().expect("Failed to create temp directory for zig cache");
    #[cfg(target_os = "macos")]
    std::env::set_var("ZIG_GLOBAL_CACHE_DIR", zig_cache_dir.path().as_os_str());

    // LLVM .bc FILES

    generate_bc_file(&bitcode_path, "ir", "builtins-host");

    if !DEBUG {
        generate_bc_file(&bitcode_path, "ir-wasm32", "builtins-wasm32");
    }

    generate_bc_file(&bitcode_path, "ir-x86", "builtins-x86");
    generate_bc_file(&bitcode_path, "ir-x86_64", "builtins-x86_64");
    generate_bc_file(&bitcode_path, "ir-aarch64", "builtins-aarch64");
    generate_bc_file(
        &bitcode_path,
        "ir-windows-x86_64",
        "builtins-windows-x86_64",
    );

    get_zig_files(bitcode_path.as_path(), &|path| {
        let path: &Path = path;
        println!(
            "cargo:rerun-if-changed={}",
            path.to_str().expect("Failed to convert path to str")
        );
    })
    .unwrap();

    #[cfg(target_os = "macos")]
    zig_cache_dir
        .close()
        .expect("Failed to delete temp dir zig_cache_dir.");
}

fn generate_bc_file(bitcode_path: &Path, zig_object: &str, file_name: &str) {
    let mut ll_path = bitcode_path.join("zig-out").join(file_name);
    ll_path.set_extension("ll");
    let dest_ir_host = ll_path.to_str().expect("Invalid dest ir path");

    println!("Compiling host ir to: {dest_ir_host}");

    let mut bc_path = bitcode_path.join("zig-out").join(file_name);
    bc_path.set_extension("bc");
    let dest_bc_64bit = bc_path.to_str().expect("Invalid dest bc path");
    println!("Compiling 64-bit bitcode to: {dest_bc_64bit}");

    // workaround for github.com/ziglang/zig/issues/20501
    #[cfg(target_os = "macos")]
    let _ = fs::remove_dir_all("./.zig-cache");

    let mut zig_cmd = zig();

    zig_cmd
        .current_dir(bitcode_path)
        .args(["build", zig_object, "-Drelease=true"]);

    run_command(zig_cmd, 0);
}

fn run_command(mut command: Command, flaky_fail_counter: usize) {
    let command_str = pretty_command_string(&command);
    let command_str = command_str.to_string_lossy();

    let output_result = command.output();

    match output_result {
        Ok(output) => match output.status.success() {
            true => (),
            false => {
                let error_str = match str::from_utf8(&output.stderr) {
                    Ok(stderr) => stderr.to_string(),
                    Err(_) => format!("Failed to run \"{command_str}\""),
                };

                // Flaky test errors that only occur sometimes on MacOS ci server.
                if error_str.contains("FileNotFound")
                    || error_str.contains("unable to save cached ZIR code")
                    || error_str.contains("LLVM failed to emit asm")
                {
                    if flaky_fail_counter == 10 {
                        internal_error!("{} failed 10 times in a row. The following error is unlikely to be a flaky error: {}", command_str, error_str);
                    } else {
                        run_command(command, flaky_fail_counter + 1)
                    }
                } else if error_str
                    .contains("lld-link: error: failed to write the output file: Permission denied")
                {
                    internal_error!("{} failed with:\n\n  {}\n\nWorkaround:\n\n  Re-run the cargo command that triggered this build.\n\n", command_str, error_str);
                } else {
                    internal_error!("{} failed with:\n\n  {}\n", command_str, error_str);
                }
            }
        },
        Err(reason) => internal_error!("{} failed: {}", command_str, reason),
    }
}

fn get_zig_files(dir: &Path, cb: &dyn Fn(&Path)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path_buf = entry.path();
            if path_buf.is_dir() {
                if !path_buf.ends_with(".zig-cache") {
                    get_zig_files(&path_buf, cb).unwrap();
                }
            } else {
                let path = path_buf.as_path();

                match path.extension() {
                    Some(osstr) if osstr == "zig" => {
                        cb(path);
                    }
                    _ => {}
                }
            }
        }
    }
    Ok(())
}
