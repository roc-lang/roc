use std::env;
use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/dummy.c");

    let out_dir = env::var("OUT_DIR").unwrap();
    let zig_cache_dir = format!("{out_dir}/zig-cache");
    let out_file = format!("{out_dir}/wasi-libc.a");

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
            &zig_cache_dir,
            "src/dummy.c",
            &format!("-femit-bin={}/dummy.wasm", out_dir),
        ],
    );

    // Find the libc.a and compiler_rt.o files that Zig wrote (as a side-effect of compiling the dummy program)
    let cwd = std::env::current_dir().unwrap();
    let find_libc_output = run_command(&cwd, "find", [&zig_cache_dir, "-name", "libc.a"]);
    // If `find` printed multiple results, take the first.
    let zig_libc_path = find_libc_output.split('\n').next().unwrap();

    let find_crt_output = run_command(&cwd, "find", [&zig_cache_dir, "-name", "compiler_rt.o"]);
    // If `find` printed multiple results, take the first.
    let zig_crt_path = find_crt_output.split('\n').next().unwrap();

    // Copy libc to where Cargo expects the output of this crate
    fs::copy(&zig_libc_path, &out_file).unwrap();

    // Generate some Rust code to indicate where the file is
    let generated_rust = [
        "pub const WASI_LIBC_PATH: &str =",
        &format!("    \"{}\";", out_file),
        "pub const WASI_COMPILER_RT_PATH: &str =",
        &format!("  \"{}\";", zig_crt_path),
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
