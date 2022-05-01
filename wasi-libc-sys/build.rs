use std::env;
use std::fs;

// Environment variable that can be used from other build scripts
const WASI_LIBC_SYS_PATH: &str = "WASI_LIBC_SYS_PATH";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/dummy.c");

    let out_dir = env::var("OUT_DIR").unwrap();
    let zig_cache_dir = format!("{}/zig-cache", out_dir);
    let out_file = format!("{}/wasi-libc.a", out_dir);
    println!("cargo:rustc-env={}={}", WASI_LIBC_SYS_PATH, &out_file);

    // Compile a dummy C program with Zig, putting libc into our own private cache directory
    let args = [
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
    ];

    let zig = zig_executable();

    // println!("{} {}", zig, args.join(" "));

    run_command(Path::new("."), &zig, args);
    let zig_libc_path = run_command(Path::new("."), "find", [&zig_cache_dir, "-name", "libc.a"]);

    // Copy libc out of Zig's cache, to where Cargo expects it
    fs::copy(&zig_libc_path, &out_file);
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
