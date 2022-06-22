use std::convert::AsRef;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io;
use std::path::Path;
use std::process::Command;
use std::str;

#[cfg(target_os = "macos")]
use tempfile::tempdir;

/// To debug the zig code with debug prints, we need to disable the wasm code gen
const DEBUG: bool = false;

fn zig_executable() -> String {
    match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    }
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    // "." is relative to where "build.rs" is
    // dunce can be removed once ziglang/zig#5109 is fixed
    let build_script_dir_path = dunce::canonicalize(Path::new(".")).unwrap();
    let bitcode_path = build_script_dir_path.join("bitcode");

    // workaround for github.com/ziglang/zig/issues/9711
    #[cfg(target_os = "macos")]
    let zig_cache_dir = tempdir().expect("Failed to create temp directory for zig cache");
    #[cfg(target_os = "macos")]
    std::env::set_var("ZIG_GLOBAL_CACHE_DIR", zig_cache_dir.path().as_os_str());

    // LLVM .bc FILES

    generate_bc_file(&bitcode_path, "ir", "builtins-host");

    if !DEBUG {
        generate_bc_file(&bitcode_path, "ir-wasm32", "builtins-wasm32");
    }

    generate_bc_file(&bitcode_path, "ir-i386", "builtins-i386");

    generate_bc_file(&bitcode_path, "ir-x86_64", "builtins-x86_64");

    // OBJECT FILES
    #[cfg(windows)]
    const BUILTINS_HOST_FILE: &str = "builtins-host.obj";

    #[cfg(not(windows))]
    const BUILTINS_HOST_FILE: &str = "builtins-host.o";

    generate_object_file(
        &bitcode_path,
        "BUILTINS_HOST_O",
        "object",
        BUILTINS_HOST_FILE,
    );

    generate_object_file(
        &bitcode_path,
        "BUILTINS_WASM32_O",
        "wasm32-object",
        "builtins-wasm32.o",
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

fn generate_object_file(
    bitcode_path: &Path,
    env_var_name: &str,
    zig_object: &str,
    object_file_name: &str,
) {
    let out_dir = env::var_os("OUT_DIR").unwrap();

    let dest_obj_path = Path::new(&out_dir).join(object_file_name);
    let dest_obj = dest_obj_path.to_str().expect("Invalid dest object path");

    // set the variable (e.g. BUILTINS_HOST_O) that is later used in
    // `compiler/builtins/src/bitcode.rs` to load the object file
    println!("cargo:rustc-env={}={}", env_var_name, dest_obj);

    let src_obj_path = bitcode_path.join(object_file_name);
    let src_obj = src_obj_path.to_str().expect("Invalid src object path");

    println!("Compiling zig object `{}` to: {}", zig_object, src_obj);

    if !DEBUG {
        run_command(
            &bitcode_path,
            &zig_executable(),
            &["build", zig_object, "-Drelease=true"],
        );

        println!("Moving zig object `{}` to: {}", zig_object, dest_obj);

        // we store this .o file in rust's `target` folder (for wasm we need to leave a copy here too)
        fs::copy(src_obj, dest_obj).unwrap_or_else(|err| {
            panic!(
                "Failed to copy object file {} to {}: {:?}",
                src_obj, dest_obj, err
            );
        });
    }
}

fn generate_bc_file(bitcode_path: &Path, zig_object: &str, file_name: &str) {
    let mut ll_path = bitcode_path.join(file_name);
    ll_path.set_extension("ll");
    let dest_ir_host = ll_path.to_str().expect("Invalid dest ir path");

    println!("Compiling host ir to: {}", dest_ir_host);

    let mut bc_path = bitcode_path.join(file_name);
    bc_path.set_extension("bc");
    let dest_bc_64bit = bc_path.to_str().expect("Invalid dest bc path");
    println!("Compiling 64-bit bitcode to: {}", dest_bc_64bit);

    // workaround for github.com/ziglang/zig/issues/9711
    #[cfg(target_os = "macos")]
    let _ = fs::remove_dir_all("./bitcode/zig-cache");

    run_command(
        &bitcode_path,
        &zig_executable(),
        &["build", zig_object, "-Drelease=true"],
    );
}

fn run_command<S, I: Copy, P: AsRef<Path> + Copy>(path: P, command_str: &str, args: I)
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
            true => (),
            false => {
                let error_str = match str::from_utf8(&output.stderr) {
                    Ok(stderr) => stderr.to_string(),
                    Err(_) => format!("Failed to run \"{}\"", command_str),
                };

                // flaky test error that only occurs sometimes inside MacOS ci run
                if error_str.contains("unable to build stage1 zig object: FileNotFound")
                    || error_str.contains("unable to save cached ZIR code")
                {
                    run_command(path, command_str, args)
                } else {
                    panic!("{} failed: {}", command_str, error_str);
                }
            }
        },
        Err(reason) => panic!("{} failed: {}", command_str, reason),
    }
}

fn get_zig_files(dir: &Path, cb: &dyn Fn(&Path)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path_buf = entry.path();
            if path_buf.is_dir() {
                if !path_buf.ends_with("zig-cache") {
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
