use std::convert::AsRef;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
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
    generate_bc_file(
        &bitcode_path,
        "ir-windows-x86_64",
        "builtins-windows-x86_64",
    );

    // OBJECT FILES
    #[cfg(windows)]
    const BUILTINS_HOST_FILE: &str = "builtins-host.obj";

    #[cfg(not(windows))]
    const BUILTINS_HOST_FILE: &str = "builtins-host.o";

    generate_object_file(&bitcode_path, "object", BUILTINS_HOST_FILE);

    generate_object_file(
        &bitcode_path,
        "windows-x86_64-object",
        "builtins-windows-x86_64.obj",
    );

    generate_object_file(&bitcode_path, "wasm32-object", "builtins-wasm32.o");

    copy_zig_builtins_to_target_dir(&bitcode_path);

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

fn generate_object_file(bitcode_path: &Path, zig_object: &str, object_file_name: &str) {
    let dest_obj_path = get_lib_dir().join(object_file_name);
    let dest_obj = dest_obj_path.to_str().expect("Invalid dest object path");

    let src_obj_path = bitcode_path.join(object_file_name);
    let src_obj = src_obj_path.to_str().expect("Invalid src object path");

    println!("Compiling zig object `{}` to: {}", zig_object, src_obj);

    if !DEBUG {
        run_command(
            &bitcode_path,
            &zig_executable(),
            &["build", zig_object, "-Drelease=true"],
            0,
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
        0,
    );
}

pub fn get_lib_dir() -> PathBuf {
    // Currently we have the OUT_DIR variable which points to `/target/debug/build/roc_builtins-*/out/`.
    // So we just need to shed a 3 of the outer layers to get `/target/debug/` and then add `lib`.
    let out_dir = env::var_os("OUT_DIR").unwrap();

    let lib_path = Path::new(&out_dir)
        .parent()
        .and_then(|path| path.parent())
        .and_then(|path| path.parent())
        .unwrap()
        .join("lib");

    // create dir of it does not exist
    fs::create_dir_all(lib_path.clone()).expect("Failed to make lib dir.");

    lib_path
}

fn copy_zig_builtins_to_target_dir(bitcode_path: &Path) {
    // To enable roc to find the zig biultins, we want them to be moved to a folder next to the roc executable.
    // So if <roc_folder>/roc is the executable. The zig files will be in <roc_folder>/lib/*.zig
    let target_profile_dir = get_lib_dir();

    let zig_src_dir = bitcode_path.join("src");

    cp_unless_zig_cache(&zig_src_dir, &target_profile_dir).unwrap_or_else(|err| {
        panic!(
            "Failed to copy zig bitcode files {:?} to {:?}: {:?}",
            zig_src_dir, target_profile_dir, err
        );
    });
}

// recursively copy all the .zig files from this directory, but do *not* recurse into zig-cache/
fn cp_unless_zig_cache(src_dir: &Path, target_dir: &Path) -> io::Result<()> {
    // Make sure the destination directory exists before we try to copy anything into it.
    std::fs::create_dir_all(&target_dir).unwrap_or_else(|err| {
        panic!(
            "Failed to create output library directory for zig bitcode {:?}: {:?}",
            target_dir, err
        );
    });

    for entry in fs::read_dir(src_dir)? {
        let src_path = entry?.path();
        let src_filename = src_path.file_name().unwrap();

        // Only copy individual files if they have the .zig extension
        if src_path.extension().unwrap_or_default() == "zig" {
            let dest = target_dir.join(src_filename);

            fs::copy(&src_path, &dest).unwrap_or_else(|err| {
                panic!(
                    "Failed to copy zig bitcode file {:?} to {:?}: {:?}",
                    src_path, dest, err
                );
            });
        } else if src_path.is_dir() && src_filename != "zig-cache" {
            // Recursively copy all directories except zig-cache
            cp_unless_zig_cache(&src_path, &target_dir.join(src_filename))?;
        }
    }

    Ok(())
}

fn run_command<S, I: Copy, P: AsRef<Path> + Copy>(
    path: P,
    command_str: &str,
    args: I,
    flaky_fail_counter: usize,
) where
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
                if error_str.contains("FileNotFound")
                    || error_str.contains("unable to save cached ZIR code")
                {
                    if flaky_fail_counter == 10 {
                        panic!("{} failed 10 times in a row. The following error is unlikely to be a flaky error: {}", command_str, error_str);
                    } else {
                        run_command(path, command_str, args, flaky_fail_counter + 1)
                    }
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
