use std::convert::AsRef;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io;
use std::path::Path;
use std::process::Command;
use std::str;

fn zig_executable() -> String {
    match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    }
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    // When we build on Netlify, zig is not installed (but also not used,
    // since all we're doing is generating docs), so we can skip the steps
    // that require having zig installed.
    if env::var_os("NO_ZIG_INSTALLED").is_some() {
        // We still need to do the other things before this point, because
        // setting the env vars is needed for other parts of the build.
        return;
    }

    // "." is relative to where "build.rs" is
    let build_script_dir_path = fs::canonicalize(Path::new(".")).unwrap();
    let bitcode_path = build_script_dir_path.join("bitcode");

    // LLVM .bc FILES

    generate_bc_file(&bitcode_path, &build_script_dir_path, "ir", "builtins-host");

    generate_bc_file(
        &bitcode_path,
        &build_script_dir_path,
        "ir-wasm32",
        "builtins-wasm32",
    );

    generate_bc_file(
        &bitcode_path,
        &build_script_dir_path,
        "ir-i386",
        "builtins-i386",
    );

    // OBJECT FILES

    generate_object_file(
        &bitcode_path,
        "BUILTINS_HOST_O",
        "object",
        "builtins-host.o",
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

    run_command(
        &bitcode_path,
        &zig_executable(),
        &["build", zig_object, "-Drelease=true"],
    );

    println!("Moving zig object `{}` to: {}", zig_object, dest_obj);

    // we store this .o file in rust's `target` folder
    run_command(&bitcode_path, "mv", &[src_obj, dest_obj]);
}

fn generate_bc_file(
    bitcode_path: &Path,
    build_script_dir_path: &Path,
    zig_object: &str,
    file_name: &str,
) {
    let mut ll_path = bitcode_path.join(file_name);
    ll_path.set_extension("ll");
    let dest_ir_host = ll_path.to_str().expect("Invalid dest ir path");

    println!("Compiling host ir to: {}", dest_ir_host);

    run_command(
        &bitcode_path,
        &zig_executable(),
        &["build", zig_object, "-Drelease=true"],
    );

    let mut bc_path = bitcode_path.join(file_name);
    bc_path.set_extension("bc");
    let dest_bc_64bit = bc_path.to_str().expect("Invalid dest bc path");

    println!("Compiling 64-bit bitcode to: {}", dest_bc_64bit);

    run_command(
        &build_script_dir_path,
        "llvm-as",
        &[dest_ir_host, "-o", dest_bc_64bit],
    );
}

fn run_command<S, I, P: AsRef<Path>>(path: P, command_str: &str, args: I)
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
                panic!("{} failed: {}", command_str, error_str);
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
