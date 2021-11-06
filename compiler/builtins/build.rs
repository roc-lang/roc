use std::convert::AsRef;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io;
use std::path::Path;
use std::process::Command;
use std::str;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();

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

    generate_64bit_native(&bitcode_path, &build_script_dir_path);
    generate_wasm32(&bitcode_path, &build_script_dir_path);
    generate_i386(&bitcode_path, &build_script_dir_path);

    {
        let dest_obj_path = Path::new(&out_dir).join("builtins.o");
        let dest_obj = dest_obj_path.to_str().expect("Invalid dest object path");

        println!("cargo:rustc-env=BUILTINS_O={}", dest_obj);

        generate_64bit_dev_backend_object(&bitcode_path, dest_obj);
    }

    {
        let dest_obj_path = Path::new(&out_dir).join("builtins-wasm32.o");
        let dest_obj = dest_obj_path.to_str().expect("Invalid dest object path");

        println!("cargo:rustc-env=BUILTINS_WASM32_O={}", dest_obj);

        generate_wasm32_dev_backend_object(&bitcode_path, dest_obj);
    }

    get_zig_files(bitcode_path.as_path(), &|path| {
        let path: &Path = path;
        println!(
            "cargo:rerun-if-changed={}",
            path.to_str().expect("Failed to convert path to str")
        );
    })
    .unwrap();
}

fn generate_64bit_dev_backend_object(bitcode_path: &Path, dest_obj: &str) {
    let src_obj_path = bitcode_path.join("builtins-host.o");
    let src_obj = src_obj_path.to_str().expect("Invalid src object path");

    println!("Compiling zig object to: {}", src_obj);
    run_command(&bitcode_path, "zig", &["build", "object", "-Drelease=true"]);

    println!("Moving zig object to: {}", dest_obj);

    run_command(&bitcode_path, "mv", &[src_obj, dest_obj]);
}

fn generate_wasm32_dev_backend_object(bitcode_path: &Path, dest_obj: &str) {
    let src_obj_path = bitcode_path.join("builtins-wasm32.o");
    let src_obj = src_obj_path.to_str().expect("Invalid src object path");

    println!("Compiling zig object to: {}", src_obj);
    run_command(
        &bitcode_path,
        "zig",
        &["build", "wasm32-object", "-Drelease=true"],
    );

    println!("Moving zig object to: {}", dest_obj);

    run_command(&bitcode_path, "mv", &[src_obj, dest_obj]);
}

fn generate_64bit_native(bitcode_path: &Path, build_script_dir_path: &Path) {
    let dest_ir_path = bitcode_path.join("builtins-host.ll");
    let dest_ir_host = dest_ir_path.to_str().expect("Invalid dest ir path");

    println!("Compiling host ir to: {}", dest_ir_host);
    run_command(&bitcode_path, "zig", &["build", "ir", "-Drelease=true"]);

    let dest_bc_path = bitcode_path.join("builtins-host.bc");
    let dest_bc_64bit = dest_bc_path.to_str().expect("Invalid dest bc path");
    println!("Compiling 64-bit bitcode to: {}", dest_bc_64bit);

    run_command(
        &build_script_dir_path,
        "llvm-as",
        &[dest_ir_host, "-o", dest_bc_64bit],
    );
}

fn generate_i386(bitcode_path: &Path, build_script_dir_path: &Path) {
    let dest_ir_path = bitcode_path.join("builtins-i386.ll");
    let dest_ir_i386 = dest_ir_path.to_str().expect("Invalid dest ir path");

    println!("Compiling 32-bit i386 ir to: {}", dest_ir_i386);
    run_command(
        &bitcode_path,
        "zig",
        &["build", "ir-i386", "-Drelease=true"],
    );

    let dest_bc_path = bitcode_path.join("builtins-i386.bc");
    let dest_bc_32bit = dest_bc_path.to_str().expect("Invalid dest bc path");
    println!("Compiling 32-bit bitcode to: {}", dest_bc_32bit);

    run_command(
        &build_script_dir_path,
        "llvm-as",
        &[dest_ir_i386, "-o", dest_bc_32bit],
    );
}

fn generate_wasm32(bitcode_path: &Path, build_script_dir_path: &Path) {
    let dest_ir_path = bitcode_path.join("builtins-wasm32.ll");
    let dest_ir_wasm32 = dest_ir_path.to_str().expect("Invalid dest ir path");

    println!("Compiling 32-bit wasm32 ir to: {}", dest_ir_wasm32);
    run_command(
        &bitcode_path,
        "zig",
        &["build", "ir-wasm32", "-Drelease=true"],
    );

    let dest_bc_path = bitcode_path.join("builtins-wasm32.bc");
    let dest_bc_32bit = dest_bc_path.to_str().expect("Invalid dest bc path");
    println!("Compiling 32-bit bitcode to: {}", dest_bc_32bit);

    run_command(
        &build_script_dir_path,
        "llvm-as",
        &[dest_ir_wasm32, "-o", dest_bc_32bit],
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

// fn get_zig_files(dir: &Path) -> io::Result<Vec<&Path>> {
// let mut vec = Vec::new();
// if dir.is_dir() {
// for entry in fs::read_dir(dir)? {
// let entry = entry?;
// let path_buf = entry.path();
// if path_buf.is_dir() {
// match get_zig_files(&path_buf) {
// Ok(sub_files) => vec = [vec, sub_files].concat(),
// Err(_) => (),
// };
// } else {
// let path = path_buf.as_path();
// let path_ext = path.extension().unwrap();
// if path_ext == "zig" {
// vec.push(path.clone());
// }
// }
// }
// }
// Ok(vec)
// }
