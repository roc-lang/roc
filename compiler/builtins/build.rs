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
    let dest_obj_path = Path::new(&out_dir).join("builtins.o");
    let dest_obj = dest_obj_path.to_str().expect("Invalid dest object path");

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rustc-env=BUILTINS_O={}", dest_obj);

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

    let src_obj_path = bitcode_path.join("builtins-64bit.o");
    let src_obj = src_obj_path.to_str().expect("Invalid src object path");

    let dest_ir_path = bitcode_path.join("builtins-32bit.ll");
    let dest_ir_32bit = dest_ir_path.to_str().expect("Invalid dest ir path");

    let dest_ir_path = bitcode_path.join("builtins-64bit.ll");
    let dest_ir_64bit = dest_ir_path.to_str().expect("Invalid dest ir path");

    println!("Compiling zig object to: {}", src_obj);
    run_command(&bitcode_path, "zig", &["build", "object", "-Drelease=true"]);

    println!("Compiling 64-bit ir to: {}", dest_ir_64bit);
    run_command(&bitcode_path, "zig", &["build", "ir", "-Drelease=true"]);

    println!("Compiling 32-bit ir to: {}", dest_ir_32bit);
    run_command(
        &bitcode_path,
        "zig",
        &["build", "ir-32bit", "-Drelease=true"],
    );

    println!("Moving zig object to: {}", dest_obj);

    run_command(&bitcode_path, "mv", &[src_obj, dest_obj]);

    let dest_bc_path = bitcode_path.join("builtins-32bit.bc");
    let dest_bc_32bit = dest_bc_path.to_str().expect("Invalid dest bc path");
    println!("Compiling 32-bit bitcode to: {}", dest_bc_32bit);

    run_command(
        &build_script_dir_path,
        "llvm-as",
        &[dest_ir_32bit, "-o", dest_bc_32bit],
    );

    let dest_bc_path = bitcode_path.join("builtins-64bit.bc");
    let dest_bc_64bit = dest_bc_path.to_str().expect("Invalid dest bc path");
    println!("Compiling 64-bit bitcode to: {}", dest_bc_64bit);

    run_command(
        &build_script_dir_path,
        "llvm-as",
        &[dest_ir_64bit, "-o", dest_bc_64bit],
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
