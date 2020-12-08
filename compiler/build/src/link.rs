use crate::target;
use crate::target::arch_str;
use inkwell::module::Module;
use inkwell::targets::{CodeModel, FileType, RelocMode};
use libloading::{Error, Library};
use roc_gen::llvm::build::OptLevel;
use std::collections::HashMap;
use std::env;
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output};
use target_lexicon::{Architecture, OperatingSystem, Triple};
use tempfile::tempdir;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LinkType {
    Executable,
    Dylib,
}

/// input_paths can include the host as well as the app. e.g. &["host.o", "roc_app.o"]
pub fn link(
    target: &Triple,
    output_path: PathBuf,
    input_paths: &[&str],
    link_type: LinkType,
) -> io::Result<(Child, PathBuf)> {
    match target {
        Triple {
            operating_system: OperatingSystem::Linux,
            ..
        } => link_linux(target, output_path, input_paths, link_type),
        Triple {
            architecture: Architecture::X86_64,
            operating_system: OperatingSystem::Darwin,
            ..
        } => link_macos(target, output_path, input_paths, link_type),
        _ => panic!("TODO gracefully handle unsupported target: {:?}", target),
    }
}

pub fn rebuild_host(host_input_path: &Path) {
    let c_host_src = host_input_path.with_file_name("host.c");
    let c_host_dest = host_input_path.with_file_name("c_host.o");
    let zig_host_src = host_input_path.with_file_name("host.zig");
    let rust_host_src = host_input_path.with_file_name("host.rs");
    let rust_host_dest = host_input_path.with_file_name("rust_host.o");
    let cargo_host_src = host_input_path.with_file_name("Cargo.toml");
    let host_dest = host_input_path.with_file_name("host.o");

    let env_path = env::var("PATH").unwrap_or_else(|_| "".to_string());
    let env_home = env::var("HOME").unwrap_or_else(|_| "".to_string());

    if zig_host_src.exists() {
        // Compile host.zig
        let emit_bin = format!("-femit-bin={}", host_dest.to_str().unwrap());
        let output = Command::new("zig")
            .env_clear()
            .env("PATH", &env_path)
            .env("HOME", &env_home)
            .args(&[
                "build-lib",
                zig_host_src.to_str().unwrap(),
                &emit_bin,
                // include the zig runtime
                "-fcompiler-rt",
                // include libc
                "--library",
                "c",
            ])
            .output()
            .unwrap();

        validate_output("host.zig", "zig", output);
    } else {
        // Compile host.c
        let output = Command::new("clang")
            .env_clear()
            .env("PATH", &env_path)
            .args(&[
                "-c",
                c_host_src.to_str().unwrap(),
                "-o",
                c_host_dest.to_str().unwrap(),
            ])
            .output()
            .unwrap();

        validate_output("host.c", "clang", output);
    }

    if cargo_host_src.exists() {
        // Compile and link Cargo.toml, if it exists
        let cargo_dir = host_input_path.parent().unwrap();
        let libhost_dir = cargo_dir.join("target").join("release");

        let output = Command::new("cargo")
            .args(&["build", "--release"])
            .current_dir(cargo_dir)
            .output()
            .unwrap();

        validate_output("src/lib.rs", "cargo build --release", output);

        let output = Command::new("ld")
            .env_clear()
            .env("PATH", &env_path)
            .args(&[
                "-r",
                "-L",
                libhost_dir.to_str().unwrap(),
                c_host_dest.to_str().unwrap(),
                "-lhost",
                "-o",
                host_dest.to_str().unwrap(),
            ])
            .output()
            .unwrap();

        validate_output("c_host.o", "ld", output);
    } else if rust_host_src.exists() {
        // Compile and link host.rs, if it exists
        let output = Command::new("rustc")
            .args(&[
                rust_host_src.to_str().unwrap(),
                "-o",
                rust_host_dest.to_str().unwrap(),
            ])
            .output()
            .unwrap();

        validate_output("host.rs", "rustc", output);

        let output = Command::new("ld")
            .env_clear()
            .env("PATH", &env_path)
            .args(&[
                "-r",
                c_host_dest.to_str().unwrap(),
                rust_host_dest.to_str().unwrap(),
                "-o",
                host_dest.to_str().unwrap(),
            ])
            .output()
            .unwrap();

        validate_output("rust_host.o", "ld", output);

        // Clean up rust_host.o
        let output = Command::new("rm")
            .env_clear()
            .args(&[
                "-f",
                rust_host_dest.to_str().unwrap(),
                c_host_dest.to_str().unwrap(),
            ])
            .output()
            .unwrap();

        validate_output("rust_host.o", "rm", output);
    } else if c_host_dest.exists() {
        // Clean up c_host.o
        let output = Command::new("mv")
            .env_clear()
            .args(&[c_host_dest, host_dest])
            .output()
            .unwrap();

        validate_output("c_host.o", "mv", output);
    }
}

fn link_linux(
    target: &Triple,
    output_path: PathBuf,
    input_paths: &[&str],
    link_type: LinkType,
) -> io::Result<(Child, PathBuf)> {
    let usr_lib_path = Path::new("/usr/lib").to_path_buf();
    let usr_lib_gnu_path = usr_lib_path.join(format!("{}-linux-gnu", target.architecture));
    let lib_gnu_path = Path::new("/lib/").join(format!("{}-linux-gnu", target.architecture));

    let libcrt_path = if usr_lib_gnu_path.exists() {
        &usr_lib_gnu_path
    } else {
        &usr_lib_path
    };

    let libgcc_name = "libgcc_s.so.1";
    let libgcc_path = if lib_gnu_path.join(libgcc_name).exists() {
        lib_gnu_path.join(libgcc_name)
    } else if usr_lib_gnu_path.join(libgcc_name).exists() {
        usr_lib_gnu_path.join(libgcc_name)
    } else {
        usr_lib_path.join(libgcc_name)
    };
    let ld_linux = match target.architecture {
        Architecture::X86_64 => "/lib64/ld-linux-x86-64.so.2",
        Architecture::Aarch64(_) => "/lib/ld-linux-aarch64.so.1",
        _ => panic!(
            "TODO gracefully handle unsupported linux architecture: {:?}",
            target.architecture
        ),
    };

    let mut soname;
    let (base_args, output_path) = match link_type {
        LinkType::Executable => (
            // Presumably this S stands for Static, since if we include Scrt1.o
            // in the linking for dynamic builds, linking fails.
            vec![libcrt_path.join("Scrt1.o").to_str().unwrap().to_string()],
            output_path,
        ),
        LinkType::Dylib => {
            // TODO: do we acually need the version number on this?
            // Do we even need the "-soname" argument?
            //
            // See https://software.intel.com/content/www/us/en/develop/articles/create-a-unix-including-linux-shared-library.html

            soname = output_path.clone();
            soname.set_extension("so.1");

            let mut output_path = output_path;

            output_path.set_extension("so.1.0");

            (
                // TODO: find a way to avoid using a vec! here - should theoretically be
                // able to do this somehow using &[] but the borrow checker isn't having it.
                // Also find a way to have these be string slices instead of Strings.
                vec![
                    "-shared".to_string(),
                    "-soname".to_string(),
                    soname.as_path().to_str().unwrap().to_string(),
                ],
                output_path,
            )
        }
    };

    let env_path = env::var("PATH").unwrap_or_else(|_| "".to_string());
    // NOTE: order of arguments to `ld` matters here!
    // The `-l` flags should go after the `.o` arguments
    Ok((
        Command::new("ld")
            // Don't allow LD_ env vars to affect this
            .env_clear()
            .env("PATH", &env_path)
            // Keep NIX_ env vars
            .envs(
                env::vars()
                    .filter(|&(ref k, _)| k.starts_with("NIX_"))
                    .collect::<HashMap<String, String>>(),
            )
            .args(&[
                "-arch",
                arch_str(target),
                libcrt_path.join("crti.o").to_str().unwrap(),
                libcrt_path.join("crtn.o").to_str().unwrap(),
            ])
            .args(&base_args)
            .args(&["-dynamic-linker", ld_linux])
            .args(input_paths)
            .args(&[
                // Libraries - see https://github.com/rtfeldman/roc/pull/554#discussion_r496365925
                // for discussion and further references
                "-lc",
                "-lm",
                "-lpthread",
                "-ldl",
                "-lrt",
                "-lutil",
                "-lc_nonshared",
                "-lc++",
                "-lc++abi",
                "-lunwind",
                libgcc_path.to_str().unwrap(),
                // Output
                "-o",
                output_path.as_path().to_str().unwrap(), // app (or app.so or app.dylib etc.)
            ])
            .spawn()?,
        output_path,
    ))
}

fn link_macos(
    target: &Triple,
    output_path: PathBuf,
    input_paths: &[&str],
    link_type: LinkType,
) -> io::Result<(Child, PathBuf)> {
    let (link_type_arg, output_path) = match link_type {
        LinkType::Executable => ("-execute", output_path),
        LinkType::Dylib => {
            let mut output_path = output_path;

            output_path.set_extension("dylib");

            ("-dylib", output_path)
        }
    };

    // This path only exists on macOS Big Sur, and it causes ld errors
    // on Catalina if it's specified with -L, so we replace it with a
    // redundant -lSystem if the directory isn't there.
    let big_sur_path = "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib";
    let big_sur_fix = if Path::new(big_sur_path).exists() {
        format!("-L{}", big_sur_path)
    } else {
        String::from("-lSystem")
    };

    Ok((
        // NOTE: order of arguments to `ld` matters here!
        // The `-l` flags should go after the `.o` arguments
        Command::new("ld")
            // Don't allow LD_ env vars to affect this
            .env_clear()
            .args(&[
                link_type_arg,
                "-arch",
                target.architecture.to_string().as_str(),
            ])
            .args(input_paths)
            .args(&[
                // Libraries - see https://github.com/rtfeldman/roc/pull/554#discussion_r496392274
                // for discussion and further references
                &big_sur_fix,
                "-lSystem",
                "-lresolv",
                "-lpthread",
                // "-lrt", // TODO shouldn't we need this?
                // "-lc_nonshared", // TODO shouldn't we need this?
                // "-lgcc", // TODO will eventually need compiler_rt from gcc or something - see https://github.com/rtfeldman/roc/pull/554#discussion_r496370840
                // "-lunwind", // TODO will eventually need this, see https://github.com/rtfeldman/roc/pull/554#discussion_r496370840
                "-lc++", // TODO shouldn't we need this?
                // Output
                "-o",
                output_path.to_str().unwrap(), // app
            ])
            .spawn()?,
        output_path,
    ))
}

pub fn module_to_dylib(
    module: &Module,
    target: &Triple,
    opt_level: OptLevel,
) -> Result<Library, Error> {
    let dir = tempdir().unwrap();
    let filename = PathBuf::from("Test.roc");
    let file_path = dir.path().join(filename);
    let mut app_o_file = file_path;

    app_o_file.set_file_name("app.o");

    // Emit the .o file using position-indepedent code (PIC) - needed for dylibs
    let reloc = RelocMode::PIC;
    let model = CodeModel::Default;
    let target_machine = target::target_machine(target, opt_level.into(), reloc, model).unwrap();

    target_machine
        .write_to_file(module, FileType::Object, &app_o_file)
        .expect("Writing .o file failed");

    // Link app.o into a dylib - e.g. app.so or app.dylib
    let (mut child, dylib_path) = link(
        &Triple::host(),
        app_o_file.clone(),
        &[app_o_file.to_str().unwrap()],
        LinkType::Dylib,
    )
    .unwrap();

    child.wait().unwrap();

    // Load the dylib
    let path = dylib_path.as_path().to_str().unwrap();

    Library::new(path)
}

fn validate_output(file_name: &str, cmd_name: &str, output: Output) {
    if !output.status.success() {
        match std::str::from_utf8(&output.stderr) {
            Ok(stderr) => panic!(
                "Failed to rebuild {} - stderr of the `{}` command was:\n{}",
                file_name, cmd_name, stderr
            ),
            Err(utf8_err) => panic!(
                "Failed to rebuild {} - stderr of the `{}` command was invalid utf8 ({:?})",
                file_name, cmd_name, utf8_err
            ),
        }
    }
}
