use crate::target;
use inkwell::module::Module;
use inkwell::targets::{CodeModel, FileType, RelocMode};
use libloading::{Error, Library};
use roc_gen::llvm::build::OptLevel;
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
            architecture: Architecture::X86_64,
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
    let rust_host_src = host_input_path.with_file_name("host.rs");
    let rust_host_dest = host_input_path.with_file_name("rust_host.o");
    let cargo_host_src = host_input_path.with_file_name("Cargo.toml");
    let host_dest = host_input_path.with_file_name("host.o");

    // Compile host.c
    let env_path = std::env::var("PATH").unwrap_or_else(|_| "".to_string());
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

    if cargo_host_src.exists() {
        // Compile and link Cargo.toml, if it exists
        let cargo_dir = host_input_path.parent().unwrap();
        let libhost_dir = cargo_dir.join("target").join("release");

        let output = Command::new("cargo")
            .args(&["build", "--release"])
            .current_dir(cargo_dir)
            .output()
            .unwrap();

        validate_output("host.rs", "cargo build --release", output);

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
            .env("PATH", &env_path)
            .args(&[
                "-f",
                rust_host_dest.to_str().unwrap(),
                c_host_dest.to_str().unwrap(),
            ])
            .output()
            .unwrap();

        validate_output("rust_host.o", "rm", output);
    } else {
        // Clean up rust_host.o
        let output = Command::new("mv")
            .env_clear()
            .env("PATH", &env_path)
            .args(&[c_host_dest, host_dest])
            .output()
            .unwrap();

        validate_output("rust_host.o", "mv", output);
    }
}

fn link_linux(
    target: &Triple,
    output_path: PathBuf,
    input_paths: &[&str],
    link_type: LinkType,
) -> io::Result<(Child, PathBuf)> {
    let (base_args, output_path) = match link_type {
        LinkType::Executable => (vec![], output_path),
        LinkType::Dylib => {
            // TODO: do we acually need the version number on this?
            //
            // See https://software.intel.com/content/www/us/en/develop/articles/create-a-unix-including-linux-shared-library.html
            let mut output_path = output_path;

            output_path.set_extension("so.1.0");

            (
                // TODO: find a way to avoid using a vec! here - should theoretically be
                // able to do this somehow using &[] but the borrow checker isn't having it.
                vec!["-shared"],
                output_path,
            )
        }
    };

    let env_path = std::env::var("PATH").unwrap_or_else(|_| "".to_string());
    Ok((
        Command::new("clang++")
            // Don't allow LD_ env vars to affect this
            .env_clear()
            .env("PATH", &env_path)
            .args(&[
                "-target",
                &target.to_string(),
                "-o",
                output_path.as_path().to_str().unwrap(), // app (or app.so or app.dylib etc.)
            ])
            .args(base_args)
            .args(input_paths)
            .args(&["-fuse-ld=lld", "-stdlib=libc++"])
            .args(&["-lpthread", "-ldl"])
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

    let env_path = std::env::var("PATH").unwrap_or_else(|_| "".to_string());
    Ok((
        // NOTE: order of arguments to `ld` matters here!
        // The `-l` flags should go after the `.o` arguments
        Command::new("ld")
            // Don't allow LD_ env vars to affect this
            .env_clear()
            .env("PATH", &env_path)
            .args(&[
                link_type_arg,
                "-arch",
                target.architecture.to_string().as_str(),
            ])
            .args(input_paths)
            .args(&[
                // Libraries - see https://github.com/rtfeldman/roc/pull/554#discussion_r496392274
                // for discussion and further references
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
