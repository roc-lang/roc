use crate::target::{arch_str, target_triple_str};
#[cfg(feature = "llvm")]
use libloading::{Error, Library};
use roc_builtins::bitcode;
// #[cfg(feature = "llvm")]
use roc_mono::ir::OptLevel;
use std::collections::HashMap;
use std::env;
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output};
use target_lexicon::{Architecture, OperatingSystem, Triple};

fn zig_executable() -> String {
    match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LinkType {
    // These numbers correspond to the --lib flag; if it's present
    // (e.g. is_present returns `1 as bool`), this will be 1 as well.
    Executable = 0,
    Dylib = 1,
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
            architecture: Architecture::Wasm32,
            ..
        } => link_wasm32(target, output_path, input_paths, link_type),
        Triple {
            operating_system: OperatingSystem::Linux,
            ..
        } => link_linux(target, output_path, input_paths, link_type),
        Triple {
            operating_system: OperatingSystem::Darwin,
            ..
        } => link_macos(target, output_path, input_paths, link_type),
        _ => panic!("TODO gracefully handle unsupported target: {:?}", target),
    }
}

fn find_zig_str_path() -> PathBuf {
    let zig_str_path = PathBuf::from("compiler/builtins/bitcode/src/str.zig");

    if std::path::Path::exists(&zig_str_path) {
        return zig_str_path;
    }

    // when running the tests, we start in the /cli directory
    let zig_str_path = PathBuf::from("../compiler/builtins/bitcode/src/str.zig");
    if std::path::Path::exists(&zig_str_path) {
        return zig_str_path;
    }

    panic!("cannot find `str.zig`. Launch me from either the root of the roc repo or one level down(roc/examples, roc/cli...)")
}

fn find_wasi_libc_path() -> PathBuf {
    let wasi_libc_path = PathBuf::from("compiler/builtins/bitcode/wasi-libc.a");

    if std::path::Path::exists(&wasi_libc_path) {
        return wasi_libc_path;
    }

    // when running the tests, we start in the /cli directory
    let wasi_libc_path = PathBuf::from("../compiler/builtins/bitcode/wasi-libc.a");
    if std::path::Path::exists(&wasi_libc_path) {
        return wasi_libc_path;
    }

    panic!("cannot find `wasi-libc.a`")
}

#[cfg(not(target_os = "macos"))]
#[allow(clippy::too_many_arguments)]
pub fn build_zig_host_native(
    env_path: &str,
    env_home: &str,
    emit_bin: &str,
    zig_host_src: &str,
    zig_str_path: &str,
    target: &str,
    opt_level: OptLevel,
    shared_lib_path: Option<&Path>,
    _target_valgrind: bool,
) -> Output {
    let mut command = Command::new(&zig_executable());
    command
        .env_clear()
        .env("PATH", env_path)
        .env("HOME", env_home);
    if let Some(shared_lib_path) = shared_lib_path {
        command.args(&[
            "build-exe",
            "-fPIE",
            shared_lib_path.to_str().unwrap(),
            bitcode::BUILTINS_HOST_OBJ_PATH,
        ]);
    } else {
        command.args(&["build-obj", "-fPIC"]);
    }
    command.args(&[
        zig_host_src,
        emit_bin,
        "--pkg-begin",
        "str",
        zig_str_path,
        "--pkg-end",
        // include the zig runtime
        "-fcompiler-rt",
        // include libc
        "--library",
        "c",
        // cross-compile?
        "-target",
        target,
    ]);

    // use single threaded testing for cli_run and enable this code if valgrind fails with unhandled instruction bytes, see #1963.
    /*if target_valgrind {
        command.args(&[
        "-mcpu",
        "x86_64"
        ]);
    }*/

    if matches!(opt_level, OptLevel::Optimize) {
        command.args(&["-O", "ReleaseSafe"]);
    }
    command.output().unwrap()
}

#[cfg(target_os = "macos")]
#[allow(clippy::too_many_arguments)]
pub fn build_zig_host_native(
    env_path: &str,
    env_home: &str,
    emit_bin: &str,
    zig_host_src: &str,
    zig_str_path: &str,
    _target: &str,
    opt_level: OptLevel,
    shared_lib_path: Option<&Path>,
    // For compatibility with the non-macOS def above. Keep these in sync.
    _target_valgrind: bool,
) -> Output {
    use serde_json::Value;

    // Run `zig env` to find the location of zig's std/ directory
    let zig_env_output = Command::new(&zig_executable())
        .args(&["env"])
        .output()
        .unwrap();

    let zig_env_json = if zig_env_output.status.success() {
        std::str::from_utf8(&zig_env_output.stdout).unwrap_or_else(|utf8_err| {
            panic!(
                "`zig env` failed; its stderr output was invalid utf8 ({:?})",
                utf8_err
            );
        })
    } else {
        match std::str::from_utf8(&zig_env_output.stderr) {
            Ok(stderr) => panic!("`zig env` failed - stderr output was: {:?}", stderr),
            Err(utf8_err) => panic!(
                "`zig env` failed; its stderr output was invalid utf8 ({:?})",
                utf8_err
            ),
        }
    };

    let mut zig_compiler_rt_path = match serde_json::from_str(zig_env_json) {
        Ok(Value::Object(map)) => match map.get("std_dir") {
            Some(Value::String(std_dir)) => PathBuf::from(Path::new(std_dir)),
            _ => {
                panic!("Expected JSON containing a `std_dir` String field from `zig env`, but got: {:?}", zig_env_json);
            }
        },
        _ => {
            panic!(
                "Expected JSON containing a `std_dir` field from `zig env`, but got: {:?}",
                zig_env_json
            );
        }
    };

    zig_compiler_rt_path.push("special");
    zig_compiler_rt_path.push("compiler_rt.zig");

    let mut command = Command::new(&zig_executable());
    command
        .env_clear()
        .env("PATH", &env_path)
        .env("HOME", &env_home);
    if let Some(shared_lib_path) = shared_lib_path {
        command.args(&[
            "build-exe",
            "-fPIE",
            shared_lib_path.to_str().unwrap(),
            bitcode::BUILTINS_HOST_OBJ_PATH,
        ]);
    } else {
        command.args(&["build-obj", "-fPIC"]);
    }
    command.args(&[
        zig_host_src,
        emit_bin,
        "--pkg-begin",
        "str",
        zig_str_path,
        "--pkg-end",
        // include the zig runtime
        "--pkg-begin",
        "compiler_rt",
        zig_compiler_rt_path.to_str().unwrap(),
        "--pkg-end",
        // include libc
        "--library",
        "c",
    ]);
    if matches!(opt_level, OptLevel::Optimize) {
        command.args(&["-O", "ReleaseSafe"]);
    }
    command.output().unwrap()
}

pub fn build_zig_host_wasm32(
    env_path: &str,
    env_home: &str,
    emit_bin: &str,
    zig_host_src: &str,
    zig_str_path: &str,
    opt_level: OptLevel,
    shared_lib_path: Option<&Path>,
) -> Output {
    if shared_lib_path.is_some() {
        unimplemented!("Linking a shared library to wasm not yet implemented");
    }
    // NOTE currently just to get compiler warnings if the host code is invalid.
    // the produced artifact is not used
    //
    // NOTE we're emitting LLVM IR here (again, it is not actually used)
    //
    // we'd like to compile with `-target wasm32-wasi` but that is blocked on
    //
    // https://github.com/ziglang/zig/issues/9414
    let mut command = Command::new(&zig_executable());
    command
        .env_clear()
        .env("PATH", env_path)
        .env("HOME", env_home)
        .args(&[
            "build-obj",
            zig_host_src,
            emit_bin,
            "--pkg-begin",
            "str",
            zig_str_path,
            "--pkg-end",
            // include the zig runtime
            // "-fcompiler-rt",
            // include libc
            "--library",
            "c",
            "-target",
            "i386-linux-musl",
            // "wasm32-wasi",
            // "-femit-llvm-ir=/home/folkertdev/roc/roc/examples/benchmarks/platform/host.ll",
            "-fPIC",
            "--strip",
        ]);
    if matches!(opt_level, OptLevel::Optimize) {
        command.args(&["-O", "ReleaseSafe"]);
    }
    command.output().unwrap()
}

pub fn build_c_host_native(
    env_path: &str,
    env_home: &str,
    dest: &str,
    sources: &[&str],
    opt_level: OptLevel,
    shared_lib_path: Option<&Path>,
) -> Output {
    let mut command = Command::new("clang");
    command
        .env_clear()
        .env("PATH", &env_path)
        .env("HOME", &env_home)
        .args(sources)
        .args(&["-o", dest]);
    if let Some(shared_lib_path) = shared_lib_path {
        command.args(&[
            shared_lib_path.to_str().unwrap(),
            bitcode::BUILTINS_HOST_OBJ_PATH,
            "-fPIE",
            "-pie",
            "-lm",
            "-lpthread",
            "-ldl",
            "-lrt",
            "-lutil",
        ]);
    } else {
        command.args(&["-fPIC", "-c"]);
    }
    if matches!(opt_level, OptLevel::Optimize) {
        command.arg("-O2");
    }
    command.output().unwrap()
}

pub fn build_swift_host_native(
    env_path: &str,
    env_home: &str,
    dest: &str,
    sources: &[&str],
    opt_level: OptLevel,
    shared_lib_path: Option<&Path>,
    objc_header_path: Option<&str>,
) -> Output {
    if shared_lib_path.is_some() {
        unimplemented!("Linking a shared library to Swift not yet implemented");
    }

    let mut command = Command::new("swiftc");
    command
        .env_clear()
        .env("PATH", &env_path)
        .env("HOME", &env_home)
        .args(sources)
        .arg("-emit-object")
        .arg("-parse-as-library")
        .args(&["-o", dest]);

    if let Some(objc_header) = objc_header_path {
        command.args(&["-import-objc-header", objc_header]);
    }

    if matches!(opt_level, OptLevel::Optimize) {
        command.arg("-O");
    }

    command.output().unwrap()
}

pub fn rebuild_host(
    opt_level: OptLevel,
    target: &Triple,
    host_input_path: &Path,
    shared_lib_path: Option<&Path>,
    target_valgrind: bool,
) {
    let c_host_src = host_input_path.with_file_name("host.c");
    let c_host_dest = host_input_path.with_file_name("c_host.o");
    let zig_host_src = host_input_path.with_file_name("host.zig");
    let rust_host_src = host_input_path.with_file_name("host.rs");
    let rust_host_dest = host_input_path.with_file_name("rust_host.o");
    let cargo_host_src = host_input_path.with_file_name("Cargo.toml");
    let swift_host_src = host_input_path.with_file_name("host.swift");
    let swift_host_header_src = host_input_path.with_file_name("host.h");

    let host_dest_native = host_input_path.with_file_name(if shared_lib_path.is_some() {
        "dynhost"
    } else {
        "host.o"
    });
    let host_dest_wasm = host_input_path.with_file_name("host.bc");

    let env_path = env::var("PATH").unwrap_or_else(|_| "".to_string());
    let env_home = env::var("HOME").unwrap_or_else(|_| "".to_string());

    if zig_host_src.exists() {
        // Compile host.zig

        let zig_str_path = find_zig_str_path();

        debug_assert!(
            std::path::Path::exists(&zig_str_path),
            "Cannot find str.zig, looking at {:?}",
            &zig_str_path
        );

        let output = match target.architecture {
            Architecture::Wasm32 => {
                let emit_bin = format!("-femit-llvm-ir={}", host_dest_wasm.to_str().unwrap());
                build_zig_host_wasm32(
                    &env_path,
                    &env_home,
                    &emit_bin,
                    zig_host_src.to_str().unwrap(),
                    zig_str_path.to_str().unwrap(),
                    opt_level,
                    shared_lib_path,
                )
            }
            Architecture::X86_64 => {
                let emit_bin = format!("-femit-bin={}", host_dest_native.to_str().unwrap());
                build_zig_host_native(
                    &env_path,
                    &env_home,
                    &emit_bin,
                    zig_host_src.to_str().unwrap(),
                    zig_str_path.to_str().unwrap(),
                    "native",
                    opt_level,
                    shared_lib_path,
                    target_valgrind,
                )
            }
            Architecture::X86_32(_) => {
                let emit_bin = format!("-femit-bin={}", host_dest_native.to_str().unwrap());
                build_zig_host_native(
                    &env_path,
                    &env_home,
                    &emit_bin,
                    zig_host_src.to_str().unwrap(),
                    zig_str_path.to_str().unwrap(),
                    "i386-linux-musl",
                    opt_level,
                    shared_lib_path,
                    target_valgrind,
                )
            }

            Architecture::Aarch64(_) => {
                let emit_bin = format!("-femit-bin={}", host_dest_native.to_str().unwrap());
                build_zig_host_native(
                    &env_path,
                    &env_home,
                    &emit_bin,
                    zig_host_src.to_str().unwrap(),
                    zig_str_path.to_str().unwrap(),
                    target_triple_str(target),
                    opt_level,
                    shared_lib_path,
                    target_valgrind,
                )
            }
            _ => panic!("Unsupported architecture {:?}", target.architecture),
        };

        validate_output("host.zig", &zig_executable(), output)
    } else if cargo_host_src.exists() {
        // Compile and link Cargo.toml, if it exists
        let cargo_dir = host_input_path.parent().unwrap();
        let cargo_out_dir =
            cargo_dir
                .join("target")
                .join(if matches!(opt_level, OptLevel::Optimize) {
                    "release"
                } else {
                    "debug"
                });

        let mut command = Command::new("cargo");
        command.arg("build").current_dir(cargo_dir);
        if matches!(opt_level, OptLevel::Optimize) {
            command.arg("--release");
        }
        let source_file = if shared_lib_path.is_some() {
            command.env("RUSTFLAGS", "-C link-dead-code");
            command.args(&["--bin", "host"]);
            "src/main.rs"
        } else {
            command.arg("--lib");
            "src/lib.rs"
        };
        let output = command.output().unwrap();

        validate_output(source_file, "cargo build", output);

        if shared_lib_path.is_some() {
            // For surgical linking, just copy the dynamically linked rust app.
            std::fs::copy(cargo_out_dir.join("host"), host_dest_native).unwrap();
        } else {
            // Cargo hosts depend on a c wrapper for the api. Compile host.c as well.

            let output = build_c_host_native(
                &env_path,
                &env_home,
                c_host_dest.to_str().unwrap(),
                &[c_host_src.to_str().unwrap()],
                opt_level,
                shared_lib_path,
            );
            validate_output("host.c", "clang", output);

            let output = Command::new("ld")
                .env_clear()
                .env("PATH", &env_path)
                .args(&[
                    "-r",
                    "-L",
                    cargo_out_dir.to_str().unwrap(),
                    c_host_dest.to_str().unwrap(),
                    "-lhost",
                    "-o",
                    host_dest_native.to_str().unwrap(),
                ])
                .output()
                .unwrap();
            validate_output("c_host.o", "ld", output);

            // Clean up c_host.o
            let output = Command::new("rm")
                .env_clear()
                .args(&["-f", c_host_dest.to_str().unwrap()])
                .output()
                .unwrap();

            validate_output("rust_host.o", "rm", output);
        }
    } else if rust_host_src.exists() {
        // Compile and link host.rs, if it exists
        let mut command = Command::new("rustc");
        command.args(&[
            rust_host_src.to_str().unwrap(),
            "-o",
            rust_host_dest.to_str().unwrap(),
        ]);
        if matches!(opt_level, OptLevel::Optimize) {
            command.arg("-O");
        }
        let output = command.output().unwrap();

        validate_output("host.rs", "rustc", output);

        // Rust hosts depend on a c wrapper for the api. Compile host.c as well.
        if shared_lib_path.is_some() {
            // If compiling to executable, let c deal with linking as well.
            let output = build_c_host_native(
                &env_path,
                &env_home,
                host_dest_native.to_str().unwrap(),
                &[
                    c_host_src.to_str().unwrap(),
                    rust_host_dest.to_str().unwrap(),
                ],
                opt_level,
                shared_lib_path,
            );
            validate_output("host.c", "clang", output);
        } else {
            let output = build_c_host_native(
                &env_path,
                &env_home,
                c_host_dest.to_str().unwrap(),
                &[c_host_src.to_str().unwrap()],
                opt_level,
                shared_lib_path,
            );

            validate_output("host.c", "clang", output);
            let output = Command::new("ld")
                .env_clear()
                .env("PATH", &env_path)
                .args(&[
                    "-r",
                    c_host_dest.to_str().unwrap(),
                    rust_host_dest.to_str().unwrap(),
                    "-o",
                    host_dest_native.to_str().unwrap(),
                ])
                .output()
                .unwrap();

            validate_output("rust_host.o", "ld", output);
        }

        // Clean up rust_host.o and c_host.o
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
    } else if c_host_src.exists() {
        // Compile host.c, if it exists
        let output = build_c_host_native(
            &env_path,
            &env_home,
            host_dest_native.to_str().unwrap(),
            &[c_host_src.to_str().unwrap()],
            opt_level,
            shared_lib_path,
        );
        validate_output("host.c", "clang", output);
    } else if swift_host_src.exists() {
        // Compile host.swift, if it exists
        let output = build_swift_host_native(
            &env_path,
            &env_home,
            host_dest_native.to_str().unwrap(),
            &[swift_host_src.to_str().unwrap()],
            opt_level,
            shared_lib_path,
            swift_host_header_src
                .exists()
                .then(|| swift_host_header_src.to_str().unwrap()),
        );
        validate_output("host.swift", "swiftc", output);
    }
}

fn nix_path_opt() -> Option<String> {
    env::var_os("NIX_GLIBC_PATH").map(|path| path.into_string().unwrap())
}

fn library_path<const N: usize>(segments: [&str; N]) -> Option<PathBuf> {
    let mut guess_path = PathBuf::new();
    for s in segments {
        guess_path.push(s);
    }
    if guess_path.exists() {
        Some(guess_path)
    } else {
        None
    }
}

fn link_linux(
    target: &Triple,
    output_path: PathBuf,
    input_paths: &[&str],
    link_type: LinkType,
) -> io::Result<(Child, PathBuf)> {
    let architecture = format!("{}-linux-gnu", target.architecture);

    //    Command::new("cp")
    //        .args(&[input_paths[0], "/home/folkertdev/roc/wasm/host.o"])
    //        .output()
    //        .unwrap();
    //
    //    Command::new("cp")
    //        .args(&[input_paths[1], "/home/folkertdev/roc/wasm/app.o"])
    //        .output()
    //        .unwrap();

    if let Architecture::X86_32(_) = target.architecture {
        return Ok((
            Command::new(&zig_executable())
                .args(&["build-exe"])
                .args(input_paths)
                .args(&[
                    "-target",
                    "i386-linux-musl",
                    "-lc",
                    &format!("-femit-bin={}", output_path.to_str().unwrap()),
                ])
                .spawn()?,
            output_path,
        ));
    }

    let libcrt_path =
        // give preference to nix_path if it's defined, this prevents bugs
        if let Some(nix_path) = nix_path_opt() {
            library_path([&nix_path])
            .unwrap()
        } else {
            library_path(["/usr", "lib", &architecture])
            .or_else(|| library_path(["/usr", "lib"]))
            .unwrap()
        };

    let libgcc_name = "libgcc_s.so.1";
    let libgcc_path =
        // give preference to nix_path if it's defined, this prevents bugs
        if let Some(nix_path) = nix_path_opt() {
            library_path([&nix_path, libgcc_name])
            .unwrap()
        } else {
            library_path(["/lib", &architecture, libgcc_name])
            .or_else(|| library_path(["/usr", "lib", &architecture, libgcc_name]))
            .or_else(|| library_path(["/usr", "lib", libgcc_name]))
            .unwrap()
        };

    let ld_linux = match target.architecture {
        Architecture::X86_64 => {
            // give preference to nix_path if it's defined, this prevents bugs
            if let Some(nix_path) = nix_path_opt() {
                library_path([&nix_path, "ld-linux-x86-64.so.2"])
            } else {
                library_path(["/lib64", "ld-linux-x86-64.so.2"])
            }
        }
        Architecture::Aarch64(_) => library_path(["/lib", "ld-linux-aarch64.so.1"]),
        _ => panic!(
            "TODO gracefully handle unsupported linux architecture: {:?}",
            target.architecture
        ),
    };
    let ld_linux = ld_linux.unwrap();
    let ld_linux = ld_linux.to_str().unwrap();

    let mut soname;
    let (base_args, output_path) = match link_type {
        LinkType::Executable => (
            // Presumably this S stands for Static, since if we include Scrt1.o
            // in the linking for dynamic builds, linking fails.
            vec![libcrt_path.join("Scrt1.o").to_str().unwrap().to_string()],
            output_path,
        ),
        LinkType::Dylib => {
            // TODO: do we actually need the version number on this?
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

    init_arch(target);

    // NOTE: order of arguments to `ld` matters here!
    // The `-l` flags should go after the `.o` arguments

    let mut command = Command::new("ld");

    command
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
            "--gc-sections",
            "--eh-frame-hdr",
            "-A",
            arch_str(target),
            "-pie",
            libcrt_path.join("crti.o").to_str().unwrap(),
            libcrt_path.join("crtn.o").to_str().unwrap(),
        ])
        .args(&base_args)
        .args(&["-dynamic-linker", ld_linux])
        .args(input_paths)
        // ld.lld requires this argument, and does not accept --arch
        // .args(&["-L/usr/lib/x86_64-linux-gnu"])
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
            libgcc_path.to_str().unwrap(),
            // Output
            "-o",
            output_path.as_path().to_str().unwrap(), // app (or app.so or app.dylib etc.)
        ]);

    let output = command.spawn()?;

    Ok((output, output_path))
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

    let arch = match target.architecture {
        Architecture::Aarch64(_) => "arm64".to_string(),
        _ => target.architecture.to_string(),
    };

    let mut ld_command = Command::new("ld");

    ld_command
        // NOTE: order of arguments to `ld` matters here!
        // The `-l` flags should go after the `.o` arguments
        // Don't allow LD_ env vars to affect this
        .env_clear()
        .args(&[
            // NOTE: we don't do --gc-sections on macOS because the default
            // macOS linker doesn't support it, but it's a performance
            // optimization, so if we ever switch to a different linker,
            // we'd like to re-enable it on macOS!
            // "--gc-sections",
            link_type_arg,
            "-arch",
            &arch,
            "-macos_version_min",
            &get_macos_version(),
        ])
        .args(input_paths);

    let sdk_path = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib";
    if Path::new(sdk_path).exists() {
        ld_command.arg(format!("-L{}", sdk_path));
        ld_command.arg(format!("-L{}/swift", sdk_path));
    };

    ld_command.args(&[
        // Libraries - see https://github.com/rtfeldman/roc/pull/554#discussion_r496392274
        // for discussion and further references
        "-lSystem",
        "-lresolv",
        "-lpthread",
        // "-lrt", // TODO shouldn't we need this?
        // "-lc_nonshared", // TODO shouldn't we need this?
        // "-lgcc", // TODO will eventually need compiler_rt from gcc or something - see https://github.com/rtfeldman/roc/pull/554#discussion_r496370840
        // "-framework", // Uncomment this line & the following ro run the `rand` crate in examples/cli
        // "Security",
        // Output
        "-o",
        output_path.to_str().unwrap(), // app
    ]);

    let mut ld_child = ld_command.spawn()?;

    match target.architecture {
        Architecture::Aarch64(_) => {
            ld_child.wait()?;
            let codesign_child = Command::new("codesign")
                .args(&["-s", "-", output_path.to_str().unwrap()])
                .spawn()?;

            Ok((codesign_child, output_path))
        }
        _ => Ok((ld_child, output_path)),
    }
}

fn get_macos_version() -> String {
    let cmd_stdout = Command::new("sw_vers")
        .arg("-productVersion")
        .output()
        .expect("Failed to execute command 'sw_vers -productVersion'")
        .stdout;

    let full_version_string = String::from_utf8(cmd_stdout)
        .expect("Failed to convert output of command 'sw_vers -productVersion' into a utf8 string");

    full_version_string
        .trim_end()
        .split('.')
        .take(2)
        .collect::<Vec<&str>>()
        .join(".")
}

fn link_wasm32(
    _target: &Triple,
    output_path: PathBuf,
    input_paths: &[&str],
    _link_type: LinkType,
) -> io::Result<(Child, PathBuf)> {
    let zig_str_path = find_zig_str_path();
    let wasi_libc_path = find_wasi_libc_path();

    let child = Command::new(&zig_executable())
        // .env_clear()
        // .env("PATH", &env_path)
        .args(&["build-exe"])
        .args(input_paths)
        .args([
            // include wasi libc
            // using `-lc` is broken in zig 8 (and early 9) in combination with ReleaseSmall
            wasi_libc_path.to_str().unwrap(),
            &format!("-femit-bin={}", output_path.to_str().unwrap()),
            "-target",
            "wasm32-wasi-musl",
            "--pkg-begin",
            "str",
            zig_str_path.to_str().unwrap(),
            "--pkg-end",
            "--strip",
            "-O",
            "ReleaseSmall",
            // useful for debugging
            // "-femit-llvm-ir=/home/folkertdev/roc/roc/examples/benchmarks/platform/host.ll",
        ])
        .spawn()?;

    Ok((child, output_path))
}

#[cfg(feature = "llvm")]
pub fn module_to_dylib(
    module: &inkwell::module::Module,
    target: &Triple,
    opt_level: OptLevel,
) -> Result<Library, Error> {
    use crate::target::{self, convert_opt_level};
    use inkwell::targets::{CodeModel, FileType, RelocMode};

    let dir = tempfile::tempdir().unwrap();
    let filename = PathBuf::from("Test.roc");
    let file_path = dir.path().join(filename);
    let mut app_o_file = file_path;

    app_o_file.set_file_name("app.o");

    // Emit the .o file using position-independent code (PIC) - needed for dylibs
    let reloc = RelocMode::PIC;
    let model = CodeModel::Default;
    let target_machine =
        target::target_machine(target, convert_opt_level(opt_level), reloc, model).unwrap();

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

    unsafe { Library::new(path) }
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

#[cfg(feature = "llvm")]
fn init_arch(target: &Triple) {
    crate::target::init_arch(target);
}

#[cfg(not(feature = "llvm"))]
fn init_arch(_target: &Triple) {
    panic!("Tried to initialize LLVM when crate was not built with `feature = \"llvm\"` enabled");
}
