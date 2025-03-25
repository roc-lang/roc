use crate::target::arch_str;
use libloading::{Error, Library};
use roc_command_utils::{cargo, clang, rustup, zig};
use roc_debug_flags;
use roc_error_macros::internal_error;
use roc_mono::ir::OptLevel;
use roc_target::{Architecture, OperatingSystem, Target};
use std::collections::HashMap;
use std::ffi::OsString;
use std::fs::DirEntry;
use std::io;
use std::path::{Path, PathBuf};
use std::process::{self, Child, Command};
use std::{env, fs};
use wasi_libc_sys::{WASI_COMPILER_RT_PATH, WASI_LIBC_PATH};

pub use roc_linker::LinkType;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LinkingStrategy {
    /// Compile app and host object files, then use a linker like lld or wasm-ld
    Legacy,
    /// Compile app and host object files, then use the Roc surgical linker
    Surgical,
    /// Initialise the backend from a host object file, then add the app to it. No linker needed.
    Additive,
}

/// input_paths can include the host as well as the app. e.g. &["host.o", "roc_app.o"]
pub fn link(
    target: Target,
    output_path: PathBuf,
    input_paths: &[&str],
    link_type: LinkType,
) -> io::Result<(Child, PathBuf)> {
    match target.arch_os() {
        (Architecture::Wasm32, _) => link_wasm32(target, output_path, input_paths, link_type),
        (_, OperatingSystem::Linux) => link_linux(target, output_path, input_paths, link_type),
        (_, OperatingSystem::Mac) => link_macos(target, output_path, input_paths, link_type),
        (_, OperatingSystem::Windows) => link_windows(output_path, input_paths, link_type),
        _ => internal_error!("TODO gracefully handle unsupported target: {:?}", target),
    }
}

// Attempts to find a file that is stored relative to the roc executable.
// Since roc is built in target/debug/roc, we may need to drop that path to find the file.
// This is used to avoid depending on the current working directory.
pub fn get_relative_path(sub_path: &Path) -> Option<PathBuf> {
    if sub_path.is_absolute() {
        internal_error!(
            "get_relative_path requires sub_path to be relative, instead got {:?}",
            sub_path
        );
    }
    let exe_relative_str_path_opt = std::env::current_exe().ok();

    if let Some(exe_relative_str_path) = exe_relative_str_path_opt {
        #[cfg(windows)]
        let exe_relative_str_path = roc_command_utils::strip_windows_prefix(&exe_relative_str_path);

        let mut curr_parent_opt = exe_relative_str_path.parent();

        // We need to support paths like ./roc, ./bin/roc, ./target/debug/roc and tests like ./target/debug/deps/valgrind-63c787aa176d1277
        // This requires dropping up to 3 directories.
        for _ in 0..=3 {
            if let Some(curr_parent) = curr_parent_opt {
                let potential_path = curr_parent.join(sub_path);

                if std::path::Path::exists(&potential_path) {
                    return Some(potential_path);
                } else {
                    curr_parent_opt = curr_parent.parent();
                }
            } else {
                break;
            }
        }
    }

    None
}

fn find_wasi_libc_path() -> PathBuf {
    // This path is available when built and run from source
    // Environment variable defined in wasi-libc-sys/build.rs
    let build_path = PathBuf::from(WASI_LIBC_PATH);
    if std::path::Path::exists(&build_path) {
        return build_path;
    }

    // This path is available in the release tarball
    match get_relative_path(Path::new("lib/wasi-libc.a")) {
        Some(path) if path.exists() => return path,
        _ => (),
    }

    internal_error!("cannot find `wasi-libc.a`")
}

#[cfg(unix)]
#[allow(clippy::too_many_arguments)]
pub fn build_zig_host_native(
    env_path: &str,
    env_home: &str,
    emit_bin: &str,
    zig_host_src: &str,
    target: &str,
    opt_level: OptLevel,
    shared_lib_path: Option<&Path>,
    builtins_host_path: &Path,
) -> Command {
    let mut zig_cmd = zig();
    zig_cmd
        .env_clear()
        .env("PATH", env_path)
        .env("HOME", env_home);

    if let Some(shared_lib_path) = shared_lib_path {
        // with LLVM, the builtins are already part of the roc app,
        // but with the dev backend, they are missing. To minimize work,
        // we link them as part of the host executable
        zig_cmd.args([
            "build-exe",
            "-fPIE",
            "-rdynamic", // make sure roc_alloc and friends are exposed
            shared_lib_path.to_str().unwrap(),
            builtins_host_path.to_str().unwrap(),
        ]);
    } else {
        zig_cmd.args(["build-obj", "-fPIC"]);
    }

    zig_cmd.args([
        zig_host_src,
        &format!("-femit-bin={emit_bin}"),
        // include libc
        "-lc",
        // cross-compile?
        "-target",
        target,
    ]);

    // some examples need the compiler-rt in the app object file.
    // but including it on windows causes weird crashes, at least
    // when we use zig 0.9. It looks like zig 0.10 is going to fix
    // this problem for us, so this is a temporary workaround
    if !target.contains("windows") {
        zig_cmd.args([
            // include the zig runtime
            "-fcompiler-rt",
        ]);
    }

    // valgrind does not yet support avx512 instructions, see #1963.
    if env::var("NO_AVX512").is_ok() {
        zig_cmd.args(["-mcpu", "x86_64"]);
    }

    if matches!(opt_level, OptLevel::Optimize) {
        zig_cmd.args(["-O", "ReleaseSafe"]);
    } else if matches!(opt_level, OptLevel::Size) {
        zig_cmd.args(["-O", "ReleaseSmall", "-fno-strip"]);
    }

    zig_cmd
}

#[cfg(windows)]
#[allow(clippy::too_many_arguments)]
pub fn build_zig_host_native(
    env_path: &str,
    env_home: &str,
    emit_bin: &str,
    zig_host_src: &str,
    target: &str,
    opt_level: OptLevel,
    shared_lib_path: Option<&Path>,
    builtins_host_path: &Path,
) -> Command {
    // to prevent `clang failed with stderr: zig: error: unable to make temporary file: No such file or directory`
    let env_userprofile = env::var("USERPROFILE").unwrap_or_else(|_| "".to_string());

    let mut zig_cmd = zig();
    zig_cmd
        .env_clear()
        .env("PATH", env_path)
        .env("HOME", env_home)
        .env("USERPROFILE", env_userprofile);

    if let Some(shared_lib_path) = shared_lib_path {
        zig_cmd.args(&[
            "build-exe",
            // "-fPIE", PIE seems to fail on windows
            shared_lib_path.to_str().unwrap(),
            builtins_host_path.to_str().unwrap(),
        ]);
    } else {
        zig_cmd.args(&["build-obj"]);
    }

    zig_cmd.args(&[
        zig_host_src,
        &format!("-femit-bin={}", emit_bin),
        // include the zig runtime
        // "-fcompiler-rt", compiler-rt causes segfaults on windows; investigate why
        // include libc
        "-lc",
        "-rdynamic",
        // cross-compile?
        "-target",
        target,
    ]);

    if matches!(opt_level, OptLevel::Optimize) {
        zig_cmd.args(&["-O", "ReleaseSafe"]);
    } else if matches!(opt_level, OptLevel::Size) {
        zig_cmd.args(&["-O", "ReleaseSmall"]);
    }

    zig_cmd
}

pub fn build_zig_host_wasm32(
    env_path: &str,
    env_home: &str,
    emit_bin: &str,
    zig_host_src: &str,
    opt_level: OptLevel,
    shared_lib_path: Option<&Path>,
) -> Command {
    if shared_lib_path.is_some() {
        unimplemented!("Linking a shared library to wasm not yet implemented");
    }

    // NOTE currently just to get compiler warnings if the host code is invalid.
    // the produced artifact is not used
    let mut zig_cmd = zig();

    zig_cmd
        .env_clear()
        .env("PATH", env_path)
        .env("HOME", env_home)
        .args([
            "build-obj",
            zig_host_src,
            emit_bin,
            // include the zig runtime
            // "-fcompiler-rt",
            "-target",
            "wasm32-wasi",
            // "-femit-llvm-ir=/home/folkertdev/roc/roc/crates/cli/tests/benchmarks/platform/host.ll",
            "-fPIC",
            "-fstrip",
        ]);

    if matches!(opt_level, OptLevel::Optimize) {
        zig_cmd.args(["-O", "ReleaseSafe"]);
    } else if matches!(opt_level, OptLevel::Size) {
        zig_cmd.args(["-O", "ReleaseSmall"]);
    }

    zig_cmd
}

#[allow(clippy::too_many_arguments)]
pub fn build_c_host_native(
    target: Target,
    env_path: &str,
    env_home: &str,
    env_cpath: &str,
    dest: &str,
    sources: &[&str],
    opt_level: OptLevel,
    shared_lib_path: Option<&Path>,
    builtins_host_path: &Path,
) -> Command {
    let mut clang_cmd = clang();
    clang_cmd
        .env_clear()
        .env("PATH", env_path)
        .env("CPATH", env_cpath)
        .env("HOME", env_home)
        .args(sources)
        .args(["-o", dest]);
    if let Some(shared_lib_path) = shared_lib_path {
        match target.operating_system() {
            OperatingSystem::Windows => {
                // just use zig as a C compiler

                // I think we only ever have one C source file in practice
                assert_eq!(sources.len(), 1);

                return build_zig_host_native(
                    env_path,
                    env_home,
                    dest,
                    sources[0],
                    "native",
                    opt_level,
                    Some(shared_lib_path),
                    builtins_host_path,
                );
            }
            _ => {
                clang_cmd.args([
                    shared_lib_path.to_str().unwrap(),
                    // This line is commented out because
                    // @bhansconnect: With the addition of Str.graphemes, always
                    // linking the built-ins led to a surgical linker bug for
                    // optimized builds. Disabling until it is needed for dev
                    // builds.
                    // builtins_host_path,
                    "-fPIE",
                    "-pie",
                    "-lm",
                    "-lpthread",
                    "-ldl",
                    "-lrt",
                    "-lutil",
                ]);
            }
        }
    } else {
        clang_cmd.args(["-fPIC", "-c"]);
    }
    if matches!(opt_level, OptLevel::Optimize) {
        clang_cmd.arg("-O3");
    } else if matches!(opt_level, OptLevel::Size) {
        clang_cmd.arg("-Os");
    }

    clang_cmd
}

#[allow(clippy::too_many_arguments)]
pub fn build_swift_host_native(
    env_path: &str,
    env_home: &str,
    dest: &str,
    sources: &[&str],
    opt_level: OptLevel,
    shared_lib_path: Option<&Path>,
    objc_header_path: Option<&str>,
    arch: Architecture,
) -> Command {
    if shared_lib_path.is_some() {
        unimplemented!("Linking a shared library to Swift not yet implemented");
    }

    let mut command = Command::new("arch");
    command
        .env_clear()
        .env("PATH", env_path)
        .env("HOME", env_home);

    match arch {
        Architecture::Aarch64 => command.arg("-arm64"),
        _ => command.arg(format!("-{arch}")),
    };

    command
        .arg("xcrun") // xcrun helps swiftc to find the right header files
        .arg("swiftc")
        .args(sources)
        .arg("-emit-object")
        // `-module-name host` renames the .o file to "host" - otherwise you get an error like:
        //   error: module name "legacy_macos-arm64" is not a valid identifier; use -module-name flag to specify an alternate name
        .arg("-module-name")
        .arg("host")
        .arg("-parse-as-library")
        .args(["-o", dest]);

    if let Some(objc_header) = objc_header_path {
        command.args(["-import-objc-header", objc_header]);
    }

    if matches!(opt_level, OptLevel::Optimize) {
        command.arg("-O");
    } else if matches!(opt_level, OptLevel::Size) {
        command.arg("-Osize");
    }

    command
}

pub fn rebuild_host(
    opt_level: OptLevel,
    target: Target,
    platform_main_roc: &Path,
    shared_lib_path: Option<&Path>,
) -> PathBuf {
    let c_host_src = platform_main_roc.with_file_name("host.c");
    let c_host_dest = platform_main_roc.with_file_name("c_host.o");
    let zig_host_src = platform_main_roc.with_file_name("host.zig");
    let rust_host_src = platform_main_roc.with_file_name("host.rs");
    let rust_host_dest = platform_main_roc.with_file_name("rust_host.o");
    let cargo_host_src = platform_main_roc.with_file_name("Cargo.toml");
    let swift_host_src = platform_main_roc.with_file_name("host.swift");
    let swift_host_header_src = platform_main_roc.with_file_name("host.h");

    let executable_extension = match target.operating_system() {
        OperatingSystem::Windows => "exe",
        _ => "",
    };

    let host_dest = if matches!(target.architecture(), Architecture::Wasm32) {
        // TODO verify this is corect, how do we do get here with OptLevel::Development
        if matches!(opt_level, OptLevel::Development) {
            platform_main_roc.with_extension("o")
        } else {
            platform_main_roc.with_extension("bc")
        }
    } else if shared_lib_path.is_some() {
        platform_main_roc
            .with_file_name("dynhost")
            .with_extension(executable_extension)
    } else {
        platform_main_roc.with_file_name(target.prebuilt_static_object())
    };

    let env_path = env::var("PATH").unwrap_or_else(|_| "".to_string());
    let env_home = env::var("HOME").unwrap_or_else(|_| "".to_string());
    let env_cpath = env::var("CPATH").unwrap_or_else(|_| "".to_string());

    let builtins_host_tempfile =
        roc_bitcode::host_tempfile().expect("failed to write host builtins object to tempfile");

    if zig_host_src.exists() {
        // Compile host.zig
        let zig_cmd = match target.architecture() {
            Architecture::Wasm32 => {
                let emit_bin = if matches!(opt_level, OptLevel::Development) {
                    format!("-femit-bin={}", host_dest.to_str().unwrap())
                } else {
                    format!("-femit-llvm-ir={}", host_dest.to_str().unwrap())
                };
                build_zig_host_wasm32(
                    &env_path,
                    &env_home,
                    &emit_bin,
                    zig_host_src.to_str().unwrap(),
                    opt_level,
                    shared_lib_path,
                )
            }
            Architecture::X86_64 => build_zig_host_native(
                &env_path,
                &env_home,
                host_dest.to_str().unwrap(),
                zig_host_src.to_str().unwrap(),
                // This used to be "native" but that caused segfaults that were hard to
                // reproduce and investigate.
                // For context: github.com/roc-lang/roc/pull/6591#issuecomment-2039808944
                "x86_64-native",
                opt_level,
                shared_lib_path,
                builtins_host_tempfile.path(),
            ),
            Architecture::X86_32 => build_zig_host_native(
                &env_path,
                &env_home,
                host_dest.to_str().unwrap(),
                zig_host_src.to_str().unwrap(),
                "i386-linux-musl",
                opt_level,
                shared_lib_path,
                builtins_host_tempfile.path(),
            ),
            Architecture::Aarch64 => build_zig_host_native(
                &env_path,
                &env_home,
                host_dest.to_str().unwrap(),
                zig_host_src.to_str().unwrap(),
                "native",
                opt_level,
                shared_lib_path,
                builtins_host_tempfile.path(),
            ),
            _ => internal_error!("Unsupported architecture {:?}", target.architecture()),
        };

        run_build_command(zig_cmd, "host.zig", 0);
    } else if cargo_host_src.exists() {
        // Compile and link Cargo.toml, if it exists
        let cargo_dir = platform_main_roc.parent().unwrap();

        let mut cargo_cmd = if cfg!(windows) {
            // on windows, we need the nightly toolchain so we can use `-Z export-executable-symbols`
            // using `+nightly` only works when running cargo through rustup
            let mut cmd = rustup();
            cmd.args(["run", "nightly-2024-02-03", "cargo"]);

            cmd
        } else {
            cargo()
        };

        cargo_cmd.arg("build").current_dir(cargo_dir);
        // Rust doesn't expose size without editing the cargo.toml. Instead just use release.
        if matches!(opt_level, OptLevel::Optimize | OptLevel::Size) {
            cargo_cmd.arg("--release");
        }

        let source_file = if shared_lib_path.is_some() {
            let rust_flags = if cfg!(windows) {
                "-Z export-executable-symbols"
            } else {
                "-C link-args=-rdynamic"
            };
            cargo_cmd.env("RUSTFLAGS", rust_flags);
            cargo_cmd.args(["--bin", "host"]);
            "src/main.rs"
        } else {
            cargo_cmd.arg("--lib");
            "src/lib.rs"
        };

        run_build_command(cargo_cmd, source_file, 0);

        let cargo_out_dir = find_used_target_sub_folder(opt_level, cargo_dir.join("target"));

        if shared_lib_path.is_some() {
            // For surgical linking, just copy the dynamically linked rust app.
            let mut exe_path = cargo_out_dir.join("host");
            exe_path.set_extension(executable_extension);
            if let Err(e) = std::fs::copy(&exe_path, &host_dest) {
                internal_error!(
                    "unable to copy {} => {}: {:?}\n\nIs the file used by another invocation of roc?",
                    exe_path.display(),
                    host_dest.display(),
                    e,
                );
            }
        } else {
            // Cargo hosts depend on a c wrapper for the api. Compile host.c as well.

            let clang_cmd = build_c_host_native(
                target,
                &env_path,
                &env_home,
                &env_cpath,
                c_host_dest.to_str().unwrap(),
                &[c_host_src.to_str().unwrap()],
                opt_level,
                shared_lib_path,
                builtins_host_tempfile.path(),
            );

            run_build_command(clang_cmd, "host.c", 0);

            let mut ld_cmd = Command::new("ld");

            ld_cmd.env_clear().env("PATH", &env_path).args([
                "-r",
                "-L",
                cargo_out_dir.to_str().unwrap(),
                c_host_dest.to_str().unwrap(),
                "-lhost",
                "-o",
                host_dest.to_str().unwrap(),
            ]);

            run_build_command(ld_cmd, "c_host.o", 0);

            // Clean up c_host.o
            if c_host_dest.exists() {
                // there can be a race condition on this file cleanup
                let _ = std::fs::remove_file(c_host_dest);
            }
        }
    } else if rust_host_src.exists() {
        // Compile and link host.rs, if it exists
        let mut rustc_cmd = Command::new("rustc");
        rustc_cmd.args([
            rust_host_src.to_str().unwrap(),
            "-o",
            rust_host_dest.to_str().unwrap(),
        ]);
        if matches!(opt_level, OptLevel::Optimize) {
            rustc_cmd.arg("-O");
        } else if matches!(opt_level, OptLevel::Size) {
            rustc_cmd.args(["-C", "opt-level=s"]);
        }

        run_build_command(rustc_cmd, "host.rs", 0);

        // Rust hosts depend on a c wrapper for the api. Compile host.c as well.
        if shared_lib_path.is_some() {
            // If compiling to executable, let c deal with linking as well.
            let clang_cmd = build_c_host_native(
                target,
                &env_path,
                &env_home,
                &env_cpath,
                host_dest.to_str().unwrap(),
                &[
                    c_host_src.to_str().unwrap(),
                    rust_host_dest.to_str().unwrap(),
                ],
                opt_level,
                shared_lib_path,
                builtins_host_tempfile.path(),
            );
            run_build_command(clang_cmd, "host.c", 0);
        } else {
            let clang_cmd = build_c_host_native(
                target,
                &env_path,
                &env_home,
                &env_cpath,
                c_host_dest.to_str().unwrap(),
                &[c_host_src.to_str().unwrap()],
                opt_level,
                shared_lib_path,
                builtins_host_tempfile.path(),
            );

            run_build_command(clang_cmd, "host.c", 0);

            let mut ld_cmd = Command::new("ld");

            ld_cmd.env_clear().env("PATH", &env_path).args([
                "-r",
                c_host_dest.to_str().unwrap(),
                rust_host_dest.to_str().unwrap(),
                "-o",
                host_dest.to_str().unwrap(),
            ]);

            run_build_command(ld_cmd, "rust_host.o", 0);
        }

        // Clean up rust_host.o and c_host.o
        if c_host_dest.exists() {
            std::fs::remove_file(c_host_dest).unwrap();
        }
        if rust_host_dest.exists() {
            std::fs::remove_file(rust_host_dest).unwrap();
        }
    } else if c_host_src.exists() {
        // Compile host.c, if it exists
        let clang_cmd = build_c_host_native(
            target,
            &env_path,
            &env_home,
            &env_cpath,
            host_dest.to_str().unwrap(),
            &[c_host_src.to_str().unwrap()],
            opt_level,
            shared_lib_path,
            builtins_host_tempfile.path(),
        );

        run_build_command(clang_cmd, "host.c", 0);
    } else if swift_host_src.exists() {
        // Compile host.swift, if it exists
        let swiftc_cmd = build_swift_host_native(
            &env_path,
            &env_home,
            host_dest.to_str().unwrap(),
            &[swift_host_src.to_str().unwrap()],
            opt_level,
            shared_lib_path,
            swift_host_header_src
                .exists()
                .then(|| swift_host_header_src.to_str().unwrap()),
            target.architecture(),
        );

        run_build_command(swiftc_cmd, "host.swift", 0);
    }

    // Extend the lifetime of the tempfile so it doesn't get dropped
    // (and thus deleted) before the build process is done using it!
    let _ = builtins_host_tempfile;

    host_dest
}

// there can be multiple release folders, one in target and one in target/x86_64-unknown-linux-musl,
// we want the one that was most recently used
fn find_used_target_sub_folder(opt_level: OptLevel, target_folder: PathBuf) -> PathBuf {
    let out_folder_name = if matches!(opt_level, OptLevel::Optimize | OptLevel::Size) {
        "release"
    } else {
        "debug"
    };

    let matching_folders = find_in_folder_or_subfolders(&target_folder, out_folder_name);
    let mut matching_folders_iter = matching_folders.iter();

    let mut out_folder = match matching_folders_iter.next() {
        Some(dir_entry) => dir_entry,
        None => internal_error!("I could not find a folder named {} in {:?}. This may be because the `cargo build` for the platform went wrong.", out_folder_name, target_folder)
    };

    let mut out_folder_last_change = out_folder.metadata().unwrap().modified().unwrap();

    for dir_entry in matching_folders_iter {
        let last_modified = dir_entry.metadata().unwrap().modified().unwrap();

        if last_modified > out_folder_last_change {
            out_folder_last_change = last_modified;
            out_folder = dir_entry;
        }
    }

    out_folder.path().canonicalize().unwrap()
}

fn find_in_folder_or_subfolders(path: &PathBuf, folder_to_find: &str) -> Vec<DirEntry> {
    let mut matching_dirs = vec![];

    if let Ok(entries) = fs::read_dir(path) {
        for entry in entries.flatten() {
            if entry.file_type().unwrap().is_dir() {
                let dir_name = entry
                    .file_name()
                    .into_string()
                    .unwrap_or_else(|_| "".to_string());

                if dir_name == folder_to_find {
                    matching_dirs.push(entry)
                } else {
                    let matched_in_sub_dir =
                        find_in_folder_or_subfolders(&entry.path(), folder_to_find);

                    matching_dirs.extend(matched_in_sub_dir);
                }
            }
        }
    }

    matching_dirs
}

fn nix_paths() -> Vec<String> {
    let mut paths = vec![];

    if let Some(nix_libgcc_s_path) = env::var_os("NIX_LIBGCC_S_PATH") {
        paths.push(nix_libgcc_s_path.into_string().unwrap())
    }

    if let Some(nix_glibc_path) = nix_glibc_path_opt() {
        paths.push(nix_glibc_path.into_string().unwrap())
    }

    paths
}

fn nix_glibc_path_opt() -> Option<OsString> {
    env::var_os("NIX_GLIBC_PATH")
}

fn check_path_or_panic(path_str: &str) -> PathBuf {
    let guess_path = PathBuf::from(path_str);

    if guess_path.exists() {
        guess_path
    } else {
        panic!("{} does not exist.", guess_path.display());
    }
}

/// Given a list of library directories and the name of a library, find the 1st match
///
/// The provided list of library directories should be in the form of a list of
/// directories, where each directory is represented by a series of path segments, like
///
/// ["/usr", "lib"]
///
/// Each directory will be checked for a file with the provided filename, and the first
/// match will be returned.
///
/// If there are no matches, [`None`] will be returned.
fn look_for_library(lib_dirs: &[PathBuf], lib_filename: &str) -> Option<PathBuf> {
    lib_dirs
        .iter()
        .map(|path| {
            let mut path_cl = path.clone();
            path_cl.push(lib_filename);
            path_cl
        })
        .find(|path| path.exists())
}

fn strs_to_path(strs: &[&str]) -> PathBuf {
    strs.iter().collect()
}

fn extra_link_flags() -> Vec<String> {
    match env::var("ROC_LINK_FLAGS") {
        Ok(flags) => {
            println!("⚠️ CAUTION: The ROC_LINK_FLAGS environment variable is a temporary workaround, and will no longer do anything once surgical linking lands! If you're concerned about what this means for your use case, please ask about it on Zulip.");

            flags
        }
        Err(_) => "".to_string(),
    }.split_whitespace().map(|x| x.to_owned()).collect()
}

fn link_linux(
    target: Target,
    output_path: PathBuf,
    input_paths: &[&str],
    link_type: LinkType,
) -> io::Result<(Child, PathBuf)> {
    let architecture = format!("{}-linux-gnu", target.architecture());

    //    Command::new("cp")
    //        .args(&[input_paths[0], "/home/folkertdev/roc/wasm/host.o"])
    //        .output()
    //        .unwrap();
    //
    //    Command::new("cp")
    //        .args(&[input_paths[1], "/home/folkertdev/roc/wasm/app.o"])
    //        .output()
    //        .unwrap();

    if let Architecture::X86_32 = target.architecture() {
        return Ok((
            zig()
                .args(["build-exe"])
                .args(input_paths)
                .args([
                    "-target",
                    "i386-linux-musl",
                    "-lc",
                    &format!("-femit-bin={}", output_path.to_str().unwrap()),
                ])
                .spawn()?,
            output_path,
        ));
    }

    let nix_paths_vec_string = nix_paths();
    let nix_paths_vec: Vec<PathBuf> = nix_paths_vec_string.iter().map(PathBuf::from).collect();
    let usr_lib_arch_path = strs_to_path(&["/usr", "lib", &architecture]);
    let lib_arch_path = strs_to_path(&["/lib", &architecture]);

    let mut lib_dirs: Vec<PathBuf> = vec![];

    // start with nix paths, this prevents version incompatibility
    if !nix_paths_vec.is_empty() {
        lib_dirs.extend(nix_paths_vec)
    }

    lib_dirs.extend([
        usr_lib_arch_path,
        lib_arch_path,
        strs_to_path(&["/usr", "lib64"]),
        strs_to_path(&["/usr", "lib"]),
    ]);

    // Look for the libraries we'll need
    let libgcc_name = "libgcc_s.so.1";
    let libgcc_path = look_for_library(&lib_dirs, libgcc_name);

    let crti_name = "crti.o";
    let crti_path = look_for_library(&lib_dirs, crti_name);

    let crtn_name = "crtn.o";
    let crtn_path = look_for_library(&lib_dirs, crtn_name);

    let scrt1_name = "Scrt1.o";
    let scrt1_path = look_for_library(&lib_dirs, scrt1_name);

    // Unwrap all the paths at once so we can inform the user of any missing libs
    let (libgcc_path, crti_path, crtn_path, scrt1_path) =
        match (libgcc_path, crti_path, crtn_path, scrt1_path) {
            (Some(libgcc), Some(crti), Some(crtn), Some(scrt1)) => (libgcc, crti, crtn, scrt1),
            (maybe_gcc, maybe_crti, maybe_crtn, maybe_scrt1) => {
                if maybe_gcc.is_none() {
                    eprintln!("Couldn't find libgcc_s.so.1!");
                    eprintln!("You may need to install libgcc\n");
                }
                if maybe_crti.is_none() | maybe_crtn.is_none() | maybe_scrt1.is_none() {
                    eprintln!("Couldn't find the libc development files!");
                    eprintln!("We need the files crti.o, crtn.o, and Scrt1.o");
                    eprintln!();
                    eprintln!("On Ubuntu/Debian execute:");
                    eprintln!("\tsudo apt install libc-dev\n");
                    eprintln!("On ArchLinux/Manjaro execute:");
                    eprintln!("\tsudo pacman -S glibc\n");
                    eprintln!("On Fedora execute:");
                    eprintln!("\tsudo dnf install glibc-devel\n");
                }

                let dirs = lib_dirs
                    .iter()
                    .map(|path_buf| {
                        path_buf
                            .as_path()
                            .to_str()
                            .unwrap_or("FAILED TO CONVERT PATH TO STR")
                            .to_string()
                    })
                    .collect::<Vec<String>>()
                    .join("\n");
                eprintln!("We looked in the following directories:\n{dirs}");
                process::exit(1);
            }
        };

    let (libgcc_path_str, crti_path_str, crtn_path_str, scrt1_path_str) = (
        libgcc_path.to_string_lossy(),
        crti_path.to_string_lossy(),
        crtn_path.to_string_lossy(),
        scrt1_path.to_string_lossy(),
    );

    fn get_ld_linux_path(
        nix_glibc_path_opt: Option<OsString>,
        native_lib_path: &str,
        ld_file_name: &str,
    ) -> String {
        let mut full_path_str = String::new();

        if let Some(nix_glibc_path) = nix_glibc_path_opt {
            full_path_str.push_str(&nix_glibc_path.to_string_lossy())
        } else {
            full_path_str.push_str(native_lib_path)
        };

        full_path_str.push('/');
        full_path_str.push_str(ld_file_name);

        check_path_or_panic(&full_path_str);
        full_path_str.to_string()
    }

    let ld_linux_path = match target.architecture() {
        Architecture::X86_64 => {
            get_ld_linux_path(nix_glibc_path_opt(), "/lib64", "ld-linux-x86-64.so.2")
        }
        Architecture::Aarch64 => {
            get_ld_linux_path(nix_glibc_path_opt(), "/lib", "ld-linux-aarch64.so.1")
        }
        _ => internal_error!(
            "TODO gracefully handle unsupported linux architecture: {:?}",
            target.architecture()
        ),
    };

    let ld_linux_path_str = &ld_linux_path;

    let (base_args, output_path) = match link_type {
        LinkType::Executable => (
            // Presumably this S stands for Static, since if we include Scrt1.o
            // in the linking for dynamic builds, linking fails.
            [scrt1_path_str.as_ref()],
            output_path,
        ),
        LinkType::Dylib => {
            let mut output_path = output_path;
            output_path.set_extension("so");

            (["-shared"], output_path)
        }
        LinkType::None => internal_error!("link_linux should not be called with link type of none"),
    };

    let env_path = env::var("PATH").unwrap_or_else(|_| "".to_string());

    // NOTE: order of arguments to `ld` matters here!
    // The `-l` flags should go after the `.o` arguments

    let mut ld_command = Command::new("ld");

    ld_command
        // Don't allow LD_ env vars to affect this
        .env_clear()
        .env("PATH", &env_path)
        // Keep NIX_ env vars
        .envs(
            env::vars()
                .filter(|(k, _)| k.starts_with("NIX_"))
                .collect::<HashMap<String, String>>(),
        )
        .args([
            "--gc-sections",
            "--eh-frame-hdr",
            "-A",
            arch_str(target),
            "-pie",
            &crti_path_str,
            &crtn_path_str,
        ])
        .args(base_args)
        .args(["-dynamic-linker", ld_linux_path_str])
        .args(input_paths)
        .args(extra_link_flags())
        // ld.lld requires this argument, and does not accept --arch
        // .args(&["-L/usr/lib/x86_64-linux-gnu"])
        .args([
            // Libraries - see https://github.com/roc-lang/roc/pull/554#discussion_r496365925
            // for discussion and further references
            "-lc",
            "-lm",
            "-lpthread",
            "-ldl",
            "-lrt",
            "-lutil",
            "-lc_nonshared",
            &libgcc_path_str,
            // Output
            "-o",
            output_path.as_path().to_str().unwrap(), // app (or app.so or app.dylib etc.)
        ]);
    debug_print_command(&ld_command);

    let ld_output = ld_command.spawn()?;

    Ok((ld_output, output_path))
}

fn link_macos(
    target: Target,
    output_path: PathBuf,
    input_paths: &[&str],
    link_type: LinkType,
) -> io::Result<(Child, PathBuf)> {
    let (link_type_args, output_path) = match link_type {
        LinkType::Executable => (vec!["-execute"], output_path),
        LinkType::Dylib => {
            let mut output_path = output_path;

            output_path.set_extension("dylib");

            (vec!["-dylib", "-undefined", "dynamic_lookup"], output_path)
        }
        LinkType::None => internal_error!("link_macos should not be called with link type of none"),
    };

    let arch = match target.architecture() {
        Architecture::Aarch64 => "arm64".to_string(),
        _ => target.architecture().to_string(),
    };

    let mut ld_command = Command::new("ld");

    ld_command
        // NOTE: order of arguments to `ld` matters here!
        // The `-l` flags should go after the `.o` arguments
        // Don't allow LD_ env vars to affect this
        .env_clear()
        .args(&link_type_args)
        .args([
            // NOTE: we don't do --gc-sections on macOS because the default
            // macOS linker doesn't support it, but it's a performance
            // optimization, so if we ever switch to a different linker,
            // we'd like to re-enable it on macOS!
            // "--gc-sections",
            "-arch",
            &arch,
            "-macos_version_min",
            &get_macos_version(),
            // Suppress fixup chains to ease working out dynamic relocs by the
            // surgical linker. In my experience, working with dyld opcodes is
            // slightly easier than unpacking compressed info from the __got section
            // and fixups load command.
            "-no_fixup_chains",
            // Suppress all warnings, at least for now. Ideally, there are no warnings
            // from the linker.
            "-w",
        ])
        .args(input_paths)
        .args(extra_link_flags());

    if get_xcode_version() >= 15.0 {
        ld_command.arg("-ld_classic");
    }

    let sdk_path = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib";
    if Path::new(sdk_path).exists() {
        ld_command.arg(format!("-L{sdk_path}"));
        ld_command.arg(format!("-L{sdk_path}/swift"));
    };

    ld_command.args([
        // Libraries - see https://github.com/roc-lang/roc/pull/554#discussion_r496392274
        // for discussion and further references
        "-lSystem",
        "-lresolv",
        "-lpthread",
        // This `-F PATH` flag is needed for `-framework` flags to work
        "-F",
        "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/System/Library/Frameworks/",
        // These frameworks are needed for GUI examples to work
        "-framework",
        "AudioUnit",
        "-framework",
        "Cocoa",
        "-framework",
        "CoreAudio",
        "-framework",
        "CoreVideo",
        "-framework",
        "IOKit",
        "-framework",
        "Metal",
        "-framework",
        "QuartzCore",
        // "-lrt", // TODO shouldn't we need this?
        // "-lc_nonshared", // TODO shouldn't we need this?
        // "-lgcc", // TODO will eventually need compiler_rt from gcc or something - see https://github.com/roc-lang/roc/pull/554#discussion_r496370840
        "-framework",
        "Security",
        "-framework",
        "SystemConfiguration",
        // Output
        "-o",
        output_path.to_str().unwrap(), // app
    ]);

    debug_print_command(&ld_command);

    let mut ld_child = ld_command.spawn()?;

    match target.architecture() {
        Architecture::Aarch64 => {
            ld_child.wait()?;

            let mut codesign_cmd = Command::new("/usr/bin/codesign");
            codesign_cmd.args(["-s", "-", output_path.to_str().unwrap()]);
            debug_print_command(&codesign_cmd);
            let codesign_child = codesign_cmd.spawn()?;

            Ok((codesign_child, output_path))
        }
        _ => Ok((ld_child, output_path)),
    }
}

fn get_macos_version() -> String {
    let mut cmd = Command::new("sw_vers");
    cmd.arg("-productVersion");
    debug_print_command(&cmd);

    let cmd_stdout = cmd
        .output()
        .expect("Failed to execute command 'sw_vers -productVersion'")
        .stdout;

    let full_version_string = String::from_utf8(cmd_stdout)
        .expect("Failed to convert output of command 'sw_vers -productVersion' into a utf8 string");

    full_version_string
        .trim_end()
        .split('.')
        .take(3)
        .collect::<Vec<&str>>()
        .join(".")
}

fn get_xcode_version() -> f32 {
    let mut cmd = Command::new("xcodebuild");
    cmd.arg("-version");
    debug_print_command(&cmd);

    cmd.output()
        .map_err(|_| ())
        .and_then(|out| String::from_utf8(out.stdout).map_err(|_| ()))
        .and_then(|str| {
            str.split_whitespace()
                .nth(1)
                .map(|s| s.to_string())
                .ok_or(())
        })
        .and_then(|version| version.parse::<f32>().map_err(|_| ()))
        .unwrap_or(0.0)
}

fn link_wasm32(
    _target: Target,
    output_path: PathBuf,
    input_paths: &[&str],
    _link_type: LinkType,
) -> io::Result<(Child, PathBuf)> {
    let child = zig()
        // .env_clear()
        // .env("PATH", &env_path)
        .args(["build-exe"])
        .args(input_paths)
        .args([
            // include wasi libc
            // TOOD: This now compiles fine with `-lc`. That said, the output file doesn't work.
            // using `-lc` is broken in zig 8 (and early 9) in combination with ReleaseSmall
            find_wasi_libc_path().to_str().unwrap(),
            &format!("-femit-bin={}", output_path.to_str().unwrap()),
            "-target",
            "wasm32-wasi-musl",
            "-fstrip",
            "-O",
            "ReleaseSmall",
            "-rdynamic",
            // useful for debugging
            // "-femit-llvm-ir=/home/folkertdev/roc/roc/crates/cli/tests/benchmarks/platform/host.ll",
        ])
        .spawn()?;

    Ok((child, output_path))
}

fn link_windows(
    output_path: PathBuf,
    input_paths: &[&str],
    link_type: LinkType,
) -> io::Result<(Child, PathBuf)> {
    match link_type {
        LinkType::Dylib => {
            let child = zig()
                .args(["build-lib"])
                .args(input_paths)
                .args([
                    "-lc",
                    &format!("-femit-bin={}", output_path.to_str().unwrap()),
                    "-target",
                    "native",
                    "-O",
                    "Debug",
                    "-dynamic",
                ])
                .spawn()?;

            Ok((child, output_path))
        }
        LinkType::Executable => {
            let child = zig()
                .args(["build-exe"])
                .args(input_paths)
                .args([
                    "-target",
                    "native",
                    "--subsystem",
                    "console",
                    "-lc",
                    &format!("-femit-bin={}", output_path.to_str().unwrap()),
                ])
                .spawn()?;

            Ok((child, output_path))
        }
        LinkType::None => todo!(),
    }
}

pub fn llvm_module_to_dylib(
    module: &inkwell::module::Module,
    target: Target,
    opt_level: OptLevel,
) -> Result<Library, Error> {
    use crate::target::{self, convert_opt_level};
    use inkwell::targets::{FileType, RelocMode};

    let dir = tempfile::tempdir().unwrap();
    let filename = PathBuf::from("Test.roc");
    let file_path = dir.path().join(filename);
    let mut app_o_file = file_path;

    app_o_file.set_file_name("app.o");

    // Emit the .o file using position-independent code (PIC) - needed for dylibs
    let reloc = RelocMode::PIC;
    let target_machine =
        target::target_machine(target, convert_opt_level(opt_level), reloc).unwrap();

    target_machine
        .write_to_file(module, FileType::Object, &app_o_file)
        .expect("Writing .o file failed");

    // Link app.o into a dylib - e.g. app.so or app.dylib
    let (mut child, dylib_path) = link(
        target,
        app_o_file.clone(),
        &[app_o_file.to_str().unwrap()],
        LinkType::Dylib,
    )
    .unwrap();

    let exit_status = child.wait().unwrap();

    assert!(
        exit_status.success(),
        "\n___________\nLinking command failed with status {exit_status:?}:\n\n  {child:?}\n___________\n"
    );

    // Load the dylib
    let path = dylib_path.as_path().to_str().unwrap();

    if matches!(target.architecture(), Architecture::Aarch64) {
        // On AArch64 darwin machines, calling `ldopen` on Roc-generated libs from multiple threads
        // sometimes fails with
        //   cannot dlopen until fork() handlers have completed
        // This may be due to codesigning. In any case, spinning until we are able to dlopen seems
        // to be okay.
        loop {
            match unsafe { Library::new(path) } {
                Ok(lib) => return Ok(lib),
                Err(Error::DlOpen { .. }) => continue,
                Err(other) => return Err(other),
            }
        }
    }

    unsafe { Library::new(path) }
}

pub fn preprocess_host_wasm32(host_input_path: &Path, host_output_path: &Path) {
    let host_input = host_input_path.to_str().unwrap();
    let output_file = host_output_path.to_str().unwrap();

    /*
    Notes:
        zig build-obj just gives you back the first input file, doesn't combine them!
        zig build-lib works but doesn't emit relocations, even with --emit-relocs (bug?)
            (gen_wasm needs relocs for host-to-app calls and stack size adjustment)
        zig wasm-ld is a wrapper around wasm-ld and gives us maximum flexiblity
            (but seems to be an unofficial API)
    */

    let builtins_host_tempfile = roc_bitcode::host_wasm_tempfile()
        .expect("failed to write host builtins object to tempfile");

    let wasi_libc_path = find_wasi_libc_path();

    let mut zig_cmd = zig();
    let args = &[
        "wasm-ld",
        builtins_host_tempfile.path().to_str().unwrap(),
        host_input,
        wasi_libc_path.to_str().unwrap(),
        WASI_COMPILER_RT_PATH, // builtins need __multi3, __udivti3, __fixdfti
        "-o",
        output_file,
        "--export-all",
        "--no-entry",
        "--import-undefined",
        "--relocatable",
    ];

    zig_cmd.args(args);

    // println!("\npreprocess_host_wasm32");
    // println!("zig {}\n", args.join(" "));

    run_build_command(zig_cmd, output_file, 0);

    // Extend the lifetime of the tempfile so it doesn't get dropped
    // (and thus deleted) before the Zig process is done using it!
    let _ = builtins_host_tempfile;
}

fn run_build_command(mut command: Command, file_to_build: &str, flaky_fail_counter: usize) {
    let command_string = stringify_command(&command, false);
    let cmd_str = &command_string;

    roc_debug_flags::dbg_do!(roc_debug_flags::ROC_PRINT_BUILD_COMMANDS, {
        print_command_str(cmd_str);
    });

    let cmd_output = command.output().unwrap();
    let max_flaky_fail_count = 10;

    if !cmd_output.status.success() {
        match std::str::from_utf8(&cmd_output.stderr) {
            Ok(stderr) => {
                // flaky error seen on macos 12 apple silicon, related to https://github.com/ziglang/zig/issues/20501
                if stderr.contains("unable to save cached ZIR code") {
                    if flaky_fail_counter < max_flaky_fail_count {
                        run_build_command(command, file_to_build, flaky_fail_counter + 1)
                    } else {
                        internal_error!(
                            "Error:\n    Failed to rebuild {} {} times, this is not a flaky failure:\n        The executed command was:\n            {}\n        stderr of that command:\n            {}",
                            file_to_build,
                            max_flaky_fail_count,
                            cmd_str,
                            stderr
                        )
                    }
                } else {
                    internal_error!(
                        "Error:\n    Failed to rebuild {}:\n        The executed command was:\n            {}\n        stderr of that command:\n            {}",
                        file_to_build,
                        cmd_str,
                        stderr
                    )
                }
            },
            Err(utf8_err) => internal_error!(
                "Error:\n    Failed to rebuild {}:\n        The executed command was:\n            {}\n        stderr of that command could not be parsed as valid utf8:\n            {}",
                file_to_build,
                cmd_str,
                utf8_err
            ),
        }
    }
}

/// Stringify a command for printing
/// e.g. `HOME=~ zig build-exe foo.zig -o foo`
fn stringify_command(cmd: &Command, include_env_vars: bool) -> String {
    let mut command_string = std::ffi::OsString::new();

    if include_env_vars {
        for (name, opt_val) in cmd.get_envs() {
            command_string.push(name);
            command_string.push("=");
            if let Some(val) = opt_val {
                command_string.push(val);
            } else {
                command_string.push("''");
            }
            command_string.push(" ");
        }
    }

    command_string.push(cmd.get_program());

    for arg in cmd.get_args() {
        command_string.push(" ");
        command_string.push(arg);
    }

    String::from(command_string.to_str().unwrap())
}

#[cfg(debug_assertions)]
fn print_command_str(s: &str) {
    println!("\nRoc build command:\n{}\n", s);
}

fn debug_print_command(_cmd: &Command) {
    // This debug macro is compiled out in release mode, so the argument is unused
    roc_debug_flags::dbg_do!(roc_debug_flags::ROC_PRINT_BUILD_COMMANDS_WITH_ENV_VARS, {
        print_command_str(&stringify_command(_cmd, true));
    });

    roc_debug_flags::dbg_do!(roc_debug_flags::ROC_PRINT_BUILD_COMMANDS, {
        print_command_str(&stringify_command(_cmd, false));
    });
}
