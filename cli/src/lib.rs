#[macro_use]
extern crate const_format;

use build::BuiltFile;
use bumpalo::Bump;
use clap::{Arg, ArgMatches, Command};
use libc::{c_char, c_int};
use roc_build::link::{LinkType, LinkingStrategy};
use roc_collections::VecMap;
use roc_error_macros::{internal_error, user_error};
use roc_load::{Expectations, LoadingProblem, Threading};
use roc_module::symbol::{Interns, ModuleId};
use roc_mono::ir::OptLevel;
use roc_region::all::Region;
use roc_reporting::error::r#type::error_type_to_doc;
use roc_reporting::report::{Palette, Report, RocDocAllocator, RocDocBuilder};
use roc_target::TargetInfo;
use std::env;
use std::ffi::{CString, OsStr};
use std::io;
use std::path::{Path, PathBuf};
use std::process;
use target_lexicon::BinaryFormat;
use target_lexicon::{
    Architecture, Environment, OperatingSystem, Triple, Vendor, X86_32Architecture,
};
#[cfg(not(target_os = "linux"))]
use tempfile::TempDir;

pub mod build;
mod format;
pub use format::format;

pub const CMD_BUILD: &str = "build";
pub const CMD_RUN: &str = "run";
pub const CMD_REPL: &str = "repl";
pub const CMD_EDIT: &str = "edit";
pub const CMD_DOCS: &str = "docs";
pub const CMD_CHECK: &str = "check";
pub const CMD_VERSION: &str = "version";
pub const CMD_FORMAT: &str = "format";

pub const FLAG_DEBUG: &str = "debug";
pub const FLAG_DEV: &str = "dev";
pub const FLAG_OPTIMIZE: &str = "optimize";
pub const FLAG_MAX_THREADS: &str = "max-threads";
pub const FLAG_OPT_SIZE: &str = "opt-size";
pub const FLAG_LIB: &str = "lib";
pub const FLAG_NO_LINK: &str = "no-link";
pub const FLAG_TARGET: &str = "target";
pub const FLAG_TIME: &str = "time";
pub const FLAG_LINKER: &str = "linker";
pub const FLAG_PRECOMPILED: &str = "precompiled-host";
pub const FLAG_VALGRIND: &str = "valgrind";
pub const FLAG_CHECK: &str = "check";
pub const ROC_FILE: &str = "ROC_FILE";
pub const ROC_DIR: &str = "ROC_DIR";
pub const DIRECTORY_OR_FILES: &str = "DIRECTORY_OR_FILES";
pub const ARGS_FOR_APP: &str = "ARGS_FOR_APP";

const VERSION: &str = include_str!("../../version.txt");

pub fn build_app<'a>() -> Command<'a> {
    let flag_optimize = Arg::new(FLAG_OPTIMIZE)
        .long(FLAG_OPTIMIZE)
        .help("Optimize the compiled program to run faster. (Optimization takes time to complete.)")
        .requires(ROC_FILE)
        .required(false);

    let flag_max_threads = Arg::new(FLAG_MAX_THREADS)
        .long(FLAG_MAX_THREADS)
        .help("Limit the number of threads (and hence cores) used during compilation.")
        .requires(ROC_FILE)
        .takes_value(true)
        .validator(|s| s.parse::<usize>())
        .required(false);

    let flag_opt_size = Arg::new(FLAG_OPT_SIZE)
        .long(FLAG_OPT_SIZE)
        .help("Optimize the compiled program to have a small binary size. (Optimization takes time to complete.)")
        .required(false);

    let flag_dev = Arg::new(FLAG_DEV)
        .long(FLAG_DEV)
        .help("Make compilation finish as soon as possible, at the expense of runtime performance.")
        .required(false);

    let flag_debug = Arg::new(FLAG_DEBUG)
        .long(FLAG_DEBUG)
        .help("Store LLVM debug information in the generated program.")
        .requires(ROC_FILE)
        .required(false);

    let flag_valgrind = Arg::new(FLAG_VALGRIND)
        .long(FLAG_VALGRIND)
        .help("Some assembly instructions are not supported by valgrind, this flag prevents those from being output when building the host.")
        .required(false);

    let flag_time = Arg::new(FLAG_TIME)
        .long(FLAG_TIME)
        .help("Prints detailed compilation time information.")
        .required(false);

    let flag_linker = Arg::new(FLAG_LINKER)
        .long(FLAG_LINKER)
        .help("Sets which linker to use. The surgical linker is enabled by default only when building for wasm32 or x86_64 Linux, because those are the only targets it currently supports. Otherwise the legacy linker is used by default.")
        .possible_values(["surgical", "legacy"])
        .required(false);

    let flag_precompiled = Arg::new(FLAG_PRECOMPILED)
        .long(FLAG_PRECOMPILED)
        .help("Assumes the host has been precompiled and skips recompiling the host. (Enabled by default when using `roc build` with a --target other than `--target host`)")
        .possible_values(["true", "false"])
        .required(false);

    let roc_file_to_run = Arg::new(ROC_FILE)
        .help("The .roc file of an app to run")
        .allow_invalid_utf8(true);

    let args_for_app = Arg::new(ARGS_FOR_APP)
        .help("Arguments to pass into the app being run")
        .requires(ROC_FILE)
        .allow_invalid_utf8(true)
        .multiple_values(true);

    let app = Command::new("roc")
        .version(concatcp!(VERSION, "\n"))
        .about("Runs the given .roc file, if there are no compilation errors.\nUse one of the SUBCOMMANDS below to do something else!")
        .subcommand(Command::new(CMD_BUILD)
            .about("Build a binary from the given .roc file, but don't run it")
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_debug.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_precompiled.clone())
            .arg(flag_valgrind.clone())
            .arg(
                Arg::new(FLAG_TARGET)
                    .long(FLAG_TARGET)
                    .help("Choose a different target")
                    .default_value(Target::default().as_str())
                    .possible_values(Target::OPTIONS)
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_LIB)
                    .long(FLAG_LIB)
                    .help("Build a C library instead of an executable.")
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_NO_LINK)
                    .long(FLAG_NO_LINK)
                    .help("Does not link. Instead just outputs the `.o` file")
                    .required(false),
            )
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file to build")
                    .allow_invalid_utf8(true)
                    .required(true),
            )
        )
        .subcommand(Command::new(CMD_REPL)
            .about("Launch the interactive Read Eval Print Loop (REPL)")
        )
        .subcommand(Command::new(CMD_RUN)
            .about("Run a .roc file even if it has build errors")
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_debug.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_precompiled.clone())
            .arg(flag_valgrind.clone())
            .arg(roc_file_to_run.clone().required(true))
            .arg(args_for_app.clone())
        )
        .subcommand(Command::new(CMD_FORMAT)
            .about("Format a .roc file using standard Roc formatting")
            .arg(
                Arg::new(DIRECTORY_OR_FILES)
                    .index(1)
                    .multiple_values(true)
                    .required(false)
                    .allow_invalid_utf8(true))
            .arg(
                Arg::new(FLAG_CHECK)
                    .long(FLAG_CHECK)
                    .help("Checks that specified files are formatted. If formatting is needed, it will return a non-zero exit code.")
                    .required(false),
            )
        )
        .subcommand(Command::new(CMD_VERSION)
            .about(concatcp!("Print the Roc compiler’s version, which is currently ", VERSION)))
        .subcommand(Command::new(CMD_CHECK)
            .about("Check the code for problems, but doesn’t build or run it")
            .arg(flag_time.clone())
            .arg(flag_max_threads.clone())
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file of an app to check")
                    .allow_invalid_utf8(true)
                    .required(true),
            )
            )
        .subcommand(
            Command::new(CMD_DOCS)
                .about("Generate documentation for Roc modules (Work In Progress)")
                .arg(Arg::new(DIRECTORY_OR_FILES)
                    .multiple_values(true)
                    .required(false)
                    .help("The directory or files to build documentation for")
                    .allow_invalid_utf8(true)
                )
        )
        .trailing_var_arg(true)
        .arg(flag_optimize)
            .arg(flag_max_threads.clone())
        .arg(flag_opt_size)
        .arg(flag_dev)
        .arg(flag_debug)
        .arg(flag_time)
        .arg(flag_linker)
        .arg(flag_precompiled)
        .arg(flag_valgrind)
        .arg(roc_file_to_run.required(false))
        .arg(args_for_app);

    if cfg!(feature = "editor") {
        app.subcommand(
            Command::new(CMD_EDIT)
                .about("Launch the Roc editor (Work In Progress)")
                .arg(
                    Arg::new(DIRECTORY_OR_FILES)
                        .multiple_values(true)
                        .required(false)
                        .help("(optional) The directory or files to open on launch."),
                ),
        )
    } else {
        app
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BuildConfig {
    BuildOnly,
    BuildAndRun,
    BuildAndRunIfNoErrors,
}

pub enum FormatMode {
    Format,
    CheckOnly,
}

pub fn build(
    matches: &ArgMatches,
    config: BuildConfig,
    triple: Triple,
    link_type: LinkType,
) -> io::Result<i32> {
    use build::build_file;
    use BuildConfig::*;

    let arena = Bump::new();
    let filename = matches.value_of_os(ROC_FILE).unwrap();
    let opt_level = match (
        matches.is_present(FLAG_OPTIMIZE),
        matches.is_present(FLAG_OPT_SIZE),
        matches.is_present(FLAG_DEV),
    ) {
        (true, false, false) => OptLevel::Optimize,
        (false, true, false) => OptLevel::Size,
        (false, false, true) => OptLevel::Development,
        (false, false, false) => OptLevel::Normal,
        _ => user_error!("build can be only one of `--dev`, `--optimize`, or `--opt-size`"),
    };
    let emit_debug_info = matches.is_present(FLAG_DEBUG);
    let emit_timings = matches.is_present(FLAG_TIME);

    let threading = match matches
        .value_of(FLAG_MAX_THREADS)
        .and_then(|s| s.parse::<usize>().ok())
    {
        None => Threading::AllAvailable,
        Some(0) => user_error!("cannot build with at most 0 threads"),
        Some(1) => Threading::Single,
        Some(n) => Threading::AtMost(n),
    };

    let wasm_dev_backend = matches!(opt_level, OptLevel::Development)
        && matches!(triple.architecture, Architecture::Wasm32);

    let linking_strategy = if wasm_dev_backend {
        LinkingStrategy::Additive
    } else if !roc_linker::supported(&link_type, &triple)
        || matches.value_of(FLAG_LINKER) == Some("legacy")
    {
        LinkingStrategy::Legacy
    } else {
        LinkingStrategy::Surgical
    };

    let precompiled = if matches.is_present(FLAG_PRECOMPILED) {
        matches.value_of(FLAG_PRECOMPILED) == Some("true")
    } else {
        // When compiling for a different target, default to assuming a precompiled host.
        // Otherwise compilation would most likely fail because many toolchains assume you're compiling for the host
        // We make an exception for Wasm, because cross-compiling is the norm in that case.
        triple != Triple::host() && !matches!(triple.architecture, Architecture::Wasm32)
    };
    let path = Path::new(filename);

    // Spawn the root task
    let path = path.canonicalize().unwrap_or_else(|err| {
        use io::ErrorKind::*;

        match err.kind() {
            NotFound => {
                match path.to_str() {
                    Some(path_str) => println!("File not found: {}", path_str),
                    None => println!("Malformed file path : {:?}", path),
                }

                process::exit(1);
            }
            _ => {
                todo!("TODO Gracefully handle opening {:?} - {:?}", path, err);
            }
        }
    });

    unsafe {
        let name = "/roc_expect_buffer"; // IMPORTANT: shared memory object names must begin with / and contain no other slashes!
        let cstring = CString::new(name).unwrap();
        let shared_fd =
            libc::shm_open(cstring.as_ptr().cast(), libc::O_RDWR | libc::O_CREAT, 0o666);

        libc::ftruncate(shared_fd, 1024);

        let _shared_ptr = libc::mmap(
            std::ptr::null_mut(),
            4096,
            libc::PROT_WRITE,
            libc::MAP_SHARED,
            shared_fd,
            0,
        );
    }

    let src_dir = path.parent().unwrap().canonicalize().unwrap();
    let target_valgrind = matches.is_present(FLAG_VALGRIND);
    let res_binary_path = build_file(
        &arena,
        &triple,
        src_dir,
        path,
        opt_level,
        emit_debug_info,
        emit_timings,
        link_type,
        linking_strategy,
        precompiled,
        target_valgrind,
        threading,
    );

    match res_binary_path {
        Ok(BuiltFile {
            binary_path,
            problems,
            total_time,
            expectations,
            interns,
        }) => {
            match config {
                BuildOnly => {
                    // If possible, report the generated executable name relative to the current dir.
                    let generated_filename = binary_path
                        .strip_prefix(env::current_dir().unwrap())
                        .unwrap_or(&binary_path);

                    // No need to waste time freeing this memory,
                    // since the process is about to exit anyway.
                    std::mem::forget(arena);

                    println!(
                        "\x1B[{}m{}\x1B[39m {} and \x1B[{}m{}\x1B[39m {} found in {} ms while successfully building:\n\n    {}",
                        if problems.errors == 0 {
                            32 // green
                        } else {
                            33 // yellow
                        },
                        problems.errors,
                        if problems.errors == 1 {
                            "error"
                        } else {
                            "errors"
                        },
                        if problems.warnings == 0 {
                            32 // green
                        } else {
                            33 // yellow
                        },
                        problems.warnings,
                        if problems.warnings == 1 {
                            "warning"
                        } else {
                            "warnings"
                        },
                        total_time.as_millis(),
                        generated_filename.to_str().unwrap()
                    );

                    // Return a nonzero exit code if there were problems
                    Ok(problems.exit_code())
                }
                BuildAndRun => {
                    if problems.errors > 0 || problems.warnings > 0 {
                        println!(
                            "\x1B[{}m{}\x1B[39m {} and \x1B[{}m{}\x1B[39m {} found in {} ms.\n\nRunning program anyway…\n\n\x1B[36m{}\x1B[39m",
                            if problems.errors == 0 {
                                32 // green
                            } else {
                                33 // yellow
                            },
                            problems.errors,
                            if problems.errors == 1 {
                                "error"
                            } else {
                                "errors"
                            },
                            if problems.warnings == 0 {
                                32 // green
                            } else {
                                33 // yellow
                            },
                            problems.warnings,
                            if problems.warnings == 1 {
                                "warning"
                            } else {
                                "warnings"
                            },
                            total_time.as_millis(),
                            "─".repeat(80)
                        );
                    }

                    let args = matches.values_of_os(ARGS_FOR_APP).unwrap_or_default();

                    let mut bytes = std::fs::read(&binary_path).unwrap();

                    let x = roc_run(
                        arena,
                        triple,
                        opt_level,
                        args,
                        &mut bytes,
                        expectations,
                        interns,
                    );
                    std::mem::forget(bytes);
                    x
                }
                BuildAndRunIfNoErrors => {
                    if problems.errors == 0 {
                        if problems.warnings > 0 {
                            println!(
                                "\x1B[32m0\x1B[39m errors and \x1B[33m{}\x1B[39m {} found in {} ms.\n\nRunning program…\n\n\x1B[36m{}\x1B[39m",
                                problems.warnings,
                                if problems.warnings == 1 {
                                    "warning"
                                } else {
                                    "warnings"
                                },
                                total_time.as_millis(),
                                "─".repeat(80)
                            );
                        }

                        let args = matches.values_of_os(ARGS_FOR_APP).unwrap_or_default();

                        let mut bytes = std::fs::read(&binary_path).unwrap();

                        let x = roc_run(
                            arena,
                            triple,
                            opt_level,
                            args,
                            &mut bytes,
                            expectations,
                            interns,
                        );
                        std::mem::forget(bytes);
                        x
                    } else {
                        println!(
                            "\x1B[{}m{}\x1B[39m {} and \x1B[{}m{}\x1B[39m {} found in {} ms.\n\nYou can run the program anyway with: \x1B[32mroc run {}\x1B[39m",
                            if problems.errors == 0 {
                                32 // green
                            } else {
                                33 // yellow
                            },
                            problems.errors,
                            if problems.errors == 1 {
                                "error"
                            } else {
                                "errors"
                            },
                            if problems.warnings == 0 {
                                32 // green
                            } else {
                                33 // yellow
                            },
                            problems.warnings,
                            if problems.warnings == 1 {
                                "warning"
                            } else {
                                "warnings"
                            },
                            total_time.as_millis(),
                            filename.to_string_lossy()
                        );

                        Ok(problems.exit_code())
                    }
                }
            }
        }
        Err(LoadingProblem::FormattedReport(report)) => {
            print!("{}", report);

            Ok(1)
        }
        Err(other) => {
            panic!("build_file failed with error:\n{:?}", other);
        }
    }
}

fn roc_run<'a, I: IntoIterator<Item = &'a OsStr>>(
    arena: Bump, // This should be passed an owned value, not a reference, so we can usefully mem::forget it!
    triple: Triple,
    opt_level: OptLevel,
    args: I,
    binary_bytes: &mut [u8],
    expectations: VecMap<ModuleId, Expectations>,
    interns: Interns,
) -> io::Result<i32> {
    match triple.architecture {
        Architecture::Wasm32 => {
            let executable = roc_run_executable_file_path(binary_bytes)?;
            let path = executable.as_path();
            // If possible, report the generated executable name relative to the current dir.
            let generated_filename = path
                .strip_prefix(env::current_dir().unwrap())
                .unwrap_or(path);

            // No need to waste time freeing this memory,
            // since the process is about to exit anyway.
            std::mem::forget(arena);

            if cfg!(target_family = "unix") {
                use std::os::unix::ffi::OsStrExt;

                run_with_wasmer(
                    generated_filename,
                    args.into_iter().map(|os_str| os_str.as_bytes()),
                );
            } else {
                run_with_wasmer(
                    generated_filename,
                    args.into_iter().map(|os_str| {
                        os_str.to_str().expect(
                            "Roc does not currently support passing non-UTF8 arguments to Wasmer.",
                        )
                    }),
                );
            }

            Ok(0)
        }
        _ => roc_run_native(arena, opt_level, args, binary_bytes, expectations, interns),
    }
}

/// Run on the native OS (not on wasm)
#[cfg(target_family = "unix")]
fn roc_run_native<I: IntoIterator<Item = S>, S: AsRef<OsStr>>(
    arena: Bump,
    opt_level: OptLevel,
    args: I,
    binary_bytes: &mut [u8],
    expectations: VecMap<ModuleId, Expectations>,
    interns: Interns,
) -> std::io::Result<i32> {
    use bumpalo::collections::CollectIn;
    use std::os::unix::ffi::OsStrExt;

    unsafe {
        let executable = roc_run_executable_file_path(binary_bytes)?;
        let path = executable.as_path();
        let path_cstring = CString::new(path.as_os_str().as_bytes()).unwrap();

        // argv is an array of pointers to strings passed to the new program
        // as its command-line arguments.  By convention, the first of these
        // strings (i.e., argv[0]) should contain the filename associated
        // with the file being executed.  The argv array must be terminated
        // by a NULL pointer. (Thus, in the new program, argv[argc] will be NULL.)
        let c_strings: bumpalo::collections::Vec<CString> = args
            .into_iter()
            .map(|x| CString::new(x.as_ref().as_bytes()).unwrap())
            .collect_in(&arena);

        let c_string_pointers = c_strings
            .iter()
            .map(|x| x.as_bytes_with_nul().as_ptr().cast());

        let argv: bumpalo::collections::Vec<*const c_char> = std::iter::once(path_cstring.as_ptr())
            .chain(c_string_pointers)
            .chain([std::ptr::null()])
            .collect_in(&arena);

        // envp is an array of pointers to strings, conventionally of the
        // form key=value, which are passed as the environment of the new
        // program.  The envp array must be terminated by a NULL pointer.
        let envp_cstrings: bumpalo::collections::Vec<CString> = std::env::vars_os()
            .flat_map(|(k, v)| {
                [
                    CString::new(k.as_bytes()).unwrap(),
                    CString::new(v.as_bytes()).unwrap(),
                ]
            })
            .collect_in(&arena);

        let envp: bumpalo::collections::Vec<*const c_char> = envp_cstrings
            .iter()
            .map(|s| s.as_ptr())
            .chain([std::ptr::null()])
            .collect_in(&arena);

        //        match opt_level {
        //            OptLevel::Development => roc_run_native_debug(executable, &argv, &envp),
        //            OptLevel::Normal | OptLevel::Size | OptLevel::Optimize => {
        //                roc_run_native_fast(executable, &argv, &envp);
        //            }
        //        }

        roc_run_native_debug(executable, &argv, &envp, expectations, interns)
    }

    Ok(1)
}

unsafe fn roc_run_native_fast(
    executable: ExecutableFile,
    argv: &[*const c_char],
    envp: &[*const c_char],
) {
    match executable {
        #[cfg(target_os = "linux")]
        ExecutableFile::MemFd(fd, path) => {
            if libc::fexecve(fd, argv.as_ptr(), envp.as_ptr()) != 0 {
                internal_error!(
                    "libc::fexecve({:?}, ..., ...) failed: {:?}",
                    path,
                    errno::errno()
                );
            }
        }

        #[cfg(all(target_family = "unix", not(target_os = "linux")))]
        ExecutableFile::OnDisk(_, path) => {
            use std::os::unix::ffi::OsStrExt;

            let path_cstring = CString::new(path.as_os_str().as_bytes()).unwrap();
            if libc::execve(path_cstring.as_ptr().cast(), argv.as_ptr(), envp.as_ptr()) != 0 {
                internal_error!(
                    "libc::execve({:?}, ..., ...) failed: {:?}",
                    path,
                    errno::errno()
                );
            }
        }
    }
}

// with Expect
unsafe fn roc_run_native_debug(
    executable: ExecutableFile,
    argv: &[*const c_char],
    envp: &[*const c_char],
    mut expectations: VecMap<ModuleId, Expectations>,
    interns: Interns,
) {
    use signal_hook::{consts::signal::SIGCHLD, consts::signal::SIGUSR1, iterator::Signals};
    use std::os::unix::ffi::OsStrExt;

    let mut signals = Signals::new(&[SIGCHLD, SIGUSR1]).unwrap();

    let parent_pid = std::process::id();

    match libc::fork() {
        0 => {
            // we are the child

            if executable.execve(argv, envp) < 0 {
                // Get the current value of errno
                let e = errno::errno();

                // Extract the error code as an i32
                let code = e.0;

                // Display a human-friendly error message
                println!("💥 Error {}: {}", code, e);
            }
        }
        -1 => {
            // something failed

            // Get the current value of errno
            let e = errno::errno();

            // Extract the error code as an i32
            let code = e.0;

            // Display a human-friendly error message
            println!("Error {}: {}", code, e);

            process::exit(1)
        }
        1.. => {
            let name = "/roc_expect_buffer"; // IMPORTANT: shared memory object names must begin with / and contain no other slashes!
            let cstring = CString::new(name).unwrap();

            let arena = &bumpalo::Bump::new();
            let interns = arena.alloc(interns);

            for sig in &mut signals {
                match sig {
                    SIGCHLD => {
                        // clean up
                        libc::shm_unlink(cstring.as_ptr().cast());

                        // dbg!(libc::unlink(app_path.as_ptr().cast()));

                        // done!
                        process::exit(0);
                    }
                    SIGUSR1 => {
                        // this is the signal we use for an expect failure. Let's see what the child told us
                        let shared_fd =
                            libc::shm_open(cstring.as_ptr().cast(), libc::O_RDONLY, 0o666);

                        let shared_ptr = libc::mmap(
                            std::ptr::null_mut(),
                            4096,
                            libc::PROT_READ,
                            libc::MAP_SHARED,
                            shared_fd,
                            0,
                        );

                        let shared_memory_ptr: *const u8 = shared_ptr.cast();

                        render_expect_failure(arena, &mut expectations, interns, shared_memory_ptr);
                    }
                    _ => println!("received signal {}", sig),
                }
            }
        }
        _ => unreachable!(),
    }
}

fn render_expect_failure<'a>(
    arena: &'a Bump,
    expectations: &mut VecMap<ModuleId, Expectations>,
    interns: &'a Interns,
    start: *const u8,
) {
    use ven_pretty::DocAllocator;

    // we always run programs as the host
    let target_info = (&target_lexicon::Triple::host()).into();

    let region_bytes: [u8; 8] = unsafe { *(start.cast()) };
    let region: Region = unsafe { std::mem::transmute(region_bytes) };

    let module_id_bytes: [u8; 4] = unsafe { *(start.add(8).cast()) };
    let module_id: ModuleId = unsafe { std::mem::transmute(module_id_bytes) };

    let data = expectations.get_mut(&module_id).unwrap();
    let current = data.expectations.get(&region).unwrap();
    let subs = arena.alloc(&mut data.subs);

    // TODO cache these line offsets?
    let path = &data.path;
    let filename = data.path.to_owned();
    let file_string = std::fs::read_to_string(path).unwrap();
    let src_lines: Vec<_> = file_string.lines().collect();

    let line_info = roc_region::all::LineInfo::new(&file_string);
    let line_col_region = line_info.convert_region(region);

    let alloc = RocDocAllocator::new(&src_lines, module_id, interns);

    // 8 bytes for region, 4 for module id
    let start_offset = 12;

    let (symbols, variables): (Vec<_>, Vec<_>) = current.iter().map(|(a, b)| (*a, *b)).unzip();

    let error_types: Vec<_> = variables
        .iter()
        .map(|variable| {
            let (error_type, _) = subs.var_to_error_type(*variable);
            error_type
        })
        .collect();

    let expressions = roc_repl_expect::get_values(
        module_id,
        target_info,
        arena,
        subs,
        interns,
        start,
        start_offset,
        &variables,
    )
    .unwrap();

    use roc_fmt::annotation::Formattable;

    let it =
        symbols
            .iter()
            .zip(expressions)
            .zip(error_types)
            .map(|((symbol, expr), error_type)| {
                let mut buf = roc_fmt::Buf::new_in(arena);
                expr.format(&mut buf, 0);

                alloc.vcat([
                    alloc
                        .symbol_unqualified(*symbol)
                        .append(" : ")
                        .append(error_type_to_doc(&alloc, error_type)),
                    alloc
                        .symbol_unqualified(*symbol)
                        .append(" = ")
                        .append(buf.into_bump_str()),
                ])
            });

    let doc = alloc.stack([
        alloc.text("This expectation failed:"),
        alloc.region(line_col_region),
        alloc.text("The variables used in this expression are:"),
        alloc.stack(it),
    ]);

    let report = Report {
        title: "EXPECT FAILED".into(),
        doc,
        filename,
        severity: roc_reporting::report::Severity::RuntimeError,
    };

    let mut buf = String::new();

    report.render(
        roc_reporting::report::RenderTarget::ColorTerminal,
        &mut buf,
        &alloc,
        &roc_reporting::report::DEFAULT_PALETTE,
    );

    println!("{}", buf);
}

#[derive(Debug)]
enum ExecutableFile {
    #[cfg(target_os = "linux")]
    MemFd(c_int, PathBuf),
    #[cfg(all(target_family = "unix", not(target_os = "linux")))]
    OnDisk(TempDir, PathBuf),
}

impl ExecutableFile {
    pub fn as_path(&self) -> &Path {
        match self {
            #[cfg(target_os = "linux")]
            ExecutableFile::MemFd(_, path_buf) => path_buf.as_ref(),
            #[cfg(all(target_family = "unix", not(target_os = "linux")))]
            ExecutableFile::OnDisk(_, path_buf) => path_buf.as_ref(),
        }
    }

    #[cfg(target_os = "linux")]
    pub unsafe fn execve(&self, argv: &[*const c_char], envp: &[*const c_char]) -> c_int {
        let ExecutableFile::MemFd(fd, _) = self;

        libc::fexecve(*fd, argv.as_ptr(), envp.as_ptr())
    }

    #[cfg(all(target_family = "unix", not(target_os = "linux")))]
    pub unsafe fn execve(&self, argv: &[*const c_char], envp: &[*const c_char]) -> c_int {
        use std::os::unix::ffi::OsStrExt;

        let ExecutableFile::OnDisk(dir, path) = self;
        let path_cstring = CString::new(path.as_os_str().as_bytes()).unwrap();

        libc::execve(path_cstring.as_ptr().cast(), argv.as_ptr(), envp.as_ptr())
    }
}

#[cfg(target_os = "linux")]
fn roc_run_executable_file_path(binary_bytes: &mut [u8]) -> std::io::Result<ExecutableFile> {
    // on linux, we use the `memfd_create` function to create an in-memory anonymous file.
    let flags = 0;
    let anonymous_file_name = "roc_file_descriptor\0";
    let fd = unsafe { libc::memfd_create(anonymous_file_name.as_ptr().cast(), flags) };

    if fd == 0 {
        internal_error!(
            "libc::memfd_create({:?}, {}) failed: file descriptor is 0",
            anonymous_file_name,
            flags
        );
    }

    let path = PathBuf::from(format!("/proc/self/fd/{}", fd));

    std::fs::write(&path, binary_bytes)?;

    Ok(ExecutableFile::MemFd(fd, path))
}

#[cfg(all(target_family = "unix", not(target_os = "linux")))]
fn roc_run_executable_file_path(binary_bytes: &mut [u8]) -> std::io::Result<ExecutableFile> {
    use std::fs::OpenOptions;
    use std::io::Write;
    use std::os::unix::fs::OpenOptionsExt;

    let temp_dir = tempfile::tempdir()?;

    // We have not found a way to use a virtual file on non-Linux OSes.
    // Hence we fall back to just writing the file to the file system, and using that file.
    let app_path_buf = temp_dir.path().join("roc_app_binary");
    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .mode(0o777) // create the file as executable
        .open(&app_path_buf)?;

    file.write_all(binary_bytes)?;

    // We store the TempDir in this variant alongside the path to the executable,
    // so that the TempDir doesn't get dropped until after we're done with the path.
    // If we didn't do that, then the tempdir would potentially get deleted by the
    // TempDir's Drop impl before the file had been executed.
    Ok(ExecutableFile::OnDisk(temp_dir, app_path_buf))
}

/// Run on the native OS (not on wasm)
#[cfg(not(target_family = "unix"))]
fn roc_run_native<I: IntoIterator<Item = S>, S: AsRef<OsStr>>(
    _arena: Bump, // This should be passed an owned value, not a reference, so we can usefully mem::forget it!
    _opt_level: OptLevel,
    _args: I,
    _binary_bytes: &mut [u8],
) -> io::Result<i32> {
    todo!("TODO support running roc programs on non-UNIX targets");
    // let mut cmd = std::process::Command::new(&binary_path);

    // // Run the compiled app
    // let exit_status = cmd
    //     .spawn()
    //     .unwrap_or_else(|err| panic!("Failed to run app after building it: {:?}", err))
    //     .wait()
    //     .expect("TODO gracefully handle block_on failing when `roc` spawns a subprocess for the compiled app");

    // // `roc [FILE]` exits with the same status code as the app it ran.
    // //
    // // If you want to know whether there were compilation problems
    // // via status code, use either `roc build` or `roc check` instead!
    // match exit_status.code() {
    //     Some(code) => Ok(code),
    //     None => {
    //         todo!("TODO gracefully handle the `roc [FILE]` subprocess terminating with a signal.");
    //     }
    // }
}

#[cfg(feature = "run-wasm32")]
fn run_with_wasmer<I: Iterator<Item = S>, S: AsRef<[u8]>>(wasm_path: &std::path::Path, args: I) {
    use wasmer::{Instance, Module, Store};

    let store = Store::default();
    let module = Module::from_file(&store, &wasm_path).unwrap();

    // First, we create the `WasiEnv`
    use wasmer_wasi::WasiState;
    let mut wasi_env = WasiState::new("hello").args(args).finalize().unwrap();

    // Then, we get the import object related to our WASI
    // and attach it to the Wasm instance.
    let import_object = wasi_env.import_object(&module).unwrap();

    let instance = Instance::new(&module, &import_object).unwrap();

    let start = instance.exports.get_function("_start").unwrap();

    use wasmer_wasi::WasiError;
    match start.call(&[]) {
        Ok(_) => {}
        Err(e) => match e.downcast::<WasiError>() {
            Ok(WasiError::Exit(0)) => {
                // we run the `_start` function, so exit(0) is expected
            }
            other => panic!("Wasmer error: {:?}", other),
        },
    }
}

#[cfg(not(feature = "run-wasm32"))]
fn run_with_wasmer<I: Iterator<Item = S>, S: AsRef<[u8]>>(_wasm_path: &std::path::Path, _args: I) {
    println!("Running wasm files is not supported on this target.");
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Target {
    System,
    Linux32,
    Linux64,
    Wasm32,
}

impl Default for Target {
    fn default() -> Self {
        Target::System
    }
}

impl Target {
    const fn as_str(&self) -> &'static str {
        use Target::*;

        match self {
            System => "system",
            Linux32 => "linux32",
            Linux64 => "linux64",
            Wasm32 => "wasm32",
        }
    }

    /// NOTE keep up to date!
    const OPTIONS: &'static [&'static str] = &[
        Target::System.as_str(),
        Target::Linux32.as_str(),
        Target::Linux64.as_str(),
        Target::Wasm32.as_str(),
    ];

    pub fn to_triple(self) -> Triple {
        use Target::*;

        match self {
            System => Triple::host(),
            Linux32 => Triple {
                architecture: Architecture::X86_32(X86_32Architecture::I386),
                vendor: Vendor::Unknown,
                operating_system: OperatingSystem::Linux,
                environment: Environment::Musl,
                binary_format: BinaryFormat::Elf,
            },
            Linux64 => Triple {
                architecture: Architecture::X86_64,
                vendor: Vendor::Unknown,
                operating_system: OperatingSystem::Linux,
                environment: Environment::Musl,
                binary_format: BinaryFormat::Elf,
            },
            Wasm32 => Triple {
                architecture: Architecture::Wasm32,
                vendor: Vendor::Unknown,
                operating_system: OperatingSystem::Unknown,
                environment: Environment::Unknown,
                binary_format: BinaryFormat::Wasm,
            },
        }
    }
}

impl From<&Target> for Triple {
    fn from(target: &Target) -> Self {
        target.to_triple()
    }
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl std::str::FromStr for Target {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        match string {
            "system" => Ok(Target::System),
            "linux32" => Ok(Target::Linux32),
            "linux64" => Ok(Target::Linux64),
            "wasm32" => Ok(Target::Wasm32),
            _ => Err(format!("Roc does not know how to compile to {}", string)),
        }
    }
}
