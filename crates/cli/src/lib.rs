#[macro_use]
extern crate const_format;

use build::BuiltFile;
use bumpalo::Bump;
use clap::{Arg, ArgMatches, Command, ValueSource};
use roc_build::link::{LinkType, LinkingStrategy};
use roc_collections::VecMap;
use roc_error_macros::{internal_error, user_error};
use roc_load::{Expectations, LoadingProblem, Threading};
use roc_module::symbol::{Interns, ModuleId};
use roc_mono::ir::OptLevel;
use std::env;
use std::ffi::{CString, OsStr};
use std::io;
use std::mem::ManuallyDrop;
use std::os::raw::{c_char, c_int};
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

use crate::build::{BuildFileError, BuildOrdering};

const DEFAULT_ROC_FILENAME: &str = "main.roc";

pub const CMD_BUILD: &str = "build";
pub const CMD_RUN: &str = "run";
pub const CMD_DEV: &str = "dev";
pub const CMD_REPL: &str = "repl";
pub const CMD_EDIT: &str = "edit";
pub const CMD_DOCS: &str = "docs";
pub const CMD_CHECK: &str = "check";
pub const CMD_VERSION: &str = "version";
pub const CMD_FORMAT: &str = "format";
pub const CMD_TEST: &str = "test";
pub const CMD_GLUE: &str = "glue";

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
pub const FLAG_CHECK: &str = "check";
pub const FLAG_WASM_STACK_SIZE_KB: &str = "wasm-stack-size-kb";
pub const ROC_FILE: &str = "ROC_FILE";
pub const ROC_DIR: &str = "ROC_DIR";
pub const GLUE_FILE: &str = "GLUE_FILE";
pub const DIRECTORY_OR_FILES: &str = "DIRECTORY_OR_FILES";
pub const ARGS_FOR_APP: &str = "ARGS_FOR_APP";

const VERSION: &str = include_str!("../../../version.txt");

pub fn build_app<'a>() -> Command<'a> {
    let flag_optimize = Arg::new(FLAG_OPTIMIZE)
        .long(FLAG_OPTIMIZE)
        .help("Optimize the compiled program to run faster\n(Optimization takes time to complete.)")
        .required(false);

    let flag_max_threads = Arg::new(FLAG_MAX_THREADS)
        .long(FLAG_MAX_THREADS)
        .help("Limit the number of threads (and hence cores) used during compilation")
        .takes_value(true)
        .validator(|s| s.parse::<usize>())
        .required(false);

    let flag_opt_size = Arg::new(FLAG_OPT_SIZE)
        .long(FLAG_OPT_SIZE)
        .help("Optimize the compiled program to have a small binary size\n(Optimization takes time to complete.)")
        .required(false);

    let flag_dev = Arg::new(FLAG_DEV)
        .long(FLAG_DEV)
        .help("Make compilation finish as soon as possible, at the expense of runtime performance")
        .required(false);

    let flag_debug = Arg::new(FLAG_DEBUG)
        .long(FLAG_DEBUG)
        .help("Store LLVM debug information in the generated program")
        .required(false);

    let flag_time = Arg::new(FLAG_TIME)
        .long(FLAG_TIME)
        .help("Print detailed compilation time information")
        .required(false);

    let flag_linker = Arg::new(FLAG_LINKER)
        .long(FLAG_LINKER)
        .help("Set which linker to use\n(The surgical linker is enabled by default only when building for wasm32 or x86_64 Linux, because those are the only targets it currently supports. Otherwise the legacy linker is used by default.)")
        .possible_values(["surgical", "legacy"])
        .required(false);

    let flag_precompiled = Arg::new(FLAG_PRECOMPILED)
        .long(FLAG_PRECOMPILED)
        .help("Assume the host has been precompiled and skip recompiling the host\n(This is enabled by default when using `roc build` with a --target other than `--target host`.)")
        .possible_values(["true", "false"])
        .required(false);

    let flag_wasm_stack_size_kb = Arg::new(FLAG_WASM_STACK_SIZE_KB)
        .long(FLAG_WASM_STACK_SIZE_KB)
        .help("Stack size in kilobytes for wasm32 target\n(This only applies when --dev also provided.)")
        .takes_value(true)
        .validator(|s| s.parse::<u32>())
        .required(false);

    let roc_file_to_run = Arg::new(ROC_FILE)
        .help("The .roc file of an app to run")
        .allow_invalid_utf8(true)
        .required(false)
        .default_value(DEFAULT_ROC_FILENAME);

    let args_for_app = Arg::new(ARGS_FOR_APP)
        .help("Arguments to pass into the app being run\ne.g. `roc run -- arg1 arg2`")
        .allow_invalid_utf8(true)
        .multiple_values(true)
        .takes_value(true)
        .allow_hyphen_values(true)
        .last(true);

    let app = Command::new("roc")
        .version(concatcp!(VERSION, "\n"))
        .about("Run the given .roc file, if there are no compilation errors.\nYou can use one of the SUBCOMMANDS below to do something else!")
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
            .arg(flag_wasm_stack_size_kb.clone())
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
                    .help("Build a C library instead of an executable")
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_NO_LINK)
                    .long(FLAG_NO_LINK)
                    .help("Do not link\n(Instead, just output the `.o` file.)")
                    .required(false),
            )
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file to build")
                    .allow_invalid_utf8(true)
                    .required(false)
                    .default_value(DEFAULT_ROC_FILENAME),
            )
        )
        .subcommand(Command::new(CMD_TEST)
            .about("Run all top-level `expect`s in a main module and any modules it imports")
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_debug.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_precompiled.clone())
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file for the main module")
                    .allow_invalid_utf8(true)
                    .required(false)
                    .default_value(DEFAULT_ROC_FILENAME)
            )
            .arg(args_for_app.clone())
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
            .arg(roc_file_to_run.clone())
            .arg(args_for_app.clone())
        )
        .subcommand(Command::new(CMD_DEV)
            .about("`check` a .roc file, and then run it if there were no errors")
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_debug.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_precompiled.clone())
            .arg(roc_file_to_run.clone())
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
                    .help("Checks that specified files are formatted\n(If formatting is needed, return a non-zero exit code.)")
                    .required(false),
            )
        )
        .subcommand(Command::new(CMD_VERSION)
            .about(concatcp!("Print the Roc compiler’s version, which is currently ", VERSION)))
        .subcommand(Command::new(CMD_CHECK)
            .about("Check the code for problems, but don’t build or run it")
            .arg(flag_time.clone())
            .arg(flag_max_threads.clone())
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file of an app to check")
                    .allow_invalid_utf8(true)
                    .required(false)
                    .default_value(DEFAULT_ROC_FILENAME),
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
        .subcommand(Command::new(CMD_GLUE)
            .about("Generate glue code between a platform's Roc API and its host language")
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file for the platform module")
                    .allow_invalid_utf8(true)
                    .required(true)
            )
            .arg(
                Arg::new(GLUE_FILE)
                    .help("The filename for the generated glue code\n(Currently, this must be a .rs file because only Rust glue generation is supported so far.)")
                    .allow_invalid_utf8(true)
                    .required(true)
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
                        .help("(optional) The directory or files to open on launch"),
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

#[cfg(windows)]
pub fn test(_matches: &ArgMatches, _triple: Triple) -> io::Result<i32> {
    todo!("running tests does not work on windows right now")
}

#[cfg(not(windows))]
pub fn test(matches: &ArgMatches, triple: Triple) -> io::Result<i32> {
    use roc_gen_llvm::llvm::build::LlvmBackendMode;
    use roc_load::{ExecutionMode, LoadConfig};
    use roc_target::TargetInfo;
    use std::time::Instant;

    let start_time = Instant::now();
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

    let threading = match matches
        .value_of(FLAG_MAX_THREADS)
        .and_then(|s| s.parse::<usize>().ok())
    {
        None => Threading::AllAvailable,
        Some(0) => user_error!("cannot build with at most 0 threads"),
        Some(1) => Threading::Single,
        Some(n) => Threading::AtMost(n),
    };

    let path = Path::new(filename);

    // Spawn the root task
    if !path.exists() {
        let path_string = path.to_string_lossy();

        // TODO these should use roc_reporting to display nicer error messages.
        match matches.value_source(ROC_FILE) {
            Some(ValueSource::DefaultValue) => {
                eprintln!(
                    "\nNo `.roc` file was specified, and the current directory does not contain a {} file to use as a default.\n\nYou can run `roc help` for more information on how to provide a .roc file.\n",
                    DEFAULT_ROC_FILENAME
                )
            }
            _ => eprintln!("\nThis file was not found: {}\n\nYou can run `roc help` for more information on how to provide a .roc file.\n", path_string),
        }

        process::exit(1);
    }

    let arena = &arena;
    let target = &triple;
    let opt_level = opt_level;
    let target_info = TargetInfo::from(target);

    // Step 1: compile the app and generate the .o file
    let subs_by_module = Default::default();

    let load_config = LoadConfig {
        target_info,
        // TODO: expose this from CLI?
        render: roc_reporting::report::RenderTarget::ColorTerminal,
        threading,
        exec_mode: ExecutionMode::Test,
    };
    let loaded =
        roc_load::load_and_monomorphize(arena, path.to_path_buf(), subs_by_module, load_config)
            .unwrap();

    let mut loaded = loaded;
    let mut expectations = std::mem::take(&mut loaded.expectations);
    let loaded = loaded;

    let interns = loaded.interns.clone();

    let (lib, expects, layout_interner) = roc_repl_expect::run::expect_mono_module_to_dylib(
        arena,
        target.clone(),
        loaded,
        opt_level,
        LlvmBackendMode::CliTest,
    )
    .unwrap();

    let arena = &bumpalo::Bump::new();
    let interns = arena.alloc(interns);

    let mut writer = std::io::stdout();

    let (failed, passed) = roc_repl_expect::run::run_expects(
        &mut writer,
        roc_reporting::report::RenderTarget::ColorTerminal,
        arena,
        interns,
        &layout_interner.into_global(),
        &lib,
        &mut expectations,
        expects,
    )
    .unwrap();

    let total_time = start_time.elapsed();

    if failed == 0 && passed == 0 {
        // TODO print this in a more nicely formatted way!
        println!("No expectations were found.");

        // If no tests ran, treat that as an error. This is perhaps
        // briefly annoying at the very beginning of a project when
        // you actually have zero tests, but it can save you from
        // having a change to your CI script accidentally stop
        // running tests altogether!
        Ok(2)
    } else {
        let failed_color = if failed == 0 {
            32 // green
        } else {
            31 // red
        };

        println!(
            "\n\x1B[{failed_color}m{failed}\x1B[39m failed and \x1B[32m{passed}\x1B[39m passed in {} ms.\n",
            total_time.as_millis(),
        );

        Ok((failed > 0) as i32)
    }
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
    } else if !roc_linker::supported(link_type, &triple)
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
    if !path.exists() {
        let path_string = path.to_string_lossy();

        // TODO these should use roc_reporting to display nicer error messages.
        match matches.value_source(ROC_FILE) {
                    Some(ValueSource::DefaultValue) => {
                        eprintln!(
                            "\nNo `.roc` file was specified, and the current directory does not contain a {} file to use as a default.\n\nYou can run `roc help` for more information on how to provide a .roc file.\n",
                            DEFAULT_ROC_FILENAME
                        )
                    }
                    _ => eprintln!("\nThis file was not found: {}\n\nYou can run `roc help` for more information on how to provide a .roc file.\n", path_string),
                }

        process::exit(1);
    }

    let wasm_dev_stack_bytes: Option<u32> = matches
        .try_get_one::<&str>(FLAG_WASM_STACK_SIZE_KB)
        .ok()
        .flatten()
        .and_then(|s| s.parse::<u32>().ok())
        .map(|x| x * 1024);

    let build_ordering = match config {
        BuildAndRunIfNoErrors => BuildOrdering::BuildIfChecks,
        _ => BuildOrdering::AlwaysBuild,
    };
    let res_binary_path = build_file(
        &arena,
        &triple,
        path.to_path_buf(),
        opt_level,
        emit_debug_info,
        emit_timings,
        link_type,
        linking_strategy,
        precompiled,
        threading,
        wasm_dev_stack_bytes,
        build_ordering,
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

                    let bytes = std::fs::read(&binary_path).unwrap();

                    let x = roc_run(
                        arena,
                        opt_level,
                        triple,
                        args,
                        &bytes,
                        expectations,
                        interns,
                    );
                    std::mem::forget(bytes);
                    x
                }
                BuildAndRunIfNoErrors => {
                    debug_assert!(
                        problems.errors == 0,
                        "if there are errors, they should have been returned as an error variant"
                    );
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

                    // ManuallyDrop will leak the bytes because we don't drop manually
                    let bytes = &ManuallyDrop::new(std::fs::read(&binary_path).unwrap());

                    roc_run(arena, opt_level, triple, args, bytes, expectations, interns)
                }
            }
        }
        Err(BuildFileError::ErrorModule {
            mut module,
            total_time,
        }) => {
            debug_assert!(module.total_problems() > 0);

            let problems = roc_build::program::report_problems_typechecked(&mut module);

            let mut output = format!(
                "\x1B[{}m{}\x1B[39m {} and \x1B[{}m{}\x1B[39m {} found in {} ms.\n\nYou can run the program anyway with \x1B[32mroc run",
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
            );
            // If you're running "main.roc" then you can just do `roc run`
            // to re-run the program.
            if filename != DEFAULT_ROC_FILENAME {
                output.push(' ');
                output.push_str(&filename.to_string_lossy());
            }

            println!("{}\x1B[39m", output);

            Ok(problems.exit_code())
        }
        Err(BuildFileError::LoadingProblem(LoadingProblem::FormattedReport(report))) => {
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
    opt_level: OptLevel,
    triple: Triple,
    args: I,
    binary_bytes: &[u8],
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

            #[cfg(target_family = "unix")]
            {
                use std::os::unix::ffi::OsStrExt;

                run_with_wasmer(
                    generated_filename,
                    args.into_iter().map(|os_str| os_str.as_bytes()),
                );
            }

            #[cfg(not(target_family = "unix"))]
            {
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

#[cfg(target_family = "unix")]
fn make_argv_envp<'a, I: IntoIterator<Item = S>, S: AsRef<OsStr>>(
    arena: &'a Bump,
    executable: &ExecutableFile,
    args: I,
) -> (
    bumpalo::collections::Vec<'a, CString>,
    bumpalo::collections::Vec<'a, CString>,
) {
    use bumpalo::collections::CollectIn;
    use std::os::unix::ffi::OsStrExt;

    let path = executable.as_path();
    let path_cstring = CString::new(path.as_os_str().as_bytes()).unwrap();

    // argv is an array of pointers to strings passed to the new program
    // as its command-line arguments.  By convention, the first of these
    // strings (i.e., argv[0]) should contain the filename associated
    // with the file being executed.  The argv array must be terminated
    // by a NULL pointer. (Thus, in the new program, argv[argc] will be NULL.)
    let it = args
        .into_iter()
        .map(|x| CString::new(x.as_ref().as_bytes()).unwrap());

    let argv_cstrings: bumpalo::collections::Vec<CString> =
        std::iter::once(path_cstring).chain(it).collect_in(arena);

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
        .collect_in(arena);

    (argv_cstrings, envp_cstrings)
}

/// Run on the native OS (not on wasm)
#[cfg(target_family = "unix")]
fn roc_run_native<I: IntoIterator<Item = S>, S: AsRef<OsStr>>(
    arena: Bump,
    opt_level: OptLevel,
    args: I,
    binary_bytes: &[u8],
    expectations: VecMap<ModuleId, Expectations>,
    interns: Interns,
) -> std::io::Result<i32> {
    use bumpalo::collections::CollectIn;

    unsafe {
        let executable = roc_run_executable_file_path(binary_bytes)?;
        let (argv_cstrings, envp_cstrings) = make_argv_envp(&arena, &executable, args);

        let argv: bumpalo::collections::Vec<*const c_char> = argv_cstrings
            .iter()
            .map(|s| s.as_ptr())
            .chain([std::ptr::null()])
            .collect_in(&arena);

        let envp: bumpalo::collections::Vec<*const c_char> = envp_cstrings
            .iter()
            .map(|s| s.as_ptr())
            .chain([std::ptr::null()])
            .collect_in(&arena);

        match opt_level {
            OptLevel::Development => {
                roc_run_native_debug(executable, &argv, &envp, expectations, interns)
            }
            OptLevel::Normal | OptLevel::Size | OptLevel::Optimize => {
                roc_run_native_fast(executable, &argv, &envp);
            }
        }
    }

    Ok(1)
}

unsafe fn roc_run_native_fast(
    executable: ExecutableFile,
    argv: &[*const c_char],
    envp: &[*const c_char],
) {
    if executable.execve(argv, envp) != 0 {
        internal_error!(
            "libc::{}({:?}, ..., ...) failed: {:?}",
            ExecutableFile::SYSCALL,
            executable.as_path(),
            errno::errno()
        );
    }
}

#[derive(Debug)]
enum ExecutableFile {
    #[cfg(target_os = "linux")]
    MemFd(libc::c_int, PathBuf),
    #[cfg(not(target_os = "linux"))]
    OnDisk(TempDir, PathBuf),
}

impl ExecutableFile {
    #[cfg(target_os = "linux")]
    const SYSCALL: &'static str = "fexecve";

    #[cfg(not(target_os = "linux"))]
    const SYSCALL: &'static str = "execve";

    fn as_path(&self) -> &Path {
        match self {
            #[cfg(target_os = "linux")]
            ExecutableFile::MemFd(_, path_buf) => path_buf.as_ref(),
            #[cfg(not(target_os = "linux"))]
            ExecutableFile::OnDisk(_, path_buf) => path_buf.as_ref(),
        }
    }

    unsafe fn execve(&self, argv: &[*const c_char], envp: &[*const c_char]) -> c_int {
        match self {
            #[cfg(target_os = "linux")]
            ExecutableFile::MemFd(fd, _path) => libc::fexecve(*fd, argv.as_ptr(), envp.as_ptr()),

            #[cfg(all(target_family = "unix", not(target_os = "linux")))]
            ExecutableFile::OnDisk(_, path) => {
                use std::os::unix::ffi::OsStrExt;

                let path_cstring = CString::new(path.as_os_str().as_bytes()).unwrap();
                libc::execve(path_cstring.as_ptr().cast(), argv.as_ptr(), envp.as_ptr())
            }

            #[cfg(target_family = "windows")]
            ExecutableFile::OnDisk(_, path) => {
                let _ = argv;
                let _ = envp;
                use memexec::memexec_exe;
                let bytes = std::fs::read(path).unwrap();
                memexec_exe(&bytes).unwrap();
                std::process::exit(0);
            }
        }
    }
}

// with Expect
#[cfg(target_family = "unix")]
unsafe fn roc_run_native_debug(
    _executable: ExecutableFile,
    _argv: &[*const c_char],
    _envp: &[*const c_char],
    _expectations: VecMap<ModuleId, Expectations>,
    _interns: Interns,
) {
    todo!()
}

#[cfg(target_os = "linux")]
fn roc_run_executable_file_path(binary_bytes: &[u8]) -> std::io::Result<ExecutableFile> {
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
fn roc_run_executable_file_path(binary_bytes: &[u8]) -> std::io::Result<ExecutableFile> {
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

#[cfg(all(target_family = "windows"))]
fn roc_run_executable_file_path(binary_bytes: &[u8]) -> std::io::Result<ExecutableFile> {
    use std::fs::OpenOptions;
    use std::io::Write;

    let temp_dir = tempfile::tempdir()?;

    // We have not found a way to use a virtual file on non-Linux OSes.
    // Hence we fall back to just writing the file to the file system, and using that file.
    let app_path_buf = temp_dir.path().join("roc_app_binary.exe");
    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        //.mode(0o777) // create the file as executable
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
    arena: Bump, // This should be passed an owned value, not a reference, so we can usefully mem::forget it!
    opt_level: OptLevel,
    _args: I,
    binary_bytes: &[u8],
    _expectations: VecMap<ModuleId, Expectations>,
    _interns: Interns,
) -> io::Result<i32> {
    use bumpalo::collections::CollectIn;

    unsafe {
        let executable = roc_run_executable_file_path(binary_bytes)?;

        // TODO forward the arguments
        // let (argv_cstrings, envp_cstrings) = make_argv_envp(&arena, &executable, args);
        let argv_cstrings = bumpalo::vec![ in &arena; CString::default()];
        let envp_cstrings = bumpalo::vec![ in &arena; CString::default()];

        let argv: bumpalo::collections::Vec<*const c_char> = argv_cstrings
            .iter()
            .map(|s| s.as_ptr())
            .chain([std::ptr::null()])
            .collect_in(&arena);

        let envp: bumpalo::collections::Vec<*const c_char> = envp_cstrings
            .iter()
            .map(|s| s.as_ptr())
            .chain([std::ptr::null()])
            .collect_in(&arena);

        match opt_level {
            OptLevel::Development => {
                // roc_run_native_debug(executable, &argv, &envp, expectations, interns)
                todo!()
            }
            OptLevel::Normal | OptLevel::Size | OptLevel::Optimize => {
                roc_run_native_fast(executable, &argv, &envp);
            }
        }
    }

    Ok(1)
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
    Windows64,
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
            Windows64 => "windows64",
            Wasm32 => "wasm32",
        }
    }

    /// NOTE keep up to date!
    const OPTIONS: &'static [&'static str] = &[
        Target::System.as_str(),
        Target::Linux32.as_str(),
        Target::Linux64.as_str(),
        Target::Windows64.as_str(),
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
            Windows64 => Triple {
                architecture: Architecture::X86_64,
                vendor: Vendor::Unknown,
                operating_system: OperatingSystem::Windows,
                environment: Environment::Gnu,
                binary_format: BinaryFormat::Coff,
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
            "windows64" => Ok(Target::Windows64),
            "wasm32" => Ok(Target::Wasm32),
            _ => Err(format!("Roc does not know how to compile to {}", string)),
        }
    }
}

// These functions don't end up in the final Roc binary but Windows linker needs a definition inside the crate.
// On Windows, there seems to be less dead-code-elimination than on Linux or MacOS, or maybe it's done later.
#[cfg(windows)]
#[allow(unused_imports)]
use windows_roc_platform_functions::*;

#[cfg(windows)]
mod windows_roc_platform_functions {
    use core::ffi::c_void;

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
        libc::malloc(size)
    }

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_realloc(
        c_ptr: *mut c_void,
        new_size: usize,
        _old_size: usize,
        _alignment: u32,
    ) -> *mut c_void {
        libc::realloc(c_ptr, new_size)
    }

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
        libc::free(c_ptr)
    }
}
