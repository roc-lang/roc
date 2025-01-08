//! Provides the core CLI functionality for the Roc binary.

#[macro_use]
extern crate const_format;

use bumpalo::Bump;
use clap::{
    builder::PossibleValuesParser, parser::ValueSource, value_parser, Arg, ArgAction, ArgMatches,
    Command,
};
use roc_build::link::{LinkType, LinkingStrategy};
use roc_build::program::{
    handle_error_module, handle_loading_problem, standard_load_config, BuildFileError,
    BuildOrdering, BuiltFile, CodeGenBackend, CodeGenOptions, DEFAULT_ROC_FILENAME,
};
#[cfg(not(windows))]
use roc_collections::MutMap;
use roc_error_macros::{internal_error, user_error};
use roc_gen_dev::AssemblyBackendMode;
use roc_gen_llvm::llvm::build::LlvmBackendMode;
use roc_load::{ExpectMetadata, Threading};
#[cfg(not(windows))]
use roc_module::symbol::ModuleId;
use roc_mono::ir::OptLevel;
use roc_packaging::cache::RocCacheDir;
use roc_packaging::tarball::Compression;
#[cfg(not(windows))]
use roc_reporting::report::ANSI_STYLE_CODES;
use roc_target::{Architecture, Target};
use std::env;
use std::ffi::{CString, OsStr, OsString};
use std::io;
use std::mem::ManuallyDrop;
use std::os::raw::{c_char, c_int};
use std::path::{Path, PathBuf};
use std::process;
#[cfg(not(windows))]
use std::time::Duration;
use std::time::Instant;
use strum::IntoEnumIterator;
#[cfg(not(target_os = "linux"))]
use tempfile::TempDir;

mod format;
pub use format::{format_files, format_src, FormatMode};

pub const CMD_BUILD: &str = "build";
pub const CMD_RUN: &str = "run";
pub const CMD_DEV: &str = "dev";
pub const CMD_REPL: &str = "repl";
pub const CMD_DOCS: &str = "docs";
pub const CMD_CHECK: &str = "check";
pub const CMD_VERSION: &str = "version";
pub const CMD_FORMAT: &str = "format";
pub const CMD_TEST: &str = "test";
pub const CMD_GLUE: &str = "glue";
pub const CMD_PREPROCESS_HOST: &str = "preprocess-host";

pub const FLAG_EMIT_LLVM_IR: &str = "emit-llvm-ir";
pub const FLAG_PROFILING: &str = "profiling";
pub const FLAG_BUNDLE: &str = "bundle";
pub const FLAG_DEV: &str = "dev";
pub const FLAG_OPTIMIZE: &str = "optimize";
pub const FLAG_MAX_THREADS: &str = "max-threads";
pub const FLAG_OPT_SIZE: &str = "opt-size";
pub const FLAG_LIB: &str = "lib";
pub const FLAG_NO_LINK: &str = "no-link";
pub const FLAG_TARGET: &str = "target";
pub const FLAG_TIME: &str = "time";
pub const FLAG_VERBOSE: &str = "verbose";
pub const FLAG_NO_COLOR: &str = "no-color";
pub const FLAG_NO_HEADER: &str = "no-header";
pub const FLAG_LINKER: &str = "linker";
pub const FLAG_BUILD_HOST: &str = "build-host";
pub const FLAG_SUPPRESS_BUILD_HOST_WARNING: &str = "suppress-build-host-warning";
pub const FLAG_CHECK: &str = "check";
pub const FLAG_STDIN: &str = "stdin";
pub const FLAG_STDOUT: &str = "stdout";
pub const FLAG_WASM_STACK_SIZE_KB: &str = "wasm-stack-size-kb";
pub const FLAG_OUTPUT: &str = "output";
pub const FLAG_FUZZ: &str = "fuzz";
pub const FLAG_MAIN: &str = "main";
pub const ROC_FILE: &str = "ROC_FILE";
pub const GLUE_DIR: &str = "GLUE_DIR";
pub const GLUE_SPEC: &str = "GLUE_SPEC";
pub const DIRECTORY_OR_FILES: &str = "DIRECTORY_OR_FILES";
pub const ARGS_FOR_APP: &str = "ARGS_FOR_APP";
pub const FLAG_PP_HOST: &str = "host";
pub const FLAG_PP_PLATFORM: &str = "platform";
pub const FLAG_PP_DYLIB: &str = "lib";
pub const FLAG_MIGRATE: &str = "migrate";
pub const FLAG_DOCS_ROOT: &str = "root-dir";

pub const VERSION: &str = env!("ROC_VERSION");
const DEFAULT_GENERATED_DOCS_DIR: &str = "generated-docs";

pub fn build_app() -> Command {
    let flag_optimize = Arg::new(FLAG_OPTIMIZE)
        .long(FLAG_OPTIMIZE)
        .help("Optimize the compiled program to run faster\n(Optimization takes time to complete.)")
        .action(ArgAction::SetTrue)
        .required(false);

    let flag_max_threads = Arg::new(FLAG_MAX_THREADS)
        .long(FLAG_MAX_THREADS)
        .help("Limit the number of threads (and hence cores) used during compilation")
        .value_parser(value_parser!(usize))
        .required(false);

    let flag_opt_size = Arg::new(FLAG_OPT_SIZE)
        .long(FLAG_OPT_SIZE)
        .help("Optimize the compiled program to have a small binary size\n(Optimization takes time to complete.)")
        .action(ArgAction::SetTrue)
        .required(false);

    let flag_dev = Arg::new(FLAG_DEV)
        .long(FLAG_DEV)
        .help("Make compilation finish as soon as possible, at the expense of runtime performance")
        .action(ArgAction::SetTrue)
        .required(false);

    let flag_emit_llvm_ir = Arg::new(FLAG_EMIT_LLVM_IR)
        .long(FLAG_EMIT_LLVM_IR)
        .help("Emit a `.ll` file containing the LLVM IR of the program")
        .action(ArgAction::SetTrue)
        .required(false);

    let flag_profiling = Arg::new(FLAG_PROFILING)
        .long(FLAG_PROFILING)
        .help("Keep debug info in the final generated program even in optimized builds")
        .action(ArgAction::SetTrue)
        .required(false);

    let flag_time = Arg::new(FLAG_TIME)
        .long(FLAG_TIME)
        .help("Print detailed compilation time information")
        .action(ArgAction::SetTrue)
        .required(false);

    let flag_linker = Arg::new(FLAG_LINKER)
        .long(FLAG_LINKER)
        .help("Set which linker to use\n(The surgical linker is enabled by default only when building for wasm32 or x86_64 Linux, because those are the only targets it currently supports. Otherwise the legacy linker is used by default.)")
        .value_parser(["surgical", "legacy"])
        .required(false);

    let flag_build_host = Arg::new(FLAG_BUILD_HOST)
        .long(FLAG_BUILD_HOST)
        .help("WARNING: platforms are responsible for building hosts, this flag will be removed when internal test platforms have a build script")
        .action(ArgAction::SetTrue)
        .required(false);

    let flag_suppress_build_host_warning = Arg::new(FLAG_SUPPRESS_BUILD_HOST_WARNING)
        .long(FLAG_SUPPRESS_BUILD_HOST_WARNING)
        .help("WARNING: platforms are responsible for building hosts, this flag will be removed when internal test platforms have a build script")
        .action(ArgAction::SetTrue)
        .required(false);

    let flag_wasm_stack_size_kb = Arg::new(FLAG_WASM_STACK_SIZE_KB)
        .long(FLAG_WASM_STACK_SIZE_KB)
        .help("Stack size in kilobytes for wasm32 target\n(This only applies when --dev also provided.)")
        .value_parser(value_parser!(u32))
        .required(false);

    let flag_fuzz = Arg::new(FLAG_FUZZ)
        .long(FLAG_FUZZ)
        .help("Instrument the roc binary for fuzzing with roc-fuzz")
        .action(ArgAction::SetTrue)
        .required(false);

    let flag_main = Arg::new(FLAG_MAIN)
        .long(FLAG_MAIN)
        .help("The .roc file of the main app/package module to resolve dependencies from")
        .value_parser(value_parser!(PathBuf))
        .required(false);

    let roc_file_to_run = Arg::new(ROC_FILE)
        .help("The .roc file of an app to run")
        .value_parser(value_parser!(PathBuf))
        .required(false)
        .default_value(DEFAULT_ROC_FILENAME);

    let args_for_app = Arg::new(ARGS_FOR_APP)
        .help("Arguments to pass into the app being run\ne.g. `roc run -- arg1 arg2`")
        .value_parser(value_parser!(OsString))
        .num_args(0..)
        .allow_hyphen_values(true);

    let flag_docs_root_dir = Arg::new(FLAG_DOCS_ROOT)
        .long(FLAG_DOCS_ROOT)
        .help("Set a root directory path to be used as a prefix for URL links in the generated documentation files.")
        .value_parser(value_parser!(Option<String>))
        .required(false);

    let build_target_values_parser =
        PossibleValuesParser::new(Target::iter().map(Into::<&'static str>::into));

    Command::new("roc")
        .version(VERSION)
        .about("Run the given .roc file, if there are no compilation errors.\nYou can use one of the SUBCOMMANDS below to do something else!")
        .args_conflicts_with_subcommands(true)
        .subcommand(Command::new(CMD_BUILD)
            .about("Build a binary from the given .roc file, but don't run it")
            .arg(Arg::new(FLAG_OUTPUT)
                .long(FLAG_OUTPUT)
                .help("The full path to the output binary, including filename. To specify directory only, specify a path that ends in a directory separator (e.g. a slash).")
                .value_parser(value_parser!(OsString))
                .required(false)
            )
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_emit_llvm_ir.clone())
            .arg(flag_profiling.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_build_host.clone())
            .arg(flag_suppress_build_host_warning.clone())
            .arg(flag_fuzz.clone())
            .arg(flag_wasm_stack_size_kb)
            .arg(
                Arg::new(FLAG_TARGET)
                    .long(FLAG_TARGET)
                    .help("Choose a different target")
                    .default_value(Into::<&'static str>::into(Target::default()))
                    .value_parser(build_target_values_parser.clone())
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_LIB)
                    .long(FLAG_LIB)
                    .help("Build a C library instead of an executable")
                    .action(ArgAction::SetTrue)
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_BUNDLE)
                    .long(FLAG_BUNDLE)
                    .help("Create an archive of a package (for example, a .tar, .tar.gz, or .tar.br file), so others can add it as a HTTPS dependency.")
                    .conflicts_with(FLAG_TARGET)
                    .value_parser([".tar", ".tar.gz", ".tar.br"])
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_NO_LINK)
                    .long(FLAG_NO_LINK)
                    .help("Do not link\n(Instead, just output the `.o` file.)")
                    .action(ArgAction::SetTrue)
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_VERBOSE)
                    .long(FLAG_VERBOSE)
                    .help("Print detailed information while building")
                    .action(ArgAction::SetTrue)
                    .required(false)
            )
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file to build")
                    .value_parser(value_parser!(PathBuf))
                    .required(false)
                    .default_value(DEFAULT_ROC_FILENAME),
            )
        )
        .subcommand(Command::new(CMD_TEST)
            .about("Run all top-level `expect`s in a main module and any modules it imports")
            .arg(flag_main.clone())
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_emit_llvm_ir.clone())
            .arg(flag_profiling.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_build_host.clone())
            .arg(flag_suppress_build_host_warning.clone())
            .arg(flag_fuzz.clone())
            .arg(
                Arg::new(FLAG_VERBOSE)
                    .long(FLAG_VERBOSE)
                    .help("Print detailed test statistics by module")
                    .action(ArgAction::SetTrue)
                    .required(false)
            )
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file to test")
                    .value_parser(value_parser!(PathBuf))
                    .num_args(0..)
                    .required(false)
                    .default_value(DEFAULT_ROC_FILENAME)
            )
            .arg(args_for_app.clone().last(true))
        )
        .subcommand(Command::new(CMD_REPL)
            .about("Launch the interactive Read Eval Print Loop (REPL)")
            .arg(
                Arg::new(FLAG_NO_COLOR)
                    .long(FLAG_NO_COLOR)
                    .help("Do not use any ANSI color codes in the repl output")
                    .action(ArgAction::SetTrue)
                    .required(false)
            )
            .arg(
                Arg::new(FLAG_NO_HEADER)
                    .long(FLAG_NO_HEADER)
                    .help("Do not print the repl header")
                    .action(ArgAction::SetTrue)
                    .required(false)
            )
        )
        .subcommand(Command::new(CMD_RUN)
            .about("Run a .roc file even if it has build errors")
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_emit_llvm_ir.clone())
            .arg(flag_profiling.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_build_host.clone())
            .arg(flag_suppress_build_host_warning.clone())
            .arg(flag_fuzz.clone())
            .arg(roc_file_to_run.clone())
            .arg(args_for_app.clone().last(true))
        )
        .subcommand(Command::new(CMD_DEV)
            .about("`check` a .roc file, and then run it if there were no errors")
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_emit_llvm_ir.clone())
            .arg(flag_profiling.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_build_host.clone())
            .arg(flag_suppress_build_host_warning.clone())
            .arg(flag_fuzz.clone())
            .arg(roc_file_to_run.clone())
            .arg(args_for_app.clone().last(true))
        )
        .subcommand(Command::new(CMD_FORMAT)
            .about("Format a .roc file or the .roc files contained in a directory using standard\nRoc formatting")
            .arg(
                Arg::new(DIRECTORY_OR_FILES)
                    .index(1)
                    .num_args(0..)
                    .required(false)
                    .value_parser(value_parser!(OsString)))
            .arg(
                Arg::new(FLAG_CHECK)
                    .long(FLAG_CHECK)
                    .help("Checks that specified files are formatted\n(If formatting is needed, return a non-zero exit code.)")
                    .action(ArgAction::SetTrue)
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_MIGRATE)
                    .long(FLAG_MIGRATE)
                    .help("Will change syntax to match the latest preferred style. This can cause changes to variable names and more.")
                    .action(ArgAction::SetTrue)
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_STDIN)
                    .long(FLAG_STDIN)
                    .help("Read file to format from stdin")
                    .action(ArgAction::SetTrue)
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_STDOUT)
                    .long(FLAG_STDOUT)
                    .help("Print formatted file to stdout")
                    .action(ArgAction::SetTrue)
                    .required(false),
            )
            .after_help("If DIRECTORY_OR_FILES is omitted, the .roc files in the current working\ndirectory are formatted.")
        )
        .subcommand(Command::new(CMD_VERSION)
            .about(concatcp!("Print the Roc compiler’s version, which is currently ", VERSION)))
        .subcommand(Command::new(CMD_CHECK)
            .about("Check the code for problems, but don’t build or run it")
            .arg(flag_main.clone())
            .arg(flag_time.clone())
            .arg(flag_max_threads.clone())
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file to check")
                    .value_parser(value_parser!(PathBuf))
                    .required(false)
                    .default_value(DEFAULT_ROC_FILENAME),
            )
            )
        .subcommand(
            Command::new(CMD_DOCS)
                .about("Generate documentation for a Roc package")
                .arg(Arg::new(FLAG_OUTPUT)
                    .long(FLAG_OUTPUT)
                    .help("Output directory for the generated documentation files.")
                    .value_parser(value_parser!(OsString))
                    .required(false)
                    .default_value(DEFAULT_GENERATED_DOCS_DIR),
                )
                .arg(Arg::new(ROC_FILE)
                    .help("The package's main .roc file")
                    .value_parser(value_parser!(PathBuf))
                    .required(false)
                    .default_value(DEFAULT_ROC_FILENAME),
                )
                .arg(flag_docs_root_dir)
        )
        .subcommand(Command::new(CMD_GLUE)
            .about("Generate glue code between a platform's Roc API and its host language")
            .arg(&flag_dev)
            .arg(
                Arg::new(GLUE_SPEC)
                    .help("The specification for how to translate Roc types into output files.")
                    .value_parser(value_parser!(PathBuf))
                    .required(true)
            )
            .arg(
                Arg::new(GLUE_DIR)
                    .help("The directory for the generated glue code.\nNote: The implementation can write to any file in this directory.")
                    .value_parser(value_parser!(PathBuf))
                    .required(true)
            )
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file whose exposed types should be translated.")
                    .value_parser(value_parser!(PathBuf))
                    .required(false)
                    .default_value(DEFAULT_ROC_FILENAME)
            )
            .arg(flag_linker.clone())
        )
        .subcommand(Command::new(CMD_PREPROCESS_HOST)
            .about("Runs the surgical linker preprocessor to generate `.rh` and `.rm` files.")
            .arg(
                Arg::new(FLAG_PP_HOST)
                    .help("Path to the host executable where the app was linked dynamically")
                    .value_parser(value_parser!(PathBuf))
                    .required(true)
            )
            .arg(
                Arg::new(FLAG_PP_PLATFORM)
                    .help("Path to the platform/main.roc file")
                    .value_parser(value_parser!(PathBuf))
                    .required(true)
            )
            .arg(
                Arg::new(FLAG_PP_DYLIB)
                    .help("Path to a stubbed app dynamic library (e.g. roc build --lib app.roc)")
                    .value_parser(value_parser!(PathBuf))
                    .required(true)
            )
            .arg(
                Arg::new(FLAG_VERBOSE)
                    .long(FLAG_VERBOSE)
                    .help("Print detailed information while pre-processing host")
                    .action(ArgAction::SetTrue)
                    .required(false)
            )
            .arg(
                Arg::new(FLAG_TARGET)
                    .long(FLAG_TARGET)
                    .help("Choose a different target")
                    .default_value(Into::<&'static str>::into(Target::default()))
                    .value_parser(build_target_values_parser.clone())
                    .required(false),
            )
        )
        .arg(flag_optimize)
        .arg(flag_max_threads)
        .arg(flag_opt_size)
        .arg(flag_dev)
        .arg(flag_emit_llvm_ir)
        .arg(flag_profiling)
        .arg(flag_time)
        .arg(flag_linker)
        .arg(flag_build_host)
        .arg(flag_suppress_build_host_warning)
        .arg(flag_fuzz)
        .arg(roc_file_to_run)
        .arg(args_for_app.trailing_var_arg(true))
}

#[derive(Debug, PartialEq, Eq)]
pub enum BuildConfig {
    BuildOnly,
    BuildAndRun,
    BuildAndRunIfNoErrors,
}

fn opt_level_from_flags(matches: &ArgMatches) -> OptLevel {
    match (
        matches.get_flag(FLAG_OPTIMIZE),
        matches.get_flag(FLAG_OPT_SIZE),
        matches.get_flag(FLAG_DEV),
    ) {
        (true, false, false) => OptLevel::Optimize,
        (false, true, false) => OptLevel::Size,
        (false, false, true) => OptLevel::Development,
        (false, false, false) => OptLevel::Normal,
        _ => user_error!("build can be only one of `--dev`, `--optimize`, or `--opt-size`"),
    }
}

#[cfg(windows)]
pub fn test(_matches: &ArgMatches, _target: Target) -> io::Result<i32> {
    todo!("running tests does not work on windows right now")
}

#[cfg(not(windows))]
struct ModuleTestResults {
    module_id: ModuleId,
    failed_count: usize,
    passed_count: usize,
    tests_duration: Duration,
}

#[cfg(not(windows))]
pub fn test(matches: &ArgMatches, target: Target) -> io::Result<i32> {
    use roc_build::program::report_problems_monomorphized;
    use roc_load::{ExecutionMode, FunctionKind, LoadConfig, LoadMonomorphizedError};
    use roc_packaging::cache;

    let start_time = Instant::now();
    let arena = Bump::new();
    let opt_level = opt_level_from_flags(matches);

    let threading = match matches.get_one::<usize>(FLAG_MAX_THREADS) {
        None => Threading::AllAvailable,
        Some(0) => user_error!("cannot build with at most 0 threads"),
        Some(1) => Threading::Single,
        Some(n) => Threading::AtMost(*n),
    };

    let paths: Vec<_> = matches.get_many::<PathBuf>(ROC_FILE).unwrap().collect();

    let paths: Vec<_> = {
        let mut flatten_paths: Vec<_> = vec![];
        for path in paths.into_iter() {
            // Spawn the root task
            if !path.exists() {
                let current_dir = env::current_dir().unwrap();
                let expected_file_path = current_dir.join(path);

                let current_dir_string = current_dir.display();
                let expected_file_path_string = expected_file_path.display();

                // TODO these should use roc_reporting to display nicer error messages.
                match matches.value_source(ROC_FILE) {
            Some(ValueSource::DefaultValue) => {
                eprintln!(
                    "\nThe current directory ({current_dir_string}) does not contain a {DEFAULT_ROC_FILENAME} file to use as a default.\n\nYou can run `roc help` for more information on how to provide a .roc file.\n"
                )
            }
            _ => eprintln!("\nThis file was not found: {expected_file_path_string}\n\nYou can run `roc help` for more information on how to provide a .roc file.\n"),
        }
                process::exit(1);
            } else if path.is_dir() {
                find_all_roc_files(path, &mut flatten_paths);
            } else {
                flatten_paths.push(path.clone());
            }
        }
        flatten_paths
    };

    let mut all_files_total_failed_count = 0;
    let mut all_files_total_passed_count = 0;

    for path in paths.iter() {
        let arena = &arena;
        let function_kind = FunctionKind::from_env();

        let opt_main_path = matches.get_one::<PathBuf>(FLAG_MAIN);

        // Step 1: compile the app and generate the .o file
        let load_config = LoadConfig {
            target,
            function_kind,
            // TODO: expose this from CLI?
            render: roc_reporting::report::RenderTarget::ColorTerminal,
            palette: roc_reporting::report::DEFAULT_PALETTE,
            threading,
            exec_mode: ExecutionMode::Test,
        };
        let load_result = roc_load::load_and_monomorphize(
            arena,
            path.to_path_buf(),
            opt_main_path.cloned(),
            RocCacheDir::Persistent(cache::roc_cache_packages_dir().as_path()),
            load_config,
        );

        let mut loaded = match load_result {
            Ok(loaded) => loaded,
            Err(LoadMonomorphizedError::LoadingProblem(problem)) => {
                return handle_loading_problem(problem);
            }
            Err(LoadMonomorphizedError::ErrorModule(module)) => {
                return handle_error_module(module, start_time.elapsed(), path.as_os_str(), false);
            }
        };
        let problems = report_problems_monomorphized(&mut loaded);

        let mut expectations = std::mem::take(&mut loaded.expectations);

        let interns = loaded.interns.clone();
        let sources = loaded.sources.clone();

        let (dyn_lib, expects_by_module, layout_interner) =
            roc_repl_expect::run::expect_mono_module_to_dylib(
                arena,
                target,
                loaded,
                opt_level,
                LlvmBackendMode::CliTest,
            )
            .unwrap();

        // Print warnings before running tests.
        {
            debug_assert_eq!(
                problems.errors, 0,
                "if there were errors, we would have already exited."
            );
            if problems.warnings > 0 {
                problems.print_error_warning_count(start_time.elapsed());
                println!(".\n\nRunning tests…\n\n\x1B[36m{}\x1B[39m", "─".repeat(80));
            }
        }

        // Run the tests.
        let arena = &bumpalo::Bump::new();
        let interns = arena.alloc(interns);

        let mut writer = std::io::stdout();

        let mut total_failed_count = 0;
        let mut total_passed_count = 0;

        let mut results_by_module = Vec::new();
        let global_layout_interner = layout_interner.into_global();

        let compilation_duration = start_time.elapsed();

        for (module_id, expects) in expects_by_module.into_iter() {
            let test_start_time = Instant::now();

            let (failed_count, passed_count) = roc_repl_expect::run::run_toplevel_expects(
                &mut writer,
                roc_reporting::report::RenderTarget::ColorTerminal,
                arena,
                interns,
                &global_layout_interner,
                &dyn_lib,
                &mut expectations,
                expects,
            )
            .unwrap();

            let tests_duration = test_start_time.elapsed();

            results_by_module.push(ModuleTestResults {
                module_id,
                failed_count,
                passed_count,
                tests_duration,
            });

            total_failed_count += failed_count;
            total_passed_count += passed_count;
        }

        let total_duration = start_time.elapsed();
        all_files_total_failed_count += total_failed_count;
        all_files_total_passed_count += total_passed_count;
        if total_failed_count == 0 && total_passed_count == 0 {
            // Only report no expectations found once.
            continue;
        } else if matches.get_flag(FLAG_VERBOSE) {
            println!("Compiled in {} ms.", compilation_duration.as_millis());
            for module_test_results in results_by_module {
                print_test_results(module_test_results, &sources);
            }
        } else {
            let test_summary_str =
                test_summary(total_failed_count, total_passed_count, total_duration);
            println!("{test_summary_str}");
        }
    }
    if all_files_total_failed_count == 0 && all_files_total_passed_count == 0 {
        // TODO print this in a more nicely formatted way!
        println!("No expectations were found.");

        // If no tests ran, treat that as an error. This is perhaps
        // briefly annoying at the very beginning of a project when
        // you actually have zero tests, but it can save you from
        // having a change to your CI script accidentally stop
        // running tests altogether!
        Ok(2)
    } else {
        Ok((all_files_total_failed_count > 0) as i32)
    }
}

fn find_all_roc_files(path: &PathBuf, flatten_paths: &mut Vec<PathBuf>) {
    if path.is_dir() {
        if let Ok(entries) = std::fs::read_dir(path) {
            entries.for_each(|entry| {
                if let Ok(entry) = entry {
                    let entry_path = entry.path();
                    find_all_roc_files(&entry_path, flatten_paths);
                }
            });
        } else {
            eprintln!(
                "\nSomething went wrong opening the directory {}\n",
                path.display()
            );
        }
    } else if path.is_file() {
        match path.extension() {
            Some(extension) if extension == "roc" => {
                flatten_paths.push(path.clone());
            }
            _ => {}
        }
    }
}

#[cfg(not(windows))]
fn print_test_results(
    module_test_results: ModuleTestResults,
    sources: &MutMap<ModuleId, (PathBuf, Box<str>)>,
) {
    let ModuleTestResults {
        module_id,
        failed_count,
        passed_count,
        tests_duration,
    } = module_test_results;

    let test_summary_str = test_summary(failed_count, passed_count, tests_duration);

    let (module_path, _) = sources.get(&module_id).unwrap();
    let module_name = module_path.file_name().unwrap().to_str().unwrap();

    println!("\n{module_name}:\n    {test_summary_str}",);
}

#[cfg(not(windows))]
fn test_summary(failed_count: usize, passed_count: usize, tests_duration: Duration) -> String {
    let failed_color = if failed_count == 0 {
        ANSI_STYLE_CODES.green
    } else {
        ANSI_STYLE_CODES.red
    };
    let passed_color = ANSI_STYLE_CODES.green;
    let reset = ANSI_STYLE_CODES.reset;

    format!(
        "{failed_color}{failed_count}{reset} failed and {passed_color}{passed_count}{reset} passed in {} ms.",
        tests_duration.as_millis()
    )
}

/// Find the element of `options` with the smallest edit distance to
/// `reference`. Returns a tuple containing the element and the distance, or
/// `None` if the `options` `Vec` is empty.
fn nearest_match<'a>(reference: &str, options: &'a [String]) -> Option<(&'a String, usize)> {
    options
        .iter()
        .map(|s| (s, distance::damerau_levenshtein(reference, s)))
        .min_by(|(_, a), (_, b)| a.cmp(b))
}

pub fn default_linking_strategy(
    matches: &ArgMatches,
    link_type: LinkType,
    target: Target,
) -> LinkingStrategy {
    let linker_support_level = roc_linker::support_level(link_type, target);
    match matches.get_one::<String>(FLAG_LINKER).map(AsRef::as_ref) {
        Some("legacy") => LinkingStrategy::Legacy,
        Some("surgical") => match linker_support_level {
            roc_linker::SupportLevel::Full => LinkingStrategy::Surgical,
            roc_linker::SupportLevel::Wip => {
                println!("Warning! Using an unfinished surgical linker for target {target}");
                LinkingStrategy::Surgical
            }
            roc_linker::SupportLevel::None => LinkingStrategy::Legacy,
        },
        _ => match linker_support_level {
            roc_linker::SupportLevel::Full => LinkingStrategy::Surgical,
            _ => LinkingStrategy::Legacy,
        },
    }
}

#[allow(clippy::too_many_arguments)]
pub fn build(
    matches: &ArgMatches,
    subcommands: &[String],
    config: BuildConfig,
    target: Target,
    out_path: Option<&Path>,
    roc_cache_dir: RocCacheDir<'_>,
    link_type: LinkType,
    verbose: bool,
) -> io::Result<i32> {
    use BuildConfig::*;

    let path = matches.get_one::<PathBuf>(ROC_FILE).unwrap();
    {
        // Spawn the root task
        if !path.exists() {
            let current_dir = env::current_dir().unwrap();
            let expected_file_path = current_dir.join(path);

            let current_dir_string = current_dir.display();
            let expected_file_path_string = expected_file_path.display();

            // TODO these should use roc_reporting to display nicer error messages.
            match matches.value_source(ROC_FILE) {
                Some(ValueSource::DefaultValue) => {
                    eprintln!(
                        "\nThe current directory ({current_dir_string}) does not contain a {DEFAULT_ROC_FILENAME} file to use as a default.\n\nYou can run `roc help` for more information on how to provide a .roc file.\n"
                    )
                }
                _ => {
                    let mut error_lines = Vec::new();
                    error_lines.push(format!(
                        "This file was not found: {expected_file_path_string}"
                    ));
                    // Add some additional hints if run as `roc [FILENAME]`.
                    if matches.subcommand().is_none() {
                        match path.to_str() {
                            Some(possible_typo) if !possible_typo.ends_with(".roc") => {
                                if let Some((nearest_command, _)) =
                                    nearest_match(possible_typo, subcommands)
                                {
                                    error_lines.push(format!(
                                        "Did you mean to use the {nearest_command} subcommand?"
                                    ));
                                }
                            }
                            _ => (),
                        }
                    }
                    error_lines.push("You can run `roc help` to see the list of available subcommands and for more information on how to provide a .roc file.".to_string());

                    eprintln!("\n{}\n", error_lines.join("\n\n"));
                }
            }

            process::exit(1);
        }

        if config == BuildConfig::BuildOnly && matches.contains_id(FLAG_BUNDLE) {
            let start_time = Instant::now();

            let compression =
                Compression::try_from(matches.get_one::<String>(FLAG_BUNDLE).unwrap().as_str())
                    .unwrap();

            // Print a note of advice. This is mainly here because brotli takes so long but produces
            // such smaller output files; the idea is to encourage people to wait for brotli,
            // so that downloads go faster. The compression only happens once, but the network
            // transfer and decompression will happen many more times!
            match compression {
                Compression::Brotli => {
                    println!("Compressing with Brotli at maximum quality level…\n\n(Note: Brotli compression can take awhile! Using --{FLAG_BUNDLE} .tar.gz takes less time, but usually produces a significantly larger output file. Brotli is generally worth the up-front wait if this is a file people will be downloading!)\n");
                }
                Compression::Gzip => {
                    println!("Compressing with gzip at minimum quality…\n\n(Note: Gzip usually runs faster than Brotli but typically produces significantly larger output files. Consider using --{FLAG_BUNDLE} .tar.br if this is a file people will be downloading!)\n");
                }
                Compression::Uncompressed => {
                    println!("Building .tar archive without compression…\n\n(Note: Compression takes more time to run but typically produces much smaller output files. Consider using --{FLAG_BUNDLE} .tar.br if this is a file people will be downloading!)\n");
                }
            }

            // Rather than building an executable or library, we're building
            // a tarball so this code can be distributed via HTTPS
            let filename = roc_packaging::tarball::build(path, compression)?;
            let total_time_ms = start_time.elapsed().as_millis();
            let total_time = if total_time_ms > 1000 {
                format!("{}s {}ms", total_time_ms / 1000, total_time_ms % 1000)
            } else {
                format!("{total_time_ms} ms")
            };
            let created_path = path.with_file_name(&filename);

            println!(
                "\nBundled \x1B[33m{}\x1B[39m and its dependent files into the following archive in {total_time}:\n\n\t\x1B[33m{}\x1B[39m\n\nTo distribute this archive as a package, upload this to some URL and then add it as a dependency with:\n\n\t\x1B[32m\"https://your-url-goes-here/{filename}\"\x1B[39m\n",
                path.to_string_lossy(),
                created_path.to_string_lossy()
            );

            return Ok(0);
        }
    }

    // the process will end after this function,
    // so we don't want to spend time freeing these values
    let arena = ManuallyDrop::new(Bump::new());

    let opt_level = opt_level_from_flags(matches);

    let should_run_expects = matches!(opt_level, OptLevel::Development | OptLevel::Normal) &&
        // TODO: once expect is decoupled from roc launching the executable, remove this part of the conditional.
        matches!(
            config,
            BuildConfig::BuildAndRun | BuildConfig::BuildAndRunIfNoErrors
        );

    let code_gen_backend = if matches!(opt_level, OptLevel::Development) {
        if matches!(target.architecture(), Architecture::Wasm32) {
            CodeGenBackend::Wasm
        } else {
            CodeGenBackend::Assembly(AssemblyBackendMode::Binary)
        }
    } else {
        let backend_mode = if should_run_expects {
            LlvmBackendMode::BinaryWithExpect
        } else {
            LlvmBackendMode::Binary
        };

        CodeGenBackend::Llvm(backend_mode)
    };

    let emit_llvm_ir = matches.get_flag(FLAG_EMIT_LLVM_IR);
    if emit_llvm_ir && !matches!(code_gen_backend, CodeGenBackend::Llvm(_)) {
        user_error!("Cannot emit llvm ir while using a dev backend.");
    }

    let emit_debug_info = matches.get_flag(FLAG_PROFILING)
        || matches!(opt_level, OptLevel::Development | OptLevel::Normal);
    let emit_timings = matches.get_flag(FLAG_TIME);

    let threading = match matches.get_one::<usize>(FLAG_MAX_THREADS) {
        None => Threading::AllAvailable,
        Some(0) => user_error!("cannot build with at most 0 threads"),
        Some(1) => Threading::Single,
        Some(n) => Threading::AtMost(*n),
    };

    let wasm_dev_backend = matches!(code_gen_backend, CodeGenBackend::Wasm);

    let linking_strategy = if wasm_dev_backend {
        LinkingStrategy::Additive
    } else {
        default_linking_strategy(matches, link_type, target)
    };

    // All hosts should be prebuilt, this flag keeps the rebuilding behvaiour
    // as required for internal tests
    let build_host = matches.get_flag(FLAG_BUILD_HOST);
    let suppress_build_host_warning = matches.get_flag(FLAG_SUPPRESS_BUILD_HOST_WARNING);

    let fuzz = matches.get_flag(FLAG_FUZZ);
    if fuzz && !matches!(code_gen_backend, CodeGenBackend::Llvm(_)) {
        user_error!("Cannot instrument binary for fuzzing while using a dev backend.");
    }

    let wasm_dev_stack_bytes: Option<u32> = matches
        .try_get_one::<u32>(FLAG_WASM_STACK_SIZE_KB)
        .ok()
        .flatten()
        .map(|x| x * 1024);

    let build_ordering = match config {
        BuildAndRunIfNoErrors => BuildOrdering::BuildIfChecks,
        _ => BuildOrdering::AlwaysBuild,
    };

    let code_gen_options = CodeGenOptions {
        backend: code_gen_backend,
        opt_level,
        emit_debug_info,
        emit_llvm_ir,
        fuzz,
    };

    let load_config = standard_load_config(target, build_ordering, threading);

    let res_binary_path = roc_build::program::build_file(
        &arena,
        target,
        path.to_owned(),
        code_gen_options,
        emit_timings,
        link_type,
        linking_strategy,
        build_host,
        suppress_build_host_warning,
        wasm_dev_stack_bytes,
        roc_cache_dir,
        load_config,
        out_path,
        verbose,
    );

    match res_binary_path {
        Ok(BuiltFile {
            binary_path,
            problems,
            total_time,
            expect_metadata,
        }) => {
            match config {
                BuildOnly => {
                    // If possible, report the generated executable name relative to the current dir.
                    let generated_filename = binary_path
                        .strip_prefix(env::current_dir().unwrap())
                        .unwrap_or(&binary_path)
                        .to_str()
                        .unwrap();

                    // No need to waste time freeing this memory,
                    // since the process is about to exit anyway.
                    // std::mem::forget(arena);

                    problems.print_error_warning_count(total_time);
                    println!(" while successfully building:\n\n    {generated_filename}");

                    // Return a nonzero exit code if there were problems
                    Ok(problems.exit_code())
                }
                BuildAndRun => {
                    if problems.fatally_errored {
                        problems.print_error_warning_count(total_time);
                        println!(
                            ".\n\nCannot run program due to fatal error…\n\n\x1B[36m{}\x1B[39m",
                            "─".repeat(80)
                        );

                        // Return a nonzero exit code due to fatal problem
                        return Ok(problems.exit_code());
                    }
                    if problems.errors > 0 || problems.warnings > 0 {
                        problems.print_error_warning_count(total_time);
                        println!(
                            ".\n\nRunning program anyway…\n\n\x1B[36m{}\x1B[39m",
                            "─".repeat(80)
                        );
                    }

                    let args = matches
                        .get_many::<OsString>(ARGS_FOR_APP)
                        .unwrap_or_default()
                        .map(|s| s.as_os_str());

                    // don't waste time deallocating; the process ends anyway
                    // ManuallyDrop will leak the bytes because we don't drop manually
                    let bytes = &ManuallyDrop::new(std::fs::read(&binary_path).unwrap());

                    roc_run(
                        &arena,
                        path,
                        should_run_expects,
                        target,
                        args,
                        bytes,
                        expect_metadata,
                    )
                }
                BuildAndRunIfNoErrors => {
                    if problems.fatally_errored {
                        problems.print_error_warning_count(total_time);
                        println!(
                            ".\n\nCannot run program due to fatal error…\n\n\x1B[36m{}\x1B[39m",
                            "─".repeat(80)
                        );

                        // Return a nonzero exit code due to fatal problem
                        return Ok(problems.exit_code());
                    }
                    debug_assert_eq!(
                        problems.errors, 0,
                        "if there are non-fatal errors, they should have been returned as an error variant"
                    );

                    if problems.warnings > 0 {
                        problems.print_error_warning_count(total_time);
                        println!(
                            ".\n\nRunning program…\n\n\x1B[36m{}\x1B[39m",
                            "─".repeat(80)
                        );
                    }

                    let args = matches
                        .get_many::<OsString>(ARGS_FOR_APP)
                        .unwrap_or_default()
                        .map(|s| s.as_os_str());

                    // don't waste time deallocating; the process ends anyway
                    // ManuallyDrop will leak the bytes because we don't drop manually
                    let bytes = &ManuallyDrop::new(std::fs::read(&binary_path).unwrap());

                    roc_run(
                        &arena,
                        path,
                        should_run_expects,
                        target,
                        args,
                        bytes,
                        expect_metadata,
                    )
                }
            }
        }
        Err(BuildFileError::ErrorModule { module, total_time }) => {
            handle_error_module(module, total_time, path.as_os_str(), true)
        }
        Err(BuildFileError::LoadingProblem(problem)) => handle_loading_problem(problem),
    }
}

fn roc_run<'a, I: IntoIterator<Item = &'a OsStr>>(
    arena: &Bump,
    script_path: &Path,
    should_run_expects: bool,
    target: Target,
    args: I,
    binary_bytes: &[u8],
    expect_metadata: ExpectMetadata,
) -> io::Result<i32> {
    match target.architecture() {
        Architecture::Wasm32 => {
            let executable = roc_run_executable_file_path(binary_bytes)?;
            let path = executable.as_path();
            // If possible, report the generated executable name relative to the current dir.
            let generated_filename = path
                .strip_prefix(env::current_dir().unwrap())
                .unwrap_or(path);

            #[cfg(target_family = "unix")]
            {
                use std::os::unix::ffi::OsStrExt;

                run_wasm(
                    generated_filename,
                    args.into_iter().map(|os_str| os_str.as_bytes()),
                );
            }

            #[cfg(not(target_family = "unix"))]
            {
                run_wasm(
                    generated_filename,
                    args.into_iter().map(|os_str| {
                        os_str.to_str().expect(
                            "Roc does not currently support passing non-UTF8 arguments to Wasm.",
                        )
                    }),
                );
            }

            Ok(0)
        }
        _ => roc_run_native(
            arena,
            script_path,
            should_run_expects,
            args,
            binary_bytes,
            expect_metadata,
        ),
    }
}

#[cfg(target_family = "unix")]
fn os_str_as_utf8_bytes(os_str: &OsStr) -> &[u8] {
    use std::os::unix::ffi::OsStrExt;
    os_str.as_bytes()
}

#[cfg(not(target_family = "unix"))]
fn os_str_as_utf8_bytes(os_str: &OsStr) -> &[u8] {
    os_str.to_str().unwrap().as_bytes()
}

fn make_argv_envp<'a, I: IntoIterator<Item = S>, S: AsRef<OsStr>>(
    arena: &'a Bump,
    script_path: &Path,
    args: I,
) -> (
    bumpalo::collections::Vec<'a, CString>,
    bumpalo::collections::Vec<'a, CString>,
) {
    use bumpalo::collections::CollectIn;

    let path_cstring = CString::new(os_str_as_utf8_bytes(script_path.as_os_str())).unwrap();

    // argv is an array of pointers to strings passed to the new program
    // as its command-line arguments.  By convention, the first of these
    // strings (i.e., argv[0]) should contain the filename associated
    // with the file being executed.  The argv array must be terminated
    // by a NULL pointer. (Thus, in the new program, argv[argc] will be NULL.)
    let it = args
        .into_iter()
        .map(|x| CString::new(os_str_as_utf8_bytes(x.as_ref())).unwrap());

    let argv_cstrings: bumpalo::collections::Vec<CString> =
        std::iter::once(path_cstring).chain(it).collect_in(arena);

    // envp is an array of pointers to strings, conventionally of the
    // form key=value, which are passed as the environment of the new
    // program.  The envp array must be terminated by a NULL pointer.
    let mut buffer = Vec::with_capacity(100);
    let envp_cstrings: bumpalo::collections::Vec<CString> = std::env::vars_os()
        .map(|(k, v)| {
            buffer.clear();

            use std::io::Write;
            buffer.write_all(os_str_as_utf8_bytes(&k)).unwrap();
            buffer.write_all(b"=").unwrap();
            buffer.write_all(os_str_as_utf8_bytes(&v)).unwrap();

            CString::new(buffer.as_slice()).unwrap()
        })
        .collect_in(arena);

    (argv_cstrings, envp_cstrings)
}

/// Run on the native OS (not on wasm)
#[cfg(target_family = "unix")]
fn roc_run_native<I: IntoIterator<Item = S>, S: AsRef<OsStr>>(
    arena: &Bump,
    script_path: &Path,
    should_run_expects: bool,
    args: I,
    binary_bytes: &[u8],
    expect_metadata: ExpectMetadata,
) -> std::io::Result<i32> {
    use bumpalo::collections::CollectIn;

    let executable = roc_run_executable_file_path(binary_bytes)?;
    let (argv_cstrings, envp_cstrings) = make_argv_envp(arena, script_path, args);

    let argv: bumpalo::collections::Vec<*const c_char> = argv_cstrings
        .iter()
        .map(|s| s.as_ptr())
        .chain([std::ptr::null()])
        .collect_in(arena);

    let envp: bumpalo::collections::Vec<*const c_char> = envp_cstrings
        .iter()
        .map(|s| s.as_ptr())
        .chain([std::ptr::null()])
        .collect_in(arena);

    if should_run_expects {
        roc_dev_native(arena, executable, argv, envp, expect_metadata);
    } else {
        unsafe { roc_run_native_fast(executable, &argv, &envp) };
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
    // We store the TempDir in the onDisk variant alongside the path to the executable,
    // so that the TempDir doesn't get dropped until after we're done with the path.
    // If we didn't do that, then the tempdir would potentially get deleted by the
    // TempDir's Drop impl before the file had been executed.
    #[allow(dead_code)]
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
                let path_cstring = CString::new(path.to_str().unwrap()).unwrap();

                libc::execve(path_cstring.as_ptr().cast(), argv.as_ptr(), envp.as_ptr())
            }
        }
    }
}

// with Expect
#[cfg(target_family = "unix")]
fn roc_dev_native(
    arena: &Bump,
    executable: ExecutableFile,
    argv: bumpalo::collections::Vec<*const c_char>,
    envp: bumpalo::collections::Vec<*const c_char>,
    expect_metadata: ExpectMetadata,
) -> ! {
    use std::sync::{atomic::AtomicBool, Arc};

    use roc_repl_expect::run::{ChildProcessMsg, ExpectMemory};

    let ExpectMetadata {
        mut expectations,
        interns,
        layout_interner,
    } = expect_metadata;

    let shm_name = format!("/roc_expect_buffer_{}", std::process::id());
    let mut memory = ExpectMemory::create_or_reuse_mmap(&shm_name);

    let layout_interner = layout_interner.into_global();

    match unsafe { libc::fork() } {
        0 => unsafe {
            // we are the child

            executable.execve(&argv, &envp);

            // Display a human-friendly error message
            println!("Error {:?}", std::io::Error::last_os_error());

            std::process::exit(1);
        },
        -1 => {
            // something failed

            // Display a human-friendly error message
            println!("Error {:?}", std::io::Error::last_os_error());

            std::process::exit(1)
        }
        pid @ 1.. => {
            let sigchld = Arc::new(AtomicBool::new(false));
            signal_hook::flag::register(signal_hook::consts::SIGCHLD, Arc::clone(&sigchld))
                .unwrap();

            let exit_code = loop {
                match memory.wait_for_child(sigchld.clone()) {
                    ChildProcessMsg::Terminate => {
                        let mut status = 0;
                        let options = 0;
                        unsafe { libc::waitpid(pid, &mut status, options) };

                        // if `WIFEXITED` returns false, `WEXITSTATUS` will just return junk
                        break if libc::WIFEXITED(status) {
                            libc::WEXITSTATUS(status)
                        } else {
                            // we don't have an exit code, but something went wrong if we're in this else
                            1
                        };
                    }
                    ChildProcessMsg::Expect => {
                        let mut writer = std::io::stdout();
                        roc_repl_expect::run::render_expects_in_memory(
                            &mut writer,
                            arena,
                            &mut expectations,
                            &interns,
                            &layout_interner,
                            &memory,
                        )
                        .unwrap();

                        memory.reset();
                    }
                }
            };

            std::process::exit(exit_code)
        }
        _ => unreachable!(),
    }
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

    let path = PathBuf::from(format!("/proc/self/fd/{fd}"));

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
        .truncate(true)
        .mode(0o777) // create the file as executable
        .open(&app_path_buf)?;

    file.write_all(binary_bytes)?;

    // We store the TempDir in this variant alongside the path to the executable,
    // so that the TempDir doesn't get dropped until after we're done with the path.
    // If we didn't do that, then the tempdir would potentially get deleted by the
    // TempDir's Drop impl before the file had been executed.
    Ok(ExecutableFile::OnDisk(temp_dir, app_path_buf))
}

#[cfg(target_family = "windows")]
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
    arena: &Bump, // This should be passed an owned value, not a reference, so we can usefully mem::forget it!
    script_path: &Path,
    should_run_expects: bool,
    args: I,
    binary_bytes: &[u8],
    _expect_metadata: ExpectMetadata,
) -> io::Result<i32> {
    use bumpalo::collections::CollectIn;

    unsafe {
        let executable = roc_run_executable_file_path(binary_bytes)?;

        // TODO forward the arguments
        let (argv_cstrings, envp_cstrings) = make_argv_envp(&arena, script_path, args);

        let argv: bumpalo::collections::Vec<*const c_char> = argv_cstrings
            .iter()
            .map(|s| s.as_ptr())
            .chain([std::ptr::null()])
            .collect_in(arena);

        let envp: bumpalo::collections::Vec<*const c_char> = envp_cstrings
            .iter()
            .map(|s| s.as_ptr())
            .chain([std::ptr::null()])
            .collect_in(arena);

        if should_run_expects {
            // roc_run_native_debug(executable, &argv, &envp, expectations, interns)
            internal_error!("running `expect`s does not currently work on windows");
        } else {
            roc_run_native_fast(executable, &argv, &envp);
        }
    }

    Ok(1)
}

#[cfg(feature = "run-wasm32")]
fn run_wasm<I: Iterator<Item = S>, S: AsRef<[u8]>>(wasm_path: &std::path::Path, args: I) {
    use bumpalo::collections::Vec;
    use roc_wasm_interp::{DefaultImportDispatcher, Instance};

    let bytes = std::fs::read(wasm_path).unwrap();
    let arena = Bump::new();

    let mut argv = Vec::<&[u8]>::new_in(&arena);
    for arg in args {
        let mut arg_copy = Vec::<u8>::new_in(&arena);
        arg_copy.extend_from_slice(arg.as_ref());
        argv.push(arg_copy.into_bump_slice());
    }
    let import_dispatcher = DefaultImportDispatcher::new(&argv);

    let mut instance = Instance::from_bytes(&arena, &bytes, import_dispatcher, false).unwrap();

    instance
        .call_export("_start", [])
        .unwrap()
        .unwrap()
        .expect_i32()
        .unwrap();
}

#[cfg(not(feature = "run-wasm32"))]
fn run_wasm<I: Iterator<Item = S>, S: AsRef<[u8]>>(_wasm_path: &std::path::Path, _args: I) {
    println!("Running wasm files is not supported on this target.");
}
