//! Provides the core CLI functionality for the Roc binary.

#[macro_use]
extern crate const_format;

use build::BuiltFile;
use bumpalo::Bump;
use clap::{Arg, ArgMatches, Command, ValueSource};
use roc_build::link::{LinkType, LinkingStrategy};
use roc_build::program::{CodeGenBackend, CodeGenOptions};
use roc_error_macros::{internal_error, user_error};
use roc_load::{ExpectMetadata, LoadingProblem, Threading};
use roc_mono::ir::OptLevel;
use roc_packaging::cache::RocCacheDir;
use roc_packaging::tarball::Compression;
use roc_reporting::cli::Problems;
use std::env;
use std::ffi::{CString, OsStr};
use std::io;
use std::mem::ManuallyDrop;
use std::os::raw::{c_char, c_int};
use std::path::{Path, PathBuf};
use std::process;
use std::time::Instant;
use strum::{EnumIter, IntoEnumIterator, IntoStaticStr};
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
pub const CMD_GEN_STUB_LIB: &str = "gen-stub-lib";

pub const FLAG_DEBUG: &str = "debug";
pub const FLAG_BUNDLE: &str = "bundle";
pub const FLAG_DEV: &str = "dev";
pub const FLAG_OPTIMIZE: &str = "optimize";
pub const FLAG_MAX_THREADS: &str = "max-threads";
pub const FLAG_OPT_SIZE: &str = "opt-size";
pub const FLAG_LIB: &str = "lib";
pub const FLAG_NO_LINK: &str = "no-link";
pub const FLAG_TARGET: &str = "target";
pub const FLAG_TIME: &str = "time";
pub const FLAG_LINKER: &str = "linker";
pub const FLAG_PREBUILT: &str = "prebuilt-platform";
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

    let flag_prebuilt = Arg::new(FLAG_PREBUILT)
        .long(FLAG_PREBUILT)
        .help("Assume the platform has been prebuilt and skip rebuilding the platform\n(This is enabled by default when using `roc build` with a --target other than `--target <current machine>`.)")
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
            .arg(flag_prebuilt.clone())
            .arg(flag_wasm_stack_size_kb.clone())
            .arg(
                Arg::new(FLAG_TARGET)
                    .long(FLAG_TARGET)
                    .help("Choose a different target")
                    .default_value(Target::default().into())
                    .possible_values(Target::iter().map(|target| {
                        Into::<&'static str>::into(target)
                    }))
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_LIB)
                    .long(FLAG_LIB)
                    .help("Build a C library instead of an executable")
                    .required(false),
            )
            .arg(
                Arg::new(FLAG_BUNDLE)
                    .long(FLAG_BUNDLE)
                    .help("Create an archive of a package (for example, a .tar, .tar.gz, or .tar.br file), so others can add it as a HTTPS dependency.")
                    .conflicts_with(FLAG_TARGET)
                    .possible_values([".tar", ".tar.gz", ".tar.br"])
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
            .arg(flag_prebuilt.clone())
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
            .arg(flag_prebuilt.clone())
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
            .arg(flag_prebuilt.clone())
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
        .subcommand(Command::new(CMD_GEN_STUB_LIB)
            .about("Generate a stubbed shared library that can be used for linking a platform binary.\nThe stubbed library has prototypes, but no function bodies.\n\nNote: This command will be removed in favor of just using `roc build` once all platforms support the surgical linker")
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file for an app using the platform")
                    .allow_invalid_utf8(true)
                    .required(true)
            )
            .arg(
                Arg::new(FLAG_TARGET)
                    .long(FLAG_TARGET)
                    .help("Choose a different target")
                    .default_value(Target::default().into())
                    .possible_values(Target::iter().map(|target| {
                        Into::<&'static str>::into(target)
                    }))
                    .required(false),
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
        .arg(flag_prebuilt)
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
    use roc_packaging::cache;
    use roc_target::TargetInfo;

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
        palette: roc_reporting::report::DEFAULT_PALETTE,
        threading,
        exec_mode: ExecutionMode::Test,
    };
    let loaded = roc_load::load_and_monomorphize(
        arena,
        path.to_path_buf(),
        subs_by_module,
        RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
        load_config,
    )
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

    let (failed, passed) = roc_repl_expect::run::run_toplevel_expects(
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
    roc_cache_dir: RocCacheDir<'_>,
    link_type: LinkType,
) -> io::Result<i32> {
    use build::build_file;
    use BuildConfig::*;

    let filename = matches.value_of_os(ROC_FILE).unwrap();
    let path_buf = {
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

        if config == BuildConfig::BuildOnly && matches.is_present(FLAG_BUNDLE) {
            let start_time = Instant::now();

            let compression =
                Compression::try_from(matches.value_of(FLAG_BUNDLE).unwrap()).unwrap();

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
            // a tarball so this code can be distributed via a HTTPS
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

        path.to_path_buf()
    };

    // the process will end after this function,
    // so we don't want to spend time freeing these values
    let arena = ManuallyDrop::new(Bump::new());

    let code_gen_backend = if matches!(triple.architecture, Architecture::Wasm32) {
        CodeGenBackend::Wasm
    } else {
        match matches.is_present(FLAG_DEV) {
            true => CodeGenBackend::Assembly,
            false => CodeGenBackend::Llvm,
        }
    };

    let opt_level = if let BuildConfig::BuildAndRunIfNoErrors = config {
        OptLevel::Development
    } else {
        match (
            matches.is_present(FLAG_OPTIMIZE),
            matches.is_present(FLAG_OPT_SIZE),
        ) {
            (true, false) => OptLevel::Optimize,
            (false, true) => OptLevel::Size,
            (false, false) => OptLevel::Normal,
            (true, true) => {
                user_error!("build can be only one of `--optimize` and `--opt-size`")
            }
        }
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
        && matches!(code_gen_backend, CodeGenBackend::Wasm);

    let linking_strategy = if wasm_dev_backend {
        LinkingStrategy::Additive
    } else if !roc_linker::supported(link_type, &triple)
        || matches.value_of(FLAG_LINKER) == Some("legacy")
    {
        LinkingStrategy::Legacy
    } else {
        LinkingStrategy::Surgical
    };

    let prebuilt = if matches.is_present(FLAG_PREBUILT) {
        matches.value_of(FLAG_PREBUILT) == Some("true")
    } else {
        // When compiling for a different target, default to assuming a prebuilt platform.
        // Otherwise compilation would most likely fail because many toolchains assume you're compiling for the current machine.
        // We make an exception for Wasm, because cross-compiling is the norm in that case.
        triple != Triple::host() && !matches!(triple.architecture, Architecture::Wasm32)
    };

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

    let code_gen_options = CodeGenOptions {
        backend: code_gen_backend,
        opt_level,
        emit_debug_info,
    };

    let res_binary_path = build_file(
        &arena,
        &triple,
        path_buf,
        code_gen_options,
        emit_timings,
        link_type,
        linking_strategy,
        prebuilt,
        threading,
        wasm_dev_stack_bytes,
        roc_cache_dir,
        build_ordering,
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

                    print_problems(problems, total_time);
                    println!(" while successfully building:\n\n    {generated_filename}");

                    // Return a nonzero exit code if there were problems
                    Ok(problems.exit_code())
                }
                BuildAndRun => {
                    if problems.errors > 0 || problems.warnings > 0 {
                        print_problems(problems, total_time);
                        println!(
                            ".\n\nRunning program anyway…\n\n\x1B[36m{}\x1B[39m",
                            "─".repeat(80)
                        );
                    }

                    let args = matches.values_of_os(ARGS_FOR_APP).unwrap_or_default();

                    // don't waste time deallocating; the process ends anyway
                    // ManuallyDrop will leak the bytes because we don't drop manually
                    let bytes = &ManuallyDrop::new(std::fs::read(&binary_path).unwrap());

                    roc_run(&arena, opt_level, triple, args, bytes, expect_metadata)
                }
                BuildAndRunIfNoErrors => {
                    debug_assert_eq!(
                        problems.errors, 0,
                        "if there are errors, they should have been returned as an error variant"
                    );
                    if problems.warnings > 0 {
                        print_problems(problems, total_time);
                        println!(
                            ".\n\nRunning program…\n\n\x1B[36m{}\x1B[39m",
                            "─".repeat(80)
                        );
                    }

                    let args = matches.values_of_os(ARGS_FOR_APP).unwrap_or_default();

                    // don't waste time deallocating; the process ends anyway
                    // ManuallyDrop will leak the bytes because we don't drop manually
                    let bytes = &ManuallyDrop::new(std::fs::read(&binary_path).unwrap());

                    roc_run(&arena, opt_level, triple, args, bytes, expect_metadata)
                }
            }
        }
        Err(BuildFileError::ErrorModule {
            mut module,
            total_time,
        }) => {
            debug_assert!(module.total_problems() > 0);

            let problems = roc_build::program::report_problems_typechecked(&mut module);

            print_problems(problems, total_time);

            print!(".\n\nYou can run the program anyway with \x1B[32mroc run");

            // If you're running "main.roc" then you can just do `roc run`
            // to re-run the program.
            if filename != DEFAULT_ROC_FILENAME {
                print!(" {}", &filename.to_string_lossy());
            }

            println!("\x1B[39m");

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

fn print_problems(problems: Problems, total_time: std::time::Duration) {
    const GREEN: usize = 32;
    const YELLOW: usize = 33;

    print!(
        "\x1B[{}m{}\x1B[39m {} and \x1B[{}m{}\x1B[39m {} found in {} ms",
        match problems.errors {
            0 => GREEN,
            _ => YELLOW,
        },
        problems.errors,
        match problems.errors {
            1 => "error",
            _ => "errors",
        },
        match problems.warnings {
            0 => GREEN,
            _ => YELLOW,
        },
        problems.warnings,
        match problems.warnings {
            1 => "warning",
            _ => "warnings",
        },
        total_time.as_millis(),
    );
}

fn roc_run<'a, I: IntoIterator<Item = &'a OsStr>>(
    arena: &Bump,
    opt_level: OptLevel,
    triple: Triple,
    args: I,
    binary_bytes: &[u8],
    expect_metadata: ExpectMetadata,
) -> io::Result<i32> {
    match triple.architecture {
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
        _ => roc_run_native(arena, opt_level, args, binary_bytes, expect_metadata),
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
    executable: &ExecutableFile,
    args: I,
) -> (
    bumpalo::collections::Vec<'a, CString>,
    bumpalo::collections::Vec<'a, CString>,
) {
    use bumpalo::collections::CollectIn;

    let path = executable.as_path();
    let path_cstring = CString::new(os_str_as_utf8_bytes(path.as_os_str())).unwrap();

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
    opt_level: OptLevel,
    args: I,
    binary_bytes: &[u8],
    expect_metadata: ExpectMetadata,
) -> std::io::Result<i32> {
    use bumpalo::collections::CollectIn;

    unsafe {
        let executable = roc_run_executable_file_path(binary_bytes)?;
        let (argv_cstrings, envp_cstrings) = make_argv_envp(arena, &executable, args);

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

        match opt_level {
            OptLevel::Development => roc_dev_native(arena, executable, argv, envp, expect_metadata),
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
    use roc_repl_expect::run::ExpectMemory;
    use signal_hook::{
        consts::signal::SIGCHLD,
        consts::signal::{SIGUSR1, SIGUSR2},
        iterator::Signals,
    };

    let ExpectMetadata {
        mut expectations,
        interns,
        layout_interner,
    } = expect_metadata;

    let mut signals = Signals::new(&[SIGCHLD, SIGUSR1, SIGUSR2]).unwrap();

    // let shm_name =
    let shm_name = format!("/roc_expect_buffer_{}", std::process::id());
    let memory = ExpectMemory::create_or_reuse_mmap(&shm_name);

    let layout_interner = layout_interner.into_global();

    let mut writer = std::io::stdout();

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
        1.. => {
            for sig in &mut signals {
                match sig {
                    SIGCHLD => break,
                    SIGUSR1 => {
                        // this is the signal we use for an expect failure. Let's see what the child told us

                        roc_repl_expect::run::render_expects_in_memory(
                            &mut writer,
                            arena,
                            &mut expectations,
                            &interns,
                            &layout_interner,
                            &memory,
                        )
                        .unwrap();
                    }
                    SIGUSR2 => {
                        // this is the signal we use for a dbg

                        roc_repl_expect::run::render_dbgs_in_memory(
                            &mut writer,
                            arena,
                            &mut expectations,
                            &interns,
                            &layout_interner,
                            &memory,
                        )
                        .unwrap();
                    }
                    _ => println!("received signal {}", sig),
                }
            }

            std::process::exit(0)
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
    arena: &Bump, // This should be passed an owned value, not a reference, so we can usefully mem::forget it!
    opt_level: OptLevel,
    args: I,
    binary_bytes: &[u8],
    _expect_metadata: ExpectMetadata,
) -> io::Result<i32> {
    use bumpalo::collections::CollectIn;

    unsafe {
        let executable = roc_run_executable_file_path(binary_bytes)?;

        // TODO forward the arguments
        let (argv_cstrings, envp_cstrings) = make_argv_envp(&arena, &executable, args);

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

        match opt_level {
            OptLevel::Development => {
                // roc_run_native_debug(executable, &argv, &envp, expectations, interns)
                internal_error!("running `expect`s does not currently work on windows")
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

#[derive(Debug, Copy, Clone, EnumIter, IntoStaticStr, PartialEq, Eq)]
pub enum Target {
    #[strum(serialize = "system")]
    System,
    #[strum(serialize = "linux32")]
    Linux32,
    #[strum(serialize = "linux64")]
    Linux64,
    #[strum(serialize = "windows64")]
    Windows64,
    #[strum(serialize = "wasm32")]
    Wasm32,
}

impl Default for Target {
    fn default() -> Self {
        Target::System
    }
}

impl Target {
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
                operating_system: OperatingSystem::Wasi,
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
        write!(f, "{}", Into::<&'static str>::into(self))
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
