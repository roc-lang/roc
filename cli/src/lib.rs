#[macro_use]
extern crate clap;

use build::{BuildOutcome, BuiltFile};
use bumpalo::Bump;
use clap::{App, AppSettings, Arg, ArgMatches};
use roc_build::link::LinkType;
use roc_load::file::LoadingProblem;
use roc_mono::ir::OptLevel;
use std::env;
use std::io;
use std::path::{Path, PathBuf};
use std::process;
use std::process::Command;
use target_lexicon::Triple;

pub mod build;
pub mod repl;

pub const CMD_RUN: &str = "run";
pub const CMD_BUILD: &str = "build";
pub const CMD_REPL: &str = "repl";
pub const CMD_EDIT: &str = "edit";
pub const CMD_DOCS: &str = "docs";

pub const FLAG_DEBUG: &str = "debug";
pub const FLAG_OPTIMIZE: &str = "optimize";
pub const ROC_FILE: &str = "ROC_FILE";
pub const DIRECTORY_OR_FILES: &str = "DIRECTORY_OR_FILES";
pub const ARGS_FOR_APP: &str = "ARGS_FOR_APP";

pub fn build_app<'a>() -> App<'a> {
    let app = App::new("roc")
        .version(crate_version!())
        .subcommand(App::new(CMD_BUILD)
            .about("Build a program")
            .arg(
                Arg::with_name(ROC_FILE)
                    .help("The .roc file to build")
                    .required(true),
            )
            .arg(
                Arg::with_name(FLAG_OPTIMIZE)
                    .long(FLAG_OPTIMIZE)
                    .help("Optimize the compiled program to run faster. (Optimization takes time to complete.)")
                    .required(false),
            )
            .arg(
                Arg::with_name(FLAG_DEBUG)
                    .long(FLAG_DEBUG)
                    .help("Store LLVM debug information in the generated program")
                    .required(false),
            )
        )
        .subcommand(App::new(CMD_RUN)
            .about("Build and run a program")
            .setting(AppSettings::TrailingVarArg)
            .arg(
                Arg::with_name(FLAG_OPTIMIZE)
                    .long(FLAG_OPTIMIZE)
                    .help("Optimize the compiled program to run faster. (Optimization takes time to complete.)")
                    .required(false),
            )
            .arg(
                Arg::with_name(FLAG_DEBUG)
                    .long(FLAG_DEBUG)
                    .help("Store LLVM debug information in the generated program")
                    .required(false),
            )
            .arg(
                Arg::with_name(ROC_FILE)
                    .help("The .roc file of an app to build and run")
                    .required(true),
            )
            .arg(
                Arg::with_name(ARGS_FOR_APP)
                    .help("Arguments to pass into the app being run")
                    .multiple(true),
            )
        )
        .subcommand(App::new(CMD_REPL)
            .about("Launch the interactive Read Eval Print Loop (REPL)")
        )
        .subcommand(
            App::new(CMD_DOCS)
                .about("Generate documentation for Roc modules")
                .arg(Arg::with_name(DIRECTORY_OR_FILES)
                    .index(1)
                    .multiple(true)
                    .required(true)
                    .help("The directory or files to build documentation for")

                )
        );

    if cfg!(feature = "edit") {
        app.subcommand(
            App::new(CMD_EDIT).about("Launch the Roc editor").arg(
                Arg::with_name(DIRECTORY_OR_FILES)
                    .index(1)
                    .multiple(true)
                    .required(false)
                    .help("(optional) The directory or files to open on launch."),
            ),
        )
    } else {
        app
    }
}

pub fn docs(files: Vec<PathBuf>) {
    roc_docs::generate(
        files,
        roc_builtins::std::standard_stdlib(),
        Path::new("./generated-docs"),
    )
}

pub enum BuildConfig {
    BuildOnly,
    BuildAndRun { roc_file_arg_index: usize },
}

#[cfg(feature = "llvm")]
pub fn build(target: &Triple, matches: &ArgMatches, config: BuildConfig) -> io::Result<i32> {
    use build::build_file;
    use BuildConfig::*;

    let arena = Bump::new();
    let filename = matches.value_of(ROC_FILE).unwrap();

    let original_cwd = std::env::current_dir()?;
    let opt_level = if matches.is_present(FLAG_OPTIMIZE) {
        OptLevel::Optimize
    } else {
        OptLevel::Normal
    };
    let emit_debug_info = matches.is_present(FLAG_DEBUG);

    let path = Path::new(filename).canonicalize().unwrap();
    let src_dir = path.parent().unwrap().canonicalize().unwrap();

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

    let res_binary_path = build_file(
        &arena,
        target,
        src_dir,
        path,
        opt_level,
        emit_debug_info,
        LinkType::Executable,
    );

    match res_binary_path {
        Ok(BuiltFile {
            binary_path,
            outcome,
            total_time,
        }) => {
            match config {
                BuildOnly => {
                    // If possible, report the generated executable name relative to the current dir.
                    let generated_filename = binary_path
                        .strip_prefix(env::current_dir().unwrap())
                        .unwrap_or(&binary_path);

                    // Return a nonzero exit code if there were problems
                    let status_code = match outcome {
                        BuildOutcome::NoProblems => 0,
                        BuildOutcome::OnlyWarnings => 1,
                        BuildOutcome::Errors => 2,
                    };

                    // No need to waste time freeing this memory,
                    // since the process is about to exit anyway.
                    std::mem::forget(arena);

                    println!(
                        "🎉 Built {} in {} ms",
                        generated_filename.to_str().unwrap(),
                        total_time.as_millis()
                    );

                    Ok(status_code)
                }
                BuildAndRun { roc_file_arg_index } => {
                    let mut cmd = Command::new(binary_path);

                    // Forward all the arguments after the .roc file argument
                    // to the new process. This way, you can do things like:
                    //
                    // roc run app.roc foo bar baz
                    //
                    // ...and have it so that app.roc will receive only `foo`,
                    // `bar`, and `baz` as its arguments.
                    for (index, arg) in std::env::args().enumerate() {
                        if index > roc_file_arg_index {
                            cmd.arg(arg);
                        }
                    }

                    // Run the compiled app
                    let exit_status = cmd
                        .current_dir(original_cwd)
                        .spawn()
                        .unwrap_or_else(|err| panic!("Failed to run app after building it: {:?}", err))
                        .wait()
                        .expect("TODO gracefully handle block_on failing when roc run spawns a subprocess for the compiled app");

                    // `roc run` exits with the same status code as the app it ran.
                    //
                    // If you want to know whether there were compilation problems
                    // via status code, use either `roc build` or `roc check` instead!
                    match exit_status.code() {
                        Some(code) => Ok(code),
                        None => {
                            todo!("TODO gracefully handle the roc run subprocess terminating with a signal.");
                        }
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
