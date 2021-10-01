#[macro_use]
extern crate clap;

#[macro_use]
extern crate const_format;

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
use target_lexicon::BinaryFormat;
use target_lexicon::{Architecture, OperatingSystem, Triple, X86_32Architecture};

pub mod build;
pub mod repl;

pub const CMD_RUN: &str = "run";
pub const CMD_BUILD: &str = "build";
pub const CMD_REPL: &str = "repl";
pub const CMD_EDIT: &str = "edit";
pub const CMD_DOCS: &str = "docs";
pub const CMD_CHECK: &str = "check";

pub const FLAG_DEBUG: &str = "debug";
pub const FLAG_DEV: &str = "dev";
pub const FLAG_OPTIMIZE: &str = "optimize";
pub const FLAG_LIB: &str = "lib";
pub const FLAG_BACKEND: &str = "backend";
pub const FLAG_TIME: &str = "time";
pub const ROC_FILE: &str = "ROC_FILE";
pub const BACKEND: &str = "BACKEND";
pub const DIRECTORY_OR_FILES: &str = "DIRECTORY_OR_FILES";
pub const ARGS_FOR_APP: &str = "ARGS_FOR_APP";

pub fn build_app<'a>() -> App<'a> {
    let app = App::new("roc")
        .version(concatcp!(crate_version!(), "\n"))
        .about("Runs the given .roc file. Use one of the SUBCOMMANDS below to do something else!")
        .subcommand(App::new(CMD_BUILD)
            .about("Build a binary from the given .roc file, but don't run it")
            .arg(
                Arg::with_name(ROC_FILE)
                    .help("The .roc file to build")
                    .required(true),
            )
            .arg(
                Arg::with_name(FLAG_OPTIMIZE)
                    .long(FLAG_OPTIMIZE)
                    .help("Optimize your compiled Roc program to run faster. (Optimization takes time to complete.)")
                    .required(false),
            )
            .arg(
                Arg::with_name(FLAG_DEV)
                    .long(FLAG_DEV)
                    .help("Make compilation as fast as possible. (Runtime performance may suffer)")
                    .required(false),
            )
            .arg(
                Arg::with_name(FLAG_BACKEND)
                    .long(FLAG_BACKEND)
                    .help("Choose a different backend")
                    // .requires(BACKEND)
                .default_value(Backend::default().as_str())
                    .possible_values(Backend::OPTIONS)
                    .required(false),
            )
            .arg(
                Arg::with_name(FLAG_LIB)
                    .long(FLAG_LIB)
                    .help("Build a C library instead of an executable.")
                    .required(false),
            )
            .arg(
                Arg::with_name(FLAG_DEBUG)
                    .long(FLAG_DEBUG)
                    .help("Store LLVM debug information in the generated program")
                    .required(false),
            )
            .arg(
                Arg::with_name(FLAG_TIME)
                    .long(FLAG_TIME)
                    .help("Prints detailed compilation time information.")
                    .required(false),
            )
        )
        .subcommand(App::new(CMD_RUN)
            .about("DEPRECATED - now use `roc [FILE]` instead of `roc run [FILE]`")
            .setting(AppSettings::TrailingVarArg)
            .arg(
                Arg::with_name(FLAG_OPTIMIZE)
                    .long(FLAG_OPTIMIZE)
                    .help("Optimize the compiled program to run faster. (Optimization takes time to complete.)")
                    .required(false),
            )
            .arg(
                Arg::with_name(FLAG_DEV)
                    .long(FLAG_DEV)
                    .help("Make compilation as fast as possible. (Runtime performance may suffer)")
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
                    .help("The .roc file of an app to run")
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
        .subcommand(App::new(CMD_CHECK)
            .about("Build a binary from the given .roc file, but don't run it")
            .arg(
                Arg::with_name(FLAG_TIME)
                    .long(FLAG_TIME)
                    .help("Prints detailed compilation time information.")
                    .required(false),
            )
            .arg(
                Arg::with_name(ROC_FILE)
                    .help("The .roc file of an app to run")
                    .required(true),
            )
            )
        .subcommand(
            App::new(CMD_DOCS)
                .about("Generate documentation for Roc modules")
                .arg(Arg::with_name(DIRECTORY_OR_FILES)
                    .index(1)
                    .multiple(true)
                    .required(false)
                    .help("The directory or files to build documentation for")

                )
        )
        .setting(AppSettings::TrailingVarArg)
        .arg(
            Arg::with_name(FLAG_OPTIMIZE)
                .long(FLAG_OPTIMIZE)
                .help("Optimize the compiled program to run faster. (Optimization takes time to complete.)")
                .requires(ROC_FILE)
                .required(false),
        )
            .arg(
                Arg::with_name(FLAG_DEV)
                    .long(FLAG_DEV)
                    .help("Make compilation as fast as possible. (Runtime performance may suffer)")
                    .required(false),
            )
        .arg(
            Arg::with_name(FLAG_DEBUG)
                .long(FLAG_DEBUG)
                .help("Store LLVM debug information in the generated program")
                .requires(ROC_FILE)
                .required(false),
        )
        .arg(
            Arg::with_name(FLAG_TIME)
                .long(FLAG_TIME)
                .help("Prints detailed compilation time information.")
                    .required(false),
        )
        .arg(
            Arg::with_name(FLAG_BACKEND)
                .long(FLAG_BACKEND)
                .help("Choose a different backend")
                // .requires(BACKEND)
                .default_value(Backend::default().as_str())
                .possible_values(Backend::OPTIONS)
                .required(false),
        )
        .arg(
            Arg::with_name(ROC_FILE)
                .help("The .roc file of an app to build and run")
                .required(false),
        )
        .arg(
            Arg::with_name(ARGS_FOR_APP)
                .help("Arguments to pass into the app being run")
                .requires(ROC_FILE)
                .multiple(true),
        );

    if cfg!(feature = "editor") {
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
    roc_docs::generate_docs_html(
        files,
        roc_builtins::std::standard_stdlib(),
        Path::new("./generated-docs"),
    )
}

#[derive(Debug, PartialEq, Eq)]
pub enum BuildConfig {
    BuildOnly,
    BuildAndRun { roc_file_arg_index: usize },
}

#[cfg(feature = "llvm")]
pub fn build(matches: &ArgMatches, config: BuildConfig) -> io::Result<i32> {
    use build::build_file;
    use std::str::FromStr;
    use BuildConfig::*;

    let backend = match matches.value_of(FLAG_BACKEND) {
        Some(name) => Backend::from_str(name).unwrap(),
        None => Backend::default(),
    };

    let target = backend.to_triple();

    let arena = Bump::new();
    let filename = matches.value_of(ROC_FILE).unwrap();

    let original_cwd = std::env::current_dir()?;
    let opt_level = match (
        matches.is_present(FLAG_OPTIMIZE),
        matches.is_present(FLAG_DEV),
    ) {
        (true, false) => OptLevel::Optimize,
        (true, true) => panic!("development cannot be optimized!"),
        (false, true) => OptLevel::Development,
        (false, false) => OptLevel::Normal,
    };
    let emit_debug_info = matches.is_present(FLAG_DEBUG);
    let emit_timings = matches.is_present(FLAG_TIME);

    let link_type = if matches.is_present(FLAG_LIB) {
        LinkType::Dylib
    } else {
        LinkType::Executable
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

    let src_dir = path.parent().unwrap().canonicalize().unwrap();
    let res_binary_path = build_file(
        &arena,
        &target,
        src_dir,
        path,
        opt_level,
        emit_debug_info,
        emit_timings,
        link_type,
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

                    // No need to waste time freeing this memory,
                    // since the process is about to exit anyway.
                    std::mem::forget(arena);

                    println!(
                        "🎉 Built {} in {} ms",
                        generated_filename.to_str().unwrap(),
                        total_time.as_millis()
                    );

                    // Return a nonzero exit code if there were problems
                    Ok(outcome.status_code())
                }
                BuildAndRun { roc_file_arg_index } => {
                    let mut cmd = match target.architecture {
                        Architecture::Wasm32 => {
                            // If possible, report the generated executable name relative to the current dir.
                            let generated_filename = binary_path
                                .strip_prefix(env::current_dir().unwrap())
                                .unwrap_or(&binary_path);

                            // No need to waste time freeing this memory,
                            // since the process is about to exit anyway.
                            std::mem::forget(arena);

                            let args = std::env::args()
                                .skip(roc_file_arg_index)
                                .collect::<Vec<_>>();

                            run_with_wasmer(generated_filename, &args);
                            return Ok(0);
                        }
                        _ => Command::new(&binary_path),
                    };

                    if let Architecture::Wasm32 = target.architecture {
                        cmd.arg(binary_path);
                    }

                    // Forward all the arguments after the .roc file argument
                    // to the new process. This way, you can do things like:
                    //
                    // roc app.roc foo bar baz
                    //
                    // ...and have it so that app.roc will receive only `foo`,
                    // `bar`, and `baz` as its arguments.
                    for (index, arg) in std::env::args().enumerate() {
                        if index > roc_file_arg_index {
                            cmd.arg(arg);
                        }
                    }

                    match outcome {
                        BuildOutcome::Errors => Ok(outcome.status_code()),
                        _ => roc_run(cmd.current_dir(original_cwd)),
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

#[cfg(target_family = "unix")]
fn roc_run(cmd: &mut Command) -> io::Result<i32> {
    use std::os::unix::process::CommandExt;

    // This is much faster than spawning a subprocess if we're on a UNIX system!
    let err = cmd.exec();

    // If exec actually returned, it was definitely an error! (Otherwise,
    // this process would have been replaced by the other one, and we'd
    // never actually reach this line of code.)
    Err(err)
}

#[cfg(not(target_family = "unix"))]
fn roc_run(cmd: &mut Command) -> io::Result<i32> {
    // Run the compiled app
    let exit_status = cmd
                        .spawn()
                        .unwrap_or_else(|err| panic!("Failed to run app after building it: {:?}", err))
                        .wait()
                        .expect("TODO gracefully handle block_on failing when `roc` spawns a subprocess for the compiled app");

    // `roc [FILE]` exits with the same status code as the app it ran.
    //
    // If you want to know whether there were compilation problems
    // via status code, use either `roc build` or `roc check` instead!
    match exit_status.code() {
        Some(code) => Ok(code),
        None => {
            todo!("TODO gracefully handle the `roc [FILE]` subprocess terminating with a signal.");
        }
    }
}

fn run_with_wasmer(wasm_path: &std::path::Path, args: &[String]) {
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

enum Backend {
    Host,
    X86_32,
    X86_64,
    Wasm32,
}

impl Default for Backend {
    fn default() -> Self {
        Backend::Host
    }
}

impl Backend {
    const fn as_str(&self) -> &'static str {
        match self {
            Backend::Host => "host",
            Backend::X86_32 => "x86_32",
            Backend::X86_64 => "x86_64",
            Backend::Wasm32 => "wasm32",
        }
    }

    /// NOTE keep up to date!
    const OPTIONS: &'static [&'static str] = &[
        Backend::Host.as_str(),
        Backend::X86_32.as_str(),
        Backend::X86_64.as_str(),
        Backend::Wasm32.as_str(),
    ];

    fn to_triple(&self) -> Triple {
        let mut triple = Triple::unknown();

        match self {
            Backend::Host => Triple::host(),
            Backend::X86_32 => {
                triple.architecture = Architecture::X86_32(X86_32Architecture::I386);
                triple.binary_format = BinaryFormat::Elf;

                // TODO make this user-specified?
                triple.operating_system = OperatingSystem::Linux;

                triple
            }
            Backend::X86_64 => {
                triple.architecture = Architecture::X86_64;
                triple.binary_format = BinaryFormat::Elf;

                triple
            }
            Backend::Wasm32 => {
                triple.architecture = Architecture::Wasm32;
                triple.binary_format = BinaryFormat::Wasm;

                triple
            }
        }
    }
}

impl std::fmt::Display for Backend {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl std::str::FromStr for Backend {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "host" => Ok(Backend::Host),
            "x86_32" => Ok(Backend::X86_32),
            "x86_64" => Ok(Backend::X86_64),
            "wasm32" => Ok(Backend::Wasm32),
            _ => Err(()),
        }
    }
}
