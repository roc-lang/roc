#[macro_use]
extern crate clap;

use bumpalo::Bump;
use roc_collections::all::MutMap;
use roc_gen::llvm::build::OptLevel;
use roc_build::program::gen;
use roc_load::file::LoadingProblem;
use std::time::SystemTime;

use clap::{App, Arg, ArgMatches};
use std::io::{self, ErrorKind};
use std::path::{Path, PathBuf};
use std::process;
use target_lexicon::Triple;
use tokio::process::Command;
use tokio::runtime::Builder;

pub mod repl;

pub static FLAG_OPTIMIZE: &str = "optimize";
pub static FLAG_ROC_FILE: &str = "ROC_FILE";
pub static DIRECTORY_OR_FILES: &str = "DIRECTORY_OR_FILES";

pub fn build_app<'a>() -> App<'a> {
    App::new("roc")
        .version(crate_version!())
        .subcommand(App::new("build")
            .about("Build a program")
            .arg(
                Arg::with_name(FLAG_ROC_FILE)
                    .help("The .roc file to build")
                    .required(true),
            )
            .arg(
                Arg::with_name(FLAG_OPTIMIZE)
                    .long(FLAG_OPTIMIZE)
                    .help("Optimize the compiled program to run faster. (Optimization takes time to complete.)")
                    .required(false),
            )
        )
        .subcommand(App::new("run")
            .about("Build and run a program")
            .arg(
                Arg::with_name(FLAG_ROC_FILE)
                    .help("The .roc file to build and run")
                    .required(true),
            )
            .arg(
                Arg::with_name(FLAG_OPTIMIZE)
                    .long(FLAG_OPTIMIZE)
                    .help("Optimize the compiled program to run faster. (Optimization takes time to complete.)")
                    .required(false),
            )
        )
        .subcommand(App::new("repl")
            .about("Launch the interactive Read Eval Print Loop (REPL)")
        )
        .subcommand(App::new("edit")
            .about("Launch the Roc editor")
            .arg(Arg::with_name(DIRECTORY_OR_FILES)
                .index(1)
                .multiple(true)
                .required(false)
                .help("(optional) The directory or files to open on launch.")
            )
        )
}

fn main() -> io::Result<()> {
    let matches = build_app().get_matches();

    match matches.subcommand_name() {
        None => roc_editor::launch(&[]),
        Some("build") => build(matches.subcommand_matches("build").unwrap(), false),
        Some("run") => build(matches.subcommand_matches("run").unwrap(), true),
        Some("repl") => repl::main(),
        Some("edit") => {
            match matches
                .subcommand_matches("edit")
                .unwrap()
                .values_of_os(DIRECTORY_OR_FILES)
            {
                None => roc_editor::launch(&[]),
                Some(values) => {
                    let paths = values
                        .map(|os_str| Path::new(os_str))
                        .collect::<Vec<&Path>>();

                    roc_editor::launch(&paths)
                }
            }
        }
        _ => unreachable!(),
    }
}

pub fn build(matches: &ArgMatches, run_after_build: bool) -> io::Result<()> {
    let filename = matches.value_of(FLAG_ROC_FILE).unwrap();
    let opt_level = if matches.is_present(FLAG_OPTIMIZE) {
        OptLevel::Optimize
    } else {
        OptLevel::Normal
    };
    let path = Path::new(filename);
    let src_dir = path.parent().unwrap().canonicalize().unwrap();

    // Create the runtime
    let mut rt = Builder::new()
        .thread_name("roc")
        .threaded_scheduler()
        .enable_io()
        .build()
        .expect("Error spawning initial compiler thread."); // TODO make this error nicer.

    // Spawn the root task
    let path = path.canonicalize().unwrap_or_else(|err| {
        use ErrorKind::*;

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
    let binary_path = rt
        .block_on(build_file(src_dir, path, opt_level))
        .expect("TODO gracefully handle block_on failing");

    if run_after_build {
        // Run the compiled app
        rt.block_on(async {
            Command::new(binary_path)
                .spawn()
                .unwrap_or_else(|err| panic!("Failed to run app after building it: {:?}", err))
                .await
                .map_err(|_| {
                    todo!("gracefully handle error after `app` spawned");
                })
        })
        .expect("TODO gracefully handle block_on failing");
    }

    Ok(())
}

async fn build_file(
    src_dir: PathBuf,
    filename: PathBuf,
    opt_level: OptLevel,
) -> Result<PathBuf, LoadingProblem> {
    let compilation_start = SystemTime::now();
    let arena = Bump::new();

    // Step 1: compile the app and generate the .o file
    let subs_by_module = MutMap::default();

    // Release builds use uniqueness optimizations
    let stdlib = match opt_level {
        OptLevel::Normal => roc_builtins::std::standard_stdlib(),
        OptLevel::Optimize => roc_builtins::unique::uniq_stdlib(),
    };
    let loaded = roc_load::file::load(&stdlib, src_dir, filename.clone(), subs_by_module).await?;
    let dest_filename = filename.with_extension("o");

    gen(
        &arena,
        loaded,
        filename,
        Triple::host(),
        &dest_filename,
        opt_level,
    );

    let compilation_end = compilation_start.elapsed().unwrap();

    println!(
        "Finished compilation and code gen in {} ms\n",
        compilation_end.as_millis()
    );

    let cwd = dest_filename.parent().unwrap();
    let lib_path = dest_filename.with_file_name("libroc_app.a");

    // Step 2: turn the .o file into a .a static library
    Command::new("ar") // TODO on Windows, use `link`
        .args(&[
            "rcs",
            lib_path.to_str().unwrap(),
            dest_filename.to_str().unwrap(),
        ])
        .spawn()
        .map_err(|_| {
            todo!("gracefully handle `ar` failing to spawn.");
        })?
        .await
        .map_err(|_| {
            todo!("gracefully handle error after `ar` spawned");
        })?;

    // Step 3: have rustc compile the host and link in the .a file
    let binary_path = cwd.join("app");

    Command::new("rustc")
        .args(&[
            "-L",
            ".",
            "--crate-type",
            "bin",
            "host.rs",
            "-o",
            binary_path.as_path().to_str().unwrap(),
        ])
        .current_dir(cwd)
        .spawn()
        .map_err(|_| {
            todo!("gracefully handle `rustc` failing to spawn.");
        })?
        .await
        .map_err(|_| {
            todo!("gracefully handle error after `rustc` spawned");
        })?;

    Ok(binary_path)
}
