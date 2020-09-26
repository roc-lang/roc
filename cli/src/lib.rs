#[macro_use]
extern crate clap;

use bumpalo::Bump;
use clap::ArgMatches;
use clap::{App, Arg};
use roc_build::program::gen;
use roc_collections::all::MutMap;
use roc_gen::llvm::build::OptLevel;
use roc_load::file::LoadingProblem;
use std::io::{self, ErrorKind};
use std::path::{Path, PathBuf};
use std::process;
use std::process::Command;
use std::time::{Duration, SystemTime};
use target_lexicon::Triple;

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

pub fn build(matches: &ArgMatches, run_after_build: bool) -> io::Result<()> {
    let filename = matches.value_of(FLAG_ROC_FILE).unwrap();
    let opt_level = if matches.is_present(FLAG_OPTIMIZE) {
        OptLevel::Optimize
    } else {
        OptLevel::Normal
    };
    let path = Path::new(filename).canonicalize().unwrap();
    let src_dir = path.parent().unwrap().canonicalize().unwrap();

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

    let binary_path =
        build_file(src_dir, path, opt_level).expect("TODO gracefully handle build_file failing");

    if run_after_build {
        // Run the compiled app
        Command::new(binary_path)
            .spawn()
            .unwrap_or_else(|err| panic!("Failed to run app after building it: {:?}", err))
            .wait()
            .expect("TODO gracefully handle block_on failing");
    }

    Ok(())
}

fn report_timing(buf: &mut String, label: &str, duration: Duration) {
    buf.push_str(&format!(
        "        {:.3} ms   {}\n",
        duration.as_secs_f64() * 1000.0,
        label,
    ));
}

fn build_file(
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
    let loaded =
        roc_load::file::load(filename.clone(), &stdlib, src_dir.as_path(), subs_by_module)?;
    let dest_filename = filename.with_extension("o");

    let buf = &mut String::with_capacity(1024);

    for (module_id, module_timing) in loaded.timings.iter() {
        let module_name = loaded.interns.module_name(*module_id);

        buf.push_str("    ");
        buf.push_str(module_name);
        buf.push_str("\n");

        report_timing(buf, "Read .roc file from disk", module_timing.read_roc_file);
        report_timing(buf, "Parse header", module_timing.parse_header);
        report_timing(buf, "Parse body", module_timing.parse_body);
        report_timing(buf, "Canonicalize", module_timing.canonicalize);
        report_timing(buf, "Constrain", module_timing.constrain);
        report_timing(buf, "Solve", module_timing.solve);
        report_timing(buf, "Other", module_timing.other());
        buf.push('\n');
        report_timing(buf, "Total", module_timing.total());
    }

    println!(
        "\n\nCompilation finished! Here's how long each module took to compile:\n\n{}",
        buf
    );

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
        .wait()
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
            // ensure we don't make a position-independent executable
            "-C",
            "link-arg=-no-pie",
            // explicitly link in the c++ stdlib, for exceptions
            "-C",
            "link-arg=-lc++",
        ])
        .current_dir(cwd)
        .spawn()
        .map_err(|_| {
            todo!("gracefully handle `rustc` failing to spawn.");
        })?
        .wait()
        .map_err(|_| {
            todo!("gracefully handle error after `rustc` spawned");
        })?;

    Ok(binary_path)
}
