use roc_cli::build::check_file;
use roc_cli::{
    build_app, docs, repl, BuildConfig, CMD_BUILD, CMD_CHECK, CMD_DOCS, CMD_EDIT, CMD_REPL,
    CMD_RUN, DIRECTORY_OR_FILES, FLAG_TIME, ROC_FILE,
};
use roc_load::file::LoadingProblem;
use std::fs::{self, FileType};
use std::io;
use std::path::{Path, PathBuf};

#[global_allocator]
static ALLOC: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[cfg(feature = "llvm")]
use roc_cli::build;
use std::ffi::{OsStr, OsString};

#[cfg(not(feature = "llvm"))]
fn build(_target: &Triple, _matches: &clap::ArgMatches, _config: BuildConfig) -> io::Result<i32> {
    panic!("Building without LLVM is not currently supported.");
}

fn main() -> io::Result<()> {
    let matches = build_app().get_matches();

    let exit_code = match matches.subcommand_name() {
        None => {
            match matches.index_of(ROC_FILE) {
                Some(arg_index) => {
                    let roc_file_arg_index = arg_index + 1; // Not sure why this +1 is necessary, but it is!

                    build(&matches, BuildConfig::BuildAndRun { roc_file_arg_index })
                }

                None => {
                    launch_editor(&[])?;

                    Ok(0)
                }
            }
        }
        Some(CMD_BUILD) => Ok(build(
            matches.subcommand_matches(CMD_BUILD).unwrap(),
            BuildConfig::BuildOnly,
        )?),
        Some(CMD_RUN) => {
            // TODO remove CMD_RUN altogether if it is currently September 2021 or later.
            println!(
                r#"`roc run` is deprecated!
If you're using a prebuilt binary, you no longer need the `run` - just do `roc [FILE]` instead of `roc run [FILE]`.
If you're building the compiler from source you'll want to do `cargo run [FILE]` instead of `cargo run run [FILE]`.
"#
            );

            Ok(1)
        }
        Some(CMD_CHECK) => {
            let arena = bumpalo::Bump::new();

            let matches = matches.subcommand_matches(CMD_CHECK).unwrap();
            let emit_timings = matches.is_present(FLAG_TIME);
            let filename = matches.value_of(ROC_FILE).unwrap();
            let roc_file_path = PathBuf::from(filename);
            let src_dir = roc_file_path.parent().unwrap().to_owned();

            match check_file(&arena, src_dir, roc_file_path, emit_timings) {
                Ok(number_of_errors) => {
                    let exit_code = if number_of_errors != 0 { 1 } else { 0 };
                    Ok(exit_code)
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
        Some(CMD_REPL) => {
            repl::main()?;

            // Exit 0 if the repl exited normally
            Ok(0)
        }
        Some(CMD_EDIT) => {
            match matches
                .subcommand_matches(CMD_EDIT)
                .unwrap()
                .values_of_os(DIRECTORY_OR_FILES)
            {
                None => {
                    launch_editor(&[])?;
                }
                Some(values) => {
                    let paths = values
                        .map(|os_str| Path::new(os_str))
                        .collect::<Vec<&Path>>();

                    launch_editor(&paths)?;
                }
            }

            // Exit 0 if the editor exited normally
            Ok(0)
        }
        Some(CMD_DOCS) => {
            let maybe_values = matches
                .subcommand_matches(CMD_DOCS)
                .unwrap()
                .values_of_os(DIRECTORY_OR_FILES);

            let mut values: Vec<OsString> = Vec::new();

            match maybe_values {
                None => {
                    let mut os_string_values: Vec<OsString> = Vec::new();
                    read_all_roc_files(&OsStr::new("./").to_os_string(), &mut os_string_values)?;
                    for os_string in os_string_values {
                        values.push(os_string);
                    }
                }
                Some(os_values) => {
                    for os_str in os_values {
                        values.push(os_str.to_os_string());
                    }
                }
            }

            let mut roc_files = Vec::new();

            // Populate roc_files
            for os_str in values {
                let metadata = fs::metadata(os_str.clone())?;
                roc_files_recursive(os_str.as_os_str(), metadata.file_type(), &mut roc_files)?;
            }

            docs(roc_files);

            Ok(0)
        }
        _ => unreachable!(),
    }?;

    std::process::exit(exit_code);
}

fn read_all_roc_files(
    dir: &OsString,
    mut roc_file_paths: &mut Vec<OsString>,
) -> Result<(), std::io::Error> {
    let entries = fs::read_dir(dir)?;

    for entry in entries {
        let path = entry?.path();

        if path.is_dir() {
            read_all_roc_files(&path.into_os_string(), &mut roc_file_paths)?;
        } else if path.extension().and_then(OsStr::to_str) == Some("roc") {
            let file_path = path.into_os_string();
            roc_file_paths.push(file_path);
        }
    }

    Ok(())
}

fn roc_files_recursive<P: AsRef<Path>>(
    path: P,
    file_type: FileType,
    roc_files: &mut Vec<PathBuf>,
) -> io::Result<()> {
    if file_type.is_dir() {
        for entry_res in fs::read_dir(path)? {
            let entry = entry_res?;

            roc_files_recursive(entry.path(), entry.file_type()?, roc_files)?;
        }
    } else {
        roc_files.push(path.as_ref().to_path_buf());
    }

    Ok(())
}

#[cfg(feature = "editor")]
fn launch_editor(filepaths: &[&Path]) -> io::Result<()> {
    roc_editor::launch(filepaths)
}

#[cfg(not(feature = "editor"))]
fn launch_editor(_filepaths: &[&Path]) -> io::Result<()> {
    panic!("Cannot launch the editor because this build of roc did not include `feature = \"editor\"`!");
}
