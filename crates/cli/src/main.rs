//! The `roc` binary that brings together all functionality in the Roc toolset.
use roc_build::link::LinkType;
use roc_cli::build::check_file;
use roc_cli::{
    build_app, format, test, BuildConfig, FormatMode, Target, CMD_BUILD, CMD_CHECK, CMD_DEV,
    CMD_DOCS, CMD_EDIT, CMD_FORMAT, CMD_GEN_STUB_LIB, CMD_GLUE, CMD_REPL, CMD_RUN, CMD_TEST,
    CMD_VERSION, DIRECTORY_OR_FILES, FLAG_CHECK, FLAG_LIB, FLAG_NO_LINK, FLAG_TARGET, FLAG_TIME,
    GLUE_FILE, ROC_FILE,
};
use roc_docs::generate_docs_html;
use roc_error_macros::user_error;
use roc_load::{LoadingProblem, Threading};
use roc_packaging::cache::{self, RocCacheDir};
use std::fs::{self, FileType};
use std::io;
use std::path::{Path, PathBuf};
use target_lexicon::Triple;

#[macro_use]
extern crate const_format;

#[global_allocator]
static ALLOC: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::ffi::{OsStr, OsString};

use roc_cli::build;

fn main() -> io::Result<()> {
    let _tracing_guards = roc_tracing::setup_tracing!();

    let matches = build_app().get_matches();

    let exit_code = match matches.subcommand() {
        None => {
            if matches.is_present(ROC_FILE) {
                build(
                    &matches,
                    BuildConfig::BuildAndRunIfNoErrors,
                    Triple::host(),
                    RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                    LinkType::Executable,
                )
            } else {
                launch_editor(None)?;

                Ok(0)
            }
        }
        Some((CMD_RUN, matches)) => {
            if matches.is_present(ROC_FILE) {
                build(
                    matches,
                    BuildConfig::BuildAndRun,
                    Triple::host(),
                    RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                    LinkType::Executable,
                )
            } else {
                eprintln!("What .roc file do you want to run? Specify it at the end of the `roc run` command.");

                Ok(1)
            }
        }
        Some((CMD_TEST, matches)) => {
            if matches.is_present(ROC_FILE) {
                test(matches, Triple::host())
            } else {
                eprintln!("What .roc file do you want to test? Specify it at the end of the `roc test` command.");

                Ok(1)
            }
        }
        Some((CMD_DEV, matches)) => {
            if matches.is_present(ROC_FILE) {
                build(
                    matches,
                    BuildConfig::BuildAndRunIfNoErrors,
                    Triple::host(),
                    RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                    LinkType::Executable,
                )
            } else {
                eprintln!("What .roc file do you want to build? Specify it at the end of the `roc run` command.");

                Ok(1)
            }
        }
        Some((CMD_GLUE, matches)) => {
            let input_path = Path::new(matches.value_of_os(ROC_FILE).unwrap());
            let output_path = Path::new(matches.value_of_os(GLUE_FILE).unwrap());

            if Some("rs") == output_path.extension().and_then(OsStr::to_str) {
                roc_glue::generate(input_path, output_path)
            } else {
                eprintln!("Currently, `roc glue` only supports generating Rust glue files (with the .rs extension). In the future, the plan is to decouple `roc glue` from any particular output format, by having it accept a second .roc file which gets executed as a plugin to generate glue code for any desired language. However, this has not yet been implemented, and for now only .rs is supported.");

                Ok(1)
            }
        }
        Some((CMD_GEN_STUB_LIB, matches)) => {
            let input_path = Path::new(matches.value_of_os(ROC_FILE).unwrap());
            let target: Target = matches.value_of_t(FLAG_TARGET).unwrap_or_default();
            roc_linker::generate_stub_lib(
                input_path,
                RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                &target.to_triple(),
            )
        }
        Some((CMD_BUILD, matches)) => {
            let target: Target = matches.value_of_t(FLAG_TARGET).unwrap_or_default();
            let link_type = match (
                matches.is_present(FLAG_LIB),
                matches.is_present(FLAG_NO_LINK),
            ) {
                (true, false) => LinkType::Dylib,
                (true, true) => user_error!("build can only be one of `--lib` or `--no-link`"),
                (false, true) => LinkType::None,
                (false, false) => LinkType::Executable,
            };

            Ok(build(
                matches,
                BuildConfig::BuildOnly,
                target.to_triple(),
                RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                link_type,
            )?)
        }
        Some((CMD_CHECK, matches)) => {
            let arena = bumpalo::Bump::new();

            let emit_timings = matches.is_present(FLAG_TIME);
            let filename = matches.value_of_os(ROC_FILE).unwrap();
            let roc_file_path = PathBuf::from(filename);
            let threading = match matches
                .value_of(roc_cli::FLAG_MAX_THREADS)
                .and_then(|s| s.parse::<usize>().ok())
            {
                None => Threading::AllAvailable,
                Some(0) => user_error!("cannot build with at most 0 threads"),
                Some(1) => Threading::Single,
                Some(n) => Threading::AtMost(n),
            };

            match check_file(
                &arena,
                roc_file_path,
                emit_timings,
                RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                threading,
            ) {
                Ok((problems, total_time)) => {
                    println!(
                        "\x1B[{}m{}\x1B[39m {} and \x1B[{}m{}\x1B[39m {} found in {} ms.",
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

                    Ok(problems.exit_code())
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
        Some((CMD_REPL, _)) => Ok(roc_repl_cli::main()),
        Some((CMD_EDIT, matches)) => {
            match matches
                .values_of_os(DIRECTORY_OR_FILES)
                .map(|mut values| values.next())
            {
                Some(Some(os_str)) => {
                    launch_editor(Some(Path::new(os_str)))?;
                }
                _ => {
                    launch_editor(None)?;
                }
            }

            // Exit 0 if the editor exited normally
            Ok(0)
        }
        Some((CMD_DOCS, matches)) => {
            let maybe_values = matches.values_of_os(DIRECTORY_OR_FILES);

            let mut values: Vec<OsString> = Vec::new();

            match maybe_values {
                None => {
                    let mut os_string_values: Vec<OsString> = Vec::new();
                    read_all_roc_files(
                        &std::env::current_dir()?.as_os_str().to_os_string(),
                        &mut os_string_values,
                    )?;
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

            generate_docs_html(roc_files);

            Ok(0)
        }
        Some((CMD_FORMAT, matches)) => {
            let maybe_values = matches.values_of_os(DIRECTORY_OR_FILES);

            let mut values: Vec<OsString> = Vec::new();

            match maybe_values {
                None => {
                    let mut os_string_values: Vec<OsString> = Vec::new();
                    read_all_roc_files(
                        &std::env::current_dir()?.as_os_str().to_os_string(),
                        &mut os_string_values,
                    )?;
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

            let format_mode = match matches.is_present(FLAG_CHECK) {
                true => FormatMode::CheckOnly,
                false => FormatMode::Format,
            };

            let format_exit_code = match format(roc_files, format_mode) {
                Ok(_) => 0,
                Err(message) => {
                    eprintln!("{}", message);
                    1
                }
            };

            Ok(format_exit_code)
        }
        Some((CMD_VERSION, _)) => {
            print!(
                "{}",
                concatcp!("roc ", include_str!("../../../version.txt"))
            );

            Ok(0)
        }
        _ => unreachable!(),
    }?;

    std::process::exit(exit_code);
}

fn read_all_roc_files(
    dir: &OsString,
    roc_file_paths: &mut Vec<OsString>,
) -> Result<(), std::io::Error> {
    let entries = fs::read_dir(dir)?;

    for entry in entries {
        let path = entry?.path();

        if path.is_dir() {
            read_all_roc_files(&path.into_os_string(), roc_file_paths)?;
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
fn launch_editor(project_dir_path: Option<&Path>) -> io::Result<()> {
    roc_editor::launch(project_dir_path)
}

#[cfg(not(feature = "editor"))]
fn launch_editor(_project_dir_path: Option<&Path>) -> io::Result<()> {
    panic!("Cannot launch the editor because this build of roc did not include `feature = \"editor\"`!");
}
