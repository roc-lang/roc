//! The `roc` binary that brings together all functionality in the Roc toolset.
use bumpalo::Bump;
use roc_build::link::LinkType;
use roc_build::program::{check_file, CodeGenBackend};
use roc_cli::{
    build_app, format_files, format_src, test, BuildConfig, FormatMode, CMD_BUILD, CMD_CHECK,
    CMD_DEV, CMD_DOCS, CMD_FORMAT, CMD_GEN_STUB_LIB, CMD_GLUE, CMD_PREPROCESS_HOST, CMD_REPL,
    CMD_RUN, CMD_TEST, CMD_VERSION, DIRECTORY_OR_FILES, FLAG_CHECK, FLAG_DEV, FLAG_LIB,
    FLAG_NO_LINK, FLAG_OUTPUT, FLAG_STDIN, FLAG_STDOUT, FLAG_TARGET, FLAG_TIME, GLUE_DIR,
    GLUE_SPEC, ROC_FILE,
};
use roc_docs::generate_docs_html;
use roc_error_macros::user_error;
use roc_gen_dev::AssemblyBackendMode;
use roc_gen_llvm::llvm::build::LlvmBackendMode;
use roc_load::{FunctionKind, LoadingProblem, Threading};
use roc_packaging::cache::{self, RocCacheDir};
use roc_target::Target;
use std::fs::{self, FileType};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use target_lexicon::Triple;

#[macro_use]
extern crate const_format;

#[global_allocator]
static ALLOC: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::ffi::{OsStr, OsString};

use roc_cli::build;

fn main() -> io::Result<()> {
    let _tracing_guards = roc_tracing::setup_tracing!();

    let app = build_app();
    let subcommands: Vec<String> = app
        .get_subcommands()
        .map(|c| c.get_name().to_owned())
        .collect();
    let matches = app.get_matches();

    let exit_code = match matches.subcommand() {
        None => {
            if matches.contains_id(ROC_FILE) {
                build(
                    &matches,
                    &subcommands,
                    BuildConfig::BuildAndRunIfNoErrors,
                    Triple::host().into(),
                    None,
                    RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                    LinkType::Executable,
                )
            } else {
                Ok(1)
            }
        }
        Some((CMD_RUN, matches)) => {
            if matches.contains_id(ROC_FILE) {
                build(
                    matches,
                    &subcommands,
                    BuildConfig::BuildAndRun,
                    Triple::host().into(),
                    None,
                    RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                    LinkType::Executable,
                )
            } else {
                eprintln!("What .roc file do you want to run? Specify it at the end of the `roc run` command.");

                Ok(1)
            }
        }
        Some((CMD_TEST, matches)) => {
            if matches.contains_id(ROC_FILE) {
                test(matches, Triple::host().into())
            } else {
                eprintln!("What .roc file do you want to test? Specify it at the end of the `roc test` command.");

                Ok(1)
            }
        }
        Some((CMD_DEV, matches)) => {
            if matches.contains_id(ROC_FILE) {
                build(
                    matches,
                    &subcommands,
                    BuildConfig::BuildAndRunIfNoErrors,
                    Triple::host().into(),
                    None,
                    RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                    LinkType::Executable,
                )
            } else {
                eprintln!("What .roc file do you want to build? Specify it at the end of the `roc run` command.");

                Ok(1)
            }
        }
        Some((CMD_GLUE, matches)) => {
            let input_path = matches.get_one::<PathBuf>(ROC_FILE).unwrap();
            let output_path = matches.get_one::<PathBuf>(GLUE_DIR).unwrap();
            let spec_path = matches.get_one::<PathBuf>(GLUE_SPEC).unwrap();

            // have the backend supply `roc_alloc` and friends
            let backend = match matches.get_flag(FLAG_DEV) {
                true => CodeGenBackend::Assembly(AssemblyBackendMode::Test),
                false => CodeGenBackend::Llvm(LlvmBackendMode::BinaryGlue),
            };

            if !output_path.exists() || output_path.is_dir() {
                roc_glue::generate(input_path, output_path, spec_path, backend)
            } else {
                eprintln!("`roc glue` must be given a directory to output into, because the glue might generate multiple files.");

                Ok(1)
            }
        }
        Some((CMD_GEN_STUB_LIB, matches)) => {
            let input_path = matches.get_one::<PathBuf>(ROC_FILE).unwrap();
            let target = matches
                .get_one::<String>(FLAG_TARGET)
                .and_then(|s| Target::from_str(s).ok())
                .unwrap_or_default();
            let function_kind = FunctionKind::LambdaSet;
            roc_linker::generate_stub_lib(
                input_path,
                RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                target,
                function_kind,
            );
            Ok(0)
        }
        Some((CMD_PREPROCESS_HOST, matches)) => {
            let input_path = matches.get_one::<PathBuf>(ROC_FILE).unwrap();
            let target = matches
                .get_one::<String>(FLAG_TARGET)
                .and_then(|s| Target::from_str(s).ok())
                .unwrap_or_default();

            let function_kind = FunctionKind::LambdaSet;
            let (platform_path, stub_lib, stub_dll_symbols) = roc_linker::generate_stub_lib(
                input_path,
                RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                target,
                function_kind,
            );

            // TODO: pipeline the executable location through here.
            // Currently it is essentally hardcoded as platform_path/dynhost.
            roc_linker::preprocess_host(
                target,
                &platform_path.with_file_name("main.roc"),
                // The target triple string must be derived from the triple to convert from the generic
                // `system` target to the exact specific target.
                &platform_path.with_file_name(format!("{}.rh", target)),
                &stub_lib,
                &stub_dll_symbols,
            );
            Ok(0)
        }
        Some((CMD_BUILD, matches)) => {
            let target = matches
                .get_one::<String>(FLAG_TARGET)
                .and_then(|s| Target::from_str(s).ok())
                .unwrap_or_default();
            let link_type = match (matches.get_flag(FLAG_LIB), matches.get_flag(FLAG_NO_LINK)) {
                (true, false) => LinkType::Dylib,
                (true, true) => user_error!("build can only be one of `--lib` or `--no-link`"),
                (false, true) => LinkType::None,
                (false, false) => LinkType::Executable,
            };
            let out_path = matches
                .get_one::<OsString>(FLAG_OUTPUT)
                .map(OsString::as_ref);

            Ok(build(
                matches,
                &subcommands,
                BuildConfig::BuildOnly,
                target,
                out_path,
                RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                link_type,
            )?)
        }
        Some((CMD_CHECK, matches)) => {
            let arena = Bump::new();

            let emit_timings = matches.get_flag(FLAG_TIME);
            let roc_file_path = matches.get_one::<PathBuf>(ROC_FILE).unwrap();
            let threading = match matches.get_one::<usize>(roc_cli::FLAG_MAX_THREADS) {
                None => Threading::AllAvailable,
                Some(0) => user_error!("cannot build with at most 0 threads"),
                Some(1) => Threading::Single,
                Some(n) => Threading::AtMost(*n),
            };

            match check_file(
                &arena,
                roc_file_path.to_owned(),
                emit_timings,
                RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                threading,
            ) {
                Ok((problems, total_time)) => {
                    problems.print_error_warning_count(total_time);
                    Ok(problems.exit_code())
                }

                Err(LoadingProblem::FormattedReport(report)) => {
                    print!("{report}");

                    Ok(1)
                }
                Err(other) => {
                    panic!("build_file failed with error:\n{other:?}");
                }
            }
        }
        Some((CMD_REPL, _)) => Ok(roc_repl_cli::main()),
        Some((CMD_DOCS, matches)) => {
            let root_path = matches.get_one::<PathBuf>(ROC_FILE).unwrap();
            let out_dir = matches.get_one::<OsString>(FLAG_OUTPUT).unwrap();

            generate_docs_html(root_path.to_owned(), out_dir.as_ref());

            Ok(0)
        }
        Some((CMD_FORMAT, matches)) => {
            let from_stdin = matches.get_flag(FLAG_STDIN);
            let to_stdout = matches.get_flag(FLAG_STDOUT);
            let format_mode = if to_stdout {
                FormatMode::WriteToStdout
            } else {
                match matches.get_flag(FLAG_CHECK) {
                    true => FormatMode::CheckOnly,
                    false => FormatMode::WriteToFile,
                }
            };

            if from_stdin && matches!(format_mode, FormatMode::WriteToFile) {
                eprintln!("When using the --stdin flag, either the --check or the --stdout flag must also be specified. (Otherwise, it's unclear what filename to write to!)");
                std::process::exit(1);
            }

            let roc_files = {
                let mut roc_files = Vec::new();

                let mut values: Vec<OsString> = Vec::new();

                match matches.get_many::<OsString>(DIRECTORY_OR_FILES) {
                    Some(os_values) => {
                        for os_string in os_values {
                            values.push(os_string.to_owned());
                        }
                    }
                    None if from_stdin || to_stdout => {}
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
                }

                // Populate roc_files
                for os_str in values {
                    let metadata = fs::metadata(os_str.clone())?;
                    roc_files_recursive(os_str.as_os_str(), metadata.file_type(), &mut roc_files)?;
                }

                roc_files
            };

            let format_exit_code = if from_stdin {
                let mut buf = Vec::new();
                let arena = Bump::new();

                io::stdin().read_to_end(&mut buf)?;

                let src = std::str::from_utf8(&buf).unwrap_or_else(|err| {
                    eprintln!("Stdin contained invalid UTF-8 bytes: {err:?}");
                    std::process::exit(1);
                });

                match format_src(&arena, src) {
                    Ok(formatted_src) => {
                        match format_mode {
                            FormatMode::CheckOnly => {
                                if src == formatted_src {
                                    eprintln!("One or more files need to be reformatted.");
                                    1
                                } else {
                                    0
                                }
                            }
                            FormatMode::WriteToStdout => {
                                std::io::stdout()
                                    .lock()
                                    .write_all(formatted_src.as_bytes())
                                    .unwrap();

                                0
                            }
                            FormatMode::WriteToFile => {
                                // We would have errored out already if you specified --stdin
                                // without either --stdout or --check specified as well.
                                unreachable!()
                            }
                        }
                    }
                    Err(problem) => {
                        eprintln!("`roc format` failed: {problem:?}");
                        1
                    }
                }
            } else {
                match format_files(roc_files, format_mode) {
                    Ok(()) => 0,
                    Err(message) => {
                        eprintln!("{message}");
                        1
                    }
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
