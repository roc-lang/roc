use roc_cli::{
    build_app, docs, repl, BuildConfig, CMD_BUILD, CMD_DOCS, CMD_EDIT, CMD_REPL, CMD_RUN,
    DIRECTORY_OR_FILES, ROC_FILE,
};
use std::io;
use std::path::{Path, PathBuf};
use target_lexicon::Triple;

#[cfg(feature = "llvm")]
use roc_cli::build;

#[cfg(not(feature = "llvm"))]
fn build(_target: &Triple, _matches: &clap::ArgMatches, _config: BuildConfig) -> io::Result<i32> {
    panic!("Building without LLVM is not currently supported.");
}

fn main() -> io::Result<()> {
    let matches = build_app().get_matches();

    let exit_code = match matches.subcommand_name() {
        None => {
            launch_editor(&[])?;

            // rustc couldn't infer the error type here
            Result::<i32, io::Error>::Ok(0)
        }
        Some(CMD_BUILD) => Ok(build(
            &Triple::host(),
            matches.subcommand_matches(CMD_BUILD).unwrap(),
            BuildConfig::BuildOnly,
        )?),
        Some(CMD_RUN) => {
            let subcmd_matches = matches.subcommand_matches(CMD_RUN).unwrap();
            let roc_file_arg_index = subcmd_matches.index_of(ROC_FILE).unwrap() + 1; // Not sure why this +1 is necessary, but it is!

            Ok(build(
                &Triple::host(),
                subcmd_matches,
                BuildConfig::BuildAndRun { roc_file_arg_index },
            )?)
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
            let values = matches
                .subcommand_matches(CMD_DOCS)
                .unwrap()
                .values_of_os(DIRECTORY_OR_FILES)
                .unwrap();

            let paths = values
                .map(|os_str| Path::new(os_str).to_path_buf())
                .collect::<Vec<PathBuf>>();

            docs(paths);

            Ok(0)
        }
        _ => unreachable!(),
    }?;

    std::process::exit(exit_code);
}

#[cfg(feature = "editor")]
fn launch_editor(filepaths: &[&Path]) -> io::Result<()> {
    roc_editor::launch(filepaths)
}

#[cfg(not(feature = "editor"))]
fn launch_editor(_filepaths: &[&Path]) -> io::Result<()> {
    panic!("Cannot launch the editor because this build of roc did not include `feature = \"editor\"`!");
}
