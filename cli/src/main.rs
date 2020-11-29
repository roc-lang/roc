use roc_cli::{build, build_app, repl, DIRECTORY_OR_FILES};
use std::io;
use std::path::Path;
use target_lexicon::Triple;

fn main() -> io::Result<()> {
    let matches = build_app().get_matches();

    match matches.subcommand_name() {
        None => {
            let _ = roc_editor::launch(&[]);

            Ok(())
        }
        Some("build") => build(
            &Triple::host(),
            matches.subcommand_matches("build").unwrap(),
            false,
        ),
        Some("run") => build(
            &Triple::host(),
            matches.subcommand_matches("run").unwrap(),
            true,
        ),
        Some("repl") => repl::main(),
        Some("edit") => {
            match matches
                .subcommand_matches("edit")
                .unwrap()
                .values_of_os(DIRECTORY_OR_FILES)
            {
                None => {
                    let _ = roc_editor::launch(&[]);

                    Ok(())
                }
                Some(values) => {
                    let paths = values
                        .map(|os_str| Path::new(os_str))
                        .collect::<Vec<&Path>>();

                    let _ = roc_editor::launch(&paths);

                    Ok(())
                }
            }
        }
        _ => unreachable!(),
    }
}
