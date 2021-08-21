use roc_linker::{build_app, preprocess, surgery, CMD_PREPROCESS, CMD_SURGERY};
use std::io;

fn main() -> io::Result<()> {
    let matches = build_app().get_matches();

    let exit_code = match matches.subcommand_name() {
        None => Ok::<i32, io::Error>(-1),
        Some(CMD_PREPROCESS) => {
            let sub_matches = matches.subcommand_matches(CMD_PREPROCESS).unwrap();
            preprocess(sub_matches)
        }
        Some(CMD_SURGERY) => {
            let sub_matches = matches.subcommand_matches(CMD_SURGERY).unwrap();
            surgery(sub_matches)
        }
        _ => unreachable!(),
    }?;
    std::process::exit(exit_code);
}
