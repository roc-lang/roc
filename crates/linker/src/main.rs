use roc_linker::{build_app, preprocess, surgery, CMD_PREPROCESS, CMD_SURGERY};
use std::io;

fn main() -> io::Result<()> {
    let matches = build_app().get_matches();

    let exit_code = match matches.subcommand() {
        None => Ok::<i32, io::Error>(-1),
        Some((CMD_PREPROCESS, sub_matches)) => preprocess(sub_matches),
        Some((CMD_SURGERY, sub_matches)) => surgery(sub_matches),
        _ => unreachable!(),
    }?;
    std::process::exit(exit_code);
}
