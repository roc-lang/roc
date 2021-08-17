use clap::{App, AppSettings, Arg};
use std::io;

pub const CMD_PREPROCESS: &str = "preprocess";
pub const CMD_SURGERY: &str = "surgery";
pub const EXEC: &str = "EXEC";
pub const SHARED_LIB: &str = "SHARED_LIB";

pub fn build_app<'a>() -> App<'a> {
    App::new("link")
        .about("Preprocesses a platform and surgically links it to an application.")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .subcommand(
            App::new(CMD_PREPROCESS)
                .about("Preprocesses a dynamically linked platform to prepare for linking.")
                .arg(
                    Arg::with_name(EXEC)
                        .help("The dynamically link platform executable")
                        .required(true),
                )
                .arg(
                    Arg::with_name(SHARED_LIB)
                        .help("The dummy shared library representing the Roc application")
                        .required(true),
                ),
        )
        .subcommand(
            App::new(CMD_SURGERY).about("Links a preprocessed platform with a Roc application."),
        )
}

pub fn preprocess() -> io::Result<()> {
    panic!("sad");
}
