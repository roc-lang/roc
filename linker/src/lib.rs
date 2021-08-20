use clap::{App, AppSettings, Arg, ArgMatches};
use memmap2::Mmap;
use object::Object;
use std::fs;
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

pub fn preprocess(matches: &ArgMatches) -> io::Result<i32> {
    let _app_functions = application_functions(&matches.value_of(SHARED_LIB).unwrap())?;

    let exec_file = fs::File::open(&matches.value_of(EXEC).unwrap())?;
    let exec_mmap = unsafe { Mmap::map(&exec_file)? };
    let exec_obj = object::File::parse(&*exec_mmap).map_err(|err| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Failed to parse executable file: {}", err),
        )
    })?;

    // TODO: Extract PLT related information for these functions.
    // TODO: For all text sections check for function calls to app functions.
    // TODO: Store all this data in a nice format.
    // TODO: Potentially create a version of the executable with certain dynamic and PLT information deleted.
    // It may be fine to just add some of this information to the metadata instead and deal with it on final exec creation.

    Ok(0)
}

fn application_functions(shared_lib_name: &str) -> io::Result<Vec<String>> {
    let shared_file = fs::File::open(&shared_lib_name)?;
    let shared_mmap = unsafe { Mmap::map(&shared_file)? };
    let shared_obj = object::File::parse(&*shared_mmap).map_err(|err| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Failed to parse shared library file: {}", err),
        )
    })?;
    shared_obj
        .exports()
        .unwrap()
        .into_iter()
        .map(|export| String::from_utf8(export.name().to_vec()))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Failed to load function names from shared library: {}", err),
            )
        })
}
