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
    // The information need is really the address of each plt version of each application function.
    // To find this, first get the dynmaic symbols for the app functions.
    // Then reference them on the dynamic relocation table to figure out their plt function number.
    // Then with the plt base address and that function number(or scanning the code), it should be possible to find the address.

    // TODO: For all text sections check for function calls to app functions.
    // This should just be disassembly and then scanning for jmp and call style ops that jump to the plt offsets we care about.
    // The data well be store in a list for each function name.
    // Not really sure if/how namespacing will lead to conflicts (i.e. naming an app function printf when c alread has printf).

    // TODO: Store all this data in a nice format.

    // TODO: Potentially create a version of the executable with certain dynamic and PLT information deleted.
    // Remove shared library dependencies.
    // Delete extra plt entries, dynamic symbols, and dynamic relocations (might require updating other plt entries, may not worth it).
    // Add regular symbols pointing to 0 for the app functions (maybe not needed if it is just link metadata).
    // It may be fine to just add some of this information to the metadata instead and deal with it on final exec creation.
    // If we are copying the exec to a new location in the background anyway it may be basically free.

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
