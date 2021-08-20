use clap::{App, AppSettings, Arg, ArgMatches};
use iced_x86::{Decoder, DecoderOptions, Instruction, OpKind};
use memmap2::Mmap;
use object::{
    Architecture, BinaryFormat, Object, ObjectSection, ObjectSymbol, Relocation, RelocationKind,
    RelocationTarget, Section, Symbol,
};
use roc_collections::all::MutMap;
use std::fs;
use std::io;

pub const CMD_PREPROCESS: &str = "preprocess";
pub const CMD_SURGERY: &str = "surgery";
pub const FLAG_VERBOSE: &str = "verbose";

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
                )
                .arg(
                    Arg::with_name(FLAG_VERBOSE)
                        .long(FLAG_VERBOSE)
                        .short('v')
                        .help("enable verbose printing")
                        .required(false),
                ),
        )
        .subcommand(
            App::new(CMD_SURGERY).about("Links a preprocessed platform with a Roc application."),
        )
}

pub fn preprocess(matches: &ArgMatches) -> io::Result<i32> {
    let verbose = matches.is_present(FLAG_VERBOSE);

    let app_functions = application_functions(&matches.value_of(SHARED_LIB).unwrap())?;
    if verbose {
        println!("Found app functions: {:?}", app_functions);
    }

    let exec_file = fs::File::open(&matches.value_of(EXEC).unwrap())?;
    let exec_mmap = unsafe { Mmap::map(&exec_file)? };
    let exec_obj = match object::File::parse(&*exec_mmap) {
        Ok(obj) => obj,
        Err(err) => {
            println!("Failed to parse executable file: {}", err);
            return Ok(-1);
        }
    };

    // TODO: Deal with other file formats and architectures.
    let format = exec_obj.format();
    if format != BinaryFormat::Elf {
        println!("File Format, {:?}, not supported", format);
        return Ok(-1);
    }
    let arch = exec_obj.architecture();
    if arch != Architecture::X86_64 {
        println!("Architecture, {:?}, not supported", arch);
        return Ok(-1);
    }

    // Extract PLT related information for app functions.
    let plt_address = match exec_obj.sections().find(|sec| sec.name() == Ok(".plt")) {
        Some(section) => section.address(),
        None => {
            println!("Failed to find PLT section. Probably an malformed executable.");
            return Ok(-1);
        }
    };
    if verbose {
        println!("PLT Address: {:x}", plt_address);
    }

    let plt_relocs: Vec<Relocation> = (match exec_obj.dynamic_relocations() {
        Some(relocs) => relocs,
        None => {
            println!("Executable never calls any application functions.");
            println!("No work to do. Probably an invalid input.");
            return Ok(-1);
        }
    })
    .map(|(_, reloc)| reloc)
    .filter(|reloc| reloc.kind() == RelocationKind::Elf(7))
    .collect();
    if verbose {
        println!();
        println!("PLT relocations");
        for reloc in plt_relocs.iter() {
            println!("{:x?}", reloc);
        }
    }

    let app_syms: Vec<Symbol> = exec_obj
        .dynamic_symbols()
        .filter(|sym| {
            let name = sym.name();
            name.is_ok() && app_functions.contains(&name.unwrap().to_string())
        })
        .collect();
    if verbose {
        println!();
        println!("PLT Symbols for App Functions");
        for symbol in app_syms.iter() {
            println!("{}: {:x?}", symbol.index().0, symbol);
        }
    }

    // TODO: Analyze if this offset is always correct.
    const PLT_ADDRESS_OFFSET: u64 = 0x10;

    let mut app_func_addresses: MutMap<u64, &str> = MutMap::default();
    for (i, reloc) in plt_relocs.into_iter().enumerate() {
        for symbol in app_syms.iter() {
            if reloc.target() == RelocationTarget::Symbol(symbol.index()) {
                let func_address = (i as u64 + 1) * PLT_ADDRESS_OFFSET + plt_address;
                app_func_addresses.insert(func_address, symbol.name().unwrap());
                break;
            }
        }
    }

    if verbose {
        println!();
        println!("App Function Address Map: {:x?}", app_func_addresses);
    }

    let text_sections: Vec<Section> = exec_obj
        .sections()
        .filter(|sec| {
            let name = sec.name();
            name.is_ok() && name.unwrap().starts_with(".text")
        })
        .collect();
    if text_sections.is_empty() {
        println!("No text sections found. This application has no code.");
        return Ok(-1);
    }
    if verbose {
        println!();
        println!("Text Sections");
        for sec in text_sections.iter() {
            println!("{:x?}", sec);
        }
    }

    if verbose {
        println!();
        println!("Analyzing instuctions for branches");
    }
    let mut indirect_warning_given = false;
    for sec in text_sections {
        let data = match sec.uncompressed_data() {
            Ok(data) => data,
            Err(err) => {
                println!("Failed to load text section, {:x?}: {}", sec, err);
                return Ok(-1);
            }
        };
        let mut decoder = Decoder::with_ip(64, &data, sec.address(), DecoderOptions::NONE);
        let mut inst = Instruction::default();

        while decoder.can_decode() {
            decoder.decode_out(&mut inst);

            // Note: This gets really complex fast if we want to support more than basic calls/jumps.
            // A lot of them have to load addresses into registers/memory so we would have to discover that value.
            // Would probably require some static code analysis and would be impossible in some cases.
            // As an alternative we can leave in the calls to the plt, but change the plt to jmp to the static function.
            // That way any indirect call will just have the overhead of an extra jump.
            match inst.try_op_kind(0) {
                // Relative Offsets.
                Ok(OpKind::NearBranch16 | OpKind::NearBranch32 | OpKind::NearBranch64) => {
                    let target = inst.near_branch_target();
                    if let Some(func_name) = app_func_addresses.get(&target) {
                        if verbose {
                            println!(
                                "Found branch from {:x} to {:x}({})",
                                inst.ip(),
                                target,
                                func_name
                            );
                        }
                        // TODO: Actually correctly capture the jump type and offset.
                        // We need to know exactly which bytes to surgically replace.
                    }
                }
                Ok(OpKind::FarBranch16 | OpKind::FarBranch32) => {
                    println!(
                        "Found branch type instruction that is not yet support: {:x?}",
                        inst
                    );
                    return Ok(-1);
                }
                Ok(_) => {
                    if inst.is_call_far_indirect()
                        || inst.is_call_near_indirect()
                        || inst.is_jmp_far_indirect()
                        || inst.is_jmp_near_indirect()
                    {
                        if !indirect_warning_given {
                            indirect_warning_given = true;
                            println!("Cannot analyaze through indirect jmp type instructions");
                            println!("Most likely this is not a problem, but it could mean a loss in optimizations")
                        }
                        if verbose {
                            println!(
                                "Found indirect jump type instruction at {}: {}",
                                inst.ip(),
                                inst
                            );
                        }
                    }
                }
                Err(err) => {
                    println!("Failed to decode assembly: {}", err);
                    return Ok(-1);
                }
            }
        }
    }

    // TODO: Store all this data in a nice format.

    // TODO: Potentially create a version of the executable with certain dynamic and PLT information deleted (changing offset may break stuff so be careful).
    // Remove shared library dependencies.
    // Delete extra plt entries, dynamic symbols, and dynamic relocations (might require updating other plt entries, may not worth it).
    // Add regular symbols pointing to 0 for the app functions (maybe not needed if it is just link metadata).
    // We have to be really carefull here. If we change the size or address of any section, it will mess with offsets.
    // Must likely we want to null out data. If we have to go through and update every relative offset, this will be much more complex.
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
