use bincode::{deserialize_from, serialize_into};
use clap::{App, AppSettings, Arg, ArgMatches};
use iced_x86::{Decoder, DecoderOptions, Instruction, OpCodeOperandKind, OpKind};
use memmap2::{Mmap, MmapMut};
use object::{elf, endian};
use object::{
    Architecture, BinaryFormat, CompressedFileRange, CompressionFormat, LittleEndian, NativeEndian,
    Object, ObjectSection, ObjectSymbol, Relocation, RelocationKind, RelocationTarget, Section,
    Symbol, SymbolSection,
};
use roc_collections::all::MutMap;
use std::convert::TryFrom;
use std::ffi::CStr;
use std::fs;
use std::io;
use std::io::{BufReader, BufWriter};
use std::mem;
use std::os::raw::c_char;
use std::path::Path;
use std::time::{Duration, SystemTime};

mod metadata;

pub const CMD_PREPROCESS: &str = "preprocess";
pub const CMD_SURGERY: &str = "surgery";
pub const FLAG_VERBOSE: &str = "verbose";

pub const EXEC: &str = "EXEC";
pub const METADATA: &str = "METADATA";
pub const SHARED_LIB: &str = "SHARED_LIB";
pub const APP: &str = "APP";
pub const OUT: &str = "OUT";

// TODO: Analyze if this offset is always correct.
const PLT_ADDRESS_OFFSET: u64 = 0x10;

fn report_timing(label: &str, duration: Duration) {
    &println!("\t{:9.3} ms   {}", duration.as_secs_f64() * 1000.0, label,);
}

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
                    Arg::with_name(METADATA)
                        .help("Where to save the metadata from preprocessing")
                        .required(true),
                )
                .arg(
                    Arg::with_name(FLAG_VERBOSE)
                        .long(FLAG_VERBOSE)
                        .short('v')
                        .help("Enable verbose printing")
                        .required(false),
                ),
        )
        .subcommand(
            App::new(CMD_SURGERY)
                .about("Links a preprocessed platform with a Roc application.")
                .arg(
                    Arg::with_name(EXEC)
                        .help("The dynamically link platform executable")
                        .required(true),
                )
                .arg(
                    Arg::with_name(METADATA)
                        .help("The metadata created by preprocessing the platform")
                        .required(true),
                )
                .arg(
                    Arg::with_name(APP)
                        .help("The Roc application object file waiting to be linked")
                        .required(true),
                )
                .arg(Arg::with_name(OUT).help("The output file").required(true))
                .arg(
                    Arg::with_name(FLAG_VERBOSE)
                        .long(FLAG_VERBOSE)
                        .short('v')
                        .help("Enable verbose printing")
                        .required(false),
                ),
        )
}

pub fn preprocess(matches: &ArgMatches) -> io::Result<i32> {
    let verbose = matches.is_present(FLAG_VERBOSE);

    let total_start = SystemTime::now();
    let shared_lib_processing_start = SystemTime::now();
    let app_functions = application_functions(&matches.value_of(SHARED_LIB).unwrap())?;
    if verbose {
        println!("Found app functions: {:?}", app_functions);
    }
    let shared_lib_processing_duration = shared_lib_processing_start.elapsed().unwrap();

    let exec_parsing_start = SystemTime::now();
    let exec_file = fs::File::open(&matches.value_of(EXEC).unwrap())?;
    let exec_mmap = unsafe { Mmap::map(&exec_file)? };
    let file_data = &*exec_mmap;
    let exec_obj = match object::File::parse(file_data) {
        Ok(obj) => obj,
        Err(err) => {
            println!("Failed to parse executable file: {}", err);
            return Ok(-1);
        }
    };
    let exec_parsing_duration = exec_parsing_start.elapsed().unwrap();

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

    let mut md: metadata::Metadata = Default::default();

    // Extract PLT related information for app functions.
    let symbol_and_plt_processing_start = SystemTime::now();
    let (plt_address, plt_offset) = match exec_obj.section_by_name(".plt") {
        Some(section) => {
            let file_offset = match section.compressed_file_range() {
                Ok(
                    range
                    @
                    CompressedFileRange {
                        format: CompressionFormat::None,
                        ..
                    },
                ) => range.offset,
                _ => {
                    println!("Surgical linking does not work with compressed plt section");
                    return Ok(-1);
                }
            };
            (section.address(), file_offset)
        }
        None => {
            println!("Failed to find PLT section. Probably an malformed executable.");
            return Ok(-1);
        }
    };
    if verbose {
        println!("PLT Address: 0x{:x}", plt_address);
        println!("PLT File Offset: 0x{:x}", plt_offset);
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
            // Note: We are scrapping version information like '@GLIBC_2.2.5'
            // We probably never need to remedy this due to the focus on Roc only.
            name.is_ok()
                && app_functions.contains(&name.unwrap().split('@').next().unwrap().to_string())
        })
        .collect();
    for sym in app_syms.iter() {
        let name = sym.name().unwrap().to_string();
        md.app_functions.push(name.clone());
        md.surgeries.insert(name.clone(), vec![]);
        md.dynamic_symbol_indices.insert(name, sym.index().0 as u64);
    }
    if verbose {
        println!();
        println!("PLT Symbols for App Functions");
        for symbol in app_syms.iter() {
            println!("{}: {:x?}", symbol.index().0, symbol);
        }
    }

    let mut app_func_addresses: MutMap<u64, &str> = MutMap::default();
    for (i, reloc) in plt_relocs.into_iter().enumerate() {
        for symbol in app_syms.iter() {
            if reloc.target() == RelocationTarget::Symbol(symbol.index()) {
                let func_address = (i as u64 + 1) * PLT_ADDRESS_OFFSET + plt_address;
                let func_offset = (i as u64 + 1) * PLT_ADDRESS_OFFSET + plt_offset;
                app_func_addresses.insert(func_address, symbol.name().unwrap());
                md.plt_addresses.insert(
                    symbol.name().unwrap().to_string(),
                    (func_offset, func_address),
                );
                break;
            }
        }
    }

    if verbose {
        println!();
        println!("App Function Address Map: {:x?}", app_func_addresses);
    }
    let symbol_and_plt_processing_duration = symbol_and_plt_processing_start.elapsed().unwrap();

    let text_disassembly_start = SystemTime::now();
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
        let (file_offset, compressed) = match sec.compressed_file_range() {
            Ok(
                range
                @
                CompressedFileRange {
                    format: CompressionFormat::None,
                    ..
                },
            ) => (range.offset, false),
            Ok(range) => (range.offset, true),
            Err(err) => {
                println!(
                    "Issues dealing with section compression for {:x?}: {}",
                    sec, err
                );
                return Ok(-1);
            }
        };

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
                        if compressed {
                            println!("Surgical linking does not work with compressed text sections: {:x?}", sec);
                            return Ok(-1);
                        }

                        if verbose {
                            println!(
                                "Found branch from 0x{:x} to 0x{:x}({})",
                                inst.ip(),
                                target,
                                func_name
                            );
                        }

                        // TODO: Double check these offsets are always correct.
                        // We may need to do a custom offset based on opcode instead.
                        let op_kind = inst.op_code().try_op_kind(0).unwrap();
                        let op_size: u8 = match op_kind {
                            OpCodeOperandKind::br16_1 | OpCodeOperandKind::br32_1 => 1,
                            OpCodeOperandKind::br16_2 => 2,
                            OpCodeOperandKind::br32_4 | OpCodeOperandKind::br64_4 => 4,
                            _ => {
                                println!(
                                    "Ran into an unknown operand kind when analyzing branches: {:?}",
                                    op_kind
                                );
                                return Ok(-1);
                            }
                        };
                        let offset = inst.next_ip() - op_size as u64 - sec.address() + file_offset;
                        if verbose {
                            println!(
                                "\tNeed to surgically replace {} bytes at file offset 0x{:x}",
                                op_size, offset,
                            );
                            println!(
                                "\tIts current value is {:x?}",
                                &file_data[offset as usize..(offset + op_size as u64) as usize]
                            )
                        }
                        md.surgeries
                            .get_mut(*func_name)
                            .unwrap()
                            .push(metadata::SurgeryEntry {
                                file_offset: offset,
                                virtual_offset: inst.next_ip(),
                                size: op_size,
                            });
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
                            println!();
                            println!("Cannot analyaze through indirect jmp type instructions");
                            println!("Most likely this is not a problem, but it could mean a loss in optimizations");
                            println!();
                        }
                        // if verbose {
                        //     println!(
                        //         "Found indirect jump type instruction at {}: {}",
                        //         inst.ip(),
                        //         inst
                        //     );
                        // }
                    }
                }
                Err(err) => {
                    println!("Failed to decode assembly: {}", err);
                    return Ok(-1);
                }
            }
        }
    }
    let text_disassembly_duration = text_disassembly_start.elapsed().unwrap();

    let scanning_dynamic_deps_start = SystemTime::now();

    let dyn_sec = match exec_obj.section_by_name(".dynamic") {
        Some(sec) => sec,
        None => {
            println!("There must be a dynamic section in the executable");
            return Ok(-1);
        }
    };
    let dyn_offset = match dyn_sec.compressed_file_range() {
        Ok(
            range
            @
            CompressedFileRange {
                format: CompressionFormat::None,
                ..
            },
        ) => range.offset as usize,
        _ => {
            println!("Surgical linking does not work with compressed dynamic section");
            return Ok(-1);
        }
    };
    md.dynamic_section_offset = Some(dyn_offset as u64);

    let dynstr_sec = match exec_obj.section_by_name(".dynstr") {
        Some(sec) => sec,
        None => {
            println!("There must be a dynstr section in the executable");
            return Ok(-1);
        }
    };
    let dynstr_data = match dynstr_sec.uncompressed_data() {
        Ok(data) => data,
        Err(err) => {
            println!("Failed to load dynstr section: {}", err);
            return Ok(-1);
        }
    };

    let shared_lib_name = Path::new(matches.value_of(SHARED_LIB).unwrap())
        .file_name()
        .unwrap()
        .to_str()
        .unwrap();

    let mut dyn_lib_index = 0;
    loop {
        let dyn_tag = u64::from_le_bytes(
            <[u8; 8]>::try_from(
                &file_data[dyn_offset + dyn_lib_index * 16..dyn_offset + dyn_lib_index * 16 + 8],
            )
            .unwrap(),
        );
        if dyn_tag == 0 {
            break;
        } else if dyn_tag == 1 {
            let dynstr_off = u64::from_le_bytes(
                <[u8; 8]>::try_from(
                    &file_data
                        [dyn_offset + dyn_lib_index * 16 + 8..dyn_offset + dyn_lib_index * 16 + 16],
                )
                .unwrap(),
            ) as usize;
            let c_buf: *const c_char = dynstr_data[dynstr_off..].as_ptr() as *const i8;
            let c_str = unsafe { CStr::from_ptr(c_buf) }.to_str().unwrap();
            if verbose {
                println!("Found shared lib with name: {}", c_str);
            }
            if c_str == shared_lib_name {
                md.shared_lib_index = Some(dyn_lib_index as u64);
                if verbose {
                    println!(
                        "Found shared lib in dynamic table at index: {}",
                        dyn_lib_index
                    );
                }
            }
        }

        dyn_lib_index += 1;
    }
    md.dynamic_lib_count = Some(dyn_lib_index as u64);

    if md.shared_lib_index.is_none() {
        println!("Shared lib not found as a dependency of the executable");
        return Ok(-1);
    }

    let scanning_dynamic_deps_duration = scanning_dynamic_deps_start.elapsed().unwrap();

    let symtab_sec = match exec_obj.section_by_name(".symtab") {
        Some(sec) => sec,
        None => {
            println!("There must be a symtab section in the executable");
            return Ok(-1);
        }
    };
    let symtab_offset = match symtab_sec.compressed_file_range() {
        Ok(
            range
            @
            CompressedFileRange {
                format: CompressionFormat::None,
                ..
            },
        ) => range.offset as usize,
        _ => {
            println!("Surgical linking does not work with compressed symtab section");
            return Ok(-1);
        }
    };
    md.symbol_table_section_offset = Some(symtab_offset as u64);
    md.symbol_table_size = Some(symtab_sec.size());

    let dynsym_sec = match exec_obj.section_by_name(".dynsym") {
        Some(sec) => sec,
        None => {
            println!("There must be a dynsym section in the executable");
            return Ok(-1);
        }
    };
    let dynsym_offset = match dynsym_sec.compressed_file_range() {
        Ok(
            range
            @
            CompressedFileRange {
                format: CompressionFormat::None,
                ..
            },
        ) => range.offset as usize,
        _ => {
            println!("Surgical linking does not work with compressed dynsym section");
            return Ok(-1);
        }
    };
    md.dynamic_symbol_table_section_offset = Some(dynsym_offset as u64);

    if verbose {
        println!();
        println!("{:?}", md);
    }

    let saving_metadata_start = SystemTime::now();
    let output = fs::File::create(&matches.value_of(METADATA).unwrap())?;
    let output = BufWriter::new(output);
    if let Err(err) = serialize_into(output, &md) {
        println!("Failed to serialize metadata: {}", err);
        return Ok(-1);
    };
    let saving_metadata_duration = saving_metadata_start.elapsed().unwrap();

    let total_duration = total_start.elapsed().unwrap();

    // TODO: Potentially create a version of the executable with certain dynamic information deleted (changing offset may break stuff so be careful).
    // Remove shared library dependencies.
    // Also modify the PLT entries such that they just are jumps to the app functions. They will be used for indirect calls.
    // Add regular symbols pointing to 0 for the app functions (maybe not needed if it is just link metadata).
    // We have to be really carefull here. If we change the size or address of any section, it will mess with offsets.
    // Must likely we want to null out data. If we have to go through and update every relative offset, this will be much more complex.
    // Potentially we can take advantage of virtual address to avoid actually needing to shift any offsets.
    // It may be fine to just add some of this information to the metadata instead and deal with it on final exec creation.
    // If we are copying the exec to a new location in the background anyway it may be basically free.

    if verbose {
        println!();
        println!("Timings");
        report_timing("Shared Library Processing", shared_lib_processing_duration);
        report_timing("Executable Parsing", exec_parsing_duration);
        report_timing(
            "Symbol and PLT Processing",
            symbol_and_plt_processing_duration,
        );
        report_timing("Text Disassembly", text_disassembly_duration);
        report_timing("Scanning Dynamic Deps", scanning_dynamic_deps_duration);
        report_timing("Saving Metadata", saving_metadata_duration);
        report_timing(
            "Other",
            total_duration
                - shared_lib_processing_duration
                - exec_parsing_duration
                - symbol_and_plt_processing_duration
                - text_disassembly_duration
                - scanning_dynamic_deps_duration
                - saving_metadata_duration,
        );
        report_timing("Total", total_duration);
    }

    Ok(0)
}

pub fn surgery(matches: &ArgMatches) -> io::Result<i32> {
    let verbose = matches.is_present(FLAG_VERBOSE);

    let total_start = SystemTime::now();
    let loading_metadata_start = SystemTime::now();
    let input = fs::File::open(&matches.value_of(METADATA).unwrap())?;
    let input = BufReader::new(input);
    let md: metadata::Metadata = match deserialize_from(input) {
        Ok(data) => data,
        Err(err) => {
            println!("Failed to deserialize metadata: {}", err);
            return Ok(-1);
        }
    };
    let loading_metadata_duration = loading_metadata_start.elapsed().unwrap();

    let exec_parsing_start = SystemTime::now();
    let exec_file = fs::File::open(&matches.value_of(EXEC).unwrap())?;
    let exec_mmap = unsafe { Mmap::map(&exec_file)? };
    let exec_data = &*exec_mmap;
    let elf64 = exec_data[4] == 2;
    let litte_endian = exec_data[5] == 1;
    if !elf64 || !litte_endian {
        println!("Only 64bit little endian elf currently supported for surgery");
        return Ok(-1);
    }
    let exec_header = load_struct_inplace::<elf::FileHeader64<LittleEndian>>(exec_data, 0);

    let ph_offset = exec_header.e_phoff.get(NativeEndian);
    let ph_ent_size = exec_header.e_phentsize.get(NativeEndian);
    let ph_num = exec_header.e_phnum.get(NativeEndian);
    let sh_offset = exec_header.e_shoff.get(NativeEndian);
    let sh_ent_size = exec_header.e_shentsize.get(NativeEndian);
    let sh_num = exec_header.e_shnum.get(NativeEndian);
    if verbose {
        println!();
        println!("Is Elf64: {}", elf64);
        println!("Is Little Endian: {}", litte_endian);
        println!("PH Offset: 0x{:x}", ph_offset);
        println!("PH Entry Size: {}", ph_ent_size);
        println!("PH Entry Count: {}", ph_num);
        println!("SH Offset: 0x{:x}", sh_offset);
        println!("SH Entry Size: {}", sh_ent_size);
        println!("SH Entry Count: {}", sh_num);
    }
    let exec_parsing_duration = exec_parsing_start.elapsed().unwrap();

    let app_parsing_start = SystemTime::now();
    let app_file = fs::File::open(&matches.value_of(APP).unwrap())?;
    let app_mmap = unsafe { Mmap::map(&app_file)? };
    let app_data = &*app_mmap;
    let app_obj = match object::File::parse(app_data) {
        Ok(obj) => obj,
        Err(err) => {
            println!("Failed to parse application file: {}", err);
            return Ok(-1);
        }
    };
    let app_parsing_duration = app_parsing_start.elapsed().unwrap();

    let platform_gen_start = SystemTime::now();
    let max_out_len = exec_data.len() + app_data.len() + 4096;
    let out_file = fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open(&matches.value_of(OUT).unwrap())?;
    out_file.set_len(max_out_len as u64)?;
    let mut out_mmap = unsafe { MmapMut::map_mut(&out_file)? };

    // Write a modified elf header with an extra program header entry.
    let added_data = ph_ent_size as u64;
    let ph_end = ph_offset as usize + ph_num as usize * ph_ent_size as usize;
    out_mmap[..ph_end].copy_from_slice(&exec_data[..ph_end]);
    let file_header = load_struct_inplace_mut::<elf::FileHeader64<LittleEndian>>(&mut out_mmap, 0);
    file_header.e_phnum = endian::U16::new(LittleEndian, ph_num + 1);

    let program_headers = load_structs_inplace_mut::<elf::ProgramHeader64<LittleEndian>>(
        &mut out_mmap,
        ph_offset as usize,
        ph_num as usize + 1,
    );

    // Steal the extra bytes we need from the first loaded sections.
    // Generally this section has empty space due to alignment.
    let mut first_load_found = false;
    let mut shift_start = 0;
    let mut shift_end = 0;
    let mut first_load_aligned_size = 0;
    let mut load_align_constraint = 0;
    for mut ph in program_headers.iter_mut() {
        let p_type = ph.p_type.get(NativeEndian);
        let p_align = ph.p_align.get(NativeEndian);
        let p_filesz = ph.p_filesz.get(NativeEndian);
        let p_memsz = ph.p_memsz.get(NativeEndian);
        if p_type == elf::PT_LOAD && ph.p_offset.get(NativeEndian) == 0 {
            if p_filesz / p_align != (p_filesz + added_data) / p_align {
                println!("Not enough extra space in the executable for alignment");
                println!("This makes linking a lot harder and is not supported yet");
                return Ok(-1);
            }
            ph.p_filesz = endian::U64::new(LittleEndian, p_filesz + added_data);
            let new_memsz = p_memsz + added_data;
            ph.p_memsz = endian::U64::new(LittleEndian, new_memsz);
            let p_vaddr = ph.p_vaddr.get(NativeEndian);

            first_load_found = true;
            shift_start = p_vaddr + ph_end as u64;
            let align_remainder = new_memsz % p_align;
            first_load_aligned_size = if align_remainder == 0 {
                new_memsz
            } else {
                new_memsz + (p_align - align_remainder)
            };
            shift_end = p_vaddr + first_load_aligned_size;
            load_align_constraint = p_align;
            break;
        } else if p_type == elf::PT_PHDR {
            ph.p_filesz = endian::U64::new(LittleEndian, p_filesz + added_data);
            ph.p_memsz = endian::U64::new(LittleEndian, p_memsz + added_data);
        }
    }
    if !first_load_found {
        println!("Executable does not load any data at 0x00000000");
        println!("Probably input the wrong file as the executable");
        return Ok(-1);
    }
    if verbose {
        println!(
            "First Byte loaded after Program Headers: 0x{:x}",
            shift_start
        );
        println!("Last Byte loaded in first load: 0x{:x}", shift_end);
        println!("Aligned first load size: 0x{:x}", first_load_aligned_size);
    }

    for mut ph in program_headers {
        let p_vaddr = ph.p_vaddr.get(NativeEndian);
        if shift_start <= p_vaddr && p_vaddr < shift_end {
            let p_align = ph.p_align.get(NativeEndian);
            let p_offset = ph.p_offset.get(NativeEndian);
            let new_offset = p_offset + added_data;
            let new_vaddr = p_vaddr + added_data;
            if new_offset % p_align != 0 || new_vaddr % p_align != 0 {
                println!("Ran into alignment issues when moving segments");
                return Ok(-1);
            }
            ph.p_offset = endian::U64::new(LittleEndian, p_offset + added_data);
            ph.p_vaddr = endian::U64::new(LittleEndian, p_vaddr + added_data);
            ph.p_paddr = endian::U64::new(LittleEndian, ph.p_paddr.get(NativeEndian) + added_data);
        }
    }

    // Ensure no section overlaps with the hopefully blank data we are going to delete.
    let exec_section_headers = load_structs_inplace::<elf::SectionHeader64<LittleEndian>>(
        &exec_mmap,
        sh_offset as usize,
        sh_num as usize,
    );
    for sh in exec_section_headers {
        let offset = sh.sh_offset.get(NativeEndian);
        let size = sh.sh_size.get(NativeEndian);
        if offset <= first_load_aligned_size - added_data
            && offset + size >= first_load_aligned_size - added_data
        {
            println!("A section overlaps with some alignment data we need to delete");
            return Ok(-1);
        }
    }

    // Copy to program header, but add an extra item for the new data at the end of the file.
    // Also delete the extra padding to keep things align.
    out_mmap[ph_end + added_data as usize..first_load_aligned_size as usize].copy_from_slice(
        &exec_data[ph_end..first_load_aligned_size as usize - added_data as usize],
    );
    out_mmap[first_load_aligned_size as usize..sh_offset as usize]
        .copy_from_slice(&exec_data[first_load_aligned_size as usize..sh_offset as usize]);

    // Update dynamic table entries for shift of extra ProgramHeader.
    let dyn_offset = match md.dynamic_section_offset {
        Some(offset) => {
            if ph_end as u64 <= offset && offset < first_load_aligned_size {
                offset + added_data
            } else {
                offset
            }
        }
        None => {
            println!("Metadata missing dynamic section offset");
            return Ok(-1);
        }
    };
    let dyn_lib_count = match md.dynamic_lib_count {
        Some(count) => count as usize,
        None => {
            println!("Metadata missing dynamic library count");
            return Ok(-1);
        }
    };
    let shared_index = match md.shared_lib_index {
        Some(index) => index as usize,
        None => {
            println!("Metadata missing shared library index");
            return Ok(-1);
        }
    };

    let dyns = load_structs_inplace_mut::<elf::Dyn64<LittleEndian>>(
        &mut out_mmap,
        dyn_offset as usize,
        dyn_lib_count,
    );
    for mut d in dyns {
        match d.d_tag.get(NativeEndian) as u32 {
            // I believe this is the list of symbols that need to be update if addresses change.
            // I am less sure about the symbols from GNU_HASH down.
            elf::DT_INIT
            | elf::DT_FINI
            | elf::DT_PLTGOT
            | elf::DT_HASH
            | elf::DT_STRTAB
            | elf::DT_SYMTAB
            | elf::DT_RELA
            | elf::DT_REL
            | elf::DT_DEBUG
            | elf::DT_JMPREL
            | elf::DT_INIT_ARRAY
            | elf::DT_FINI_ARRAY
            | elf::DT_PREINIT_ARRAY
            | elf::DT_SYMTAB_SHNDX
            | elf::DT_GNU_HASH
            | elf::DT_TLSDESC_PLT
            | elf::DT_TLSDESC_GOT
            | elf::DT_GNU_CONFLICT
            | elf::DT_GNU_LIBLIST
            | elf::DT_CONFIG
            | elf::DT_DEPAUDIT
            | elf::DT_AUDIT
            | elf::DT_PLTPAD
            | elf::DT_MOVETAB
            | elf::DT_SYMINFO
            | elf::DT_VERSYM
            | elf::DT_VERDEF
            | elf::DT_VERNEED => {
                let d_addr = d.d_val.get(NativeEndian);
                if shift_start <= d_addr && d_addr < shift_end {
                    d.d_val = endian::U64::new(LittleEndian, d_addr + added_data);
                }
            }
            _ => {}
        }
    }

    // Delete shared library from the dynamic table.
    let out_ptr = out_mmap.as_mut_ptr();
    unsafe {
        std::ptr::copy(
            out_ptr.offset((dyn_offset as usize + 16 * (shared_index + 1)) as isize),
            out_ptr.offset((dyn_offset as usize + 16 * shared_index) as isize),
            16 * (dyn_lib_count - shared_index),
        );
    }

    // Update symbol table entries for shift of extra ProgramHeader.
    let symtab_offset = match md.symbol_table_section_offset {
        Some(offset) => {
            if ph_end as u64 <= offset && offset < first_load_aligned_size {
                offset + added_data
            } else {
                offset
            }
        }
        None => {
            println!("Metadata missing symbol table section offset");
            return Ok(-1);
        }
    };
    let symtab_size = match md.symbol_table_size {
        Some(count) => count as usize,
        None => {
            println!("Metadata missing symbol table size");
            return Ok(-1);
        }
    };

    let symbols = load_structs_inplace_mut::<elf::Sym64<LittleEndian>>(
        &mut out_mmap,
        symtab_offset as usize,
        symtab_size / mem::size_of::<elf::Sym64<LittleEndian>>(),
    );

    for sym in symbols {
        let addr = sym.st_value.get(NativeEndian);
        if shift_start <= addr && addr < shift_end {
            sym.st_value = endian::U64::new(LittleEndian, addr + added_data);
        }
    }
    let platform_gen_duration = platform_gen_start.elapsed().unwrap();

    // Flush platform only data to speed up write to disk.
    let mut offset = sh_offset as usize;
    out_mmap.flush_async_range(0, offset)?;

    // Align offset for new text/data section.
    let out_gen_start = SystemTime::now();
    let remainder = offset % load_align_constraint as usize;
    offset += load_align_constraint as usize - remainder;
    let new_segment_offset = offset;
    let new_data_section_offset = offset;

    // Copy sections and resolve their symbols/relocations.
    let symbols = app_obj.symbols().collect::<Vec<Symbol>>();

    let rodata_sections: Vec<Section> = app_obj
        .sections()
        .filter(|sec| {
            let name = sec.name();
            name.is_ok()
                && (name.unwrap().starts_with(".data") || name.unwrap().starts_with(".rodata"))
        })
        .collect();

    let mut rodata_address_map: MutMap<usize, usize> = MutMap::default();
    for sec in rodata_sections {
        let data = match sec.uncompressed_data() {
            Ok(data) => data,
            Err(err) => {
                println!("Failed to load data section, {:x?}: {}", sec, err);
                return Ok(-1);
            }
        };
        let size = data.len();
        out_mmap[offset..offset + size].copy_from_slice(&data);
        for (i, sym) in symbols.iter().enumerate() {
            if sym.section() == SymbolSection::Section(sec.index()) {
                rodata_address_map.insert(i, offset + sym.address() as usize - new_segment_offset);
            }
        }
        offset += size;
    }

    if verbose {
        println!("Data Relocation Addresses: {:x?}", rodata_address_map);
    }

    let text_sections: Vec<Section> = app_obj
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
    let new_text_section_offset = offset;
    let mut app_func_size_map: MutMap<String, u64> = MutMap::default();
    let mut app_func_segment_offset_map: MutMap<String, usize> = MutMap::default();
    for sec in text_sections {
        let data = match sec.uncompressed_data() {
            Ok(data) => data,
            Err(err) => {
                println!("Failed to load text section, {:x?}: {}", sec, err);
                return Ok(-1);
            }
        };
        let size = data.len();
        out_mmap[offset..offset + size].copy_from_slice(&data);
        // Deal with definitions and relocations for this section.
        if verbose {
            println!();
        }
        let current_segment_offset = (offset - new_segment_offset) as i64;
        for rel in sec.relocations() {
            if verbose {
                println!("Found Relocation: {:x?}", rel);
            }
            match rel.1.target() {
                RelocationTarget::Symbol(index) => {
                    let target = match rodata_address_map.get(&index.0) {
                        Some(x) => *x as i64,
                        None => {
                            println!("Undefined Symbol in relocation: {:x?}", rel);
                            return Ok(-1);
                        }
                    } - (rel.0 as i64 + current_segment_offset)
                        + rel.1.addend();
                    match rel.1.size() {
                        32 => {
                            let data = (target as i32).to_le_bytes();
                            let base = offset + rel.0 as usize;
                            out_mmap[base..base + 4].copy_from_slice(&data);
                        }
                        x => {
                            println!("Relocation size not yet supported: {}", x);
                            return Ok(-1);
                        }
                    }
                }
                _ => {
                    println!("Relocation not yet support: {:x?}", rel);
                    return Ok(-1);
                }
            }
        }
        for sym in symbols.iter() {
            if sym.section() == SymbolSection::Section(sec.index()) {
                let name = sym.name().unwrap_or_default().to_string();
                if md.app_functions.contains(&name) {
                    app_func_segment_offset_map.insert(
                        name.clone(),
                        offset + sym.address() as usize - new_segment_offset,
                    );
                    app_func_size_map.insert(name, sym.size());
                }
            }
        }
        offset += size;
    }

    if verbose {
        println!(
            "Found App Function Symbols: {:x?}",
            app_func_segment_offset_map
        );
    }

    let new_sh_offset = offset;
    let sh_size = sh_ent_size as usize * sh_num as usize;
    out_mmap[offset..offset + sh_size]
        .copy_from_slice(&exec_data[sh_offset as usize..sh_offset as usize + sh_size]);
    offset += sh_size;

    // Flush app only data to speed up write to disk.
    out_mmap.flush_async_range(new_segment_offset, offset - new_segment_offset)?;

    // Add 2 new sections.
    let new_section_count = 2;
    offset += new_section_count * sh_ent_size as usize;
    let section_headers = load_structs_inplace_mut::<elf::SectionHeader64<LittleEndian>>(
        &mut out_mmap,
        new_sh_offset as usize,
        sh_num as usize + new_section_count,
    );
    for mut sh in section_headers.iter_mut() {
        let offset = sh.sh_offset.get(NativeEndian);
        let addr = sh.sh_addr.get(NativeEndian);
        if ph_end as u64 <= offset && offset < first_load_aligned_size {
            sh.sh_offset = endian::U64::new(LittleEndian, offset + added_data);
        }
        if shift_start <= addr && addr < shift_end {
            sh.sh_addr = endian::U64::new(LittleEndian, addr + added_data);
        }
    }

    let last_vaddr = section_headers
        .iter()
        .map(|sh| sh.sh_addr.get(NativeEndian) + sh.sh_size.get(NativeEndian))
        .max()
        .unwrap();
    let remainder = last_vaddr % load_align_constraint;
    let new_segment_vaddr = last_vaddr + load_align_constraint - remainder;
    let new_data_section_vaddr = new_segment_vaddr;
    let new_data_section_size = new_text_section_offset - new_data_section_offset;
    let new_text_section_vaddr = new_data_section_vaddr + new_data_section_size as u64;

    let new_data_section = &mut section_headers[section_headers.len() - 2];
    new_data_section.sh_name = endian::U32::new(LittleEndian, 0);
    new_data_section.sh_type = endian::U32::new(LittleEndian, elf::SHT_PROGBITS);
    new_data_section.sh_flags = endian::U64::new(LittleEndian, (elf::SHF_ALLOC) as u64);
    new_data_section.sh_addr = endian::U64::new(LittleEndian, new_data_section_vaddr);
    new_data_section.sh_offset = endian::U64::new(LittleEndian, new_data_section_offset as u64);
    new_data_section.sh_size = endian::U64::new(LittleEndian, new_data_section_size as u64);
    new_data_section.sh_link = endian::U32::new(LittleEndian, 0);
    new_data_section.sh_info = endian::U32::new(LittleEndian, 0);
    new_data_section.sh_addralign = endian::U64::new(LittleEndian, 16);
    new_data_section.sh_entsize = endian::U64::new(LittleEndian, 0);

    let new_text_section_index = section_headers.len() - 1;
    let new_text_section = &mut section_headers[new_text_section_index];
    new_text_section.sh_name = endian::U32::new(LittleEndian, 0);
    new_text_section.sh_type = endian::U32::new(LittleEndian, elf::SHT_PROGBITS);
    new_text_section.sh_flags =
        endian::U64::new(LittleEndian, (elf::SHF_ALLOC | elf::SHF_EXECINSTR) as u64);
    new_text_section.sh_addr = endian::U64::new(LittleEndian, new_text_section_vaddr);
    new_text_section.sh_offset = endian::U64::new(LittleEndian, new_text_section_offset as u64);
    new_text_section.sh_size = endian::U64::new(
        LittleEndian,
        new_sh_offset as u64 - new_text_section_offset as u64,
    );
    new_text_section.sh_link = endian::U32::new(LittleEndian, 0);
    new_text_section.sh_info = endian::U32::new(LittleEndian, 0);
    new_text_section.sh_addralign = endian::U64::new(LittleEndian, 16);
    new_text_section.sh_entsize = endian::U64::new(LittleEndian, 0);

    // Reload and update file header and size.
    let file_header = load_struct_inplace_mut::<elf::FileHeader64<LittleEndian>>(&mut out_mmap, 0);
    file_header.e_shoff = endian::U64::new(LittleEndian, new_sh_offset as u64);
    file_header.e_shnum = endian::U16::new(LittleEndian, sh_num + new_section_count as u16);
    out_file.set_len(offset as u64 + 1)?;

    // Add new segment.
    let program_headers = load_structs_inplace_mut::<elf::ProgramHeader64<LittleEndian>>(
        &mut out_mmap,
        ph_offset as usize,
        ph_num as usize + 1,
    );
    let new_segment = program_headers.last_mut().unwrap();
    new_segment.p_type = endian::U32::new(LittleEndian, elf::PT_LOAD);
    new_segment.p_flags = endian::U32::new(LittleEndian, elf::PF_R | elf::PF_X);
    new_segment.p_offset = endian::U64::new(LittleEndian, new_segment_offset as u64);
    new_segment.p_vaddr = endian::U64::new(LittleEndian, new_segment_vaddr);
    new_segment.p_paddr = endian::U64::new(LittleEndian, new_segment_vaddr);
    let new_segment_size = (new_sh_offset - new_segment_offset) as u64;
    new_segment.p_filesz = endian::U64::new(LittleEndian, new_segment_size);
    new_segment.p_memsz = endian::U64::new(LittleEndian, new_segment_size);
    new_segment.p_align = endian::U64::new(LittleEndian, load_align_constraint);

    // Update calls from platform and dynamic symbols.
    let dynsym_offset = match md.dynamic_symbol_table_section_offset {
        Some(offset) => {
            if ph_end as u64 <= offset && offset < first_load_aligned_size {
                offset + added_data
            } else {
                offset
            }
        }
        None => {
            println!("Metadata missing dynamic symbol table section offset");
            return Ok(-1);
        }
    };

    for func_name in md.app_functions {
        let virt_offset = match app_func_segment_offset_map.get(&func_name) {
            Some(offset) => new_segment_vaddr + *offset as u64,
            None => {
                println!("Function, {}, was not defined by the app", &func_name);
                return Ok(-1);
            }
        };
        if verbose {
            println!(
                "Updating calls to {} to the address: {:x}",
                &func_name, virt_offset
            );
        }

        for s in md.surgeries.get(&func_name).unwrap_or(&vec![]) {
            if verbose {
                println!("\tPerforming surgery: {:x?}", s);
            }
            match s.size {
                4 => {
                    let target = (virt_offset as i64 - s.virtual_offset as i64) as i32;
                    if verbose {
                        println!("\tTarget Jump: {:x}", target);
                    }
                    let data = target.to_le_bytes();
                    out_mmap[s.file_offset as usize..s.file_offset as usize + 4]
                        .copy_from_slice(&data);
                }
                x => {
                    println!("Surgery size not yet supported: {}", x);
                    return Ok(-1);
                }
            }
        }

        // Replace plt call code with just a jump.
        // This is a backup incase we missed a call to the plt.
        if let Some((plt_off, plt_vaddr)) = md.plt_addresses.get(&func_name) {
            let plt_off = *plt_off as usize;
            let plt_vaddr = *plt_vaddr;
            let jmp_inst_len = 5;
            let target = (virt_offset as i64 - (plt_vaddr as i64 + jmp_inst_len as i64)) as i32;
            if verbose {
                println!("\tPLT: {:x}, {:x}", plt_off, plt_vaddr);
                println!("\tTarget Jump: {:x}", target);
            }
            let data = target.to_le_bytes();
            out_mmap[plt_off] = 0xE9;
            out_mmap[plt_off + 1..plt_off + jmp_inst_len].copy_from_slice(&data);
            for i in jmp_inst_len..PLT_ADDRESS_OFFSET as usize {
                out_mmap[plt_off + i] = 0x90;
            }
        }

        if let Some(i) = md.dynamic_symbol_indices.get(&func_name) {
            let sym = load_struct_inplace_mut::<elf::Sym64<LittleEndian>>(
                &mut out_mmap,
                dynsym_offset as usize + *i as usize * mem::size_of::<elf::Sym64<LittleEndian>>(),
            );
            sym.st_shndx = endian::U16::new(LittleEndian, new_text_section_index as u16);
            sym.st_value = endian::U64::new(LittleEndian, virt_offset as u64);
            sym.st_size = endian::U64::new(
                LittleEndian,
                match app_func_size_map.get(&func_name) {
                    Some(size) => *size,
                    None => {
                        println!("Size missing for: {}", &func_name);
                        return Ok(-1);
                    }
                },
            );
        }
    }

    let out_gen_duration = out_gen_start.elapsed().unwrap();

    let flushing_data_start = SystemTime::now();
    out_mmap.flush()?;
    let flushing_data_duration = flushing_data_start.elapsed().unwrap();
    let total_duration = total_start.elapsed().unwrap();

    if verbose {
        println!();
        println!("Timings");
        report_timing("Loading Metadata", loading_metadata_duration);
        report_timing("Executable Parsing", exec_parsing_duration);
        report_timing("Application Parsing", app_parsing_duration);
        report_timing("Platform Only Generation", platform_gen_duration);
        report_timing("Output Generation", out_gen_duration);
        report_timing("Flushing Data to Disk", flushing_data_duration);
        report_timing(
            "Other",
            total_duration
                - loading_metadata_duration
                - exec_parsing_duration
                - app_parsing_duration
                - platform_gen_duration
                - out_gen_duration
                - flushing_data_duration,
        );
        report_timing("Total", total_duration);
    }
    Ok(0)
}

fn load_struct_inplace<'a, T>(bytes: &'a [u8], offset: usize) -> &'a T {
    &load_structs_inplace(bytes, offset, 1)[0]
}

fn load_struct_inplace_mut<'a, T>(bytes: &'a mut [u8], offset: usize) -> &'a mut T {
    &mut load_structs_inplace_mut(bytes, offset, 1)[0]
}

fn load_structs_inplace<'a, T>(bytes: &'a [u8], offset: usize, count: usize) -> &'a [T] {
    let (head, body, tail) =
        unsafe { bytes[offset..offset + count * mem::size_of::<T>()].align_to::<T>() };
    assert!(head.is_empty(), "Data was not aligned");
    assert_eq!(count, body.len(), "Failed to load all structs");
    assert!(tail.is_empty(), "End of data was not aligned");
    body
}

fn load_structs_inplace_mut<'a, T>(
    bytes: &'a mut [u8],
    offset: usize,
    count: usize,
) -> &'a mut [T] {
    let (head, body, tail) =
        unsafe { bytes[offset..offset + count * mem::size_of::<T>()].align_to_mut::<T>() };
    assert!(head.is_empty(), "Data was not aligned");
    assert_eq!(count, body.len(), "Failed to load all structs");
    assert!(tail.is_empty(), "End of data was not aligned");
    body
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
