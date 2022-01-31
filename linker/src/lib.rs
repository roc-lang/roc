use bincode::{deserialize_from, serialize_into};
use clap::{App, AppSettings, Arg, ArgMatches};
use iced_x86::{Decoder, DecoderOptions, Instruction, OpCodeOperandKind, OpKind};
use memmap2::{Mmap, MmapMut};
use object::write;
use object::{elf, endian};
use object::{
    Architecture, BinaryFormat, CompressedFileRange, CompressionFormat, Endianness, LittleEndian,
    NativeEndian, Object, ObjectSection, ObjectSymbol, RelocationKind, RelocationTarget, Section,
    SectionIndex, Symbol, SymbolFlags, SymbolIndex, SymbolKind, SymbolScope, SymbolSection,
};
use roc_build::link::{rebuild_host, LinkType};
use roc_collections::all::MutMap;
use roc_mono::ir::OptLevel;
use std::cmp::Ordering;
use std::convert::TryFrom;
use std::ffi::CStr;
use std::fs;
use std::io;
use std::io::{BufReader, BufWriter};
use std::mem;
use std::os::raw::c_char;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::process::Command;
use std::time::{Duration, SystemTime};
use target_lexicon::Triple;
use tempfile::Builder;

mod metadata;
use metadata::VirtualOffset;

pub const CMD_PREPROCESS: &str = "preprocess";
pub const CMD_SURGERY: &str = "surgery";
pub const FLAG_VERBOSE: &str = "verbose";
pub const FLAG_TIME: &str = "time";

pub const EXEC: &str = "EXEC";
pub const METADATA: &str = "METADATA";
pub const SHARED_LIB: &str = "SHARED_LIB";
pub const APP: &str = "APP";
pub const OUT: &str = "OUT";

const MIN_SECTION_ALIGNMENT: usize = 0x40;

// TODO: Analyze if this offset is always correct.
const PLT_ADDRESS_OFFSET: u64 = 0x10;

fn report_timing(label: &str, duration: Duration) {
    println!("\t{:9.3} ms   {}", duration.as_secs_f64() * 1000.0, label,);
}

pub fn build_app<'a>() -> App<'a> {
    App::new("link")
        .about("Preprocesses a platform and surgically links it to an application.")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .subcommand(
            App::new(CMD_PREPROCESS)
                .about("Preprocesses a dynamically linked platform to prepare for linking.")
                .arg(
                    Arg::new(EXEC)
                        .about("The dynamically linked platform executable")
                        .required(true),
                )
                .arg(
                    Arg::new(METADATA)
                        .about("Where to save the metadata from preprocessing")
                        .required(true),
                )
                .arg(
                    Arg::new(OUT)
                        .about("The modified version of the dynamically linked platform executable")
                        .required(true),
                )
                .arg(
                    Arg::new(SHARED_LIB)
                        .about("The name of the shared library used in building the platform")
                        .default_value("libapp.so"),
                )
                .arg(
                    Arg::new(FLAG_VERBOSE)
                        .long(FLAG_VERBOSE)
                        .short('v')
                        .about("Enable verbose printing")
                        .required(false),
                )
                .arg(
                    Arg::new(FLAG_TIME)
                        .long(FLAG_TIME)
                        .short('t')
                        .about("Print timing information")
                        .required(false),
                ),
        )
        .subcommand(
            App::new(CMD_SURGERY)
                .about("Links a preprocessed platform with a Roc application.")
                .arg(
                    Arg::new(APP)
                        .about("The Roc application object file waiting to be linked")
                        .required(true),
                )
                .arg(
                    Arg::new(METADATA)
                        .about("The metadata created by preprocessing the platform")
                        .required(true),
                )
                .arg(
                    Arg::new(OUT)
                        .about(
                            "The modified version of the dynamically linked platform. \
                                It will be consumed to make linking faster.",
                        )
                        .required(true),
                )
                .arg(
                    Arg::new(FLAG_VERBOSE)
                        .long(FLAG_VERBOSE)
                        .short('v')
                        .about("Enable verbose printing")
                        .required(false),
                )
                .arg(
                    Arg::new(FLAG_TIME)
                        .long(FLAG_TIME)
                        .short('t')
                        .about("Print timing information")
                        .required(false),
                ),
        )
}

pub fn supported(link_type: &LinkType, target: &Triple) -> bool {
    link_type == &LinkType::Executable
        && target.architecture == target_lexicon::Architecture::X86_64
        && target.operating_system == target_lexicon::OperatingSystem::Linux
        && target.binary_format == target_lexicon::BinaryFormat::Elf
}

pub fn build_and_preprocess_host(
    opt_level: OptLevel,
    target: &Triple,
    host_input_path: &Path,
    exposed_to_host: Vec<String>,
    exported_closure_types: Vec<String>,
    target_valgrind: bool,
) -> io::Result<()> {
    let dummy_lib = host_input_path.with_file_name("libapp.so");
    generate_dynamic_lib(target, exposed_to_host, exported_closure_types, &dummy_lib)?;
    rebuild_host(
        opt_level,
        target,
        host_input_path,
        Some(&dummy_lib),
        target_valgrind,
    );
    let dynhost = host_input_path.with_file_name("dynhost");
    let metadata = host_input_path.with_file_name("metadata");
    let prehost = host_input_path.with_file_name("preprocessedhost");
    if preprocess_impl(
        dynhost.to_str().unwrap(),
        metadata.to_str().unwrap(),
        prehost.to_str().unwrap(),
        dummy_lib.to_str().unwrap(),
        false,
        false,
    )? != 0
    {
        panic!("Failed to preprocess host");
    }
    Ok(())
}

pub fn link_preprocessed_host(
    _target: &Triple,
    host_input_path: &Path,
    roc_app_obj: &Path,
    binary_path: &Path,
) -> io::Result<()> {
    let metadata = host_input_path.with_file_name("metadata");
    if surgery_impl(
        roc_app_obj.to_str().unwrap(),
        metadata.to_str().unwrap(),
        binary_path.to_str().unwrap(),
        false,
        false,
    )? != 0
    {
        panic!("Failed to surgically link host");
    }
    Ok(())
}

fn generate_dynamic_lib(
    _target: &Triple,
    exposed_to_host: Vec<String>,
    exported_closure_types: Vec<String>,
    dummy_lib_path: &Path,
) -> io::Result<()> {
    let dummy_obj_file = Builder::new().prefix("roc_lib").suffix(".o").tempfile()?;
    let dummy_obj_file = dummy_obj_file.path();

    // TODO deal with other architectures here.
    let mut out_object =
        write::Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);

    let text_section = out_object.section_id(write::StandardSection::Text);

    let mut add_symbol = |name: &String| {
        out_object.add_symbol(write::Symbol {
            name: name.as_bytes().to_vec(),
            value: 0,
            size: 0,
            kind: SymbolKind::Text,
            scope: SymbolScope::Dynamic,
            weak: false,
            section: write::SymbolSection::Section(text_section),
            flags: SymbolFlags::None,
        });
    };

    for sym in exposed_to_host {
        for name in &[
            format!("roc__{}_1_exposed", sym),
            format!("roc__{}_1_exposed_generic", sym),
            format!("roc__{}_size", sym),
        ] {
            add_symbol(name);
        }

        for closure_type in &exported_closure_types {
            for name in &[
                format!("roc__{}_1_{}_caller", sym, closure_type),
                format!("roc__{}_1_{}_size", sym, closure_type),
                format!("roc__{}_1_{}_result_size", sym, closure_type),
            ] {
                add_symbol(name)
            }
        }
    }
    std::fs::write(
        &dummy_obj_file,
        out_object.write().expect("failed to build output object"),
    )
    .expect("failed to write object to file");

    let output = Command::new("ld")
        .args(&[
            "-shared",
            "-soname",
            dummy_lib_path.file_name().unwrap().to_str().unwrap(),
            dummy_obj_file.to_str().unwrap(),
            "-o",
            dummy_lib_path.to_str().unwrap(),
        ])
        .output()
        .unwrap();

    if !output.status.success() {
        match std::str::from_utf8(&output.stderr) {
            Ok(stderr) => panic!(
                "Failed to link dummy shared library - stderr of the `ld` command was:\n{}",
                stderr
            ),
            Err(utf8_err) => panic!(
                "Failed to link dummy shared library  - stderr of the `ld` command was invalid utf8 ({:?})",
                utf8_err
            ),
        }
    }
    Ok(())
}

pub fn preprocess(matches: &ArgMatches) -> io::Result<i32> {
    preprocess_impl(
        matches.value_of(EXEC).unwrap(),
        matches.value_of(METADATA).unwrap(),
        matches.value_of(OUT).unwrap(),
        matches.value_of(SHARED_LIB).unwrap(),
        matches.is_present(FLAG_VERBOSE),
        matches.is_present(FLAG_TIME),
    )
}
// TODO: Most of this file is a mess of giant functions just to check if things work.
// Clean it all up and refactor nicely.
fn preprocess_impl(
    exec_filename: &str,
    metadata_filename: &str,
    out_filename: &str,
    shared_lib_filename: &str,
    verbose: bool,
    time: bool,
) -> io::Result<i32> {
    let total_start = SystemTime::now();
    let exec_parsing_start = total_start;
    let exec_file = fs::File::open(exec_filename)?;
    let exec_mmap = unsafe { Mmap::map(&exec_file)? };
    let exec_data = &*exec_mmap;
    let exec_obj = match object::File::parse(exec_data) {
        Ok(obj) => obj,
        Err(err) => {
            println!("Failed to parse executable file: {}", err);
            return Ok(-1);
        }
    };
    let exec_header = load_struct_inplace::<elf::FileHeader64<LittleEndian>>(exec_data, 0);

    let ph_offset = exec_header.e_phoff.get(NativeEndian);
    let ph_ent_size = exec_header.e_phentsize.get(NativeEndian);
    let ph_num = exec_header.e_phnum.get(NativeEndian);
    let sh_offset = exec_header.e_shoff.get(NativeEndian);
    let sh_ent_size = exec_header.e_shentsize.get(NativeEndian);
    let sh_num = exec_header.e_shnum.get(NativeEndian);
    if verbose {
        println!();
        println!("PH Offset: {:+x}", ph_offset);
        println!("PH Entry Size: {}", ph_ent_size);
        println!("PH Entry Count: {}", ph_num);
        println!("SH Offset: {:+x}", sh_offset);
        println!("SH Entry Size: {}", sh_ent_size);
        println!("SH Entry Count: {}", sh_num);
    }

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

    for sym in exec_obj.symbols().filter(|sym| {
        sym.is_definition() && sym.name().is_ok() && sym.name().unwrap().starts_with("roc_")
    }) {
        // remove potentially trailing "@version".
        let name = sym.name().unwrap().split('@').next().unwrap().to_string();

        // special exceptions for memcpy and memset.
        if &name == "roc_memcpy" {
            md.roc_symbol_vaddresses
                .insert("memcpy".to_string(), sym.address() as u64);
        } else if name == "roc_memset" {
            md.roc_symbol_vaddresses
                .insert("memset".to_string(), sym.address() as u64);
        }
        md.roc_symbol_vaddresses.insert(name, sym.address() as u64);
    }

    if verbose {
        println!(
            "Found roc symbol definitions: {:+x?}",
            md.roc_symbol_vaddresses
        );
    }

    let exec_parsing_duration = exec_parsing_start.elapsed().unwrap();

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
        println!("PLT Address: {:+x}", plt_address);
        println!("PLT File Offset: {:+x}", plt_offset);
    }

    let plt_relocs = (match exec_obj.dynamic_relocations() {
        Some(relocs) => relocs,
        None => {
            println!("Executable never calls any application functions.");
            println!("No work to do. Probably an invalid input.");
            return Ok(-1);
        }
    })
    .map(|(_, reloc)| reloc)
    .filter(|reloc| matches!(reloc.kind(), RelocationKind::Elf(7)));

    let app_syms: Vec<Symbol> = exec_obj
        .dynamic_symbols()
        .filter(|sym| {
            sym.is_undefined() && sym.name().is_ok() && sym.name().unwrap().starts_with("roc_")
        })
        .collect();

    let got_app_syms: Vec<(String, usize)> = (match exec_obj.dynamic_relocations() {
        Some(relocs) => relocs,
        None => {
            println!("Executable never calls any application functions.");
            println!("No work to do. Probably an invalid input.");
            return Ok(-1);
        }
    })
    .map(|(_, reloc)| reloc)
    .filter(|reloc| matches!(reloc.kind(), RelocationKind::Elf(6)))
    .map(|reloc| {
        for symbol in app_syms.iter() {
            if reloc.target() == RelocationTarget::Symbol(symbol.index()) {
                return Some((symbol.name().unwrap().to_string(), symbol.index().0));
            }
        }
        None
    })
    .flatten()
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
            println!("{}: {:+x?}", symbol.index().0, symbol);
        }
    }

    let mut app_func_addresses: MutMap<u64, &str> = MutMap::default();
    for (i, reloc) in plt_relocs.enumerate() {
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
        println!("App Function Address Map: {:+x?}", app_func_addresses);
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
            println!("{:+x?}", sec);
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
                    "Issues dealing with section compression for {:+x?}: {}",
                    sec, err
                );
                return Ok(-1);
            }
        };

        let data = match sec.uncompressed_data() {
            Ok(data) => data,
            Err(err) => {
                println!("Failed to load text section, {:+x?}: {}", sec, err);
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
                            println!("Surgical linking does not work with compressed text sections: {:+x?}", sec);
                            return Ok(-1);
                        }

                        if verbose {
                            println!(
                                "Found branch from {:+x} to {:+x}({})",
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
                                "\tNeed to surgically replace {} bytes at file offset {:+x}",
                                op_size, offset,
                            );
                            println!(
                                "\tIts current value is {:+x?}",
                                &exec_data[offset as usize..(offset + op_size as u64) as usize]
                            )
                        }
                        md.surgeries
                            .get_mut(*func_name)
                            .unwrap()
                            .push(metadata::SurgeryEntry {
                                file_offset: offset,
                                virtual_offset: VirtualOffset::Relative(inst.next_ip()),
                                size: op_size,
                            });
                    }
                }
                Ok(OpKind::FarBranch16 | OpKind::FarBranch32) => {
                    println!(
                        "Found branch type instruction that is not yet support: {:+x?}",
                        inst
                    );
                    return Ok(-1);
                }
                Ok(_) => {
                    if (inst.is_call_far_indirect()
                        || inst.is_call_near_indirect()
                        || inst.is_jmp_far_indirect()
                        || inst.is_jmp_near_indirect())
                        && !indirect_warning_given
                        && verbose
                    {
                        indirect_warning_given = true;
                        println!();
                        println!("Cannot analyaze through indirect jmp type instructions");
                        println!("Most likely this is not a problem, but it could mean a loss in optimizations");
                        println!();
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
    md.dynamic_section_offset = dyn_offset as u64;

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

    let shared_lib_name = Path::new(shared_lib_filename)
        .file_name()
        .unwrap()
        .to_str()
        .unwrap();

    let mut dyn_lib_index = 0;
    let mut shared_lib_index = None;
    loop {
        let dyn_tag = u64::from_le_bytes(
            <[u8; 8]>::try_from(
                &exec_data[dyn_offset + dyn_lib_index * 16..dyn_offset + dyn_lib_index * 16 + 8],
            )
            .unwrap(),
        );
        if dyn_tag == 0 {
            break;
        } else if dyn_tag == 1 {
            let dynstr_off = u64::from_le_bytes(
                <[u8; 8]>::try_from(
                    &exec_data
                        [dyn_offset + dyn_lib_index * 16 + 8..dyn_offset + dyn_lib_index * 16 + 16],
                )
                .unwrap(),
            ) as usize;
            let c_buf: *const c_char = dynstr_data[dynstr_off..].as_ptr() as *const i8;
            let c_str = unsafe { CStr::from_ptr(c_buf) }.to_str().unwrap();
            if Path::new(c_str).file_name().unwrap().to_str().unwrap() == shared_lib_name {
                shared_lib_index = Some(dyn_lib_index);
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
    let dynamic_lib_count = dyn_lib_index as usize;

    if shared_lib_index.is_none() {
        println!("Shared lib not found as a dependency of the executable");
        return Ok(-1);
    }
    let shared_lib_index = shared_lib_index.unwrap();

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
    md.symbol_table_section_offset = symtab_offset as u64;
    md.symbol_table_size = symtab_sec.size();

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
    md.dynamic_symbol_table_section_offset = dynsym_offset as u64;

    let mut got_sections: Vec<(usize, usize)> = vec![];
    for sec in exec_obj
        .sections()
        .filter(|sec| sec.name().is_ok() && sec.name().unwrap().starts_with(".got"))
    {
        match sec.compressed_file_range() {
            Ok(
                range
                @
                CompressedFileRange {
                    format: CompressionFormat::None,
                    ..
                },
            ) => got_sections.push((range.offset as usize, range.uncompressed_size as usize)),
            _ => {
                println!("Surgical linking does not work with compressed got sections");
                return Ok(-1);
            }
        }
    }

    let platform_gen_start = SystemTime::now();

    // Copy header and shift everything to enable more program sections.
    let added_header_count = 2;
    md.added_byte_count = ph_ent_size as u64 * added_header_count;
    md.added_byte_count = md.added_byte_count
        + (MIN_SECTION_ALIGNMENT as u64 - md.added_byte_count % MIN_SECTION_ALIGNMENT as u64);
    let ph_end = ph_offset as usize + ph_num as usize * ph_ent_size as usize;
    let physical_shift_start = ph_end as u64;

    md.exec_len = exec_data.len() as u64 + md.added_byte_count;
    let out_file = fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open(out_filename)?;
    out_file.set_len(md.exec_len)?;
    let mut out_mmap = unsafe { MmapMut::map_mut(&out_file)? };

    out_mmap[..ph_end].copy_from_slice(&exec_data[..ph_end]);

    let program_headers = load_structs_inplace_mut::<elf::ProgramHeader64<LittleEndian>>(
        &mut out_mmap,
        ph_offset as usize,
        ph_num as usize,
    );
    let mut first_load_found = false;
    let mut virtual_shift_start = 0;
    for ph in program_headers.iter() {
        let p_type = ph.p_type.get(NativeEndian);
        if p_type == elf::PT_LOAD && ph.p_offset.get(NativeEndian) == 0 {
            first_load_found = true;
            md.load_align_constraint = ph.p_align.get(NativeEndian);
            virtual_shift_start = physical_shift_start + ph.p_vaddr.get(NativeEndian);
        }
    }
    if !first_load_found {
        println!("Executable does not load any data at 0x00000000");
        println!("Probably input the wrong file as the executable");
        return Ok(-1);
    }
    if verbose {
        println!(
            "Shifting all data after: {:+x}({:+x})",
            physical_shift_start, virtual_shift_start
        );
    }

    // Shift all of the program headers.
    for ph in program_headers.iter_mut() {
        let p_type = ph.p_type.get(NativeEndian);
        let p_offset = ph.p_offset.get(NativeEndian);
        if (p_type == elf::PT_LOAD && p_offset == 0) || p_type == elf::PT_PHDR {
            // Extend length for the first segment and the program header.
            ph.p_filesz = endian::U64::new(
                LittleEndian,
                ph.p_filesz.get(NativeEndian) + md.added_byte_count,
            );
            ph.p_memsz = endian::U64::new(
                LittleEndian,
                ph.p_memsz.get(NativeEndian) + md.added_byte_count,
            );
        } else {
            // Shift if needed.
            if physical_shift_start <= p_offset {
                ph.p_offset = endian::U64::new(LittleEndian, p_offset + md.added_byte_count);
            }
            let p_vaddr = ph.p_vaddr.get(NativeEndian);
            if virtual_shift_start <= p_vaddr {
                ph.p_vaddr = endian::U64::new(LittleEndian, p_vaddr + md.added_byte_count);
                ph.p_paddr = endian::U64::new(LittleEndian, p_vaddr + md.added_byte_count);
            }
        }
    }

    // Get last segment virtual address.
    let last_segment_vaddr = program_headers
        .iter()
        .filter(|ph| ph.p_type.get(NativeEndian) != elf::PT_GNU_STACK)
        .map(|ph| ph.p_vaddr.get(NativeEndian) + ph.p_memsz.get(NativeEndian))
        .max()
        .unwrap();

    // Copy the rest of the file shifted as needed.
    out_mmap[physical_shift_start as usize + md.added_byte_count as usize..]
        .copy_from_slice(&exec_data[physical_shift_start as usize..]);

    // Update all sections for shift for extra program headers.
    let section_headers = load_structs_inplace_mut::<elf::SectionHeader64<LittleEndian>>(
        &mut out_mmap,
        sh_offset as usize + md.added_byte_count as usize,
        sh_num as usize,
    );

    let mut rel_sections: Vec<(u64, u64)> = vec![];
    let mut rela_sections: Vec<(u64, u64)> = vec![];
    for sh in section_headers.iter_mut() {
        let sh_offset = sh.sh_offset.get(NativeEndian);
        let sh_addr = sh.sh_addr.get(NativeEndian);
        if physical_shift_start <= sh_offset {
            sh.sh_offset = endian::U64::new(LittleEndian, sh_offset + md.added_byte_count);
        }
        if virtual_shift_start <= sh_addr {
            sh.sh_addr = endian::U64::new(LittleEndian, sh_addr + md.added_byte_count);
        }

        // Record every relocation section.
        let sh_type = sh.sh_type.get(NativeEndian);
        if sh_type == elf::SHT_REL {
            rel_sections.push((sh_offset, sh.sh_size.get(NativeEndian)));
        } else if sh_type == elf::SHT_RELA {
            rela_sections.push((sh_offset, sh.sh_size.get(NativeEndian)));
        }
    }

    // Get last section virtual address.
    let last_section_vaddr = section_headers
        .iter()
        .map(|sh| sh.sh_addr.get(NativeEndian) + sh.sh_size.get(NativeEndian))
        .max()
        .unwrap();

    // Calculate end virtual address for new segment.
    // TODO: potentially remove md.load_align_constraint here. I think we should be able to cram things together.
    md.last_vaddr =
        std::cmp::max(last_section_vaddr, last_segment_vaddr) + md.load_align_constraint;

    // Update all relocations for shift for extra program headers.
    for (sec_offset, sec_size) in rel_sections {
        let relocations = load_structs_inplace_mut::<elf::Rel64<LittleEndian>>(
            &mut out_mmap,
            sec_offset as usize + md.added_byte_count as usize,
            sec_size as usize / mem::size_of::<elf::Rel64<LittleEndian>>(),
        );
        for rel in relocations.iter_mut() {
            let r_offset = rel.r_offset.get(NativeEndian);
            if virtual_shift_start <= r_offset {
                rel.r_offset = endian::U64::new(LittleEndian, r_offset + md.added_byte_count);
            }
        }
    }
    for (sec_offset, sec_size) in rela_sections {
        let relocations = load_structs_inplace_mut::<elf::Rela64<LittleEndian>>(
            &mut out_mmap,
            sec_offset as usize + md.added_byte_count as usize,
            sec_size as usize / mem::size_of::<elf::Rela64<LittleEndian>>(),
        );
        for (i, rel) in relocations.iter_mut().enumerate() {
            let r_offset = rel.r_offset.get(NativeEndian);
            if virtual_shift_start <= r_offset {
                rel.r_offset = endian::U64::new(LittleEndian, r_offset + md.added_byte_count);
                // Deal with potential adjusts to absolute jumps.
                // TODO: Verify other relocation types.
                if rel.r_type(LittleEndian, false) == elf::R_X86_64_RELATIVE {
                    let r_addend = rel.r_addend.get(LittleEndian);
                    rel.r_addend
                        .set(LittleEndian, r_addend + md.added_byte_count as i64);
                }
            }
            // If the relocation goes to a roc function, we need to surgically link it and change it to relative.
            let r_type = rel.r_type(NativeEndian, false);
            if r_type == elf::R_X86_64_GLOB_DAT {
                let r_sym = rel.r_sym(NativeEndian, false);
                for (name, index) in got_app_syms.iter() {
                    if *index as u32 == r_sym {
                        rel.set_r_info(LittleEndian, false, 0, elf::R_X86_64_RELATIVE);
                        let addend_addr = sec_offset as usize
                            + i * mem::size_of::<elf::Rela64<LittleEndian>>()
                            // This 16 skips the first 2 fields and gets to the addend field.
                            + 16;
                        md.surgeries
                            .get_mut(name)
                            .unwrap()
                            .push(metadata::SurgeryEntry {
                                file_offset: addend_addr as u64,
                                virtual_offset: VirtualOffset::Absolute,
                                size: 8,
                            });
                    }
                }
            }
        }
    }

    // Update dynamic table entries for shift for extra program headers.
    let dyn_offset = md.dynamic_section_offset + md.added_byte_count;

    let dyns = load_structs_inplace_mut::<elf::Dyn64<LittleEndian>>(
        &mut out_mmap,
        dyn_offset as usize,
        dynamic_lib_count,
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
                if virtual_shift_start <= d_addr {
                    d.d_val = endian::U64::new(LittleEndian, d_addr + md.added_byte_count);
                }
            }
            _ => {}
        }
    }

    // Update symbol table entries for shift for extra program headers.
    let symtab_offset = md.symbol_table_section_offset + md.added_byte_count;
    let symtab_size = md.symbol_table_size as usize;

    let symbols = load_structs_inplace_mut::<elf::Sym64<LittleEndian>>(
        &mut out_mmap,
        symtab_offset as usize,
        symtab_size / mem::size_of::<elf::Sym64<LittleEndian>>(),
    );

    for sym in symbols {
        let addr = sym.st_value.get(NativeEndian);
        if virtual_shift_start <= addr {
            sym.st_value = endian::U64::new(LittleEndian, addr + md.added_byte_count);
        }
    }

    // Update all data in the global offset table.
    for (offset, size) in got_sections {
        let global_offsets = load_structs_inplace_mut::<endian::U64<LittleEndian>>(
            &mut out_mmap,
            offset as usize + md.added_byte_count as usize,
            size / mem::size_of::<endian::U64<LittleEndian>>(),
        );
        for go in global_offsets.iter_mut() {
            let go_addr = go.get(NativeEndian);
            if physical_shift_start <= go_addr {
                go.set(LittleEndian, go_addr + md.added_byte_count);
            }
        }
    }

    // TODO: look into shifting all of the debug info and eh_frames.

    // Delete shared library from the dynamic table.
    let out_ptr = out_mmap.as_mut_ptr();
    unsafe {
        std::ptr::copy(
            out_ptr.add(dyn_offset as usize + 16 * (shared_lib_index + 1)),
            out_ptr.add(dyn_offset as usize + 16 * shared_lib_index),
            16 * (dynamic_lib_count - shared_lib_index),
        );
    }

    // Update main elf header for extra data.
    let mut file_header =
        load_struct_inplace_mut::<elf::FileHeader64<LittleEndian>>(&mut out_mmap, 0);
    file_header.e_shoff = endian::U64::new(
        LittleEndian,
        file_header.e_shoff.get(NativeEndian) + md.added_byte_count,
    );
    let e_entry = file_header.e_entry.get(NativeEndian);
    if virtual_shift_start <= e_entry {
        file_header.e_entry = endian::U64::new(LittleEndian, e_entry + md.added_byte_count);
    }
    file_header.e_phnum = endian::U16::new(LittleEndian, ph_num + added_header_count as u16);

    let platform_gen_duration = platform_gen_start.elapsed().unwrap();

    if verbose {
        println!();
        println!("{:+x?}", md);
    }

    let saving_metadata_start = SystemTime::now();
    // This block ensure that the metadata is fully written and timed before continuing.
    {
        let output = fs::File::create(metadata_filename)?;
        let output = BufWriter::new(output);
        if let Err(err) = serialize_into(output, &md) {
            println!("Failed to serialize metadata: {}", err);
            return Ok(-1);
        };
    }
    let saving_metadata_duration = saving_metadata_start.elapsed().unwrap();

    let flushing_data_start = SystemTime::now();
    out_mmap.flush()?;
    // Also drop files to to ensure data is fully written here.
    drop(out_mmap);
    drop(out_file);
    let flushing_data_duration = flushing_data_start.elapsed().unwrap();

    let total_duration = total_start.elapsed().unwrap();

    if verbose || time {
        println!();
        println!("Timings");
        report_timing("Executable Parsing", exec_parsing_duration);
        report_timing(
            "Symbol and PLT Processing",
            symbol_and_plt_processing_duration,
        );
        report_timing("Text Disassembly", text_disassembly_duration);
        report_timing("Scanning Dynamic Deps", scanning_dynamic_deps_duration);
        report_timing("Generate Modified Platform", platform_gen_duration);
        report_timing("Saving Metadata", saving_metadata_duration);
        report_timing("Flushing Data to Disk", flushing_data_duration);
        report_timing(
            "Other",
            total_duration
                - exec_parsing_duration
                - symbol_and_plt_processing_duration
                - text_disassembly_duration
                - scanning_dynamic_deps_duration
                - platform_gen_duration
                - saving_metadata_duration
                - flushing_data_duration,
        );
        report_timing("Total", total_duration);
    }

    Ok(0)
}

pub fn surgery(matches: &ArgMatches) -> io::Result<i32> {
    surgery_impl(
        matches.value_of(APP).unwrap(),
        matches.value_of(METADATA).unwrap(),
        matches.value_of(OUT).unwrap(),
        matches.is_present(FLAG_VERBOSE),
        matches.is_present(FLAG_TIME),
    )
}

fn surgery_impl(
    app_filename: &str,
    metadata_filename: &str,
    out_filename: &str,
    verbose: bool,
    time: bool,
) -> io::Result<i32> {
    let total_start = SystemTime::now();
    let loading_metadata_start = total_start;
    let input = fs::File::open(metadata_filename)?;
    let input = BufReader::new(input);
    let md: metadata::Metadata = match deserialize_from(input) {
        Ok(data) => data,
        Err(err) => {
            println!("Failed to deserialize metadata: {}", err);
            return Ok(-1);
        }
    };
    let loading_metadata_duration = loading_metadata_start.elapsed().unwrap();

    let app_parsing_start = SystemTime::now();
    let app_file = fs::File::open(app_filename)?;
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

    let exec_parsing_start = SystemTime::now();
    let exec_file = fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open(out_filename)?;

    let max_out_len = md.exec_len + app_data.len() as u64 + md.load_align_constraint;
    exec_file.set_len(max_out_len)?;

    let mut exec_mmap = unsafe { MmapMut::map_mut(&exec_file)? };
    let elf64 = exec_mmap[4] == 2;
    let litte_endian = exec_mmap[5] == 1;
    if !elf64 || !litte_endian {
        println!("Only 64bit little endian elf currently supported for surgery");
        return Ok(-1);
    }
    let exec_header = load_struct_inplace::<elf::FileHeader64<LittleEndian>>(&exec_mmap, 0);

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
        println!("PH Offset: {:+x}", ph_offset);
        println!("PH Entry Size: {}", ph_ent_size);
        println!("PH Entry Count: {}", ph_num);
        println!("SH Offset: {:+x}", sh_offset);
        println!("SH Entry Size: {}", sh_ent_size);
        println!("SH Entry Count: {}", sh_num);
    }
    let exec_parsing_duration = exec_parsing_start.elapsed().unwrap();

    let out_gen_start = SystemTime::now();
    // Backup section header table.
    let sh_size = sh_ent_size as usize * sh_num as usize;
    let mut sh_tab = vec![];
    sh_tab.extend_from_slice(&exec_mmap[sh_offset as usize..sh_offset as usize + sh_size]);

    let mut offset = sh_offset as usize;
    offset = align_by_constraint(offset, MIN_SECTION_ALIGNMENT);

    let new_rodata_section_offset = offset;

    // Align physical and virtual address of new segment.
    let mut virt_offset = align_to_offset_by_constraint(
        md.last_vaddr as usize,
        offset,
        md.load_align_constraint as usize,
    );
    let new_rodata_section_vaddr = virt_offset;
    if verbose {
        println!();
        println!(
            "New Virtual Rodata Section Address: {:+x?}",
            new_rodata_section_vaddr
        );
    }

    // First decide on sections locations and then recode every exact symbol locations.

    // Copy sections and resolve their symbols/relocations.
    let symbols = app_obj.symbols().collect::<Vec<Symbol>>();
    let mut section_offset_map: MutMap<SectionIndex, (usize, usize)> = MutMap::default();
    let mut symbol_vaddr_map: MutMap<SymbolIndex, usize> = MutMap::default();
    let mut app_func_vaddr_map: MutMap<String, usize> = MutMap::default();
    let mut app_func_size_map: MutMap<String, u64> = MutMap::default();

    // TODO: Does Roc ever create a data section? I think no cause it would mess up fully functional guarantees.
    // If not we never need to think about it, but we should double check.
    let rodata_sections: Vec<Section> = app_obj
        .sections()
        .filter(|sec| sec.name().unwrap_or_default().starts_with(".rodata"))
        .collect();

    // bss section is like rodata section, but it has zero file size and non-zero virtual size.
    let bss_sections: Vec<Section> = app_obj
        .sections()
        .filter(|sec| sec.name().unwrap_or_default().starts_with(".bss"))
        .collect();

    let text_sections: Vec<Section> = app_obj
        .sections()
        .filter(|sec| sec.name().unwrap_or_default().starts_with(".text"))
        .collect();
    if text_sections.is_empty() {
        println!("No text sections found. This application has no code.");
        return Ok(-1);
    }

    // Calculate addresses and load symbols.
    // Note, it is important the bss sections come after the rodata sections.
    for sec in rodata_sections
        .iter()
        .chain(bss_sections.iter())
        .chain(text_sections.iter())
    {
        offset = align_by_constraint(offset, MIN_SECTION_ALIGNMENT);
        virt_offset =
            align_to_offset_by_constraint(virt_offset, offset, md.load_align_constraint as usize);
        if verbose {
            println!(
                "Section, {}, is being put at offset: {:+x}(virt: {:+x})",
                sec.name().unwrap(),
                offset,
                virt_offset
            )
        }
        section_offset_map.insert(sec.index(), (offset, virt_offset));
        for sym in symbols.iter() {
            if sym.section() == SymbolSection::Section(sec.index()) {
                let name = sym.name().unwrap_or_default().to_string();
                if !md.roc_symbol_vaddresses.contains_key(&name) {
                    symbol_vaddr_map.insert(sym.index(), virt_offset + sym.address() as usize);
                }
                if md.app_functions.contains(&name) {
                    app_func_vaddr_map.insert(name.clone(), virt_offset + sym.address() as usize);
                    app_func_size_map.insert(name, sym.size());
                }
            }
        }
        let section_size = match sec.file_range() {
            Some((_, size)) => size,
            None => 0,
        };
        if sec.name().unwrap_or_default().starts_with(".bss") {
            // bss sections only modify the virtual size.
            virt_offset += sec.size() as usize;
        } else if section_size != sec.size() {
            println!(
                "We do not deal with non bss sections that have different on disk and in memory sizes"
            );
            return Ok(-1);
        } else {
            offset += section_size as usize;
            virt_offset += sec.size() as usize;
        }
    }
    if verbose {
        println!("Data Relocation Offsets: {:+x?}", symbol_vaddr_map);
        println!("Found App Function Symbols: {:+x?}", app_func_vaddr_map);
    }

    let (new_text_section_offset, new_text_section_vaddr) = text_sections
        .iter()
        .map(|sec| section_offset_map.get(&sec.index()).unwrap())
        .min()
        .unwrap();
    let (new_text_section_offset, new_text_section_vaddr) =
        (*new_text_section_offset, *new_text_section_vaddr);

    // Move data and deal with relocations.
    for sec in rodata_sections
        .iter()
        .chain(bss_sections.iter())
        .chain(text_sections.iter())
    {
        let data = match sec.data() {
            Ok(data) => data,
            Err(err) => {
                println!(
                    "Failed to load data for section, {:+x?}: {}",
                    sec.name().unwrap(),
                    err
                );
                return Ok(-1);
            }
        };
        let (section_offset, section_virtual_offset) =
            section_offset_map.get(&sec.index()).unwrap();
        let (section_offset, section_virtual_offset) = (*section_offset, *section_virtual_offset);
        exec_mmap[section_offset..section_offset + data.len()].copy_from_slice(data);
        // Deal with definitions and relocations for this section.
        if verbose {
            println!();
            println!(
                "Processing Relocations for Section: {:+x?} @ {:+x} (virt: {:+x})",
                sec, section_offset, section_virtual_offset
            );
        }
        for rel in sec.relocations() {
            if verbose {
                println!("\tFound Relocation: {:+x?}", rel);
            }
            match rel.1.target() {
                RelocationTarget::Symbol(index) => {
                    let target_offset = if let Some(target_offset) = symbol_vaddr_map.get(&index) {
                        if verbose {
                            println!(
                                "\t\tRelocation targets symbol in app at: {:+x}",
                                target_offset
                            );
                        }
                        Some(*target_offset as i64)
                    } else {
                        app_obj
                            .symbol_by_index(index)
                            .and_then(|sym| sym.name())
                            .ok()
                            .and_then(|name| {
                                md.roc_symbol_vaddresses.get(name).map(|address| {
                                    let vaddr = (*address + md.added_byte_count) as i64;
                                    if verbose {
                                        println!(
                                            "\t\tRelocation targets symbol in host: {} @ {:+x}",
                                            name, vaddr
                                        );
                                    }
                                    vaddr
                                })
                            })
                    };

                    if let Some(target_offset) = target_offset {
                        let virt_base = section_virtual_offset as usize + rel.0 as usize;
                        let base = section_offset as usize + rel.0 as usize;
                        let target: i64 = match rel.1.kind() {
                            RelocationKind::Relative | RelocationKind::PltRelative => {
                                target_offset - virt_base as i64 + rel.1.addend()
                            }
                            x => {
                                println!("Relocation Kind not yet support: {:?}", x);
                                return Ok(-1);
                            }
                        };
                        if verbose {
                            println!(
                                "\t\tRelocation base location: {:+x} (virt: {:+x})",
                                base, virt_base
                            );
                            println!("\t\tFinal relocation target offset: {:+x}", target);
                        }
                        match rel.1.size() {
                            32 => {
                                let data = (target as i32).to_le_bytes();
                                exec_mmap[base..base + 4].copy_from_slice(&data);
                            }
                            64 => {
                                let data = target.to_le_bytes();
                                exec_mmap[base..base + 8].copy_from_slice(&data);
                            }
                            x => {
                                println!("Relocation size not yet supported: {}", x);
                                return Ok(-1);
                            }
                        }
                    } else if matches!(app_obj.symbol_by_index(index), Ok(sym) if ["__divti3", "__udivti3"].contains(&sym.name().unwrap_or_default()))
                    {
                        // Explicitly ignore some symbols that are currently always linked.
                        continue;
                    } else {
                        println!(
                            "Undefined Symbol in relocation, {:+x?}: {:+x?}",
                            rel,
                            app_obj.symbol_by_index(index)
                        );
                        return Ok(-1);
                    }
                }

                _ => {
                    println!("Relocation target not yet support: {:+x?}", rel);
                    return Ok(-1);
                }
            }
        }
    }

    offset = align_by_constraint(offset, MIN_SECTION_ALIGNMENT);
    let new_sh_offset = offset;
    exec_mmap[offset..offset + sh_size].copy_from_slice(&sh_tab);
    offset += sh_size;

    // Flush app only data to speed up write to disk.
    exec_mmap.flush_async_range(
        new_rodata_section_offset,
        offset - new_rodata_section_offset,
    )?;

    // TODO: look into merging symbol tables, debug info, and eh frames to enable better debugger experience.

    // Add 2 new sections and segments.
    let new_section_count = 2;
    offset += new_section_count * sh_ent_size as usize;
    let section_headers = load_structs_inplace_mut::<elf::SectionHeader64<LittleEndian>>(
        &mut exec_mmap,
        new_sh_offset as usize,
        sh_num as usize + new_section_count,
    );

    let new_rodata_section_size = new_text_section_offset as u64 - new_rodata_section_offset as u64;
    let new_rodata_section_virtual_size =
        new_text_section_vaddr as u64 - new_rodata_section_vaddr as u64;
    let new_text_section_vaddr = new_rodata_section_vaddr as u64 + new_rodata_section_size as u64;
    let new_text_section_size = new_sh_offset as u64 - new_text_section_offset as u64;

    let new_rodata_section = &mut section_headers[section_headers.len() - 2];
    new_rodata_section.sh_name = endian::U32::new(LittleEndian, 0);
    new_rodata_section.sh_type = endian::U32::new(LittleEndian, elf::SHT_PROGBITS);
    new_rodata_section.sh_flags = endian::U64::new(LittleEndian, (elf::SHF_ALLOC) as u64);
    new_rodata_section.sh_addr = endian::U64::new(LittleEndian, new_rodata_section_vaddr as u64);
    new_rodata_section.sh_offset = endian::U64::new(LittleEndian, new_rodata_section_offset as u64);
    new_rodata_section.sh_size = endian::U64::new(LittleEndian, new_rodata_section_size);
    new_rodata_section.sh_link = endian::U32::new(LittleEndian, 0);
    new_rodata_section.sh_info = endian::U32::new(LittleEndian, 0);
    new_rodata_section.sh_addralign = endian::U64::new(LittleEndian, 16);
    new_rodata_section.sh_entsize = endian::U64::new(LittleEndian, 0);

    let new_text_section_index = section_headers.len() - 1;
    let new_text_section = &mut section_headers[new_text_section_index];
    new_text_section.sh_name = endian::U32::new(LittleEndian, 0);
    new_text_section.sh_type = endian::U32::new(LittleEndian, elf::SHT_PROGBITS);
    new_text_section.sh_flags =
        endian::U64::new(LittleEndian, (elf::SHF_ALLOC | elf::SHF_EXECINSTR) as u64);
    new_text_section.sh_addr = endian::U64::new(LittleEndian, new_text_section_vaddr);
    new_text_section.sh_offset = endian::U64::new(LittleEndian, new_text_section_offset as u64);
    new_text_section.sh_size = endian::U64::new(LittleEndian, new_text_section_size);
    new_text_section.sh_link = endian::U32::new(LittleEndian, 0);
    new_text_section.sh_info = endian::U32::new(LittleEndian, 0);
    new_text_section.sh_addralign = endian::U64::new(LittleEndian, 16);
    new_text_section.sh_entsize = endian::U64::new(LittleEndian, 0);

    // Reload and update file header and size.
    let file_header = load_struct_inplace_mut::<elf::FileHeader64<LittleEndian>>(&mut exec_mmap, 0);
    file_header.e_shoff = endian::U64::new(LittleEndian, new_sh_offset as u64);
    file_header.e_shnum = endian::U16::new(LittleEndian, sh_num + new_section_count as u16);

    // Add 2 new segments that match the new sections.
    let program_headers = load_structs_inplace_mut::<elf::ProgramHeader64<LittleEndian>>(
        &mut exec_mmap,
        ph_offset as usize,
        ph_num as usize,
    );
    let new_rodata_segment = &mut program_headers[program_headers.len() - 2];
    new_rodata_segment.p_type = endian::U32::new(LittleEndian, elf::PT_LOAD);
    new_rodata_segment.p_flags = endian::U32::new(LittleEndian, elf::PF_R);
    new_rodata_segment.p_offset = endian::U64::new(LittleEndian, new_rodata_section_offset as u64);
    new_rodata_segment.p_vaddr = endian::U64::new(LittleEndian, new_rodata_section_vaddr as u64);
    new_rodata_segment.p_paddr = endian::U64::new(LittleEndian, new_rodata_section_vaddr as u64);
    new_rodata_segment.p_filesz = endian::U64::new(LittleEndian, new_rodata_section_size);
    new_rodata_segment.p_memsz = endian::U64::new(LittleEndian, new_rodata_section_virtual_size);
    new_rodata_segment.p_align = endian::U64::new(LittleEndian, md.load_align_constraint);

    let new_text_segment = &mut program_headers[program_headers.len() - 1];
    new_text_segment.p_type = endian::U32::new(LittleEndian, elf::PT_LOAD);
    new_text_segment.p_flags = endian::U32::new(LittleEndian, elf::PF_R | elf::PF_X);
    new_text_segment.p_offset = endian::U64::new(LittleEndian, new_text_section_offset as u64);
    new_text_segment.p_vaddr = endian::U64::new(LittleEndian, new_text_section_vaddr);
    new_text_segment.p_paddr = endian::U64::new(LittleEndian, new_text_section_vaddr);
    new_text_segment.p_filesz = endian::U64::new(LittleEndian, new_text_section_size);
    new_text_segment.p_memsz = endian::U64::new(LittleEndian, new_text_section_size);
    new_text_segment.p_align = endian::U64::new(LittleEndian, md.load_align_constraint);

    // Update calls from platform and dynamic symbols.
    let dynsym_offset = md.dynamic_symbol_table_section_offset + md.added_byte_count;

    for func_name in md.app_functions {
        let func_virt_offset = match app_func_vaddr_map.get(&func_name) {
            Some(offset) => *offset as u64,
            None => {
                println!("Function, {}, was not defined by the app", &func_name);
                return Ok(-1);
            }
        };
        if verbose {
            println!(
                "Updating calls to {} to the address: {:+x}",
                &func_name, func_virt_offset
            );
        }

        for s in md.surgeries.get(&func_name).unwrap_or(&vec![]) {
            if verbose {
                println!("\tPerforming surgery: {:+x?}", s);
            }
            let surgery_virt_offset = match s.virtual_offset {
                VirtualOffset::Relative(vs) => (vs + md.added_byte_count) as i64,
                VirtualOffset::Absolute => 0,
            };
            match s.size {
                4 => {
                    let target = (func_virt_offset as i64 - surgery_virt_offset) as i32;
                    if verbose {
                        println!("\tTarget Jump: {:+x}", target);
                    }
                    let data = target.to_le_bytes();
                    exec_mmap[(s.file_offset + md.added_byte_count) as usize
                        ..(s.file_offset + md.added_byte_count) as usize + 4]
                        .copy_from_slice(&data);
                }
                8 => {
                    let target = func_virt_offset as i64 - surgery_virt_offset;
                    if verbose {
                        println!("\tTarget Jump: {:+x}", target);
                    }
                    let data = target.to_le_bytes();
                    exec_mmap[(s.file_offset + md.added_byte_count) as usize
                        ..(s.file_offset + md.added_byte_count) as usize + 8]
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
            let plt_off = (*plt_off + md.added_byte_count) as usize;
            let plt_vaddr = *plt_vaddr + md.added_byte_count;
            let jmp_inst_len = 5;
            let target =
                (func_virt_offset as i64 - (plt_vaddr as i64 + jmp_inst_len as i64)) as i32;
            if verbose {
                println!("\tPLT: {:+x}, {:+x}", plt_off, plt_vaddr);
                println!("\tTarget Jump: {:+x}", target);
            }
            let data = target.to_le_bytes();
            exec_mmap[plt_off] = 0xE9;
            exec_mmap[plt_off + 1..plt_off + jmp_inst_len].copy_from_slice(&data);
            for i in jmp_inst_len..PLT_ADDRESS_OFFSET as usize {
                exec_mmap[plt_off + i] = 0x90;
            }
        }

        if let Some(i) = md.dynamic_symbol_indices.get(&func_name) {
            let sym = load_struct_inplace_mut::<elf::Sym64<LittleEndian>>(
                &mut exec_mmap,
                dynsym_offset as usize + *i as usize * mem::size_of::<elf::Sym64<LittleEndian>>(),
            );
            sym.st_shndx = endian::U16::new(LittleEndian, new_text_section_index as u16);
            sym.st_value = endian::U64::new(LittleEndian, func_virt_offset as u64);
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
    exec_mmap.flush()?;
    // Also drop files to to ensure data is fully written here.
    drop(exec_mmap);
    exec_file.set_len(offset as u64 + 1)?;
    drop(exec_file);
    let flushing_data_duration = flushing_data_start.elapsed().unwrap();

    // Make sure the final executable has permision to execute.
    let mut perms = fs::metadata(out_filename)?.permissions();
    perms.set_mode(perms.mode() | 0o111);
    fs::set_permissions(out_filename, perms)?;

    let total_duration = total_start.elapsed().unwrap();

    if verbose || time {
        println!();
        println!("Timings");
        report_timing("Loading Metadata", loading_metadata_duration);
        report_timing("Executable Parsing", exec_parsing_duration);
        report_timing("Application Parsing", app_parsing_duration);
        report_timing("Output Generation", out_gen_duration);
        report_timing("Flushing Data to Disk", flushing_data_duration);
        report_timing(
            "Other",
            total_duration
                - loading_metadata_duration
                - exec_parsing_duration
                - app_parsing_duration
                - out_gen_duration
                - flushing_data_duration,
        );
        report_timing("Total", total_duration);
    }
    Ok(0)
}

fn align_by_constraint(offset: usize, constraint: usize) -> usize {
    if offset % constraint == 0 {
        offset
    } else {
        offset + constraint - (offset % constraint)
    }
}

fn align_to_offset_by_constraint(
    current_offset: usize,
    target_offset: usize,
    constraint: usize,
) -> usize {
    let target_remainder = target_offset % constraint;
    let current_remainder = current_offset % constraint;
    match target_remainder.cmp(&current_remainder) {
        Ordering::Greater => current_offset + (target_remainder - current_remainder),
        Ordering::Less => current_offset + ((target_remainder + constraint) - current_remainder),
        Ordering::Equal => current_offset,
    }
}

fn load_struct_inplace<T>(bytes: &[u8], offset: usize) -> &T {
    &load_structs_inplace(bytes, offset, 1)[0]
}

fn load_struct_inplace_mut<T>(bytes: &mut [u8], offset: usize) -> &mut T {
    &mut load_structs_inplace_mut(bytes, offset, 1)[0]
}

fn load_structs_inplace<T>(bytes: &[u8], offset: usize, count: usize) -> &[T] {
    let (head, body, tail) =
        unsafe { bytes[offset..offset + count * mem::size_of::<T>()].align_to::<T>() };
    assert!(head.is_empty(), "Data was not aligned");
    assert_eq!(count, body.len(), "Failed to load all structs");
    assert!(tail.is_empty(), "End of data was not aligned");
    body
}

fn load_structs_inplace_mut<T>(bytes: &mut [u8], offset: usize, count: usize) -> &mut [T] {
    let (head, body, tail) =
        unsafe { bytes[offset..offset + count * mem::size_of::<T>()].align_to_mut::<T>() };
    assert!(head.is_empty(), "Data was not aligned");
    assert_eq!(count, body.len(), "Failed to load all structs");
    assert!(tail.is_empty(), "End of data was not aligned");
    body
}
