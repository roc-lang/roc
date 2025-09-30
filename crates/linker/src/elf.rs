use bincode::{deserialize_from, serialize_into};
use iced_x86::{Decoder, DecoderOptions, Instruction, OpCodeOperandKind, OpKind};
use memmap2::MmapMut;
use object::{elf, endian};
use object::{
    CompressedFileRange, CompressionFormat, LittleEndian as LE, Object, ObjectSection,
    ObjectSymbol, RelocationFlags, RelocationKind, RelocationTarget, Section, SectionIndex,
    SectionKind, Symbol, SymbolIndex, SymbolSection,
};
use roc_collections::all::MutMap;
use roc_error_macros::{internal_error, user_error};
use serde::{Deserialize, Serialize};
use std::{
    ffi::{c_char, CStr},
    io::{BufReader, BufWriter},
    mem,
    path::Path,
    time::Instant,
};

use crate::util::{is_roc_definition, is_roc_undefined, report_timing};
use crate::{
    align_by_constraint, align_to_offset_by_constraint, load_struct_inplace,
    load_struct_inplace_mut, load_structs_inplace_mut, open_mmap, open_mmap_mut,
};

const MIN_SECTION_ALIGNMENT: usize = 0x40;

// TODO: Analyze if this offset is always correct.
const PLT_ADDRESS_OFFSET: u64 = 0x10;

struct ElfDynamicDeps {
    got_app_syms: Vec<(String, usize)>,
    got_sections: Vec<(usize, usize)>,
    app_sym_indices: Vec<usize>,
    dynamic_lib_count: usize,
    shared_lib_index: usize,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
enum VirtualOffset {
    Absolute,
    Relative(u64),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
struct SurgeryEntry {
    file_offset: u64,
    virtual_offset: VirtualOffset,
    size: u8,
}

// TODO: Reanalyze each piece of data in this struct.
// I think a number of them can be combined to reduce string duplication.
// Also I think a few of them aren't need.
// For example, I think preprocessing can deal with all shifting and remove the need for added_byte_count.
// TODO: we probably should be storing numbers in an endian neutral way.
#[derive(Default, Serialize, Deserialize, PartialEq, Eq, Debug)]
struct Metadata {
    app_functions: Vec<String>,
    // offset followed by address.
    plt_addresses: MutMap<String, (u64, u64)>,
    surgeries: MutMap<String, Vec<SurgeryEntry>>,
    dynamic_symbol_indices: MutMap<String, u64>,
    static_symbol_indices: MutMap<String, u64>,
    roc_symbol_vaddresses: MutMap<String, u64>,
    exec_len: u64,
    load_align_constraint: u64,
    added_byte_count: u64,
    last_vaddr: u64,
    dynamic_section_offset: u64,
    dynamic_symbol_table_section_offset: u64,
    symbol_table_section_offset: u64,
    symbol_table_size: u64,
    _macho_cmd_loc: u64,
}

impl Metadata {
    fn write_to_file(&self, metadata_filename: &Path) {
        let metadata_file =
            std::fs::File::create(metadata_filename).unwrap_or_else(|e| internal_error!("{}", e));

        serialize_into(BufWriter::new(metadata_file), self)
            .unwrap_or_else(|err| internal_error!("Failed to serialize metadata: {err}"));
    }

    fn read_from_file(metadata_filename: &Path) -> Self {
        let input = std::fs::File::open(metadata_filename).unwrap_or_else(|e| {
            internal_error!(
                r#"

                Error:
                    {}\n"#,
                e
            )
        });

        match deserialize_from(BufReader::new(input)) {
            Ok(data) => data,
            Err(err) => {
                internal_error!("Failed to deserialize metadata: {}", err);
            }
        }
    }
}

fn collect_roc_definitions<'a>(object: &object::File<'a, &'a [u8]>) -> MutMap<String, u64> {
    let mut vaddresses = MutMap::default();

    for sym in object.symbols().filter(is_roc_definition) {
        // remove potentially trailing "@version".
        let name = sym
            .name()
            .unwrap()
            .trim_start_matches('_')
            .split('@')
            .next()
            .unwrap();

        let address = sym.address();

        // special exceptions for roc_ functions that map to libc symbols
        let direct_mapping = match name {
            "roc_memset" => Some("memset"),
            "roc_memmove" => Some("memmove"),

            // for expects
            "roc_mmap" => Some("mmap"),
            "roc_getppid" => Some("getppid"),
            "roc_shm_open" => Some("shm_open"),

            _ => None,
        };

        if let Some(libc_symbol) = direct_mapping {
            vaddresses.insert(libc_symbol.to_string(), address);
        }

        vaddresses.insert(name.to_string(), address);
    }

    vaddresses
}

struct Surgeries<'a> {
    surgeries: MutMap<String, Vec<SurgeryEntry>>,
    app_func_addresses: MutMap<u64, &'a str>,
    indirect_warning_given: bool,
}

impl<'a> Surgeries<'a> {
    fn new(application_symbols: &[Symbol], app_func_addresses: MutMap<u64, &'a str>) -> Self {
        let mut surgeries = MutMap::default();

        // for each symbol that the host expects from the application
        // we start with an empty set of places to perform surgery
        for symbol in application_symbols {
            let name = symbol.name().unwrap().to_string();
            surgeries.insert(name, vec![]);
        }

        Self {
            surgeries,
            app_func_addresses,
            indirect_warning_given: false,
        }
    }

    fn append_text_sections(
        &mut self,
        object_bytes: &[u8],
        object: &object::File<'a, &'a [u8]>,
        verbose: bool,
    ) {
        let text_sections: Vec<Section> = object
            .sections()
            .filter(|sec| sec.kind() == SectionKind::Text)
            .collect();
        if text_sections.is_empty() {
            internal_error!("No text sections found. This application has no code.");
        }
        if verbose {
            println!();
            println!("Text Sections");
            for sec in text_sections.iter() {
                println!("{sec:+x?}");
            }
        }

        if verbose {
            println!();
            println!("Analyzing instuctions for branches");
        }

        for text_section in text_sections {
            self.append_text_section(object_bytes, &text_section, verbose)
        }
    }

    fn append_text_section(&mut self, object_bytes: &[u8], sec: &Section, verbose: bool) {
        let (file_offset, compressed) = match sec.compressed_file_range() {
            Ok(CompressedFileRange {
                format: CompressionFormat::None,
                offset,
                ..
            }) => (offset, false),
            Ok(range) => (range.offset, true),
            Err(err) => {
                internal_error!(
                    "Issues dealing with section compression for {:+x?}: {}",
                    sec,
                    err
                );
            }
        };

        let data = match sec.uncompressed_data() {
            Ok(data) => data,
            Err(err) => {
                internal_error!("Failed to load text section, {:+x?}: {}", sec, err);
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
                    if let Some(func_name) = self.app_func_addresses.get(&target) {
                        if compressed {
                            internal_error!("Surgical linking does not work with compressed text sections: {:+x?}", sec);
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
                                internal_error!(
                                    "Ran into an unknown operand kind when analyzing branches: {:?}",
                                    op_kind
                                );
                            }
                        };
                        let offset = inst.next_ip() - op_size as u64 - sec.address() + file_offset;
                        if verbose {
                            println!(
                                "\tNeed to surgically replace {op_size} bytes at file offset {offset:+x}",
                            );
                            println!(
                                "\tIts current value is {:+x?}",
                                &object_bytes[offset as usize..(offset + op_size as u64) as usize]
                            )
                        }
                        self.surgeries
                            .get_mut(*func_name)
                            .unwrap()
                            .push(SurgeryEntry {
                                file_offset: offset,
                                virtual_offset: VirtualOffset::Relative(inst.next_ip()),
                                size: op_size,
                            });
                    }
                }
                Ok(OpKind::FarBranch16 | OpKind::FarBranch32) => {
                    internal_error!(
                        "Found branch type instruction that is not yet support: {:+x?}",
                        inst
                    );
                }
                Ok(_) => {
                    if (inst.is_call_far_indirect()
                        || inst.is_call_near_indirect()
                        || inst.is_jmp_far_indirect()
                        || inst.is_jmp_near_indirect())
                        && !self.indirect_warning_given
                        && verbose
                    {
                        self.indirect_warning_given = true;
                        println!();
                        println!("Cannot analyze through indirect jmp type instructions");
                        println!("Most likely this is not a problem, but it could mean a loss in optimizations");
                        println!();
                    }
                }
                Err(err) => {
                    internal_error!("Failed to decode assembly: {}", err);
                }
            }
        }
    }
}

/// Constructs a `Metadata` from a host executable binary, and writes it to disk
pub(crate) fn preprocess_elf_le(
    host_exe_path: &Path,
    metadata_path: &Path,
    preprocessed_path: &Path,
    shared_lib: &Path,
    verbose: bool,
    time: bool,
) {
    let total_start = Instant::now();
    let exec_parsing_start = total_start;
    let exec_data = &*open_mmap(host_exe_path);
    let exec_obj = match object::File::parse(exec_data) {
        Ok(obj) => obj,
        Err(err) => {
            internal_error!("Failed to parse executable file: {}", err);
        }
    };

    let mut md = Metadata {
        roc_symbol_vaddresses: collect_roc_definitions(&exec_obj),
        ..Default::default()
    };

    if verbose {
        println!(
            "Found {} roc symbol definitions:",
            md.roc_symbol_vaddresses.len()
        );

        let (mut builtins, mut other): (Vec<_>, Vec<_>) = md
            .roc_symbol_vaddresses
            .iter()
            .partition(|(n, _)| n.starts_with("roc_builtins"));

        // sort by address
        builtins.sort_by_key(|t| t.1);
        other.sort_by_key(|t| t.1);

        for (name, vaddr) in other.iter() {
            println!("\t{vaddr:#08x}: {name}");
        }

        println!("Of which {} are builtins", builtins.len(),);

        for (name, vaddr) in builtins.iter() {
            println!("\t{vaddr:#08x}: {name}");
        }
    }

    let exec_parsing_duration = exec_parsing_start.elapsed();

    // PLT stands for Procedure Linkage Table which is, put simply, used to call external
    // procedures/functions whose address isn't known in the time of linking, and is left
    // to be resolved by the dynamic linker at run time.
    let symbol_and_plt_processing_start = Instant::now();
    let plt_section_name = ".plt";
    let (plt_address, plt_offset) = match exec_obj.section_by_name(plt_section_name) {
        Some(section) => {
            let file_offset = match section.compressed_file_range() {
                Ok(
                    range @ CompressedFileRange {
                        format: CompressionFormat::None,
                        ..
                    },
                ) => range.offset,
                _ => {
                    internal_error!("Surgical linking does not work with compressed plt section");
                }
            };
            (section.address(), file_offset)
        }
        None => {
            internal_error!("Failed to find PLT section. Probably an malformed executable.");
        }
    };
    if verbose {
        println!("PLT Address: {plt_address:+x}");
        println!("PLT File Offset: {plt_offset:+x}");
    }

    let app_syms: Vec<_> = exec_obj
        .dynamic_symbols()
        .filter(is_roc_undefined)
        .collect();

    let mut app_func_addresses: MutMap<u64, &str> = MutMap::default();

    let plt_relocs = (match exec_obj.dynamic_relocations() {
                Some(relocs) => relocs,
                None => {
                    internal_error!("Executable does not have any dynamic relocations. No work to do. Probably an invalid input.");
                }
            })
            .filter_map(|(_, reloc)| {
                if let RelocationFlags::Elf { r_type}  = reloc.flags() {
                    if r_type == elf::R_X86_64_JUMP_SLOT {
                        Some(reloc)
                    } else {
                        None
                    }
                } else {
                    None
                }
            });
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

    for sym in app_syms.iter() {
        let name = sym.name().unwrap().to_string();
        md.app_functions.push(name.clone());
        md.dynamic_symbol_indices.insert(name, sym.index().0 as u64);
    }
    for sym in exec_obj.symbols().filter(is_roc_undefined) {
        let name = sym.name().unwrap().to_string();
        md.static_symbol_indices.insert(name, sym.index().0 as u64);
    }

    if verbose {
        println!();
        println!("PLT Symbols for App Functions");
        for symbol in app_syms.iter() {
            println!("{}: {:+x?}", symbol.index().0, symbol);
        }

        println!();
        println!("App Function Address Map: {app_func_addresses:+x?}");
    }
    let symbol_and_plt_processing_duration = symbol_and_plt_processing_start.elapsed();

    // look at the text (i.e. code) sections and see collect work needs to be done
    let text_disassembly_start = Instant::now();

    let mut surgeries = Surgeries::new(&app_syms, app_func_addresses);
    surgeries.append_text_sections(exec_data, &exec_obj, verbose);
    md.surgeries = surgeries.surgeries;

    let text_disassembly_duration = text_disassembly_start.elapsed();

    let scanning_dynamic_deps_start = Instant::now();

    let ElfDynamicDeps {
        got_app_syms,
        got_sections,
        app_sym_indices,
        dynamic_lib_count,
        shared_lib_index,
    } = scan_elf_dynamic_deps(
        &exec_obj, &mut md, &app_syms, shared_lib, exec_data, verbose,
    );

    let scanning_dynamic_deps_duration = scanning_dynamic_deps_start.elapsed();

    let platform_gen_start = Instant::now();

    let out_mmap = gen_elf_le(
        exec_data,
        &mut md,
        preprocessed_path,
        &got_app_syms,
        &got_sections,
        &app_sym_indices,
        dynamic_lib_count,
        shared_lib_index,
        verbose,
    );

    let platform_gen_duration = platform_gen_start.elapsed();

    if verbose {
        println!();
        println!("{md:+x?}");
    }

    let saving_metadata_start = Instant::now();
    md.write_to_file(metadata_path);
    let saving_metadata_duration = saving_metadata_start.elapsed();

    let flushing_data_start = Instant::now();
    out_mmap
        .flush()
        .unwrap_or_else(|e| internal_error!("{}", e));
    // Also drop files to to ensure data is fully written here.
    drop(out_mmap);
    let flushing_data_duration = flushing_data_start.elapsed();

    let total_duration = total_start.elapsed();

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
}

#[allow(clippy::too_many_arguments)]
fn gen_elf_le(
    exec_data: &[u8],
    md: &mut Metadata,
    preprocessed_path: &Path,
    got_app_syms: &[(String, usize)],
    got_sections: &[(usize, usize)],
    app_sym_indices: &[usize],
    dynamic_lib_count: usize,
    shared_lib_index: usize,
    verbose: bool,
) -> MmapMut {
    let exec_header = load_struct_inplace::<elf::FileHeader64<LE>>(exec_data, 0);
    let ph_offset = exec_header.e_phoff.get(LE);
    let ph_ent_size = exec_header.e_phentsize.get(LE);
    let ph_num = exec_header.e_phnum.get(LE);
    let sh_offset = exec_header.e_shoff.get(LE);
    let sh_ent_size = exec_header.e_shentsize.get(LE);
    let sh_num = exec_header.e_shnum.get(LE);

    if verbose {
        println!();
        println!("PH Offset: {ph_offset:+x}");
        println!("PH Entry Size: {ph_ent_size}");
        println!("PH Entry Count: {ph_num}");
        println!("SH Offset: {sh_offset:+x}");
        println!("SH Entry Size: {sh_ent_size}");
        println!("SH Entry Count: {sh_num}");
    }

    // Copy header and shift everything to enable more program sections.
    let added_header_count = 3;
    md.added_byte_count = ph_ent_size as u64 * added_header_count;
    md.added_byte_count = md.added_byte_count
        + (MIN_SECTION_ALIGNMENT as u64 - md.added_byte_count % MIN_SECTION_ALIGNMENT as u64);
    let ph_end = ph_offset as usize + ph_num as usize * ph_ent_size as usize;
    let physical_shift_start = ph_end as u64;

    md.exec_len = exec_data.len() as u64 + md.added_byte_count;
    let mut out_mmap = open_mmap_mut(preprocessed_path, md.exec_len as usize);

    out_mmap[..ph_end].copy_from_slice(&exec_data[..ph_end]);

    let program_headers = load_structs_inplace_mut::<elf::ProgramHeader64<LE>>(
        &mut out_mmap,
        ph_offset as usize,
        ph_num as usize,
    );
    let mut first_load_found = false;
    let mut virtual_shift_start = 0;
    for ph in program_headers.iter() {
        let p_type = ph.p_type.get(LE);
        if p_type == elf::PT_LOAD && ph.p_offset.get(LE) == 0 {
            first_load_found = true;
            md.load_align_constraint = ph.p_align.get(LE);
            virtual_shift_start = physical_shift_start + ph.p_vaddr.get(LE);
        }
    }
    if !first_load_found {
        user_error!("Executable does not load any data at 0x00000000\nProbably input the wrong file as the executable");
    }
    if verbose {
        println!("Shifting all data after: {physical_shift_start:+x}({virtual_shift_start:+x})");
    }

    // Shift all of the program headers.
    for ph in program_headers.iter_mut() {
        let p_type = ph.p_type.get(LE);
        let p_offset = ph.p_offset.get(LE);
        if (p_type == elf::PT_LOAD && p_offset == 0) || p_type == elf::PT_PHDR {
            // Extend length for the first segment and the program header.
            ph.p_filesz
                .set(LE, ph.p_filesz.get(LE) + md.added_byte_count);
            ph.p_memsz.set(LE, ph.p_memsz.get(LE) + md.added_byte_count);
        } else {
            // Shift if needed.
            if physical_shift_start <= p_offset {
                ph.p_offset.set(LE, p_offset + md.added_byte_count);
            }
            let p_vaddr = ph.p_vaddr.get(LE);
            if virtual_shift_start <= p_vaddr {
                ph.p_vaddr.set(LE, p_vaddr + md.added_byte_count);
                ph.p_paddr.set(LE, p_vaddr + md.added_byte_count);
            }
        }
    }

    // Get last segment virtual address.
    let last_segment_vaddr = program_headers
        .iter()
        .filter_map(|ph| {
            if ph.p_type.get(LE) != elf::PT_GNU_STACK {
                Some(ph.p_vaddr.get(LE) + ph.p_memsz.get(LE))
            } else {
                None
            }
        })
        .max()
        .unwrap();

    // Copy the rest of the file shifted as needed.
    out_mmap[physical_shift_start as usize + md.added_byte_count as usize..]
        .copy_from_slice(&exec_data[physical_shift_start as usize..]);

    // Update all sections for shift for extra program headers.
    let section_headers = load_structs_inplace_mut::<elf::SectionHeader64<LE>>(
        &mut out_mmap,
        sh_offset as usize + md.added_byte_count as usize,
        sh_num as usize,
    );

    let mut rel_sections: Vec<(u64, u64)> = vec![];
    let mut rela_sections: Vec<(usize, u64, u64)> = vec![];
    for (i, sh) in section_headers.iter_mut().enumerate() {
        let sh_offset = sh.sh_offset.get(LE);
        let sh_addr = sh.sh_addr.get(LE);
        if physical_shift_start <= sh_offset {
            sh.sh_offset.set(LE, sh_offset + md.added_byte_count);
        }
        if virtual_shift_start <= sh_addr {
            sh.sh_addr.set(LE, sh_addr + md.added_byte_count);
        }

        // Record every relocation section.
        let sh_type = sh.sh_type.get(LE);
        if sh_type == elf::SHT_REL {
            rel_sections.push((sh_offset, sh.sh_size.get(LE)));
        } else if sh_type == elf::SHT_RELA {
            rela_sections.push((i, sh_offset, sh.sh_size.get(LE)));
        }
    }

    // Get last section virtual address.
    let last_section_vaddr = section_headers
        .iter()
        .map(|sh| sh.sh_addr.get(LE) + sh.sh_size.get(LE))
        .max()
        .unwrap();

    // Calculate end virtual address for new segment.
    // TODO: potentially remove md.load_align_constraint here. I think we should be able to cram things together.
    md.last_vaddr =
        std::cmp::max(last_section_vaddr, last_segment_vaddr) + md.load_align_constraint;

    // Update all relocations for shift for extra program headers.
    for (sec_offset, sec_size) in rel_sections {
        let relocations = load_structs_inplace_mut::<elf::Rel64<LE>>(
            &mut out_mmap,
            sec_offset as usize + md.added_byte_count as usize,
            sec_size as usize / mem::size_of::<elf::Rel64<LE>>(),
        );
        for rel in relocations.iter_mut() {
            let r_offset = rel.r_offset.get(LE);
            if virtual_shift_start <= r_offset {
                rel.r_offset.set(LE, r_offset + md.added_byte_count);
            }
        }
    }

    let dyn_offset = md.dynamic_section_offset + md.added_byte_count;
    for (sec_index, sec_offset, sec_size) in rela_sections {
        let relocations = load_structs_inplace_mut::<elf::Rela64<LE>>(
            &mut out_mmap,
            sec_offset as usize + md.added_byte_count as usize,
            sec_size as usize / mem::size_of::<elf::Rela64<LE>>(),
        );
        for (i, rel) in relocations.iter_mut().enumerate() {
            let r_offset = rel.r_offset.get(LE);
            if virtual_shift_start <= r_offset {
                rel.r_offset.set(LE, r_offset + md.added_byte_count);
                // Deal with potential adjusts to absolute jumps.
                // TODO: Verify other relocation types.
                if rel.r_type(LE, false) == elf::R_X86_64_RELATIVE {
                    let r_addend = rel.r_addend.get(LE);
                    rel.r_addend.set(LE, r_addend + md.added_byte_count as i64);
                }
            }
            // If the relocation goes to a roc function, we need to surgically link it and change it to relative.
            let r_type = rel.r_type(LE, false);
            if r_type == elf::R_X86_64_GLOB_DAT {
                let r_sym = rel.r_sym(LE, false);
                for (name, index) in got_app_syms.iter() {
                    if *index as u32 == r_sym {
                        rel.set_r_info(LE, false, 0, elf::R_X86_64_RELATIVE);
                        let addend_addr = sec_offset as usize
                            + i * mem::size_of::<elf::Rela64<LE>>()
                            // This 16 skips the first 2 fields and gets to the addend field.
                            + 16;
                        md.surgeries.get_mut(name).unwrap().push(SurgeryEntry {
                            file_offset: addend_addr as u64,
                            virtual_offset: VirtualOffset::Absolute,
                            size: 8,
                        });
                    }
                }
            }
        }
        // To correctly remove the JUMP_SLOT relocations for Roc functions we:
        //     1. collect the indices of all of them.
        //     2. move them all to the end of the relocation sections.
        //     3. shrink the relocation section to ignore them.
        //     4. update the dynamic section to reflect the shrink as well.
        let mut to_remove = relocations
            .iter()
            .enumerate()
            .filter_map(|(i, rel)| {
                let r_type = rel.r_type(LE, false);
                let r_sym = rel.r_sym(LE, false);
                if r_type == elf::R_X86_64_JUMP_SLOT && app_sym_indices.contains(&(r_sym as usize))
                {
                    Some(i)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        // We must remove in descending order to avoid swapping an element more than once and messing up the removal.
        to_remove.sort();
        to_remove.reverse();

        let mut j = relocations.len() - 1;
        for i in to_remove.iter() {
            relocations.swap(*i, j);
            let r_sym = relocations[j].r_sym(LE, false);
            relocations[j].set_r_info(LE, false, r_sym, elf::R_X86_64_NONE);
            j -= 1;
        }

        let section_headers = load_structs_inplace_mut::<elf::SectionHeader64<LE>>(
            &mut out_mmap,
            sh_offset as usize + md.added_byte_count as usize,
            sh_num as usize,
        );

        let old_size = section_headers[sec_index].sh_size.get(LE);
        let removed_count = to_remove.len();
        let removed_size = removed_count * std::mem::size_of::<elf::Rela64<LE>>();
        section_headers[sec_index]
            .sh_size
            .set(LE, old_size - removed_size as u64);

        let dyns = load_structs_inplace_mut::<elf::Dyn64<LE>>(
            &mut out_mmap,
            dyn_offset as usize,
            dynamic_lib_count,
        );
        let is_rela_dyn = dyns
            .iter()
            .filter(|d| {
                let tag = d.d_tag.get(LE) as u32;
                tag == elf::DT_RELA
            })
            .any(|d| d.d_val.get(LE) == sec_offset);
        let is_rela_plt = dyns
            .iter()
            .filter(|d| {
                let tag = d.d_tag.get(LE) as u32;
                tag == elf::DT_JMPREL
            })
            .any(|d| d.d_val.get(LE) == sec_offset);

        for d in dyns.iter_mut() {
            match d.d_tag.get(LE) as u32 {
                elf::DT_RELACOUNT if is_rela_dyn => {
                    let old_count = d.d_val.get(LE);
                    d.d_val.set(LE, old_count - removed_count as u64);
                }
                elf::DT_RELASZ if is_rela_dyn => {
                    let old_size = d.d_val.get(LE);
                    d.d_val.set(LE, old_size - removed_size as u64);
                }
                elf::DT_PLTRELSZ if is_rela_plt => {
                    let old_size = d.d_val.get(LE);
                    d.d_val.set(LE, old_size - removed_size as u64);
                }
                _ => {}
            }
        }
    }

    // Update dynamic table entries for shift for extra program headers.
    let dyns = load_structs_inplace_mut::<elf::Dyn64<LE>>(
        &mut out_mmap,
        dyn_offset as usize,
        dynamic_lib_count,
    );
    for d in dyns {
        match d.d_tag.get(LE) as u32 {
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
                let d_addr = d.d_val.get(LE);
                if virtual_shift_start <= d_addr {
                    d.d_val.set(LE, d_addr + md.added_byte_count);
                }
            }
            _ => {}
        }
    }

    // Update symbol table entries for shift for extra program headers.
    let symtab_offset = md.symbol_table_section_offset + md.added_byte_count;
    let symtab_size = md.symbol_table_size as usize;

    let symbols = load_structs_inplace_mut::<elf::Sym64<LE>>(
        &mut out_mmap,
        symtab_offset as usize,
        symtab_size / mem::size_of::<elf::Sym64<LE>>(),
    );

    for sym in symbols {
        let addr = sym.st_value.get(LE);
        if virtual_shift_start <= addr {
            sym.st_value.set(LE, addr + md.added_byte_count);
        }
    }

    // Update all data in the global offset table.
    for (offset, size) in got_sections {
        let global_offsets = load_structs_inplace_mut::<endian::U64<LE>>(
            &mut out_mmap,
            *offset + md.added_byte_count as usize,
            size / mem::size_of::<endian::U64<LE>>(),
        );
        for go in global_offsets.iter_mut() {
            let go_addr = go.get(LE);
            if physical_shift_start <= go_addr {
                go.set(LE, go_addr + md.added_byte_count);
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
    let file_header = load_struct_inplace_mut::<elf::FileHeader64<LE>>(&mut out_mmap, 0);
    file_header
        .e_shoff
        .set(LE, file_header.e_shoff.get(LE) + md.added_byte_count);
    let e_entry = file_header.e_entry.get(LE);
    if virtual_shift_start <= e_entry {
        file_header.e_entry.set(LE, e_entry + md.added_byte_count);
    }
    file_header
        .e_phnum
        .set(LE, ph_num + added_header_count as u16);

    out_mmap
}

fn scan_elf_dynamic_deps(
    exec_obj: &object::File,
    md: &mut Metadata,
    app_syms: &[Symbol],
    shared_lib: &Path,
    exec_data: &[u8],
    verbose: bool,
) -> ElfDynamicDeps {
    let dyn_sec = match exec_obj.section_by_name(".dynamic") {
        Some(sec) => sec,
        None => {
            panic!("There must be a dynamic section in the executable");
        }
    };
    let dyn_offset = match dyn_sec.compressed_file_range() {
        Ok(
            range @ CompressedFileRange {
                format: CompressionFormat::None,
                ..
            },
        ) => range.offset as usize,
        _ => {
            panic!("Surgical linking does not work with compressed dynamic section");
        }
    };
    md.dynamic_section_offset = dyn_offset as u64;

    let dynstr_sec = match exec_obj.section_by_name(".dynstr") {
        Some(sec) => sec,
        None => {
            panic!("There must be a dynstr section in the executable");
        }
    };
    let dynstr_data = match dynstr_sec.uncompressed_data() {
        Ok(data) => data,
        Err(err) => {
            panic!("Failed to load dynstr section: {err}");
        }
    };

    let shared_lib_filename = shared_lib.file_name();

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
            let c_buf = dynstr_data[dynstr_off..].as_ptr() as *const c_char;
            let c_str = unsafe { CStr::from_ptr(c_buf) }.to_str().unwrap();
            if Path::new(c_str).file_name() == shared_lib_filename {
                shared_lib_index = Some(dyn_lib_index);
                if verbose {
                    println!("Found shared lib in dynamic table at index: {dyn_lib_index}");
                }
            }
        }

        dyn_lib_index += 1;
    }
    let dynamic_lib_count = dyn_lib_index;

    if shared_lib_index.is_none() {
        panic!("Shared lib not found as a dependency of the executable");
    }
    let shared_lib_index = shared_lib_index.unwrap();

    let symtab_sec = match exec_obj.section_by_name(".symtab") {
        Some(sec) => sec,
        None => {
            panic!("There must be a symtab section in the executable");
        }
    };
    let symtab_offset = match symtab_sec.compressed_file_range() {
        Ok(
            range @ CompressedFileRange {
                format: CompressionFormat::None,
                ..
            },
        ) => range.offset as usize,
        _ => {
            panic!("Surgical linking does not work with compressed symtab section");
        }
    };
    md.symbol_table_section_offset = symtab_offset as u64;
    md.symbol_table_size = symtab_sec.size();

    let dynsym_sec = match exec_obj.section_by_name(".dynsym") {
        Some(sec) => sec,
        None => {
            panic!("There must be a dynsym section in the executable");
        }
    };
    let dynsym_offset = match dynsym_sec.compressed_file_range() {
        Ok(
            range @ CompressedFileRange {
                format: CompressionFormat::None,
                ..
            },
        ) => range.offset as usize,
        _ => {
            panic!("Surgical linking does not work with compressed dynsym section");
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
                range @ CompressedFileRange {
                    format: CompressionFormat::None,
                    ..
                },
            ) => got_sections.push((range.offset as usize, range.uncompressed_size as usize)),
            _ => {
                panic!("Surgical linking does not work with compressed got sections");
            }
        }
    }

    let got_app_syms: Vec<(String, usize)> = (match exec_obj.dynamic_relocations() {
        Some(relocs) => relocs,
        None => {
            eprintln!("Executable never calls any application functions.");
            panic!("No work to do. Probably an invalid input.");
        }
    })
    .filter_map(|(_, reloc)| {
        if let RelocationFlags::Elf { r_type } = reloc.flags() {
            if r_type == elf::R_X86_64_GLOB_DAT {
                for symbol in app_syms.iter() {
                    if reloc.target() == RelocationTarget::Symbol(symbol.index()) {
                        return Some((symbol.name().unwrap().to_string(), symbol.index().0));
                    }
                }
            }
        }
        None
    })
    .collect();

    let app_sym_indices: Vec<usize> = (match exec_obj.dynamic_relocations() {
        Some(relocs) => relocs,
        None => {
            eprintln!("Executable never calls any application functions.");
            panic!("No work to do. Probably an invalid input.");
        }
    })
    .filter_map(|(_, reloc)| {
        if let RelocationFlags::Elf { r_type } = reloc.flags() {
            if r_type == elf::R_X86_64_JUMP_SLOT {
                for symbol in app_syms.iter() {
                    if reloc.target() == RelocationTarget::Symbol(symbol.index()) {
                        return Some(symbol.index().0);
                    }
                }
            }
        }
        None
    })
    .collect();

    ElfDynamicDeps {
        got_app_syms,
        got_sections,
        app_sym_indices,
        dynamic_lib_count,
        shared_lib_index,
    }
}

pub(crate) fn surgery_elf(
    roc_app_bytes: &[u8],
    metadata_path: &Path,
    executable_path: &Path,
    verbose: bool,
    time: bool,
) {
    let app_obj = match object::File::parse(roc_app_bytes) {
        Ok(obj) => obj,
        Err(err) => {
            internal_error!("Failed to parse application file: {}", err);
        }
    };

    if app_obj
        .sections()
        .filter(|sec| {
            let name = sec.name().unwrap_or_default();
            !name.starts_with(".debug") && !name.starts_with(".eh")
        })
        .flat_map(|sec| sec.relocations())
        .any(|(_, reloc)| reloc.kind() == RelocationKind::Absolute)
    {
        eprintln!("The surgical linker currently has issue #3609 and would fail linking your app.");
        eprintln!("Please use `--linker=legacy` to avoid the issue for now.");
        std::process::exit(1);
    }

    let total_start = Instant::now();

    let loading_metadata_start = total_start;
    let md = Metadata::read_from_file(metadata_path);
    let loading_metadata_duration = loading_metadata_start.elapsed();

    let load_and_mmap_start = Instant::now();
    let max_out_len = md.exec_len + roc_app_bytes.len() as u64 + md.load_align_constraint;
    let mut exec_mmap = open_mmap_mut(executable_path, max_out_len as usize);
    let load_and_mmap_duration = load_and_mmap_start.elapsed();

    let out_gen_start = Instant::now();
    let mut offset = 0;

    surgery_elf_help(verbose, &md, &mut exec_mmap, &mut offset, app_obj);

    let out_gen_duration = out_gen_start.elapsed();
    let flushing_data_start = Instant::now();

    // TODO investigate using the async version of flush - might be faster due to not having to block on that
    exec_mmap
        .flush()
        .unwrap_or_else(|e| internal_error!("{}", e));
    // Also drop files to to ensure data is fully written here.
    drop(exec_mmap);

    let flushing_data_duration = flushing_data_start.elapsed();

    // Make sure the final executable has permision to execute.
    #[cfg(target_family = "unix")]
    {
        use std::fs;
        use std::os::unix::fs::PermissionsExt;

        let mut perms = fs::metadata(executable_path)
            .unwrap_or_else(|e| internal_error!("{}", e))
            .permissions();
        perms.set_mode(perms.mode() | 0o111);
        fs::set_permissions(executable_path, perms).unwrap_or_else(|e| internal_error!("{}", e));
    }

    let total_duration = total_start.elapsed();

    if verbose || time {
        println!("\nTimings");
        report_timing("Loading Metadata", loading_metadata_duration);
        report_timing("Loading and mmap-ing", load_and_mmap_duration);
        report_timing("Output Generation", out_gen_duration);
        report_timing("Flushing Data to Disk", flushing_data_duration);

        let sum = loading_metadata_duration
            + load_and_mmap_duration
            + out_gen_duration
            + flushing_data_duration;

        report_timing("Other", total_duration.saturating_sub(sum));
        report_timing("Total", total_duration);
    }
}

fn surgery_elf_help(
    verbose: bool,
    md: &Metadata,
    exec_mmap: &mut MmapMut,
    offset_ref: &mut usize, // TODO return this instead of taking a mutable reference to it
    app_obj: object::File,
) {
    let elf64 = exec_mmap[4] == 2;
    let litte_endian = exec_mmap[5] == 1;
    if !elf64 || !litte_endian {
        internal_error!("Only 64bit little endian elf currently supported for surgery");
    }
    let exec_header = load_struct_inplace::<elf::FileHeader64<LE>>(exec_mmap, 0);

    let ph_offset = exec_header.e_phoff.get(LE);
    let ph_ent_size = exec_header.e_phentsize.get(LE);
    let ph_num = exec_header.e_phnum.get(LE);
    let sh_offset = exec_header.e_shoff.get(LE);
    let sh_ent_size = exec_header.e_shentsize.get(LE);
    let sh_num = exec_header.e_shnum.get(LE);

    if verbose {
        println!();
        println!("Is Elf64: {elf64}");
        println!("Is Little Endian: {litte_endian}");
        println!("PH Offset: {ph_offset:+x}");
        println!("PH Entry Size: {ph_ent_size}");
        println!("PH Entry Count: {ph_num}");
        println!("SH Offset: {sh_offset:+x}");
        println!("SH Entry Size: {sh_ent_size}");
        println!("SH Entry Count: {sh_num}");
    }

    // Backup section header table.
    let sh_size = sh_ent_size as usize * sh_num as usize;
    let sh_tab = exec_mmap[sh_offset as usize..][..sh_size].to_vec();

    let mut offset = sh_offset as usize;
    offset = align_by_constraint(offset, MIN_SECTION_ALIGNMENT);

    // Align physical and virtual address of new segment.
    let mut virt_offset = align_to_offset_by_constraint(
        md.last_vaddr as usize,
        offset,
        md.load_align_constraint as usize,
    );

    // First decide on sections locations and then recode every exact symbol locations.

    // TODO: In the future Roc may use a data section to store memoized toplevel thunks
    // in development builds for caching the results of top-level constants
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
        internal_error!("No text sections found. This application has no code.");
    }

    // Copy sections and resolve their symbols/relocations.
    let symbols = app_obj.symbols().collect::<Vec<Symbol>>();
    let mut section_offset_map: MutMap<SectionIndex, (usize, usize)> = MutMap::default();
    let mut symbol_vaddr_map: MutMap<SymbolIndex, usize> = MutMap::default();
    let mut app_func_vaddr_map: MutMap<String, usize> = MutMap::default();
    let mut app_func_size_map: MutMap<String, u64> = MutMap::default();

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
            internal_error!( "We do not deal with non bss sections that have different on disk and in memory sizes");
        } else {
            offset += section_size as usize;
            virt_offset += sec.size() as usize;
        }
    }
    if verbose {
        println!("Data Relocation Offsets: {symbol_vaddr_map:+x?}");
        println!("Found App Function Symbols: {app_func_vaddr_map:+x?}");
    }

    let (new_text_section_offset, new_text_section_vaddr) = text_sections
        .iter()
        .map(|sec| section_offset_map.get(&sec.index()).unwrap())
        .min()
        .unwrap();
    let (new_text_section_offset, new_text_section_vaddr) = (
        *new_text_section_offset as u64,
        *new_text_section_vaddr as u64,
    );
    // BSS section is not guaranteed to exist.
    // If it doesn't exist, just use the text section offset.
    // This will make a bss section of size 0.
    let bss_default = (
        new_text_section_offset as usize,
        new_text_section_vaddr as usize,
    );
    let (new_bss_section_offset, new_bss_section_vaddr) = bss_sections
        .iter()
        .map(|sec| section_offset_map.get(&sec.index()).unwrap())
        .min()
        .unwrap_or(&bss_default);
    let (new_bss_section_offset, new_bss_section_vaddr) = (
        *new_bss_section_offset as u64,
        *new_bss_section_vaddr as u64,
    );

    // rodata section is not guaranteed to exist.
    // If it doesn't exist, just use the bss section offset.
    // This will make a rodata section of size 0.
    let rodata_default = (
        new_bss_section_offset as usize,
        new_bss_section_vaddr as usize,
    );
    let (new_rodata_section_offset, new_rodata_section_vaddr) = rodata_sections
        .iter()
        .map(|sec| section_offset_map.get(&sec.index()).unwrap())
        .min()
        .unwrap_or(&rodata_default);
    let (new_rodata_section_offset, new_rodata_section_vaddr) = (
        *new_rodata_section_offset as u64,
        *new_rodata_section_vaddr as u64,
    );

    // Move data and deal with relocations.
    for sec in rodata_sections
        .iter()
        .chain(bss_sections.iter())
        .chain(text_sections.iter())
    {
        let data = sec.data().unwrap_or_else(|err| {
            internal_error!(
                "Failed to load data for section, {:+x?}: {err}",
                sec.name().unwrap(),
            )
        });
        let (section_offset, section_virtual_offset) =
            section_offset_map.get(&sec.index()).unwrap();
        let (section_offset, section_virtual_offset) = (*section_offset, *section_virtual_offset);
        exec_mmap[section_offset..][..data.len()].copy_from_slice(data);
        // Deal with definitions and relocations for this section.
        if verbose {
            println!();
            println!(
                "Processing Relocations for Section: 0x{sec:+x?} @ {section_offset:+x} (virt: {section_virtual_offset:+x})"
            );
        }
        for rel in sec.relocations() {
            if verbose {
                println!("\tFound Relocation: {rel:+x?}");
            }
            match rel.1.target() {
                RelocationTarget::Symbol(index) => {
                    let target_offset = if let Some(target_offset) = symbol_vaddr_map.get(&index) {
                        if verbose {
                            println!("\t\tRelocation targets symbol in app at: {target_offset:+x}");
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
                                            "\t\tRelocation targets symbol in host: {name} @ {vaddr:+x}"
                                        );
                                    }
                                    vaddr
                                })
                            })
                    };

                    if let Some(target_offset) = target_offset {
                        let virt_base = section_virtual_offset + rel.0 as usize;
                        let base = section_offset + rel.0 as usize;
                        let target: i64 = match rel.1.kind() {
                            RelocationKind::Relative | RelocationKind::PltRelative => {
                                target_offset - virt_base as i64 + rel.1.addend()
                            }
                            x => {
                                internal_error!("Relocation Kind not yet support: {:?}", x);
                            }
                        };
                        if verbose {
                            println!(
                                "\t\tRelocation base location: {base:+x} (virt: {virt_base:+x})",
                            );
                            println!("\t\tFinal relocation target offset: {target:+x}");
                        }
                        match rel.1.size() {
                            32 => {
                                let data = (target as i32).to_le_bytes();
                                exec_mmap[base..][..4].copy_from_slice(&data);
                            }
                            64 => {
                                let data = target.to_le_bytes();
                                exec_mmap[base..][..8].copy_from_slice(&data);
                            }
                            other => {
                                internal_error!("Relocation size not yet supported: {other}");
                            }
                        }
                    } else {
                        internal_error!(
                            "Undefined Symbol in relocation, {:+x?}: {:+x?}\n\nTIPS:\n\t- try compiling with `--linker legacy`\n\t- If you're building a platform from source and you added functionality, you may have forgotten to rebuild the platform before running.",
                            rel,
                            app_obj.symbol_by_index(index)
                        );
                    }
                }

                _ => {
                    internal_error!("Relocation target not yet support: {:+x?}", rel);
                }
            }
        }
    }

    offset = align_by_constraint(offset, MIN_SECTION_ALIGNMENT);
    let new_sh_offset = offset;
    exec_mmap[offset..][..sh_size].copy_from_slice(&sh_tab);
    offset += sh_size;

    // Flush app only data to speed up write to disk.
    exec_mmap
        .flush_async_range(
            new_rodata_section_offset as usize,
            offset - new_rodata_section_offset as usize,
        )
        .unwrap_or_else(|e| internal_error!("{}", e));

    // TODO: look into merging symbol tables, debug info, and eh frames to enable better debugger experience.

    // Add 3 new sections and segments.
    let new_section_count = 3;
    offset += new_section_count * sh_ent_size as usize;
    let section_headers = load_structs_inplace_mut::<elf::SectionHeader64<LE>>(
        exec_mmap,
        new_sh_offset,
        sh_num as usize + new_section_count,
    );

    let new_rodata_section_size = new_text_section_offset - new_rodata_section_offset;
    let new_bss_section_virtual_size = new_text_section_vaddr - new_bss_section_vaddr;
    let new_text_section_size = new_sh_offset as u64 - new_text_section_offset;

    // set the new rodata section header
    section_headers[section_headers.len() - 3] = elf::SectionHeader64 {
        sh_name: endian::U32::new(LE, 0),
        sh_type: endian::U32::new(LE, elf::SHT_PROGBITS),
        sh_flags: endian::U64::new(LE, elf::SHF_ALLOC as u64),
        sh_addr: endian::U64::new(LE, new_rodata_section_vaddr),
        sh_offset: endian::U64::new(LE, new_rodata_section_offset),
        sh_size: endian::U64::new(LE, new_rodata_section_size),
        sh_link: endian::U32::new(LE, 0),
        sh_info: endian::U32::new(LE, 0),
        sh_addralign: endian::U64::new(LE, 16),
        sh_entsize: endian::U64::new(LE, 0),
    };

    // set the new bss section header
    section_headers[section_headers.len() - 2] = elf::SectionHeader64 {
        sh_name: endian::U32::new(LE, 0),
        sh_type: endian::U32::new(LE, elf::SHT_NOBITS),
        sh_flags: endian::U64::new(LE, (elf::SHF_ALLOC) as u64),
        sh_addr: endian::U64::new(LE, new_bss_section_vaddr),
        sh_offset: endian::U64::new(LE, new_bss_section_offset),
        sh_size: endian::U64::new(LE, new_bss_section_virtual_size),
        sh_link: endian::U32::new(LE, 0),
        sh_info: endian::U32::new(LE, 0),
        sh_addralign: endian::U64::new(LE, 16),
        sh_entsize: endian::U64::new(LE, 0),
    };

    // set the new text section header
    section_headers[section_headers.len() - 1] = elf::SectionHeader64 {
        sh_name: endian::U32::new(LE, 0),
        sh_type: endian::U32::new(LE, elf::SHT_PROGBITS),
        sh_flags: endian::U64::new(LE, (elf::SHF_ALLOC | elf::SHF_EXECINSTR) as u64),
        sh_addr: endian::U64::new(LE, new_text_section_vaddr),
        sh_offset: endian::U64::new(LE, new_text_section_offset),
        sh_size: endian::U64::new(LE, new_text_section_size),
        sh_link: endian::U32::new(LE, 0),
        sh_info: endian::U32::new(LE, 0),
        sh_addralign: endian::U64::new(LE, 16),
        sh_entsize: endian::U64::new(LE, 0),
    };

    // Reload and update file header and size.
    let file_header = load_struct_inplace_mut::<elf::FileHeader64<LE>>(exec_mmap, 0);
    file_header.e_shoff.set(LE, new_sh_offset as u64);
    file_header
        .e_shnum
        .set(LE, sh_num + new_section_count as u16);

    // Add 2 new segments that match the new sections.
    let program_headers = load_structs_inplace_mut::<elf::ProgramHeader64<LE>>(
        exec_mmap,
        ph_offset as usize,
        ph_num as usize,
    );

    // set the new rodata section program header
    program_headers[program_headers.len() - 3] = elf::ProgramHeader64 {
        p_type: endian::U32::new(LE, elf::PT_LOAD),
        p_flags: endian::U32::new(LE, elf::PF_R),
        p_offset: endian::U64::new(LE, new_rodata_section_offset),
        p_vaddr: endian::U64::new(LE, new_rodata_section_vaddr),
        p_paddr: endian::U64::new(LE, new_rodata_section_vaddr),
        p_filesz: endian::U64::new(LE, new_rodata_section_size),
        p_memsz: endian::U64::new(LE, new_rodata_section_size),
        p_align: endian::U64::new(LE, md.load_align_constraint),
    };

    // set the new bss section program header
    program_headers[program_headers.len() - 2] = elf::ProgramHeader64 {
        p_type: endian::U32::new(LE, elf::PT_LOAD),
        p_flags: endian::U32::new(LE, elf::PF_R | elf::PF_W),
        p_offset: endian::U64::new(LE, new_bss_section_offset),
        p_vaddr: endian::U64::new(LE, new_bss_section_vaddr),
        p_paddr: endian::U64::new(LE, new_bss_section_vaddr),
        p_filesz: endian::U64::new(LE, 0),
        p_memsz: endian::U64::new(LE, new_bss_section_virtual_size),
        p_align: endian::U64::new(LE, md.load_align_constraint),
    };

    // set the new text section program header
    let new_text_section_index = program_headers.len() - 1;
    program_headers[new_text_section_index] = elf::ProgramHeader64 {
        p_type: endian::U32::new(LE, elf::PT_LOAD),
        p_flags: endian::U32::new(LE, elf::PF_R | elf::PF_X),
        p_offset: endian::U64::new(LE, new_text_section_offset),
        p_vaddr: endian::U64::new(LE, new_text_section_vaddr),
        p_paddr: endian::U64::new(LE, new_text_section_vaddr),
        p_filesz: endian::U64::new(LE, new_text_section_size),
        p_memsz: endian::U64::new(LE, new_text_section_size),
        p_align: endian::U64::new(LE, md.load_align_constraint),
    };

    // Update calls from platform and dynamic symbols.
    let dynsym_offset = md.dynamic_symbol_table_section_offset + md.added_byte_count;
    let symtab_offset = md.symbol_table_section_offset + md.added_byte_count;

    for func_name in md.app_functions.iter() {
        let func_virt_offset = match app_func_vaddr_map.get(func_name) {
            Some(offset) => *offset as u64,
            None => {
                eprintln!("Error:");
                eprintln!("\n\tFunction, {}, was not defined by the app.", &func_name);
                eprintln!("\nPotential causes:");
                eprintln!("\n\t- because the platform was built with a non-compatible version of roc compared to the one you are running.");
                eprintln!("\n\t\tsolutions:");
                eprintln!("\t\t\t+ Downgrade your roc version to the one that was used to build the platform.");
                eprintln!("\t\t\t+ Or ask the platform author to release a new version of the platform using a current roc release.");
                eprintln!("\n\t- This can also occur due to a bug in the compiler. In that case, file an issue here: https://github.com/roc-lang/roc/issues/new/choose");

                std::process::exit(1);
            }
        };
        if verbose {
            println!(
                "Updating calls to {} to the address: {:+x}",
                &func_name, func_virt_offset
            );
        }

        for s in md.surgeries.get(func_name).unwrap_or(&vec![]) {
            if verbose {
                println!("\tPerforming surgery: {s:+x?}");
            }
            let surgery_virt_offset = match s.virtual_offset {
                VirtualOffset::Relative(vs) => (vs + md.added_byte_count) as i64,
                VirtualOffset::Absolute => 0,
            };
            match s.size {
                4 => {
                    let target = (func_virt_offset as i64 - surgery_virt_offset) as i32;
                    if verbose {
                        println!("\tTarget Jump: {target:+x}");
                    }
                    let data = target.to_le_bytes();
                    exec_mmap[(s.file_offset + md.added_byte_count) as usize..][..4]
                        .copy_from_slice(&data);
                }
                8 => {
                    let target = func_virt_offset as i64 - surgery_virt_offset;
                    if verbose {
                        println!("\tTarget Jump: {target:+x}");
                    }
                    let data = target.to_le_bytes();
                    exec_mmap[(s.file_offset + md.added_byte_count) as usize..][..8]
                        .copy_from_slice(&data);
                }
                x => {
                    internal_error!("Surgery size not yet supported: {}", x);
                }
            }
        }

        // Replace plt call code with just a jump.
        // This is a backup incase we missed a call to the plt.
        if let Some((plt_off, plt_vaddr)) = md.plt_addresses.get(func_name) {
            let plt_off = (*plt_off + md.added_byte_count) as usize;
            let plt_vaddr = *plt_vaddr + md.added_byte_count;
            let jmp_inst_len = 5;
            let target =
                (func_virt_offset as i64 - (plt_vaddr as i64 + jmp_inst_len as i64)) as i32;
            if verbose {
                println!("\tPLT: {plt_off:+x}, {plt_vaddr:+x}");
                println!("\tTarget Jump: {target:+x}");
            }
            let data = target.to_le_bytes();
            exec_mmap[plt_off] = 0xE9;
            exec_mmap[plt_off + 1..plt_off + jmp_inst_len].copy_from_slice(&data);
            for i in jmp_inst_len..PLT_ADDRESS_OFFSET as usize {
                exec_mmap[plt_off + i] = 0x90;
            }
        }

        if let Some(i) = md.dynamic_symbol_indices.get(func_name) {
            let sym = load_struct_inplace_mut::<elf::Sym64<LE>>(
                exec_mmap,
                dynsym_offset as usize + *i as usize * mem::size_of::<elf::Sym64<LE>>(),
            );
            sym.st_shndx.set(LE, new_text_section_index as u16);
            sym.st_value.set(LE, func_virt_offset);
            sym.st_size.set(
                LE,
                match app_func_size_map.get(func_name) {
                    Some(size) => *size,
                    None => internal_error!("Size missing for: {func_name}"),
                },
            );
        }

        // Also update symbols in the regular symbol table as well.
        if let Some(i) = md.static_symbol_indices.get(func_name) {
            let sym = load_struct_inplace_mut::<elf::Sym64<LE>>(
                exec_mmap,
                symtab_offset as usize + *i as usize * mem::size_of::<elf::Sym64<LE>>(),
            );
            sym.st_shndx.set(LE, new_text_section_index as u16);
            sym.st_value.set(LE, func_virt_offset);
            sym.st_size.set(
                LE,
                match app_func_size_map.get(func_name) {
                    Some(size) => *size,
                    None => internal_error!("Size missing for: {func_name}"),
                },
            );
        }
    }

    // TODO return this instead of accepting a mutable ref!
    *offset_ref = offset;
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use roc_target::Target;

    const ELF64_DYNHOST: &[u8] = include_bytes!("../dynhost_benchmarks_elf64") as &[_];

    #[test]
    fn collect_definitions() {
        let object = object::File::parse(ELF64_DYNHOST).unwrap();

        let symbols = collect_roc_definitions(&object);

        let mut keys = symbols.keys().collect::<Vec<_>>();
        keys.sort_unstable();

        assert_eq!(
            [
                "memset",
                "roc_alloc",
                "roc_dealloc",
                "roc_fx_getInt",
                "roc_fx_getInt_help",
                "roc_fx_putInt",
                "roc_fx_putLine",
                "roc_memcpy",
                "roc_memset",
                "roc_panic",
                "roc_realloc"
            ],
            keys.as_slice(),
        )
    }

    #[test]
    fn collect_undefined_symbols_elf() {
        let object = object::File::parse(ELF64_DYNHOST).unwrap();

        let mut keys: Vec<_> = object
            .dynamic_symbols()
            .filter(is_roc_undefined)
            .filter_map(|s| s.name().ok())
            .collect();
        keys.sort_unstable();

        assert_eq!(
            [
                "roc__mainForHost_1__Fx_caller",
                "roc__mainForHost_1__Fx_result_size",
                "roc__mainForHost_1_exposed_generic",
                "roc__mainForHost_size"
            ],
            keys.as_slice()
        )
    }

    #[allow(dead_code)]
    fn zig_host_app_help(dir: &Path, target: Target) {
        let host_zig = indoc!(
            r#"
            const std = @import("std");

            extern fn roc_magic1(usize) callconv(.c) [*]const u8;

            pub fn main() !void {
                const stdout = std.io.getStdOut().writer();
                try stdout.print("Hello {s}\n", .{roc_magic1(0)[0..3]});
            }
            "#
        );

        let app_zig = indoc!(
            r#"
            const X = [_][]const u8 { "foo" };

            export fn roc_magic1(index: usize) [*]const u8 {
                return X[index].ptr;
            }
            "#
        );

        let zig = std::env::var("ROC_ZIG").unwrap_or_else(|_| "zig".into());

        std::fs::write(dir.join("host.zig"), host_zig.as_bytes()).unwrap();
        std::fs::write(dir.join("app.zig"), app_zig.as_bytes()).unwrap();

        // we need to compile the app first
        let output = std::process::Command::new(&zig)
            .current_dir(dir)
            .args(["build-obj", "app.zig", "-fPIC", "-OReleaseFast"])
            .output()
            .unwrap();

        if !output.status.success() {
            use std::io::Write;

            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();

            panic!("zig build-obj failed");
        }

        // open our app object; we'll copy sections from it later
        let file = std::fs::File::open(dir.join("app.o")).unwrap();
        let roc_app = unsafe { memmap2::Mmap::map(&file) }.unwrap();

        let names: Vec<String> = {
            let object = object::File::parse(&*roc_app).unwrap();

            object
                .symbols()
                .filter(|s| !s.is_local())
                .map(|e| e.name().unwrap().to_string())
                .collect()
        };

        let dylib_bytes = crate::generate_dylib::create_dylib_elf64(&names).unwrap();
        std::fs::write(dir.join("libapp.so"), dylib_bytes).unwrap();

        // now we can compile the host (it uses libapp.so, hence the order here)
        let output = std::process::Command::new(&zig)
            .current_dir(dir)
            .args([
                "build-exe",
                "libapp.so",
                "host.zig",
                "-fPIE",
                "-lc",
                "-OReleaseFast",
            ])
            .output()
            .unwrap();

        if !output.status.success() {
            use std::io::Write;

            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();

            panic!("zig build-exe failed");
        }

        let preprocessed_host_filename = dir.join(target.prebuilt_surgical_host());

        preprocess_elf_le(
            &dir.join("host"),
            &dir.join("metadata"),
            &preprocessed_host_filename,
            &dir.join("libapp.so"),
            false,
            false,
        );

        std::fs::copy(&preprocessed_host_filename, dir.join("final")).unwrap();

        surgery_elf(
            &roc_app,
            &dir.join("metadata"),
            &dir.join("final"),
            false,
            false,
        );
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn zig_host_app() {
        let dir = tempfile::tempdir().unwrap();
        let dir = dir.path();

        zig_host_app_help(dir, Target::LinuxX64);

        let output = std::process::Command::new(dir.join("final"))
            .current_dir(dir)
            .output()
            .unwrap();

        if !output.status.success() {
            use std::io::Write;

            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();

            panic!("app.exe failed");
        }

        let output = String::from_utf8_lossy(&output.stdout);

        assert_eq!("Hello foo\n", output);
    }
}
