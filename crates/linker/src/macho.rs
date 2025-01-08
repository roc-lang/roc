use bincode::{deserialize_from, serialize_into};
use iced_x86::{Decoder, DecoderOptions, Instruction, OpCodeOperandKind, OpKind};
use memmap2::MmapMut;
use object::macho;
use object::{
    LittleEndian as LE, Object, ObjectSection, ObjectSymbol, RelocationFlags, RelocationKind,
    RelocationTarget, Section, SectionIndex, SectionKind, Symbol, SymbolIndex, SymbolSection,
};
use roc_collections::all::MutMap;
use roc_error_macros::internal_error;
use roc_target::Architecture;
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
    load_struct_inplace_mut, load_structs_inplace, load_structs_inplace_mut, open_mmap,
    open_mmap_mut,
};

const MIN_SECTION_ALIGNMENT: usize = 0x40;

// TODO: Analyze if this offset is always correct.
const PLT_ADDRESS_OFFSET: u64 = 0x10;
const STUB_ADDRESS_OFFSET: u64 = 0x06;

// struct MachoDynamicDeps {
//     got_app_syms: Vec<(String, usize)>,
//     got_sections: Vec<(usize, usize)>,
//     dynamic_lib_count: usize,
//     shared_lib_index: usize,
// }

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
    _static_symbol_indices: MutMap<String, u64>,
    roc_symbol_vaddresses: MutMap<String, u64>,
    exec_len: u64,
    load_align_constraint: u64,
    added_byte_count: u64,
    last_vaddr: u64,
    _dynamic_section_offset: u64,
    _dynamic_symbol_table_section_offset: u64,
    _symbol_table_section_offset: u64,
    _symbol_table_size: u64,
    macho_cmd_loc: u64,
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
        let name = sym.name().unwrap().trim_start_matches('_');
        let address = sym.address();

        // special exceptions for memcpy and memset.
        let direct_mapping = match name {
            "roc_memset" => Some("memset"),
            "roc_memmove" => Some("memmove"),

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
        let file_offset = if let Some((file_offset, _)) = sec.file_range() {
            file_offset
        } else {
            internal_error!("Could not get file range for {sec:+x?}");
        };

        let data = match sec.data() {
            Ok(data) => data,
            Err(err) => internal_error!("Failed to load text section, {:+x?}: {err}", sec),
        };
        let mut decoder = Decoder::with_ip(64, data, sec.address(), DecoderOptions::NONE);
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
pub(crate) fn preprocess_macho_le(
    arch: Architecture,
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
            "Found roc symbol definitions: {:+x?}",
            md.roc_symbol_vaddresses
        );
    }

    let exec_parsing_duration = exec_parsing_start.elapsed();

    // PLT stands for Procedure Linkage Table which is, put simply, used to call external
    // procedures/functions whose address isn't known in the time of linking, and is left
    // to be resolved by the dynamic linker at run time.
    let symbol_and_plt_processing_start = Instant::now();
    let plt_section_name = "__stubs";

    let (plt_address, plt_offset) = match exec_obj.section_by_name(plt_section_name) {
        Some(section) => {
            let file_offset = if let Some((file_offset, _)) = section.file_range() {
                file_offset
            } else {
                internal_error!("Could not get file range for {section:+x?}");
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

    let app_syms: Vec<_> = exec_obj.symbols().filter(is_roc_undefined).collect();

    let mut app_func_addresses: MutMap<u64, &str> = MutMap::default();
    let mut macho_load_so_offset = None;

    {
        use macho::{DyldInfoCommand, DylibCommand, Section64, SegmentCommand64};

        let exec_header = load_struct_inplace::<macho::MachHeader64<LE>>(exec_data, 0);
        let num_load_cmds = exec_header.ncmds.get(LE);

        let mut offset = mem::size_of_val(exec_header);

        let mut stubs_symbol_index = None;
        let mut stubs_symbol_count = None;

        'cmds: for _ in 0..num_load_cmds {
            let info = load_struct_inplace::<macho::LoadCommand<LE>>(exec_data, offset);
            let cmd = info.cmd.get(LE);
            let cmdsize = info.cmdsize.get(LE);

            if cmd == macho::LC_SEGMENT_64 {
                let info = load_struct_inplace::<SegmentCommand64<LE>>(exec_data, offset);

                if &info.segname[0..6] == b"__TEXT" {
                    let sections = info.nsects.get(LE);

                    let sections_info = load_structs_inplace::<Section64<LE>>(
                        exec_data,
                        offset + mem::size_of_val(info),
                        sections as usize,
                    );

                    for section_info in sections_info {
                        if &section_info.sectname[0..7] == b"__stubs" {
                            stubs_symbol_index = Some(section_info.reserved1.get(LE));
                            stubs_symbol_count =
                                Some(section_info.size.get(LE) / STUB_ADDRESS_OFFSET);

                            break 'cmds;
                        }
                    }
                }
            }

            offset += cmdsize as usize;
        }

        let stubs_symbol_index = stubs_symbol_index.unwrap_or_else(|| {
            panic!("Could not find stubs symbol index.");
        });
        let stubs_symbol_count = stubs_symbol_count.unwrap_or_else(|| {
            panic!("Could not find stubs symbol count.");
        });

        // Reset offset before looping through load commands again
        offset = mem::size_of_val(exec_header);

        let shared_lib_filename = shared_lib.file_name();

        for _ in 0..num_load_cmds {
            let info = load_struct_inplace::<macho::LoadCommand<LE>>(exec_data, offset);
            let cmd = info.cmd.get(LE);
            let cmdsize = info.cmdsize.get(LE);

            if cmd == macho::LC_DYLD_INFO_ONLY {
                let info = load_struct_inplace::<DyldInfoCommand<LE>>(exec_data, offset);

                let lazy_bind_offset = info.lazy_bind_off.get(LE) as usize;

                let lazy_bind_symbols = mach_object::LazyBind::parse(
                    &exec_data[lazy_bind_offset..],
                    mem::size_of::<usize>(),
                );

                // Find all the lazily-bound roc symbols
                // (e.g. "_roc__main_for_host_1_exposed")
                // For Macho, we may need to deal with some GOT stuff here as well.
                for (i, symbol) in lazy_bind_symbols
                    .skip(stubs_symbol_index as usize)
                    .take(stubs_symbol_count as usize)
                    .enumerate()
                {
                    if let Some(sym) = app_syms
                        .iter()
                        .find(|app_sym| app_sym.name() == Ok(&symbol.name))
                    {
                        let func_address = (i as u64 + 1) * STUB_ADDRESS_OFFSET + plt_address;
                        let func_offset = (i as u64 + 1) * STUB_ADDRESS_OFFSET + plt_offset;
                        app_func_addresses.insert(func_address, sym.name().unwrap());
                        md.plt_addresses
                            .insert(sym.name().unwrap().to_string(), (func_offset, func_address));
                    }
                }
            } else if cmd == macho::LC_LOAD_DYLIB {
                let info = load_struct_inplace::<DylibCommand<LE>>(exec_data, offset);
                let name_offset = info.dylib.name.offset.get(LE) as usize;
                let str_start_index = offset + name_offset;
                let str_end_index = offset + cmdsize as usize;
                let str_bytes = &exec_data[str_start_index..str_end_index];
                let path = {
                    if str_bytes[str_bytes.len() - 1] == 0 {
                        // If it's nul-terminated, it's a C String.
                        // Use the unchecked version because these are
                        // padded with 0s at the end, so since we don't
                        // know the exact length, using the checked version
                        // of this can fail due to the interior nul bytes.
                        //
                        // Also, we have to use from_ptr instead of
                        // from_bytes_with_nul_unchecked because currently
                        // std::ffi::CStr is actually not a char* under
                        // the hood (!) but rather an array, so to strip
                        // the trailing null bytes we have to use from_ptr.
                        let c_str = unsafe { CStr::from_ptr(str_bytes.as_ptr() as *const c_char) };

                        Path::new(c_str.to_str().unwrap())
                    } else {
                        // It wasn't nul-terminated, so treat all the bytes
                        // as the string

                        Path::new(std::str::from_utf8(str_bytes).unwrap())
                    }
                };

                if path.file_name() == shared_lib_filename {
                    macho_load_so_offset = Some(offset);
                }
            }

            offset += cmdsize as usize;
        }
    }

    for sym in app_syms.iter() {
        let name = sym.name().unwrap().to_string();
        md.app_functions.push(name.clone());
        md.dynamic_symbol_indices.insert(name, sym.index().0 as u64);
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

    // let ElfDynamicDeps {
    //     got_app_syms,
    //     got_sections,
    //     dynamic_lib_count,
    //     shared_lib_index,
    // } = scan_elf_dynamic_deps(
    //     &exec_obj, &mut md, &app_syms, shared_lib, exec_data, verbose,
    // );

    let scanning_dynamic_deps_duration = scanning_dynamic_deps_start.elapsed();

    let platform_gen_start = Instant::now();

    // TODO little endian
    let macho_load_so_offset = match macho_load_so_offset {
        Some(offset) => offset,
        None => {
            internal_error!("Host does not link library `{}`!", shared_lib.display());
        }
    };

    md.load_align_constraint = page_size(arch);

    let out_mmap = gen_macho_le(
        exec_data,
        &mut md,
        preprocessed_path,
        macho_load_so_offset,
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

fn gen_macho_le(
    exec_data: &[u8],
    md: &mut Metadata,
    out_filename: &Path,
    macho_load_so_offset: usize,
    _verbose: bool,
) -> MmapMut {
    // Just adding some extra context/useful info here.
    // I was talking to Jakub from the Zig team about macho linking and here are some useful comments:
    // 1) Macho WILL run fine with multiple text segments (and theoretically data segments).
    // 2) Theoretically the headers just need to be in a loadable segment,
    //    but otherwise don't need to relate to the starting text segment (releated to some added byte shifting information below).
    // 2) Apple tooling dislikes whenever you do something non-standard,
    //    and there is a chance it won't work right (e.g. codesigning might fail)
    // 3) Jakub wants to make apple tooling absolute is working on zignature for code signing and zig-deploy for ios apps
    // https://github.com/kubkon/zignature
    // https://github.com/kubkon/zig-deploy

    use macho::{Section64, SegmentCommand64};

    let exec_header = load_struct_inplace::<macho::MachHeader64<LE>>(exec_data, 0);
    let num_load_cmds = exec_header.ncmds.get(LE);
    let size_of_cmds = exec_header.sizeofcmds.get(LE) as usize;

    // Add a new text segment and data segment
    let segment_cmd_size = mem::size_of::<SegmentCommand64<LE>>();
    let section_size = mem::size_of::<Section64<LE>>();

    // We need the full command size, including the dynamic-length string at the end.
    // To get that, we need to load the command.
    let info = load_struct_inplace::<macho::LoadCommand<LE>>(exec_data, macho_load_so_offset);
    let total_cmd_size = info.cmdsize.get(LE) as usize;

    // ======================== Important TODO ==========================
    // TODO: we accidentally instroduced a big change here.
    // We use a mix of added_bytes and md.added_byte_count.
    // Theses should proabably be the same variable.
    // Also, I just realized that I have been shifting a lot of virtual offsets.
    // This should not be necessary. If we add a fully 4k of buffer to the file, all of the virtual offsets will still stay aligned.
    // So a lot of the following work can probably be commented out if we fix that.
    // Of coruse, it will cost about 4k bytes to a final executable.
    // This is what the elf version currently does.
    // Theoretically it is not needed (if we update all virtual addresses in the binary),
    // but I definitely ran into problems with elf when not adding this extra buffering.

    // Copy header and shift everything to enable more program sections.
    let added_byte_count = ((2 * segment_cmd_size) + (2 * section_size) - total_cmd_size) as u64;
    md.added_byte_count = added_byte_count
        // add some alignment padding
        + (MIN_SECTION_ALIGNMENT as u64 - added_byte_count % MIN_SECTION_ALIGNMENT as u64);

    md.exec_len = exec_data.len() as u64 + md.added_byte_count;

    let mut out_mmap = open_mmap_mut(out_filename, md.exec_len as usize);
    let end_of_cmds = size_of_cmds + mem::size_of_val(exec_header);

    // "Delete" the dylib load command - by copying all the bytes before it
    // and all the bytes after it, while skipping over its bytes.
    // It has a dynamic-length string at the end that we also need to delete,
    // in addition to the header.
    out_mmap[..macho_load_so_offset].copy_from_slice(&exec_data[..macho_load_so_offset]);

    out_mmap[macho_load_so_offset..end_of_cmds - total_cmd_size]
        .copy_from_slice(&exec_data[macho_load_so_offset + total_cmd_size..end_of_cmds]);

    // Copy the rest of the file, leaving a gap for the surgical linking to add our 2 commands
    // (which happens after preprocessing), and some zero padding at the end for alignemnt.
    // (It seems to cause bugs if that padding isn't there!)
    let rest_of_data = &exec_data[end_of_cmds..];
    let start_index = end_of_cmds + md.added_byte_count as usize;

    out_mmap[start_index..start_index + rest_of_data.len()].copy_from_slice(rest_of_data);

    let out_header = load_struct_inplace_mut::<macho::MachHeader64<LE>>(&mut out_mmap, 0);

    // TODO: this needs to change to adding the 2 new commands when we are ready.
    // -1 because we're deleting 1 load command and then NOT adding 2 new ones.
    {
        let added_bytes = -(total_cmd_size as isize); // TODO: Change when add the new sections.
        out_header.ncmds.set(LE, num_load_cmds - 1);
        out_header
            .sizeofcmds
            .set(LE, (size_of_cmds as isize + added_bytes) as u32);
    }

    // Go through every command and shift it by added_bytes if it's absolute, unless it's inside the command header
    let mut offset = mem::size_of_val(exec_header);

    // TODO: Add this back in the future when needed.
    // let cpu_type = match target.architecture {
    //     target_lexicon::Architecture::X86_64 => macho::CPU_TYPE_X86_64,
    //     target_lexicon::Architecture::Aarch64(_) => macho::CPU_TYPE_ARM64,
    //     _ => {
    //         // We should have verified this via supported() before calling this function
    //         unreachable!()
    //     }
    // };

    // minus one because we "deleted" a load command
    for _ in 0..(num_load_cmds - 1) {
        let info = load_struct_inplace::<macho::LoadCommand<LE>>(&out_mmap, offset);
        let cmd_size = info.cmdsize.get(LE) as usize;

        match info.cmd.get(LE) {
            macho::LC_SEGMENT_64 => {
                let cmd =
                    load_struct_inplace_mut::<macho::SegmentCommand64<LE>>(&mut out_mmap, offset);

                // Ignore page zero, it never moves.
                if cmd.segname == "__PAGEZERO\0\0\0\0\0\0".as_bytes() || cmd.vmaddr.get(LE) == 0 {
                    offset += cmd_size;
                    continue;
                }

                let old_file_offest = cmd.fileoff.get(LE);
                // The segment with file offset zero also includes the header.
                // As such, its file offset does not change.
                // Instead, its file size should be increased.
                if old_file_offest > 0 {
                    cmd.fileoff.set(LE, old_file_offest + md.added_byte_count);
                    cmd.vmaddr.set(LE, cmd.vmaddr.get(LE) + md.added_byte_count);
                } else {
                    cmd.filesize
                        .set(LE, cmd.filesize.get(LE) + md.added_byte_count);
                    cmd.vmsize.set(LE, cmd.vmsize.get(LE) + md.added_byte_count);
                }

                // let num_sections = cmd.nsects.get(LE);
                // let sections = load_structs_inplace_mut::<macho::Section64<LE >>(
                //     &mut out_mmap,
                //     offset + mem::size_of::<macho::SegmentCommand64<LE >>(),
                //     num_sections as usize,
                // );
                // struct Relocation {
                //     offset: u32,
                //     num_relocations: u32,
                // }

                // let mut relocation_offsets = Vec::with_capacity(sections.len());

                // for section in sections {
                //     section.addr.set(
                //         LE ,
                //         section.addr.get(LE) + md.added_byte_count as u64,
                //     );

                //     // If offset is zero, don't update it.
                //     // Zero is used for things like BSS that don't exist in the file.
                //     let old_offset = section.offset.get(LE);
                //     if old_offset > 0 {
                //         section
                //             .offset
                //             .set(LE , old_offset + md.added_byte_count as u32);
                //     }

                //     if section.nreloc.get(LE) > 0 {
                //         section.reloff.set(
                //             LE ,
                //             section.reloff.get(LE) + md.added_byte_count as u32,
                //         );
                //     }

                //     relocation_offsets.push(Relocation {
                //         offset: section.reloff.get(LE),
                //         num_relocations: section.nreloc.get(LE),
                //     });
                // }

                // TODO FIXME this is necessary for ARM, but seems to be broken. Skipping for now since we're just targeting x86
                // for Relocation {
                //     offset,
                //     num_relocations,
                // } in relocation_offsets
                // {
                //     let relos = load_structs_inplace_mut::<macho::Relocation<LE >>(
                //         &mut out_mmap,
                //         offset as usize,
                //         num_relocations as usize,
                //     );

                //     // TODO this has never been tested, because scattered relocations only come up on ARM!
                //     for relo in relos.iter_mut() {
                //         if relo.r_scattered(LE , cpu_type) {
                //             let mut scattered_info = relo.scattered_info(LE);

                //             if !scattered_info.r_pcrel {
                //                 scattered_info.r_value += md.added_byte_count as u32;

                //                 let new_info = scattered_info.relocation(LE );

                //                 relo.r_word0 = new_info.r_word0;
                //                 relo.r_word1 = new_info.r_word1;
                //             }
                //         }
                //     }
                // }

                // TODO this seems to be wrong and unnecessary, and should probably be deleted.
                // offset += num_sections as usize * mem::size_of::<macho::Section64<LE >>();
            }
            macho::LC_SYMTAB => {
                let cmd =
                    load_struct_inplace_mut::<macho::SymtabCommand<LE>>(&mut out_mmap, offset);

                let sym_offset = cmd.symoff.get(LE);
                let num_syms = cmd.nsyms.get(LE);

                if num_syms > 0 {
                    cmd.symoff.set(LE, sym_offset + md.added_byte_count as u32);
                }

                if cmd.strsize.get(LE) > 0 {
                    cmd.stroff
                        .set(LE, cmd.stroff.get(LE) + md.added_byte_count as u32);
                }

                let table = load_structs_inplace_mut::<macho::Nlist64<LE>>(
                    &mut out_mmap,
                    sym_offset as usize + md.added_byte_count as usize,
                    num_syms as usize,
                );

                for entry in table {
                    let entry_type = entry.n_type & macho::N_TYPE;
                    if entry_type == macho::N_ABS || entry_type == macho::N_SECT {
                        entry
                            .n_value
                            .set(LE, entry.n_value.get(LE) + md.added_byte_count);
                    }
                }
            }
            macho::LC_DYSYMTAB => {
                let cmd =
                    load_struct_inplace_mut::<macho::DysymtabCommand<LE>>(&mut out_mmap, offset);

                if cmd.ntoc.get(LE) > 0 {
                    cmd.tocoff
                        .set(LE, cmd.tocoff.get(LE) + md.added_byte_count as u32);
                }

                if cmd.nmodtab.get(LE) > 0 {
                    cmd.modtaboff
                        .set(LE, cmd.modtaboff.get(LE) + md.added_byte_count as u32);
                }

                if cmd.nextrefsyms.get(LE) > 0 {
                    cmd.extrefsymoff
                        .set(LE, cmd.extrefsymoff.get(LE) + md.added_byte_count as u32);
                }

                if cmd.nindirectsyms.get(LE) > 0 {
                    cmd.indirectsymoff
                        .set(LE, cmd.indirectsymoff.get(LE) + md.added_byte_count as u32);
                }

                if cmd.nextrel.get(LE) > 0 {
                    cmd.extreloff
                        .set(LE, cmd.extreloff.get(LE) + md.added_byte_count as u32);
                }

                if cmd.nlocrel.get(LE) > 0 {
                    cmd.locreloff
                        .set(LE, cmd.locreloff.get(LE) + md.added_byte_count as u32);
                }

                // TODO maybe we need to update something else too - relocations maybe?
                // I think this also has symbols that need to get moved around.
                // Look at otool -I at least for the indirect symbols.
            }
            macho::LC_TWOLEVEL_HINTS => {
                let cmd = load_struct_inplace_mut::<macho::TwolevelHintsCommand<LE>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.nhints.get(LE) > 0 {
                    cmd.offset
                        .set(LE, cmd.offset.get(LE) + md.added_byte_count as u32);
                }
            }
            macho::LC_FUNCTION_STARTS => {
                let cmd = load_struct_inplace_mut::<macho::LinkeditDataCommand<LE>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.datasize.get(LE) > 0 {
                    cmd.dataoff
                        .set(LE, cmd.dataoff.get(LE) + md.added_byte_count as u32);
                    // TODO: This lists the start of every function. Which, of course, have moved.
                    // That being said, to my understanding this section is optional and may just be debug information.
                    // As such, updating it should not be required.
                    // It will be more work to update due to being in "DWARF-style ULEB128" values.
                }
            }
            macho::LC_DATA_IN_CODE => {
                let (offset, size) = {
                    let cmd = load_struct_inplace_mut::<macho::LinkeditDataCommand<LE>>(
                        &mut out_mmap,
                        offset,
                    );

                    if cmd.datasize.get(LE) > 0 {
                        cmd.dataoff
                            .set(LE, cmd.dataoff.get(LE) + md.added_byte_count as u32);
                    }
                    (cmd.dataoff.get(LE), cmd.datasize.get(LE))
                };

                // Update every data in code entry.
                if size > 0 {
                    let entry_size = mem::size_of::<macho::DataInCodeEntry<LE>>();
                    let entries = load_structs_inplace_mut::<macho::DataInCodeEntry<LE>>(
                        &mut out_mmap,
                        offset as usize,
                        size as usize / entry_size,
                    );
                    for entry in entries.iter_mut() {
                        entry
                            .offset
                            .set(LE, entry.offset.get(LE) + md.added_byte_count as u32)
                    }
                }
            }
            macho::LC_CODE_SIGNATURE
            | macho::LC_SEGMENT_SPLIT_INFO
            | macho::LC_DYLIB_CODE_SIGN_DRS
            | macho::LC_LINKER_OPTIMIZATION_HINT
            | macho::LC_DYLD_EXPORTS_TRIE
            | macho::LC_DYLD_CHAINED_FIXUPS => {
                let cmd = load_struct_inplace_mut::<macho::LinkeditDataCommand<LE>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.datasize.get(LE) > 0 {
                    cmd.dataoff
                        .set(LE, cmd.dataoff.get(LE) + md.added_byte_count as u32);
                }
            }
            macho::LC_ENCRYPTION_INFO_64 => {
                let cmd = load_struct_inplace_mut::<macho::EncryptionInfoCommand64<LE>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.cryptsize.get(LE) > 0 {
                    cmd.cryptoff
                        .set(LE, cmd.cryptoff.get(LE) + md.added_byte_count as u32);
                }
            }
            macho::LC_DYLD_INFO | macho::LC_DYLD_INFO_ONLY => {
                let cmd =
                    load_struct_inplace_mut::<macho::DyldInfoCommand<LE>>(&mut out_mmap, offset);

                if cmd.rebase_size.get(LE) > 0 {
                    cmd.rebase_off
                        .set(LE, cmd.rebase_off.get(LE) + md.added_byte_count as u32);
                }

                if cmd.bind_size.get(LE) > 0 {
                    cmd.bind_off
                        .set(LE, cmd.bind_off.get(LE) + md.added_byte_count as u32);
                }

                if cmd.weak_bind_size.get(LE) > 0 {
                    cmd.weak_bind_off
                        .set(LE, cmd.weak_bind_off.get(LE) + md.added_byte_count as u32);
                }

                if cmd.lazy_bind_size.get(LE) > 0 {
                    cmd.lazy_bind_off
                        .set(LE, cmd.lazy_bind_off.get(LE) + md.added_byte_count as u32);
                }

                // TODO: Parse and update the related tables here.
                // It is possible we may just need to delete things that point to stuff that will be in the roc app.
                // We also may just be able to ignore it (lazy bindings should never run).
                // This definitely has a list of virtual address that need to be updated.
                // Some of them definitely will point to the roc app and should probably be removed.
                // Also `xcrun dyldinfo` is useful for debugging this.
            }
            macho::LC_SYMSEG => {
                let cmd =
                    load_struct_inplace_mut::<macho::SymsegCommand<LE>>(&mut out_mmap, offset);

                if cmd.size.get(LE) > 0 {
                    cmd.offset
                        .set(LE, cmd.offset.get(LE) + md.added_byte_count as u32);
                }
            }
            macho::LC_MAIN => {
                let cmd =
                    load_struct_inplace_mut::<macho::EntryPointCommand<LE>>(&mut out_mmap, offset);

                cmd.entryoff
                    .set(LE, cmd.entryoff.get(LE) + md.added_byte_count);
            }
            macho::LC_NOTE => {
                let cmd = load_struct_inplace_mut::<macho::NoteCommand<LE>>(&mut out_mmap, offset);

                if cmd.size.get(LE) > 0 {
                    cmd.offset.set(LE, cmd.offset.get(LE) + md.added_byte_count);
                }
            }
            macho::LC_ID_DYLIB
            | macho::LC_LOAD_WEAK_DYLIB
            | macho::LC_LOAD_DYLIB
            | macho::LC_REEXPORT_DYLIB
            | macho::LC_SOURCE_VERSION
            | macho::LC_IDENT
            | macho::LC_LINKER_OPTION
            | macho::LC_BUILD_VERSION
            | macho::LC_VERSION_MIN_MACOSX
            | macho::LC_VERSION_MIN_IPHONEOS
            | macho::LC_VERSION_MIN_WATCHOS
            | macho::LC_VERSION_MIN_TVOS
            | macho::LC_UUID
            | macho::LC_RPATH
            | macho::LC_ID_DYLINKER
            | macho::LC_LOAD_DYLINKER
            | macho::LC_DYLD_ENVIRONMENT
            | macho::LC_ROUTINES_64
            | macho::LC_THREAD
            | macho::LC_UNIXTHREAD
            | macho::LC_PREBOUND_DYLIB
            | macho::LC_SUB_FRAMEWORK
            | macho::LC_SUB_CLIENT
            | macho::LC_SUB_UMBRELLA
            | macho::LC_SUB_LIBRARY => {
                // These don't involve file offsets, so no change is needed for these.
                // Just continue the loop!
            }
            macho::LC_PREBIND_CKSUM => {
                // We don't *think* we need to change this, but
                // maybe what we're doing will break checksums?
            }
            macho::LC_SEGMENT | macho::LC_ROUTINES | macho::LC_ENCRYPTION_INFO => {
                // These are 32-bit and unsuppoted
                unreachable!();
            }
            macho::LC_FVMFILE | macho::LC_IDFVMLIB | macho::LC_LOADFVMLIB => {
                // These are obsolete and unsupported
                unreachable!()
            }
            cmd => {
                eprintln!(
                    "- - - Unrecognized Mach-O command during linker preprocessing: 0x{cmd:x?}"
                );
                // panic!(
                //     "Unrecognized Mach-O command during linker preprocessing: 0x{:x?}",
                //     cmd
                // );
            }
        }

        offset += cmd_size;
    }

    // cmd_loc should be where the last offset ended
    md.macho_cmd_loc = offset as u64;

    out_mmap
}

// fn scan_macho_dynamic_deps(
//     _exec_obj: &object::File,
//     _md: &mut Metadata,
//     _app_syms: &[Symbol],
//     _shared_lib: &str,
//     _exec_data: &[u8],
//     _verbose: bool,
// ) {
//     // TODO
// }

pub(crate) fn surgery_macho(
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

    surgery_macho_help(
        metadata_path,
        executable_path,
        verbose,
        time,
        &md,
        &mut exec_mmap,
        &mut offset,
        app_obj,
    );

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

#[allow(clippy::too_many_arguments)]
fn surgery_macho_help(
    _metadata_filename: &Path,
    _out_filename: &Path,
    verbose: bool,
    _time: bool,
    md: &Metadata,
    exec_mmap: &mut MmapMut,
    offset_ref: &mut usize, // TODO return this instead of taking a mutable reference to it
    app_obj: object::File,
) {
    let mut offset = align_by_constraint(md.exec_len as usize, MIN_SECTION_ALIGNMENT);
    // let new_rodata_section_offset = offset;

    // Align physical and virtual address of new segment.
    let mut virt_offset = align_to_offset_by_constraint(
        md.last_vaddr as usize,
        offset,
        md.load_align_constraint as usize,
    );
    let new_rodata_section_vaddr = virt_offset;
    if verbose {
        println!();
        println!("New Virtual Rodata Section Address: {new_rodata_section_vaddr:+x?}");
    }

    // First decide on sections locations and then recode every exact symbol locations.

    // Copy sections and resolve their symbols/relocations.
    let symbols = app_obj.symbols().collect::<Vec<Symbol>>();
    let mut section_offset_map: MutMap<SectionIndex, (usize, usize)> = MutMap::default();
    let mut symbol_vaddr_map: MutMap<SymbolIndex, usize> = MutMap::default();
    let mut app_func_vaddr_map: MutMap<String, usize> = MutMap::default();
    let mut app_func_size_map: MutMap<String, u64> = MutMap::default();

    // TODO: In the future Roc may use a data section to store memoized toplevel thunks
    // in development builds for caching the results of top-level constants

    let rodata_sections: Vec<Section> = app_obj
        .sections()
        .filter(|sec| match sec.kind() {
            SectionKind::ReadOnlyData => sec.name().unwrap_or("") != "__eh_frame",
            SectionKind::ReadOnlyString => true,
            _ => false,
        })
        .collect();

    // bss section is like rodata section, but it has zero file size and non-zero virtual size.
    let bss_sections: Vec<Section> = app_obj
        .sections()
        .filter(|sec| sec.kind() == SectionKind::UninitializedData)
        .collect();

    let text_sections: Vec<Section> = app_obj
        .sections()
        .filter(|sec| sec.kind() == SectionKind::Text)
        .collect();
    if text_sections.is_empty() {
        internal_error!("No text sections found. This application has no code.");
    }

    if verbose {
        println!();
        println!("Roc symbol addresses: {:+x?}", md.roc_symbol_vaddresses);
        println!("App functions: {:?}", md.app_functions);
        println!("Dynamic symbol indices: {:+x?}", md.dynamic_symbol_indices);
        println!("PLT addresses: {:+x?}", md.plt_addresses);
        println!();
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
            );
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
        if sec.name().unwrap_or_default().starts_with("__bss") {
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

    // let (new_text_section_offset, new_text_section_vaddr) = text_sections
    //     .iter()
    //     .map(|sec| section_offset_map.get(&sec.index()).unwrap())
    //     .min()
    //     .unwrap();
    // let (new_text_section_offset, new_text_section_vaddr) =
    //     (*new_text_section_offset, *new_text_section_vaddr);

    // Move data and deal with relocations.
    for sec in rodata_sections
        .iter()
        .chain(bss_sections.iter())
        // TODO why do we even include uninitialized data if it cannot
        // ever have any relocations in the first place?
        .chain(text_sections.iter())
    {
        let data = sec.data().unwrap_or_else(|err| {
            internal_error!(
                "Failed to load data for section, {:+x?}: {err}",
                sec.name().unwrap(),
            );
        });
        let (section_offset, section_virtual_offset) =
            section_offset_map.get(&sec.index()).unwrap();
        let (section_offset, section_virtual_offset) = (*section_offset, *section_virtual_offset);
        exec_mmap[section_offset..section_offset + data.len()].copy_from_slice(data);
        // Deal with definitions and relocations for this section.
        if verbose {
            let segname = sec
                .segment_name()
                .expect(
                    "valid segment
                    name",
                )
                .unwrap();
            let sectname = sec.name().unwrap();
            println!();
            println!(
                "Processing Relocations for Section '{segname},{sectname}': 0x{sec:+x?} @ {section_offset:+x} (virt: {section_virtual_offset:+x})"
            );
        }

        let mut subtractor: Option<SymbolIndex> = None;
        for rel in sec.relocations() {
            if verbose {
                println!("\tFound Relocation: {rel:+x?}");
            }
            match rel.1.target() {
                RelocationTarget::Symbol(index) => {
                    let target_offset = if let Some(target_offset) =
                        get_target_offset(index, &app_obj, md, &symbol_vaddr_map, verbose)
                    {
                        target_offset
                    } else if matches!(app_obj.symbol_by_index(index), Ok(sym) if ["__divti3", "__udivti3", "___divti3", "___udivti3"].contains(&sym.name().unwrap_or_default()))
                    {
                        // Explicitly ignore some symbols that are currently always linked.
                        continue;
                    } else if matches!(app_obj.symbol_by_index(index), Ok(sym) if ["_longjmp", "_setjmp"].contains(&sym.name().unwrap_or_default()))
                    {
                        // Explicitly ignore `longjmp` and `setjmp` which are used only in `roc test` mode and thus are unreferenced
                        // by the app and can be safely skipped.
                        // In the future, `longjmp` and `setjmp` will be obsoleted and thus this prong can be safely deleted.
                        continue;
                    } else {
                        internal_error!(
                            "Undefined Symbol in relocation, {:+x?}: {:+x?}",
                            rel,
                            app_obj.symbol_by_index(index)
                        )
                    };

                    let virt_base = section_virtual_offset + rel.0 as usize;
                    let base = section_offset + rel.0 as usize;
                    let target: i64 = match rel.1.kind() {
                        RelocationKind::Relative | RelocationKind::PltRelative => {
                            target_offset - virt_base as i64 + rel.1.addend()
                        }
                        RelocationKind::Absolute => {
                            target_offset + rel.1.addend()
                                - subtractor
                                    .take()
                                    .map(|index| {
                                        get_target_offset(
                                            index,
                                            &app_obj,
                                            md,
                                            &symbol_vaddr_map,
                                            verbose,
                                        )
                                        .unwrap_or(0)
                                    })
                                    .unwrap()
                        }
                        RelocationKind::Unknown => {
                            if let RelocationFlags::MachO { r_type, .. } = rel.1.flags() {
                                match r_type {
                                    macho::ARM64_RELOC_SUBTRACTOR => {
                                        if subtractor.is_some() {
                                            internal_error!("Malformed object: SUBTRACTOR must not be followed by SUBTRACTOR");
                                        } else {
                                            subtractor = Some(index);
                                        }
                                        continue;
                                    }
                                    _ => {
                                        if verbose {
                                            println!(
                                                "\t\tHandle other MachO relocs: {}",
                                                format_reloc_type(r_type)
                                            );
                                        }
                                        0
                                    }
                                }
                            } else {
                                internal_error!("Invalid relocation found for Mach-O: {:?}", rel);
                            }
                        }
                        x => {
                            internal_error!("Relocation Kind not yet support: {:?}", x);
                        }
                    };
                    if verbose {
                        println!("\t\tRelocation base location: {base:+x} (virt: {virt_base:+x})");
                        println!("\t\tFinal relocation target offset: {target:+x}");
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
                            internal_error!("Relocation size not yet supported: {}", x);
                        }
                    }
                }

                _ => {
                    internal_error!("Relocation target not yet support: {:+x?}", rel);
                }
            }
        }
    }

    // Flush app only data to speed up write to disk.
    exec_mmap
        .flush_async_range(md.exec_len as usize, offset - md.exec_len as usize)
        .unwrap_or_else(|e| internal_error!("{}", e));

    // TODO: look into merging symbol tables, debug info, and eh frames to enable better debugger experience.

    // let mut cmd_offset = md.macho_cmd_loc as usize;

    // // Load this section (we made room for it earlier) and then mutate all its data to make it the desired command
    // {
    //     let cmd =
    //         load_struct_inplace_mut::<macho::SegmentCommand64<LE >>(exec_mmap, cmd_offset);
    //     let size_of_section = mem::size_of::<macho::Section64<LE >>() as u32;
    //     let size_of_cmd = mem::size_of_val(cmd);

    //     cmd_offset += size_of_cmd;

    //     cmd.cmd.set(LE , macho::LC_SEGMENT_64);
    //     cmd.cmdsize
    //         .set(LE , size_of_section + size_of_cmd as u32);
    //     cmd.segname = *b"__DATA_CONST\0\0\0\0";
    //     cmd.vmaddr
    //         .set(LE , new_rodata_section_vaddr as u64);
    //     cmd.vmsize.set(
    //         LE ,
    //         (new_text_section_vaddr - new_rodata_section_vaddr) as u64,
    //     );
    //     cmd.fileoff
    //         .set(LE , new_rodata_section_offset as u64);
    //     cmd.filesize.set(
    //         LE ,
    //         (new_text_section_offset - new_rodata_section_offset) as u64,
    //     );
    //     cmd.nsects.set(LE , 1);
    //     cmd.maxprot.set(LE , 0x00000003);
    //     cmd.initprot.set(LE , 0x00000003);

    //     // TODO set protection
    // }

    // {
    //     let cmd = load_struct_inplace_mut::<macho::Section64<LE >>(exec_mmap, cmd_offset);
    //     let size_of_cmd = mem::size_of_val(cmd);

    //     cmd_offset += size_of_cmd;

    //     cmd.sectname = *b"__const\0\0\0\0\0\0\0\0\0";
    //     cmd.segname = *b"__DATA_CONST\0\0\0\0";
    //     cmd.addr.set(LE , new_rodata_section_vaddr as u64);
    //     cmd.size.set(
    //         LE ,
    //         (new_text_section_offset - new_rodata_section_offset) as u64,
    //     );
    //     cmd.offset.set(LE , 0); // TODO is this offset since the start of the file, or segment offset?
    //     cmd.align.set(LE , 12); // TODO should this be 4096?
    //     cmd.reloff.set(LE , 264); // TODO this should NOT be hardcoded! Should get it from somewhere.
    // }

    // {
    //     let cmd =
    //         load_struct_inplace_mut::<macho::SegmentCommand64<LE >>(exec_mmap, cmd_offset);
    //     let size_of_section = mem::size_of::<macho::Section64<LE >>() as u32;
    //     let size_of_cmd = mem::size_of_val(cmd);

    //     cmd_offset += size_of_cmd;

    //     cmd.cmd.set(LE , macho::LC_SEGMENT_64);
    //     cmd.cmdsize
    //         .set(LE , size_of_section + size_of_cmd as u32);
    //     cmd.segname = *b"__TEXT\0\0\0\0\0\0\0\0\0\0";
    //     cmd.vmaddr.set(LE , new_text_section_vaddr as u64);
    //     cmd.vmsize
    //         .set(LE , (offset - new_text_section_offset) as u64);
    //     cmd.fileoff
    //         .set(LE , new_text_section_offset as u64);
    //     cmd.filesize
    //         .set(LE , (offset - new_text_section_offset) as u64);
    //     cmd.nsects.set(LE , 1);
    //     cmd.maxprot.set(LE , 0x00000005); // this is what a zig-generated host had
    //     cmd.initprot.set(LE , 0x00000005); // this is what a zig-generated host had
    // }

    // {
    //     let cmd = load_struct_inplace_mut::<macho::Section64<LE >>(exec_mmap, cmd_offset);

    //     cmd.segname = *b"__TEXT\0\0\0\0\0\0\0\0\0\0";
    //     cmd.sectname = *b"__text\0\0\0\0\0\0\0\0\0\0";
    //     cmd.addr.set(LE , new_text_section_vaddr as u64);
    //     cmd.size
    //         .set(LE , (offset - new_text_section_offset) as u64);
    //     cmd.offset.set(LE , 0); // TODO is this offset since the start of the file, or segment offset?
    //     cmd.align.set(LE , 12); // TODO this is 4096 (2^12) - which load_align_constraint does, above - but should it?
    //     cmd.flags.set(LE , 0x80000400); // TODO this is what a zig-generated host had
    //     cmd.reloff.set(LE , 264); // TODO this should NOT be hardcoded! Should get it from somewhere.
    // }

    // Update calls from platform and dynamic symbols.
    // let dynsym_offset = md.dynamic_symbol_table_section_offset + md.added_byte_count;

    for func_name in md.app_functions.iter() {
        let func_virt_offset = match app_func_vaddr_map.get(func_name) {
            Some(offset) => *offset as u64,
            None => {
                internal_error!("Function, {}, was not defined by the app", &func_name);
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
                    exec_mmap[(s.file_offset + md.added_byte_count) as usize
                        ..(s.file_offset + md.added_byte_count) as usize + 4]
                        .copy_from_slice(&data);
                }
                8 => {
                    let target = func_virt_offset as i64 - surgery_virt_offset;
                    if verbose {
                        println!("\tTarget Jump: {target:+x}");
                    }
                    let data = target.to_le_bytes();
                    exec_mmap[(s.file_offset + md.added_byte_count) as usize
                        ..(s.file_offset + md.added_byte_count) as usize + 8]
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

        // Commented out because it doesn't apply to mach-o
        // if let Some(i) = md.dynamic_symbol_indices.get(func_name) {
        //     let sym = load_struct_inplace_mut::<elf::Sym64<LE >>(
        //         exec_mmap,
        //         dynsym_offset as usize + *i as usize * mem::size_of::<elf::Sym64<LE >>(),
        //     );
        //     sym.st_value.set(LE , func_virt_offset as u64);
        //     sym.st_size.set(
        //         LE ,
        //         match app_func_size_map.get(func_name) {
        //             Some(size) => *size,
        //             None => {
        //                 internal_error!("Size missing for: {}", func_name);
        //             }
        //         },
        //     );
        // }
    }

    *offset_ref = offset;
}

fn get_target_offset(
    index: SymbolIndex,
    app_obj: &object::File,
    md: &Metadata,
    symbol_vaddr_map: &MutMap<SymbolIndex, usize>,
    verbose: bool,
) -> Option<i64> {
    if let Some(target_offset) = symbol_vaddr_map.get(&index) {
        if verbose {
            println!("\t\tRelocation targets symbol in app at: {target_offset:+x}");
        }
        Some(*target_offset as i64)
    } else {
        app_obj
            .symbol_by_index(index)
            .and_then(|sym| sym.name().map(|name| name.trim_start_matches('_')))
            .ok()
            .and_then(|name| {
                md.roc_symbol_vaddresses.get(name).map(|address| {
                    let vaddr = (*address + md.added_byte_count) as i64;
                    if verbose {
                        println!("\t\tRelocation targets symbol in host: {name} @ {vaddr:+x}");
                    }
                    vaddr
                })
            })
    }
}

fn format_reloc_type(value: u8) -> impl std::fmt::Display {
    struct Inner(u8);

    impl std::fmt::Display for Inner {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let name: &str = match self.0 {
                macho::ARM64_RELOC_ADDEND => "ARM64_RELOC_ADDEND",
                macho::ARM64_RELOC_PAGE21 => "ARM64_RELOC_PAGE21",
                macho::ARM64_RELOC_PAGEOFF12 => "ARM64_RELOC_PAGEOFF12",
                macho::ARM64_RELOC_BRANCH26 => "ARM64_RELOC_BRANCH26",
                macho::ARM64_RELOC_UNSIGNED => "ARM64_RELOC_UNSIGNED",
                macho::ARM64_RELOC_SUBTRACTOR => "ARM64_RELOC_SUBTRACTOR",
                macho::ARM64_RELOC_GOT_LOAD_PAGE21 => "ARM64_RELOC_GOT_LOAD_PAGE21",
                macho::ARM64_RELOC_GOT_LOAD_PAGEOFF12 => "ARM64_RELOC_GOT_LOAD_PAGEOFF12",
                macho::ARM64_RELOC_TLVP_LOAD_PAGE21 => "ARM64_RELOC_TLVP_LOAD_PAGE21",
                macho::ARM64_RELOC_TLVP_LOAD_PAGEOFF12 => "ARM64_RELOC_TLVP_LOAD_PAGEOFF12",
                macho::ARM64_RELOC_POINTER_TO_GOT => "ARM64_RELOC_POINTER_TO_GOT",
                macho::ARM64_RELOC_AUTHENTICATED_POINTER => "ARM64_RELOC_AUTHENTICATED_POINTER",
                _ => "ARM64_RELOC_UNKNOWN",
            };
            write!(f, "{name}({})", self.0)
        }
    }

    Inner(value)
}

fn page_size(arch: Architecture) -> u64 {
    match arch {
        Architecture::X86_64 => 0x1000,
        Architecture::Aarch64 => 0x4000,
        _ => unreachable!(),
    }
}
