use crate::metadata::{self, Metadata, VirtualOffset};
use iced_x86::{Decoder, DecoderOptions, Instruction, OpCodeOperandKind, OpKind};
use memmap2::MmapMut;
use object::macho;
use object::{
    CompressedFileRange, CompressionFormat, LittleEndian, NativeEndian, Object, ObjectSection,
    ObjectSymbol, RelocationKind, RelocationTarget, Section, SectionIndex, SectionKind, Symbol,
    SymbolIndex, SymbolSection,
};
use roc_collections::all::MutMap;
use roc_error_macros::internal_error;
use std::ffi::CStr;
use std::mem;
use std::path::Path;
use std::time::{Duration, Instant};
use target_lexicon::Triple;

use crate::{
    align_by_constraint, align_to_offset_by_constraint, load_struct_inplace,
    load_struct_inplace_mut, load_structs_inplace, load_structs_inplace_mut, open_mmap,
    open_mmap_mut, redirect_libc_functions,
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

fn report_timing(label: &str, duration: Duration) {
    println!("\t{:9.3} ms   {}", duration.as_secs_f64() * 1000.0, label,);
}

fn is_roc_symbol(sym: &object::Symbol) -> bool {
    if let Ok(name) = sym.name() {
        name.trim_start_matches('_').starts_with("roc_")
    } else {
        false
    }
}

fn is_roc_definition(sym: &object::Symbol) -> bool {
    sym.is_definition() && is_roc_symbol(sym)
}

fn is_roc_undefined(sym: &object::Symbol) -> bool {
    sym.is_undefined() && is_roc_symbol(sym)
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

        let address = sym.address() as u64;

        // special exceptions for memcpy and memset.
        if let Some(name) = redirect_libc_functions(name) {
            vaddresses.insert(name.to_string(), address);
        }

        vaddresses.insert(name.to_string(), address);
    }

    vaddresses
}

struct Surgeries<'a> {
    surgeries: MutMap<String, Vec<metadata::SurgeryEntry>>,
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
                println!("{:+x?}", sec);
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
                                "\tNeed to surgically replace {} bytes at file offset {:+x}",
                                op_size, offset,
                            );
                            println!(
                                "\tIts current value is {:+x?}",
                                &object_bytes[offset as usize..(offset + op_size as u64) as usize]
                            )
                        }
                        self.surgeries
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

/// Constructs a `metadata::Metadata` from a host executable binary, and writes it to disk
pub(crate) fn preprocess_macho(
    target: &Triple,
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

    let mut md = metadata::Metadata {
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
        println!("PLT Address: {:+x}", plt_address);
        println!("PLT File Offset: {:+x}", plt_offset);
    }

    let app_syms: Vec<_> = exec_obj.symbols().filter(is_roc_undefined).collect();

    let mut app_func_addresses: MutMap<u64, &str> = MutMap::default();
    let mut macho_load_so_offset = None;

    {
        use macho::{DyldInfoCommand, DylibCommand, Section64, SegmentCommand64};

        let exec_header = load_struct_inplace::<macho::MachHeader64<LittleEndian>>(exec_data, 0);
        let num_load_cmds = exec_header.ncmds.get(NativeEndian);

        let mut offset = mem::size_of_val(exec_header);

        let mut stubs_symbol_index = None;
        let mut stubs_symbol_count = None;

        'cmds: for _ in 0..num_load_cmds {
            let info = load_struct_inplace::<macho::LoadCommand<LittleEndian>>(exec_data, offset);
            let cmd = info.cmd.get(NativeEndian);
            let cmdsize = info.cmdsize.get(NativeEndian);

            if cmd == macho::LC_SEGMENT_64 {
                let info = load_struct_inplace::<SegmentCommand64<LittleEndian>>(exec_data, offset);

                if &info.segname[0..6] == b"__TEXT" {
                    let sections = info.nsects.get(NativeEndian);

                    let sections_info = load_structs_inplace::<Section64<LittleEndian>>(
                        exec_data,
                        offset + mem::size_of_val(info),
                        sections as usize,
                    );

                    for section_info in sections_info {
                        if &section_info.sectname[0..7] == b"__stubs" {
                            stubs_symbol_index = Some(section_info.reserved1.get(NativeEndian));
                            stubs_symbol_count =
                                Some(section_info.size.get(NativeEndian) / STUB_ADDRESS_OFFSET);

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
            let info = load_struct_inplace::<macho::LoadCommand<LittleEndian>>(exec_data, offset);
            let cmd = info.cmd.get(NativeEndian);
            let cmdsize = info.cmdsize.get(NativeEndian);

            if cmd == macho::LC_DYLD_INFO_ONLY {
                let info = load_struct_inplace::<DyldInfoCommand<LittleEndian>>(exec_data, offset);

                let lazy_bind_offset = info.lazy_bind_off.get(NativeEndian) as usize;

                let lazy_bind_symbols = mach_object::LazyBind::parse(
                    &exec_data[lazy_bind_offset..],
                    mem::size_of::<usize>(),
                );

                // Find all the lazily-bound roc symbols
                // (e.g. "_roc__mainForHost_1_exposed")
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
                let info = load_struct_inplace::<DylibCommand<LittleEndian>>(exec_data, offset);
                let name_offset = info.dylib.name.offset.get(NativeEndian) as usize;
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
                        let c_str = unsafe { CStr::from_ptr(str_bytes.as_ptr() as *const i8) };

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
        println!("App Function Address Map: {:+x?}", app_func_addresses);
    }
    let symbol_and_plt_processing_duration = symbol_and_plt_processing_start.elapsed();

    // look at the text (i.e. code) sections and see collect work needs to be done
    let text_disassembly_start = Instant::now();

    let mut surgeries = Surgeries::new(&app_syms, app_func_addresses);
    surgeries.append_text_sections(exec_data, &exec_obj, verbose);
    md.surgeries = surgeries.surgeries;

    let text_disassembly_duration = text_disassembly_start.elapsed();

    let scanning_dynamic_deps_duration;
    let platform_gen_start;

    let out_mmap = {
        match target
            .endianness()
            .unwrap_or(target_lexicon::Endianness::Little)
        {
            target_lexicon::Endianness::Little => {
                let scanning_dynamic_deps_start = Instant::now();

                // let ElfDynamicDeps {
                //     got_app_syms,
                //     got_sections,
                //     dynamic_lib_count,
                //     shared_lib_index,
                // } = scan_elf_dynamic_deps(
                //     &exec_obj, &mut md, &app_syms, shared_lib, exec_data, verbose,
                // );

                scanning_dynamic_deps_duration = scanning_dynamic_deps_start.elapsed();

                platform_gen_start = Instant::now();

                // TODO little endian
                let macho_load_so_offset = match macho_load_so_offset {
                    Some(offset) => offset,
                    None => {
                        internal_error!("Host does not link library `{}`!", shared_lib.display());
                    }
                };

                // TODO this is correct on modern Macs (they align to the page size)
                // but maybe someone can override the alignment somehow? Maybe in the
                // future this could change? Is there some way to make this more future-proof?
                md.load_align_constraint = 4096;

                gen_macho_le(
                    exec_data,
                    &mut md,
                    preprocessed_path,
                    macho_load_so_offset,
                    target,
                    verbose,
                )
            }
            target_lexicon::Endianness::Big => {
                // TODO Is big-endian macOS even a thing that exists anymore?
                // Just ancient PowerPC machines maybe?
                todo!("Roc does not yet support big-endian macOS hosts!");
            }
        }
    };

    let platform_gen_duration = platform_gen_start.elapsed();

    if verbose {
        println!();
        println!("{:+x?}", md);
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
    md: &mut metadata::Metadata,
    out_filename: &Path,
    macho_load_so_offset: usize,
    _target: &Triple,
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

    let exec_header = load_struct_inplace::<macho::MachHeader64<LittleEndian>>(exec_data, 0);
    let num_load_cmds = exec_header.ncmds.get(NativeEndian);
    let size_of_cmds = exec_header.sizeofcmds.get(NativeEndian) as usize;

    // Add a new text segment and data segment
    let segment_cmd_size = mem::size_of::<SegmentCommand64<LittleEndian>>();
    let section_size = mem::size_of::<Section64<LittleEndian>>();

    // We need the full command size, including the dynamic-length string at the end.
    // To get that, we need to load the command.
    let info =
        load_struct_inplace::<macho::LoadCommand<LittleEndian>>(exec_data, macho_load_so_offset);
    let total_cmd_size = info.cmdsize.get(NativeEndian) as usize;

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

    let out_header = load_struct_inplace_mut::<macho::MachHeader64<LittleEndian>>(&mut out_mmap, 0);

    // TODO: this needs to change to adding the 2 new commands when we are ready.
    // -1 because we're deleting 1 load command and then NOT adding 2 new ones.
    {
        let added_bytes = -(total_cmd_size as isize); // TODO: Change when add the new sections.
        out_header.ncmds.set(LittleEndian, num_load_cmds - 1);
        out_header
            .sizeofcmds
            .set(LittleEndian, (size_of_cmds as isize + added_bytes) as u32);
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
        let info = load_struct_inplace::<macho::LoadCommand<LittleEndian>>(&out_mmap, offset);
        let cmd_size = info.cmdsize.get(NativeEndian) as usize;

        match info.cmd.get(NativeEndian) {
            macho::LC_SEGMENT_64 => {
                let cmd = load_struct_inplace_mut::<macho::SegmentCommand64<LittleEndian>>(
                    &mut out_mmap,
                    offset,
                );

                // Ignore page zero, it never moves.
                if cmd.segname == "__PAGEZERO\0\0\0\0\0\0".as_bytes()
                    || cmd.vmaddr.get(NativeEndian) == 0
                {
                    offset += cmd_size;
                    continue;
                }

                let old_file_offest = cmd.fileoff.get(NativeEndian);
                // The segment with file offset zero also includes the header.
                // As such, its file offset does not change.
                // Instead, its file size should be increased.
                if old_file_offest > 0 {
                    cmd.fileoff
                        .set(LittleEndian, old_file_offest + md.added_byte_count as u64);
                    cmd.vmaddr.set(
                        LittleEndian,
                        cmd.vmaddr.get(NativeEndian) + md.added_byte_count as u64,
                    );
                } else {
                    cmd.filesize.set(
                        LittleEndian,
                        cmd.filesize.get(NativeEndian) + md.added_byte_count as u64,
                    );
                    cmd.vmsize.set(
                        LittleEndian,
                        cmd.vmsize.get(NativeEndian) + md.added_byte_count as u64,
                    );
                }

                // let num_sections = cmd.nsects.get(NativeEndian);
                // let sections = load_structs_inplace_mut::<macho::Section64<LittleEndian>>(
                //     &mut out_mmap,
                //     offset + mem::size_of::<macho::SegmentCommand64<LittleEndian>>(),
                //     num_sections as usize,
                // );
                // struct Relocation {
                //     offset: u32,
                //     num_relocations: u32,
                // }

                // let mut relocation_offsets = Vec::with_capacity(sections.len());

                // for section in sections {
                //     section.addr.set(
                //         LittleEndian,
                //         section.addr.get(NativeEndian) + md.added_byte_count as u64,
                //     );

                //     // If offset is zero, don't update it.
                //     // Zero is used for things like BSS that don't exist in the file.
                //     let old_offset = section.offset.get(NativeEndian);
                //     if old_offset > 0 {
                //         section
                //             .offset
                //             .set(LittleEndian, old_offset + md.added_byte_count as u32);
                //     }

                //     // dbg!(&section.reloff.get(NativeEndian));
                //     // dbg!(section.reloff.get(NativeEndian) as i32);
                //     // dbg!(&section);
                //     // dbg!(&md.added_byte_count);
                //     // dbg!(String::from_utf8_lossy(&section.sectname));
                //     if section.nreloc.get(NativeEndian) > 0 {
                //         section.reloff.set(
                //             LittleEndian,
                //             section.reloff.get(NativeEndian) + md.added_byte_count as u32,
                //         );
                //     }

                //     relocation_offsets.push(Relocation {
                //         offset: section.reloff.get(NativeEndian),
                //         num_relocations: section.nreloc.get(NativeEndian),
                //     });
                // }

                // TODO FIXME this is necessary for ARM, but seems to be broken. Skipping for now since we're just targeting x86
                // for Relocation {
                //     offset,
                //     num_relocations,
                // } in relocation_offsets
                // {
                //     let relos = load_structs_inplace_mut::<macho::Relocation<LittleEndian>>(
                //         &mut out_mmap,
                //         offset as usize,
                //         num_relocations as usize,
                //     );

                //     // TODO this has never been tested, because scattered relocations only come up on ARM!
                //     for relo in relos.iter_mut() {
                //         if relo.r_scattered(LittleEndian, cpu_type) {
                //             let mut scattered_info = relo.scattered_info(NativeEndian);

                //             if !scattered_info.r_pcrel {
                //                 scattered_info.r_value += md.added_byte_count as u32;

                //                 let new_info = scattered_info.relocation(LittleEndian);

                //                 relo.r_word0 = new_info.r_word0;
                //                 relo.r_word1 = new_info.r_word1;
                //             }
                //         }
                //     }
                // }

                // TODO this seems to be wrong and unnecessary, and should probably be deleted.
                // offset += num_sections as usize * mem::size_of::<macho::Section64<LittleEndian>>();
            }
            macho::LC_SYMTAB => {
                let cmd = load_struct_inplace_mut::<macho::SymtabCommand<LittleEndian>>(
                    &mut out_mmap,
                    offset,
                );

                let sym_offset = cmd.symoff.get(NativeEndian);
                let num_syms = cmd.nsyms.get(NativeEndian);

                if num_syms > 0 {
                    cmd.symoff
                        .set(LittleEndian, sym_offset + md.added_byte_count as u32);
                }

                if cmd.strsize.get(NativeEndian) > 0 {
                    cmd.stroff.set(
                        LittleEndian,
                        cmd.stroff.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }

                let table = load_structs_inplace_mut::<macho::Nlist64<LittleEndian>>(
                    &mut out_mmap,
                    sym_offset as usize + md.added_byte_count as usize,
                    num_syms as usize,
                );

                for entry in table {
                    let entry_type = entry.n_type & macho::N_TYPE;
                    if entry_type == macho::N_ABS || entry_type == macho::N_SECT {
                        entry.n_value.set(
                            LittleEndian,
                            entry.n_value.get(NativeEndian) + md.added_byte_count as u64,
                        );
                    }
                }
            }
            macho::LC_DYSYMTAB => {
                let cmd = load_struct_inplace_mut::<macho::DysymtabCommand<LittleEndian>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.ntoc.get(NativeEndian) > 0 {
                    cmd.tocoff.set(
                        LittleEndian,
                        cmd.tocoff.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }

                if cmd.nmodtab.get(NativeEndian) > 0 {
                    cmd.modtaboff.set(
                        LittleEndian,
                        cmd.modtaboff.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }

                if cmd.nextrefsyms.get(NativeEndian) > 0 {
                    cmd.extrefsymoff.set(
                        LittleEndian,
                        cmd.extrefsymoff.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }

                if cmd.nindirectsyms.get(NativeEndian) > 0 {
                    cmd.indirectsymoff.set(
                        LittleEndian,
                        cmd.indirectsymoff.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }

                if cmd.nextrel.get(NativeEndian) > 0 {
                    cmd.extreloff.set(
                        LittleEndian,
                        cmd.extreloff.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }

                if cmd.nlocrel.get(NativeEndian) > 0 {
                    cmd.locreloff.set(
                        LittleEndian,
                        cmd.locreloff.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }

                // TODO maybe we need to update something else too - relocations maybe?
                // I think this also has symbols that need to get moved around.
                // Look at otool -I at least for the indirect symbols.
            }
            macho::LC_TWOLEVEL_HINTS => {
                let cmd = load_struct_inplace_mut::<macho::TwolevelHintsCommand<LittleEndian>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.nhints.get(NativeEndian) > 0 {
                    cmd.offset.set(
                        LittleEndian,
                        cmd.offset.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }
            }
            macho::LC_FUNCTION_STARTS => {
                let cmd = load_struct_inplace_mut::<macho::LinkeditDataCommand<LittleEndian>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.datasize.get(NativeEndian) > 0 {
                    cmd.dataoff.set(
                        LittleEndian,
                        cmd.dataoff.get(NativeEndian) + md.added_byte_count as u32,
                    );
                    // TODO: This lists the start of every function. Which, of course, have moved.
                    // That being said, to my understanding this section is optional and may just be debug information.
                    // As such, updating it should not be required.
                    // It will be more work to update due to being in "DWARF-style ULEB128" values.
                }
            }
            macho::LC_DATA_IN_CODE => {
                let (offset, size) = {
                    let cmd = load_struct_inplace_mut::<macho::LinkeditDataCommand<LittleEndian>>(
                        &mut out_mmap,
                        offset,
                    );

                    if cmd.datasize.get(NativeEndian) > 0 {
                        cmd.dataoff.set(
                            LittleEndian,
                            cmd.dataoff.get(NativeEndian) + md.added_byte_count as u32,
                        );
                    }
                    (
                        cmd.dataoff.get(NativeEndian),
                        cmd.datasize.get(NativeEndian),
                    )
                };

                // Update every data in code entry.
                if size > 0 {
                    let entry_size = mem::size_of::<macho::DataInCodeEntry<LittleEndian>>();
                    let entries = load_structs_inplace_mut::<macho::DataInCodeEntry<LittleEndian>>(
                        &mut out_mmap,
                        offset as usize,
                        size as usize / entry_size,
                    );
                    for entry in entries.iter_mut() {
                        entry.offset.set(
                            LittleEndian,
                            entry.offset.get(NativeEndian) + md.added_byte_count as u32,
                        )
                    }
                }
            }
            macho::LC_CODE_SIGNATURE
            | macho::LC_SEGMENT_SPLIT_INFO
            | macho::LC_DYLIB_CODE_SIGN_DRS
            | macho::LC_LINKER_OPTIMIZATION_HINT
            | macho::LC_DYLD_EXPORTS_TRIE
            | macho::LC_DYLD_CHAINED_FIXUPS => {
                let cmd = load_struct_inplace_mut::<macho::LinkeditDataCommand<LittleEndian>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.datasize.get(NativeEndian) > 0 {
                    cmd.dataoff.set(
                        LittleEndian,
                        cmd.dataoff.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }
            }
            macho::LC_ENCRYPTION_INFO_64 => {
                let cmd = load_struct_inplace_mut::<macho::EncryptionInfoCommand64<LittleEndian>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.cryptsize.get(NativeEndian) > 0 {
                    cmd.cryptoff.set(
                        LittleEndian,
                        cmd.cryptoff.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }
            }
            macho::LC_DYLD_INFO | macho::LC_DYLD_INFO_ONLY => {
                let cmd = load_struct_inplace_mut::<macho::DyldInfoCommand<LittleEndian>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.rebase_size.get(NativeEndian) > 0 {
                    cmd.rebase_off.set(
                        LittleEndian,
                        cmd.rebase_off.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }

                if cmd.bind_size.get(NativeEndian) > 0 {
                    cmd.bind_off.set(
                        LittleEndian,
                        cmd.bind_off.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }

                if cmd.weak_bind_size.get(NativeEndian) > 0 {
                    cmd.weak_bind_off.set(
                        LittleEndian,
                        cmd.weak_bind_off.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }

                if cmd.lazy_bind_size.get(NativeEndian) > 0 {
                    cmd.lazy_bind_off.set(
                        LittleEndian,
                        cmd.lazy_bind_off.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }

                // TODO: Parse and update the related tables here.
                // It is possible we may just need to delete things that point to stuff that will be in the roc app.
                // We also may just be able to ignore it (lazy bindings should never run).
                // This definitely has a list of virtual address that need to be updated.
                // Some of them definitely will point to the roc app and should probably be removed.
                // Also `xcrun dyldinfo` is useful for debugging this.
            }
            macho::LC_SYMSEG => {
                let cmd = load_struct_inplace_mut::<macho::SymsegCommand<LittleEndian>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.size.get(NativeEndian) > 0 {
                    cmd.offset.set(
                        LittleEndian,
                        cmd.offset.get(NativeEndian) + md.added_byte_count as u32,
                    );
                }
            }
            macho::LC_MAIN => {
                let cmd = load_struct_inplace_mut::<macho::EntryPointCommand<LittleEndian>>(
                    &mut out_mmap,
                    offset,
                );

                cmd.entryoff.set(
                    LittleEndian,
                    cmd.entryoff.get(NativeEndian) + md.added_byte_count as u64,
                );
            }
            macho::LC_NOTE => {
                let cmd = load_struct_inplace_mut::<macho::NoteCommand<LittleEndian>>(
                    &mut out_mmap,
                    offset,
                );

                if cmd.size.get(NativeEndian) > 0 {
                    cmd.offset.set(
                        LittleEndian,
                        cmd.offset.get(NativeEndian) + md.added_byte_count as u64,
                    );
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
                    "- - - Unrecognized Mach-O command during linker preprocessing: 0x{:x?}",
                    cmd
                );
                // panic!(
                //     "Unrecognized Mach-O command during linker preprocessing: 0x{:x?}",
                //     cmd
                // );
            }
        }

        offset += dbg!(cmd_size);
    }

    // cmd_loc should be where the last offset ended
    md.macho_cmd_loc = offset as u64;

    out_mmap
}

// fn scan_macho_dynamic_deps(
//     _exec_obj: &object::File,
//     _md: &mut metadata::Metadata,
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
    md: &metadata::Metadata,
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

    // TODO: In the future Roc may use a data section to store memoized toplevel thunks
    // in development builds for caching the results of top-level constants

    let rodata_sections: Vec<Section> = app_obj
        .sections()
        .filter(|sec| sec.kind() == SectionKind::ReadOnlyData)
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
        if sec.name().unwrap_or_default().starts_with("__BSS") {
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
        println!("Data Relocation Offsets: {:+x?}", symbol_vaddr_map);
        println!("Found App Function Symbols: {:+x?}", app_func_vaddr_map);
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
        .chain(text_sections.iter())
    {
        let data = match sec.data() {
            Ok(data) => data,
            Err(err) => {
                internal_error!(
                    "Failed to load data for section, {:+x?}: {}",
                    sec.name().unwrap(),
                    err
                );
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
                "Processing Relocations for Section: 0x{:+x?} @ {:+x} (virt: {:+x})",
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
                                internal_error!("Relocation Kind not yet support: {:?}", x);
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
                                internal_error!("Relocation size not yet supported: {}", x);
                            }
                        }
                    } else if matches!(app_obj.symbol_by_index(index), Ok(sym) if ["__divti3", "__udivti3", "___divti3", "___udivti3"].contains(&sym.name().unwrap_or_default()))
                    {
                        // Explicitly ignore some symbols that are currently always linked.
                        continue;
                    } else {
                        internal_error!(
                            "Undefined Symbol in relocation, {:+x?}: {:+x?}",
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

    // Flush app only data to speed up write to disk.
    exec_mmap
        .flush_async_range(md.exec_len as usize, offset - md.exec_len as usize)
        .unwrap_or_else(|e| internal_error!("{}", e));

    // TODO: look into merging symbol tables, debug info, and eh frames to enable better debugger experience.

    // let mut cmd_offset = md.macho_cmd_loc as usize;

    // // Load this section (we made room for it earlier) and then mutate all its data to make it the desired command
    // {
    //     let cmd =
    //         load_struct_inplace_mut::<macho::SegmentCommand64<LittleEndian>>(exec_mmap, cmd_offset);
    //     let size_of_section = mem::size_of::<macho::Section64<LittleEndian>>() as u32;
    //     let size_of_cmd = mem::size_of_val(cmd);

    //     cmd_offset += size_of_cmd;

    //     cmd.cmd.set(LittleEndian, macho::LC_SEGMENT_64);
    //     cmd.cmdsize
    //         .set(LittleEndian, size_of_section + size_of_cmd as u32);
    //     cmd.segname = *b"__DATA_CONST\0\0\0\0";
    //     cmd.vmaddr
    //         .set(LittleEndian, new_rodata_section_vaddr as u64);
    //     cmd.vmsize.set(
    //         LittleEndian,
    //         (new_text_section_vaddr - new_rodata_section_vaddr) as u64,
    //     );
    //     cmd.fileoff
    //         .set(LittleEndian, new_rodata_section_offset as u64);
    //     cmd.filesize.set(
    //         LittleEndian,
    //         (new_text_section_offset - new_rodata_section_offset) as u64,
    //     );
    //     cmd.nsects.set(LittleEndian, 1);
    //     cmd.maxprot.set(LittleEndian, 0x00000003);
    //     cmd.initprot.set(LittleEndian, 0x00000003);

    //     // TODO set protection
    // }

    // {
    //     let cmd = load_struct_inplace_mut::<macho::Section64<LittleEndian>>(exec_mmap, cmd_offset);
    //     let size_of_cmd = mem::size_of_val(cmd);

    //     cmd_offset += size_of_cmd;

    //     cmd.sectname = *b"__const\0\0\0\0\0\0\0\0\0";
    //     cmd.segname = *b"__DATA_CONST\0\0\0\0";
    //     cmd.addr.set(LittleEndian, new_rodata_section_vaddr as u64);
    //     cmd.size.set(
    //         LittleEndian,
    //         (new_text_section_offset - new_rodata_section_offset) as u64,
    //     );
    //     cmd.offset.set(LittleEndian, 0); // TODO is this offset since the start of the file, or segment offset?
    //     cmd.align.set(LittleEndian, 12); // TODO should this be 4096?
    //     cmd.reloff.set(LittleEndian, 264); // TODO this should NOT be hardcoded! Should get it from somewhere.
    // }

    // {
    //     let cmd =
    //         load_struct_inplace_mut::<macho::SegmentCommand64<LittleEndian>>(exec_mmap, cmd_offset);
    //     let size_of_section = mem::size_of::<macho::Section64<LittleEndian>>() as u32;
    //     let size_of_cmd = mem::size_of_val(cmd);

    //     cmd_offset += size_of_cmd;

    //     cmd.cmd.set(LittleEndian, macho::LC_SEGMENT_64);
    //     cmd.cmdsize
    //         .set(LittleEndian, size_of_section + size_of_cmd as u32);
    //     cmd.segname = *b"__TEXT\0\0\0\0\0\0\0\0\0\0";
    //     cmd.vmaddr.set(LittleEndian, new_text_section_vaddr as u64);
    //     cmd.vmsize
    //         .set(LittleEndian, (offset - new_text_section_offset) as u64);
    //     cmd.fileoff
    //         .set(LittleEndian, new_text_section_offset as u64);
    //     cmd.filesize
    //         .set(LittleEndian, (offset - new_text_section_offset) as u64);
    //     cmd.nsects.set(LittleEndian, 1);
    //     cmd.maxprot.set(LittleEndian, 0x00000005); // this is what a zig-generated host had
    //     cmd.initprot.set(LittleEndian, 0x00000005); // this is what a zig-generated host had
    // }

    // {
    //     let cmd = load_struct_inplace_mut::<macho::Section64<LittleEndian>>(exec_mmap, cmd_offset);

    //     cmd.segname = *b"__TEXT\0\0\0\0\0\0\0\0\0\0";
    //     cmd.sectname = *b"__text\0\0\0\0\0\0\0\0\0\0";
    //     cmd.addr.set(LittleEndian, new_text_section_vaddr as u64);
    //     cmd.size
    //         .set(LittleEndian, (offset - new_text_section_offset) as u64);
    //     cmd.offset.set(LittleEndian, 0); // TODO is this offset since the start of the file, or segment offset?
    //     cmd.align.set(LittleEndian, 12); // TODO this is 4096 (2^12) - which load_align_constraint does, above - but should it?
    //     cmd.flags.set(LittleEndian, 0x80000400); // TODO this is what a zig-generated host had
    //     cmd.reloff.set(LittleEndian, 264); // TODO this should NOT be hardcoded! Should get it from somewhere.
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

        // Commented out because it doesn't apply to mach-o
        // if let Some(i) = md.dynamic_symbol_indices.get(func_name) {
        //     let sym = load_struct_inplace_mut::<elf::Sym64<LittleEndian>>(
        //         exec_mmap,
        //         dynsym_offset as usize + *i as usize * mem::size_of::<elf::Sym64<LittleEndian>>(),
        //     );
        //     sym.st_value = endian::U64::new(LittleEndian, func_virt_offset as u64);
        //     sym.st_size = endian::U64::new(
        //         LittleEndian,
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
