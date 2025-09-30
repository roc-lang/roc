use std::{
    io::{BufReader, BufWriter},
    path::Path,
};

use bincode::{deserialize_from, serialize_into};
use memmap2::MmapMut;
use object::{
    pe::{
        self, ImageBaseRelocation, ImageFileHeader, ImageImportDescriptor, ImageNtHeaders64,
        ImageSectionHeader, ImageThunkData64,
    },
    read::pe::ImportTable,
    LittleEndian as LE, Object, ObjectSection, ObjectSymbol, RelocationTarget, SectionIndex,
};
use serde::{Deserialize, Serialize};

use roc_collections::{MutMap, VecMap};
use roc_error_macros::internal_error;

use crate::{
    generate_dylib::APP_DLL, load_struct_inplace, load_struct_inplace_mut,
    load_structs_inplace_mut, open_mmap, open_mmap_mut, util::is_roc_definition,
};

/// The metadata stores information about/from the host .exe because
///
/// - it is faster to retrieve than parsing the host .exe on every link
/// - the information is erased from the host .exe to make linking faster
///
/// For instance, we remove our dummy .dll from the import table, but its dynamic relocations are
/// still somewhere in the host .exe. We use the metadata to store this offset, so at link time we
/// can make modifications at that location.
///
/// PeMetadata is created during preprocessing and stored to disk.
#[derive(Debug, Serialize, Deserialize)]
struct PeMetadata {
    dynhost_file_size: usize,

    last_host_section_size: u64,
    last_host_section_address: u64,

    /// Number of sections in the unmodified host .exe
    host_section_count: usize,

    optional_header_offset: usize,

    dynamic_relocations: DynamicRelocationsPe,

    /// File offset for the thunks of our dummy .dll
    thunks_start_offset_in_file: usize,

    /// Offset for the thunks of our dummy .dll within the .rdata section
    thunks_start_offset_in_section: usize,

    /// Virtual address of the .rdata section
    dummy_dll_thunk_section_virtual_address: u32,

    /// The offset into the file of the .reloc section
    reloc_offset_in_file: usize,

    reloc_section_index: usize,

    /// Constants from the host .exe header
    image_base: u64,
    file_alignment: u32,
    section_alignment: u32,

    /// Symbols that the host imports, like roc__mainForHost_1_exposed_generic
    imports: Vec<String>,

    /// Symbols that the host exports, like roc_alloc
    exports: MutMap<String, i64>,
}

impl PeMetadata {
    fn write_to_file(&self, metadata_filename: &Path) {
        let metadata_file =
            std::fs::File::create(metadata_filename).unwrap_or_else(|e| internal_error!("{}", e));

        serialize_into(BufWriter::new(metadata_file), self)
            .unwrap_or_else(|err| internal_error!("Failed to serialize metadata: {err}"));
    }

    fn read_from_file(metadata_filename: &Path) -> Self {
        let input =
            std::fs::File::open(metadata_filename).unwrap_or_else(|e| internal_error!("{}", e));

        match deserialize_from(BufReader::new(input)) {
            Ok(data) => data,
            Err(err) => {
                internal_error!("Failed to deserialize metadata: {}", err);
            }
        }
    }

    fn from_preprocessed_host(preprocessed_data: &[u8], new_sections: &[[u8; 8]]) -> Self {
        let dynhost_obj = object::read::pe::PeFile64::parse(preprocessed_data)
            .unwrap_or_else(|err| internal_error!("Failed to parse executable file: {}", err));

        let host_section_count = dynhost_obj.sections().count() - new_sections.len();
        let last_host_section = dynhost_obj
            .sections()
            .nth(host_section_count - 1) // -1 because we have a count and want an index
            .unwrap();

        let dynamic_relocations = DynamicRelocationsPe::new(preprocessed_data);

        let dummy_dll_thunks = find_thunks_start_offset(preprocessed_data, &dynamic_relocations);
        let thunks_start_offset_in_file = dummy_dll_thunks.offset_in_file as usize;

        let dummy_dll_thunk_section_virtual_address =
            dummy_dll_thunks.section.virtual_address.get(LE);

        let thunks_start_offset_in_section = (dummy_dll_thunks.offset_in_file
            - dummy_dll_thunks.section.pointer_to_raw_data.get(LE) as u64)
            as usize;

        let (reloc_section_index, reloc_section) = dynhost_obj
            .sections()
            .enumerate()
            .find(|(_, s)| s.name() == Ok(".reloc"))
            .unwrap();

        let reloc_offset_in_file = reloc_section.file_range().unwrap().0 as usize;

        let optional_header = dynhost_obj.nt_headers().optional_header;
        let optional_header_offset = dynhost_obj.dos_header().nt_headers_offset() as usize
            + std::mem::size_of::<u32>()
            + std::mem::size_of::<ImageFileHeader>();

        let exports: MutMap<String, i64> = dynhost_obj
            .exports()
            .unwrap()
            .into_iter()
            .map(|e| {
                (
                    String::from_utf8(e.name().to_vec()).unwrap(),
                    (e.address() - optional_header.image_base.get(LE)) as i64,
                )
            })
            .collect();

        let imports: Vec<_> = dynhost_obj
            .imports()
            .unwrap()
            .iter()
            .filter(|import| import.library() == APP_DLL.as_bytes())
            .map(|import| {
                std::str::from_utf8(import.name())
                    .unwrap_or_default()
                    .to_owned()
            })
            .collect();

        let last_host_section_size = last_host_section.size();
        let last_host_section_address = last_host_section.address();

        PeMetadata {
            dynhost_file_size: preprocessed_data.len(),
            image_base: optional_header.image_base.get(LE),
            file_alignment: optional_header.file_alignment.get(LE),
            section_alignment: optional_header.section_alignment.get(LE),
            last_host_section_size,
            last_host_section_address,
            host_section_count,
            optional_header_offset,
            imports,
            exports,
            dynamic_relocations,
            thunks_start_offset_in_file,
            thunks_start_offset_in_section,
            dummy_dll_thunk_section_virtual_address,
            reloc_offset_in_file,
            reloc_section_index,
        }
    }
}

pub(crate) fn preprocess_windows(
    host_exe_filename: &Path,
    metadata_filename: &Path,
    preprocessed_filename: &Path,
    shared_lib: &Path,
    _verbose: bool,
    _time: bool,
) -> object::read::Result<()> {
    let exec_data = open_mmap(host_exe_filename);
    let shared_lib_data = &*open_mmap(shared_lib);
    let shared_lib_obj = match object::File::parse(shared_lib_data) {
        Ok(obj) => obj,
        Err(e) => internal_error!("Failed to parse shared library file: {e}"),
    };
    let dummy_dll_symbols = shared_lib_obj.symbols().filter(is_roc_definition).count();

    let new_sections = [*b".text\0\0\0", *b".rdata\0\0"];
    let mut preprocessed = Preprocessor::preprocess(
        preprocessed_filename,
        &exec_data,
        dummy_dll_symbols,
        &new_sections,
    );

    // get the metadata from the preprocessed executable before the destructive operations below
    let md = PeMetadata::from_preprocessed_host(&preprocessed, &new_sections);

    // in the data directories, update the length of the imports (there is one fewer now)
    {
        let start = md.dynamic_relocations.data_directories_offset_in_file as usize
            + object::pe::IMAGE_DIRECTORY_ENTRY_IMPORT
                * std::mem::size_of::<pe::ImageDataDirectory>();

        let dir = load_struct_inplace_mut::<pe::ImageDataDirectory>(&mut preprocessed, start);

        let new = dir.size.get(LE) - std::mem::size_of::<pe::ImageImportDescriptor>() as u32;
        dir.size.set(LE, new);
    }

    remove_dummy_dll_import_table_entry(&mut preprocessed, &md);

    md.write_to_file(metadata_filename);

    Ok(())
}

fn remove_dummy_dll_import_table_entry(executable: &mut [u8], md: &PeMetadata) {
    const W: usize = std::mem::size_of::<ImageImportDescriptor>();

    let dr = &md.dynamic_relocations;

    // there is one zeroed-out descriptor at the back
    let count = dr.import_directory_size as usize / W - 1;

    let descriptors = load_structs_inplace_mut::<ImageImportDescriptor>(
        executable,
        dr.import_directory_offset_in_file as usize,
        count,
    );

    // move the dummy to the final position
    descriptors.swap(dr.dummy_import_index as usize, count - 1);

    // make this the new zeroed-out descriptor
    if let Some(d) = descriptors.last_mut() {
        *d = ImageImportDescriptor {
            original_first_thunk: Default::default(),
            time_date_stamp: Default::default(),
            forwarder_chain: Default::default(),
            name: Default::default(),
            first_thunk: Default::default(),
        }
    }
}

fn relocate_to(
    executable: &mut [u8],
    file_offset: usize,
    destination_in_file: i64,
    relocation: &object::Relocation,
) {
    match relocation.size() {
        32 => {
            let slice = &mut executable[file_offset..][..4];
            let implicit = if relocation.has_implicit_addend() {
                i32::from_le_bytes(slice.try_into().unwrap())
            } else {
                0
            };

            let delta = destination_in_file + relocation.addend() + implicit as i64;

            slice.copy_from_slice(&(delta as i32).to_le_bytes());
        }

        64 => {
            let slice = &mut executable[file_offset..][..8];
            let implicit = if relocation.has_implicit_addend() {
                i64::from_le_bytes(slice.try_into().unwrap())
            } else {
                0
            };

            let delta = destination_in_file + relocation.addend() + implicit;

            slice.copy_from_slice(&delta.to_le_bytes());
        }

        other => unimplemented!("relocations of {other} bits are not supported"),
    }
}

pub(crate) fn surgery_pe(executable_path: &Path, metadata_path: &Path, roc_app_bytes: &[u8]) {
    let md = PeMetadata::read_from_file(metadata_path);

    let app_obj_sections = AppSections::from_data(roc_app_bytes);

    let mut symbols = app_obj_sections.roc_symbols;

    let image_base: u64 = md.image_base;
    let file_alignment = md.file_alignment as usize;
    let section_alignment = md.section_alignment as usize;

    let app_sections_size: usize = app_obj_sections
        .sections
        .iter()
        .map(|s| next_multiple_of(s.bytes.len(), file_alignment))
        .sum();

    let executable = &mut open_mmap_mut(executable_path, md.dynhost_file_size + app_sections_size);

    let app_code_section_va = md.last_host_section_address
        + next_multiple_of(md.last_host_section_size as usize, section_alignment) as u64;

    let mut section_file_offset = md.dynhost_file_size;
    let mut section_virtual_address = (app_code_section_va - image_base) as u32;

    // find the location to write the section headers for our new sections
    let mut section_header_start = md.dynamic_relocations.section_headers_offset_in_file as usize
        + md.host_section_count * std::mem::size_of::<ImageSectionHeader>();

    relocate_dummy_dll_entries(executable, &md);

    let mut code_bytes_added = 0;
    let mut data_bytes_added = 0;
    let mut file_bytes_added = 0;

    // relocations between the sections of the roc application
    // (as opposed to relocations for symbols the app imports from the host)
    let inter_app_relocations = process_internal_relocations(
        &app_obj_sections.sections,
        &app_obj_sections.other_symbols,
        (app_code_section_va - image_base) as u32,
        section_alignment,
    );

    for kind in [SectionKind::Text, SectionKind::ReadOnlyData] {
        let length: usize = app_obj_sections
            .sections
            .iter()
            .filter(|s| s.kind == kind)
            .map(|s| s.bytes.len())
            .sum();

        // offset_in_section now becomes a proper virtual address
        for symbol in symbols.iter_mut() {
            if symbol.section_kind == kind {
                symbol.offset_in_section += image_base as usize + section_virtual_address as usize;
            }
        }

        // NOTE: sections cannot be zero in size!
        let virtual_size = u32::max(1, length as u32);
        let size_of_raw_data = next_multiple_of(length, file_alignment) as u32;

        match kind {
            SectionKind::Text => {
                code_bytes_added += size_of_raw_data;

                write_section_header(
                    executable,
                    *b".text1\0\0",
                    pe::IMAGE_SCN_MEM_READ | pe::IMAGE_SCN_CNT_CODE | pe::IMAGE_SCN_MEM_EXECUTE,
                    section_header_start,
                    section_file_offset,
                    virtual_size,
                    section_virtual_address,
                    size_of_raw_data,
                );
            }
            SectionKind::ReadOnlyData => {
                data_bytes_added += size_of_raw_data;

                write_section_header(
                    executable,
                    *b".rdata1\0",
                    pe::IMAGE_SCN_MEM_READ | pe::IMAGE_SCN_CNT_INITIALIZED_DATA,
                    section_header_start,
                    section_file_offset,
                    virtual_size,
                    section_virtual_address,
                    size_of_raw_data,
                );
            }
        }

        let mut offset = section_file_offset;
        let it = app_obj_sections.sections.iter().filter(|s| s.kind == kind);
        for section in it {
            let slice = section.bytes;
            executable[offset..][..slice.len()].copy_from_slice(slice);

            let it = section
                .relocations
                .iter()
                .flat_map(|(name, rs)| rs.iter().map(move |r| (name, r)));

            for (name, app_relocation) in it {
                let AppRelocation {
                    offset_in_section,
                    relocation,
                    address,
                } = app_relocation;

                if let Some(destination) = md.exports.get(name) {
                    match relocation.kind() {
                        object::RelocationKind::Relative => {
                            relocate_to(
                                executable,
                                offset + *offset_in_section as usize,
                                destination
                                    - section_virtual_address as i64
                                    - *offset_in_section as i64,
                                relocation,
                            );
                        }
                        _ => todo!(),
                    }
                } else if let Some(destination) = inter_app_relocations.get(name) {
                    relocate_to(
                        executable,
                        offset + *offset_in_section as usize,
                        destination - section_virtual_address as i64 - *offset_in_section as i64,
                        relocation,
                    );
                } else if name == "___chkstk_ms" {
                    // this is a stack probe that is inserted when a function uses more than 2
                    // pages of stack space. The source of this function is not linked in, so we
                    // have to do it ourselves. We patch in the bytes as a separate section, and
                    // here just need to jump to those bytes

                    // This relies on the ___CHKSTK_MS section being the last text section in the list of sections
                    let destination = length - ___CHKSTK_MS.len();

                    relocate_to(
                        executable,
                        offset + *offset_in_section as usize,
                        destination as i64 - *offset_in_section as i64,
                        relocation,
                    );
                } else {
                    let is_ingested_compiler_rt = [
                        "__addtf3",
                        "__ceilx",
                        "__cmpdf2",
                        "__cmphf2",
                        "__cmpsf2",
                        "__cmptf2",
                        "__cmpxf2",
                        "__cosx",
                        "__divsf3",
                        "__divtf3",
                        "__divti3",
                        "__exp2x",
                        "__expx",
                        "__extendhfsf2",
                        "__fabsx",
                        "__fixdfti",
                        "__fixsfti",
                        "__fixunsdfti",
                        "__fixunssfti",
                        "__floorx",
                        "__fmax",
                        "__fmaxx",
                        "__fminx",
                        "__fmodx",
                        "__gedf2",
                        "__gehf2",
                        "__gesf2",
                        "__getf2",
                        "__gexf2",
                        "__log10x",
                        "__log2x",
                        "__logx",
                        "__lshrti3",
                        "__modti3",
                        "__muloti4",
                        "__multf3",
                        "__roundx",
                        "__sincosx",
                        "__sinx",
                        "__sqrtx",
                        "__tanx",
                        "__truncsfhf2",
                        "__truncx",
                        "__udivmoddi4",
                        "__udivti3",
                        "__umodti3",
                        "ceilq",
                        "cos",
                        "cosf",
                        "cosq",
                        "exp",
                        "exp2",
                        "exp2q",
                        "expf",
                        "expq",
                        "floor",
                        "floorf",
                        "floorq",
                        "fmaq",
                        "fmaxf",
                        "fmaxl",
                        "fmodf",
                        "log10",
                        "log10q",
                        "log2",
                        "log2q",
                        "logq",
                        "memcpy",
                        "roundq",
                        "sin",
                        "sincos",
                        "sincosf",
                        "sincosq",
                        "sinf",
                        "sinq",
                        "sqrt",
                        "sqrtf",
                        "sqrtq",
                        "tan",
                        "tanf",
                        "tanq",
                    ]
                    .contains(&name.as_str());
                    if *address == 0 && !name.starts_with("roc") && !is_ingested_compiler_rt {
                        eprintln!(
                            "I don't know the address of the {name} function! this may cause segfaults"
                        );
                    }

                    match relocation.kind() {
                        object::RelocationKind::Relative => {
                            relocate_to(
                                executable,
                                offset + *offset_in_section as usize,
                                *address as i64 - *offset_in_section as i64,
                                relocation,
                            );
                        }
                        _ => todo!(),
                    }
                }
            }

            offset += slice.len();
        }

        section_header_start += std::mem::size_of::<ImageSectionHeader>();
        section_file_offset += size_of_raw_data as usize;
        section_virtual_address += next_multiple_of(length, section_alignment) as u32;
        file_bytes_added += next_multiple_of(length, section_alignment) as u32;
    }

    update_optional_header(
        executable,
        md.optional_header_offset,
        code_bytes_added,
        file_bytes_added,
        data_bytes_added,
    );

    let symbols: Vec<_> = symbols
        .into_iter()
        .map(|s| (s.name, s.offset_in_section as u64))
        .collect();

    redirect_dummy_dll_functions(
        executable,
        &symbols,
        &md.imports,
        md.thunks_start_offset_in_file,
    );
}

#[derive(Debug, Serialize, Deserialize)]
struct DynamicRelocationsPe {
    name_by_virtual_address: MutMap<u32, String>,

    /// Map symbol name to the virtual address and file offset
    /// where the actual address of this symbol is stored
    address_and_offset: MutMap<String, (u32, u32)>,

    /// Virtual offset of the first thunk
    section_virtual_address: u32,

    /// Offset in the file of the first thunk
    section_offset_in_file: u32,

    /// Offset in the file of the imports directory
    import_directory_offset_in_file: u32,

    /// Size in the file of the imports directory
    import_directory_size: u32,

    /// Offset in the file of the data directories
    data_directories_offset_in_file: u32,

    /// The dummy .dll is the `dummy_import_index`th import of the host .exe
    dummy_import_index: u32,

    /// Start of the first ImageSectionHeader of the file
    section_headers_offset_in_file: u64,
}

impl DynamicRelocationsPe {
    fn new(data: &[u8]) -> Self {
        Self::new_help(data).unwrap()
    }

    fn find_roc_dummy_dll(
        import_table: &ImportTable,
    ) -> object::read::Result<Option<(ImageImportDescriptor, u32)>> {
        let mut index = 0;
        let mut it = import_table.descriptors()?;
        while let Some(descriptor) = it.next()? {
            let name = import_table.name(descriptor.name.get(LE))?;

            if name == APP_DLL.as_bytes() {
                return Ok(Some((*descriptor, index)));
            }

            index += 1;
        }

        Ok(None)
    }

    /// Append metadata for the functions (e.g. mainForHost) that the host needs from the app
    fn append_roc_imports(
        &mut self,
        import_table: &ImportTable,
        roc_dll_descriptor: &ImageImportDescriptor,
    ) -> object::read::Result<()> {
        // offset of first thunk from the start of the section
        let thunk_start_offset =
            roc_dll_descriptor.original_first_thunk.get(LE) - self.section_virtual_address;
        let mut thunk_offset = 0;

        let mut thunks = import_table.thunks(roc_dll_descriptor.original_first_thunk.get(LE))?;
        while let Some(thunk_data) = thunks.next::<ImageNtHeaders64>()? {
            use object::read::pe::ImageThunkData;

            let temporary_address = thunk_data.address();
            let (_, name) = import_table.hint_name(temporary_address as _)?;
            let name = String::from_utf8_lossy(name).to_string();

            let offset_in_file = self.section_offset_in_file + thunk_start_offset + thunk_offset;
            let virtual_address = roc_dll_descriptor.original_first_thunk.get(LE) + thunk_offset;

            self.name_by_virtual_address
                .insert(virtual_address, name.clone());
            self.address_and_offset
                .insert(name, (virtual_address, offset_in_file));

            thunk_offset += std::mem::size_of::<ImageThunkData64>() as u32;
        }

        Ok(())
    }

    fn new_help(data: &[u8]) -> object::read::Result<Self> {
        use object::read::pe::ImageNtHeaders;

        let dos_header = pe::ImageDosHeader::parse(data)?;
        let mut offset = dos_header.nt_headers_offset().into();
        let data_directories_offset_in_file =
            offset as u32 + std::mem::size_of::<ImageNtHeaders64>() as u32;

        let (nt_headers, data_directories) = ImageNtHeaders64::parse(data, &mut offset)?;
        let sections = nt_headers.sections(data, offset)?;
        let section_headers_offset_in_file = offset;

        let data_dir = match data_directories.get(pe::IMAGE_DIRECTORY_ENTRY_IMPORT) {
            Some(data_dir) => data_dir,
            None => internal_error!("No dynamic imports, probably a bug"),
        };

        let import_va = data_dir.virtual_address.get(LE);

        let (section_data, section_va, offset_in_file) = sections
            .iter()
            .find_map(|section| {
                section
                    .pe_data_containing(data, import_va)
                    .map(|(section_data, section_va)| {
                        (
                            section_data,
                            section_va,
                            section.pointer_to_raw_data.get(LE),
                        )
                    })
            })
            .expect("Invalid import data dir virtual address");

        let import_table = ImportTable::new(section_data, section_va, import_va);

        let (import_directory_offset_in_file, import_directory_size) = data_dir
            .file_range(&sections)
            .expect("import directory exists");

        let (descriptor, dummy_import_index) = Self::find_roc_dummy_dll(&import_table)?.unwrap();

        let mut this = Self {
            name_by_virtual_address: Default::default(),
            address_and_offset: Default::default(),
            section_virtual_address: section_va,
            section_offset_in_file: offset_in_file,
            import_directory_offset_in_file,
            data_directories_offset_in_file,
            dummy_import_index,
            section_headers_offset_in_file,
            import_directory_size,
        };

        this.append_roc_imports(&import_table, &descriptor)?;

        Ok(this)
    }
}

/// Preprocess the host's .exe to make space for extra sections
///
/// We will later take code and data sections from the app and concatenate them with this
/// preprocessed host. That means we need to do some bookkeeping: add extra entries to the
/// section table, update the header with the new section count, and (because we added data)
/// update existing section headers to point to a different (shifted) location in the file
struct Preprocessor {
    header_offset: u64,
    additional_header_space: usize,
    additional_reloc_space: usize,
    extra_sections_start: usize,
    extra_sections_width: usize,
    section_count_offset: u64,
    section_table_offset: u64,
    new_section_count: usize,
    old_headers_size: usize,
    new_headers_size: usize,
    section_alignment: usize,
}

impl Preprocessor {
    const SECTION_HEADER_WIDTH: usize = std::mem::size_of::<ImageSectionHeader>();

    fn preprocess(
        output_path: &Path,
        data: &[u8],
        dummy_dll_symbols: usize,
        extra_sections: &[[u8; 8]],
    ) -> MmapMut {
        let this = Self::new(data, dummy_dll_symbols, extra_sections);
        let mut result = open_mmap_mut(
            output_path,
            data.len() + this.additional_header_space + this.additional_reloc_space,
        );

        this.copy(&mut result, data);
        this.fix(&mut result, extra_sections);

        result
    }

    fn new(data: &[u8], dummy_dll_symbols: usize, extra_sections: &[[u8; 8]]) -> Self {
        use object::read::pe::ImageNtHeaders;

        let dos_header = pe::ImageDosHeader::parse(data).unwrap_or_else(|e| internal_error!("{e}"));
        let mut offset = dos_header.nt_headers_offset().into();
        let header_offset = offset;
        let (nt_headers, _data_directories) =
            ImageNtHeaders64::parse(data, &mut offset).unwrap_or_else(|e| internal_error!("{e}"));
        let section_table_offset = offset;
        let sections = nt_headers
            .sections(data, offset)
            .unwrap_or_else(|e| internal_error!("{e}"));

        // recalculate the size of the headers. The size of the headers must be rounded up to the
        // next multiple of `file_alignment`.
        let old_headers_size = nt_headers.optional_header.size_of_headers.get(LE) as usize;
        let file_alignment = nt_headers.optional_header.file_alignment.get(LE) as usize;
        let section_alignment = nt_headers.optional_header.section_alignment.get(LE) as usize;
        let extra_sections_width = extra_sections.len() * Self::SECTION_HEADER_WIDTH;

        // in a better world `extra_sections_width.div_ceil(file_alignment)` would be stable
        // check on https://github.com/rust-lang/rust/issues/88581 some time in the future
        let extra_alignments = div_ceil(extra_sections_width, file_alignment);
        let new_headers_size = old_headers_size + extra_alignments * file_alignment;

        let additional_header_space = new_headers_size - old_headers_size;
        let additional_reloc_space =
            Self::additional_reloc_space(data, dummy_dll_symbols, file_alignment);

        Self {
            extra_sections_start: section_table_offset as usize
                + sections.len() * Self::SECTION_HEADER_WIDTH,
            extra_sections_width,
            additional_header_space,
            additional_reloc_space,
            header_offset,
            // the section count is stored 6 bytes into the header
            section_count_offset: header_offset + 4 + 2,
            section_table_offset,
            new_section_count: sections.len() + extra_sections.len(),
            old_headers_size,
            new_headers_size,
            section_alignment,
        }
    }

    fn additional_reloc_space(data: &[u8], extra_symbols: usize, file_alignment: usize) -> usize {
        let file = object::read::pe::PeFile64::parse(data).unwrap();

        let reloc_section = file
            .section_table()
            .iter()
            .find(|h| h.name == *b".reloc\0\0")
            .unwrap_or_else(|| internal_error!("host binary does not have a .reloc section"));

        // we use the virtual size here because it is more granular; the `size_of_raw_data` is
        // rounded up to the file alignment
        let available_space =
            reloc_section.size_of_raw_data.get(LE) - reloc_section.virtual_size.get(LE);

        // worst case, each relocation needs its own header
        let worst_case = std::mem::size_of::<ImageBaseRelocation>() + std::mem::size_of::<u16>();

        if available_space < (extra_symbols * worst_case) as u32 {
            // resize that section
            let new_size = next_multiple_of(
                reloc_section.virtual_size.get(LE) as usize + (extra_symbols * worst_case),
                file_alignment,
            );

            let delta = new_size - reloc_section.size_of_raw_data.get(LE) as usize;
            debug_assert_eq!(delta % file_alignment, 0);

            delta
        } else {
            0
        }
    }

    fn copy(&self, result: &mut MmapMut, data: &[u8]) {
        let extra_sections_start = self.extra_sections_start;

        // copy the headers up to and including the current section table entries
        // (but omitting the terminating NULL section table entry)
        result[..extra_sections_start].copy_from_slice(&data[..extra_sections_start]);

        // update the header with the new number of sections. From what I can gather, this number of
        // sections is usually ignored, and section table entry of all NULL fields is used to know when
        // the section table ends. But the rust `object` crate does use this number, and it seems like
        // a good idea to keep the header data accurate.
        result[self.section_count_offset as usize..][..2]
            .copy_from_slice(&(self.new_section_count as u16).to_le_bytes());

        // copy the rest of the headers
        let rest_of_headers = self.old_headers_size - extra_sections_start;
        result[extra_sections_start + self.extra_sections_width..][..rest_of_headers]
            .copy_from_slice(&data[extra_sections_start..][..rest_of_headers]);

        // copy all of the actual (post-header) data
        let source = &data[self.old_headers_size..];
        result[self.new_headers_size..][..source.len()].copy_from_slice(source);
    }

    fn write_dummy_sections(&self, result: &mut MmapMut, extra_section_names: &[[u8; 8]]) {
        const W: usize = std::mem::size_of::<ImageSectionHeader>();

        // only correct for the first section, but that is OK because it's overwritten later
        // anyway. But, this value may be used to check whether a previous section overruns into
        // the app sections.
        let pointer_to_raw_data = result.len() - self.additional_header_space;

        let previous_section_header =
            load_struct_inplace::<ImageSectionHeader>(result, self.extra_sections_start - W);

        let previous_section_header_end = previous_section_header.virtual_address.get(LE)
            + previous_section_header.virtual_size.get(LE);

        let mut next_virtual_address =
            next_multiple_of(previous_section_header_end as usize, self.section_alignment);

        let extra_section_headers = load_structs_inplace_mut::<ImageSectionHeader>(
            result,
            self.extra_sections_start,
            extra_section_names.len(),
        );

        for (header, name) in extra_section_headers.iter_mut().zip(extra_section_names) {
            *header = ImageSectionHeader {
                name: *name,
                // NOTE: the virtual_size CANNOT BE ZERO! the binary is invalid if a section has
                // zero virtual size. Setting it to 1 works, (because this one byte is not backed
                // up by space on disk, the loader will zero the memory if you run the executable)
                virtual_size: object::U32::new(LE, 1),
                // NOTE: this must be a valid virtual address, using 0 is invalid!
                virtual_address: object::U32::new(LE, next_virtual_address as u32),
                size_of_raw_data: Default::default(),
                pointer_to_raw_data: object::U32::new(LE, pointer_to_raw_data as u32),
                pointer_to_relocations: Default::default(),
                pointer_to_linenumbers: Default::default(),
                number_of_relocations: Default::default(),
                number_of_linenumbers: Default::default(),
                characteristics: Default::default(),
            };

            next_virtual_address += self.section_alignment;
        }
    }

    fn fix(&self, result: &mut MmapMut, extra_sections: &[[u8; 8]]) {
        self.write_dummy_sections(result, extra_sections);

        // update the size of the headers
        //
        // Time/Date		Wed Sep 14 16:04:36 2022
        // Magic			020b	(PE32+)
        // MajorLinkerVersion	14
        // MinorLinkerVersion	0
        // ...
        // SizeOfImage		    00067000
        // SizeOfHeaders		00000400
        // ...
        //
        // we added extra bytes to the headers, so this value must be updated
        let nt_headers =
            load_struct_inplace_mut::<ImageNtHeaders64>(result, self.header_offset as _);
        nt_headers
            .optional_header
            .size_of_headers
            .set(LE, self.new_headers_size as u32);

        // adding new sections increased the size of the image. We update this value so the
        // preprocessedhost is, in theory, runnable. In practice for roc programs it will crash
        // because there are missing symbols (those that the app should provide), but for testing
        // being able to run the preprocessedhost is nice.
        nt_headers.optional_header.size_of_image.set(
            LE,
            nt_headers.optional_header.size_of_image.get(LE)
                + (self.section_alignment * extra_sections.len()) as u32,
        );

        // update the section file offsets
        //
        // Sections:
        // Idx Name          Size      VMA               LMA               File off  Algn
        //   0 .text         00054d96  0000000140001000  0000000140001000  00000400  2**4
        //                   CONTENTS, ALLOC, LOAD, READONLY, CODE
        //   1 .rdata        00008630  0000000140056000  0000000140056000  00055200  2**4
        //                   CONTENTS, ALLOC, LOAD, READONLY, DATA
        //   2 .data         00000200  000000014005f000  000000014005f000  0005da00  2**4
        //                   CONTENTS, ALLOC, LOAD, DATA
        //   3 .pdata        0000228c  0000000140061000  0000000140061000  0005dc00  2**2
        //
        // The file offset of the sections has changed (we inserted some bytes) and this value must
        // now be updated to point to the correct place in the file
        let shift = self.new_headers_size - self.old_headers_size;
        let mut offset = self.section_table_offset as usize;

        let headers = load_structs_inplace_mut::<object::pe::ImageSectionHeader>(
            result,
            offset,
            self.new_section_count,
        );

        let mut it = headers.iter_mut().peekable();
        while let Some(header) = it.next() {
            let old = header.pointer_to_raw_data.get(LE);
            header.pointer_to_raw_data.set(LE, old + shift as u32);

            if header.name == *b".reloc\0\0" {
                // assumes `.reloc` is the final section
                debug_assert_eq!(it.peek().map(|s| s.name).as_ref(), extra_sections.first());

                let old = header.size_of_raw_data.get(LE);
                let new = old + self.additional_reloc_space as u32;
                header.size_of_raw_data.set(LE, new);
            }

            offset += std::mem::size_of::<object::pe::ImageSectionHeader>();
        }
    }
}
struct DummyDllThunks<'a> {
    section: &'a object::pe::ImageSectionHeader,
    offset_in_file: u64,
}

/// Find the place in the executable where the thunks for our dummy .dll are stored
fn find_thunks_start_offset<'a>(
    executable: &'a [u8],
    dynamic_relocations: &DynamicRelocationsPe,
) -> DummyDllThunks<'a> {
    // The host executable contains indirect calls to functions that the host should provide
    //
    //     14000105d:	e8 8e 27 00 00       	call   0x1400037f0
    //     140001062:	89 c3                	mov    ebx,eax
    //     140001064:	b1 21                	mov    cl,0x21
    //
    // This `call` is an indirect call (see https://www.felixcloutier.com/x86/call, our call starts
    // with 0xe8, so it is relative) In other words, at runtime the program will go to 0x1400037f0,
    // read a pointer-sized value there, and jump to that location.
    //
    // The 0x1400037f0 virtual address is located in the .rdata section
    //
    //     Sections:
    //     Idx Name          Size      VMA               LMA               File off  Algn
    //       0 .text         0000169a  0000000140001000  0000000140001000  00000600  2**4
    //                       CONTENTS, ALLOC, LOAD, READONLY, CODE
    //       1 .rdata        00000ba8  0000000140003000  0000000140003000  00001e00  2**4
    //                       CONTENTS, ALLOC, LOAD, READONLY, DATA
    //
    // Our task is then to put a valid function pointer at 0x1400037f0. The value should be a
    // virtual address pointing at the start of a function (which must be located in an executable
    // section)
    //
    // sources:
    //
    // - https://learn.microsoft.com/en-us/archive/msdn-magazine/2002/february/inside-windows-win32-portable-executable-file-format-in-detail
    const W: usize = std::mem::size_of::<ImageImportDescriptor>();

    let dummy_import_desc_start = dynamic_relocations.import_directory_offset_in_file as usize
        + W * dynamic_relocations.dummy_import_index as usize;

    let dummy_import_desc =
        load_struct_inplace::<ImageImportDescriptor>(executable, dummy_import_desc_start);

    // per https://en.wikibooks.org/wiki/X86_Disassembly/Windows_Executable_Files#The_Import_directory,
    //
    // > Once an imported value has been resolved, the pointer to it is stored in the FirstThunk array.
    // > It can then be used at runtime to address imported values.
    //
    // There is also an OriginalFirstThunk array, which we don't use.
    //
    // The `dummy_thunks_address` is the 0x1400037f0 of our example. In and of itself that is no good though,
    // this is a virtual address, we need a file offset
    let dummy_thunks_address_va = dummy_import_desc.first_thunk;
    let dummy_thunks_address = dummy_thunks_address_va.get(LE);

    let dos_header = object::pe::ImageDosHeader::parse(executable).unwrap();
    let mut offset = dos_header.nt_headers_offset().into();

    use object::read::pe::ImageNtHeaders;
    let (nt_headers, _data_directories) = ImageNtHeaders64::parse(executable, &mut offset).unwrap();
    let sections = nt_headers.sections(executable, offset).unwrap();

    // find the section the virtual address is in
    let (section_virtual_address, section) = sections
        .iter()
        .find_map(|section| {
            section
                .pe_data_containing(executable, dummy_thunks_address)
                .map(|(_section_data, section_va)| (section_va, section))
        })
        .expect("Invalid thunk virtual address");

    DummyDllThunks {
        section,
        // and get the offset in the file of 0x1400037f0
        offset_in_file: dummy_thunks_address as u64 - section_virtual_address as u64
            + section.pointer_to_raw_data.get(LE) as u64,
    }
}

/// Make the thunks point to our actual roc application functions
fn redirect_dummy_dll_functions(
    executable: &mut [u8],
    function_definition_vas: &[(String, u64)],
    imports: &[String],
    thunks_start_offset: usize,
) {
    // it could be that a symbol exposed by the app is not used by the host. We must skip unused symbols
    // this is an O(n^2) loop, hopefully that does not become a problem. If it does we can sort
    // both vectors to get linear complexity in the loop.
    'outer: for (i, host_name) in imports.iter().enumerate() {
        for (roc_app_target_name, roc_app_target_va) in function_definition_vas {
            if host_name == roc_app_target_name {
                // addresses are 64-bit values
                let address_bytes = &mut executable[thunks_start_offset + i * 8..][..8];

                // the array of addresses is null-terminated; make sure we haven't reached the end
                let current = u64::from_le_bytes(address_bytes.try_into().unwrap());
                if current == 0 {
                    internal_error!("invalid state: fewer thunks than function addresses");
                }

                // update the address to a function VA
                address_bytes.copy_from_slice(&roc_app_target_va.to_le_bytes());

                continue 'outer;
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
enum SectionKind {
    Text,
    // Data,
    ReadOnlyData,
}

#[derive(Debug)]
struct AppRelocation {
    offset_in_section: u64,
    address: u64,
    relocation: object::Relocation,
}

#[derive(Debug)]
struct Section<'a> {
    bytes: &'a [u8],
    kind: SectionKind,
    relocations: MutMap<String, Vec<AppRelocation>>,
    app_section_index: SectionIndex,
}

#[derive(Debug)]
struct AppSymbol {
    name: String,
    section_kind: SectionKind,
    offset_in_section: usize,
}

#[derive(Debug, Default)]
struct AppSections<'a> {
    sections: Vec<Section<'a>>,
    roc_symbols: Vec<AppSymbol>,
    other_symbols: Vec<(SectionIndex, AppSymbol)>,
}

/// Process relocations between two places within the app. This a bit different from doing a
/// relocation of a symbol that will be "imported" from the host
fn process_internal_relocations(
    sections: &[Section],
    other_symbols: &[(SectionIndex, AppSymbol)],
    first_host_section_virtual_address: u32,
    section_alignment: usize,
) -> VecMap<String, i64> {
    let mut result = VecMap::default();
    let mut host_section_virtual_address = first_host_section_virtual_address;

    for kind in [SectionKind::Text, SectionKind::ReadOnlyData] {
        let it = sections.iter().filter(|s| s.kind == kind);
        for section in it {
            for (s_index, app_symbol) in other_symbols.iter() {
                if *s_index == section.app_section_index {
                    result.insert(
                        app_symbol.name.clone(),
                        app_symbol.offset_in_section as i64 + host_section_virtual_address as i64,
                    );
                }
            }
        }

        let length: usize = sections
            .iter()
            .filter(|s| s.kind == kind)
            .map(|s| s.bytes.len())
            .sum();

        host_section_virtual_address += next_multiple_of(length, section_alignment) as u32;
    }

    result
}

impl<'a> AppSections<'a> {
    fn from_data(data: &'a [u8]) -> Self {
        let file = object::File::parse(data).unwrap();

        let mut sections = Vec::new();

        let mut section_starts = MutMap::default();

        let mut text_bytes = 0;
        let mut rdata_bytes = 0;

        for (i, section) in file.sections().enumerate() {
            let kind = match section.name() {
                Ok(name) => {
                    match name {
                        _ if name.starts_with(".text") => SectionKind::Text,
                        // _ if name.starts_with(".data") => SectionKind::Data,
                        _ if name.starts_with(".rdata") => SectionKind::ReadOnlyData,
                        _ => continue,
                    }
                }
                Err(_) => continue,
            };

            let mut relocations: MutMap<String, Vec<AppRelocation>> = MutMap::default();

            for (offset_in_section, relocation) in section.relocations() {
                match relocation.target() {
                    RelocationTarget::Symbol(symbol_index) => {
                        let symbol = file.symbol_by_index(symbol_index);

                        let address = symbol.as_ref().map(|s| s.address()).unwrap_or_default();
                        let name = symbol.and_then(|s| s.name()).unwrap_or_default();
                        let name = redirect_libc_functions(name).unwrap_or(name).to_string();

                        relocations.entry(name).or_default().push(AppRelocation {
                            offset_in_section,
                            address,
                            relocation,
                        });
                    }
                    _ => todo!(),
                }
            }

            let (start, length) = section.file_range().unwrap();
            let file_range = &data[start as usize..][..length as usize];

            // sections are one-indexed...
            let index = SectionIndex(i + 1);

            match kind {
                SectionKind::Text => {
                    section_starts.insert(index, (kind, text_bytes));
                    text_bytes += length;
                }
                SectionKind::ReadOnlyData => {
                    section_starts.insert(index, (kind, rdata_bytes));
                    rdata_bytes += length;
                }
            }

            let section = Section {
                app_section_index: index,
                bytes: file_range,
                kind,
                relocations,
            };

            sections.push(section);
        }

        // add a fake section that contains code for a stack probe that some app functions need
        let stack_check_section = Section {
            bytes: &___CHKSTK_MS,
            kind: SectionKind::Text,
            relocations: Default::default(),
            app_section_index: object::SectionIndex(0),
        };

        sections.push(stack_check_section);

        let mut roc_symbols = Vec::new();
        let mut other_symbols = Vec::new();

        for symbol in file.symbols() {
            if symbol.name_bytes().unwrap_or_default().starts_with(b"roc") {
                if let object::SymbolSection::Section(index) = symbol.section() {
                    let (kind, offset_in_host_section) = section_starts[&index];

                    let symbol = AppSymbol {
                        name: symbol.name().unwrap_or_default().to_string(),
                        section_kind: kind,
                        offset_in_section: (offset_in_host_section + symbol.address()) as usize,
                    };

                    roc_symbols.push(symbol);
                }
            } else if let object::SymbolSection::Section(index) = symbol.section() {
                if let Some((kind, offset_in_host_section)) = section_starts.get(&index) {
                    let symbol = AppSymbol {
                        name: symbol.name().unwrap_or_default().to_string(),
                        section_kind: *kind,
                        offset_in_section: (offset_in_host_section + symbol.address()) as usize,
                    };

                    other_symbols.push((index, symbol));
                }
            }
        }

        AppSections {
            sections,
            roc_symbols,
            other_symbols,
        }
    }
}

// check on https://github.com/rust-lang/rust/issues/88581 some time in the future
pub const fn next_multiple_of(lhs: usize, rhs: usize) -> usize {
    match lhs % rhs {
        0 => lhs,
        r => lhs + (rhs - r),
    }
}

pub const fn div_ceil(lhs: usize, rhs: usize) -> usize {
    let d = lhs / rhs;
    let r = lhs % rhs;
    if r > 0 && rhs > 0 {
        d + 1
    } else {
        d
    }
}

#[derive(Debug)]
#[repr(C)]
struct HeaderMetadata {
    image_base: u64,
    file_alignment: usize,
    section_alignment: usize,
}

/// NOTE: the caller must make sure the `*_added` values are rounded
/// up to the section and file alignment respectively
fn update_optional_header(
    data: &mut [u8],
    optional_header_offset: usize,
    code_bytes_added: u32,
    file_bytes_added: u32,
    data_bytes_added: u32,
) {
    use object::pe::ImageOptionalHeader64;

    let optional_header =
        load_struct_inplace_mut::<ImageOptionalHeader64>(data, optional_header_offset);

    optional_header
        .size_of_code
        .set(LE, optional_header.size_of_code.get(LE) + code_bytes_added);

    optional_header
        .size_of_image
        .set(LE, optional_header.size_of_image.get(LE) + file_bytes_added);

    optional_header.size_of_initialized_data.set(
        LE,
        optional_header.size_of_initialized_data.get(LE) + data_bytes_added,
    );
}

#[allow(clippy::too_many_arguments)]
fn write_section_header(
    data: &mut [u8],
    name: [u8; 8],
    characteristics: u32,
    section_header_start: usize,
    section_file_offset: usize,
    virtual_size: u32,
    virtual_address: u32,
    size_of_raw_data: u32,
) {
    use object::U32;

    let header = ImageSectionHeader {
        name,
        virtual_size: U32::new(LE, virtual_size),
        virtual_address: U32::new(LE, virtual_address),
        size_of_raw_data: U32::new(LE, size_of_raw_data),
        pointer_to_raw_data: U32::new(LE, section_file_offset as u32),
        pointer_to_relocations: Default::default(),
        pointer_to_linenumbers: Default::default(),
        number_of_relocations: Default::default(),
        number_of_linenumbers: Default::default(),
        characteristics: U32::new(LE, characteristics),
    };

    let header_array: [u8; std::mem::size_of::<ImageSectionHeader>()] =
        unsafe { std::mem::transmute(header) };

    data[section_header_start..][..header_array.len()].copy_from_slice(&header_array);
}

fn relocate_dummy_dll_entries(executable: &mut [u8], md: &PeMetadata) {
    // in the data directories, update the length of the base relocations
    let dir = load_struct_inplace_mut::<pe::ImageDataDirectory>(
        executable,
        md.dynamic_relocations.data_directories_offset_in_file as usize
            + object::pe::IMAGE_DIRECTORY_ENTRY_BASERELOC
                * std::mem::size_of::<pe::ImageDataDirectory>(),
    );

    // for unclear reasons, we must bump the image directory size here.
    // we also need some zeroed-out memory at the end, so if the directory
    // ends at a multiple of `file_alignment`, pick the next one.
    let new_reloc_directory_size =
        next_multiple_of(dir.size.get(LE) as usize + 4, md.file_alignment as usize);
    dir.size.set(LE, new_reloc_directory_size as u32);
}

/// Redirect `memcpy` and similar libc functions to their roc equivalents
pub(crate) fn redirect_libc_functions(name: &str) -> Option<&str> {
    match name {
        "memset" => Some("roc_memset"),
        "memmove" => Some("roc_memmove"),
        _ => None,
    }
}

// 0000000000000000 <.text>:
//    0:	51                   	push   rcx
//    1:	50                   	push   rax
//    2:	48 3d 00 10 00 00    	cmp    rax,0x1000
//    8:	48 8d 4c 24 18       	lea    rcx,[rsp+0x18]
//    d:	72 18                	jb     27 <.text+0x27>
//    f:	48 81 e9 00 10 00 00 	sub    rcx,0x1000
//   16:	48 85 09             	test   QWORD PTR [rcx],rcx
//   19:	48 2d 00 10 00 00    	sub    rax,0x1000
//   1f:	48 3d 00 10 00 00    	cmp    rax,0x1000
//   25:	77 e8                	ja     f <.text+0xf>
//   27:	48 29 c1             	sub    rcx,rax
//   2a:	48 85 09             	test   QWORD PTR [rcx],rcx
//   2d:	58                   	pop    rax
//   2e:	59                   	pop    rcx
//   2f:	c3                   	ret
const ___CHKSTK_MS: [u8; 48] = [
    0x51, //  push   rcx
    0x50, //  push   rax
    0x48, 0x3d, 0x00, 0x10, 0x00, 0x00, //  cmp    rax,0x0x1000
    0x48, 0x8d, 0x4c, 0x24, 0x18, //  lea    rcx,0x[rsp+0x18]
    0x72, 0x18, //  jb     0x27
    0x48, 0x81, 0xe9, 0x00, 0x10, 0x00, 0x00, //  sub    rcx,0x0x1000
    0x48, 0x85, 0x09, //  test   QWORD PTR [rcx],0xrcx
    0x48, 0x2d, 0x00, 0x10, 0x00, 0x00, //  sub    rax,0x0x1000
    0x48, 0x3d, 0x00, 0x10, 0x00, 0x00, //  cmp    rax,0x0x1000
    0x77, 0xe8, //  ja     0xf
    0x48, 0x29, 0xc1, //  sub    rcx,0xrax
    0x48, 0x85, 0x09, //  test   QWORD PTR [rcx],rcx
    0x58, //  pop    rax
    0x59, //  pop    rcx
    0xc3, // ret
];

#[cfg(test)]
mod test {
    const PE_DYNHOST: &[u8] = include_bytes!("../dynhost_benchmarks_windows.exe") as &[_];

    use indoc::indoc;
    use object::read::pe::PeFile64;
    use object::{pe, LittleEndian as LE, Object};
    use serial_test::serial;
    use target_lexicon::Triple;

    use super::*;

    #[test]
    fn dynamic_relocations() {
        use object::LittleEndian as LE;

        // find the location in the virtual address space and the on-disk file where dynamic
        // relocations are stored. For the example dynhost, objdump shows

        // Sections:
        // Idx Name          Size      VMA               LMA               File off  Algn
        //   0 .text         0007bb26  0000000140001000  0000000140001000  00000400  2**4
        //                   CONTENTS, ALLOC, LOAD, READONLY, CODE
        //   1 .rdata        000369d4  000000014007d000  000000014007d000  0007c000  2**4
        //

        // we parse that into this structure. Note how the VMA and File off correspond to the
        // values that we find. Also note how the delta between virtual address and offset is the
        // same for values of `address_and_offset` and the section values that we find
        //
        // DynamicRelocationsPe {
        //     name_by_virtual_address: {
        //         0xaf4e0: "roc__mainForHost_size",
        //         0xaf4c8: "roc__mainForHost_1__Fx_caller",
        //         0xaf4d0: "roc__mainForHost_1__Fx_result_size",
        //         0xaf4d8: "roc__mainForHost_1_exposed_generic",
        //     },
        //     address_and_offset: {
        //         "roc__mainForHost_1__Fx_result_size": (
        //             0xaf4d0,
        //             0xae4d0,
        //         ),
        //         "roc__mainForHost_1__Fx_caller": (
        //             0xaf4c8,
        //             0xae4c8,
        //         ),
        //         "roc__mainForHost_1_exposed_generic": (
        //             0xaf4d8,
        //             0xae4d8,
        //         ),
        //         "roc__mainForHost_size": (
        //             0xaf4e0,
        //             0xae4e0,
        //         ),
        //     },
        //     section_virtual_address: 0x7d000,
        //     section_offset_in_file: 0x7c000,
        // }

        let dynamic_relocations = DynamicRelocationsPe::new(PE_DYNHOST);
        // println!("{:#x?}", dynamic_relocations);

        let file = PeFile64::parse(PE_DYNHOST).unwrap();
        let import_table = file.import_table().unwrap().unwrap();

        //        let mut name_by_virtual_address: Vec<_> = dynamic_relocations
        //            .name_by_virtual_address
        //            .into_iter()
        //            .collect();
        //
        //
        //        name_by_virtual_address.sort_unstable();

        let mut address_and_offset: Vec<_> =
            dynamic_relocations.address_and_offset.into_iter().collect();

        address_and_offset.sort_unstable();

        // get the relocations through the API
        let addresses_api = {
            let (descriptor, _) = DynamicRelocationsPe::find_roc_dummy_dll(&import_table)
                .unwrap()
                .unwrap();

            let mut addresses = Vec::new();

            let mut thunks = import_table
                .thunks(descriptor.original_first_thunk.get(LE))
                .unwrap();
            while let Some(thunk_data) = thunks.next::<ImageNtHeaders64>().unwrap() {
                use object::read::pe::ImageThunkData;

                let temporary_address = thunk_data.address();

                let (_, name) = import_table.hint_name(temporary_address as _).unwrap();
                let name = String::from_utf8_lossy(name).to_string();

                addresses.push((name, temporary_address));
            }

            addresses.sort_unstable();

            addresses
        };

        // get the relocations through using our offsets into the file
        let addresses_file: Vec<_> = address_and_offset
            .iter()
            .map(|(name, (_, offset))| {
                (
                    name.to_string(),
                    u32::from_le_bytes(PE_DYNHOST[*offset as usize..][..4].try_into().unwrap()),
                )
            })
            .collect();

        // we want our file offset approach to equal the API
        assert_eq!(addresses_api, addresses_file);
    }

    #[test]
    fn collect_undefined_symbols_pe() {
        let object = object::File::parse(PE_DYNHOST).unwrap();

        let imports: Vec<_> = object
            .imports()
            .unwrap()
            .iter()
            .filter(|import| import.library() == APP_DLL.as_bytes())
            .map(|import| String::from_utf8_lossy(import.name()))
            .collect();

        assert_eq!(
            [
                "roc__mainForHost_1__Fx_caller",
                "roc__mainForHost_1__Fx_result_size",
                "roc__mainForHost_1_exposed_generic",
                "roc__mainForHost_size"
            ],
            imports.as_slice(),
        )
    }

    fn remove_dummy_dll_import_table_test(
        data: &mut [u8],
        data_directories_offset_in_file: u32,
        imports_offset_in_file: u32,
        dummy_import_index: u32,
    ) {
        const W: usize = std::mem::size_of::<ImageImportDescriptor>();

        // clear out the import table entry. we do implicitly assume that our dummy .dll is the last
        let start = imports_offset_in_file as usize + W * dummy_import_index as usize;
        for b in data[start..][..W].iter_mut() {
            *b = 0;
        }

        // in the data directories, update the length of the imports (there is one fewer now)
        let dir = load_struct_inplace_mut::<pe::ImageDataDirectory>(
            data,
            data_directories_offset_in_file as usize
                + object::pe::IMAGE_DIRECTORY_ENTRY_IMPORT
                    * std::mem::size_of::<pe::ImageDataDirectory>(),
        );

        let current = dir.size.get(LE);
        dir.size.set(
            LE,
            current - std::mem::size_of::<pe::ImageImportDescriptor>() as u32,
        );
    }

    #[test]
    fn remove_dummy_dll_import() {
        let object = PeFile64::parse(PE_DYNHOST).unwrap();
        let before = object
            .data_directories()
            .get(pe::IMAGE_DIRECTORY_ENTRY_IMPORT)
            .unwrap()
            .size
            .get(LE);

        let dynamic_relocations = DynamicRelocationsPe::new(PE_DYNHOST);
        let mut data = PE_DYNHOST.to_vec();

        remove_dummy_dll_import_table_test(
            &mut data,
            dynamic_relocations.data_directories_offset_in_file,
            dynamic_relocations.import_directory_offset_in_file,
            dynamic_relocations.dummy_import_index,
        );

        // parse and check that it's really gone
        let object = PeFile64::parse(data.as_slice()).unwrap();

        let after = object
            .data_directories()
            .get(pe::IMAGE_DIRECTORY_ENTRY_IMPORT)
            .unwrap()
            .size
            .get(LE);

        assert_eq!(
            before - after,
            std::mem::size_of::<pe::ImageImportDescriptor>() as u32
        );

        assert_eq!(
            object
                .imports()
                .unwrap()
                .iter()
                .filter(|import| import.library() == APP_DLL.as_bytes())
                .count(),
            0
        );
    }

    #[test]
    fn find_number_of_sections() {
        use object::pe;

        let dos_header = pe::ImageDosHeader::parse(PE_DYNHOST).unwrap();
        let offset = dos_header.nt_headers_offset();

        let number_of_sections_offset = offset + 4 + 2;

        let actual = u16::from_le_bytes(
            PE_DYNHOST[number_of_sections_offset as usize..][..2]
                .try_into()
                .unwrap(),
        );

        assert_eq!(actual, 7)
    }

    fn increase_number_of_sections_help(
        input_data: &[u8],
        new_sections: &[[u8; 8]],
        output_file: &Path,
    ) -> MmapMut {
        use object::read::pe::ImageNtHeaders;

        let dos_header = object::pe::ImageDosHeader::parse(input_data).unwrap();
        let mut offset = dos_header.nt_headers_offset().into();

        let image_headers_old = load_struct_inplace::<ImageNtHeaders64>(
            input_data,
            dos_header.nt_headers_offset() as usize,
        );

        let sections_len_old = image_headers_old.file_header().number_of_sections.get(LE);

        let mmap = Preprocessor::preprocess(output_file, input_data, 0, new_sections);

        let image_headers_new =
            load_struct_inplace::<ImageNtHeaders64>(&mmap, dos_header.nt_headers_offset() as usize);

        let sections_len_new = image_headers_new.file_header().number_of_sections.get(LE);

        assert_eq!(
            sections_len_old + new_sections.len() as u16,
            sections_len_new
        );

        let (nt_headers, _data_directories) = ImageNtHeaders64::parse(&*mmap, &mut offset).unwrap();
        let sections = nt_headers.sections(&*mmap, offset).unwrap();

        let names: Vec<_> = sections
            .iter()
            .map(|h| h.name)
            .skip(sections_len_old as usize)
            .take(new_sections.len())
            .collect();

        assert_eq!(new_sections, names.as_slice());

        mmap
    }

    #[test]
    fn increase_number_of_sections() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.exe");

        let new_sections = [*b"placehol", *b"aaaabbbb"];

        increase_number_of_sections_help(PE_DYNHOST, &new_sections, &path);
    }

    fn zig_host_app(dir: &Path, host_zig: &str, app_zig: &str) {
        let zig = std::env::var("ROC_ZIG").unwrap_or_else(|_| "zig".into());

        std::fs::write(dir.join("host.zig"), host_zig.as_bytes()).unwrap();
        std::fs::write(dir.join("app.zig"), app_zig.as_bytes()).unwrap();

        // we need to compile the app first
        let output = std::process::Command::new(&zig)
            .current_dir(dir)
            .args([
                "build-obj",
                "app.zig",
                "-target",
                "x86_64-windows-gnu",
                "-fstrip",
                "-rdynamic",
                "-OReleaseFast",
            ])
            .output()
            .unwrap();

        if !output.status.success() {
            use std::io::Write;

            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();

            panic!("zig build-obj failed");
        }

        // open our app object; we'll copy sections from it later
        let file = std::fs::File::open(dir.join("app.obj")).unwrap();
        let roc_app = unsafe { memmap2::Mmap::map(&file) }.unwrap();

        let roc_app_sections = AppSections::from_data(&roc_app);
        let symbols = roc_app_sections.roc_symbols;

        // make the dummy dylib based on the app object
        let names: Vec<_> = symbols.iter().map(|s| s.name.clone()).collect();
        let dylib_bytes = crate::generate_dylib::synthetic_dll(&names);
        std::fs::write(dir.join("libapp.dll"), dylib_bytes).unwrap();

        // now we can compile the host (it uses libapp.dll, hence the order here)
        let mut command = std::process::Command::new(&zig);
        command.current_dir(dir).args([
            "build-exe",
            "libapp.dll",
            "host.zig",
            "-lc",
            "-target",
            "x86_64-windows-gnu",
            "-rdynamic",
            "-fstrip",
            "-rdynamic",
            "-OReleaseFast",
        ]);

        let command_str = format!("{:?}", &command);
        let output = command.output().unwrap();

        if !output.status.success() {
            use std::io::Write;

            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();

            panic!("zig build-exe failed: {command_str}");
        }

        let target: roc_target::Target = Triple::host().into();
        let preprocessed_host_filename = dir.join(target.prebuilt_surgical_host());

        preprocess_windows(
            &dir.join("host.exe"),
            &dir.join("metadata"),
            &preprocessed_host_filename,
            &dir.join("libapp.dll"),
            false,
            false,
        )
        .unwrap();

        std::fs::copy(&preprocessed_host_filename, dir.join("app.exe")).unwrap();

        surgery_pe(&dir.join("app.exe"), &dir.join("metadata"), &roc_app);
    }

    #[allow(dead_code)]
    fn windows_test<F>(runner: F) -> String
    where
        F: Fn(&Path),
    {
        let dir = tempfile::tempdir().unwrap();
        let dir = dir.path();

        runner(dir);

        let output = std::process::Command::new(dir.join("app.exe"))
            .current_dir(dir)
            .output()
            .unwrap();

        if !output.status.success() {
            use std::io::Write;

            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();

            panic!("app.exe failed");
        }

        String::from_utf8(output.stdout.to_vec()).unwrap()
    }

    #[allow(dead_code)]
    fn wine_test<F>(runner: F) -> String
    where
        F: Fn(&Path),
    {
        let dir = tempfile::tempdir().unwrap();
        let dir = dir.path();

        runner(dir);

        let output = std::process::Command::new("wine")
            .current_dir(dir)
            .args(["app.exe"])
            .output()
            .unwrap();

        if !output.status.success() {
            use std::io::Write;

            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();

            panic!("wine failed");
        }

        String::from_utf8(output.stdout.to_vec()).unwrap()
    }

    /// Basics of linking: symbols imported and exported by the host and app, both values and
    /// functions
    #[allow(dead_code)]
    fn test_basics(dir: &Path) {
        zig_host_app(
            dir,
            indoc!(
                r#"
                const std = @import("std");

                extern const roc_one: u64;
                extern const roc_three: u64;

                extern fn roc_magic1() callconv(.c) u64;
                extern fn roc_magic2() callconv(.c) u8;

                pub export fn roc_alloc() u64 {
                    return 123456;
                }

                pub export fn roc_realloc() u64 {
                    return 111111;
                }

                pub fn main() !void {
                    const stdout = std.io.getStdOut().writer();
                    try stdout.print("Hello, {} {} {} {}!\n", .{roc_magic1(), roc_magic2(), roc_one, roc_three});
                }
                "#
            ),
            indoc!(
                r#"
                export const roc_one: u64 = 1;
                export const roc_three: u64 = 3;

                extern fn roc_alloc() u64;
                extern fn roc_realloc() u64;

                export fn roc_magic1() u64 {
                    return roc_alloc() + roc_realloc();
                }

                export fn roc_magic2() u8 {
                    return 32;
                }
                "#
            ),
        );
    }

    #[cfg(windows)]
    #[serial(zig_build)]
    #[test]
    fn basics_windows() {
        assert_eq!("Hello, 234567 32 1 3!\n", windows_test(test_basics))
    }

    #[test]
    #[serial(zig_build)]
    #[ignore]
    fn basics_wine() {
        assert_eq!("Hello, 234567 32 1 3!\n", wine_test(test_basics))
    }

    /// This zig code sample has a relocation in the text section that points into the rodata
    /// section. That means we need to correctly track where each app section ends up in the host.
    #[allow(dead_code)]
    fn test_internal_relocations(dir: &Path) {
        zig_host_app(
            dir,
            indoc!(
                r#"
                const std = @import("std");

                extern fn roc_magic1(usize) callconv(.c) [*]const u8;

                pub fn main() !void {
                    const stdout = std.io.getStdOut().writer();
                    try stdout.print("Hello {s}\n", .{roc_magic1(0)[0..3]});
                }
                "#
            ),
            indoc!(
                r#"
                const X = [_][]const u8 { "foo" };

                export fn roc_magic1(index: usize) [*]const u8 {
                    return X[index].ptr;
                }
                "#
            ),
        );
    }

    #[cfg(windows)]
    #[serial(zig_build)]
    #[test]
    fn app_internal_relocations_windows() {
        assert_eq!("Hello foo\n", windows_test(test_internal_relocations))
    }

    #[test]
    #[serial(zig_build)]
    #[ignore]
    fn app_internal_relocations_wine() {
        assert_eq!("Hello foo\n", wine_test(test_internal_relocations))
    }

    /// Run our preprocessing on an all-zig host. There is no app here to simplify things.
    fn preprocessing_help(dir: &Path) {
        let zig = std::env::var("ROC_ZIG").unwrap_or_else(|_| "zig".into());

        let host_zig = indoc!(
            r#"
            const std = @import("std");
            pub fn main() !void {
                const stdout = std.io.getStdOut().writer();
                try stdout.print("Hello there\n", .{});
            }
            "#
        );

        std::fs::write(dir.join("host.zig"), host_zig.as_bytes()).unwrap();

        let mut command = std::process::Command::new(zig);
        command.current_dir(dir).args([
            "build-exe",
            "host.zig",
            "-lc",
            "-target",
            "x86_64-windows-gnu",
            "-rdynamic",
            "-fstrip",
            "-OReleaseFast",
        ]);

        let command_str = format!("{:?}", &command);
        let output = command.output().unwrap();

        if !output.status.success() {
            use std::io::Write;

            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();

            panic!("zig build-exe failed: {command_str}");
        }

        let host_bytes = std::fs::read(dir.join("host.exe")).unwrap();
        let host_bytes = host_bytes.as_slice();

        let extra_sections = [*b"\0\0\0\0\0\0\0\0", *b"\0\0\0\0\0\0\0\0"];

        Preprocessor::preprocess(
            &dir.join("app.exe"),
            host_bytes,
            0,
            extra_sections.as_slice(),
        );
    }

    #[cfg(windows)]
    #[test]
    fn preprocessing_windows() {
        assert_eq!("Hello there\n", windows_test(preprocessing_help))
    }

    #[test]
    #[ignore]
    fn preprocessing_wine() {
        assert_eq!("Hello there\n", wine_test(preprocessing_help))
    }
}
