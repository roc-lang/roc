use std::{
    io::{BufReader, BufWriter},
    ops::Range,
    path::Path,
};

use bincode::{deserialize_from, serialize_into};
use memmap2::MmapMut;
use object::{
    pe::{
        self, ImageFileHeader, ImageImportDescriptor, ImageNtHeaders64, ImageSectionHeader,
        ImageThunkData64,
    },
    read::pe::ImportTable,
    LittleEndian as LE, Object, RelocationTarget, SectionIndex,
};
use serde::{Deserialize, Serialize};

use roc_collections::MutMap;
use roc_error_macros::internal_error;

use crate::{
    generate_dylib::APP_DLL, load_struct_inplace, load_struct_inplace_mut, open_mmap, open_mmap_mut,
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
    thunks_start_offset: usize,

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
        use object::ObjectSection;

        let dynhost_obj = object::read::pe::PeFile64::parse(preprocessed_data)
            .unwrap_or_else(|err| internal_error!("Failed to parse executable file: {}", err));

        let host_section_count = dynhost_obj.sections().count() - new_sections.len();
        let last_host_section = dynhost_obj
            .sections()
            .nth(host_section_count - 1) // -1 because we have a count and want an index
            .unwrap();

        let dynamic_relocations = DynamicRelocationsPe::new(preprocessed_data);
        let thunks_start_offset = find_thunks_start_offset(preprocessed_data, &dynamic_relocations);

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
            thunks_start_offset,
        }
    }
}

pub(crate) fn preprocess_windows(
    host_exe_filename: &Path,
    metadata_filename: &Path,
    preprocessed_filename: &Path,
    _verbose: bool,
    _time: bool,
) -> object::read::Result<()> {
    let data = open_mmap(host_exe_filename);
    let new_sections = [*b".text\0\0\0", *b".rdata\0\0"];
    let mut preprocessed = Preprocessor::preprocess(preprocessed_filename, &data, &new_sections);

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

    // clear out the import table entry. we do implicitly assume that our dummy .dll is the last
    {
        const W: usize = std::mem::size_of::<ImageImportDescriptor>();

        let start = md.dynamic_relocations.imports_offset_in_file as usize
            + W * md.dynamic_relocations.dummy_import_index as usize;

        for b in preprocessed[start..][..W].iter_mut() {
            *b = 0;
        }
    }

    md.write_to_file(metadata_filename);

    Ok(())
}

pub(crate) fn surgery_pe(executable_path: &Path, metadata_path: &Path, app_path: &Path) {
    let md = PeMetadata::read_from_file(metadata_path);
    let app_bytes = open_mmap(app_path);

    let app_obj_sections = AppSections::from_data(&app_bytes);
    let mut symbols = app_obj_sections.symbols;

    let image_base: u64 = md.image_base;
    let file_alignment = md.file_alignment as usize;
    let section_alignment = md.section_alignment as usize;

    let app_sections_size: usize = app_obj_sections
        .sections
        .iter()
        .map(|s| next_multiple_of(s.file_range.end - s.file_range.start, file_alignment))
        .sum();

    let executable = &mut open_mmap_mut(executable_path, md.dynhost_file_size + app_sections_size);

    let app_code_section_va = md.last_host_section_address
        + next_multiple_of(
            md.last_host_section_size as usize,
            section_alignment as usize,
        ) as u64;

    let mut section_file_offset = md.dynhost_file_size;
    let mut virtual_address = (app_code_section_va - image_base) as u32;

    // find the location to write the section headers for our new sections
    let mut section_header_start = md.dynamic_relocations.section_headers_offset_in_file as usize
        + md.host_section_count * std::mem::size_of::<ImageSectionHeader>();

    let mut code_bytes_added = 0;
    let mut data_bytes_added = 0;
    let mut file_bytes_added = 0;

    for kind in [SectionKind::Text, SectionKind::ReadOnlyData] {
        let length: usize = app_obj_sections
            .sections
            .iter()
            .filter(|s| s.kind == kind)
            .map(|s| s.file_range.end - s.file_range.start)
            .sum();

        // offset_in_section now becomes a proper virtual address
        for symbol in symbols.iter_mut() {
            if symbol.section_kind == kind {
                symbol.offset_in_section += image_base as usize + virtual_address as usize;
            }
        }

        let virtual_size = length as u32;
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
                    virtual_address,
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
                    virtual_address,
                    size_of_raw_data,
                );
            }
        }

        let mut offset = section_file_offset;
        let it = app_obj_sections.sections.iter().filter(|s| s.kind == kind);
        for section in it {
            let slice = &app_bytes[section.file_range.start..section.file_range.end];
            executable[offset..][..slice.len()].copy_from_slice(slice);

            for (name, app_relocation) in section.relocations.iter() {
                let AppRelocation {
                    offset_in_section,
                    relocation,
                    address,
                } = app_relocation;

                match md.exports.get(name) {
                    Some(destination) => {
                        match relocation.kind() {
                            object::RelocationKind::Relative => {
                                // we implicitly only do 32-bit relocations
                                debug_assert_eq!(relocation.size(), 32);

                                let delta = destination
                                    - virtual_address as i64
                                    - *offset_in_section as i64
                                    + relocation.addend();

                                executable[offset + *offset_in_section as usize..][..4]
                                    .copy_from_slice(&(delta as i32).to_le_bytes());
                            }
                            _ => todo!(),
                        }
                    }
                    None => {
                        match relocation.kind() {
                            object::RelocationKind::Relative => {
                                // we implicitly only do 32-bit relocations
                                debug_assert_eq!(relocation.size(), 32);

                                let delta = *address as i64 - *offset_in_section as i64
                                    + relocation.addend();

                                executable[offset + *offset_in_section as usize..][..4]
                                    .copy_from_slice(&(delta as i32).to_le_bytes());
                            }
                            _ => todo!(),
                        }
                    }
                }
            }

            offset += slice.len();
        }

        section_header_start += std::mem::size_of::<ImageSectionHeader>();
        section_file_offset += size_of_raw_data as usize;
        virtual_address += next_multiple_of(length, section_alignment) as u32;
        file_bytes_added += next_multiple_of(length, section_alignment) as u32;
    }

    update_optional_header(
        executable,
        md.optional_header_offset,
        code_bytes_added as u32,
        file_bytes_added as u32,
        data_bytes_added as u32,
    );

    let symbols: Vec<_> = symbols
        .into_iter()
        .map(|s| (s.name, s.offset_in_section as u64))
        .collect();

    redirect_dummy_dll_functions(executable, &symbols, &md.imports, md.thunks_start_offset);
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
    imports_offset_in_file: u32,

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
            // dbg!(String::from_utf8_lossy(name));

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

        let imports_offset_in_section = import_va.wrapping_sub(section_va);
        let imports_offset_in_file = offset_in_file + imports_offset_in_section;

        let (descriptor, dummy_import_index) = Self::find_roc_dummy_dll(&import_table)?.unwrap();

        let mut this = Self {
            name_by_virtual_address: Default::default(),
            address_and_offset: Default::default(),
            section_virtual_address: section_va,
            section_offset_in_file: offset_in_file,
            imports_offset_in_file,
            data_directories_offset_in_file,
            dummy_import_index,
            section_headers_offset_in_file,
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
    additional_length: usize,
    extra_sections_start: usize,
    extra_sections_width: usize,
    section_count_offset: u64,
    section_table_offset: u64,
    new_section_count: usize,
    old_headers_size: usize,
    new_headers_size: usize,
}

impl Preprocessor {
    const SECTION_HEADER_WIDTH: usize = std::mem::size_of::<ImageSectionHeader>();

    fn preprocess(output_path: &Path, data: &[u8], extra_sections: &[[u8; 8]]) -> MmapMut {
        let this = Self::new(data, extra_sections);
        let mut result = open_mmap_mut(output_path, data.len() + this.additional_length);

        this.copy(&mut result, data);
        this.fix(&mut result, extra_sections);

        result
    }

    fn new(data: &[u8], extra_sections: &[[u8; 8]]) -> Self {
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
        let extra_sections_width = extra_sections.len() * Self::SECTION_HEADER_WIDTH;

        // in a better world `extra_sections_width.div_ceil(file_alignment)` would be stable
        // check on https://github.com/rust-lang/rust/issues/88581 some time in the future
        let extra_alignments = div_ceil(extra_sections_width, file_alignment);
        let new_headers_size = old_headers_size + extra_alignments * file_alignment;

        let additional_length = new_headers_size - old_headers_size;

        Self {
            extra_sections_start: section_table_offset as usize
                + sections.len() as usize * Self::SECTION_HEADER_WIDTH,
            extra_sections_width,
            additional_length,
            header_offset,
            // the section count is stored 6 bytes into the header
            section_count_offset: header_offset + 4 + 2,
            section_table_offset,
            new_section_count: sections.len() + extra_sections.len(),
            old_headers_size,
            new_headers_size,
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
        result[self.new_headers_size..].copy_from_slice(&data[self.old_headers_size..]);
    }

    fn write_dummy_sections(&self, result: &mut MmapMut, extra_sections: &[[u8; 8]]) {
        for (i, name) in extra_sections.iter().enumerate() {
            let header = ImageSectionHeader {
                name: *name,
                virtual_size: Default::default(),
                virtual_address: Default::default(),
                size_of_raw_data: Default::default(),
                pointer_to_raw_data: Default::default(),
                pointer_to_relocations: Default::default(),
                pointer_to_linenumbers: Default::default(),
                number_of_relocations: Default::default(),
                number_of_linenumbers: Default::default(),
                characteristics: Default::default(),
            };

            let header_array: [u8; std::mem::size_of::<ImageSectionHeader>()] =
                unsafe { std::mem::transmute(header) };

            result[self.extra_sections_start + i * header_array.len()..][..header_array.len()]
                .copy_from_slice(&header_array);
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
        loop {
            let header = load_struct_inplace_mut::<object::pe::ImageSectionHeader>(result, offset);

            let old = header.pointer_to_raw_data.get(LE);
            header.pointer_to_raw_data.set(LE, old + shift as u32);

            // stop when we hit the NULL section
            if header.name == [0; 8] {
                break;
            }

            offset += std::mem::size_of::<object::pe::ImageSectionHeader>();
        }
    }
}

/// Find the place in the executable where the thunks for our dummy .dll are stored
fn find_thunks_start_offset(
    executable: &[u8],
    dynamic_relocations: &DynamicRelocationsPe,
) -> usize {
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

    let dummy_import_desc_start = dynamic_relocations.imports_offset_in_file as usize
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
    let (section_va, offset_in_file) = sections
        .iter()
        .find_map(|section| {
            section
                .pe_data_containing(executable, dummy_thunks_address)
                .map(|(_section_data, section_va)| {
                    (section_va, section.pointer_to_raw_data.get(LE))
                })
        })
        .expect("Invalid thunk virtual address");

    // and get the offset in the file of 0x1400037f0
    (dummy_thunks_address - section_va + offset_in_file) as usize
}

/// Make the thunks point to our actual roc application functions
fn redirect_dummy_dll_functions(
    executable: &mut [u8],
    function_definition_vas: &[(String, u64)],
    imports: &[String],
    thunks_start_offset: usize,
) {
    // it could be that a symbol exposed by the app is not used by the host. We must skip unused symbols
    let mut targets = function_definition_vas.iter();
    'outer: for (i, host_name) in imports.iter().enumerate() {
        for (roc_app_target_name, roc_app_target_va) in targets.by_ref() {
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
struct Section {
    /// File range of the section (in the app object)
    file_range: Range<usize>,
    kind: SectionKind,
    relocations: MutMap<String, AppRelocation>,
}

#[derive(Debug)]
struct AppSymbol {
    name: String,
    section_kind: SectionKind,
    offset_in_section: usize,
}

#[derive(Debug, Default)]
struct AppSections {
    sections: Vec<Section>,
    symbols: Vec<AppSymbol>,
}

impl AppSections {
    fn from_data(data: &[u8]) -> Self {
        use object::ObjectSection;

        let file = object::File::parse(data).unwrap();

        let mut sections = Vec::new();

        let mut section_starts = MutMap::default();

        let mut text_bytes = 0;
        let mut rdata_bytes = 0;

        for (i, section) in file.sections().enumerate() {
            let kind = match section.name() {
                Ok(".text") => SectionKind::Text,
                // Ok(".data") => SectionKind::Data,
                Ok(".rdata") => SectionKind::ReadOnlyData,

                _ => continue,
            };

            let mut relocations = MutMap::default();

            for (offset_in_section, relocation) in section.relocations() {
                match relocation.target() {
                    RelocationTarget::Symbol(symbol_index) => {
                        use object::ObjectSymbol;

                        let symbol = file.symbol_by_index(symbol_index);

                        let address = symbol.as_ref().map(|s| s.address()).unwrap_or_default();
                        let name = symbol.and_then(|s| s.name()).unwrap_or_default();

                        relocations.insert(
                            name.to_string(),
                            AppRelocation {
                                offset_in_section,
                                address,
                                relocation,
                            },
                        );
                    }
                    _ => todo!(),
                }
            }

            let (start, length) = section.file_range().unwrap();
            let file_range = start as usize..(start + length) as usize;

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
                file_range,
                kind,
                relocations,
            };

            sections.push(section);
        }

        let mut symbols = Vec::new();

        for symbol in file.symbols() {
            use object::ObjectSymbol;

            if symbol.name_bytes().unwrap_or_default().starts_with(b"roc") {
                if let object::SymbolSection::Section(index) = symbol.section() {
                    let (kind, offset_in_host_section) = section_starts[&index];

                    let symbol = AppSymbol {
                        name: symbol.name().unwrap_or_default().to_string(),
                        section_kind: kind,
                        offset_in_section: (offset_in_host_section + symbol.address()) as usize,
                    };

                    symbols.push(symbol);
                }
            }
        }

        AppSections { sections, symbols }
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

#[cfg(test)]
mod test {
    const PE_DYNHOST: &[u8] = include_bytes!("../dynhost_benchmarks_windows.exe") as &[_];

    use std::ops::Deref;

    use object::pe::ImageFileHeader;
    use object::read::pe::PeFile64;
    use object::{pe, LittleEndian as LE, Object};

    use indoc::indoc;

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
            dynamic_relocations.imports_offset_in_file,
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

        let mmap = Preprocessor::preprocess(output_file, input_data, new_sections);

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

    fn redirect_dummy_dll_functions_test(
        executable: &mut [u8],
        dynamic_relocations: &DynamicRelocationsPe,
        function_definition_vas: &[(String, u64)],
    ) {
        let object = object::read::pe::PeFile64::parse(&*executable).unwrap();
        let imports: Vec<_> = object
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

        // and get the offset in the file of 0x1400037f0
        let thunks_start_offset = find_thunks_start_offset(executable, dynamic_relocations);

        redirect_dummy_dll_functions(
            executable,
            function_definition_vas,
            &imports,
            thunks_start_offset,
        )
    }

    fn link_zig_host_and_app_help(dir: &Path) {
        use object::ObjectSection;

        let host_zig = indoc!(
            r#"
            const std = @import("std");

            extern const roc_one: u64;
            extern const roc_three: u64;

            extern fn roc_magic1() callconv(.C) u64;
            extern fn roc_magic2() callconv(.C) u8;

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
        );

        let app_zig = indoc!(
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
        );

        let zig = std::env::var("ROC_ZIG").unwrap_or_else(|_| "zig".into());

        std::fs::write(dir.join("host.zig"), host_zig.as_bytes()).unwrap();
        std::fs::write(dir.join("app.zig"), app_zig.as_bytes()).unwrap();

        // we need to compile the app first
        let output = std::process::Command::new(&zig)
            .current_dir(dir)
            .args(&[
                "build-obj",
                "app.zig",
                "-target",
                "x86_64-windows-gnu",
                "--strip",
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

        let roc_app_sections = AppSections::from_data(&*roc_app);
        let mut symbols = roc_app_sections.symbols;

        // make the dummy dylib based on the app object
        let names: Vec<_> = symbols.iter().map(|s| s.name.clone()).collect();
        let dylib_bytes = crate::generate_dylib::synthetic_dll(&names);
        std::fs::write(dir.join("libapp.obj"), dylib_bytes).unwrap();

        // now we can compile the host (it uses libapp.obj, hence the order here)
        let output = std::process::Command::new(&zig)
            .current_dir(dir)
            .args(&[
                "build-exe",
                "libapp.obj",
                "host.zig",
                "-lc",
                "-target",
                "x86_64-windows-gnu",
                "-rdynamic",
                "--strip",
                "-rdynamic",
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

        let data = std::fs::read(dir.join("host.exe")).unwrap();
        let new_sections = [*b".text\0\0\0", *b".rdata\0\0"];
        increase_number_of_sections_help(&data, &new_sections, &dir.join("dynhost.exe"));

        // hardcoded for now, should come from the precompiled metadata in the future
        let image_base: u64 = 0x140000000;
        let file_alignment = 0x200;
        let section_alignment = 0x1000;
        let last_host_section_index = 5;

        let roc_app_sections_size: usize = roc_app_sections
            .sections
            .iter()
            .map(|s| next_multiple_of(s.file_range.end - s.file_range.start, file_alignment))
            .sum();

        let dynhost_bytes = std::fs::read(dir.join("dynhost.exe")).unwrap();

        let mut executable = open_mmap_mut(
            &dir.join("app.exe"),
            dynhost_bytes.len() + roc_app_sections_size,
        );

        // copying over all of the dynhost.exe bytes
        executable[..dynhost_bytes.len()].copy_from_slice(&dynhost_bytes);

        let file = PeFile64::parse(executable.deref()).unwrap();
        let last_host_section = file.sections().nth(last_host_section_index).unwrap();

        let exports: MutMap<String, i64> = file
            .exports()
            .unwrap()
            .into_iter()
            .map(|e| {
                (
                    String::from_utf8(e.name().to_vec()).unwrap(),
                    (e.address() - image_base) as i64,
                )
            })
            .collect();

        let optional_header_offset = file.dos_header().nt_headers_offset() as usize
            + std::mem::size_of::<u32>()
            + std::mem::size_of::<ImageFileHeader>();

        let extra_code_section_va = last_host_section.address()
            + next_multiple_of(
                last_host_section.size() as usize,
                section_alignment as usize,
            ) as u64;

        let mut section_header_start = 624;
        let mut section_file_offset = dynhost_bytes.len();
        let mut virtual_address = (extra_code_section_va - image_base) as u32;

        let mut code_bytes_added = 0;
        let mut data_bytes_added = 0;
        let mut file_bytes_added = 0;

        for kind in [SectionKind::Text, SectionKind::ReadOnlyData] {
            let length: usize = roc_app_sections
                .sections
                .iter()
                .filter(|s| s.kind == kind)
                .map(|s| s.file_range.end - s.file_range.start)
                .sum();

            // offset_in_section now becomes a proper virtual address
            for symbol in symbols.iter_mut() {
                if symbol.section_kind == kind {
                    symbol.offset_in_section += image_base as usize + virtual_address as usize;
                }
            }

            let virtual_size = length as u32;
            let size_of_raw_data = next_multiple_of(length, file_alignment) as u32;

            match kind {
                SectionKind::Text => {
                    code_bytes_added += size_of_raw_data;

                    write_section_header(
                        &mut executable,
                        *b".text1\0\0",
                        pe::IMAGE_SCN_MEM_READ | pe::IMAGE_SCN_CNT_CODE | pe::IMAGE_SCN_MEM_EXECUTE,
                        section_header_start,
                        section_file_offset,
                        virtual_size,
                        virtual_address,
                        size_of_raw_data,
                    );
                }
                SectionKind::ReadOnlyData => {
                    data_bytes_added += size_of_raw_data;

                    write_section_header(
                        &mut executable,
                        *b".rdata1\0",
                        pe::IMAGE_SCN_MEM_READ | pe::IMAGE_SCN_CNT_INITIALIZED_DATA,
                        section_header_start,
                        section_file_offset,
                        virtual_size,
                        virtual_address,
                        size_of_raw_data,
                    );
                }
            }

            let mut offset = section_file_offset;
            let it = roc_app_sections.sections.iter().filter(|s| s.kind == kind);
            for section in it {
                let slice = &roc_app[section.file_range.start..section.file_range.end];
                executable[offset..][..slice.len()].copy_from_slice(slice);

                for (name, app_relocation) in section.relocations.iter() {
                    let destination = exports[name];

                    let AppRelocation {
                        offset_in_section,
                        relocation,
                        ..
                    } = app_relocation;

                    match relocation.kind() {
                        object::RelocationKind::Relative => {
                            // we implicitly only do 32-bit relocations
                            debug_assert_eq!(relocation.size(), 32);

                            let delta =
                                destination - virtual_address as i64 - *offset_in_section as i64
                                    + relocation.addend();

                            executable[offset + *offset_in_section as usize..][..4]
                                .copy_from_slice(&(delta as i32).to_le_bytes());
                        }
                        _ => todo!(),
                    }
                }

                offset += slice.len();
            }

            section_header_start += std::mem::size_of::<ImageSectionHeader>();
            section_file_offset += size_of_raw_data as usize;
            virtual_address += next_multiple_of(length, section_alignment) as u32;
            file_bytes_added += next_multiple_of(length, section_alignment) as u32;
        }

        update_optional_header(
            &mut executable,
            optional_header_offset,
            code_bytes_added as u32,
            file_bytes_added as u32,
            data_bytes_added as u32,
        );

        let dynamic_relocations = DynamicRelocationsPe::new(&executable);
        let symbols: Vec<_> = symbols
            .into_iter()
            .map(|s| (s.name, s.offset_in_section as u64))
            .collect();
        redirect_dummy_dll_functions_test(&mut executable, &dynamic_relocations, &symbols);

        remove_dummy_dll_import_table_test(
            &mut executable,
            dynamic_relocations.data_directories_offset_in_file,
            dynamic_relocations.imports_offset_in_file,
            dynamic_relocations.dummy_import_index,
        );
    }

    #[cfg(windows)]
    #[test]
    fn link_zig_host_and_app_windows() {
        let dir = tempfile::tempdir().unwrap();
        let dir = dir.path();

        link_zig_host_and_app_help(dir);

        let output = std::process::Command::new("app.exe")
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

        assert_eq!("Hello, 234567 32 1 3!\n", output);
    }

    #[ignore]
    #[test]
    fn link_zig_host_and_app_wine() {
        let dir = tempfile::tempdir().unwrap();
        let dir = dir.path();

        link_zig_host_and_app_help(dir);

        let output = std::process::Command::new("wine")
            .current_dir(dir)
            .args(&["app.exe"])
            .output()
            .unwrap();

        if !output.status.success() {
            use std::io::Write;

            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();

            panic!("wine failed");
        }

        let output = String::from_utf8_lossy(&output.stdout);

        assert_eq!("Hello, 234567 32 1 3!\n", output);
    }
}
