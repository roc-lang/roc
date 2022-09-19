use std::{path::Path, time::Instant};

use memmap2::{Mmap, MmapMut};
use object::{
    pe::{ImageImportDescriptor, ImageNtHeaders64, ImageSectionHeader, ImageThunkData64},
    read::pe::ImportTable,
    LittleEndian as LE,
};
use roc_collections::MutMap;
use roc_error_macros::internal_error;

use crate::load_struct_inplace_mut;

#[allow(dead_code)]
pub(crate) fn preprocess_windows(
    exec_filename: &str,
    _metadata_filename: &str,
    _out_filename: &str,
    _shared_lib: &Path,
    _verbose: bool,
    _time: bool,
) -> object::read::Result<()> {
    let total_start = Instant::now();
    let exec_parsing_start = total_start;
    let exec_file = std::fs::File::open(exec_filename).unwrap_or_else(|e| internal_error!("{}", e));
    let exec_mmap = unsafe { Mmap::map(&exec_file).unwrap_or_else(|e| internal_error!("{}", e)) };
    let exec_data = &*exec_mmap;
    let _exec_obj = match object::read::pe::PeFile64::parse(exec_data) {
        Ok(obj) => obj,
        Err(err) => {
            internal_error!("Failed to parse executable file: {}", err);
        }
    };

    let _exec_parsing_duration = exec_parsing_start.elapsed();

    let dynamic_relocations = DynamicRelocationsPe::new(exec_data);

    println!("{:#x?}", dynamic_relocations);

    Ok(())
}

#[derive(Debug)]
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
    #[allow(dead_code)]
    imports_offset_in_file: u32,

    /// Offset in the file of the data directories
    #[allow(dead_code)]
    data_directories_offset_in_file: u32,

    /// The dummy .dll is the `dummy_import_index`th import of the host .exe
    #[allow(dead_code)]
    dummy_import_index: u32,
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

            if name == b"roc-cheaty-lib.dll" {
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
        use object::pe;
        use object::read::pe::ImageNtHeaders;

        let dos_header = pe::ImageDosHeader::parse(data)?;
        let mut offset = dos_header.nt_headers_offset().into();
        let data_directories_offset_in_file =
            offset as u32 + std::mem::size_of::<ImageNtHeaders64>() as u32;

        let (nt_headers, data_directories) = ImageNtHeaders64::parse(data, &mut offset)?;
        let sections = nt_headers.sections(data, offset)?;

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
        };

        this.append_roc_imports(&import_table, &descriptor)?;

        Ok(this)
    }
}

#[allow(dead_code)]
fn remove_dummy_dll_import_table(
    data: &mut [u8],
    data_directories_offset_in_file: u32,
    imports_offset_in_file: u32,
    dummy_import_index: u32,
) {
    use object::pe;

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

/// Preprocess the host's .exe to make space for extra sections
///
/// We will later take code and data sections from the app and concatenate them with this
/// preprocessed host. That means we need to do some bookkeeping: add extra entries to the
/// section table, update the header with the new section count, and (because we added data)
/// update existing section headers to point to a different (shifted) location in the file
#[allow(dead_code)]
struct Preprocessor {
    header_offset: u64,
    additional_length: usize,
    extra_sections_start: usize,
    extra_sections_width: usize,
    section_count_offset: u64,
    section_table_offset: u64,
    old_section_count: usize,
    new_section_count: usize,
    old_headers_size: usize,
    new_headers_size: usize,
}

impl Preprocessor {
    const SECTION_HEADER_WIDTH: usize = std::mem::size_of::<ImageSectionHeader>();

    #[allow(dead_code)]
    fn preprocess(output_path: &Path, data: &[u8], extra_sections: &[[u8; 8]]) -> MmapMut {
        let this = Self::new(data, extra_sections);
        let mut result = mmap_mut(output_path, data.len() + this.additional_length);

        this.copy(&mut result, data);
        this.fix(&mut result, extra_sections);

        result
    }

    fn new(data: &[u8], extra_sections: &[[u8; 8]]) -> Self {
        use object::pe;
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
        let extra_alignments = (extra_sections_width + file_alignment - 1) / file_alignment;
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
            old_section_count: sections.len(),
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
        // start of the first new section

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

fn mmap_mut(path: &Path, length: usize) -> MmapMut {
    let out_file = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)
        .unwrap_or_else(|e| internal_error!("{e}"));
    out_file
        .set_len(length as u64)
        .unwrap_or_else(|e| internal_error!("{e}"));

    unsafe { MmapMut::map_mut(&out_file).unwrap_or_else(|e| internal_error!("{e}")) }
}

#[allow(dead_code)]
fn redirect_dummy_dll_functions(
    app: &mut [u8],
    dynamic_relocations: &DynamicRelocationsPe,
    function_definition_vas: &[u64],
) {
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
        load_struct_inplace_mut::<ImageImportDescriptor>(app, dummy_import_desc_start);

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

    let data: &[u8] = app;

    let dos_header = object::pe::ImageDosHeader::parse(data).unwrap();
    let mut offset = dos_header.nt_headers_offset().into();

    use object::read::pe::ImageNtHeaders;
    let (nt_headers, _data_directories) = ImageNtHeaders64::parse(data, &mut offset).unwrap();
    let sections = nt_headers.sections(data, offset).unwrap();

    // so we look at what section this virtual address is in
    let (section_va, offset_in_file) = sections
        .iter()
        .find_map(|section| {
            section.pe_data_containing(data, dummy_thunks_address).map(
                |(_section_data, section_va)| (section_va, section.pointer_to_raw_data.get(LE)),
            )
        })
        .expect("Invalid thunk virtual address");

    // and get the offset in the file of 0x1400037f0
    let thunks_start_offset = (dummy_thunks_address - section_va + offset_in_file) as usize;

    for (i, target) in function_definition_vas.iter().enumerate() {
        // addresses are 64-bit values
        let address_bytes = &mut app[thunks_start_offset + i * 8..][..8];

        // the array of addresses is null-terminated; make sure we haven't reached the end
        let current = u64::from_le_bytes(address_bytes.try_into().unwrap());
        if current == 0 {
            internal_error!("invalid state: fewer thunks than function addresses");
        }

        // update the address to a function VA
        address_bytes.copy_from_slice(&target.to_le_bytes());
    }
}

#[cfg(test)]
mod test {
    const PE_DYNHOST: &[u8] = include_bytes!("../dynhost_benchmarks_windows.exe") as &[_];

    use std::ops::Deref;

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
            .filter(|import| import.library() == b"roc-cheaty-lib.dll")
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

        remove_dummy_dll_import_table(
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
                .filter(|import| import.library() == b"roc-cheaty-lib.dll")
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
    ) {
        use object::read::pe::ImageNtHeaders;

        let dos_header = pe::ImageDosHeader::parse(input_data).unwrap();
        let mut offset = dos_header.nt_headers_offset().into();
        let header_offset = offset;
        let number_of_sections_offset = header_offset + 4 + 2;

        let sections_len_old = u16::from_le_bytes(
            input_data[number_of_sections_offset as usize..][..2]
                .try_into()
                .unwrap(),
        );

        let mmap = Preprocessor::preprocess(output_file, input_data, new_sections);

        let data = mmap.deref();

        let sections_len_new = u16::from_le_bytes(
            data[number_of_sections_offset as usize..][..2]
                .try_into()
                .unwrap(),
        );

        assert_eq!(
            sections_len_old + new_sections.len() as u16,
            sections_len_new
        );

        let (nt_headers, _data_directories) = ImageNtHeaders64::parse(data, &mut offset).unwrap();
        let sections = nt_headers.sections(data, offset).unwrap();

        let names: Vec<_> = sections
            .iter()
            .map(|h| h.name)
            .skip(sections_len_old as usize)
            .take(new_sections.len())
            .collect();

        assert_eq!(new_sections, names.as_slice());
    }

    #[test]
    fn increase_number_of_sections() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.exe");

        let new_sections = [*b"placehol", *b"aaaabbbb"];

        increase_number_of_sections_help(PE_DYNHOST, &new_sections, &path);
    }

    fn link_zig_host_and_app_help(dir: &Path) {
        use object::ObjectSection;

        let host_zig = indoc!(
            r#"
            const std = @import("std");

            extern fn magic() callconv(.C) u64;

            pub fn main() !void {
                const stdout = std.io.getStdOut().writer();
                try stdout.print("Hello, {}!\n", .{magic()});
            }
            "#
        );

        let app_zig = indoc!(
            r#"
            export fn magic() u64 {
                return 42;
            }
            "#
        );

        let zig = std::env::var("ROC_ZIG").unwrap_or_else(|_| "zig".into());

        std::fs::write(dir.join("host.zig"), host_zig.as_bytes()).unwrap();
        std::fs::write(dir.join("app.zig"), app_zig.as_bytes()).unwrap();

        let dylib_bytes = crate::generate_dylib::synthetic_dll(&["magic".into()]);
        std::fs::write(dir.join("libapp.obj"), dylib_bytes).unwrap();

        let output = std::process::Command::new(&zig)
            .current_dir(dir)
            .args(&[
                "build-exe",
                "libapp.obj",
                "host.zig",
                "-lc",
                "-target",
                "x86_64-windows-gnu",
                "--strip",
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

        let output = std::process::Command::new(&zig)
            .current_dir(dir)
            .args(&[
                "build-obj",
                "app.zig",
                "-target",
                "x86_64-windows-gnu",
                "--strip",
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

        let mut app = std::fs::read(dir.join("host.exe")).unwrap();
        let dynamic_relocations = DynamicRelocationsPe::new(&app);

        // Here we manually add the app.obj assembly to the host's .text section
        // We can "just" do this because the space reserved for this section is bigger
        // than what is actually used (because PE rounds up sections to a big alignment,
        // here 0x200)
        //
        // So, we append the bytes at the end of the current .text section, redirect the call
        // to `magic` to the virtual address of the .text section (so right before the new
        // assembly bytes that we added) and then update the length of the .text section

        let file = PeFile64::parse(app.as_slice()).unwrap();
        let text_section = file.sections().next().unwrap();

        // end of the code section as an offset into the file
        let code_section_end =
            (text_section.file_range().unwrap().0 + text_section.size()) as usize;

        // the virtual address of the end of the text section
        let code_section_end_virtual = text_section.address() + text_section.size();

        // this is cheating a little bit: these bytes are copied from the `app.obj`
        // we could get them out programatically of course.
        let app_bytes: &[u8] = &[0xb8, 0x2a, 0, 0, 0, 0xc3];

        // put our new code into the existing .text section
        app[code_section_end..][..app_bytes.len()].copy_from_slice(app_bytes);

        redirect_dummy_dll_functions(&mut app, &dynamic_relocations, &[code_section_end_virtual]);

        remove_dummy_dll_import_table(
            &mut app,
            dynamic_relocations.data_directories_offset_in_file,
            dynamic_relocations.imports_offset_in_file,
            dynamic_relocations.dummy_import_index,
        );

        // again, a constant that we could get programatically
        let section_table_offset = 384;

        // update the size of the .text section (which is the first section header)
        let p = load_struct_inplace_mut::<ImageSectionHeader>(&mut app, section_table_offset);
        p.virtual_size
            .set(LE, p.virtual_size.get(LE) + app_bytes.len() as u32);

        std::fs::write(dir.join("hostapp.exe"), app).unwrap()
    }

    #[ignore]
    #[test]
    fn link_zig_host_and_app_wine() {
        let dir = tempfile::tempdir().unwrap();
        let dir = dir.path();

        link_zig_host_and_app_help(dir);

        let output = std::process::Command::new("wine")
            .current_dir(dir)
            .args(&["hostapp.exe"])
            .output()
            .unwrap();

        if !output.status.success() {
            use std::io::Write;

            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();

            panic!("wine failed");
        }

        let output = String::from_utf8_lossy(&output.stdout);

        assert_eq!("Hello, 42!\n", output);
    }
}
