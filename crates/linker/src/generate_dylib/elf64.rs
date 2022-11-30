use object::{elf, Endianness};

use crate::pe::next_multiple_of;

pub fn create_dylib_elf64(custom_names: &[String]) -> object::read::Result<Vec<u8>> {
    let endian = Endianness::Little;

    let mut out_data = Vec::new();
    let mut writer = object::write::elf::Writer::new(endian, true, &mut out_data);

    const DYNAMIC_SECTION_INDEX: usize = 2;

    let out_sections_index = [
        writer.reserve_dynsym_section_index(),
        writer.reserve_dynstr_section_index(),
        writer.reserve_dynamic_section_index(),
        writer.reserve_strtab_section_index(),
        writer.reserve_shstrtab_section_index(),
    ];

    let soname = writer.add_dynamic_string(b"libapp.so");
    let out_dynamic = [(elf::DT_SONAME, 1, Some(soname)), (elf::DT_NULL, 0, None)];

    // Assign dynamic symbol indices.
    let out_dynsyms: Vec<_> = custom_names
        .iter()
        .map(|name| {
            (
                writer.reserve_dynamic_symbol_index(),
                writer.add_dynamic_string(name.as_bytes()),
            )
        })
        .collect();

    writer.reserve_file_header();

    let mut program_headers = [
        object::write::elf::ProgramHeader {
            p_type: object::elf::PT_LOAD,
            p_flags: object::elf::PF_R | object::elf::PF_W,
            p_offset: 0,
            p_vaddr: 0,
            p_paddr: 0,
            p_filesz: 0,
            p_memsz: 0,
            p_align: 1 << 12,
        },
        object::write::elf::ProgramHeader {
            p_type: object::elf::PT_DYNAMIC,
            p_flags: object::elf::PF_R | object::elf::PF_W,
            p_offset: 0,
            p_vaddr: 0,
            p_paddr: 0,
            p_filesz: 0,
            p_memsz: 0,
            p_align: 1 << 3,
        },
    ];

    writer.reserve_program_headers(program_headers.len() as u32);

    let dynsym_address = writer.reserved_len();
    writer.reserve_dynsym();

    let dynstr_address = writer.reserved_len();
    writer.reserve_dynstr();

    // aligned to the next multiple of 8
    let dynamic_address = next_multiple_of(writer.reserved_len(), 8);
    writer.reserve_dynamic(out_dynamic.len());

    writer.reserve_strtab();
    writer.reserve_shstrtab();

    writer.reserve_section_headers();

    // just enough program header info to satisfy the dynamic loader
    for program_header in program_headers.iter_mut() {
        match program_header.p_type {
            object::elf::PT_LOAD | object::elf::PT_DYNAMIC => {
                program_header.p_offset = dynamic_address as u64;
                program_header.p_vaddr = dynamic_address as u64 + 0x1000;
                program_header.p_paddr = dynamic_address as u64 + 0x1000;
                program_header.p_filesz = 0x8;
                program_header.p_memsz = 0x8;
            }
            _ => {}
        }
    }

    // WRITING SECTION CONTENT

    writer
        .write_file_header(&object::write::elf::FileHeader {
            os_abi: 0,
            abi_version: 0,
            e_type: 3,
            e_machine: 62,
            e_entry: 0x1000,
            e_flags: 0,
        })
        .unwrap();

    {
        writer.write_align_program_headers();

        for header in program_headers {
            writer.write_program_header(&header);
        }
    }

    // dynsym content
    {
        writer.write_null_dynamic_symbol();
        for (_index, name) in out_dynsyms {
            writer.write_dynamic_symbol(&object::write::elf::Sym {
                name: Some(name),
                section: Some(out_sections_index[DYNAMIC_SECTION_INDEX]),
                st_info: (elf::STB_GLOBAL << 4) | elf::STT_FUNC,
                st_other: 0,
                st_shndx: 0,
                st_value: 0x1000,
                st_size: 0,
            });
        }
    }

    // dynstr content
    writer.write_dynstr();

    // dynamic content
    writer.write_align_dynamic();
    for (tag, val, opt_string) in out_dynamic {
        if let Some(string) = opt_string {
            writer.write_dynamic_string(tag, string);
        } else {
            writer.write_dynamic(tag, val);
        }
    }

    // strtab content
    writer.write_strtab();
    writer.write_shstrtab();

    // SECTION HEADERS

    writer.write_null_section_header();

    writer.write_dynsym_section_header(dynsym_address as _, 1);

    writer.write_dynstr_section_header(dynstr_address as _);

    writer.write_dynamic_section_header(dynamic_address as u64 + 0x1000);

    writer.write_strtab_section_header();
    writer.write_shstrtab_section_header();

    debug_assert_eq!(writer.reserved_len(), writer.len());

    Ok(out_data)
}
