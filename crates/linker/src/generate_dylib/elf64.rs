use object::{elf, Endianness};

pub const fn round_up_to_alignment(width: usize, alignment: usize) -> usize {
    match alignment {
        0 => width,
        1 => width,
        _ => {
            if width % alignment > 0 {
                width + alignment - (width % alignment)
            } else {
                width
            }
        }
    }
}

pub fn create_dylib_elf64(custom_names: &[String]) -> object::read::Result<Vec<u8>> {
    let endian = Endianness::Little;

    let mut out_data = Vec::new();
    let mut writer = object::write::elf::Writer::new(endian, true, &mut out_data);

    const DYNAMIC_SECTION: usize = 2;

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

    let dynamic_count = out_dynamic.len();

    writer.reserve_file_header();

    let dynsym_address = writer.reserved_len();
    writer.reserve_dynsym();

    let dynstr_address = writer.reserved_len();
    writer.reserve_dynstr();

    let dynamic_address = round_up_to_alignment(writer.reserved_len(), 8);
    writer.reserve_dynamic(dynamic_count);

    writer.reserve_strtab();
    writer.reserve_shstrtab();

    writer.reserve_section_headers();

    // writing

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

    // dynsym content
    {
        writer.write_null_dynamic_symbol();
        for (_index, name) in out_dynsyms {
            writer.write_dynamic_symbol(&object::write::elf::Sym {
                name: Some(name),
                section: Some(out_sections_index[DYNAMIC_SECTION]),
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

    writer.write_dynamic_section_header(dynamic_address as _);

    writer.write_strtab_section_header();
    writer.write_shstrtab_section_header();

    debug_assert_eq!(writer.reserved_len(), writer.len());

    Ok(out_data)
}
