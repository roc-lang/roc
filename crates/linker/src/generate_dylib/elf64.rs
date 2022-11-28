use object::{elf, Endianness};

pub fn create_dylib_elf64(custom_names: &[String]) -> object::read::Result<Vec<u8>> {
    match false {
        true => {
            let bytes = create_dylib_elf64_old(custom_names)?;
            std::fs::write("/tmp/libapp.old.so", bytes.as_slice());

            Ok(bytes)
        }
        false => {
            let bytes = create_dylib_elf64_new(custom_names)?;
            std::fs::write("/tmp/libapp.new.so", bytes.as_slice());

            Ok(bytes)
        }
    }
}

pub fn create_dylib_elf64_old(custom_names: &[String]) -> object::read::Result<Vec<u8>> {
    let dummy_obj_file = "/tmp/app.o";

    let obj_target = BinaryFormat::Elf;
    let obj_arch = Architecture::X86_64;

    let mut out_object = write::Object::new(obj_target, obj_arch, Endianness::Little);

    let text_section = out_object.section_id(write::StandardSection::Text);

    use object::write;
    use object::{Architecture, BinaryFormat, SymbolFlags, SymbolKind, SymbolScope};

    for name in custom_names {
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
    }

    std::fs::write(
        dummy_obj_file,
        out_object.write().expect("failed to build output object"),
    )
    .expect("failed to write object to file");

    std::process::Command::new("zig")
        .args(&["build-lib", "app.o", "-dynamic"])
        .current_dir("/tmp")
        .output()
        .unwrap();

    let file = std::fs::read("/tmp/libapp.so").unwrap();

    let modified = copy_file::<elf::FileHeader64<Endianness>>(&file).unwrap();

    Ok(modified)
}

use std::convert::TryInto;
use std::error::Error;

use object::read::elf::{Dyn, FileHeader, ProgramHeader, SectionHeader, Sym};

use crate::pe::next_multiple_of;

struct Section {
    name: Option<object::write::StringId>,
    offset: usize,
}

struct Dynamic {
    tag: u32,
    // Ignored if `string` is set.
    val: u64,
    string: Option<object::write::StringId>,
}

struct Symbol {
    in_sym: usize,
    name: Option<object::write::StringId>,
    section: Option<object::write::elf::SectionIndex>,
}

struct DynamicSymbol {
    in_sym: usize,
    name: Option<object::write::StringId>,
    section: Option<object::write::elf::SectionIndex>,
}

fn copy_file<Elf: FileHeader<Endian = Endianness>>(
    in_data: &[u8],
) -> Result<Vec<u8>, Box<dyn Error>> {
    let in_elf = Elf::parse(in_data)?;
    let endian = in_elf.endian()?;
    let is_mips64el = in_elf.is_mips64el(endian);
    let in_segments = in_elf.program_headers(endian, in_data)?;
    let in_sections = in_elf.sections(endian, in_data)?;
    let in_syms = in_sections.symbols(endian, in_data, elf::SHT_SYMTAB)?;
    let in_dynsyms = in_sections.symbols(endian, in_data, elf::SHT_DYNSYM)?;

    let mut out_data = Vec::new();
    let mut writer = object::write::elf::Writer::new(endian, in_elf.is_class_64(), &mut out_data);

    // Find metadata sections, and assign section indices.
    let mut in_dynamic = None;
    let mut out_sections = Vec::with_capacity(4);
    let mut out_sections_index = Vec::with_capacity(4);
    for (i, in_section) in in_sections.iter().enumerate() {
        let mut name = None;
        let index;
        match in_section.sh_type(endian) {
            elf::SHT_NULL => {
                index = writer.reserve_null_section_index();
            }
            elf::SHT_PROGBITS
            | elf::SHT_NOBITS
            | elf::SHT_NOTE
            | elf::SHT_REL
            | elf::SHT_RELA
            | elf::SHT_INIT_ARRAY
            | elf::SHT_FINI_ARRAY => {
                // name = Some(writer.add_section_name(in_sections.section_name(endian, in_section)?));
                // index = writer.reserve_section_index();
                continue;
            }
            elf::SHT_STRTAB => {
                if i == in_syms.string_section().0 {
                    dbg!();
                    index = writer.reserve_strtab_section_index();
                } else if i == in_dynsyms.string_section().0 {
                    dbg!();
                    index = writer.reserve_dynstr_section_index();
                } else if i == in_elf.shstrndx(endian, in_data)? as usize {
                    dbg!();
                    index = writer.reserve_shstrtab_section_index();
                } else {
                    panic!("Unsupported string section {}", i);
                }
            }
            elf::SHT_SYMTAB => {
                if i == in_syms.section().0 {
                    dbg!("symtab");
                    index = writer.reserve_symtab_section_index();
                } else {
                    panic!("Unsupported symtab section {}", i);
                }
            }
            elf::SHT_SYMTAB_SHNDX => {
                if i == in_syms.shndx_section().0 {
                    dbg!("here");
                    index = writer.reserve_symtab_shndx_section_index();
                } else {
                    panic!("Unsupported symtab shndx section {}", i);
                }
            }
            elf::SHT_DYNSYM => {
                if i == in_dynsyms.section().0 {
                    index = writer.reserve_dynsym_section_index();
                } else {
                    panic!("Unsupported dynsym section {}", i);
                }
            }
            elf::SHT_DYNAMIC => {
                assert!(in_dynamic.is_none());
                in_dynamic = in_section.dynamic(endian, in_data)?;
                debug_assert!(in_dynamic.is_some());
                index = writer.reserve_dynamic_section_index();
            }
            elf::SHT_HASH => {
                // assert!(in_hash.is_none());
                // in_hash = in_section.hash_header(endian, in_data)?;
                // debug_assert!(in_hash.is_some());
                // index = writer.reserve_hash_section_index();
                continue;
            }
            elf::SHT_GNU_HASH => {
                // assert!(in_gnu_hash.is_none());
                // in_gnu_hash = in_section.gnu_hash_header(endian, in_data)?;
                // debug_assert!(in_gnu_hash.is_some());
                // index = writer.reserve_gnu_hash_section_index();
                continue;
            }
            other => {
                panic!("Unsupported section type {:x}", other);
            }
        }
        out_sections.push(Section { name, offset: 0 });
        out_sections_index.push(index);
    }

    // Assign dynamic strings.
    let mut out_dynamic = Vec::new();
    if let Some((in_dynamic, link)) = in_dynamic {
        out_dynamic.reserve(in_dynamic.len());
        let in_dynamic_strings = in_sections.strings(endian, in_data, link)?;
        for d in in_dynamic {
            let tag = d.d_tag(endian).into().try_into()?;
            let val = d.d_val(endian).into();

            if ![elf::DT_SONAME, elf::DT_SYMTAB, elf::DT_STRTAB, elf::DT_NULL]
                .contains(&(tag as u32))
            {
                continue;
            }

            let string = if d.is_string(endian) {
                let s = in_dynamic_strings
                    .get(val.try_into()?)
                    .map_err(|_| "Invalid dynamic string")?;
                Some(writer.add_dynamic_string(s))
            } else {
                None
            };
            out_dynamic.push(Dynamic { tag, val, string });
            if tag == elf::DT_NULL {
                break;
            }
        }
    }

    // Assign dynamic symbol indices.
    let mut out_dynsyms = Vec::with_capacity(in_dynsyms.len());
    for (i, in_dynsym) in in_dynsyms.iter().enumerate().skip(1) {
        let section = match in_dynsyms.symbol_section(endian, in_dynsym, i)? {
            Some(in_section) => {
                // Skip symbols for sections we aren't copying.
                if out_sections_index[in_section.0].0 == 0 {
                    continue;
                }
                Some(out_sections_index[in_section.0])
            }
            None => None,
        };
        let mut name = None;
        if in_dynsym.st_name(endian) != 0 {
            let in_name = in_dynsyms.symbol_name(endian, in_dynsym)?;
            name = Some(writer.add_dynamic_string(in_name));
        };
        out_dynsyms.push(DynamicSymbol {
            in_sym: i,
            name,
            section,
        });
    }
    let mut out_dynsyms_index = vec![Default::default(); in_dynsyms.len()];
    for out_dynsym in out_dynsyms.iter_mut() {
        out_dynsyms_index[out_dynsym.in_sym] = writer.reserve_dynamic_symbol_index();
    }

    // Assign symbol indices.
    let mut num_local = 0;

    // Start reserving file ranges.
    writer.reserve_file_header();

    let mut dynamic_addr = 0;
    let mut dynsym_addr = 0;
    let mut dynstr_addr = 0;

    let mut alloc_sections = Vec::new();

    // We don't support moving program headers.
    assert_eq!(in_elf.e_phoff(endian).into(), writer.reserved_len() as u64);
    writer.reserve_program_headers(in_segments.len() as u32 - 1);

    // Reserve alloc sections at original offsets.
    alloc_sections = in_sections
        .iter()
        .enumerate()
        .filter(|(_, s)| s.sh_flags(endian).into() & u64::from(elf::SHF_ALLOC) != 0)
        .collect();

    // The data for alloc sections may need to be written in a different order
    // from their section headers.
    alloc_sections.sort_by_key(|(_, x)| x.sh_offset(endian).into());
    for (i, in_section) in alloc_sections.iter() {
        writer.reserve_until(in_section.sh_offset(endian).into() as usize);
        match in_section.sh_type(endian) {
            elf::SHT_PROGBITS | elf::SHT_NOTE | elf::SHT_INIT_ARRAY | elf::SHT_FINI_ARRAY => {}
            elf::SHT_DYNAMIC => {
                dynamic_addr = in_section.sh_addr(endian).into();
                writer.reserve_dynamic(out_dynamic.len());
            }
            elf::SHT_DYNSYM if *i == in_dynsyms.section().0 => {
                dynsym_addr = in_section.sh_addr(endian).into();
                writer.reserve_dynsym();
            }
            elf::SHT_STRTAB if *i == in_dynsyms.string_section().0 => {
                dbg!("reserve strtab section");
                dynstr_addr = in_section.sh_addr(endian).into();
                writer.reserve_dynstr();
            }
            elf::SHT_HASH => {}
            elf::SHT_GNU_HASH => {}
            other => {
                panic!("Unsupported alloc section index {}, type {}", *i, other);
            }
        }
    }

    writer.reserve_symtab();
    writer.reserve_symtab_shndx();

    writer.reserve_strtab();
    writer.reserve_shstrtab();

    writer.reserve_section_headers();

    writer.write_file_header(&object::write::elf::FileHeader {
        os_abi: in_elf.e_ident().os_abi,
        abi_version: in_elf.e_ident().abi_version,
        e_type: in_elf.e_type(endian),
        e_machine: in_elf.e_machine(endian),
        e_entry: in_elf.e_entry(endian).into(),
        e_flags: in_elf.e_flags(endian),
    })?;

    writer.write_align_program_headers();
    for in_segment in in_segments {
        if in_segment.p_type(endian) == object::elf::PT_LOAD
            && in_segment.p_flags(endian) == object::elf::PF_R
        {
            continue;
        }

        writer.write_program_header(&object::write::elf::ProgramHeader {
            p_type: in_segment.p_type(endian),
            p_flags: in_segment.p_flags(endian),
            p_offset: in_segment.p_offset(endian).into(),
            p_vaddr: in_segment.p_vaddr(endian).into(),
            p_paddr: in_segment.p_paddr(endian).into(),
            p_filesz: in_segment.p_filesz(endian).into(),
            p_memsz: in_segment.p_memsz(endian).into(),
            p_align: in_segment.p_align(endian).into(),
        });
    }

    for (i, in_section) in alloc_sections.iter() {
        writer.pad_until(in_section.sh_offset(endian).into() as usize);
        match in_section.sh_type(endian) {
            elf::SHT_PROGBITS | elf::SHT_NOTE | elf::SHT_INIT_ARRAY | elf::SHT_FINI_ARRAY => {
                //                debug_assert_eq!(out_sections[*i].offset, writer.len());
                //                writer.write(in_section.data(endian, in_data)?);
            }
            elf::SHT_NOBITS => {}
            elf::SHT_DYNAMIC => {
                for d in &out_dynamic {
                    if let Some(string) = d.string {
                        writer.write_dynamic_string(d.tag, string);
                    } else {
                        // TODO: fix values
                        let val = d.val;
                        writer.write_dynamic(d.tag, val);
                    }
                }
            }
            elf::SHT_DYNSYM if *i == in_dynsyms.section().0 => {
                writer.write_null_dynamic_symbol();
                for sym in &out_dynsyms {
                    let in_dynsym = in_dynsyms.symbol(sym.in_sym)?;
                    writer.write_dynamic_symbol(&object::write::elf::Sym {
                        name: sym.name,
                        section: sym.section,
                        st_info: in_dynsym.st_info(),
                        st_other: in_dynsym.st_other(),
                        st_shndx: in_dynsym.st_shndx(endian),
                        st_value: in_dynsym.st_value(endian).into(),
                        st_size: in_dynsym.st_size(endian).into(),
                    });
                }
            }
            elf::SHT_STRTAB if *i == in_dynsyms.string_section().0 => {
                dbg!("write dynstr");
                writer.write_dynstr();
            }
            elf::SHT_HASH => {}
            elf::SHT_GNU_HASH => {}
            other => {
                panic!("Unsupported alloc section type {:x}", other);
            }
        }
    }

    for (i, in_section) in in_sections.iter().enumerate() {
        if in_section.sh_flags(endian).into() & u64::from(elf::SHF_ALLOC) != 0 {
            continue;
        }
        match in_section.sh_type(endian) {
            elf::SHT_PROGBITS | elf::SHT_NOTE => {
                //                writer.write_align(in_section.sh_addralign(endian).into() as usize);
                //                debug_assert_eq!(out_sections[i].offset, writer.len());
                //                writer.write(in_section.data(endian, in_data)?);
            }
            _ => {}
        }
    }

    writer.write_null_symbol();
    writer.write_symtab_shndx();

    dbg!("write strtab");
    writer.write_strtab();
    writer.write_shstrtab();

    writer.write_null_section_header();
    for (i, in_section) in in_sections.iter().enumerate() {
        match in_section.sh_type(endian) {
            elf::SHT_NULL => {}
            elf::SHT_PROGBITS
            | elf::SHT_NOBITS
            | elf::SHT_NOTE
            | elf::SHT_REL
            | elf::SHT_RELA
            | elf::SHT_INIT_ARRAY
            | elf::SHT_FINI_ARRAY => {
                /*
                let out_section = &out_sections[i];
                let sh_link = out_sections_index[in_section.sh_link(endian) as usize].0 as u32;
                let mut sh_info = in_section.sh_info(endian);
                if in_section.sh_flags(endian).into() as u32 & elf::SHF_INFO_LINK != 0 {
                    sh_info = out_sections_index[sh_info as usize].0 as u32;
                }
                writer.write_section_header(&object::write::elf::SectionHeader {
                    name: out_section.name,
                    sh_type: in_section.sh_type(endian),
                    sh_flags: in_section.sh_flags(endian).into(),
                    sh_addr: in_section.sh_addr(endian).into(),
                    sh_offset: out_section.offset as u64,
                    sh_size: in_section.sh_size(endian).into(),
                    sh_link,
                    sh_info,
                    sh_addralign: in_section.sh_addralign(endian).into(),
                    sh_entsize: in_section.sh_entsize(endian).into(),
                });
                */
            }
            elf::SHT_STRTAB => {
                if i == in_syms.string_section().0 {
                    dbg!();
                    writer.write_strtab_section_header();
                } else if i == in_dynsyms.string_section().0 {
                    dbg!();
                    writer.write_dynstr_section_header(dynstr_addr);
                } else if i == in_elf.shstrndx(endian, in_data)? as usize {
                    dbg!();
                    writer.write_shstrtab_section_header();
                } else {
                    panic!("Unsupported string section {}", i);
                }
            }
            elf::SHT_SYMTAB => {
                if i == in_syms.section().0 {
                    dbg!(num_local);
                    writer.write_symtab_section_header(num_local);
                } else {
                    panic!("Unsupported symtab section {}", i);
                }
            }
            elf::SHT_SYMTAB_SHNDX => {
                if i == in_syms.shndx_section().0 {
                    writer.write_symtab_shndx_section_header();
                } else {
                    panic!("Unsupported symtab shndx section {}", i);
                }
            }
            elf::SHT_DYNSYM => {
                if i == in_dynsyms.section().0 {
                    writer.write_dynsym_section_header(dynsym_addr, 1);
                } else {
                    panic!("Unsupported dynsym section {}", i);
                }
            }
            elf::SHT_DYNAMIC => {
                writer.write_dynamic_section_header(dynamic_addr);
            }
            elf::SHT_HASH => {
                writer.write_hash_section_header(0);
            }
            elf::SHT_GNU_HASH => {
                writer.write_gnu_hash_section_header(0);
            }
            other => {
                panic!("Unsupported section type {:x}", other);
            }
        }
    }
    debug_assert_eq!(writer.reserved_len(), writer.len());

    Ok(out_data)
}

pub fn create_dylib_elf64_new(custom_names: &[String]) -> object::read::Result<Vec<u8>> {
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
    let out_dynamic = [
        (elf::DT_SONAME, 1, Some(soname)),
        (elf::DT_SYMTAB, 0x158, None),
        (elf::DT_STRTAB, 0x3b0, None),
        (elf::DT_NULL, 0, None),
    ];

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
