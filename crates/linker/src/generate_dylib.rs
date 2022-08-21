use std::convert::TryInto;
use std::error::Error;

use object::elf::{self, SectionHeader64};
use object::read::elf::{Dyn, FileHeader, ProgramHeader, SectionHeader, Sym};
use object::Endianness;

use object::write::elf::Writer;
use roc_error_macros::internal_error;

// an empty shared library, that we build on top of
const DUMMY: &[u8] = include_bytes!("../dummy-elf64-x86-64.so");

pub fn generate(custom_names: &[String]) -> Result<Vec<u8>, Box<dyn Error>> {
    let kind = match object::FileKind::parse(DUMMY) {
        Ok(file) => file,
        Err(err) => {
            internal_error!("Failed to parse file: {}", err);
        }
    };

    match kind {
        object::FileKind::Elf64 => copy_file(DUMMY, custom_names),
        _ => {
            internal_error!("Not an ELF file");
        }
    }
}

struct Section {
    name: Option<object::write::StringId>,
    offset: usize,
}

#[derive(Debug)]
struct Dynamic {
    tag: u32,
    // Ignored if `string` is set.
    val: u64,
    string: Option<object::write::StringId>,
}

#[derive(Debug)]
struct Symbol {
    in_sym: usize,
    name: Option<object::write::StringId>,
}

#[derive(Debug)]
struct DynamicSymbol {
    in_sym: usize,
    name: Option<object::write::StringId>,
    section: Option<object::write::elf::SectionIndex>,
    hash: Option<u32>,
    gnu_hash: Option<u32>,
}

#[derive(Debug)]
struct MySection {
    in_index: usize,
    section: SectionHeader64<object::Endianness>,
}

struct Sections {
    hash: MySection,
    gnu_hash: MySection,
    dynsym: MySection,
    dynstr: MySection,
    eh_frame: MySection,
    dynamic: MySection,
    symtab: MySection,
    strtab: MySection,
    shstrtab: MySection,
}

struct Addresses {
    hash: u64,
    gnu_hash: u64,
    dynsym: u64,
    dynstr: u64,
    dynamic: u64,
}

impl Sections {
    fn iter(&self) -> impl Iterator<Item = (usize, &'_ MySection)> + '_ {
        [
            &self.hash,
            &self.gnu_hash,
            &self.dynsym,
            &self.dynstr,
            &self.eh_frame,
            &self.dynamic,
            &self.symtab,
            &self.strtab,
            &self.shstrtab,
        ]
        .into_iter()
        .enumerate()
    }

    fn reserve_alloc_sections(
        &self,
        writer: &mut Writer,
        hash_chain_count: u32,
        gnu_hash_symbol_count: u32,
        dynamic_count: usize,
    ) -> ([usize; 6], Addresses) {
        let sections = self;
        let endian = Endianness::Little;

        let mut offsets = [0; 6];

        let mut extra_offset: usize = 0;

        // elf::SHT_HASH
        let in_section = sections.hash.section;
        let offset = in_section.sh_offset(endian) as usize;
        offsets[0] = offset;
        writer.reserve_until(offset);

        let hash_addr = in_section.sh_addr(endian);
        // here we fake a bigger hash table than the input file has
        // let hash = in_hash.as_ref().unwrap();
        // writer.reserve_hash(hash.bucket_count.get(endian), hash_chain_count);
        let reserved_before = writer.reserved_len();
        writer.reserve_hash(3, hash_chain_count);
        let reserved_after = writer.reserved_len();

        extra_offset += {
            let actual_size = reserved_after - reserved_before;

            actual_size - sections.hash.section.sh_size(endian) as usize
        };

        // elf::SHT_GNU_HASH
        let in_section = &sections.gnu_hash.section;
        let offset = in_section.sh_offset(endian) as usize + extra_offset;
        offsets[1] = offset;
        writer.reserve_until(offset);

        let gnu_hash_addr = in_section.sh_addr(endian);
        // let hash = in_gnu_hash.as_ref().unwrap();
        let reserved_before = writer.reserved_len();
        writer.reserve_gnu_hash(
            // hash.bloom_count.get(endian),
            // hash.bucket_count.get(endian),
            1,
            1,
            gnu_hash_symbol_count,
        );
        let reserved_after = writer.reserved_len();

        extra_offset += {
            let actual_size = reserved_after - reserved_before;

            actual_size - sections.gnu_hash.section.sh_size(endian) as usize
        };

        // elf::SHT_DYNSYM
        let in_section = &sections.dynsym.section;
        let offset = in_section.sh_offset(endian) as usize + extra_offset;
        offsets[2] = offset;
        writer.reserve_until(offset);

        let dynsym_addr = in_section.sh_addr(endian);
        let reserved_before = writer.reserved_len();
        writer.reserve_dynsym();
        let reserved_after = writer.reserved_len();

        extra_offset += {
            let actual_size = reserved_after - reserved_before;

            actual_size - sections.dynsym.section.sh_size(endian) as usize
        };

        // elf::SHT_STRTAB
        let in_section = &sections.dynstr.section;
        let offset = in_section.sh_offset(endian) as usize + extra_offset;
        offsets[3] = offset;
        writer.reserve_until(offset);

        let dynstr_addr = in_section.sh_addr(endian);
        let reserved_before = writer.reserved_len();
        writer.reserve_dynstr();
        let reserved_after = writer.reserved_len();

        extra_offset += {
            let actual_size = reserved_after - reserved_before;

            actual_size - sections.dynstr.section.sh_size(endian) as usize
        };

        // elf::SHT_PROGBITS
        let in_section = &sections.eh_frame.section;
        let offset = in_section.sh_offset(endian) as usize + extra_offset;
        offsets[4] = offset;
        writer.reserve_until(offset);
        // out_sections[*i].offset = writer.reserve(in_section.sh_size(endian) as usize, 1);
        let reserved_before = writer.reserved_len();
        writer.reserve(in_section.sh_size(endian) as usize, 1);
        let reserved_after = writer.reserved_len();

        extra_offset += {
            let actual_size = reserved_after - reserved_before;

            actual_size - sections.eh_frame.section.sh_size(endian) as usize
        };

        // elf::SHT_DYNAMIC
        let in_section = &sections.dynamic.section;
        let offset = in_section.sh_offset(endian) as usize + extra_offset;
        offsets[5] = round_up_to_alignment(offset, 8); // seems like this needs to be aligned?!
        writer.reserve_until(offset);

        let dynamic_addr = in_section.sh_addr(endian);
        writer.reserve_dynamic(dynamic_count);

        // symtab, strtab and shstrtab is ignored because they are not ALLOC
        let addresses = Addresses {
            hash: hash_addr,
            gnu_hash: gnu_hash_addr,
            dynsym: dynsym_addr,
            dynstr: dynstr_addr,
            dynamic: dynamic_addr,
        };

        (offsets, addresses)
    }
}

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

fn copy_file(in_data: &[u8], custom_names: &[String]) -> Result<Vec<u8>, Box<dyn Error>> {
    let in_elf: &elf::FileHeader64<Endianness> = elf::FileHeader64::parse(in_data)?;
    let endian = in_elf.endian()?;
    let in_segments = in_elf.program_headers(endian, in_data)?;
    let in_sections = in_elf.sections(endian, in_data)?;
    let in_syms = in_sections.symbols(endian, in_data, elf::SHT_SYMTAB)?;
    let in_dynsyms = in_sections.symbols(endian, in_data, elf::SHT_DYNSYM)?;

    let help = |name: &[u8]| {
        let (in_index, section) = in_sections
            .section_by_name(Endianness::Little, name)
            .unwrap();

        MySection {
            in_index,
            section: *section,
        }
    };

    let sections = Sections {
        hash: help(b".hash" as &[_]),
        gnu_hash: help(b".gnu.hash" as &[_]),
        dynsym: help(b".dynsym" as &[_]),
        dynstr: help(b".dynstr" as &[_]),
        eh_frame: help(b".eh_frame" as &[_]),
        dynamic: help(b".dynamic" as &[_]),
        symtab: help(b".symtab" as &[_]),
        strtab: help(b".strtab" as &[_]),
        shstrtab: help(b".shstrtab" as &[_]),
    };

    let mut out_data = Vec::new();
    let mut writer = object::write::elf::Writer::new(endian, in_elf.is_class_64(), &mut out_data);

    // Find metadata sections, and assign section indices.
    let mut in_dynamic = None;
    let mut in_hash = None;
    let mut in_gnu_hash = None;
    let mut out_sections = Vec::with_capacity(in_sections.len());
    let mut out_sections_index = Vec::with_capacity(in_sections.len());
    for (_enum_index, my_section) in sections.iter() {
        let i = my_section.in_index;
        let in_section = &my_section.section;

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
                name = Some(writer.add_section_name(in_sections.section_name(endian, in_section)?));
                index = writer.reserve_section_index();
            }
            elf::SHT_STRTAB => {
                if i == in_syms.string_section().0 {
                    index = writer.reserve_strtab_section_index();
                } else if i == in_dynsyms.string_section().0 {
                    index = writer.reserve_dynstr_section_index();
                } else if i == in_elf.shstrndx(endian, in_data)? as usize {
                    index = writer.reserve_shstrtab_section_index();
                } else {
                    panic!("Unsupported string section {}", i);
                }
            }
            elf::SHT_SYMTAB => {
                if i == in_syms.section().0 {
                    index = writer.reserve_symtab_section_index();
                } else {
                    panic!("Unsupported symtab section {}", i);
                }
            }
            elf::SHT_SYMTAB_SHNDX => {
                if i == in_syms.shndx_section().0 {
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
                assert!(in_hash.is_none());
                in_hash = in_section.hash_header(endian, in_data)?;
                debug_assert!(in_hash.is_some());
                index = writer.reserve_hash_section_index();
            }
            elf::SHT_GNU_HASH => {
                assert!(in_gnu_hash.is_none());
                in_gnu_hash = in_section.gnu_hash_header(endian, in_data)?;
                debug_assert!(in_gnu_hash.is_some());
                index = writer.reserve_gnu_hash_section_index();
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
            let tag: u32 = d.d_tag(endian) as u32;
            let val: u64 = d.d_val(endian);
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

    for (j, name) in custom_names.iter().enumerate() {
        let i = in_dynsyms.len().saturating_sub(1) + j;

        let in_name = name.as_bytes();
        let name = Some(writer.add_dynamic_string(in_name));

        let hash = Some(elf::hash(in_name));
        let gnu_hash = Some(elf::gnu_hash(in_name));

        // .eh_frame
        let section = Some(out_sections_index[5]);

        out_dynsyms.push(DynamicSymbol {
            in_sym: i,
            name,
            section,
            hash,
            gnu_hash,
        })
    }

    // We must sort for GNU hash before allocating symbol indices.
    if let Some(in_gnu_hash) = in_gnu_hash.as_ref() {
        //        // TODO: recalculate bucket_count
        out_dynsyms.sort_by_key(|sym| match sym.gnu_hash {
            None => (0, 0),
            Some(hash) => (1, hash % in_gnu_hash.bucket_count.get(endian)),
        });
    }
    let mut out_dynsyms_index = vec![Default::default(); in_dynsyms.len() + custom_names.len()];
    for out_dynsym in out_dynsyms.iter_mut() {
        out_dynsyms_index[out_dynsym.in_sym] = writer.reserve_dynamic_symbol_index();
    }

    // Hash parameters.
    let hash_index_base = out_dynsyms
        .first()
        .map(|sym| out_dynsyms_index[sym.in_sym].0)
        .unwrap_or(0);
    let hash_chain_count = writer.dynamic_symbol_count();

    // GNU hash parameters.
    let gnu_hash_index_base = out_dynsyms
        .iter()
        .position(|sym| sym.gnu_hash.is_some())
        .unwrap_or(0);
    let gnu_hash_symbol_base = out_dynsyms
        .iter()
        .find(|sym| sym.gnu_hash.is_some())
        .map(|sym| out_dynsyms_index[sym.in_sym].0)
        .unwrap_or_else(|| writer.dynamic_symbol_count());
    let gnu_hash_symbol_count = writer.dynamic_symbol_count() - gnu_hash_symbol_base;

    // Assign symbol indices.
    let mut num_local = 0;
    let mut out_syms = Vec::with_capacity(in_syms.len());
    let mut out_syms_index = Vec::with_capacity(in_syms.len());
    out_syms_index.push(Default::default());
    for (i, in_sym) in in_syms.iter().enumerate().skip(1) {
        let section = match in_syms.symbol_section(endian, in_sym, i)? {
            Some(in_section) => {
                // Skip symbols for sections we aren't copying.
                match out_sections_index.get(in_section.0) {
                    None => continue,
                    Some(index) => {
                        if index.0 == 0 {
                            out_syms_index.push(Default::default());
                            continue;
                        } else {
                            Some(out_sections_index[in_section.0])
                        }
                    }
                }
            }
            None => None,
        };
        out_syms_index.push(writer.reserve_symbol_index(section));
        let name = if in_sym.st_name(endian) != 0 {
            let s = in_syms.symbol_name(endian, in_sym)?;
            Some(writer.add_string(s))
        } else {
            None
        };
        out_syms.push(Symbol { in_sym: i, name });
        if in_sym.st_bind() == elf::STB_LOCAL {
            num_local = writer.symbol_count();
        }
    }

    // Start reserving file ranges.
    writer.reserve_file_header();

    // We don't support moving program headers.
    assert_eq!(in_elf.e_phoff(endian), writer.reserved_len() as u64);
    writer.reserve_program_headers(in_segments.len() as u32);

    // Reserve alloc sections at original offsets.
    let alloc_sections: Vec<_> = in_sections
        .iter()
        .enumerate()
        .filter(|(_, s)| s.sh_flags(endian) & u64::from(elf::SHF_ALLOC) != 0)
        .collect();

    let (offsets, addresses) = sections.reserve_alloc_sections(
        &mut writer,
        hash_chain_count,
        gnu_hash_symbol_count,
        out_dynamic.len(),
    );

    // Reserve non-alloc sections at any offset.
    for (i, in_section) in in_sections.iter().enumerate() {
        if in_section.sh_flags(endian) & u64::from(elf::SHF_ALLOC) != 0 {
            continue;
        }
        match in_section.sh_type(endian) {
            elf::SHT_PROGBITS | elf::SHT_NOTE => {
                out_sections[i].offset = writer.reserve(
                    in_section.sh_size(endian) as usize,
                    in_section.sh_addralign(endian) as usize,
                );
            }
            _ => {}
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
        e_entry: in_elf.e_entry(endian),
        e_flags: in_elf.e_flags(endian),
    })?;

    writer.write_align_program_headers();
    for in_segment in in_segments {
        if in_segment.p_type(endian) == elf::PT_DYNAMIC {
            writer.write_program_header(&object::write::elf::ProgramHeader {
                p_type: in_segment.p_type(endian),
                p_flags: in_segment.p_flags(endian),
                // dirty hack really. Not sure if this is correct on its own
                p_offset: offsets[5] as _,
                p_vaddr: in_segment.p_vaddr(endian),
                p_paddr: in_segment.p_paddr(endian),
                p_filesz: in_segment.p_filesz(endian),
                p_memsz: in_segment.p_memsz(endian),
                p_align: in_segment.p_align(endian),
            });
        } else {
            writer.write_program_header(&object::write::elf::ProgramHeader {
                p_type: in_segment.p_type(endian),
                p_flags: in_segment.p_flags(endian),
                // dirty hack really. Not sure if this is correct on its own
                p_offset: if in_segment.p_offset(endian) > 0 {
                    offsets[5] as _
                } else {
                    0
                },
                p_vaddr: in_segment.p_vaddr(endian),
                p_paddr: in_segment.p_paddr(endian),
                p_filesz: in_segment.p_filesz(endian),
                p_memsz: in_segment.p_memsz(endian),
                p_align: in_segment.p_align(endian),
            });
        }
    }

    for (i, in_section) in alloc_sections.iter() {
        // writer.pad_until(in_section.sh_offset(endian) as usize);
        writer.pad_until(offsets[*i - 1]);
        match in_section.sh_type(endian) {
            elf::SHT_PROGBITS | elf::SHT_NOTE | elf::SHT_INIT_ARRAY | elf::SHT_FINI_ARRAY => {
                writer.write(in_section.data(endian, in_data)?);
            }
            elf::SHT_DYNAMIC => {
                for d in &out_dynamic {
                    if let Some(string) = d.string {
                        writer.write_dynamic_string(d.tag, string);
                    } else {
                        writer.write_dynamic(d.tag, d.val);
                    }
                }
            }
            elf::SHT_DYNSYM if *i == in_dynsyms.section().0 => {
                writer.write_null_dynamic_symbol();
                for sym in &out_dynsyms {
                    writer.write_dynamic_symbol(&object::write::elf::Sym {
                        name: sym.name,
                        section: sym.section,
                        st_info: (elf::STB_GLOBAL << 4) | elf::STT_FUNC,
                        st_other: 0,
                        st_shndx: 0,
                        st_value: 0x1000,
                        st_size: 0,
                    });
                }
            }
            elf::SHT_STRTAB if *i == in_dynsyms.string_section().0 => {
                writer.write_dynstr();
            }
            elf::SHT_HASH => {
                let hash = in_hash.as_ref().unwrap();
                writer.write_hash(hash.bucket_count.get(endian), hash_chain_count, |index| {
                    out_dynsyms
                        .get(index.checked_sub(hash_index_base)? as usize)?
                        .hash
                });
            }
            elf::SHT_GNU_HASH => {
                let gnu_hash = in_gnu_hash.as_ref().unwrap();
                writer.write_gnu_hash(
                    gnu_hash_symbol_base,
                    gnu_hash.bloom_shift.get(endian),
                    gnu_hash.bloom_count.get(endian),
                    gnu_hash.bucket_count.get(endian),
                    gnu_hash_symbol_count,
                    |index| {
                        out_dynsyms[gnu_hash_index_base + index as usize]
                            .gnu_hash
                            .unwrap()
                    },
                );
            }
            other => {
                panic!("Unsupported alloc section type {:x}", other);
            }
        }
    }

    for (i, in_section) in in_sections.iter().enumerate() {
        if in_section.sh_flags(endian) & u64::from(elf::SHF_ALLOC) != 0 {
            continue;
        }
        match in_section.sh_type(endian) {
            elf::SHT_PROGBITS | elf::SHT_NOTE => {
                writer.write_align(in_section.sh_addralign(endian) as usize);
                debug_assert_eq!(out_sections[i].offset, writer.len());
                writer.write(in_section.data(endian, in_data)?);
            }
            _ => {}
        }
    }

    writer.write_null_symbol();
    let section_indices = [0, 1, 2, 3, 4, 5, 5];
    for (sym, sindex) in out_syms.iter().zip(section_indices) {
        let in_sym = in_syms.symbol(sym.in_sym)?;
        writer.write_symbol(&object::write::elf::Sym {
            name: sym.name,
            section: Some(object::write::elf::SectionIndex(sindex)),
            st_info: in_sym.st_info(),
            st_other: in_sym.st_other(),
            st_shndx: in_sym.st_shndx(endian),
            st_value: in_sym.st_value(endian),
            st_size: in_sym.st_size(endian),
        });
    }

    writer.write_symtab_shndx();
    writer.write_strtab();

    writer.write_shstrtab();

    writer.write_null_section_header();

    writer.write_hash_section_header(addresses.hash);
    writer.write_gnu_hash_section_header(addresses.gnu_hash);
    writer.write_dynsym_section_header(addresses.dynsym, 1);

    writer.write_dynstr_section_header(addresses.dynstr);

    {
        let in_section = &sections.eh_frame.section;
        let out_section = &out_sections[5]; // 5 is a random choice
        let sh_link = out_sections_index[in_section.sh_link(endian) as usize].0 as u32;
        let mut sh_info = in_section.sh_info(endian);
        if in_section.sh_flags(endian) as u32 & elf::SHF_INFO_LINK != 0 {
            sh_info = out_sections_index[sh_info as usize].0 as u32;
        }

        writer.write_section_header(&object::write::elf::SectionHeader {
            name: out_section.name,
            sh_type: in_section.sh_type(endian),
            sh_flags: in_section.sh_flags(endian),
            sh_addr: in_section.sh_addr(endian),
            sh_offset: offsets[4] as u64,
            sh_size: in_section.sh_size(endian),
            sh_link,
            sh_info,
            sh_addralign: in_section.sh_addralign(endian),
            sh_entsize: in_section.sh_entsize(endian),
        });
    }

    writer.write_dynamic_section_header(addresses.dynamic);

    writer.write_symtab_section_header(num_local);

    writer.write_strtab_section_header();
    writer.write_shstrtab_section_header();

    debug_assert_eq!(writer.reserved_len(), writer.len());

    Ok(out_data)
}
