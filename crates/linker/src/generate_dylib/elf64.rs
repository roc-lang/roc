use object::elf::{self, SectionHeader64};
use object::read::elf::{FileHeader, ProgramHeader, SectionHeader, Sym};
use object::Endianness;

use object::write::elf::Writer;

// index of the dynamic section
const DYMAMIC_SECTION: usize = 4;

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
    gnu_hash: Option<u32>,
}

type Section = SectionHeader64<object::Endianness>;

struct Sections {
    hash: Section,
    gnu_hash: Section,
    dynsym: Section,
    dynstr: Section,
    dynamic: Section,
}

#[derive(Default)]
struct Addresses {
    hash: u64,
    gnu_hash: u64,
    dynsym: u64,
    dynstr: u64,
    dynamic: u64,
}

impl Sections {
    fn reserve_alloc_sections(
        &self,
        writer: &mut Writer,
        hash_chain_count: u32,
        gnu_hash_symbol_count: u32,
        dynamic_count: usize,
    ) -> ([usize; 5], Addresses) {
        let sections = self;
        let endian = Endianness::Little;

        let mut offsets = [0; 5];

        let mut extra_offset: usize = 0;
        let mut addresses = Addresses::default();

        for (i, out_offset) in offsets.iter_mut().enumerate() {
            // symtab, strtab and shstrtab is ignored because they are not ALLOC
            let in_section = match i {
                0 => sections.hash,
                1 => sections.gnu_hash,
                2 => sections.dynsym,
                3 => sections.dynstr,
                4 => sections.dynamic,
                _ => unreachable!(),
            };

            let offset = in_section.sh_offset(endian) as usize + extra_offset;
            let offset = round_up_to_alignment(offset, 8);
            *out_offset = offset;
            writer.reserve_until(offset);

            let reserved_before = writer.reserved_len();

            match i {
                0 => {
                    writer.reserve_hash(3, hash_chain_count);
                }
                1 => {
                    addresses.gnu_hash = in_section.sh_addr(endian);
                    writer.reserve_gnu_hash(1, 1, gnu_hash_symbol_count);
                }
                2 => {
                    addresses.dynsym = in_section.sh_addr(endian);
                    writer.reserve_dynsym();
                }
                3 => {
                    addresses.dynstr = in_section.sh_addr(endian);
                    writer.reserve_dynstr();
                }
                4 => {
                    addresses.dynamic = in_section.sh_addr(endian);
                    writer.reserve_dynamic(dynamic_count);
                }
                _ => unreachable!(),
            }

            let reserved_after = writer.reserved_len();

            extra_offset += {
                let actual_size = reserved_after - reserved_before;

                actual_size.saturating_sub(in_section.sh_size(endian) as usize)
            };
        }

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

pub fn create_dylib_elf64(
    in_data: &[u8],
    custom_names: &[String],
) -> object::read::Result<Vec<u8>> {
    let in_elf: &elf::FileHeader64<Endianness> = elf::FileHeader64::parse(in_data)?;
    let endian = in_elf.endian()?;
    let in_segments = in_elf.program_headers(endian, in_data)?;
    let in_sections = in_elf.sections(endian, in_data)?;
    let in_syms = in_sections.symbols(endian, in_data, elf::SHT_SYMTAB)?;
    let in_dynsyms = in_sections.symbols(endian, in_data, elf::SHT_DYNSYM)?;

    let get_section = |name: &[u8]| {
        *in_sections
            .section_by_name(Endianness::Little, name)
            .unwrap()
            .1
    };

    let sections = Sections {
        hash: get_section(b".hash" as &[_]),
        gnu_hash: get_section(b".gnu.hash" as &[_]),
        dynsym: get_section(b".dynsym" as &[_]),
        dynstr: get_section(b".dynstr" as &[_]),
        dynamic: get_section(b".dynamic" as &[_]),
    };

    let mut out_data = Vec::new();
    let mut writer = object::write::elf::Writer::new(endian, in_elf.is_class_64(), &mut out_data);

    let out_sections_index = [
        writer.reserve_hash_section_index(),
        writer.reserve_gnu_hash_section_index(),
        writer.reserve_dynsym_section_index(),
        writer.reserve_dynstr_section_index(),
        writer.reserve_dynamic_section_index(),
        writer.reserve_symtab_section_index(),
        writer.reserve_strtab_section_index(),
        writer.reserve_shstrtab_section_index(),
    ];

    // Assign dynamic strings.
    //
    // Dynamic section at offset 0x2268 contains 8 entries:
    //   Tag        Type                         Name/Value
    //  0x000000000000000e (SONAME)             Library soname: [libapp.so]
    //  0x0000000000000004 (HASH)               0x120
    //  0x000000006ffffef5 (GNU_HASH)           0x130
    //  0x0000000000000005 (STRTAB)             0x168
    //  0x0000000000000006 (SYMTAB)             0x150
    //  0x000000000000000a (STRSZ)              11 (bytes)
    //  0x000000000000000b (SYMENT)             24 (bytes)
    //  0x0000000000000000 (NULL)               0x0
    let dynamic = |tag, val, string| Dynamic { tag, val, string };
    let soname = writer.add_dynamic_string(b"libapp.so");
    let out_dynamic = [
        dynamic(elf::DT_SONAME, 1, Some(soname)),
        dynamic(elf::DT_HASH, 288, None),
        dynamic(elf::DT_GNU_HASH, 304, None),
        dynamic(elf::DT_STRTAB, 360, None),
        dynamic(elf::DT_SYMTAB, 336, None),
        dynamic(elf::DT_STRSZ, 11, None),
        dynamic(elf::DT_SYMENT, 24, None),
        dynamic(elf::DT_NULL, 0, None),
    ];

    // Assign dynamic symbol indices.
    let mut out_dynsyms = Vec::with_capacity(in_dynsyms.len());

    for (j, name) in custom_names.iter().enumerate() {
        let i = in_dynsyms.len().saturating_sub(1) + j;

        let in_name = name.as_bytes();
        let name = Some(writer.add_dynamic_string(in_name));

        let gnu_hash = Some(elf::gnu_hash(in_name));

        // .dynamic
        let section = Some(out_sections_index[DYMAMIC_SECTION]);

        out_dynsyms.push(DynamicSymbol {
            in_sym: i,
            name,
            section,
            gnu_hash,
        })
    }

    let mut out_dynsyms_index = vec![Default::default(); in_dynsyms.len() + custom_names.len()];
    for out_dynsym in out_dynsyms.iter_mut() {
        out_dynsyms_index[out_dynsym.in_sym] = writer.reserve_dynamic_symbol_index();
    }

    // Hash parameters.
    let hash_chain_count = writer.dynamic_symbol_count();

    // GNU hash parameters.
    let gnu_hash_index_base = 0;
    let gnu_hash_symbol_base = 1;
    let gnu_hash_symbol_count = writer.dynamic_symbol_count() - gnu_hash_symbol_base;

    // content of the symbol table
    //
    // Symbol table '.symtab' contains 8 entries:
    //    Num:    Value          Size Type    Bind   Vis      Ndx Name
    //      0: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT  UND
    //      1: 0000000000000120     0 SECTION LOCAL  DEFAULT  UND
    //      2: 0000000000000130     0 SECTION LOCAL  DEFAULT    1
    //      3: 0000000000000150     0 SECTION LOCAL  DEFAULT    2
    //      4: 0000000000000168     0 SECTION LOCAL  DEFAULT    3
    //      5: 0000000000001000     0 SECTION LOCAL  DEFAULT    4
    //      6: 0000000000001f30     0 SECTION LOCAL  DEFAULT    5
    //      7: 0000000000001f30     0 OBJECT  LOCAL  DEFAULT    5 _DYNAMIC
    let symbol = |index, name| Symbol {
        in_sym: index,
        name,
    };

    let out_syms = [
        symbol(1, None),
        symbol(2, None),
        symbol(3, None),
        symbol(4, None),
        symbol(5, None),
        symbol(6, None),
        symbol(7, Some(writer.add_string(b"_DYNAMIC"))),
    ];

    // include the NULL symbol
    let num_local = 1 + out_syms.len();

    let mut out_syms_index = Vec::with_capacity(in_syms.len());
    out_syms_index.push(Default::default());
    for sym in out_syms.iter() {
        out_syms_index.push(writer.reserve_symbol_index(Some(out_sections_index[sym.in_sym])));
    }

    // Start reserving file ranges.
    writer.reserve_file_header();

    // We don't support moving program headers.
    assert_eq!(in_elf.e_phoff(endian), writer.reserved_len() as u64);
    writer.reserve_program_headers(in_segments.len() as u32);

    let (offsets, addresses) = sections.reserve_alloc_sections(
        &mut writer,
        hash_chain_count,
        gnu_hash_symbol_count,
        out_dynamic.len(),
    );

    writer.reserve_symtab();
    writer.reserve_symtab_shndx();
    writer.reserve_strtab();

    writer.reserve_shstrtab();
    writer.reserve_section_headers();

    writer
        .write_file_header(&object::write::elf::FileHeader {
            os_abi: in_elf.e_ident().os_abi,
            abi_version: in_elf.e_ident().abi_version,
            e_type: in_elf.e_type(endian),
            e_machine: in_elf.e_machine(endian),
            e_entry: in_elf.e_entry(endian),
            e_flags: in_elf.e_flags(endian),
        })
        .unwrap();

    writer.write_align_program_headers();
    for in_segment in in_segments {
        writer.write_program_header(&object::write::elf::ProgramHeader {
            p_type: in_segment.p_type(endian),
            p_flags: in_segment.p_flags(endian),
            // dirty hack really. Not sure if this is correct on its own
            p_offset: if in_segment.p_offset(endian) > 0 {
                offsets[DYMAMIC_SECTION] as _
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

    for (i, offset) in offsets.iter().enumerate() {
        writer.pad_until(*offset);

        match i {
            0 => {
                writer.write_hash(1, hash_chain_count, |_| None);
            }
            1 => {
                writer.write_gnu_hash(1, 0, 1, 1, gnu_hash_symbol_count, |index| {
                    out_dynsyms[gnu_hash_index_base + index as usize]
                        .gnu_hash
                        .unwrap()
                });
            }
            2 => {
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
            3 => {
                writer.write_dynstr();
            }
            4 => {
                for d in &out_dynamic {
                    if let Some(string) = d.string {
                        writer.write_dynamic_string(d.tag, string);
                    } else {
                        writer.write_dynamic(d.tag, d.val);
                    }
                }
            }
            _ => unreachable!(),
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

    writer.write_dynamic_section_header(addresses.dynamic);

    writer.write_symtab_section_header(num_local as _);

    writer.write_strtab_section_header();
    writer.write_shstrtab_section_header();

    debug_assert_eq!(writer.reserved_len(), writer.len());

    Ok(out_data)
}
