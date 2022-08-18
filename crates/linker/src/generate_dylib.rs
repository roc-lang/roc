use std::convert::TryInto;
use std::error::Error;

use object::elf::{self, SectionHeader64};
use object::read::elf::{Dyn, FileHeader, ProgramHeader, Rel, Rela, SectionHeader, Sym};
use object::Endianness;

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
    section: Option<object::write::elf::SectionIndex>,
}

#[derive(Debug)]
struct DynamicSymbol {
    in_sym: usize,
    name: Option<object::write::StringId>,
    section: Option<object::write::elf::SectionIndex>,
    hash: Option<u32>,
    gnu_hash: Option<u32>,
}

struct Sections {
    hash: SectionHeader64<object::Endianness>,
    gnu_hash: SectionHeader64<object::Endianness>,
    dynsym: SectionHeader64<object::Endianness>,
    dynstr: SectionHeader64<object::Endianness>,
    eh_frame: SectionHeader64<object::Endianness>,
    dynamic: SectionHeader64<object::Endianness>,
}

impl Sections {
    fn iter(&self) -> impl Iterator<Item = (usize, &'_ SectionHeader64<object::Endianness>)> + '_ {
        [
            &self.hash,
            &self.gnu_hash,
            &self.dynsym,
            &self.dynstr,
            &self.eh_frame,
            &self.dynamic,
        ]
        .into_iter()
        .enumerate()
    }
}

enum Origin {
    Real,
    Fake,
}

fn copy_file(in_data: &[u8], custom_names: &[String]) -> Result<Vec<u8>, Box<dyn Error>> {
    //    use object::read::Object;
    //    let in_elf = ElfFile64::parse(in_data)?;

    let in_elf: &elf::FileHeader64<Endianness> = elf::FileHeader64::parse(in_data)?;
    let endian = in_elf.endian()?;
    let is_mips64el = in_elf.is_mips64el(endian);
    let in_segments = in_elf.program_headers(endian, in_data)?;
    let in_sections = in_elf.sections(endian, in_data)?;
    let in_syms = in_sections.symbols(endian, in_data, elf::SHT_SYMTAB)?;
    let in_dynsyms = in_sections.symbols(endian, in_data, elf::SHT_DYNSYM)?;

    let help = |name: &[u8]| {
        *in_sections
            .section_by_name(Endianness::Little, name)
            .unwrap()
            .1
    };

    let sections = Sections {
        hash: help(b".hash" as &[_]),
        gnu_hash: help(b".gnu.hash" as &[_]),
        dynsym: help(b".dynsym" as &[_]),
        dynstr: help(b".dynstr" as &[_]),
        eh_frame: help(b".eh_frame" as &[_]),
        dynamic: help(b".dynamic" as &[_]),
    };

    let mut out_data = Vec::new();
    let mut writer = object::write::elf::Writer::new(endian, in_elf.is_class_64(), &mut out_data);

    // Find metadata sections, and assign section indices.
    let mut in_dynamic = None;
    let mut in_hash = None;
    let mut in_gnu_hash = None;
    let mut in_versym = None;
    let mut in_verdef = None;
    let mut in_verneed = None;
    let mut out_sections = Vec::with_capacity(in_sections.len());
    let mut out_sections_index = Vec::with_capacity(in_sections.len());
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
                dbg!("strtab");
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
                dbg!(in_hash);
                debug_assert!(in_hash.is_some());
                index = writer.reserve_hash_section_index();
            }
            elf::SHT_GNU_HASH => {
                assert!(in_gnu_hash.is_none());
                in_gnu_hash = in_section.gnu_hash_header(endian, in_data)?;
                debug_assert!(in_gnu_hash.is_some());
                index = writer.reserve_gnu_hash_section_index();
            }
            elf::SHT_GNU_VERSYM => {
                in_versym = in_section.gnu_versym(endian, in_data)?;
                debug_assert!(in_versym.is_some());
                index = writer.reserve_gnu_versym_section_index();
            }
            elf::SHT_GNU_VERDEF => {
                in_verdef = in_section.gnu_verdef(endian, in_data)?;
                debug_assert!(in_verdef.is_some());
                index = writer.reserve_gnu_verdef_section_index();
            }
            elf::SHT_GNU_VERNEED => {
                in_verneed = in_section.gnu_verneed(endian, in_data)?;
                debug_assert!(in_verneed.is_some());
                index = writer.reserve_gnu_verneed_section_index();
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
    for (i, in_dynsym) in in_dynsyms.iter().enumerate().skip(1) {
        dbg!(i, in_dynsym);
        let section = match in_dynsyms.symbol_section(endian, in_dynsym, i)? {
            Some(in_section) => {
                // Skip symbols for sections we aren't copying.
                if out_sections_index[in_section.0].0 == 0 {
                    dbg!(&in_section);
                    continue;
                }
                Some(out_sections_index[in_section.0])
            }
            None => None,
        };
        let mut name = None;
        let mut hash = None;
        let mut gnu_hash = None;
        if in_dynsym.st_name(endian) != 0 {
            let in_name = in_dynsyms.symbol_name(endian, in_dynsym)?;
            name = Some(writer.add_dynamic_string(in_name));
            if !in_name.is_empty() {
                hash = Some(elf::hash(in_name));
                if !in_dynsym.is_undefined(endian) {
                    gnu_hash = Some(elf::gnu_hash(in_name));
                }
            }
        };
        out_dynsyms.push((
            Origin::Real,
            DynamicSymbol {
                in_sym: i,
                name,
                section,
                hash,
                gnu_hash,
            },
        ));
    }

    for (j, name) in custom_names.iter().enumerate() {
        let i = in_dynsyms.len().saturating_sub(1) + j;

        let in_name = name.as_bytes();
        let name = Some(writer.add_dynamic_string(in_name));

        let hash = Some(elf::hash(in_name));
        let gnu_hash = Some(elf::gnu_hash(in_name));

        // .eh_frame
        let section = Some(out_sections_index[5]);

        out_dynsyms.push((
            Origin::Fake,
            DynamicSymbol {
                in_sym: i,
                name,
                section,
                hash,
                gnu_hash,
            },
        ))
    }

    // We must sort for GNU hash before allocating symbol indices.
    if let Some(in_gnu_hash) = in_gnu_hash.as_ref() {
        //        // TODO: recalculate bucket_count
        out_dynsyms.sort_by_key(|(_, sym)| match sym.gnu_hash {
            None => (0, 0),
            Some(hash) => (1, hash % in_gnu_hash.bucket_count.get(endian)),
        });
    }
    let mut out_dynsyms_index = vec![Default::default(); in_dynsyms.len() + custom_names.len()];
    for (_, out_dynsym) in out_dynsyms.iter_mut() {
        out_dynsyms_index[out_dynsym.in_sym] = writer.reserve_dynamic_symbol_index();
    }

    dbg!(
        in_dynsyms.len() + custom_names.len(),
        in_dynsyms.len(),
        custom_names.len()
    );

    // Hash parameters.
    let hash_index_base = out_dynsyms
        .first()
        .map(|(_, sym)| out_dynsyms_index[sym.in_sym].0)
        .unwrap_or(0);
    let hash_chain_count = writer.dynamic_symbol_count();
    dbg!(hash_index_base, hash_chain_count);

    // GNU hash parameters.
    let gnu_hash_index_base = out_dynsyms
        .iter()
        .position(|(_, sym)| sym.gnu_hash.is_some())
        .unwrap_or(0);
    let gnu_hash_symbol_base = out_dynsyms
        .iter()
        .find(|(_, sym)| sym.gnu_hash.is_some())
        .map(|(_, sym)| out_dynsyms_index[sym.in_sym].0)
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
                if out_sections_index[in_section.0].0 == 0 {
                    out_syms_index.push(Default::default());
                    continue;
                }
                Some(out_sections_index[in_section.0])
            }
            None => None,
        };
        out_syms_index.push(writer.reserve_symbol_index(section));
        let name = if in_sym.st_name(endian) != 0 {
            Some(writer.add_string(in_syms.symbol_name(endian, in_sym)?))
        } else {
            None
        };
        out_syms.push(Symbol {
            in_sym: i,
            name,
            section,
        });
        if in_sym.st_bind() == elf::STB_LOCAL {
            num_local = writer.symbol_count();
        }
    }

    let custom_symbols: Vec<_> = custom_names
        .iter()
        .map(|name| {
            // .eh_frame
            let section = out_sections_index[5];

            out_syms_index.push(writer.reserve_symbol_index(Some(section)));

            let name = writer.add_string(name.as_bytes());

            (name, section)
        })
        .collect();

    // Symbol version parameters.
    let mut verdef_count = 0;
    let mut verdaux_count = 0;
    if let Some((mut verdefs, link)) = in_verdef.clone() {
        let strings = in_sections.strings(endian, in_data, link)?;
        while let Some((verdef, mut verdauxs)) = verdefs.next()? {
            assert!(verdef.vd_cnt.get(endian) > 0);
            verdef_count += 1;
            while let Some(verdaux) = verdauxs.next()? {
                writer.add_dynamic_string(verdaux.name(endian, strings)?);
                verdaux_count += 1;
            }
        }
    }

    let mut verneed_count = 0;
    let mut vernaux_count = 0;
    if let Some((mut verneeds, link)) = in_verneed.clone() {
        let strings = in_sections.strings(endian, in_data, link)?;
        while let Some((verneed, mut vernauxs)) = verneeds.next()? {
            writer.add_dynamic_string(verneed.file(endian, strings)?);
            verneed_count += 1;
            while let Some(vernaux) = vernauxs.next()? {
                writer.add_dynamic_string(vernaux.name(endian, strings)?);
                vernaux_count += 1;
            }
        }
    }

    // Start reserving file ranges.
    writer.reserve_file_header();

    let mut hash_addr = 0;
    let mut gnu_hash_addr = 0;
    let mut versym_addr = 0;
    let mut verdef_addr = 0;
    let mut verneed_addr = 0;
    let mut dynamic_addr = 0;
    let mut dynsym_addr = 0;
    let mut dynstr_addr = 0;

    let mut alloc_sections = Vec::new();
    if in_segments.is_empty() {
        // Reserve sections at any offset.
        for (i, in_section) in in_sections.iter().enumerate() {
            match in_section.sh_type(endian) {
                elf::SHT_PROGBITS | elf::SHT_NOTE | elf::SHT_INIT_ARRAY | elf::SHT_FINI_ARRAY => {
                    out_sections[i].offset = writer.reserve(
                        in_section.sh_size(endian) as usize,
                        in_section.sh_addralign(endian) as usize,
                    );
                }
                _ => {}
            }
        }
    } else {
        // We don't support moving program headers.
        assert_eq!(in_elf.e_phoff(endian), writer.reserved_len() as u64);
        writer.reserve_program_headers(in_segments.len() as u32);

        // Reserve alloc sections at original offsets.
        alloc_sections = in_sections
            .iter()
            .enumerate()
            .filter(|(_, s)| s.sh_flags(endian) & u64::from(elf::SHF_ALLOC) != 0)
            .collect();
        // The data for alloc sections may need to be written in a different order
        // from their section headers.
        alloc_sections.sort_by_key(|(_, x)| x.sh_offset(endian));
        dbg!(alloc_sections.len());
        for (i, in_section) in alloc_sections.iter() {
            dbg!(i, in_section);
            writer.reserve_until(in_section.sh_offset(endian) as usize);
            match dbg!(in_section.sh_type(endian)) {
                elf::SHT_PROGBITS | elf::SHT_NOTE | elf::SHT_INIT_ARRAY | elf::SHT_FINI_ARRAY => {
                    out_sections[*i].offset =
                        writer.reserve(in_section.sh_size(endian) as usize, 1);
                }
                elf::SHT_NOBITS => {
                    out_sections[*i].offset = writer.reserved_len();
                }
                elf::SHT_REL => {
                    let (rels, _link) = in_section.rel(endian, in_data)?.unwrap();
                    out_sections[*i].offset = writer.reserve_relocations(rels.len(), false);
                }
                elf::SHT_RELA => {
                    let (rels, _link) = in_section.rela(endian, in_data)?.unwrap();
                    out_sections[*i].offset = writer.reserve_relocations(rels.len(), true);
                }
                elf::SHT_DYNAMIC => {
                    dynamic_addr = in_section.sh_addr(endian).into();
                    writer.reserve_dynamic(out_dynamic.len());
                }
                elf::SHT_DYNSYM if *i == in_dynsyms.section().0 => {
                    dynsym_addr = in_section.sh_addr(endian).into();
                    writer.reserve_dynsym();
                }
                elf::SHT_STRTAB if *i == in_dynsyms.string_section().0 => {
                    dbg!("strtab");
                    dynstr_addr = in_section.sh_addr(endian).into();
                    writer.reserve_dynstr();
                }
                elf::SHT_HASH => {
                    hash_addr = in_section.sh_addr(endian).into();
                    dbg!(writer.reserved_len());
                    dbg!(hash_addr);
                    let hash = in_hash.as_ref().unwrap();
                    // writer.reserve_hash(hash.bucket_count.get(endian), hash_chain_count);
                    writer.reserve_hash(3, 13);
                }
                elf::SHT_GNU_HASH => {
                    gnu_hash_addr = in_section.sh_addr(endian).into();
                    dbg!(writer.reserved_len());
                    dbg!(gnu_hash_addr);
                    let hash = in_gnu_hash.as_ref().unwrap();
                    writer.reserve_gnu_hash(
                        hash.bloom_count.get(endian),
                        hash.bucket_count.get(endian),
                        gnu_hash_symbol_count,
                    );
                }
                elf::SHT_GNU_VERSYM => {
                    versym_addr = in_section.sh_addr(endian).into();
                    writer.reserve_gnu_versym();
                }
                elf::SHT_GNU_VERDEF => {
                    verdef_addr = in_section.sh_addr(endian).into();
                    writer.reserve_gnu_verdef(verdef_count, verdaux_count);
                }
                elf::SHT_GNU_VERNEED => {
                    verneed_addr = in_section.sh_addr(endian).into();
                    writer.reserve_gnu_verneed(verneed_count, vernaux_count);
                }
                other => {
                    panic!("Unsupported alloc section index {}, type {}", *i, other);
                }
            }
        }

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
    }

    writer.reserve_symtab();
    writer.reserve_symtab_shndx();
    writer.reserve_strtab();

    for (i, in_section) in in_sections.iter().enumerate() {
        if !in_segments.is_empty() && in_section.sh_flags(endian) & u64::from(elf::SHF_ALLOC) != 0 {
            continue;
        }
        match in_section.sh_type(endian) {
            elf::SHT_REL => {
                let (rels, _link) = in_section.rel(endian, in_data)?.unwrap();
                out_sections[i].offset = writer.reserve_relocations(rels.len(), false);
            }
            elf::SHT_RELA => {
                let (rels, _link) = in_section.rela(endian, in_data)?.unwrap();
                out_sections[i].offset = writer.reserve_relocations(rels.len(), true);
            }
            _ => {}
        }
    }

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

    if in_segments.is_empty() {
        for (i, in_section) in in_sections.iter().enumerate() {
            match in_section.sh_type(endian) {
                elf::SHT_PROGBITS | elf::SHT_NOTE | elf::SHT_INIT_ARRAY | elf::SHT_FINI_ARRAY => {
                    writer.write_align(in_section.sh_addralign(endian) as usize);
                    debug_assert_eq!(out_sections[i].offset, writer.len());
                    writer.write(in_section.data(endian, in_data)?);
                }
                _ => {}
            }
        }
    } else {
        writer.write_align_program_headers();
        for in_segment in in_segments {
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
            writer.pad_until(in_section.sh_offset(endian) as usize);
            match in_section.sh_type(endian) {
                elf::SHT_PROGBITS | elf::SHT_NOTE | elf::SHT_INIT_ARRAY | elf::SHT_FINI_ARRAY => {
                    debug_assert_eq!(out_sections[*i].offset, writer.len());
                    writer.write(in_section.data(endian, in_data)?);
                }
                elf::SHT_NOBITS => {}
                elf::SHT_REL => {
                    let (rels, _link) = in_section.rel(endian, in_data)?.unwrap();
                    writer.write_align_relocation();
                    for rel in rels {
                        let in_sym = rel.r_sym(endian);
                        let out_sym = if in_sym != 0 {
                            out_dynsyms_index[in_sym as usize].0
                        } else {
                            0
                        };
                        writer.write_relocation(
                            false,
                            &object::write::elf::Rel {
                                r_offset: rel.r_offset(endian).into(),
                                r_sym: out_sym,
                                r_type: rel.r_type(endian),
                                r_addend: 0,
                            },
                        );
                    }
                }
                elf::SHT_RELA => {
                    let (rels, _link) = in_section.rela(endian, in_data)?.unwrap();
                    writer.write_align_relocation();
                    for rel in rels {
                        let in_sym = rel.r_sym(endian, is_mips64el);
                        let out_sym = if in_sym != 0 {
                            out_dynsyms_index[in_sym as usize].0
                        } else {
                            0
                        };
                        writer.write_relocation(
                            true,
                            &object::write::elf::Rel {
                                r_offset: rel.r_offset(endian).into(),
                                r_sym: out_sym,
                                r_type: rel.r_type(endian, is_mips64el),
                                r_addend: rel.r_addend(endian).into(),
                            },
                        );
                    }
                }
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
                    dbg!("here");
                    for (_, sym) in &out_dynsyms {
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
                    writer.write_dynstr();
                }
                elf::SHT_HASH => {
                    dbg!("here at the hash");
                    let hash = in_hash.as_ref().unwrap();
                    writer.write_hash(hash.bucket_count.get(endian), hash_chain_count, |index| {
                        out_dynsyms
                            .get(index.checked_sub(hash_index_base)? as usize)?
                            .1
                            .hash
                    });
                }
                elf::SHT_GNU_HASH => {
                    dbg!("here");
                    let gnu_hash = in_gnu_hash.as_ref().unwrap();
                    writer.write_gnu_hash(
                        gnu_hash_symbol_base,
                        gnu_hash.bloom_shift.get(endian),
                        gnu_hash.bloom_count.get(endian),
                        gnu_hash.bucket_count.get(endian),
                        gnu_hash_symbol_count,
                        |index| {
                            out_dynsyms[gnu_hash_index_base + index as usize]
                                .1
                                .gnu_hash
                                .unwrap()
                        },
                    );
                }
                elf::SHT_GNU_VERSYM => {
                    dbg!("here");
                    let (in_versym, _) = in_versym.as_ref().unwrap();
                    writer.write_null_gnu_versym();
                    for (_, out_dynsym) in &out_dynsyms {
                        writer.write_gnu_versym(
                            in_versym.get(out_dynsym.in_sym).unwrap().0.get(endian),
                        );
                    }
                }
                elf::SHT_GNU_VERDEF => {
                    let (mut verdefs, link) = in_verdef.clone().unwrap();
                    let strings = in_sections.strings(endian, in_data, link)?;
                    writer.write_align_gnu_verdef();
                    while let Some((verdef, mut verdauxs)) = verdefs.next()? {
                        let verdaux = verdauxs.next()?.unwrap();
                        writer.write_gnu_verdef(&object::write::elf::Verdef {
                            version: verdef.vd_version.get(endian),
                            flags: verdef.vd_flags.get(endian),
                            index: verdef.vd_ndx.get(endian),
                            aux_count: verdef.vd_cnt.get(endian),
                            name: writer.get_dynamic_string(verdaux.name(endian, strings)?),
                        });
                        while let Some(verdaux) = verdauxs.next()? {
                            writer.write_gnu_verdaux(
                                writer.get_dynamic_string(verdaux.name(endian, strings)?),
                            );
                        }
                    }
                }
                elf::SHT_GNU_VERNEED => {
                    let (mut verneeds, link) = in_verneed.clone().unwrap();
                    let strings = in_sections.strings(endian, in_data, link)?;
                    writer.write_align_gnu_verneed();
                    while let Some((verneed, mut vernauxs)) = verneeds.next()? {
                        writer.write_gnu_verneed(&object::write::elf::Verneed {
                            version: verneed.vn_version.get(endian),
                            aux_count: verneed.vn_cnt.get(endian),
                            file: writer.get_dynamic_string(verneed.file(endian, strings)?),
                        });
                        while let Some(vernaux) = vernauxs.next()? {
                            writer.write_gnu_vernaux(&object::write::elf::Vernaux {
                                flags: vernaux.vna_flags.get(endian),
                                index: vernaux.vna_other.get(endian),
                                name: writer.get_dynamic_string(vernaux.name(endian, strings)?),
                            });
                        }
                    }
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
    }

    writer.write_null_symbol();
    for sym in &out_syms {
        let in_sym = in_syms.symbol(sym.in_sym)?;
        writer.write_symbol(&object::write::elf::Sym {
            name: sym.name,
            section: sym.section,
            st_info: in_sym.st_info(),
            st_other: in_sym.st_other(),
            st_shndx: in_sym.st_shndx(endian),
            st_value: in_sym.st_value(endian).into(),
            st_size: in_sym.st_size(endian).into(),
        });
    }

    // DEVIATION
    for (name, section) in custom_symbols {
        writer.write_symbol(&object::write::elf::Sym {
            name: Some(name),
            section: Some(section),
            st_info: (1 << 4) /* weak */ | 2, /* function */
            st_other: 0,
            st_shndx: 0,
            st_value: 0x1000,
            st_size: 0,
        });
    }

    writer.write_symtab_shndx();
    writer.write_strtab();

    for in_section in in_sections.iter() {
        if !in_segments.is_empty() && in_section.sh_flags(endian) & u64::from(elf::SHF_ALLOC) != 0 {
            continue;
        }
        let out_syms = if in_section.sh_link(endian) as usize == in_syms.section().0 {
            &out_syms_index
        } else {
            &out_dynsyms_index
        };
        match in_section.sh_type(endian) {
            elf::SHT_REL => {
                let (rels, _link) = in_section.rel(endian, in_data)?.unwrap();
                writer.write_align_relocation();
                for rel in rels {
                    let in_sym = rel.r_sym(endian);
                    let out_sym = if in_sym != 0 {
                        out_syms[in_sym as usize].0
                    } else {
                        0
                    };
                    writer.write_relocation(
                        false,
                        &object::write::elf::Rel {
                            r_offset: rel.r_offset(endian),
                            r_sym: out_sym,
                            r_type: rel.r_type(endian),
                            r_addend: 0,
                        },
                    );
                }
            }
            elf::SHT_RELA => {
                let (rels, _link) = in_section.rela(endian, in_data)?.unwrap();
                writer.write_align_relocation();
                for rel in rels {
                    let in_sym = rel.r_sym(endian, is_mips64el);
                    let out_sym = if in_sym != 0 {
                        out_syms[in_sym as usize].0
                    } else {
                        0
                    };
                    writer.write_relocation(
                        true,
                        &object::write::elf::Rel {
                            r_offset: rel.r_offset(endian),
                            r_sym: out_sym,
                            r_type: rel.r_type(endian, is_mips64el),
                            r_addend: rel.r_addend(endian),
                        },
                    );
                }
            }
            _ => {}
        }
    }

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
                let out_section = &out_sections[i];
                let sh_link = out_sections_index[in_section.sh_link(endian) as usize].0 as u32;
                let mut sh_info = in_section.sh_info(endian);
                if in_section.sh_flags(endian) as u32 & elf::SHF_INFO_LINK != 0 {
                    sh_info = out_sections_index[sh_info as usize].0 as u32;
                }

                dbg!(i, &in_section, &sh_info);
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
            }
            elf::SHT_STRTAB => {
                if i == in_syms.string_section().0 {
                    writer.write_strtab_section_header();
                } else if i == in_dynsyms.string_section().0 {
                    writer.write_dynstr_section_header(dynstr_addr);
                } else if i == in_elf.shstrndx(endian, in_data)? as usize {
                    writer.write_shstrtab_section_header();
                } else {
                    panic!("Unsupported string section {}", i);
                }
            }
            elf::SHT_SYMTAB => {
                if i == in_syms.section().0 {
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
                dbg!("this hash");
                writer.write_hash_section_header(hash_addr);
            }
            elf::SHT_GNU_HASH => {
                writer.write_gnu_hash_section_header(gnu_hash_addr);
            }
            elf::SHT_GNU_VERSYM => {
                writer.write_gnu_versym_section_header(versym_addr);
            }
            elf::SHT_GNU_VERDEF => {
                writer.write_gnu_verdef_section_header(verdef_addr);
            }
            elf::SHT_GNU_VERNEED => {
                writer.write_gnu_verneed_section_header(verneed_addr);
            }
            other => {
                panic!("Unsupported section type {:x}", other);
            }
        }
    }
    debug_assert_eq!(writer.reserved_len(), writer.len());

    Ok(out_data)
}
