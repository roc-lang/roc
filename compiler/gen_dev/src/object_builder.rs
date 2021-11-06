use crate::generic64::{aarch64, x86_64, Backend64Bit};
use crate::{Backend, Env, Relocation};
use bumpalo::collections::Vec;
use object::write;
use object::write::{Object, StandardSection, StandardSegment, Symbol, SymbolSection};
use object::{
    Architecture, BinaryFormat, Endianness, RelocationEncoding, RelocationKind, SectionKind,
    SymbolFlags, SymbolKind, SymbolScope,
};
use roc_collections::all::MutMap;
use roc_module::symbol;
use roc_mono::ir::{Proc, ProcLayout};
use target_lexicon::{Architecture as TargetArch, BinaryFormat as TargetBF, Triple};

// This is used by some code below which is currently commented out.
// See that code for more details!
// const VERSION: &str = env!("CARGO_PKG_VERSION");

/// build_module is the high level builder/delegator.
/// It takes the request to build a module and output the object file for the module.
pub fn build_module<'a>(
    env: &'a Env,
    target: &Triple,
    procedures: MutMap<(symbol::Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<Object, String> {
    match target {
        Triple {
            architecture: TargetArch::X86_64,
            binary_format: TargetBF::Elf,
            ..
        } if cfg!(feature = "target-x86_64") => {
            let backend: Backend64Bit<
                x86_64::X86_64GeneralReg,
                x86_64::X86_64FloatReg,
                x86_64::X86_64Assembler,
                x86_64::X86_64SystemV,
            > = Backend::new(env)?;
            build_object(
                env,
                procedures,
                backend,
                Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little),
            )
        }
        Triple {
            architecture: TargetArch::X86_64,
            binary_format: TargetBF::Macho,
            ..
        } if cfg!(feature = "target-x86_64") => {
            let backend: Backend64Bit<
                x86_64::X86_64GeneralReg,
                x86_64::X86_64FloatReg,
                x86_64::X86_64Assembler,
                x86_64::X86_64SystemV,
            > = Backend::new(env)?;
            build_object(
                env,
                procedures,
                backend,
                Object::new(
                    BinaryFormat::MachO,
                    Architecture::X86_64,
                    Endianness::Little,
                ),
            )
        }
        Triple {
            architecture: TargetArch::Aarch64(_),
            binary_format: TargetBF::Elf,
            ..
        } if cfg!(feature = "target-aarch64") => {
            let backend: Backend64Bit<
                aarch64::AArch64GeneralReg,
                aarch64::AArch64FloatReg,
                aarch64::AArch64Assembler,
                aarch64::AArch64Call,
            > = Backend::new(env)?;
            build_object(
                env,
                procedures,
                backend,
                Object::new(BinaryFormat::Elf, Architecture::Aarch64, Endianness::Little),
            )
        }
        x => Err(format! {
        "the target, {:?}, is not yet implemented",
        x}),
    }
}

fn generate_wrapper<'a, B: Backend<'a>>(
    backend: &mut B,
    output: &mut Object,
    wrapper_name: String,
    wraps: String,
) -> Result<(), String> {
    let text_section = output.section_id(StandardSection::Text);
    let proc_symbol = Symbol {
        name: wrapper_name.as_bytes().to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        scope: SymbolScope::Dynamic,
        weak: false,
        section: SymbolSection::Section(text_section),
        flags: SymbolFlags::None,
    };
    let proc_id = output.add_symbol(proc_symbol);
    let (proc_data, offset) = backend.build_wrapped_jmp()?;
    let proc_offset = output.add_symbol_data(proc_id, text_section, proc_data, 16);

    let name = wraps.as_str().as_bytes();
    // If the symbol is an undefined zig builtin, we need to add it here.
    let symbol = Symbol {
        name: name.to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        scope: SymbolScope::Dynamic,
        weak: true,
        section: SymbolSection::Undefined,
        flags: SymbolFlags::None,
    };
    output.add_symbol(symbol);
    if let Some(sym_id) = output.symbol_id(name) {
        let reloc = write::Relocation {
            offset: offset + proc_offset,
            size: 32,
            kind: RelocationKind::PltRelative,
            encoding: RelocationEncoding::X86Branch,
            symbol: sym_id,
            addend: -4,
        };

        output
            .add_relocation(text_section, reloc)
            .map_err(|e| format!("{:?}", e))?;

        Ok(())
    } else {
        Err(format!("failed to find fn symbol for {:?}", wraps))
    }
}

fn build_object<'a, B: Backend<'a>>(
    env: &'a Env,
    procedures: MutMap<(symbol::Symbol, ProcLayout<'a>), Proc<'a>>,
    mut backend: B,
    mut output: Object,
) -> Result<Object, String> {
    let data_section = output.section_id(StandardSection::Data);

    /*
    // Commented out because we couldn't figure out how to get it to work on mac - see https://github.com/rtfeldman/roc/pull/1323
    let comment = output.add_section(vec![], b".comment".to_vec(), SectionKind::OtherString);
    output.append_section_data(
        comment,
        format!("\0roc dev backend version {} \0", VERSION).as_bytes(),
        1,
    );
    */

    if env.generate_allocators {
        generate_wrapper(
            &mut backend,
            &mut output,
            "roc_alloc".into(),
            "malloc".into(),
        )?;
        generate_wrapper(
            &mut backend,
            &mut output,
            "roc_realloc".into(),
            "realloc".into(),
        )?;
        generate_wrapper(
            &mut backend,
            &mut output,
            "roc_dealloc".into(),
            "free".into(),
        )?;
        generate_wrapper(
            &mut backend,
            &mut output,
            "roc_panic".into(),
            "roc_builtins.utils.test_panic".into(),
        )?;
    }

    // Setup layout_ids for procedure calls.
    let mut layout_ids = roc_mono::layout::LayoutIds::default();
    let mut procs = Vec::with_capacity_in(procedures.len(), env.arena);
    for ((sym, layout), proc) in procedures {
        let base_name = layout_ids
            .get_toplevel(sym, &layout)
            .to_symbol_string(sym, &env.interns);

        let fn_name = if env.exposed_to_host.contains(&sym) {
            format!("roc_{}_exposed", base_name)
        } else {
            base_name
        };

        let section_id = output.add_section(
            output.segment_name(StandardSegment::Text).to_vec(),
            format!(".text.{:x}", sym.as_u64()).as_bytes().to_vec(),
            SectionKind::Text,
        );

        let proc_symbol = Symbol {
            name: fn_name.as_bytes().to_vec(),
            value: 0,
            size: 0,
            kind: SymbolKind::Text,
            // TODO: Depending on whether we are building a static or dynamic lib, this should change.
            // We should use Dynamic -> anyone, Linkage -> static link, Compilation -> this module only.
            scope: if env.exposed_to_host.contains(&sym) {
                SymbolScope::Dynamic
            } else {
                SymbolScope::Linkage
            },
            weak: false,
            section: SymbolSection::Section(section_id),
            flags: SymbolFlags::None,
        };
        let proc_id = output.add_symbol(proc_symbol);
        procs.push((fn_name, section_id, proc_id, proc));
    }

    // Build procedures.
    let mut relocations = bumpalo::vec![in env.arena];
    for (fn_name, section_id, proc_id, proc) in procs {
        let mut local_data_index = 0;
        let (proc_data, relocs) = backend.build_proc(proc)?;
        let proc_offset = output.add_symbol_data(proc_id, section_id, proc_data, 16);
        for reloc in relocs {
            let elfreloc = match reloc {
                Relocation::LocalData { offset, data } => {
                    let data_symbol = write::Symbol {
                        name: format!("{}.data{}", fn_name, local_data_index)
                            .as_bytes()
                            .to_vec(),
                        value: 0,
                        size: 0,
                        kind: SymbolKind::Data,
                        scope: SymbolScope::Compilation,
                        weak: false,
                        section: SymbolSection::Section(data_section),
                        flags: SymbolFlags::None,
                    };
                    local_data_index += 1;
                    let data_id = output.add_symbol(data_symbol);
                    output.add_symbol_data(data_id, data_section, data, 4);
                    write::Relocation {
                        offset: offset + proc_offset,
                        size: 32,
                        kind: RelocationKind::Relative,
                        encoding: RelocationEncoding::Generic,
                        symbol: data_id,
                        addend: -4,
                    }
                }
                Relocation::LinkedData { offset, name } => {
                    if let Some(sym_id) = output.symbol_id(name.as_bytes()) {
                        write::Relocation {
                            offset: offset + proc_offset,
                            size: 32,
                            kind: RelocationKind::GotRelative,
                            encoding: RelocationEncoding::Generic,
                            symbol: sym_id,
                            addend: -4,
                        }
                    } else {
                        return Err(format!("failed to find data symbol for {:?}", name));
                    }
                }
                Relocation::LinkedFunction { offset, name } => {
                    // If the symbol is an undefined zig builtin, we need to add it here.
                    if output.symbol_id(name.as_bytes()) == None
                        && name.starts_with("roc_builtins.")
                    {
                        let builtin_symbol = Symbol {
                            name: name.as_bytes().to_vec(),
                            value: 0,
                            size: 0,
                            kind: SymbolKind::Text,
                            scope: SymbolScope::Linkage,
                            weak: false,
                            section: SymbolSection::Undefined,
                            flags: SymbolFlags::None,
                        };
                        output.add_symbol(builtin_symbol);
                    }
                    if let Some(sym_id) = output.symbol_id(name.as_bytes()) {
                        write::Relocation {
                            offset: offset + proc_offset,
                            size: 32,
                            kind: RelocationKind::PltRelative,
                            encoding: RelocationEncoding::X86Branch,
                            symbol: sym_id,
                            addend: -4,
                        }
                    } else {
                        return Err(format!("failed to find fn symbol for {:?}", name));
                    }
                }
                Relocation::JmpToReturn { .. } => unreachable!(),
            };
            relocations.push((section_id, elfreloc));
        }
    }
    for (section_id, reloc) in relocations {
        output
            .add_relocation(section_id, reloc)
            .map_err(|e| format!("{:?}", e))?;
    }
    Ok(output)
}
