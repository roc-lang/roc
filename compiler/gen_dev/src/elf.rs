use crate::x86_64::X86_64Backend;
use crate::{Backend, Env};
use bumpalo::collections::Vec;
use object::write::{Object, StandardSection, Symbol, SymbolSection};
use object::{
    Architecture, BinaryFormat, Endianness, SectionKind, SymbolFlags, SymbolKind, SymbolScope,
};
use roc_collections::all::MutMap;
use roc_module::symbol;
use roc_mono::ir::Proc;
use roc_mono::layout::Layout;
use target_lexicon::Triple;

const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn build_module<'a>(
    env: &'a Env,
    target: &Triple,
    procedures: MutMap<(symbol::Symbol, Layout<'a>), Proc<'a>>,
) -> Result<Object, String> {
    match target.architecture {
        target_lexicon::Architecture::X86_64 => {
            let mut output =
                Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
            let text = output.section_id(StandardSection::Text);
            let comment = output.add_section(vec![], b"comment".to_vec(), SectionKind::OtherString);
            output.append_section_data(
                comment,
                format!("\0roc dev backend version {} \0", VERSION).as_bytes(),
                1,
            );

            // Setup layout_ids for procedure calls.
            let mut layout_ids = roc_mono::layout::LayoutIds::default();
            let mut procs = Vec::with_capacity_in(procedures.len(), env.arena);
            for ((symbol, layout), proc) in procedures {
                let fn_name = layout_ids
                    .get(symbol, &layout)
                    .to_symbol_string(symbol, &env.interns);

                let proc_symbol = Symbol {
                    name: fn_name.as_bytes().to_vec(),
                    value: 0,
                    size: 0,
                    kind: SymbolKind::Text,
                    // TODO: Depending on whether we are building a static or dynamic lib, this should change.
                    // We should use Dynamic -> anyone, Linkage -> static link, Compilation -> this module only.
                    scope: if env.exposed_to_host.contains(&symbol) {
                        SymbolScope::Dynamic
                    } else {
                        SymbolScope::Linkage
                    },
                    weak: false,
                    section: SymbolSection::Section(text),
                    flags: SymbolFlags::None,
                };
                let proc_id = output.add_symbol(proc_symbol);
                procs.push((proc_id, proc));
            }

            // Build procedures.
            let mut backend: X86_64Backend = Backend::new(env, target)?;
            for (proc_id, proc) in procs {
                let (proc_data, _relocations) = backend.build_proc(proc)?;
                // TODO: handle relocations.
                output.add_symbol_data(proc_id, text, proc_data, 16);
            }
            Ok(output)
        }
        x => Err(format! {
        "the architecture, {:?}, is not yet implemented for elf",
        x}),
    }
}
