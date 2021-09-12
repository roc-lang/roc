mod backend;
pub mod from_wasm32_memory;

use bumpalo::Bump;
use parity_wasm::builder;
use parity_wasm::elements::Internal;

use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::{Interns, Symbol};
use roc_mono::ir::{Proc, ProcLayout};
use roc_mono::layout::LayoutIds;

use crate::backend::WasmBackend;

pub struct Env<'a> {
    pub arena: &'a Bump, // not really using this much, parity_wasm works with std::vec a lot
    pub interns: Interns,
    pub exposed_to_host: MutSet<Symbol>,
}

pub fn build_module<'a>(
    env: &'a Env,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<std::vec::Vec<u8>, String> {
    let mut backend = WasmBackend::new();
    let mut layout_ids = LayoutIds::default();

    // Sort procedures by occurrence order
    //
    // We sort by the "name", but those are interned strings, and the name that is
    // interned first will have a lower number.
    //
    // But, the name that occurs first is always `main` because it is in the (implicit)
    // file header. Therefore sorting high to low will put other functions before main
    //
    // This means that for now other functions in the file have to be ordered "in reverse": if A
    // uses B, then the name of A must first occur after the first occurrence of the name of B
    let mut procedures: std::vec::Vec<_> = procedures.into_iter().collect();
    procedures.sort_by(|a, b| b.0 .0.cmp(&a.0 .0));

    for ((sym, layout), proc) in procedures {
        let function_index = backend.build_proc(proc, sym)?;
        if env.exposed_to_host.contains(&sym) {
            let fn_name = layout_ids
                .get_toplevel(sym, &layout)
                .to_symbol_string(sym, &env.interns);

            let export = builder::export()
                .field(fn_name.as_str())
                .with_internal(Internal::Function(function_index))
                .build();

            backend.builder.push_export(export);
        }
    }

    const MIN_MEMORY_SIZE_KB: u32 = 1024;
    const PAGE_SIZE_KB: u32 = 64;

    let memory = builder::MemoryBuilder::new()
        .with_min(MIN_MEMORY_SIZE_KB / PAGE_SIZE_KB)
        .build();
    backend.builder.push_memory(memory);

    let memory_export = builder::export()
        .field("memory")
        .with_internal(Internal::Memory(0))
        .build();
    backend.builder.push_export(memory_export);

    let module = backend.builder.build();
    module
        .to_bytes()
        .map_err(|e| -> String { format!("Error serialising Wasm module {:?}", e) })
}
