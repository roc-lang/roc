mod backend;

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

    let mut exports = std::vec::Vec::new();
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

            exports.push(export);
        }
    }
    let module = backend.builder.build();
    module
        .to_bytes()
        .map_err(|e| -> String { format!("Error serialising Wasm module {:?}", e) })
}
