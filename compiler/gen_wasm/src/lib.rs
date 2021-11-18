mod backend;
mod layout;
mod low_level;
mod storage;
pub mod wasm_module;

use bumpalo::{self, collections::Vec, Bump};

use roc_collections::all::{MutMap, MutSet};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::ir::{Proc, ProcLayout};
use roc_mono::layout::LayoutIds;

use crate::backend::WasmBackend;
use crate::wasm_module::{
    Align, CodeBuilder, Export, ExportType, LinkingSubSection, LocalId, SymInfo, ValueType,
    WasmModule,
};

const PTR_SIZE: u32 = 4;
const PTR_TYPE: ValueType = ValueType::I32;

pub const STACK_POINTER_GLOBAL_ID: u32 = 0;
pub const FRAME_ALIGNMENT_BYTES: i32 = 16;
pub const MEMORY_NAME: &str = "memory";
pub const BUILTINS_IMPORT_MODULE_NAME: &str = "builtins";
pub const STACK_POINTER_NAME: &str = "__stack_pointer";

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub interns: Interns,
    pub exposed_to_host: MutSet<Symbol>,
}

pub fn build_module<'a>(
    env: &'a Env,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<std::vec::Vec<u8>, String> {
    let (mut wasm_module, _) = build_module_help(env, procedures)?;
    let mut buffer = std::vec::Vec::with_capacity(4096);
    wasm_module.serialize_mut(&mut buffer);
    Ok(buffer)
}

pub fn build_module_help<'a>(
    env: &'a Env,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<(WasmModule<'a>, u32), String> {
    let mut layout_ids = LayoutIds::default();
    let mut generated_procs = Vec::with_capacity_in(procedures.len(), env.arena);
    let mut generated_symbols = Vec::with_capacity_in(procedures.len(), env.arena);
    let mut linker_symbols = Vec::with_capacity_in(procedures.len() * 2, env.arena);
    let mut exports = Vec::with_capacity_in(4, env.arena);
    let mut main_fn_index = None;

    // Collect the symbols & names for the procedures,
    // and filter out procs we're going to inline
    let mut fn_index: u32 = 0;
    for ((sym, layout), proc) in procedures.into_iter() {
        if LowLevel::from_inlined_wrapper(sym).is_some() {
            continue;
        }
        generated_procs.push(proc);
        generated_symbols.push(sym);

        let fn_name = layout_ids
            .get_toplevel(sym, &layout)
            .to_symbol_string(sym, &env.interns);

        if env.exposed_to_host.contains(&sym) {
            main_fn_index = Some(fn_index);
            exports.push(Export {
                name: fn_name.clone(),
                ty: ExportType::Func,
                index: fn_index,
            });
        }

        let linker_sym = SymInfo::for_function(fn_index, fn_name);
        linker_symbols.push(linker_sym);

        fn_index += 1;
    }

    // Build the Wasm module
    let (mut module, linker_symbols) = {
        let mut backend = WasmBackend::new(
            env,
            layout_ids,
            generated_symbols.clone(),
            linker_symbols,
            exports,
        );

        for (proc, sym) in generated_procs.into_iter().zip(generated_symbols) {
            backend.build_proc(proc, sym)?;
        }
        (backend.module, backend.linker_symbols)
    };

    let symbol_table = LinkingSubSection::SymbolTable(linker_symbols);
    module.linking.subsections.push(symbol_table);

    Ok((module, main_fn_index.unwrap()))
}

pub struct CopyMemoryConfig {
    from_ptr: LocalId,
    from_offset: u32,
    to_ptr: LocalId,
    to_offset: u32,
    size: u32,
    alignment_bytes: u32,
}

pub fn copy_memory(code_builder: &mut CodeBuilder, config: CopyMemoryConfig) {
    if config.from_ptr == config.to_ptr && config.from_offset == config.to_offset {
        return;
    }

    let alignment = Align::from(config.alignment_bytes);
    let mut i = 0;
    while config.size - i >= 8 {
        code_builder.get_local(config.to_ptr);
        code_builder.get_local(config.from_ptr);
        code_builder.i64_load(alignment, i + config.from_offset);
        code_builder.i64_store(alignment, i + config.to_offset);
        i += 8;
    }
    if config.size - i >= 4 {
        code_builder.get_local(config.to_ptr);
        code_builder.get_local(config.from_ptr);
        code_builder.i32_load(alignment, i + config.from_offset);
        code_builder.i32_store(alignment, i + config.to_offset);
        i += 4;
    }
    while config.size - i > 0 {
        code_builder.get_local(config.to_ptr);
        code_builder.get_local(config.from_ptr);
        code_builder.i32_load8_u(alignment, i + config.from_offset);
        code_builder.i32_store8(alignment, i + config.to_offset);
        i += 1;
    }
}

/// Round up to alignment_bytes (which must be a power of 2)
pub fn round_up_to_alignment(unaligned: i32, alignment_bytes: i32) -> i32 {
    if alignment_bytes <= 1 {
        return unaligned;
    }
    if alignment_bytes.count_ones() != 1 {
        panic!(
            "Cannot align to {} bytes. Not a power of 2.",
            alignment_bytes
        );
    }
    let mut aligned = unaligned;
    aligned += alignment_bytes - 1; // if lower bits are non-zero, push it over the next boundary
    aligned &= -alignment_bytes; // mask with a flag that has upper bits 1, lower bits 0
    aligned
}

pub fn debug_panic<E: std::fmt::Debug>(error: E) {
    panic!("{:?}", error);
}
