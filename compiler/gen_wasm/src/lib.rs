mod backend;
pub mod from_wasm32_memory;
mod layout;
mod storage;
pub mod wasm_module;

use bumpalo::{self, collections::Vec, Bump};

use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::{Interns, Symbol};
use roc_mono::ir::{Proc, ProcLayout};
use roc_mono::layout::LayoutIds;

use crate::backend::WasmBackend;
use crate::wasm_module::{
    Align, CodeBuilder, Export, ExportType, Global, ConstExpr, GlobalType, LinkingSubSection,
    LocalId, SymInfo, ValueType, WasmModule,
};

const PTR_SIZE: u32 = 4;
const PTR_TYPE: ValueType = ValueType::I32;

pub const STACK_POINTER_GLOBAL_ID: u32 = 0;
pub const FRAME_ALIGNMENT_BYTES: i32 = 16;

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub interns: Interns,
    pub exposed_to_host: MutSet<Symbol>,
}

pub fn build_module<'a>(
    env: &'a Env,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<std::vec::Vec<u8>, String> {
    let mut wasm_module = build_module_help(env, procedures)?;
    let mut buffer = std::vec::Vec::with_capacity(4096);
    wasm_module.serialize(&mut buffer);
    Ok(buffer)
}

pub fn build_module_help<'a>(
    env: &'a Env,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<WasmModule<'a>, String> {
    let proc_symbols = Vec::from_iter_in(procedures.keys().map(|(sym, _)| *sym), env.arena);
    let mut backend = WasmBackend::new(env, proc_symbols);

    let mut layout_ids = LayoutIds::default();
    let mut symbol_table_entries = Vec::with_capacity_in(procedures.len(), env.arena);

    for (i, ((sym, layout), proc)) in procedures.into_iter().enumerate() {
        let proc_name = layout_ids
            .get(proc.name, &proc.ret_layout)
            .to_symbol_string(proc.name, &env.interns);
        symbol_table_entries.push(SymInfo::for_function(i as u32, proc_name));

        backend.build_proc(proc, sym)?;

        if env.exposed_to_host.contains(&sym) {
            let fn_name = layout_ids
                .get_toplevel(sym, &layout)
                .to_symbol_string(sym, &env.interns);

            backend.module.export.entries.push(Export {
                name: fn_name,
                ty: ExportType::Func,
                index: i as u32,
            });
        }
    }

    let symbol_table = LinkingSubSection::SymbolTable(symbol_table_entries);
    backend.module.linking.subsections.push(symbol_table);

    backend.module.export.entries.push(Export {
        name: "memory".to_string(),
        ty: ExportType::Mem,
        index: 0,
    });

    let stack_pointer_init = backend.module.memory.min_size().unwrap() as i32;
    backend.module.global.entries.push(Global {
        ty: GlobalType {
            value_type: ValueType::I32,
            is_mutable: true,
        },
        init: ConstExpr::I32(stack_pointer_init),
    });

    Ok(backend.module)
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
    debug_assert!(alignment_bytes.count_ones() == 1);
    let mut aligned = unaligned;
    aligned += alignment_bytes - 1; // if lower bits are non-zero, push it over the next boundary
    aligned &= -alignment_bytes; // mask with a flag that has upper bits 1, lower bits 0
    aligned
}

pub fn debug_panic<E: std::fmt::Debug>(error: E) {
    panic!("{:?}", error);
}
