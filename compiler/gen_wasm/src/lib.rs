mod backend;
mod layout;
mod low_level;
mod storage;
pub mod wasm_module;

use bumpalo::{self, collections::Vec, Bump};

use roc_builtins::bitcode::IntWidth;
use roc_collections::all::{MutMap, MutSet};
use roc_module::low_level::LowLevelWrapperType;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::code_gen_help::CodeGenHelp;
use roc_mono::ir::{Proc, ProcLayout};
use roc_mono::layout::LayoutIds;
use roc_reporting::internal_error;

use crate::backend::WasmBackend;
use crate::wasm_module::{
    Align, CodeBuilder, Export, ExportType, LocalId, SymInfo, ValueType, WasmModule,
};

const PTR_SIZE: u32 = 4;
const PTR_TYPE: ValueType = ValueType::I32;

pub const STACK_POINTER_GLOBAL_ID: u32 = 0;
pub const FRAME_ALIGNMENT_BYTES: i32 = 16;
pub const MEMORY_NAME: &str = "memory";
pub const BUILTINS_IMPORT_MODULE_NAME: &str = "env";
pub const STACK_POINTER_NAME: &str = "__stack_pointer";

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub module_id: ModuleId,
    pub exposed_to_host: MutSet<Symbol>,
}

pub fn build_module<'a>(
    env: &'a Env<'a>,
    interns: &'a mut Interns,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<std::vec::Vec<u8>, String> {
    let (mut wasm_module, _) = build_module_help(env, interns, procedures)?;
    let mut buffer = std::vec::Vec::with_capacity(4096);
    wasm_module.serialize_mut(&mut buffer);
    Ok(buffer)
}

pub fn build_module_help<'a>(
    env: &'a Env<'a>,
    interns: &'a mut Interns,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<(WasmModule<'a>, u32), String> {
    let mut layout_ids = LayoutIds::default();
    let mut procs = Vec::with_capacity_in(procedures.len(), env.arena);
    let mut proc_symbols = Vec::with_capacity_in(procedures.len() * 2, env.arena);
    let mut linker_symbols = Vec::with_capacity_in(procedures.len() * 2, env.arena);
    let mut exports = Vec::with_capacity_in(4, env.arena);
    let mut main_fn_index = None;

    // Collect the symbols & names for the procedures,
    // and filter out procs we're going to inline
    let mut fn_index: u32 = 0;
    for ((sym, layout), proc) in procedures.into_iter() {
        if matches!(
            LowLevelWrapperType::from_symbol(sym),
            LowLevelWrapperType::CanBeReplacedBy(_)
        ) {
            continue;
        }
        procs.push(proc);

        let fn_name = layout_ids
            .get_toplevel(sym, &layout)
            .to_symbol_string(sym, interns);

        if env.exposed_to_host.contains(&sym) {
            main_fn_index = Some(fn_index);
            exports.push(Export {
                name: fn_name.clone(),
                ty: ExportType::Func,
                index: fn_index,
            });
        }

        let linker_sym = SymInfo::for_function(fn_index, fn_name);
        proc_symbols.push((sym, linker_symbols.len() as u32));
        linker_symbols.push(linker_sym);

        fn_index += 1;
    }

    let mut backend = WasmBackend::new(
        env,
        interns,
        layout_ids,
        proc_symbols,
        linker_symbols,
        exports,
        CodeGenHelp::new(env.arena, IntWidth::I32, env.module_id),
    );

    if DEBUG_LOG_SETTINGS.user_procs_ir {
        println!("## procs");
        for proc in procs.iter() {
            println!("{}", proc.to_pretty(200));
            // println!("{:#?}", proc);
        }
    }

    // Generate procs from user code
    for proc in procs.iter() {
        backend.build_proc(proc);
    }

    // Generate specialized helpers for refcounting & equality
    let helper_procs = backend.generate_helpers();

    backend.register_symbol_debug_names();

    if DEBUG_LOG_SETTINGS.helper_procs_ir {
        println!("## helper_procs");
        for proc in helper_procs.iter() {
            println!("{}", proc.to_pretty(200));
            // println!("{:#?}", proc);
        }
    }

    // Generate Wasm for refcounting procs
    for proc in helper_procs.iter() {
        backend.build_proc(proc);
    }

    let module = backend.finalize_module();

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
    if config.size == 0 {
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
#[macro_export]
macro_rules! round_up_to_alignment {
    ($unaligned: expr, $alignment_bytes: expr) => {
        if $alignment_bytes <= 1 {
            $unaligned
        } else if $alignment_bytes.count_ones() != 1 {
            panic!(
                "Cannot align to {} bytes. Not a power of 2.",
                $alignment_bytes
            );
        } else {
            let mut aligned = $unaligned;
            aligned += $alignment_bytes - 1; // if lower bits are non-zero, push it over the next boundary
            aligned &= !$alignment_bytes + 1; // mask with a flag that has upper bits 1, lower bits 0
            aligned
        }
    };
}

pub fn debug_panic<E: std::fmt::Debug>(error: E) {
    internal_error!("{:?}", error);
}

pub struct WasmDebugLogSettings {
    proc_start_end: bool,
    user_procs_ir: bool,
    helper_procs_ir: bool,
    let_stmt_ir: bool,
    instructions: bool,
    pub keep_test_binary: bool,
}

pub const DEBUG_LOG_SETTINGS: WasmDebugLogSettings = WasmDebugLogSettings {
    proc_start_end: false && cfg!(debug_assertions),
    user_procs_ir: false && cfg!(debug_assertions),
    helper_procs_ir: false && cfg!(debug_assertions),
    let_stmt_ir: false && cfg!(debug_assertions),
    instructions: false && cfg!(debug_assertions),
    keep_test_binary: false && cfg!(debug_assertions),
};
