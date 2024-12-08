//! Provides the WASM backend to generate Roc binaries.
mod backend;
mod code_builder;
mod layout;
mod low_level;
mod storage;

// Helpers for interfacing to a Wasm module from outside
pub mod wasm32_result;
pub mod wasm32_sized;

use bitvec::prelude::BitVec;
use bumpalo::collections::Vec;
use bumpalo::{self, Bump};

use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::code_gen_help::CodeGenHelp;
use roc_mono::ir::{Proc, ProcLayout};
use roc_mono::layout::{LayoutIds, STLayoutInterner};
use roc_target::Target;
use roc_wasm_module::parse::ParseError;
use roc_wasm_module::{Align, LocalId, ValueType, WasmModule};

use crate::backend::{ProcLookupData, ProcSource, WasmBackend};
use crate::code_builder::CodeBuilder;

const TARGET: Target = Target::Wasm32;
const PTR_SIZE: u32 = {
    let value = TARGET.ptr_width() as u32;

    // const assert that our pointer width is actually 4
    // the code relies on the pointer width being exactly 4
    assert!(value == 4);

    value
};
const PTR_TYPE: ValueType = ValueType::I32;

pub const MEMORY_NAME: &str = "memory";

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub module_id: ModuleId,
    pub exposed_to_host: MutSet<Symbol>,
    pub stack_bytes: u32,
}

impl Env<'_> {
    pub const DEFAULT_STACK_BYTES: u32 = 1024 * 1024;
}

/// Parse the preprocessed host binary
/// If successful, the module can be passed to build_app_binary
pub fn parse_host<'a>(arena: &'a Bump, host_bytes: &[u8]) -> Result<WasmModule<'a>, ParseError> {
    let require_relocatable = true;
    WasmModule::preload(arena, host_bytes, require_relocatable)
}

/// Generate a Wasm module in binary form, ready to write to a file. Entry point from roc_build.
///   env            environment data from previous compiler stages
///   interns        names of functions and variables (as memory-efficient interned strings)
///   host_module    parsed module from a Wasm object file containing all of the non-Roc code
///   procedures     Roc code in monomorphized intermediate representation
pub fn build_app_binary<'a, 'r>(
    env: &'r Env<'a>,
    layout_interner: &'r mut STLayoutInterner<'a>,
    interns: &'r mut Interns,
    host_module: WasmModule<'a>,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> std::vec::Vec<u8> {
    let (mut wasm_module, called_fns, _) =
        build_app_module(env, layout_interner, interns, host_module, procedures);

    wasm_module.eliminate_dead_code(env.arena, called_fns);

    let mut buffer = std::vec::Vec::with_capacity(wasm_module.size());
    wasm_module.serialize(&mut buffer);
    buffer
}

/// Generate an unserialized Wasm module
/// Shared by all consumers of gen_wasm: roc_build, roc_repl_wasm, and test_gen
/// (roc_repl_wasm and test_gen will add more generated code for a wrapper function
/// that defines a common interface to `main`, independent of return type.)
pub fn build_app_module<'a, 'r>(
    env: &'r Env<'a>,
    layout_interner: &'r mut STLayoutInterner<'a>,
    interns: &'r mut Interns,
    host_module: WasmModule<'a>,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> (WasmModule<'a>, BitVec<usize>, u32) {
    let mut layout_ids = LayoutIds::default();
    let mut procs = Vec::with_capacity_in(procedures.len(), env.arena);
    let mut proc_lookup = Vec::with_capacity_in(procedures.len() * 2, env.arena);
    let mut host_to_app_map = Vec::with_capacity_in(env.exposed_to_host.len(), env.arena);
    let mut maybe_main_fn_index = None;

    // Adjust Wasm function indices to account for functions from the object file
    let fn_index_offset: u32 =
        host_module.import.function_count() as u32 + host_module.code.function_count;

    // Pre-pass over the procedure names & layouts
    // Create a lookup to tell us the final index of each proc in the output file
    for (i, ((sym, proc_layout), proc)) in procedures.into_iter().enumerate() {
        let fn_index = fn_index_offset + i as u32;
        procs.push(proc);
        if env.exposed_to_host.contains(&sym) {
            maybe_main_fn_index = Some(fn_index);

            let exposed_name = layout_ids
                .get_toplevel(sym, &proc_layout)
                .to_exposed_symbol_string(sym, interns);

            let exposed_name_bump: &'a str = env.arena.alloc_str(&exposed_name);

            host_to_app_map.push((exposed_name_bump, fn_index));
        }

        proc_lookup.push(ProcLookupData {
            name: sym,
            layout: proc_layout,
            source: ProcSource::Roc,
        });
    }

    let mut backend = WasmBackend::new(
        env,
        layout_interner,
        interns,
        layout_ids,
        proc_lookup,
        host_to_app_map,
        host_module,
        fn_index_offset,
        CodeGenHelp::new(env.arena, Target::Wasm32, env.module_id),
    );

    if DEBUG_SETTINGS.user_procs_ir {
        println!("## procs");
        for proc in procs.iter() {
            println!("{}", proc.to_pretty(backend.layout_interner, 200, true));
            // println!("{:?}", proc);
        }
    }

    // Generate procs from user code
    for proc in procs.iter() {
        backend.build_proc(proc);
    }

    // Generate specialized helpers for refcounting & equality
    let helper_procs = backend.get_helpers();

    backend.register_symbol_debug_names();

    if DEBUG_SETTINGS.helper_procs_ir {
        println!("## helper_procs");
        for proc in helper_procs.iter() {
            println!("{}", proc.to_pretty(backend.layout_interner, 200, true));
            // println!("{:#?}", proc);
        }
    }

    // Generate Wasm for helpers and Zig/Roc wrappers
    let sources = Vec::from_iter_in(
        backend
            .proc_lookup
            .iter()
            .map(|ProcLookupData { source, .. }| *source),
        env.arena,
    );
    let mut helper_iter = helper_procs.iter();
    for (idx, source) in sources.iter().enumerate() {
        use ProcSource::*;
        match source {
            Roc => { /* already generated */ }
            Helper => backend.build_proc(helper_iter.next().unwrap()),
            HigherOrderCompare(inner_idx) => backend.build_higher_order_compare(idx, *inner_idx),
        }
    }

    let (module, called_fns) = backend.finalize();
    let main_function_index =
        maybe_main_fn_index.expect("The app must expose at least one value to the host");

    (module, called_fns, main_function_index)
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

pub struct WasmDebugSettings {
    proc_start_end: bool,
    user_procs_ir: bool,
    helper_procs_ir: bool,
    let_stmt_ir: bool,
    instructions: bool,
    storage_map: bool,
    pub keep_test_binary: bool,
}

pub const DEBUG_SETTINGS: WasmDebugSettings = WasmDebugSettings {
    proc_start_end: false && cfg!(debug_assertions),
    user_procs_ir: false && cfg!(debug_assertions), // Note: we also have `ROC_PRINT_IR_AFTER_REFCOUNT=1 cargo test-gen-wasm`
    helper_procs_ir: false && cfg!(debug_assertions),
    let_stmt_ir: false && cfg!(debug_assertions),
    instructions: false && cfg!(debug_assertions),
    storage_map: false && cfg!(debug_assertions),
    keep_test_binary: false && cfg!(debug_assertions),
};

#[cfg(test)]
mod dummy_platform_functions {
    // `cargo test` produces an executable. At least on Windows, this means that extern symbols must be defined. This crate imports roc_std which
    // defines a bunch of externs, and uses the three below. We provide dummy implementations because these functions are not called.
    use core::ffi::c_void;

    /// # Safety
    /// This is only marked unsafe to typecheck without warnings in the rest of the code here.
    #[no_mangle]
    pub unsafe extern "C" fn roc_alloc(_size: usize, _alignment: u32) -> *mut c_void {
        unimplemented!("It is not valid to call roc alloc from within the compiler. Please use the \"platform\" feature if this is a platform.")
    }

    /// # Safety
    /// This is only marked unsafe to typecheck without warnings in the rest of the code here.
    #[no_mangle]
    pub unsafe extern "C" fn roc_realloc(
        _ptr: *mut c_void,
        _new_size: usize,
        _old_size: usize,
        _alignment: u32,
    ) -> *mut c_void {
        unimplemented!("It is not valid to call roc realloc from within the compiler. Please use the \"platform\" feature if this is a platform.")
    }

    /// # Safety
    /// This is only marked unsafe to typecheck without warnings in the rest of the code here.
    #[no_mangle]
    pub unsafe extern "C" fn roc_dealloc(_ptr: *mut c_void, _alignment: u32) {
        unimplemented!("It is not valid to call roc dealloc from within the compiler. Please use the \"platform\" feature if this is a platform.")
    }

    #[no_mangle]
    pub unsafe extern "C" fn roc_panic(_c_ptr: *mut c_void, _tag_id: u32) {
        unimplemented!("It is not valid to call roc panic from within the compiler. Please use the \"platform\" feature if this is a platform.")
    }

    #[no_mangle]
    pub unsafe extern "C" fn roc_dbg(_loc: *mut c_void, _msg: *mut c_void, _src: *mut c_void) {
        unimplemented!("It is not valid to call roc dbg from within the compiler. Please use the \"platform\" feature if this is a platform.")
    }

    #[no_mangle]
    pub fn roc_memset(_dst: *mut c_void, _c: i32, _n: usize) -> *mut c_void {
        unimplemented!("It is not valid to call roc memset from within the compiler. Please use the \"platform\" feature if this is a platform.")
    }
}
