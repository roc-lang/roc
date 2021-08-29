use bumpalo::{collections::Vec, Bump};
use parity_wasm::{elements, builder};
use parity_wasm::elements::{ValueType, Internal};
use parity_wasm::builder::{ModuleBuilder, FunctionDefinition, FunctionBuilder, CodeLocation};

// use roc_builtins::bitcode;
use roc_collections::all::{MutMap, MutSet};
// use roc_module::ident::{ModuleName, TagName};
// use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::ir::{BranchInfo, CallType, Expr, JoinPointId, Literal, Proc, ProcLayout, Stmt};
use roc_mono::layout::{Builtin, Layout, LayoutIds};


pub struct Env<'a> {
    pub arena: &'a Bump,
    pub interns: Interns,
    pub exposed_to_host: MutSet<Symbol>,
}

pub enum SymbolStorage {
    Local(ValueType, u32),
    LocalAndBase(ValueType, u32, u32),
}

pub struct LabelId(u32);

// Don't allocate any constant data at the address zero or anywhere near it.
// These addresses are not special in Wasm, but putting something there seems bug-prone.
// Emscripten leaves 1kB free so let's do the same for now, although 4 bytes would probably do.
const UNUSED_DATA_SECTION_BYTES: u32 = 1024;

pub struct BackendWasm<'a> {
    // module-level state
    env: &'a Env<'a>,
    module_builder: ModuleBuilder,
    data_offset_map: MutMap<Literal<'a>, u32>,
    data_offset_next: u32,

    // procedure-level state
    symbol_storage_map: MutMap<Symbol, SymbolStorage>,
    joinpoint_label_map: MutMap<JoinPointId, LabelId>,
}

pub fn build_module<'a>(env: &'a Env, procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>) -> Result<elements::Module, String> {
    let mut backend = BackendWasm::new(env);
    let mut layout_ids = LayoutIds::default();

    for ((sym, layout), proc) in procedures {
        let location = backend.build_proc(proc)?;

        if env.exposed_to_host.contains(&sym) {
            let fn_name = layout_ids
                .get_toplevel(sym, &layout)
                .to_symbol_string(sym, &env.interns);

            let export = builder::export()
                .field(fn_name.as_str())
                .with_internal(Internal::Function(location.body))
                .build();

            backend.module_builder.push_export(export);
        }
    }

    Ok(backend.module_builder.build())
}

impl <'a>BackendWasm<'a> {
    pub fn new(env: &'a Env) -> Self {
        BackendWasm {
            env,
            module_builder: builder::module(),
            data_offset_map: MutMap::default(),
            data_offset_next: UNUSED_DATA_SECTION_BYTES,
            symbol_storage_map: MutMap::default(),
            joinpoint_label_map: MutMap::default(),
        }
    }

    fn build_proc(&mut self, proc: Proc<'a>) -> Result<CodeLocation, String> {
        let mut function_builder = builder::function();
        // 
        // ... generate stuff ...
        // 
        let def = function_builder.build();
        let location = self.module_builder.push_function(def);
        Ok(location)
    }

    fn build_stmt(&mut self, stmt: &Stmt<'a>, ret_layout: &Layout<'a>) -> Result<(), String> {
        Err("todo: everything".to_string())
    }

    fn build_expr(
        &mut self,
        sym: &Symbol,
        expr: &Expr<'a>,
        layout: &Layout<'a>,
    ) -> Result<(), String> {
        Err("todo: everything".to_string())
    }
}
