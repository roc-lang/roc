use bumpalo::Bump;
use parity_wasm::builder::{CodeLocation, ModuleBuilder};
use parity_wasm::elements::{Instructions, Internal};
use parity_wasm::{builder, elements};

use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::{Interns, Symbol};
use roc_mono::ir::{CallType, Expr, Literal, Proc, ProcLayout, Stmt};
use roc_mono::layout::LayoutIds;

use crate::function::FunctionGenerator;

pub struct Env<'a> {
    pub arena: &'a Bump, // not really using this much, parity_wasm works with std::vec a lot
    pub interns: Interns,
    pub exposed_to_host: MutSet<Symbol>,
}

// Don't allocate any constant data at the address zero or anywhere near it.
// These addresses are not special in Wasm, but putting something there seems bug-prone.
const UNUSED_DATA_SECTION_BYTES: u32 = 1024;

pub fn build_module<'a>(
    env: &'a Env,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<elements::Module, String> {
    let mut module_state = ModuleState::new(env);
    let mut layout_ids = LayoutIds::default();

    for ((sym, layout), proc) in procedures {
        let location = build_proc(&mut module_state, proc)?;

        if env.exposed_to_host.contains(&sym) {
            let fn_name = layout_ids
                .get_toplevel(sym, &layout)
                .to_symbol_string(sym, &env.interns);

            let export = builder::export()
                .field(fn_name.as_str())
                .with_internal(Internal::Function(location.body))
                .build();

            module_state.module_builder.push_export(export);
        }
        module_state.proc_symbol_map.insert(sym, location);
    }

    Ok(module_state.module_builder.build())
}

fn build_proc<'a>(module_state: &mut ModuleState, proc: Proc<'a>) -> Result<CodeLocation, String> {
    // TODO: see if we can reuse the same memory each time and reset it?
    // Can't convince the borrow-checker to let me do that, as things get moved into the function builder.
    let mut func_gen = FunctionGenerator::new(module_state);
    func_gen.build(proc)?;

    let signature = builder::signature()
        .with_params(func_gen.arg_types) // requires std::Vec, not Bumpalo
        .with_result(func_gen.ret_type)
        .build_sig();

    let function_def = builder::function()
        .with_signature(signature)
        .body()
        .with_locals(func_gen.locals)
        .with_instructions(Instructions::new(func_gen.instructions))
        .build() // body
        .build(); // function

    let location = module_state.module_builder.push_function(function_def);
    Ok(location)
}

pub struct ModuleState<'a> {
    _env: &'a Env<'a>,
    module_builder: ModuleBuilder,
    pub proc_symbol_map: MutMap<Symbol, CodeLocation>,
    pub _data_offset_map: MutMap<Literal<'a>, u32>,
    pub _data_offset_next: u32,
}

impl<'a> ModuleState<'a> {
    fn new(_env: &'a Env) -> Self {
        ModuleState {
            _env,
            module_builder: builder::module(),
            proc_symbol_map: MutMap::default(),
            _data_offset_map: MutMap::default(),
            _data_offset_next: UNUSED_DATA_SECTION_BYTES,
        }
    }
}

// TODO: use something like this for very simple inlining
// Create a HashMap of inlined Procs, generate each call with different Symbol arguments
fn _is_lowlevel_wrapper<'a>(proc: Proc<'a>) -> bool {
    match proc.body {
        Stmt::Let(_, expr, _, Stmt::Ret(..)) => match expr {
            Expr::Call(roc_mono::ir::Call { call_type, .. }) => match call_type {
                CallType::LowLevel { .. } => true,
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}
