use bumpalo::{collections::Vec, Bump};
use parity_wasm::builder::{CodeLocation, ModuleBuilder};
use parity_wasm::elements::{Internal, ValueType};
use parity_wasm::{builder, elements};

// use roc_builtins::bitcode;
use roc_collections::all::{MutMap, MutSet};
// use roc_module::ident::{ModuleName, TagName};
// use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::ir::{Expr, JoinPointId, Literal, Proc, ProcLayout, Stmt};
use roc_mono::layout::{Builtin, Layout, LayoutIds};

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub interns: Interns,
    pub exposed_to_host: MutSet<Symbol>,
}

#[derive(Clone, Copy)]
struct LocalId(u32);

#[derive(Clone, Copy)]
struct LabelId(u32);

struct WasmLayout {
    value_type: ValueType,
    stack_memory: u32,
}

impl WasmLayout {
    fn new(layout: &Layout) -> Result<Self, String> {
        match layout {
            Layout::Builtin(Builtin::Int64) => Ok(Self {
                value_type: ValueType::I64,
                stack_memory: 0,
            }),
            x => Err(format!("layout, {:?}, not implemented yet", x)),
        }
    }
}

struct SymbolStorage(LocalId, WasmLayout);

// Don't allocate any constant data at the address zero or anywhere near it.
// These addresses are not special in Wasm, but putting something there seems bug-prone.
// Emscripten leaves 1kB free so let's do the same for now, although 4 bytes would probably do.
const UNUSED_DATA_SECTION_BYTES: u32 = 1024;

// State that gets reset for every generated function
struct FunctionGenerator {
    joinpoint_label_map: MutMap<JoinPointId, LabelId>,
    symbol_storage_map: MutMap<Symbol, SymbolStorage>,
    stack_memory: u32,
}

impl FunctionGenerator {
    fn new() -> Self {
        FunctionGenerator {
            joinpoint_label_map: MutMap::default(),
            symbol_storage_map: MutMap::default(),
            stack_memory: 0,
        }
    }

    fn reset(&mut self) {
        self.joinpoint_label_map.clear();
        self.symbol_storage_map.clear();
        self.stack_memory = 0;
    }
}

pub fn build_module<'a>(
    env: &'a Env,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<elements::Module, String> {
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

struct BackendWasm<'a> {
    env: &'a Env<'a>,
    module_builder: ModuleBuilder,
    data_offset_map: MutMap<Literal<'a>, u32>,
    data_offset_next: u32,
    func_gen: FunctionGenerator,
}

impl<'a> BackendWasm<'a> {
    fn new(env: &'a Env) -> Self {
        BackendWasm {
            env,
            module_builder: builder::module(),
            data_offset_map: MutMap::default(),
            data_offset_next: UNUSED_DATA_SECTION_BYTES,
            func_gen: FunctionGenerator::new(),
        }
    }

    fn build_proc(&mut self, proc: Proc<'a>) -> Result<CodeLocation, String> {
        self.func_gen.reset();

        let ret_layout = WasmLayout::new(&proc.ret_layout)?;
        let ret_value_type = ret_layout.value_type;
        if ret_layout.stack_memory > 0 {
            // TODO: insert an extra param for a pointer to space allocated in callee's stack... or does Roc do something else?
            return Err(format!(
                "Not yet implemented: Return in stack memory for non-primtitive layouts like {:?}",
                proc.ret_layout
            ));
        }

        let mut arg_types = Vec::with_capacity_in(proc.args.len(), self.env.arena);

        for (layout, symbol) in proc.args {
            let wasm_layout = WasmLayout::new(layout)?;
            arg_types.push(wasm_layout.value_type);
            self.allocate_local(wasm_layout, *symbol);
        }

        let signature = builder::signature()
            .with_params(arg_types.to_vec()) // TODO: yuck
            .with_result(ret_value_type)
            .build_sig();

        self.build_stmt(&proc.body, &proc.ret_layout)?;

        let function_def = builder::function().with_signature(signature).build();

        let location = self.module_builder.push_function(function_def);
        Ok(location)
    }

    fn allocate_local(&mut self, layout: WasmLayout, symbol: Symbol) -> LocalId {
        let local_id = LocalId(self.func_gen.symbol_storage_map.len() as u32);
        self.func_gen.stack_memory += layout.stack_memory;
        let storage = SymbolStorage(local_id, layout);
        self.func_gen.symbol_storage_map.insert(symbol, storage);
        local_id
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
