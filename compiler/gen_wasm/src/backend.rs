use parity_wasm::builder;
use parity_wasm::builder::{CodeLocation, ModuleBuilder};
use parity_wasm::elements::{Instruction, Instruction::*, Instructions, Local, ValueType};

use roc_collections::all::MutMap;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_mono::ir::{CallType, Expr, Literal, Proc, Stmt};
use roc_mono::layout::{Builtin, Layout};

// Don't allocate any constant data at address zero or near it. Would be valid, but bug-prone.
// Follow Emscripten's example by using 1kB (4 bytes would probably do)
const UNUSED_DATA_SECTION_BYTES: u32 = 1024;

#[derive(Clone, Copy)]
struct LocalId(u32);

#[derive(Clone, Copy)]
struct LabelId(u32);

struct SymbolStorage(LocalId, WasmLayout);

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

pub struct WasmBackend<'a> {
    // Module: Wasm AST
    pub builder: ModuleBuilder,

    // Module: internal state & IR mappings
    _data_offset_map: MutMap<Literal<'a>, u32>,
    _data_offset_next: u32,
    proc_symbol_map: MutMap<Symbol, CodeLocation>,

    // Functions: Wasm AST
    instructions: std::vec::Vec<Instruction>,
    ret_type: ValueType,
    arg_types: std::vec::Vec<ValueType>,
    locals: std::vec::Vec<Local>,

    // Functions: internal state & IR mappings
    stack_memory: u32,
    symbol_storage_map: MutMap<Symbol, SymbolStorage>,
    // joinpoint_label_map: MutMap<JoinPointId, LabelId>,
}

impl<'a> WasmBackend<'a> {
    pub fn new() -> Self {
        WasmBackend {
            // Module: Wasm AST
            builder: builder::module(),

            // Module: internal state & IR mappings
            _data_offset_map: MutMap::default(),
            _data_offset_next: UNUSED_DATA_SECTION_BYTES,
            proc_symbol_map: MutMap::default(),

            // Functions: Wasm AST
            instructions: std::vec::Vec::new(),
            ret_type: ValueType::I32,
            arg_types: std::vec::Vec::new(),
            locals: std::vec::Vec::new(),

            // Functions: internal state & IR mappings
            stack_memory: 0,
            symbol_storage_map: MutMap::default(),
            // joinpoint_label_map: MutMap::default(),
        }
    }

    pub fn build_proc(&mut self, proc: Proc<'a>, sym: Symbol) -> Result<u32, String> {
        let ret_layout = WasmLayout::new(&proc.ret_layout)?;
        if ret_layout.stack_memory > 0 {
            // TODO: if returning a struct by value, add an extra argument for a pointer to callee's stack memory
            return Err(format!(
                "Not yet implemented: Return in stack memory for non-primtitive layouts like {:?}",
                proc.ret_layout
            ));
        }

        self.ret_type = ret_layout.value_type;
        self.arg_types = std::vec::Vec::with_capacity(proc.args.len());

        for (layout, symbol) in proc.args {
            let wasm_layout = WasmLayout::new(layout)?;
            self.arg_types.push(wasm_layout.value_type);
            self.insert_local(wasm_layout, *symbol);
        }

        self.build_stmt(&proc.body, &proc.ret_layout)?;

        let signature = builder::signature()
            .with_params(self.arg_types.clone()) // requires std::Vec, not Bumpalo
            .with_result(self.ret_type)
            .build_sig();

        let function_def = builder::function()
            .with_signature(signature)
            .body()
            .with_locals(self.locals.clone())
            .with_instructions(Instructions::new(self.instructions.clone()))
            .build() // body
            .build(); // function

        let location = self.builder.push_function(function_def);
        let function_index = location.body;
        self.proc_symbol_map.insert(sym, location);

        Ok(function_index)
    }

    fn insert_local(&mut self, layout: WasmLayout, symbol: Symbol) -> LocalId {
        let local_id = LocalId(self.locals.len() as u32);
        self.locals.push(Local::new(1, layout.value_type));
        self.stack_memory += layout.stack_memory;

        let storage = SymbolStorage(local_id, layout);
        self.symbol_storage_map.insert(symbol, storage);

        local_id
    }

    fn get_symbol_storage(&self, sym: &Symbol) -> Result<&SymbolStorage, String> {
        self.symbol_storage_map
            .get(sym)
            .ok_or(format!("Symbol not found in function scope {:?}", sym))
    }

    fn load_from_symbol(&mut self, sym: &Symbol) -> Result<(), String> {
        let SymbolStorage(LocalId(local_id), _) = self.get_symbol_storage(sym)?;
        let id: u32 = *local_id;
        self.instructions.push(GetLocal(id));
        Ok(())
    }

    // Store whatever value is on top of the VM's stack
    fn store_to_symbol(&mut self, sym: &Symbol) -> Result<(), String> {
        let SymbolStorage(LocalId(local_id), _) = self.get_symbol_storage(sym)?;
        let id: u32 = *local_id;
        self.instructions.push(SetLocal(id));
        Ok(())
    }

    fn build_stmt(&mut self, stmt: &Stmt<'a>, ret_layout: &Layout<'a>) -> Result<(), String> {
        match stmt {
            Stmt::Let(sym, expr, layout, following) => {
                self.build_expr(sym, expr, layout)?;

                let wasm_layout = WasmLayout::new(layout)?;
                let local_id = self.insert_local(wasm_layout, *sym);
                self.instructions.push(SetLocal(local_id.0));

                self.build_stmt(following, ret_layout)?;
                Ok(())
            }
            Stmt::Ret(sym) => {
                if let Some(SymbolStorage(local_id, _)) = self.symbol_storage_map.get(sym) {
                    self.instructions.push(GetLocal(local_id.0));
                    self.instructions.push(Return);
                    Ok(())
                } else {
                    Err(format!(
                        "Not yet implemented: returning values with layout {:?}",
                        ret_layout
                    ))
                }
            }
            x => Err(format!("statement not yet implemented: {:?}", x)),
        }
    }

    fn build_expr(
        &mut self,
        sym: &Symbol,
        expr: &Expr<'a>,
        layout: &Layout<'a>,
    ) -> Result<(), String> {
        match expr {
            Expr::Literal(lit) => self.load_literal(lit),

            Expr::Call(roc_mono::ir::Call {
                call_type,
                arguments,
            }) => match call_type {
                CallType::ByName { name: func_sym, .. } => {
                    for arg in *arguments {
                        self.load_from_symbol(arg)?;
                    }
                    let function_location = self.proc_symbol_map.get(func_sym).ok_or(format!(
                        "Cannot find function {:?} called from {:?}",
                        func_sym, sym
                    ))?;
                    self.instructions.push(Call(function_location.body));
                    self.store_to_symbol(sym)?;
                    Ok(())
                }

                CallType::LowLevel { op: lowlevel, .. } => {
                    self.build_call_low_level(sym, lowlevel, arguments, layout)
                }
                x => Err(format!("the call type, {:?}, is not yet implemented", x)),
            },

            x => Err(format!("Expression is not yet implemented {:?}", x)),
        }
    }

    fn load_literal(&mut self, lit: &Literal<'a>) -> Result<(), String> {
        match lit {
            Literal::Int(x) => {
                self.instructions.push(I64Const(*x as i64));
                Ok(())
            }
            Literal::Float(x) => {
                let val: f64 = *x;
                self.instructions.push(F64Const(val.to_bits()));
                Ok(())
            }
            x => Err(format!("loading literal, {:?}, is not yet implemented", x)),
        }
    }

    fn build_call_low_level(
        &mut self,
        sym: &Symbol,
        lowlevel: &LowLevel,
        args: &'a [Symbol],
        layout: &Layout<'a>,
    ) -> Result<(), String> {
        for arg in args {
            self.load_from_symbol(arg)?;
        }
        let wasm_layout = WasmLayout::new(layout)?;
        self.build_instructions_lowlevel(lowlevel, wasm_layout.value_type)?;
        self.store_to_symbol(sym)?;
        Ok(())
    }

    fn build_instructions_lowlevel(
        &mut self,
        lowlevel: &LowLevel,
        value_type: ValueType,
    ) -> Result<(), String> {
        // TODO:  Find a way to organise all the lowlevel ops and layouts! There's lots!
        //
        // Some Roc low-level ops care about wrapping, clipping, sign-extending...
        // For those, we'll need to pre-process each argument before the main op,
        // so simple arrays of instructions won't work. But there are common patterns.
        let instructions: &[Instruction] = match lowlevel {
            // Wasm type might not be enough, may need to sign-extend i8 etc. Maybe in load_from_symbol?
            LowLevel::NumAdd => match value_type {
                ValueType::I32 => &[I32Add],
                ValueType::I64 => &[I64Add],
                ValueType::F32 => &[F32Add],
                ValueType::F64 => &[F64Add],
            },
            _ => {
                return Err(format!("unsupported low-level op {:?}", lowlevel));
            }
        };
        self.instructions.extend_from_slice(instructions);
        Ok(())
    }
}
