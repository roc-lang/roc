use parity_wasm::builder;
use parity_wasm::builder::{CodeLocation, ModuleBuilder};
use parity_wasm::elements::{
    BlockType, Instruction, Instruction::*, Instructions, Local, ValueType,
};

use roc_collections::all::MutMap;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_mono::ir::{CallType, Expr, JoinPointId, Literal, Proc, Stmt};
use roc_mono::layout::{Builtin, Layout, UnionLayout};

use crate::*;

// Don't allocate any constant data at address zero or near it. Would be valid, but bug-prone.
// Follow Emscripten's example by using 1kB (4 bytes would probably do)
const UNUSED_DATA_SECTION_BYTES: u32 = 1024;

#[derive(Clone, Copy, Debug)]
struct LocalId(u32);

#[derive(Clone, Copy, Debug)]
struct LabelId(u32);

#[derive(Debug)]
struct SymbolStorage(LocalId, WasmLayout);

// See README for background information on Wasm locals, memory and function calls
#[derive(Debug)]
pub enum WasmLayout {
    // Most number types can fit in a Wasm local without any stack memory.
    // Roc i8 is represented as an i32 local. Store the type and the original size.
    LocalOnly(ValueType, u32),

    // A `local` pointing to stack memory
    StackMemory(u32),

    // A `local` pointing to heap memory
    HeapMemory,
}

impl WasmLayout {
    fn new(layout: &Layout) -> Self {
        use ValueType::*;
        let size = layout.stack_size(PTR_SIZE);
        match layout {
            Layout::Builtin(Builtin::Int128) => Self::StackMemory(size),
            Layout::Builtin(Builtin::Int64) => Self::LocalOnly(I64, size),
            Layout::Builtin(Builtin::Int32) => Self::LocalOnly(I32, size),
            Layout::Builtin(Builtin::Int16) => Self::LocalOnly(I32, size),
            Layout::Builtin(Builtin::Int8) => Self::LocalOnly(I32, size),
            Layout::Builtin(Builtin::Int1) => Self::LocalOnly(I32, size),
            Layout::Builtin(Builtin::Usize) => Self::LocalOnly(I32, size),
            Layout::Builtin(Builtin::Decimal) => Self::StackMemory(size),
            Layout::Builtin(Builtin::Float128) => Self::StackMemory(size),
            Layout::Builtin(Builtin::Float64) => Self::LocalOnly(F64, size),
            Layout::Builtin(Builtin::Float32) => Self::LocalOnly(F32, size),
            Layout::Builtin(Builtin::Str) => Self::StackMemory(size),
            Layout::Builtin(Builtin::Dict(_, _)) => Self::StackMemory(size),
            Layout::Builtin(Builtin::Set(_)) => Self::StackMemory(size),
            Layout::Builtin(Builtin::List(_)) => Self::StackMemory(size),
            Layout::Builtin(Builtin::EmptyStr) => Self::StackMemory(size),
            Layout::Builtin(Builtin::EmptyList) => Self::StackMemory(size),
            Layout::Builtin(Builtin::EmptyDict) => Self::StackMemory(size),
            Layout::Builtin(Builtin::EmptySet) => Self::StackMemory(size),
            Layout::LambdaSet(lambda_set) => WasmLayout::new(&lambda_set.runtime_representation()),
            Layout::Struct(_) => Self::StackMemory(size),
            Layout::Union(UnionLayout::NonRecursive(_)) => Self::StackMemory(size),
            Layout::Union(UnionLayout::Recursive(_)) => Self::HeapMemory,
            Layout::Union(UnionLayout::NonNullableUnwrapped(_)) => Self::HeapMemory,
            Layout::Union(UnionLayout::NullableWrapped { .. }) => Self::HeapMemory,
            Layout::Union(UnionLayout::NullableUnwrapped { .. }) => Self::HeapMemory,
            Layout::RecursivePointer => Self::HeapMemory,
        }
    }

    fn value_type(&self) -> ValueType {
        match self {
            Self::LocalOnly(type_, _) => *type_,
            _ => PTR_TYPE,
        }
    }

    fn stack_memory(&self) -> u32 {
        match self {
            Self::StackMemory(size) => *size,
            _ => 0,
        }
    }

    #[allow(dead_code)]
    fn load(&self, offset: u32) -> Result<Instruction, String> {
        use crate::backend::WasmLayout::*;
        use ValueType::*;

        match self {
            LocalOnly(I32, 4) => Ok(I32Load(ALIGN_4, offset)),
            LocalOnly(I32, 2) => Ok(I32Load16S(ALIGN_2, offset)),
            LocalOnly(I32, 1) => Ok(I32Load8S(ALIGN_1, offset)),
            LocalOnly(I64, 8) => Ok(I64Load(ALIGN_8, offset)),
            LocalOnly(F64, 8) => Ok(F64Load(ALIGN_8, offset)),
            LocalOnly(F32, 4) => Ok(F32Load(ALIGN_4, offset)),

            // LocalOnly(F32, 2) => Ok(), // convert F16 to F32 (lowlevel function? Wasm-only?)
            // StackMemory(size) => Ok(), // would this be some kind of memcpy in the IR?
            HeapMemory => {
                if PTR_TYPE == I64 {
                    Ok(I64Load(ALIGN_8, offset))
                } else {
                    Ok(I32Load(ALIGN_4, offset))
                }
            }

            _ => Err(format!(
                "Failed to generate load instruction for WasmLayout {:?}",
                self
            )),
        }
    }

    #[allow(dead_code)]
    fn store(&self, offset: u32) -> Result<Instruction, String> {
        use crate::backend::WasmLayout::*;
        use ValueType::*;

        match self {
            LocalOnly(I32, 4) => Ok(I32Store(ALIGN_4, offset)),
            LocalOnly(I32, 2) => Ok(I32Store16(ALIGN_2, offset)),
            LocalOnly(I32, 1) => Ok(I32Store8(ALIGN_1, offset)),
            LocalOnly(I64, 8) => Ok(I64Store(ALIGN_8, offset)),
            LocalOnly(F64, 8) => Ok(F64Store(ALIGN_8, offset)),
            LocalOnly(F32, 4) => Ok(F32Store(ALIGN_4, offset)),

            // LocalOnly(F32, 2) => Ok(), // convert F32 to F16 (lowlevel function? Wasm-only?)
            // StackMemory(size) => Ok(), // would this be some kind of memcpy in the IR?
            HeapMemory => {
                if PTR_TYPE == I64 {
                    Ok(I64Store(ALIGN_8, offset))
                } else {
                    Ok(I32Store(ALIGN_4, offset))
                }
            }

            _ => Err(format!(
                "Failed to generate store instruction for WasmLayout {:?}",
                self
            )),
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
    /// how many blocks deep are we (used for jumps)
    block_depth: u32,
    joinpoint_label_map: MutMap<JoinPointId, (u32, std::vec::Vec<LocalId>)>,
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
            instructions: std::vec::Vec::with_capacity(256),
            ret_type: ValueType::I32,
            arg_types: std::vec::Vec::with_capacity(8),
            locals: std::vec::Vec::with_capacity(32),

            // Functions: internal state & IR mappings
            stack_memory: 0,
            symbol_storage_map: MutMap::default(),
            block_depth: 0,
            joinpoint_label_map: MutMap::default(),
        }
    }

    fn reset(&mut self) {
        // Functions: Wasm AST
        self.instructions.clear();
        self.arg_types.clear();
        self.locals.clear();

        // Functions: internal state & IR mappings
        self.stack_memory = 0;
        self.symbol_storage_map.clear();
        // joinpoint_label_map.clear();
    }

    pub fn build_proc(&mut self, proc: Proc<'a>, sym: Symbol) -> Result<u32, String> {
        let ret_layout = WasmLayout::new(&proc.ret_layout);

        if let WasmLayout::StackMemory { .. } = ret_layout {
            return Err(format!(
                "Not yet implemented: Returning values to callee stack memory {:?} {:?}",
                proc.name, sym
            ));
        }

        self.ret_type = ret_layout.value_type();
        self.arg_types.reserve(proc.args.len());

        for (layout, symbol) in proc.args {
            let wasm_layout = WasmLayout::new(layout);
            self.arg_types.push(wasm_layout.value_type());
            self.insert_local(wasm_layout, *symbol);
        }

        self.build_stmt(&proc.body, &proc.ret_layout)?;

        let signature = builder::signature()
            .with_params(self.arg_types.clone()) // requires std::Vec, not Bumpalo
            .with_result(self.ret_type)
            .build_sig();

        // functions must end with an End instruction/opcode
        let mut instructions = self.instructions.clone();
        instructions.push(Instruction::End);

        let function_def = builder::function()
            .with_signature(signature)
            .body()
            .with_locals(self.locals.clone())
            .with_instructions(Instructions::new(instructions))
            .build() // body
            .build(); // function

        let location = self.builder.push_function(function_def);
        let function_index = location.body;
        self.proc_symbol_map.insert(sym, location);
        self.reset();

        Ok(function_index)
    }

    fn insert_local(&mut self, layout: WasmLayout, symbol: Symbol) -> LocalId {
        self.stack_memory += layout.stack_memory();
        let index = self.symbol_storage_map.len();
        if index >= self.arg_types.len() {
            self.locals.push(Local::new(1, layout.value_type()));
        }
        let local_id = LocalId(index as u32);
        let storage = SymbolStorage(local_id, layout);
        self.symbol_storage_map.insert(symbol, storage);
        local_id
    }

    fn get_symbol_storage(&self, sym: &Symbol) -> Result<&SymbolStorage, String> {
        self.symbol_storage_map.get(sym).ok_or_else(|| {
            format!(
                "Symbol {:?} not found in function scope:\n{:?}",
                sym, self.symbol_storage_map
            )
        })
    }

    fn load_from_symbol(&mut self, sym: &Symbol) -> Result<(), String> {
        let SymbolStorage(LocalId(local_id), _) = self.get_symbol_storage(sym)?;
        let id: u32 = *local_id;
        self.instructions.push(GetLocal(id));
        Ok(())
    }

    /// start a loop that leaves a value on the stack
    fn start_loop_with_return(&mut self, value_type: ValueType) {
        self.block_depth += 1;

        // self.instructions.push(Loop(BlockType::NoResult));
        self.instructions.push(Loop(BlockType::Value(value_type)));
    }

    fn start_block(&mut self) {
        self.block_depth += 1;

        // Our blocks always end with a `return` or `br`,
        // so they never leave extra values on the stack
        self.instructions.push(Block(BlockType::NoResult));
    }

    fn end_block(&mut self) {
        self.block_depth -= 1;
        self.instructions.push(End);
    }

    fn build_stmt(&mut self, stmt: &Stmt<'a>, ret_layout: &Layout<'a>) -> Result<(), String> {
        match stmt {
            // This pattern is a simple optimisation to get rid of one local and two instructions per proc.
            // If we are just returning the expression result, then don't SetLocal and immediately GetLocal
            Stmt::Let(let_sym, expr, layout, Stmt::Ret(ret_sym)) if let_sym == ret_sym => {
                self.build_expr(let_sym, expr, layout)?;
                self.instructions.push(Return);
                Ok(())
            }

            Stmt::Let(sym, expr, layout, following) => {
                let wasm_layout = WasmLayout::new(layout);
                let local_id = self.insert_local(wasm_layout, *sym);

                self.build_expr(sym, expr, layout)?;
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

            Stmt::Switch {
                cond_symbol,
                cond_layout: _,
                branches,
                default_branch,
                ret_layout: _,
            } => {
                // NOTE currently implemented as a series of conditional jumps
                // We may be able to improve this in the future with `Select`
                // or `BrTable`

                // create (number_of_branches - 1) new blocks.
                for _ in 0..branches.len() {
                    self.start_block()
                }

                // the LocalId of the symbol that we match on
                let matched_on = match self.symbol_storage_map.get(cond_symbol) {
                    Some(SymbolStorage(local_id, _)) => local_id.0,
                    None => unreachable!("symbol not defined: {:?}", cond_symbol),
                };

                // then, we jump whenever the value under scrutiny is equal to the value of a branch
                for (i, (value, _, _)) in branches.iter().enumerate() {
                    // put the cond_symbol on the top of the stack
                    self.instructions.push(GetLocal(matched_on));

                    self.instructions.push(I32Const(*value as i32));

                    // compare the 2 topmost values
                    self.instructions.push(I32Eq);

                    // "break" out of `i` surrounding blocks
                    self.instructions.push(BrIf(i as u32));
                }

                // if we never jumped because a value matched, we're in the default case
                self.build_stmt(default_branch.1, ret_layout)?;

                // now put in the actual body of each branch in order
                // (the first branch would have broken out of 1 block,
                // hence we must generate its code first)
                for (_, _, branch) in branches.iter() {
                    self.end_block();

                    self.build_stmt(branch, ret_layout)?;
                }

                Ok(())
            }
            Stmt::Join {
                id,
                parameters,
                body,
                remainder,
            } => {
                // make locals for join pointer parameters
                let mut jp_parameter_local_ids = std::vec::Vec::with_capacity(parameters.len());
                for parameter in parameters.iter() {
                    let wasm_layout = WasmLayout::new(&parameter.layout);
                    let local_id = self.insert_local(wasm_layout, parameter.symbol);

                    jp_parameter_local_ids.push(local_id);
                }

                self.start_block();

                self.joinpoint_label_map
                    .insert(*id, (self.block_depth, jp_parameter_local_ids));

                self.build_stmt(remainder, ret_layout)?;

                self.end_block();

                // A `return` inside of a `loop` seems to make it so that the `loop` itself
                // also "returns" (so, leaves on the stack) a value of the return type.
                let return_wasm_layout = WasmLayout::new(ret_layout);
                self.start_loop_with_return(return_wasm_layout.value_type());

                self.build_stmt(body, ret_layout)?;

                // ends the loop
                self.end_block();

                Ok(())
            }
            Stmt::Jump(id, arguments) => {
                let (target, locals) = &self.joinpoint_label_map[id];

                // put the arguments on the stack
                for (symbol, local_id) in arguments.iter().zip(locals.iter()) {
                    let argument = match self.symbol_storage_map.get(symbol) {
                        Some(SymbolStorage(local_id, _)) => local_id.0,
                        None => unreachable!("symbol not defined: {:?}", symbol),
                    };

                    self.instructions.push(GetLocal(argument));
                    self.instructions.push(SetLocal(local_id.0));
                }

                // jump
                let levels = self.block_depth - target;
                self.instructions.push(Br(levels));

                Ok(())
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
            Expr::Literal(lit) => self.load_literal(lit, layout),

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
                    Ok(())
                }

                CallType::LowLevel { op: lowlevel, .. } => {
                    self.build_call_low_level(lowlevel, arguments, layout)
                }
                x => Err(format!("the call type, {:?}, is not yet implemented", x)),
            },

            x => Err(format!("Expression is not yet implemented {:?}", x)),
        }
    }

    fn load_literal(&mut self, lit: &Literal<'a>, layout: &Layout<'a>) -> Result<(), String> {
        match lit {
            Literal::Bool(x) => {
                self.instructions.push(I32Const(*x as i32));
                Ok(())
            }
            Literal::Byte(x) => {
                self.instructions.push(I32Const(*x as i32));
                Ok(())
            }
            Literal::Int(x) => {
                let instruction = match layout {
                    Layout::Builtin(Builtin::Int64) => I64Const(*x as i64),
                    Layout::Builtin(
                        Builtin::Int32
                        | Builtin::Int16
                        | Builtin::Int8
                        | Builtin::Int1
                        | Builtin::Usize,
                    ) => I32Const(*x as i32),
                    x => panic!("loading literal, {:?}, is not yet implemented", x),
                };
                self.instructions.push(instruction);
                Ok(())
            }
            Literal::Float(x) => {
                let instruction = match layout {
                    Layout::Builtin(Builtin::Float64) => F64Const((*x as f64).to_bits()),
                    Layout::Builtin(Builtin::Float32) => F32Const((*x as f32).to_bits()),
                    x => panic!("loading literal, {:?}, is not yet implemented", x),
                };
                self.instructions.push(instruction);
                Ok(())
            }
            x => Err(format!("loading literal, {:?}, is not yet implemented", x)),
        }
    }

    fn build_call_low_level(
        &mut self,
        lowlevel: &LowLevel,
        args: &'a [Symbol],
        return_layout: &Layout<'a>,
    ) -> Result<(), String> {
        for arg in args {
            self.load_from_symbol(arg)?;
        }
        let wasm_layout = WasmLayout::new(return_layout);
        self.build_instructions_lowlevel(lowlevel, wasm_layout.value_type())?;
        Ok(())
    }

    fn build_instructions_lowlevel(
        &mut self,
        lowlevel: &LowLevel,
        return_value_type: ValueType,
    ) -> Result<(), String> {
        // TODO:  Find a way to organise all the lowlevel ops and layouts! There's lots!
        //
        // Some Roc low-level ops care about wrapping, clipping, sign-extending...
        // For those, we'll need to pre-process each argument before the main op,
        // so simple arrays of instructions won't work. But there are common patterns.
        let instructions: &[Instruction] = match lowlevel {
            // Wasm type might not be enough, may need to sign-extend i8 etc. Maybe in load_from_symbol?
            LowLevel::NumAdd => match return_value_type {
                ValueType::I32 => &[I32Add],
                ValueType::I64 => &[I64Add],
                ValueType::F32 => &[F32Add],
                ValueType::F64 => &[F64Add],
            },
            LowLevel::NumSub => match return_value_type {
                ValueType::I32 => &[I32Sub],
                ValueType::I64 => &[I64Sub],
                ValueType::F32 => &[F32Sub],
                ValueType::F64 => &[F64Sub],
            },
            LowLevel::NumMul => match return_value_type {
                ValueType::I32 => &[I32Mul],
                ValueType::I64 => &[I64Mul],
                ValueType::F32 => &[F32Mul],
                ValueType::F64 => &[F64Mul],
            },
            LowLevel::NumGt => {
                // needs layout of the argument to be implemented fully
                &[I32GtS]
            }
            _ => {
                return Err(format!("unsupported low-level op {:?}", lowlevel));
            }
        };
        self.instructions.extend_from_slice(instructions);
        Ok(())
    }
}
