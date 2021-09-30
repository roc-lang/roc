use parity_wasm::builder;
use parity_wasm::builder::{CodeLocation, FunctionDefinition, ModuleBuilder, SignatureBuilder};
use parity_wasm::elements::{
    BlockType, Instruction, Instruction::*, Instructions, Local, ValueType,
};

use roc_collections::all::MutMap;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_mono::ir::{CallType, Expr, JoinPointId, Literal, Proc, Stmt};
use roc_mono::layout::{Builtin, Layout};

use crate::layout::WasmLayout;
use crate::storage::SymbolStorage;
use crate::{
    copy_memory, pop_stack_frame, push_stack_frame, round_up_to_alignment, LocalId, PTR_SIZE,
    PTR_TYPE,
};

// Don't allocate any constant data at address zero or near it. Would be valid, but bug-prone.
// Follow Emscripten's example by using 1kB (4 bytes would probably do)
const UNUSED_DATA_SECTION_BYTES: u32 = 1024;

#[derive(Clone, Copy, Debug)]
struct LabelId(u32);

enum LocalKind {
    Parameter,
    Variable,
}

// TODO: use Bumpalo Vec once parity_wasm supports general iterators (>=0.43)
pub struct WasmBackend<'a> {
    // Module: Wasm AST
    pub builder: ModuleBuilder,

    // Module: internal state & IR mappings
    _data_offset_map: MutMap<Literal<'a>, u32>,
    _data_offset_next: u32,
    proc_symbol_map: MutMap<Symbol, CodeLocation>,

    // Functions: Wasm AST
    instructions: std::vec::Vec<Instruction>,
    arg_types: std::vec::Vec<ValueType>,
    locals: std::vec::Vec<Local>,

    // Functions: internal state & IR mappings
    stack_memory: i32,
    stack_frame_pointer: Option<LocalId>,
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
            arg_types: std::vec::Vec::with_capacity(8),
            locals: std::vec::Vec::with_capacity(32),

            // Functions: internal state & IR mappings
            stack_memory: 0,
            stack_frame_pointer: None,
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
        self.stack_frame_pointer = None;
        self.symbol_storage_map.clear();
        self.joinpoint_label_map.clear();
        assert_eq!(self.block_depth, 0);
    }

    pub fn build_proc(&mut self, proc: Proc<'a>, sym: Symbol) -> Result<u32, String> {
        let signature_builder = self.start_proc(&proc);

        self.build_stmt(&proc.body, &proc.ret_layout)?;

        let function_def = self.finalize_proc(signature_builder);
        let location = self.builder.push_function(function_def);
        let function_index = location.body;
        self.proc_symbol_map.insert(sym, location);
        self.reset();

        Ok(function_index)
    }

    fn start_proc(&mut self, proc: &Proc<'a>) -> SignatureBuilder {
        let ret_layout = WasmLayout::new(&proc.ret_layout);

        let signature_builder = if let WasmLayout::StackMemory { .. } = ret_layout {
            self.arg_types.push(PTR_TYPE);
            self.start_block(BlockType::NoResult); // block to ensure all paths pop stack memory (if any)
            builder::signature()
        } else {
            let ret_type = ret_layout.value_type();
            self.start_block(BlockType::Value(ret_type)); // block to ensure all paths pop stack memory (if any)
            builder::signature().with_result(ret_type)
        };

        for (layout, symbol) in proc.args {
            self.insert_local(WasmLayout::new(layout), *symbol, LocalKind::Parameter);
        }

        signature_builder.with_params(self.arg_types.clone())
    }

    fn finalize_proc(&mut self, signature_builder: SignatureBuilder) -> FunctionDefinition {
        self.end_block(); // end the block from start_proc, to ensure all paths pop stack memory (if any)

        let mut final_instructions = Vec::with_capacity(self.instructions.len() + 10);

        if self.stack_memory > 0 {
            push_stack_frame(
                &mut final_instructions,
                self.stack_memory,
                self.stack_frame_pointer.unwrap(),
            );
        }

        final_instructions.extend(self.instructions.drain(0..));

        if self.stack_memory > 0 {
            pop_stack_frame(
                &mut final_instructions,
                self.stack_memory,
                self.stack_frame_pointer.unwrap(),
            );
        }
        final_instructions.push(End);

        builder::function()
            .with_signature(signature_builder.build_sig())
            .body()
            .with_locals(self.locals.clone())
            .with_instructions(Instructions::new(final_instructions))
            .build() // body
            .build() // function
    }

    fn insert_local(
        &mut self,
        wasm_layout: WasmLayout,
        symbol: Symbol,
        kind: LocalKind,
    ) -> SymbolStorage {
        let local_id = LocalId((self.arg_types.len() + self.locals.len()) as u32);

        let storage = match kind {
            LocalKind::Parameter => {
                // Already stack-allocated by the caller if needed.
                self.arg_types.push(wasm_layout.value_type());
                match wasm_layout {
                    WasmLayout::LocalOnly(value_type, size) => SymbolStorage::ParamPrimitive {
                        local_id,
                        value_type,
                        size,
                    },

                    WasmLayout::HeapMemory => SymbolStorage::ParamPrimitive {
                        local_id,
                        value_type: PTR_TYPE,
                        size: PTR_SIZE,
                    },

                    WasmLayout::StackMemory {
                        size,
                        alignment_bytes,
                    } => SymbolStorage::ParamStackMemory {
                        local_id,
                        size,
                        alignment_bytes,
                    },
                }
            }
            LocalKind::Variable => {
                self.locals.push(Local::new(1, wasm_layout.value_type()));

                match wasm_layout {
                    WasmLayout::LocalOnly(value_type, size) => SymbolStorage::VarPrimitive {
                        local_id,
                        value_type,
                        size,
                    },

                    WasmLayout::HeapMemory => SymbolStorage::VarHeapMemory { local_id },

                    WasmLayout::StackMemory {
                        size,
                        alignment_bytes,
                    } => {
                        let offset =
                            round_up_to_alignment(self.stack_memory, alignment_bytes as i32);
                        self.stack_memory = offset + size as i32;

                        match self.stack_frame_pointer {
                            None => {
                                // This is the first stack-memory variable in the function
                                // That means we can reuse it as the stack frame pointer,
                                // and it will get initialised at the start of the function
                                self.stack_frame_pointer = Some(local_id);
                            }

                            Some(frame_ptr_id) => {
                                // This local points to the base of a struct, at an offset from the stack frame pointer
                                // Having one local per variable means params and locals work the same way in code gen.
                                // (alternatively we could use one frame pointer + offset for all struct variables)
                                self.instructions.extend([
                                    GetLocal(frame_ptr_id.0),
                                    I32Const(offset),
                                    I32Add,
                                    SetLocal(local_id.0),
                                ]);
                            }
                        };

                        SymbolStorage::VarStackMemory {
                            local_id,
                            size,
                            offset: offset as u32,
                            alignment_bytes,
                        }
                    }
                }
            }
        };

        self.symbol_storage_map.insert(symbol, storage.clone());

        storage
    }

    fn get_symbol_storage(&self, sym: &Symbol) -> Result<&SymbolStorage, String> {
        self.symbol_storage_map.get(sym).ok_or_else(|| {
            format!(
                "Symbol {:?} not found in function scope:\n{:?}",
                sym, self.symbol_storage_map
            )
        })
    }

    fn local_id_from_symbol(&self, sym: &Symbol) -> Result<LocalId, String> {
        let storage = self.get_symbol_storage(sym)?;
        Ok(storage.local_id())
    }

    fn load_symbol(&mut self, sym: &Symbol) -> Result<(), String> {
        let storage = self.get_symbol_storage(sym)?;
        let index: u32 = storage.local_id().0;
        self.instructions.push(GetLocal(index));
        Ok(())
    }

    /// start a loop that leaves a value on the stack
    fn start_loop_with_return(&mut self, value_type: ValueType) {
        self.block_depth += 1;

        // self.instructions.push(Loop(BlockType::NoResult));
        self.instructions.push(Loop(BlockType::Value(value_type)));
    }

    fn start_block(&mut self, block_type: BlockType) {
        self.block_depth += 1;
        self.instructions.push(Block(block_type));
    }

    fn end_block(&mut self) {
        self.block_depth -= 1;
        self.instructions.push(End);
    }

    fn build_stmt(&mut self, stmt: &Stmt<'a>, ret_layout: &Layout<'a>) -> Result<(), String> {
        match stmt {
            // Simple optimisation: if we are just returning the expression, we don't need a local
            Stmt::Let(let_sym, expr, layout, Stmt::Ret(ret_sym)) if let_sym == ret_sym => {
                let wasm_layout = WasmLayout::new(layout);
                if let WasmLayout::StackMemory {
                    size,
                    alignment_bytes,
                } = wasm_layout
                {
                    // Map this symbol to the first argument (pointer into caller's stack)
                    // Saves us from having to copy it later
                    let storage = SymbolStorage::ParamStackMemory {
                        local_id: LocalId(0),
                        size,
                        alignment_bytes,
                    };
                    self.symbol_storage_map.insert(*let_sym, storage);
                }
                self.build_expr(let_sym, expr, layout)?;
                self.instructions.push(Br(self.block_depth)); // jump to end of function (stack frame pop)
                Ok(())
            }

            Stmt::Let(sym, expr, layout, following) => {
                let wasm_layout = WasmLayout::new(layout);
                let local_id = self
                    .insert_local(wasm_layout, *sym, LocalKind::Variable)
                    .local_id();

                self.build_expr(sym, expr, layout)?;

                // If this local is shared with the stack frame pointer, it's already assigned
                match self.stack_frame_pointer {
                    Some(sfp) if sfp == local_id => {}
                    _ => self.instructions.push(SetLocal(local_id.0)),
                }

                self.build_stmt(following, ret_layout)?;
                Ok(())
            }

            Stmt::Ret(sym) => {
                use crate::storage::SymbolStorage::*;

                let storage = self.symbol_storage_map.get(sym).unwrap();

                match storage {
                    VarStackMemory {
                        local_id,
                        size,
                        alignment_bytes,
                        ..
                    }
                    | ParamStackMemory {
                        local_id,
                        size,
                        alignment_bytes,
                    } => {
                        let from = *local_id;
                        let to = LocalId(0);
                        copy_memory(&mut self.instructions, from, to, *size, *alignment_bytes, 0)?;
                    }

                    ParamPrimitive { local_id, .. }
                    | VarPrimitive { local_id, .. }
                    | VarHeapMemory { local_id, .. } => {
                        self.instructions.push(GetLocal(local_id.0));
                        self.instructions.push(Br(self.block_depth)); // jump to end of function (for stack frame pop)
                    }
                }

                Ok(())
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
                    self.start_block(BlockType::NoResult)
                }

                // the LocalId of the symbol that we match on
                let matched_on = self.local_id_from_symbol(cond_symbol)?;

                // then, we jump whenever the value under scrutiny is equal to the value of a branch
                for (i, (value, _, _)) in branches.iter().enumerate() {
                    // put the cond_symbol on the top of the stack
                    self.instructions.push(GetLocal(matched_on.0));

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
                    let local_id = self
                        .insert_local(wasm_layout, parameter.symbol, LocalKind::Variable)
                        .local_id();

                    jp_parameter_local_ids.push(local_id);
                }

                self.start_block(BlockType::NoResult);

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
                    let argument = self.local_id_from_symbol(symbol)?;
                    self.instructions.push(GetLocal(argument.0));
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
                        self.load_symbol(arg)?;
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

            Expr::Struct(fields) => self.create_struct(sym, layout, fields),

            x => Err(format!("Expression is not yet implemented {:?}", x)),
        }
    }

    fn load_literal(&mut self, lit: &Literal<'a>, layout: &Layout<'a>) -> Result<(), String> {
        let instruction = match lit {
            Literal::Bool(x) => I32Const(*x as i32),
            Literal::Byte(x) => I32Const(*x as i32),
            Literal::Int(x) => match layout {
                Layout::Builtin(Builtin::Int64) => I64Const(*x as i64),
                Layout::Builtin(
                    Builtin::Int32
                    | Builtin::Int16
                    | Builtin::Int8
                    | Builtin::Int1
                    | Builtin::Usize,
                ) => I32Const(*x as i32),
                x => {
                    return Err(format!("loading literal, {:?}, is not yet implemented", x));
                }
            },
            Literal::Float(x) => match layout {
                Layout::Builtin(Builtin::Float64) => F64Const((*x as f64).to_bits()),
                Layout::Builtin(Builtin::Float32) => F32Const((*x as f32).to_bits()),
                x => {
                    return Err(format!("loading literal, {:?}, is not yet implemented", x));
                }
            },
            x => {
                return Err(format!("loading literal, {:?}, is not yet implemented", x));
            }
        };
        self.instructions.push(instruction);
        Ok(())
    }

    fn create_struct(
        &mut self,
        sym: &Symbol,
        layout: &Layout<'a>,
        fields: &'a [Symbol],
    ) -> Result<(), String> {
        let storage = self.get_symbol_storage(sym)?.to_owned();

        if let Layout::Struct(field_layouts) = layout {
            match storage {
                SymbolStorage::VarStackMemory { local_id, size, .. }
                | SymbolStorage::ParamStackMemory { local_id, size, .. } => {
                    if size > 0 {
                        let mut relative_offset = 0;
                        for (field, _) in fields.iter().zip(field_layouts.iter()) {
                            relative_offset += self.copy_symbol_to_pointer_at_offset(
                                local_id,
                                relative_offset,
                                field,
                            )?;
                        }
                    } else {
                        return Err(format!("Not supported yet: zero-size struct at {:?}", sym));
                    }
                }
                _ => {
                    return Err(format!(
                        "Cannot create struct {:?} with storage {:?}",
                        sym, storage
                    ));
                }
            }
        } else {
            // Struct expression but not Struct layout => single element. Copy it.
            let field_storage = self.get_symbol_storage(&fields[0])?.to_owned();
            self.copy_storage(&storage, &field_storage)?;
        }
        Ok(())
    }

    fn copy_symbol_to_pointer_at_offset(
        &mut self,
        to_ptr: LocalId,
        to_offset: u32,
        from_symbol: &Symbol,
    ) -> Result<u32, String> {
        let from_storage = self.get_symbol_storage(from_symbol)?.to_owned();
        from_storage.copy_to_memory(&mut self.instructions, to_ptr, to_offset)
    }

    fn copy_storage(&mut self, to: &SymbolStorage, from: &SymbolStorage) -> Result<(), String> {
        let has_stack_memory = to.has_stack_memory();
        debug_assert!(from.has_stack_memory() == has_stack_memory);

        if !has_stack_memory {
            debug_assert!(from.value_type() == to.value_type());
            self.instructions.push(GetLocal(from.local_id().0));
            self.instructions.push(SetLocal(to.local_id().0));
            Ok(())
        } else {
            let (size, alignment_bytes) = from.stack_size_and_alignment();
            copy_memory(
                &mut self.instructions,
                from.local_id(),
                to.local_id(),
                size,
                alignment_bytes,
                0,
            )
        }
    }

    fn build_call_low_level(
        &mut self,
        lowlevel: &LowLevel,
        args: &'a [Symbol],
        return_layout: &Layout<'a>,
    ) -> Result<(), String> {
        for arg in args {
            self.load_symbol(arg)?;
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
            // Wasm type might not be enough, may need to sign-extend i8 etc. Maybe in load_symbol?
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
