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

use crate::code_builder::CodeBuilder;
use crate::layout::WasmLayout;
use crate::storage::{StackMemoryLocation, SymbolStorage};
use crate::{
    copy_memory, pop_stack_frame, push_stack_frame, round_up_to_alignment, CopyMemoryConfig,
    LocalId, ALIGN_1, ALIGN_2, ALIGN_4, ALIGN_8, PTR_SIZE, PTR_TYPE,
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
    instructions: CodeBuilder,
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
            instructions: CodeBuilder::new(),
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
        // println!("\ngenerating procedure {:?}\n", sym);

        let signature_builder = self.start_proc(&proc);

        self.build_stmt(&proc.body, &proc.ret_layout)?;

        let function_def = self.finalize_proc(signature_builder);
        let location = self.builder.push_function(function_def);
        let function_index = location.body;
        self.proc_symbol_map.insert(sym, location);
        self.reset();
        // println!("\nfinished generating {:?}\n", sym);

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
            self.create_storage(WasmLayout::new(layout), *symbol, LocalKind::Parameter);
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

        self.instructions.finalize_into(&mut final_instructions);

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

    fn get_next_local_id(&self) -> LocalId {
        LocalId((self.arg_types.len() + self.locals.len()) as u32)
    }

    fn create_storage(
        &mut self,
        wasm_layout: WasmLayout,
        symbol: Symbol,
        kind: LocalKind,
    ) -> Option<LocalId> {
        let next_local_id = self.get_next_local_id();

        let storage = match wasm_layout {
            WasmLayout::LocalOnly(value_type, size) => match kind {
                LocalKind::Parameter => SymbolStorage::Local {
                    local_id: next_local_id,
                    value_type,
                    size,
                },
                LocalKind::Variable => SymbolStorage::VirtualMachineStack {
                    vm_state: self.instructions.set_top_symbol(symbol),
                    value_type,
                    size,
                },
            },

            WasmLayout::HeapMemory => SymbolStorage::Local {
                local_id: next_local_id,
                value_type: PTR_TYPE,
                size: PTR_SIZE,
            },

            WasmLayout::StackMemory {
                size,
                alignment_bytes,
            } => {
                let location = match kind {
                    LocalKind::Parameter => StackMemoryLocation::PointerArg(next_local_id),

                    LocalKind::Variable => {
                        match self.stack_frame_pointer {
                            Some(_) => {}
                            None => {
                                self.stack_frame_pointer = Some(next_local_id);
                            }
                        };

                        let offset =
                            round_up_to_alignment(self.stack_memory, alignment_bytes as i32);

                        self.stack_memory = offset + size as i32;

                        StackMemoryLocation::FrameOffset(offset as u32)
                    }
                };

                SymbolStorage::StackMemory {
                    location,
                    size,
                    alignment_bytes,
                }
            }
        };

        let maybe_local_id = storage.local_id();

        match kind {
            LocalKind::Parameter => {
                self.arg_types.push(wasm_layout.value_type());
            }
            LocalKind::Variable => match maybe_local_id {
                Some(_) => {
                    self.locals.push(Local::new(1, wasm_layout.value_type()));
                }
                None => {}
            },
        }

        self.symbol_storage_map.insert(symbol, storage);

        maybe_local_id
    }

    fn get_symbol_storage(&self, sym: &Symbol) -> &SymbolStorage {
        self.symbol_storage_map.get(sym).unwrap_or_else(|| {
            panic!(
                "Symbol {:?} not found in function scope:\n{:?}",
                sym, self.symbol_storage_map
            )
        })
    }

    fn local_id_from_symbol(&self, sym: &Symbol) -> LocalId {
        let storage = self.get_symbol_storage(sym);
        match storage {
            SymbolStorage::Local { local_id, .. } => *local_id,
            _ => {
                panic!("{:?} does not have a local_id", sym);
            }
        }
    }

    /// Load a symbol, e.g. for passing to a function call
    fn load_symbol(&mut self, sym: &Symbol) {
        let storage = self.get_symbol_storage(sym).to_owned();
        match storage {
            SymbolStorage::VirtualMachineStack {
                vm_state,
                value_type,
                size,
            } => {
                let next_local_id = self.get_next_local_id();
                let maybe_next_vm_state =
                    self.instructions.load_symbol(*sym, vm_state, next_local_id);
                match maybe_next_vm_state {
                    // The act of loading the value changed the VM state, so update it
                    Some(next_vm_state) => {
                        self.symbol_storage_map.insert(
                            *sym,
                            SymbolStorage::VirtualMachineStack {
                                vm_state: next_vm_state,
                                value_type,
                                size,
                            },
                        );
                    }
                    None => {
                        // Loading the value required creating a new local, because
                        // it was not in a convenient position in the VM stack.
                        self.locals.push(Local::new(1, value_type));
                        self.symbol_storage_map.insert(
                            *sym,
                            SymbolStorage::Local {
                                local_id: next_local_id,
                                value_type,
                                size,
                            },
                        );
                    }
                }
            }
            SymbolStorage::Local { local_id, .. }
            | SymbolStorage::StackMemory {
                location: StackMemoryLocation::PointerArg(local_id),
                ..
            } => {
                self.instructions.push(GetLocal(local_id.0));
            }

            SymbolStorage::StackMemory {
                location: StackMemoryLocation::FrameOffset(offset),
                ..
            } => {
                self.instructions.extend(&[
                    GetLocal(self.stack_frame_pointer.unwrap().0),
                    I32Const(offset as i32),
                    I32Add,
                ]);
            }
        }
    }

    /// start a loop that leaves a value on the stack
    fn start_loop_with_return(&mut self, value_type: ValueType) {
        self.block_depth += 1;
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
                    let storage = SymbolStorage::StackMemory {
                        location: StackMemoryLocation::PointerArg(LocalId(0)),
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

                match wasm_layout {
                    WasmLayout::StackMemory { .. } => {
                        // If the expression writes to stack memory, allocate *before* generating
                        // so that know where to write it
                        self.create_storage(wasm_layout, *sym, LocalKind::Variable);
                        self.build_expr(sym, expr, layout)?;
                    }
                    _ => {
                        // If the expression produces a primitive, create storage *after* generating,
                        // because we don't know if we need a local until afterwards
                        // TODO: should we make this uniform by having a "not yet pushed" state in VirtualMachineSymbolState?
                        self.build_expr(sym, expr, layout)?;
                        self.create_storage(wasm_layout, *sym, LocalKind::Variable);
                    }
                }

                self.build_stmt(following, ret_layout)?;
                Ok(())
            }

            Stmt::Ret(sym) => {
                use crate::storage::SymbolStorage::*;

                let storage = self.symbol_storage_map.get(sym).unwrap();

                match storage {
                    StackMemory {
                        location,
                        size,
                        alignment_bytes,
                    } => {
                        let (from_ptr, from_offset) =
                            location.local_and_offset(self.stack_frame_pointer);
                        copy_memory(
                            &mut self.instructions,
                            CopyMemoryConfig {
                                from_ptr,
                                from_offset,
                                to_ptr: LocalId(0),
                                to_offset: 0,
                                size: *size,
                                alignment_bytes: *alignment_bytes,
                            },
                        );
                    }

                    _ => {
                        self.load_symbol(sym);
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
                let matched_on = self.local_id_from_symbol(cond_symbol);

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
                    let maybe_local_id =
                        self.create_storage(wasm_layout, parameter.symbol, LocalKind::Variable);
                    let jp_param_id = maybe_local_id.unwrap();
                    jp_parameter_local_ids.push(jp_param_id);
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
                    let argument = self.local_id_from_symbol(symbol);
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
                        self.load_symbol(arg);
                    }
                    let function_location = self.proc_symbol_map.get(func_sym).ok_or(format!(
                        "Cannot find function {:?} called from {:?}",
                        func_sym, sym
                    ))?;

                    // TODO: Recreating the same WasmLayout as in the Let, for Backend compatibility
                    let wasm_layout = WasmLayout::new(layout);
                    let push = wasm_layout.stack_memory() == 0;
                    let pops = arguments.len();
                    self.instructions.call(function_location.body, pops, push);
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
        // TODO: we just calculated storage and now we're getting it out of a map
        // Not passing it as an argument because I'm trying to match Backend method signatures
        let storage = self.get_symbol_storage(sym).to_owned();

        if let Layout::Struct(field_layouts) = layout {
            match storage {
                SymbolStorage::StackMemory { location, size, .. } => {
                    if size > 0 {
                        let (local_id, struct_offset) =
                            location.local_and_offset(self.stack_frame_pointer);
                        let mut field_offset = struct_offset;
                        for (field, _) in fields.iter().zip(field_layouts.iter()) {
                            field_offset +=
                                self.copy_symbol_to_memory(local_id, field_offset, *field);
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
            };
        } else {
            // Struct expression but not Struct layout => single element. Copy it.
            let field_storage = self.get_symbol_storage(&fields[0]).to_owned();
            storage.copy_from(
                &field_storage,
                &mut self.instructions,
                self.stack_frame_pointer,
            );
        }
        Ok(())
    }

    fn copy_symbol_to_memory(
        &mut self,
        to_ptr: LocalId,
        to_offset: u32,
        from_symbol: Symbol,
    ) -> u32 {
        let from_storage = self.get_symbol_storage(&from_symbol).to_owned();
        match from_storage {
            SymbolStorage::StackMemory {
                location,
                size,
                alignment_bytes,
            } => {
                let (from_ptr, from_offset) = location.local_and_offset(self.stack_frame_pointer);
                copy_memory(
                    &mut self.instructions,
                    CopyMemoryConfig {
                        from_ptr,
                        from_offset,
                        to_ptr,
                        to_offset,
                        size,
                        alignment_bytes,
                    },
                );
                size
            }

            SymbolStorage::VirtualMachineStack {
                value_type, size, ..
            }
            | SymbolStorage::Local {
                value_type, size, ..
            } => {
                let store_instruction = match (value_type, size) {
                    (ValueType::I64, 8) => I64Store(ALIGN_8, to_offset),
                    (ValueType::I32, 4) => I32Store(ALIGN_4, to_offset),
                    (ValueType::I32, 2) => I32Store16(ALIGN_2, to_offset),
                    (ValueType::I32, 1) => I32Store8(ALIGN_1, to_offset),
                    (ValueType::F32, 4) => F32Store(ALIGN_4, to_offset),
                    (ValueType::F64, 8) => F64Store(ALIGN_8, to_offset),
                    _ => {
                        panic!("Cannot store {:?} with alignment of {:?}", value_type, size);
                    }
                };
                self.instructions.push(GetLocal(to_ptr.0));
                self.load_symbol(&from_symbol);
                self.instructions.push(store_instruction);
                size
            }
        }
    }

    fn build_call_low_level(
        &mut self,
        lowlevel: &LowLevel,
        args: &'a [Symbol],
        return_layout: &Layout<'a>,
    ) -> Result<(), String> {
        for arg in args {
            self.load_symbol(arg);
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
        self.instructions.extend(instructions);
        Ok(())
    }
}
