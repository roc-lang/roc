use bumpalo::collections::Vec;

use code_builder::Align;
use roc_collections::all::MutMap;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_mono::ir::{CallType, Expr, JoinPointId, Literal, Proc, Stmt};
use roc_mono::layout::Layout;

use crate::layout::WasmLayout;
use crate::storage::{Storage, StoredValue, StoredValueKind};
use crate::wasm_module::linking::{LinkingSection, RelocationSection};
use crate::wasm_module::sections::{
    CodeSection, DataMode, DataSection, DataSegment, ExportSection, FunctionSection, GlobalSection,
    ImportSection, MemorySection, TypeSection, WasmModule,
};
use crate::wasm_module::{
    code_builder, BlockType, CodeBuilder, ConstExpr, Export, ExportType, Global, GlobalType,
    LocalId, Signature, ValueType,
};
use crate::{copy_memory, CopyMemoryConfig, Env, PTR_TYPE};

// Don't allocate any constant data at address zero or near it. Would be valid, but bug-prone.
// Follow Emscripten's example by using 1kB (4 bytes would probably do)
const UNUSED_DATA_SECTION_BYTES: u32 = 1024;

pub struct WasmBackend<'a> {
    env: &'a Env<'a>,

    // Module-level data
    pub module: WasmModule<'a>,
    next_literal_addr: u32,
    proc_symbols: Vec<'a, Symbol>,

    // Function-level data
    code_builder: CodeBuilder<'a>,
    storage: Storage<'a>,

    /// how many blocks deep are we (used for jumps)
    block_depth: u32,
    joinpoint_label_map: MutMap<JoinPointId, (u32, Vec<'a, StoredValue>)>,
}

impl<'a> WasmBackend<'a> {
    pub fn new(env: &'a Env<'a>, proc_symbols: Vec<'a, Symbol>) -> Self {
        const MEMORY_INIT_SIZE: u32 = 1024 * 1024;

        let mut module = WasmModule {
            types: TypeSection::new(env.arena),
            import: ImportSection::new(env.arena),
            function: FunctionSection::new(env.arena),
            table: (), // Unused in Roc (mainly for function pointers)
            memory: MemorySection::new(MEMORY_INIT_SIZE),
            global: GlobalSection::new(env.arena),
            export: ExportSection::new(env.arena),
            start: (),   // Entry function. In Roc this would be part of the platform.
            element: (), // Unused in Roc (related to table section)
            code: CodeSection::new(env.arena),
            data: DataSection::new(env.arena),
            linking: LinkingSection::new(env.arena),
            reloc_code: RelocationSection::new(env.arena, "reloc.CODE"),
            reloc_data: RelocationSection::new(env.arena, "reloc.DATA"),
        };

        module.export.entries.push(Export {
            name: "memory".to_string(),
            ty: ExportType::Mem,
            index: 0,
        });

        let stack_pointer_global = Global {
            ty: GlobalType {
                value_type: ValueType::I32,
                is_mutable: true,
            },
            init: ConstExpr::I32(MEMORY_INIT_SIZE as i32),
        };
        module.global.entries.push(stack_pointer_global);

        let literal_segment = DataSegment {
            mode: DataMode::Active {
                offset: ConstExpr::I32(UNUSED_DATA_SECTION_BYTES as i32),
            },
            init: Vec::with_capacity_in(64, env.arena),
        };
        module.data.segments.push(literal_segment);

        WasmBackend {
            env,

            // Module-level data
            module,
            next_literal_addr: UNUSED_DATA_SECTION_BYTES,
            proc_symbols,

            // Function-level data
            block_depth: 0,
            joinpoint_label_map: MutMap::default(),
            code_builder: CodeBuilder::new(env.arena),
            storage: Storage::new(env.arena),
        }
    }

    /// Reset function-level data
    fn reset(&mut self) {
        // Push the completed CodeBuilder into the module and swap it for a new empty one
        let mut swap_code_builder = CodeBuilder::new(self.env.arena);
        std::mem::swap(&mut swap_code_builder, &mut self.code_builder);
        self.module.code.code_builders.push(swap_code_builder);

        self.storage.clear();
        self.joinpoint_label_map.clear();
        assert_eq!(self.block_depth, 0);
    }

    /**********************************************************

            PROCEDURE

    ***********************************************************/

    pub fn build_proc(&mut self, proc: Proc<'a>, _sym: Symbol) -> Result<(), String> {
        // println!("\ngenerating procedure {:?}\n", _sym);

        self.start_proc(&proc);

        self.build_stmt(&proc.body, &proc.ret_layout)?;

        self.finalize_proc()?;
        self.reset();

        // println!("\nfinished generating {:?}\n", _sym);

        Ok(())
    }

    fn start_proc(&mut self, proc: &Proc<'a>) {
        let ret_layout = WasmLayout::new(&proc.ret_layout);
        let ret_type = if ret_layout.is_stack_memory() {
            self.storage.arg_types.push(PTR_TYPE);
            self.start_block(BlockType::NoResult); // block to ensure all paths pop stack memory (if any)
            None
        } else {
            let ty = ret_layout.value_type();
            self.start_block(BlockType::Value(ty)); // block to ensure all paths pop stack memory (if any)
            Some(ty)
        };

        for (layout, symbol) in proc.args {
            let arg_layout = WasmLayout::new(layout);
            self.storage
                .allocate(&arg_layout, *symbol, StoredValueKind::Parameter);
        }

        self.module.add_function_signature(Signature {
            param_types: self.storage.arg_types.clone(),
            ret_type,
        });
    }

    fn finalize_proc(&mut self) -> Result<(), String> {
        // end the block from start_proc, to ensure all paths pop stack memory (if any)
        self.end_block();

        // Write local declarations and stack frame push/pop code
        self.code_builder.finalize(
            &self.storage.local_types,
            self.storage.stack_frame_size,
            self.storage.stack_frame_pointer,
        );

        Ok(())
    }

    /**********************************************************

            STATEMENTS

    ***********************************************************/

    /// start a loop that leaves a value on the stack
    fn start_loop_with_return(&mut self, value_type: ValueType) {
        self.block_depth += 1;
        self.code_builder.loop_(BlockType::Value(value_type));
    }

    fn start_block(&mut self, block_type: BlockType) {
        self.block_depth += 1;
        self.code_builder.block(block_type);
    }

    fn end_block(&mut self) {
        self.block_depth -= 1;
        self.code_builder.end();
    }

    fn build_stmt(&mut self, stmt: &Stmt<'a>, ret_layout: &Layout<'a>) -> Result<(), String> {
        match stmt {
            Stmt::Let(sym, expr, layout, following) => {
                let wasm_layout = WasmLayout::new(layout);

                let kind = match following {
                    Stmt::Ret(ret_sym) if *sym == *ret_sym => StoredValueKind::ReturnValue,
                    _ => StoredValueKind::Variable,
                };

                let sym_storage = self.storage.allocate(&wasm_layout, *sym, kind);

                self.build_expr(sym, expr, layout, &sym_storage)?;

                // For primitives, we record that this symbol is at the top of the VM stack
                // (For other values, we wrote to memory and there's nothing on the VM stack)
                if let WasmLayout::Primitive(value_type, size) = wasm_layout {
                    let vm_state = self.code_builder.set_top_symbol(*sym);
                    self.storage.symbol_storage_map.insert(
                        *sym,
                        StoredValue::VirtualMachineStack {
                            vm_state,
                            value_type,
                            size,
                        },
                    );
                }

                self.build_stmt(following, ret_layout)?;
                Ok(())
            }

            Stmt::Ret(sym) => {
                use crate::storage::StoredValue::*;

                let storage = self.storage.symbol_storage_map.get(sym).unwrap();

                match storage {
                    StackMemory {
                        location,
                        size,
                        alignment_bytes,
                    } => {
                        let (from_ptr, from_offset) =
                            location.local_and_offset(self.storage.stack_frame_pointer);
                        copy_memory(
                            &mut self.code_builder,
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
                        self.storage.load_symbols(&mut self.code_builder, &[*sym]);
                        self.code_builder.br(self.block_depth); // jump to end of function (for stack frame pop)
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

                // Ensure the condition value is not stored only in the VM stack
                // Otherwise we can't reach it from inside the block
                let cond_storage = self.storage.get(cond_symbol).to_owned();
                self.storage.ensure_value_has_local(
                    &mut self.code_builder,
                    *cond_symbol,
                    cond_storage,
                );

                // create (number_of_branches - 1) new blocks.
                for _ in 0..branches.len() {
                    self.start_block(BlockType::NoResult)
                }

                // then, we jump whenever the value under scrutiny is equal to the value of a branch
                for (i, (value, _, _)) in branches.iter().enumerate() {
                    // put the cond_symbol on the top of the stack
                    self.storage
                        .load_symbols(&mut self.code_builder, &[*cond_symbol]);

                    self.code_builder.i32_const(*value as i32);

                    // compare the 2 topmost values
                    self.code_builder.i32_eq();

                    // "break" out of `i` surrounding blocks
                    self.code_builder.br_if(i as u32);
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
                let mut jp_param_storages = Vec::with_capacity_in(parameters.len(), self.env.arena);
                for parameter in parameters.iter() {
                    let wasm_layout = WasmLayout::new(&parameter.layout);
                    let mut param_storage = self.storage.allocate(
                        &wasm_layout,
                        parameter.symbol,
                        StoredValueKind::Variable,
                    );
                    param_storage = self.storage.ensure_value_has_local(
                        &mut self.code_builder,
                        parameter.symbol,
                        param_storage,
                    );
                    jp_param_storages.push(param_storage);
                }

                self.start_block(BlockType::NoResult);

                self.joinpoint_label_map
                    .insert(*id, (self.block_depth, jp_param_storages));

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
                let (target, param_storages) = self.joinpoint_label_map[id].clone();

                for (arg_symbol, param_storage) in arguments.iter().zip(param_storages.iter()) {
                    let arg_storage = self.storage.get(arg_symbol).clone();
                    self.storage.clone_value(
                        &mut self.code_builder,
                        param_storage,
                        &arg_storage,
                        *arg_symbol,
                    );
                }

                // jump
                let levels = self.block_depth - target;
                self.code_builder.br(levels);

                Ok(())
            }
            x => Err(format!("statement not yet implemented: {:?}", x)),
        }
    }

    /**********************************************************

            EXPRESSIONS

    ***********************************************************/

    fn build_expr(
        &mut self,
        sym: &Symbol,
        expr: &Expr<'a>,
        layout: &Layout<'a>,
        storage: &StoredValue,
    ) -> Result<(), String> {
        let wasm_layout = WasmLayout::new(layout);
        match expr {
            Expr::Literal(lit) => self.load_literal(lit, storage),

            Expr::Call(roc_mono::ir::Call {
                call_type,
                arguments,
            }) => match call_type {
                CallType::ByName { name: func_sym, .. } => {
                    // TODO: See if we can make this more efficient
                    // Recreating the same WasmLayout again, rather than passing it down,
                    // to match signature of Backend::build_expr

                    let mut wasm_args_tmp: Vec<Symbol>;
                    let (wasm_args, has_return_val) = match wasm_layout {
                        WasmLayout::StackMemory { .. } => {
                            wasm_args_tmp =
                                Vec::with_capacity_in(arguments.len() + 1, self.env.arena);
                            wasm_args_tmp.push(*sym);
                            wasm_args_tmp.extend_from_slice(*arguments);
                            (wasm_args_tmp.as_slice(), false)
                        }
                        _ => (*arguments, true),
                    };

                    self.storage.load_symbols(&mut self.code_builder, wasm_args);

                    // Index of the called function in the code section
                    // TODO: account for inlined functions when we start doing that (remember we emit procs out of order)
                    let func_index = match self.proc_symbols.iter().position(|s| s == func_sym) {
                        Some(i) => i as u32,
                        None => {
                            // TODO: actually useful linking! Push a relocation for it.
                            return Err(format!(
                                "Not yet supporteed: calling foreign function {:?}",
                                func_sym
                            ));
                        }
                    };

                    // Index of the function's name in the symbol table
                    let symbol_index = func_index; // TODO: update this when we add other things to the symbol table

                    self.code_builder.call(
                        func_index,
                        symbol_index,
                        wasm_args.len(),
                        has_return_val,
                    );

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

    fn load_literal(&mut self, lit: &Literal<'a>, storage: &StoredValue) -> Result<(), String> {
        let not_supported_error = || Err(format!("Literal value {:?} is not yet implemented", lit));

        match storage {
            StoredValue::VirtualMachineStack { value_type, .. } => {
                match (lit, value_type) {
                    (Literal::Float(x), ValueType::F64) => self.code_builder.f64_const(*x as f64),
                    (Literal::Float(x), ValueType::F32) => self.code_builder.f32_const(*x as f32),
                    (Literal::Int(x), ValueType::I64) => self.code_builder.i64_const(*x as i64),
                    (Literal::Int(x), ValueType::I32) => self.code_builder.i32_const(*x as i32),
                    (Literal::Bool(x), ValueType::I32) => self.code_builder.i32_const(*x as i32),
                    (Literal::Byte(x), ValueType::I32) => self.code_builder.i32_const(*x as i32),
                    _ => {
                        return not_supported_error();
                    }
                };
            }

            StoredValue::StackMemory { location, size, .. } => match lit {
                Literal::Str(s) => {
                    // Small string is 8 bytes in Wasm
                    debug_assert!(*size == 8);
                    let len = s.len();
                    if len < 8 {
                        // A small string fits in an i64, so let's transform it to one.
                        let mut bytes = [0; 8];
                        bytes[0..len].clone_from_slice(s.as_bytes());
                        bytes[7] = 0x80 | (len as u8);
                        let str_as_int = i64::from_le_bytes(bytes);

                        // Store it to the allocated stack memory location
                        let (local_id, offset) =
                            location.local_and_offset(self.storage.stack_frame_pointer);
                        self.code_builder.get_local(local_id);
                        self.code_builder.i32_const(offset as i32);
                        self.code_builder.i32_add();
                        self.code_builder.i64_const(str_as_int);
                        self.code_builder.i64_store(Align::Bytes4, offset);
                    } else {
                        return not_supported_error();
                    }
                }
                _ => {
                    return not_supported_error();
                }
            },

            _ => {
                return not_supported_error();
            }
        };
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
        let storage = self.storage.get(sym).to_owned();

        if let Layout::Struct(field_layouts) = layout {
            match storage {
                StoredValue::StackMemory { location, size, .. } => {
                    if size > 0 {
                        let (local_id, struct_offset) =
                            location.local_and_offset(self.storage.stack_frame_pointer);
                        let mut field_offset = struct_offset;
                        for (field, _) in fields.iter().zip(field_layouts.iter()) {
                            field_offset += self.storage.copy_value_to_memory(
                                &mut self.code_builder,
                                local_id,
                                field_offset,
                                *field,
                            );
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
            let field_storage = self.storage.get(&fields[0]).to_owned();
            self.storage
                .clone_value(&mut self.code_builder, &storage, &field_storage, fields[0]);
        }
        Ok(())
    }

    fn build_call_low_level(
        &mut self,
        lowlevel: &LowLevel,
        args: &'a [Symbol],
        return_layout: &Layout<'a>,
    ) -> Result<(), String> {
        self.storage.load_symbols(&mut self.code_builder, args);
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
        match lowlevel {
            LowLevel::NumAdd => match return_value_type {
                ValueType::I32 => self.code_builder.i32_add(),
                ValueType::I64 => self.code_builder.i64_add(),
                ValueType::F32 => self.code_builder.f32_add(),
                ValueType::F64 => self.code_builder.f64_add(),
            },
            LowLevel::NumSub => match return_value_type {
                ValueType::I32 => self.code_builder.i32_sub(),
                ValueType::I64 => self.code_builder.i64_sub(),
                ValueType::F32 => self.code_builder.f32_sub(),
                ValueType::F64 => self.code_builder.f64_sub(),
            },
            LowLevel::NumMul => match return_value_type {
                ValueType::I32 => self.code_builder.i32_mul(),
                ValueType::I64 => self.code_builder.i64_mul(),
                ValueType::F32 => self.code_builder.f32_mul(),
                ValueType::F64 => self.code_builder.f64_mul(),
            },
            LowLevel::NumGt => {
                // needs layout of the argument to be implemented fully
                self.code_builder.i32_gt_s()
            }
            _ => {
                return Err(format!("unsupported low-level op {:?}", lowlevel));
            }
        };
        Ok(())
    }
}
