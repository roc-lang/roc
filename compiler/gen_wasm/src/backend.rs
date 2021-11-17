use bumpalo::{self, collections::Vec};

use code_builder::Align;
use roc_collections::all::MutMap;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_mono::ir::{CallType, Expr, JoinPointId, Literal, Proc, Stmt};
use roc_mono::layout::{Builtin, Layout, LayoutIds};

use crate::layout::WasmLayout;
use crate::low_level::{decode_low_level, LowlevelBuildResult};
use crate::storage::{Storage, StoredValue, StoredValueKind};
use crate::wasm_module::linking::{
    DataSymbol, LinkingSection, RelocationSection, WasmObjectSymbol, WASM_SYM_BINDING_WEAK,
    WASM_SYM_UNDEFINED,
};
use crate::wasm_module::sections::{
    CodeSection, DataMode, DataSection, DataSegment, ExportSection, FunctionSection, GlobalSection,
    Import, ImportDesc, ImportSection, MemorySection, TypeSection, WasmModule,
};
use crate::wasm_module::{
    code_builder, BlockType, CodeBuilder, ConstExpr, Export, ExportType, Global, GlobalType,
    LocalId, Signature, SymInfo, ValueType,
};
use crate::{
    copy_memory, CopyMemoryConfig, Env, BUILTINS_IMPORT_MODULE_NAME, MEMORY_NAME, PTR_SIZE,
    PTR_TYPE, STACK_POINTER_GLOBAL_ID, STACK_POINTER_NAME,
};

/// The memory address where the constants data will be loaded during module instantiation.
/// We avoid address zero and anywhere near it. They're valid addresses but maybe bug-prone.
/// Follow Emscripten's example by leaving 1kB unused (though 4 bytes would probably do!)
const CONST_SEGMENT_BASE_ADDR: u32 = 1024;

/// Index of the data segment where we store constants
const CONST_SEGMENT_INDEX: usize = 0;

pub struct WasmBackend<'a> {
    env: &'a Env<'a>,

    // Module-level data
    pub module: WasmModule<'a>,
    layout_ids: LayoutIds<'a>,
    constant_sym_index_map: MutMap<&'a str, usize>,
    builtin_sym_index_map: MutMap<&'a str, usize>,
    proc_symbols: Vec<'a, Symbol>,
    pub linker_symbols: Vec<'a, SymInfo>,

    // Function-level data
    code_builder: CodeBuilder<'a>,
    storage: Storage<'a>,

    /// how many blocks deep are we (used for jumps)
    block_depth: u32,
    joinpoint_label_map: MutMap<JoinPointId, (u32, Vec<'a, StoredValue>)>,
}

impl<'a> WasmBackend<'a> {
    pub fn new(
        env: &'a Env<'a>,
        layout_ids: LayoutIds<'a>,
        proc_symbols: Vec<'a, Symbol>,
        mut linker_symbols: Vec<'a, SymInfo>,
        mut exports: Vec<'a, Export>,
    ) -> Self {
        const MEMORY_INIT_SIZE: u32 = 1024 * 1024;
        let arena = env.arena;
        let num_procs = proc_symbols.len();

        exports.push(Export {
            name: MEMORY_NAME.to_string(),
            ty: ExportType::Mem,
            index: 0,
        });

        let stack_pointer = Global {
            ty: GlobalType {
                value_type: ValueType::I32,
                is_mutable: true,
            },
            init: ConstExpr::I32(MEMORY_INIT_SIZE as i32),
        };

        exports.push(Export {
            name: STACK_POINTER_NAME.to_string(),
            ty: ExportType::Global,
            index: STACK_POINTER_GLOBAL_ID,
        });

        linker_symbols.push(SymInfo::Global(WasmObjectSymbol::Defined {
            flags: WASM_SYM_BINDING_WEAK, // TODO: this works but means external .o files decide how much stack we have!
            index: STACK_POINTER_GLOBAL_ID,
            name: STACK_POINTER_NAME.to_string(),
        }));

        let const_segment = DataSegment {
            mode: DataMode::Active {
                offset: ConstExpr::I32(CONST_SEGMENT_BASE_ADDR as i32),
            },
            init: Vec::with_capacity_in(64, arena),
        };

        let module = WasmModule {
            types: TypeSection::new(arena, num_procs),
            import: ImportSection::new(arena),
            function: FunctionSection::new(arena, num_procs),
            table: (),
            memory: MemorySection::new(MEMORY_INIT_SIZE),
            global: GlobalSection {
                entries: bumpalo::vec![in arena; stack_pointer],
            },
            export: ExportSection { entries: exports },
            start: (),
            element: (),
            code: CodeSection {
                code_builders: Vec::with_capacity_in(num_procs, arena),
            },
            data: DataSection {
                segments: bumpalo::vec![in arena; const_segment],
            },
            linking: LinkingSection::new(arena),
            relocations: RelocationSection::new(arena, "reloc.CODE"),
        };

        WasmBackend {
            env,

            // Module-level data
            module,

            layout_ids,
            constant_sym_index_map: MutMap::default(),
            builtin_sym_index_map: MutMap::default(),
            proc_symbols,
            linker_symbols,

            // Function-level data
            block_depth: 0,
            joinpoint_label_map: MutMap::default(),
            code_builder: CodeBuilder::new(arena),
            storage: Storage::new(arena),
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
        self.code_builder.build_fn_header(
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
                    }
                }
                // jump to the "stack frame pop" code at the end of the function
                self.code_builder.br(self.block_depth - 1);

                Ok(())
            }

            Stmt::Switch {
                cond_symbol,
                cond_layout,
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

                // create a block for each branch except the default
                for _ in 0..branches.len() {
                    self.start_block(BlockType::NoResult)
                }

                let is_bool = matches!(cond_layout, Layout::Builtin(Builtin::Int1));
                let cond_type = WasmLayout::new(cond_layout).value_type();

                // then, we jump whenever the value under scrutiny is equal to the value of a branch
                for (i, (value, _, _)) in branches.iter().enumerate() {
                    // put the cond_symbol on the top of the stack
                    self.storage
                        .load_symbols(&mut self.code_builder, &[*cond_symbol]);

                    if is_bool {
                        // We already have a bool, don't need to compare against a const to get one
                        if *value == 0 {
                            self.code_builder.i32_eqz();
                        }
                    } else {
                        match cond_type {
                            ValueType::I32 => {
                                self.code_builder.i32_const(*value as i32);
                                self.code_builder.i32_eq();
                            }
                            ValueType::I64 => {
                                self.code_builder.i64_const(*value as i64);
                                self.code_builder.i64_eq();
                            }
                            ValueType::F32 => {
                                self.code_builder.f32_const(f32::from_bits(*value as u32));
                                self.code_builder.f32_eq();
                            }
                            ValueType::F64 => {
                                self.code_builder.f64_const(f64::from_bits(*value as u64));
                                self.code_builder.f64_eq();
                            }
                        }
                    }

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

            Stmt::Refcounting(_modify, following) => {
                // TODO: actually deal with refcounting. For hello world, we just skipped it.
                self.build_stmt(following, ret_layout)?;
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
            Expr::Literal(lit) => self.load_literal(lit, storage, *sym, layout),

            Expr::Call(roc_mono::ir::Call {
                call_type,
                arguments,
            }) => match call_type {
                CallType::ByName { name: func_sym, .. } => {
                    // If this function is just a lowlevel wrapper, then inline it
                    if let Some(lowlevel) = LowLevel::from_inlined_wrapper(*func_sym) {
                        return self.build_low_level(lowlevel, arguments, *sym, wasm_layout);
                    }

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

                    // Index of the called function in the code section. Assumes all functions end up in the binary.
                    // (We may decide to keep all procs even if calls are inlined, in case platform calls them)
                    let func_index = match self.proc_symbols.iter().position(|s| s == func_sym) {
                        Some(i) => i as u32,
                        None => {
                            // TODO: actually useful linking! Push a relocation for it.
                            return Err(format!(
                                "Not yet supported: calling foreign function {:?}",
                                func_sym
                            ));
                        }
                    };

                    // Index of the function's name in the symbol table
                    // Same as the function index since those are the first symbols we add
                    let symbol_index = func_index;

                    self.code_builder.call(
                        func_index,
                        symbol_index,
                        wasm_args.len(),
                        has_return_val,
                    );

                    Ok(())
                }

                CallType::LowLevel { op: lowlevel, .. } => {
                    self.build_low_level(*lowlevel, arguments, *sym, wasm_layout)
                }

                x => Err(format!("the call type, {:?}, is not yet implemented", x)),
            },

            Expr::Struct(fields) => self.create_struct(sym, layout, fields),

            Expr::StructAtIndex {
                index,
                field_layouts,
                structure,
            } => {
                if let StoredValue::StackMemory { location, .. } = self.storage.get(structure) {
                    let (local_id, mut offset) =
                        location.local_and_offset(self.storage.stack_frame_pointer);
                    for field in field_layouts.iter().take(*index as usize) {
                        offset += field.stack_size(PTR_SIZE);
                    }
                    self.storage.copy_value_from_memory(
                        &mut self.code_builder,
                        *sym,
                        local_id,
                        offset,
                    );
                } else {
                    unreachable!("Unexpected storage for {:?}", structure)
                }
                Ok(())
            }

            x => Err(format!("Expression is not yet implemented {:?}", x)),
        }
    }

    fn build_low_level(
        &mut self,
        lowlevel: LowLevel,
        arguments: &'a [Symbol],
        return_sym: Symbol,
        return_layout: WasmLayout,
    ) -> Result<(), String> {
        // Load symbols using the "fast calling convention" that Zig uses instead of the C ABI we normally use.
        // It's only different from the C ABI for small structs, and we are using Zig for all of those cases.
        // This is a workaround for a bug in Zig. If later versions fix it, we can change to the C ABI.
        self.storage.load_symbols_fastcc(
            &mut self.code_builder,
            arguments,
            return_sym,
            &return_layout,
        );

        let build_result = decode_low_level(
            &mut self.code_builder,
            &mut self.storage,
            lowlevel,
            arguments,
            &return_layout,
        );
        use LowlevelBuildResult::*;

        match build_result {
            Done => Ok(()),
            BuiltinCall(name) => {
                self.call_zig_builtin(name, arguments, &return_layout);
                Ok(())
            }
            NotImplemented => Err(format!(
                "Low level operation {:?} is not yet implemented",
                lowlevel
            )),
        }
    }

    fn load_literal(
        &mut self,
        lit: &Literal<'a>,
        storage: &StoredValue,
        sym: Symbol,
        layout: &Layout<'a>,
    ) -> Result<(), String> {
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

            StoredValue::StackMemory { location, .. } => match lit {
                Literal::Str(string) => {
                    let (local_id, offset) =
                        location.local_and_offset(self.storage.stack_frame_pointer);

                    let len = string.len();
                    if len < 8 {
                        let mut stack_mem_bytes = [0; 8];
                        stack_mem_bytes[0..len].clone_from_slice(string.as_bytes());
                        stack_mem_bytes[7] = 0x80 | (len as u8);
                        let str_as_int = i64::from_le_bytes(stack_mem_bytes);

                        self.code_builder.get_local(local_id);
                        self.code_builder.i64_const(str_as_int);
                        self.code_builder.i64_store(Align::Bytes4, offset);
                    } else {
                        let (linker_sym_index, elements_addr) =
                            self.lookup_string_constant(string, sym, layout);

                        self.code_builder.get_local(local_id);
                        self.code_builder
                            .i32_const_mem_addr(elements_addr, linker_sym_index);
                        self.code_builder.i32_store(Align::Bytes4, offset);

                        self.code_builder.get_local(local_id);
                        self.code_builder.i32_const(string.len() as i32);
                        self.code_builder.i32_store(Align::Bytes4, offset + 4);
                    };
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

    /// Look up a string constant in our internal data structures
    /// Return the data we need for code gen: linker symbol index and memory address
    fn lookup_string_constant(
        &mut self,
        string: &'a str,
        sym: Symbol,
        layout: &Layout<'a>,
    ) -> (u32, u32) {
        match self.constant_sym_index_map.get(string) {
            Some(linker_sym_index) => {
                // We've seen this string before. The linker metadata has a reference
                // to its offset in the constants data segment.
                let syminfo = &self.linker_symbols[*linker_sym_index];
                match syminfo {
                    SymInfo::Data(DataSymbol::Defined { segment_offset, .. }) => {
                        let elements_addr = *segment_offset + CONST_SEGMENT_BASE_ADDR;
                        (*linker_sym_index as u32, elements_addr)
                    }
                    _ => unreachable!(
                        "Compiler bug: Invalid linker symbol info for string {:?}:\n{:?}",
                        string, syminfo
                    ),
                }
            }

            None => {
                let const_segment_bytes = &mut self.module.data.segments[CONST_SEGMENT_INDEX].init;

                // Store the string in the data section, to be loaded on module instantiation
                // RocStr `elements` field will point to that constant data, not the heap
                let segment_offset = const_segment_bytes.len() as u32;
                let elements_addr = segment_offset + CONST_SEGMENT_BASE_ADDR;
                const_segment_bytes.extend_from_slice(string.as_bytes());

                // Generate linker info
                // Just pick the symbol name from the first usage
                let name = self
                    .layout_ids
                    .get(sym, layout)
                    .to_symbol_string(sym, &self.env.interns);
                let linker_symbol = SymInfo::Data(DataSymbol::Defined {
                    flags: 0,
                    name,
                    segment_index: CONST_SEGMENT_INDEX as u32,
                    segment_offset,
                    size: string.len() as u32,
                });

                let linker_sym_index = self.linker_symbols.len();
                self.constant_sym_index_map.insert(string, linker_sym_index);
                self.linker_symbols.push(linker_symbol);

                (linker_sym_index as u32, elements_addr)
            }
        }
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

    /// Generate a call instruction to a Zig builtin function.
    /// And if we haven't seen it before, add an Import and linker data for it.
    /// Zig calls use LLVM's "fast" calling convention rather than our usual C ABI.
    fn call_zig_builtin(&mut self, name: &'a str, arguments: &[Symbol], ret_layout: &WasmLayout) {
        let (fn_index, linker_symbol_index) = match self.builtin_sym_index_map.get(name) {
            Some(sym_idx) => match &self.linker_symbols[*sym_idx] {
                SymInfo::Function(WasmObjectSymbol::Imported { index, .. }) => {
                    (*index, *sym_idx as u32)
                }
                x => unreachable!("Invalid linker symbol for builtin {}: {:?}", name, x),
            },

            None => {
                let mut param_types = Vec::with_capacity_in(1 + arguments.len(), self.env.arena);

                let ret_type = if ret_layout.is_stack_memory() {
                    param_types.push(ValueType::I32);
                    None
                } else {
                    Some(ret_layout.value_type())
                };

                // Zig's "fast calling convention" packs structs into CPU registers (stack machine slots) if possible.
                // If they're small enough they can go into an I32 or I64. If they're big, they're pointers (I32).
                for arg in arguments {
                    param_types.push(match self.storage.get(arg) {
                        StoredValue::StackMemory { size, .. } if *size > 4 && *size <= 8 => {
                            ValueType::I64
                        }
                        stored => stored.value_type(),
                    });
                }

                let signature_index = self.module.types.insert(Signature {
                    param_types,
                    ret_type,
                });

                let import_index = self.module.import.entries.len() as u32;
                let import = Import {
                    module: BUILTINS_IMPORT_MODULE_NAME,
                    name: name.to_string(),
                    description: ImportDesc::Func { signature_index },
                };
                self.module.import.entries.push(import);

                let sym_idx = self.linker_symbols.len();
                let sym_info = SymInfo::Function(WasmObjectSymbol::Imported {
                    flags: WASM_SYM_UNDEFINED,
                    index: import_index,
                });
                self.linker_symbols.push(sym_info);
                self.builtin_sym_index_map.insert(name, sym_idx);

                (import_index, sym_idx as u32)
            }
        };
        self.code_builder.call(
            fn_index,
            linker_symbol_index,
            arguments.len(),
            true, // TODO: handle builtins with no return value
        );
    }
}
