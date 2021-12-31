use bumpalo::{self, collections::Vec};

use code_builder::Align;
use roc_builtins::bitcode::{self, IntWidth};
use roc_collections::all::MutMap;
use roc_module::ident::Ident;
use roc_module::low_level::{LowLevel, LowLevelWrapperType};
use roc_module::symbol::{Interns, Symbol};
use roc_mono::code_gen_help::{CodeGenHelp, REFCOUNT_MAX};
use roc_mono::ir::{
    CallType, Expr, JoinPointId, ListLiteralElement, Literal, Proc, ProcLayout, Stmt,
};

use roc_mono::layout::{Builtin, Layout, LayoutIds, TagIdIntType, UnionLayout};
use roc_reporting::internal_error;

use crate::layout::{CallConv, ReturnMethod, StackMemoryFormat, WasmLayout};
use crate::low_level::{dispatch_low_level, LowlevelBuildResult};
use crate::storage::{StackMemoryLocation, Storage, StoredValue, StoredValueKind};
use crate::wasm_module::linking::{
    DataSymbol, LinkingSection, RelocationSection, WasmObjectSymbol, WASM_SYM_BINDING_WEAK,
    WASM_SYM_UNDEFINED,
};
use crate::wasm_module::sections::{
    CodeSection, DataMode, DataSection, DataSegment, ExportSection, FunctionSection, GlobalSection,
    Import, ImportDesc, ImportSection, MemorySection, TypeSection, WasmModule,
};
use crate::wasm_module::{
    code_builder, CodeBuilder, ConstExpr, Export, ExportType, Global, GlobalType,
    LinkingSubSection, LocalId, Signature, SymInfo, ValueType,
};
use crate::{
    copy_memory, CopyMemoryConfig, Env, BUILTINS_IMPORT_MODULE_NAME, DEBUG_LOG_SETTINGS,
    MEMORY_NAME, PTR_SIZE, PTR_TYPE, STACK_POINTER_GLOBAL_ID, STACK_POINTER_NAME,
};

/// The memory address where the constants data will be loaded during module instantiation.
/// We avoid address zero and anywhere near it. They're valid addresses but maybe bug-prone.
/// Follow Emscripten's example by leaving 1kB unused (though 4 bytes would probably do!)
const CONST_SEGMENT_BASE_ADDR: u32 = 1024;

/// Index of the data segment where we store constants
const CONST_SEGMENT_INDEX: usize = 0;

pub struct WasmBackend<'a> {
    env: &'a Env<'a>,
    interns: &'a mut Interns,

    // Module-level data
    module: WasmModule<'a>,
    layout_ids: LayoutIds<'a>,
    constant_sym_index_map: MutMap<&'a str, usize>,
    builtin_sym_index_map: MutMap<&'a str, usize>,
    proc_symbols: Vec<'a, (Symbol, u32)>,
    linker_symbols: Vec<'a, SymInfo>,
    helper_proc_gen: CodeGenHelp<'a>,

    // Function-level data
    code_builder: CodeBuilder<'a>,
    storage: Storage<'a>,

    /// how many blocks deep are we (used for jumps)
    block_depth: u32,
    joinpoint_label_map: MutMap<JoinPointId, (u32, Vec<'a, StoredValue>)>,

    debug_current_proc_index: usize,
}

impl<'a> WasmBackend<'a> {
    pub fn new(
        env: &'a Env<'a>,
        interns: &'a mut Interns,
        layout_ids: LayoutIds<'a>,
        proc_symbols: Vec<'a, (Symbol, u32)>,
        mut linker_symbols: Vec<'a, SymInfo>,
        mut exports: Vec<'a, Export>,
        helper_proc_gen: CodeGenHelp<'a>,
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
            interns,

            // Module-level data
            module,

            layout_ids,
            constant_sym_index_map: MutMap::default(),
            builtin_sym_index_map: MutMap::default(),
            proc_symbols,
            linker_symbols,
            helper_proc_gen,

            // Function-level data
            block_depth: 0,
            joinpoint_label_map: MutMap::default(),
            code_builder: CodeBuilder::new(arena),
            storage: Storage::new(arena),

            debug_current_proc_index: 0,
        }
    }

    pub fn generate_helpers(&mut self) -> Vec<'a, Proc<'a>> {
        self.helper_proc_gen.take_procs()
    }

    fn register_helper_proc(&mut self, new_proc_info: (Symbol, ProcLayout<'a>)) {
        let (new_proc_sym, new_proc_layout) = new_proc_info;
        let wasm_fn_index = self.proc_symbols.len() as u32;
        let linker_sym_index = self.linker_symbols.len() as u32;

        let name = self
            .layout_ids
            .get_toplevel(new_proc_sym, &new_proc_layout)
            .to_symbol_string(new_proc_sym, self.interns);

        self.proc_symbols.push((new_proc_sym, linker_sym_index));
        self.linker_symbols
            .push(SymInfo::Function(WasmObjectSymbol::Defined {
                flags: 0,
                index: wasm_fn_index,
                name,
            }));
    }

    pub fn finalize_module(mut self) -> WasmModule<'a> {
        let symbol_table = LinkingSubSection::SymbolTable(self.linker_symbols);
        self.module.linking.subsections.push(symbol_table);
        self.module
    }

    /// Register the debug names of Symbols in a global lookup table
    /// so that they have meaningful names when you print them.
    /// Particularly useful after generating IR for refcount procedures
    #[cfg(debug_assertions)]
    pub fn register_symbol_debug_names(&self) {
        let module_id = self.env.module_id;
        let ident_ids = self.interns.all_ident_ids.get(&module_id).unwrap();
        self.env.module_id.register_debug_idents(ident_ids);
    }

    #[cfg(not(debug_assertions))]
    pub fn register_symbol_debug_names(&self) {}

    /// Create an IR Symbol for an anonymous value (such as ListLiteral)
    fn create_symbol(&mut self, debug_name: &str) -> Symbol {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        let ident_id = ident_ids.add(Ident::from(debug_name));
        Symbol::new(self.env.module_id, ident_id)
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

    pub fn build_proc(&mut self, proc: &Proc<'a>) {
        if DEBUG_LOG_SETTINGS.proc_start_end {
            println!("\ngenerating procedure {:?}\n", proc.name);
        }

        self.debug_current_proc_index += 1;

        self.start_proc(proc);

        self.build_stmt(&proc.body, &proc.ret_layout);

        self.finalize_proc();
        self.reset();

        if DEBUG_LOG_SETTINGS.proc_start_end {
            println!("\nfinished generating {:?}\n", proc.name);
        }
    }

    fn start_proc(&mut self, proc: &Proc<'a>) {
        let ret_layout = WasmLayout::new(&proc.ret_layout);

        let ret_type = match ret_layout.return_method() {
            ReturnMethod::Primitive(ty) => Some(ty),
            ReturnMethod::NoReturnValue => None,
            ReturnMethod::WriteToPointerArg => {
                self.storage.arg_types.push(PTR_TYPE);
                None
            }
        };

        // Create a block so we can exit the function without skipping stack frame "pop" code.
        // We never use the `return` instruction. Instead, we break from this block.
        self.start_block();

        for (layout, symbol) in proc.args {
            self.storage
                .allocate(*layout, *symbol, StoredValueKind::Parameter);
        }

        if let Some(ty) = ret_type {
            let ret_var = self.storage.create_anonymous_local(ty);
            self.storage.return_var = Some(ret_var);
        }

        self.module.add_function_signature(Signature {
            param_types: self.storage.arg_types.clone(),
            ret_type,
        });
    }

    fn finalize_proc(&mut self) {
        // end the block from start_proc, to ensure all paths pop stack memory (if any)
        self.end_block();

        if let Some(ret_var) = self.storage.return_var {
            self.code_builder.get_local(ret_var);
        }

        // Write local declarations and stack frame push/pop code
        self.code_builder.build_fn_header_and_footer(
            &self.storage.local_types,
            self.storage.stack_frame_size,
            self.storage.stack_frame_pointer,
        );
    }

    /**********************************************************

            STATEMENTS

    ***********************************************************/

    fn start_block(&mut self) {
        // Wasm blocks can have result types, but we don't use them.
        // You need the right type on the stack when you jump from an inner block to an outer one.
        // The rules are confusing, and implementing them would add complexity and slow down code gen.
        // Instead we use local variables to move a value from an inner block to an outer one.
        self.block_depth += 1;
        self.code_builder.block();
    }

    fn start_loop(&mut self) {
        self.block_depth += 1;
        self.code_builder.loop_();
    }

    fn end_block(&mut self) {
        self.block_depth -= 1;
        self.code_builder.end();
    }

    fn store_expr_value(
        &mut self,
        sym: Symbol,
        layout: &Layout<'a>,
        expr: &Expr<'a>,
        kind: StoredValueKind,
    ) {
        let sym_storage = self.storage.allocate(*layout, sym, kind);

        self.build_expr(&sym, expr, layout, &sym_storage);

        // If this value is stored in the VM stack, we need code_builder to track it
        // (since every instruction can change the VM stack)
        if let Some(StoredValue::VirtualMachineStack { vm_state, .. }) =
            self.storage.symbol_storage_map.get_mut(&sym)
        {
            *vm_state = self.code_builder.set_top_symbol(sym);
        }
    }

    fn build_stmt(&mut self, stmt: &Stmt<'a>, ret_layout: &Layout<'a>) {
        match stmt {
            Stmt::Let(_, _, _, _) => {
                let mut current_stmt = stmt;
                while let Stmt::Let(sym, expr, layout, following) = current_stmt {
                    if DEBUG_LOG_SETTINGS.let_stmt_ir {
                        println!("let {:?} = {}", sym, expr.to_pretty(200)); // ignore `following`! Too confusing otherwise.
                    }

                    let kind = match following {
                        Stmt::Ret(ret_sym) if *sym == *ret_sym => StoredValueKind::ReturnValue,
                        _ => StoredValueKind::Variable,
                    };

                    self.store_expr_value(*sym, layout, expr, kind);

                    current_stmt = *following;
                }

                self.build_stmt(current_stmt, ret_layout);
            }

            Stmt::Ret(sym) => {
                use crate::storage::StoredValue::*;

                let storage = self.storage.symbol_storage_map.get(sym).unwrap();

                match storage {
                    StackMemory {
                        location,
                        size,
                        alignment_bytes,
                        ..
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

                        // If we have a return value, store it to the return variable
                        // This avoids complications with block result types when returning from nested blocks
                        if let Some(ret_var) = self.storage.return_var {
                            self.code_builder.set_local(ret_var);
                        }
                    }
                }
                // jump to the "stack frame pop" code at the end of the function
                self.code_builder.br(self.block_depth - 1);
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
                    self.start_block()
                }

                let is_bool = matches!(cond_layout, Layout::Builtin(Builtin::Bool));
                let cond_type = WasmLayout::new(cond_layout).arg_types(CallConv::C)[0];

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
                self.build_stmt(default_branch.1, ret_layout);

                // now put in the actual body of each branch in order
                // (the first branch would have broken out of 1 block,
                // hence we must generate its code first)
                for (_, _, branch) in branches.iter() {
                    self.end_block();

                    self.build_stmt(branch, ret_layout);
                }
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
                    let mut param_storage = self.storage.allocate(
                        parameter.layout,
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

                self.start_block();

                self.joinpoint_label_map
                    .insert(*id, (self.block_depth, jp_param_storages));

                self.build_stmt(remainder, ret_layout);

                self.end_block();
                self.start_loop();

                self.build_stmt(body, ret_layout);

                // ends the loop
                self.end_block();
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
            }

            Stmt::Refcounting(modify, following) => {
                let value = modify.get_symbol();
                let layout = self.storage.symbol_layouts[&value];

                let ident_ids = self
                    .interns
                    .all_ident_ids
                    .get_mut(&self.env.module_id)
                    .unwrap();

                let (rc_stmt, new_specializations) = self
                    .helper_proc_gen
                    .expand_refcount_stmt(ident_ids, layout, modify, *following);

                if false {
                    self.register_symbol_debug_names();
                    println!("## rc_stmt:\n{}\n{:?}", rc_stmt.to_pretty(200), rc_stmt);
                }

                // If any new specializations were created, register their symbol data
                for spec in new_specializations.into_iter() {
                    self.register_helper_proc(spec);
                }

                self.build_stmt(rc_stmt, ret_layout);
            }

            x => todo!("statement {:?}", x),
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
    ) {
        let wasm_layout = WasmLayout::new(layout);
        match expr {
            Expr::Literal(lit) => self.load_literal(lit, storage, *sym, layout),

            Expr::Call(roc_mono::ir::Call {
                call_type,
                arguments,
            }) => match call_type {
                CallType::ByName { name: func_sym, .. } => {
                    // If this function is just a lowlevel wrapper, then inline it
                    if let LowLevelWrapperType::CanBeReplacedBy(lowlevel) =
                        LowLevelWrapperType::from_symbol(*func_sym)
                    {
                        return self.build_low_level(
                            lowlevel,
                            arguments,
                            *sym,
                            wasm_layout,
                            storage,
                        );
                    }

                    let (param_types, ret_type) = self.storage.load_symbols_for_call(
                        self.env.arena,
                        &mut self.code_builder,
                        arguments,
                        *sym,
                        &wasm_layout,
                        CallConv::C,
                    );

                    for (func_index, (ir_sym, linker_sym_index)) in
                        self.proc_symbols.iter().enumerate()
                    {
                        if ir_sym == func_sym {
                            let num_wasm_args = param_types.len();
                            let has_return_val = ret_type.is_some();
                            self.code_builder.call(
                                func_index as u32,
                                *linker_sym_index,
                                num_wasm_args,
                                has_return_val,
                            );
                            return;
                        }
                    }

                    internal_error!(
                        "Could not find procedure {:?}\nKnown procedures: {:?}",
                        func_sym,
                        self.proc_symbols
                    );
                }

                CallType::LowLevel { op: lowlevel, .. } => {
                    self.build_low_level(*lowlevel, arguments, *sym, wasm_layout, storage)
                }

                x => todo!("call type {:?}", x),
            },

            Expr::Struct(fields) => self.create_struct(sym, layout, fields),

            Expr::StructAtIndex {
                index,
                field_layouts,
                structure,
            } => {
                self.storage.ensure_value_has_local(
                    &mut self.code_builder,
                    *sym,
                    storage.to_owned(),
                );
                let (local_id, mut offset) = match self.storage.get(structure) {
                    StoredValue::StackMemory { location, .. } => {
                        location.local_and_offset(self.storage.stack_frame_pointer)
                    }

                    StoredValue::Local {
                        value_type,
                        local_id,
                        ..
                    } => {
                        debug_assert!(matches!(value_type, ValueType::I32));
                        (*local_id, 0)
                    }

                    StoredValue::VirtualMachineStack { .. } => {
                        internal_error!("ensure_value_has_local didn't work")
                    }
                };
                for field in field_layouts.iter().take(*index as usize) {
                    offset += field.stack_size(PTR_SIZE);
                }
                self.storage
                    .copy_value_from_memory(&mut self.code_builder, *sym, local_id, offset);
            }

            Expr::Array { elems, elem_layout } => {
                if let StoredValue::StackMemory { location, .. } = storage {
                    let size = elem_layout.stack_size(PTR_SIZE) * (elems.len() as u32);

                    // Allocate heap space and store its address in a local variable
                    let heap_local_id = self.storage.create_anonymous_local(PTR_TYPE);
                    let heap_alignment = elem_layout.alignment_bytes(PTR_SIZE);
                    self.allocate_with_refcount(Some(size), heap_alignment, 1);
                    self.code_builder.set_local(heap_local_id);

                    let (stack_local_id, stack_offset) =
                        location.local_and_offset(self.storage.stack_frame_pointer);

                    // elements pointer
                    self.code_builder.get_local(stack_local_id);
                    self.code_builder.get_local(heap_local_id);
                    self.code_builder.i32_store(Align::Bytes4, stack_offset);

                    // length of the list
                    self.code_builder.get_local(stack_local_id);
                    self.code_builder.i32_const(elems.len() as i32);
                    self.code_builder.i32_store(Align::Bytes4, stack_offset + 4);

                    let mut elem_offset = 0;

                    for (i, elem) in elems.iter().enumerate() {
                        let elem_sym = match elem {
                            ListLiteralElement::Literal(lit) => {
                                // This has no Symbol but our storage methods expect one.
                                // Let's just pretend it was defined in a `Let`.
                                let debug_name = format!("{:?}_{}", sym, i);
                                let elem_sym = self.create_symbol(&debug_name);
                                let expr = Expr::Literal(*lit);

                                self.store_expr_value(
                                    elem_sym,
                                    elem_layout,
                                    &expr,
                                    StoredValueKind::Variable,
                                );

                                elem_sym
                            }

                            ListLiteralElement::Symbol(elem_sym) => *elem_sym,
                        };

                        elem_offset += self.storage.copy_value_to_memory(
                            &mut self.code_builder,
                            heap_local_id,
                            elem_offset,
                            elem_sym,
                        );
                    }
                } else {
                    internal_error!("Unexpected storage for Array {:?}: {:?}", sym, storage)
                }
            }

            Expr::EmptyArray => {
                if let StoredValue::StackMemory { location, .. } = storage {
                    let (local_id, offset) =
                        location.local_and_offset(self.storage.stack_frame_pointer);

                    // This is a minor cheat.
                    // What we want to write to stack memory is { elements: null, length: 0 }
                    // But instead of two 32-bit stores, we can do a single 64-bit store.
                    self.code_builder.get_local(local_id);
                    self.code_builder.i64_const(0);
                    self.code_builder.i64_store(Align::Bytes4, offset);
                } else {
                    internal_error!("Unexpected storage for {:?}", sym)
                }
            }

            Expr::Tag {
                tag_layout: union_layout,
                tag_id,
                arguments,
                ..
            } => self.build_tag(union_layout, *tag_id, arguments, *sym, storage),

            Expr::GetTagId {
                structure,
                union_layout,
            } => self.build_get_tag_id(*structure, union_layout, *sym, storage),

            Expr::UnionAtIndex {
                structure,
                tag_id,
                union_layout,
                index,
            } => self.build_union_at_index(*structure, *tag_id, union_layout, *index, *sym),

            _ => todo!("Expression `{}`", expr.to_pretty(100)),
        }
    }

    fn build_tag(
        &mut self,
        union_layout: &UnionLayout<'a>,
        tag_id: TagIdIntType,
        arguments: &'a [Symbol],
        symbol: Symbol,
        stored: &StoredValue,
    ) {
        if union_layout.tag_is_null(tag_id) {
            self.code_builder.i32_const(0);
            return;
        }

        let stores_tag_id_as_data = union_layout.stores_tag_id_as_data(PTR_SIZE);
        let stores_tag_id_in_pointer = union_layout.stores_tag_id_in_pointer(PTR_SIZE);
        let (data_size, data_alignment) = union_layout.data_size_and_alignment(PTR_SIZE);

        // We're going to use the pointer many times, so put it in a local variable
        let stored_with_local =
            self.storage
                .ensure_value_has_local(&mut self.code_builder, symbol, stored.to_owned());

        let (local_id, data_offset) = match stored_with_local {
            StoredValue::StackMemory { location, .. } => {
                location.local_and_offset(self.storage.stack_frame_pointer)
            }
            StoredValue::Local { local_id, .. } => {
                // Tag is stored as a pointer to the heap. Call the allocator to get a memory address.
                self.allocate_with_refcount(Some(data_size), data_alignment, 1);
                self.code_builder.set_local(local_id);
                (local_id, 0)
            }
            StoredValue::VirtualMachineStack { .. } => {
                internal_error!("{:?} should have a local variable", symbol)
            }
        };

        // Write the field values to memory
        let mut field_offset = data_offset;
        for field_symbol in arguments.iter() {
            field_offset += self.storage.copy_value_to_memory(
                &mut self.code_builder,
                local_id,
                field_offset,
                *field_symbol,
            );
        }

        // Store the tag ID (if any)
        if stores_tag_id_as_data {
            let id_offset = data_offset + data_size - data_alignment;

            let id_align = union_layout.tag_id_builtin().alignment_bytes(PTR_SIZE);
            let id_align = Align::from(id_align);

            self.code_builder.get_local(local_id);

            match id_align {
                Align::Bytes1 => {
                    self.code_builder.i32_const(tag_id as i32);
                    self.code_builder.i32_store8(id_align, id_offset);
                }
                Align::Bytes2 => {
                    self.code_builder.i32_const(tag_id as i32);
                    self.code_builder.i32_store16(id_align, id_offset);
                }
                Align::Bytes4 => {
                    self.code_builder.i32_const(tag_id as i32);
                    self.code_builder.i32_store(id_align, id_offset);
                }
                Align::Bytes8 => {
                    self.code_builder.i64_const(tag_id as i64);
                    self.code_builder.i64_store(id_align, id_offset);
                }
            }
        } else if stores_tag_id_in_pointer {
            self.code_builder.get_local(local_id);
            self.code_builder.i32_const(tag_id as i32);
            self.code_builder.i32_or();
            self.code_builder.set_local(local_id);
        }
    }

    fn build_get_tag_id(
        &mut self,
        structure: Symbol,
        union_layout: &UnionLayout<'a>,
        tag_id_symbol: Symbol,
        stored_value: &StoredValue,
    ) {
        use UnionLayout::*;

        let block_result_id = match union_layout {
            NonRecursive(_) => None,
            Recursive(_) => None,
            NonNullableUnwrapped(_) => {
                self.code_builder.i32_const(0);
                return;
            }
            NullableWrapped { nullable_id, .. } => {
                let stored_with_local = self.storage.ensure_value_has_local(
                    &mut self.code_builder,
                    tag_id_symbol,
                    stored_value.to_owned(),
                );
                let local_id = match stored_with_local {
                    StoredValue::Local { local_id, .. } => local_id,
                    _ => internal_error!("ensure_value_has_local didn't work"),
                };

                // load pointer
                self.storage
                    .load_symbols(&mut self.code_builder, &[structure]);

                // null check
                self.code_builder.i32_eqz();
                self.code_builder.if_();
                self.code_builder.i32_const(*nullable_id as i32);
                self.code_builder.set_local(local_id);
                self.code_builder.else_();
                Some(local_id)
            }
            NullableUnwrapped { nullable_id, .. } => {
                self.code_builder.i32_const(!(*nullable_id) as i32);
                self.code_builder.i32_const(*nullable_id as i32);
                self.storage
                    .load_symbols(&mut self.code_builder, &[structure]);
                self.code_builder.select();
                None
            }
        };

        if union_layout.stores_tag_id_as_data(PTR_SIZE) {
            let (data_size, data_alignment) = union_layout.data_size_and_alignment(PTR_SIZE);
            let id_offset = data_size - data_alignment;

            let id_align = union_layout.tag_id_builtin().alignment_bytes(PTR_SIZE);
            let id_align = Align::from(id_align);

            self.storage
                .load_symbols(&mut self.code_builder, &[structure]);

            match union_layout.tag_id_builtin() {
                Builtin::Bool | Builtin::Int(IntWidth::U8) => {
                    self.code_builder.i32_load8_u(id_align, id_offset)
                }
                Builtin::Int(IntWidth::U16) => self.code_builder.i32_load16_u(id_align, id_offset),
                Builtin::Int(IntWidth::U32) => self.code_builder.i32_load(id_align, id_offset),
                Builtin::Int(IntWidth::U64) => self.code_builder.i64_load(id_align, id_offset),
                x => internal_error!("Unexpected layout for tag union id {:?}", x),
            }
        } else if union_layout.stores_tag_id_in_pointer(PTR_SIZE) {
            self.storage
                .load_symbols(&mut self.code_builder, &[structure]);
            self.code_builder.i32_const(3);
            self.code_builder.i32_and();
        }

        if let Some(local_id) = block_result_id {
            self.code_builder.set_local(local_id);
            self.code_builder.end();
        }
    }

    fn build_union_at_index(
        &mut self,
        structure: Symbol,
        tag_id: TagIdIntType,
        union_layout: &UnionLayout<'a>,
        index: u64,
        symbol: Symbol,
    ) {
        use UnionLayout::*;

        debug_assert!(!union_layout.tag_is_null(tag_id));

        let tag_index = tag_id as usize;
        let field_layouts = match union_layout {
            NonRecursive(tags) => tags[tag_index],
            Recursive(tags) => tags[tag_index],
            NonNullableUnwrapped(layouts) => *layouts,
            NullableWrapped {
                other_tags,
                nullable_id,
            } => {
                let index = if tag_index > *nullable_id as usize {
                    tag_index - 1
                } else {
                    tag_index
                };
                other_tags[index]
            }
            NullableUnwrapped { other_fields, .. } => *other_fields,
        };

        let field_offset: u32 = field_layouts
            .iter()
            .take(index as usize)
            .map(|field_layout| field_layout.stack_size(PTR_SIZE))
            .sum();

        // Get pointer and offset to the tag's data
        let structure_storage = self.storage.get(&structure).to_owned();
        let stored_with_local = self.storage.ensure_value_has_local(
            &mut self.code_builder,
            structure,
            structure_storage,
        );
        let (tag_local_id, tag_offset) = match stored_with_local {
            StoredValue::StackMemory { location, .. } => {
                location.local_and_offset(self.storage.stack_frame_pointer)
            }
            StoredValue::Local { local_id, .. } => (local_id, 0),
            StoredValue::VirtualMachineStack { .. } => {
                internal_error!("{:?} should have a local variable", structure)
            }
        };

        let stores_tag_id_in_pointer = union_layout.stores_tag_id_in_pointer(PTR_SIZE);

        let from_ptr = if stores_tag_id_in_pointer {
            let ptr = self.storage.create_anonymous_local(ValueType::I32);
            self.code_builder.get_local(tag_local_id);
            self.code_builder.i32_const(-4); // 11111111...1100
            self.code_builder.i32_and();
            self.code_builder.set_local(ptr);
            ptr
        } else {
            tag_local_id
        };

        let from_offset = tag_offset + field_offset;
        self.storage
            .copy_value_from_memory(&mut self.code_builder, symbol, from_ptr, from_offset);
    }

    /// Allocate heap space and write an initial refcount
    /// If the data size is known at compile time, pass it in comptime_data_size.
    /// If size is only known at runtime, push *data* size to the VM stack first.
    /// Leaves the *data* address on the VM stack
    fn allocate_with_refcount(
        &mut self,
        comptime_data_size: Option<u32>,
        alignment_bytes: u32,
        initial_refcount: u32,
    ) {
        // Add extra bytes for the refcount
        let extra_bytes = alignment_bytes.max(PTR_SIZE);

        if let Some(data_size) = comptime_data_size {
            // Data size known at compile time and passed as an argument
            self.code_builder
                .i32_const((data_size + extra_bytes) as i32);
        } else {
            // Data size known only at runtime and is on top of VM stack
            self.code_builder.i32_const(extra_bytes as i32);
            self.code_builder.i32_add();
        }

        // Provide a constant for the alignment argument
        self.code_builder.i32_const(alignment_bytes as i32);

        // Call the foreign function. (Zig and C calling conventions are the same for this signature)
        let param_types = bumpalo::vec![in self.env.arena; ValueType::I32, ValueType::I32];
        let ret_type = Some(ValueType::I32);
        self.call_zig_builtin("roc_alloc", param_types, ret_type);

        // Save the allocation address to a temporary local variable
        let local_id = self.storage.create_anonymous_local(ValueType::I32);
        self.code_builder.tee_local(local_id);

        // Write the initial refcount
        let refcount_offset = extra_bytes - PTR_SIZE;
        let encoded_refcount = (initial_refcount as i32) - 1 + i32::MIN;
        self.code_builder.i32_const(encoded_refcount);
        self.code_builder.i32_store(Align::Bytes4, refcount_offset);

        // Put the data address on the VM stack
        self.code_builder.get_local(local_id);
        self.code_builder.i32_const(extra_bytes as i32);
        self.code_builder.i32_add();
    }

    fn build_low_level(
        &mut self,
        lowlevel: LowLevel,
        arguments: &'a [Symbol],
        return_sym: Symbol,
        return_layout: WasmLayout,
        storage: &StoredValue,
    ) {
        use LowLevel::*;

        match lowlevel {
            Eq | NotEq => {
                self.build_eq_or_neq(lowlevel, arguments, return_sym, return_layout, storage)
            }
            PtrCast => {
                // Don't want Zig calling convention when casting pointers.
                self.storage.load_symbols(&mut self.code_builder, arguments);
            }
            Hash => todo!("Generic hash function generation"),

            // Almost all lowlevels take this branch, except for the special cases above
            _ => {
                // Load the arguments using Zig calling convention
                let (param_types, ret_type) = self.storage.load_symbols_for_call(
                    self.env.arena,
                    &mut self.code_builder,
                    arguments,
                    return_sym,
                    &return_layout,
                    CallConv::Zig,
                );

                // Generate instructions OR decide which Zig function to call
                let build_result = dispatch_low_level(
                    &mut self.code_builder,
                    &mut self.storage,
                    lowlevel,
                    arguments,
                    &return_layout,
                );

                // Handle the result
                use LowlevelBuildResult::*;
                match build_result {
                    Done => {}
                    BuiltinCall(name) => {
                        self.call_zig_builtin(name, param_types, ret_type);
                    }
                    NotImplemented => {
                        todo!("Low level operation {:?}", lowlevel)
                    }
                }
            }
        }
    }

    fn build_eq_or_neq(
        &mut self,
        lowlevel: LowLevel,
        arguments: &'a [Symbol],
        return_sym: Symbol,
        return_layout: WasmLayout,
        storage: &StoredValue,
    ) {
        let arg_layout = self.storage.symbol_layouts[&arguments[0]];
        let other_arg_layout = self.storage.symbol_layouts[&arguments[1]];
        debug_assert!(
            arg_layout == other_arg_layout,
            "Cannot do `==` comparison on different types"
        );

        match arg_layout {
            Layout::Builtin(
                Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal,
            ) => self.build_eq_or_neq_number(lowlevel, arguments, return_layout),

            Layout::Builtin(Builtin::Str) => {
                let (param_types, ret_type) = self.storage.load_symbols_for_call(
                    self.env.arena,
                    &mut self.code_builder,
                    arguments,
                    return_sym,
                    &return_layout,
                    CallConv::Zig,
                );
                self.call_zig_builtin(bitcode::STR_EQUAL, param_types, ret_type);
                if matches!(lowlevel, LowLevel::NotEq) {
                    self.code_builder.i32_eqz();
                }
            }

            // Empty record is always equal to empty record.
            // There are no runtime arguments to check, so just emit true or false.
            Layout::Struct(fields) if fields.is_empty() => {
                self.code_builder
                    .i32_const(if lowlevel == LowLevel::Eq { 1 } else { 0 });
            }

            // Void is always equal to void. This is the type for the contents of the empty list in `[] == []`
            // This code will never execute, but we need a true or false value to type-check
            Layout::Union(UnionLayout::NonRecursive(tags)) if tags.is_empty() => {
                self.code_builder
                    .i32_const(if lowlevel == LowLevel::Eq { 1 } else { 0 });
            }

            Layout::Builtin(Builtin::Dict(_, _) | Builtin::Set(_) | Builtin::List(_))
            | Layout::Struct(_)
            | Layout::Union(_)
            | Layout::LambdaSet(_) => {
                self.build_eq_specialized(&arg_layout, arguments, return_sym, storage);
                if matches!(lowlevel, LowLevel::NotEq) {
                    self.code_builder.i32_eqz();
                }
            }

            Layout::RecursivePointer => {
                internal_error!(
                    "Tried to apply `==` to RecursivePointer values {:?}",
                    arguments,
                )
            }
        }
    }

    fn build_eq_or_neq_number(
        &mut self,
        lowlevel: LowLevel,
        arguments: &'a [Symbol],
        return_layout: WasmLayout,
    ) {
        use StoredValue::*;
        match self.storage.get(&arguments[0]).to_owned() {
            VirtualMachineStack { value_type, .. } | Local { value_type, .. } => {
                self.storage.load_symbols(&mut self.code_builder, arguments);
                match lowlevel {
                    LowLevel::Eq => match value_type {
                        ValueType::I32 => self.code_builder.i32_eq(),
                        ValueType::I64 => self.code_builder.i64_eq(),
                        ValueType::F32 => self.code_builder.f32_eq(),
                        ValueType::F64 => self.code_builder.f64_eq(),
                    },
                    LowLevel::NotEq => match value_type {
                        ValueType::I32 => self.code_builder.i32_ne(),
                        ValueType::I64 => self.code_builder.i64_ne(),
                        ValueType::F32 => self.code_builder.f32_ne(),
                        ValueType::F64 => self.code_builder.f64_ne(),
                    },
                    _ => internal_error!("Low-level op {:?} handled in the wrong place", lowlevel),
                }
            }
            StackMemory {
                format,
                location: location0,
                ..
            } => {
                if let StackMemory {
                    location: location1,
                    ..
                } = self.storage.get(&arguments[1]).to_owned()
                {
                    self.build_eq_num128(format, [location0, location1], arguments, return_layout);
                    if matches!(lowlevel, LowLevel::NotEq) {
                        self.code_builder.i32_eqz();
                    }
                }
            }
        }
    }

    fn build_eq_num128(
        &mut self,
        format: StackMemoryFormat,
        locations: [StackMemoryLocation; 2],
        arguments: &'a [Symbol],
        return_layout: WasmLayout,
    ) {
        match format {
            StackMemoryFormat::Decimal => {
                // Both args are finite
                let first = [arguments[0]];
                let second = [arguments[1]];
                dispatch_low_level(
                    &mut self.code_builder,
                    &mut self.storage,
                    LowLevel::NumIsFinite,
                    &first,
                    &return_layout,
                );
                dispatch_low_level(
                    &mut self.code_builder,
                    &mut self.storage,
                    LowLevel::NumIsFinite,
                    &second,
                    &return_layout,
                );
                self.code_builder.i32_and();

                // AND they have the same bytes
                self.build_eq_num128_bytes(locations);
                self.code_builder.i32_and();
            }

            StackMemoryFormat::Int128 => self.build_eq_num128_bytes(locations),

            StackMemoryFormat::Float128 => todo!("equality for f128"),

            StackMemoryFormat::DataStructure => {
                internal_error!("Data structure equality is handled elsewhere")
            }
        }
    }

    /// Check that two 128-bit numbers contain the same bytes
    fn build_eq_num128_bytes(&mut self, locations: [StackMemoryLocation; 2]) {
        let (local0, offset0) = locations[0].local_and_offset(self.storage.stack_frame_pointer);
        let (local1, offset1) = locations[1].local_and_offset(self.storage.stack_frame_pointer);

        self.code_builder.get_local(local0);
        self.code_builder.i64_load(Align::Bytes8, offset0);
        self.code_builder.get_local(local1);
        self.code_builder.i64_load(Align::Bytes8, offset1);
        self.code_builder.i64_eq();

        self.code_builder.get_local(local0);
        self.code_builder.i64_load(Align::Bytes8, offset0 + 8);
        self.code_builder.get_local(local1);
        self.code_builder.i64_load(Align::Bytes8, offset1 + 8);
        self.code_builder.i64_eq();

        self.code_builder.i32_and();
    }

    /// Call a helper procedure that implements `==` for a specific data structure
    fn build_eq_specialized(
        &mut self,
        arg_layout: &Layout<'a>,
        arguments: &'a [Symbol],
        return_sym: Symbol,
        storage: &StoredValue,
    ) {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        // Get an IR expression for the call to the specialized procedure
        let (specialized_call_expr, new_specializations) = self
            .helper_proc_gen
            .call_specialized_equals(ident_ids, arg_layout, arguments);

        // If any new specializations were created, register their symbol data
        for spec in new_specializations.into_iter() {
            self.register_helper_proc(spec);
        }

        // Generate Wasm code for the IR call expression
        let bool_layout = Layout::Builtin(Builtin::Bool);
        self.build_expr(
            &return_sym,
            self.env.arena.alloc(specialized_call_expr),
            &bool_layout,
            storage,
        );
    }

    fn load_literal(
        &mut self,
        lit: &Literal<'a>,
        storage: &StoredValue,
        sym: Symbol,
        layout: &Layout<'a>,
    ) {
        let not_supported_error = || todo!("Literal value {:?}", lit);

        match storage {
            StoredValue::VirtualMachineStack { value_type, .. } => {
                match (lit, value_type) {
                    (Literal::Float(x), ValueType::F64) => self.code_builder.f64_const(*x as f64),
                    (Literal::Float(x), ValueType::F32) => self.code_builder.f32_const(*x as f32),
                    (Literal::Int(x), ValueType::I64) => self.code_builder.i64_const(*x as i64),
                    (Literal::Int(x), ValueType::I32) => self.code_builder.i32_const(*x as i32),
                    (Literal::Bool(x), ValueType::I32) => self.code_builder.i32_const(*x as i32),
                    (Literal::Byte(x), ValueType::I32) => self.code_builder.i32_const(*x as i32),
                    _ => not_supported_error(),
                };
            }

            StoredValue::StackMemory { location, .. } => {
                let mut write128 = |lower_bits, upper_bits| {
                    let (local_id, offset) =
                        location.local_and_offset(self.storage.stack_frame_pointer);

                    self.code_builder.get_local(local_id);
                    self.code_builder.i64_const(lower_bits);
                    self.code_builder.i64_store(Align::Bytes8, offset);

                    self.code_builder.get_local(local_id);
                    self.code_builder.i64_const(upper_bits);
                    self.code_builder.i64_store(Align::Bytes8, offset + 8);
                };

                match lit {
                    Literal::Decimal(decimal) => {
                        let lower_bits = (decimal.0 & 0xffff_ffff_ffff_ffff) as i64;
                        let upper_bits = (decimal.0 >> 64) as i64;
                        write128(lower_bits, upper_bits);
                    }
                    Literal::Int(x) => {
                        let lower_bits = (*x & 0xffff_ffff_ffff_ffff) as i64;
                        let upper_bits = (*x >> 64) as i64;
                        write128(lower_bits, upper_bits);
                    }
                    Literal::Float(_) => {
                        // Also not implemented in LLVM backend (nor in Rust!)
                        todo!("f128 type");
                    }
                    Literal::Str(string) => {
                        let (local_id, offset) =
                            location.local_and_offset(self.storage.stack_frame_pointer);

                        let len = string.len();
                        if len < 8 {
                            let mut stack_mem_bytes = [0; 8];
                            stack_mem_bytes[0..len].clone_from_slice(string.as_bytes());
                            stack_mem_bytes[7] = 0x80 | (len as u8);
                            let str_as_int = i64::from_le_bytes(stack_mem_bytes);

                            // Write all 8 bytes at once using an i64
                            // Str is normally two i32's, but in this special case, we can get away with fewer instructions
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
                    _ => not_supported_error(),
                }
            }

            _ => not_supported_error(),
        };
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
                    _ => internal_error!(
                        "Compiler bug: Invalid linker symbol info for string {:?}:\n{:?}",
                        string,
                        syminfo
                    ),
                }
            }

            None => {
                let const_segment_bytes = &mut self.module.data.segments[CONST_SEGMENT_INDEX].init;

                // Store the string in the data section
                // Prefix it with a special refcount value (treated as "infinity")
                // The string's `elements` field points at the data after the refcount
                let refcount_max_bytes: [u8; 4] = (REFCOUNT_MAX as i32).to_le_bytes();
                const_segment_bytes.extend_from_slice(&refcount_max_bytes);
                let elements_offset = const_segment_bytes.len() as u32;
                let elements_addr = elements_offset + CONST_SEGMENT_BASE_ADDR;
                const_segment_bytes.extend_from_slice(string.as_bytes());

                // Generate linker info
                // Just pick the symbol name from the first usage
                let name = self
                    .layout_ids
                    .get(sym, layout)
                    .to_symbol_string(sym, self.interns);
                let linker_symbol = SymInfo::Data(DataSymbol::Defined {
                    flags: 0,
                    name,
                    segment_index: CONST_SEGMENT_INDEX as u32,
                    segment_offset: elements_offset,
                    size: string.len() as u32,
                });

                let linker_sym_index = self.linker_symbols.len();
                self.constant_sym_index_map.insert(string, linker_sym_index);
                self.linker_symbols.push(linker_symbol);

                (linker_sym_index as u32, elements_addr)
            }
        }
    }

    fn create_struct(&mut self, sym: &Symbol, layout: &Layout<'a>, fields: &'a [Symbol]) {
        // TODO: we just calculated storage and now we're getting it out of a map
        // Not passing it as an argument because I'm trying to match Backend method signatures
        let storage = self.storage.get(sym).to_owned();

        if matches!(layout, Layout::Struct(_)) {
            match storage {
                StoredValue::StackMemory { location, size, .. } => {
                    if size > 0 {
                        let (local_id, struct_offset) =
                            location.local_and_offset(self.storage.stack_frame_pointer);
                        let mut field_offset = struct_offset;
                        for field in fields.iter() {
                            field_offset += self.storage.copy_value_to_memory(
                                &mut self.code_builder,
                                local_id,
                                field_offset,
                                *field,
                            );
                        }
                    } else {
                        // Zero-size struct. No code to emit.
                        // These values are purely conceptual, they only exist internally in the compiler
                    }
                }
                _ => internal_error!("Cannot create struct {:?} with storage {:?}", sym, storage),
            };
        } else {
            // Struct expression but not Struct layout => single element. Copy it.
            let field_storage = self.storage.get(&fields[0]).to_owned();
            self.storage
                .clone_value(&mut self.code_builder, &storage, &field_storage, fields[0]);
        }
    }

    /// Generate a call instruction to a Zig builtin function.
    /// And if we haven't seen it before, add an Import and linker data for it.
    /// Zig calls use LLVM's "fast" calling convention rather than our usual C ABI.
    fn call_zig_builtin(
        &mut self,
        name: &'a str,
        param_types: Vec<'a, ValueType>,
        ret_type: Option<ValueType>,
    ) {
        let num_wasm_args = param_types.len();
        let has_return_val = ret_type.is_some();

        let (fn_index, linker_symbol_index) = match self.builtin_sym_index_map.get(name) {
            Some(sym_idx) => match &self.linker_symbols[*sym_idx] {
                SymInfo::Function(WasmObjectSymbol::Imported { index, .. }) => {
                    (*index, *sym_idx as u32)
                }
                x => internal_error!("Invalid linker symbol for builtin {}: {:?}", name, x),
            },

            None => {
                // Wasm function signature
                let signature = Signature {
                    param_types,
                    ret_type,
                };
                let signature_index = self.module.types.insert(signature);

                // Declare it as an import since it comes from a different .o file
                let import_index = self.module.import.entries.len() as u32;
                let import = Import {
                    module: BUILTINS_IMPORT_MODULE_NAME,
                    name: name.to_string(),
                    description: ImportDesc::Func { signature_index },
                };
                self.module.import.entries.push(import);

                // Provide symbol information for the linker
                let sym_idx = self.linker_symbols.len();
                let sym_info = SymInfo::Function(WasmObjectSymbol::Imported {
                    flags: WASM_SYM_UNDEFINED,
                    index: import_index,
                });
                self.linker_symbols.push(sym_info);

                // Remember that we have created all of this data, and don't need to do it again
                self.builtin_sym_index_map.insert(name, sym_idx);

                (import_index, sym_idx as u32)
            }
        };

        self.code_builder
            .call(fn_index, linker_symbol_index, num_wasm_args, has_return_val);
    }

    /// Debug utility
    ///
    /// if self._debug_current_proc_is("#UserApp_foo_1") {
    ///     self.code_builder._debug_assert_i32(0x1234);
    /// }
    fn _debug_current_proc_is(&self, linker_name: &'static str) -> bool {
        let (_, linker_sym_index) = self.proc_symbols[self.debug_current_proc_index];
        let sym_info = &self.linker_symbols[linker_sym_index as usize];
        match sym_info {
            SymInfo::Function(WasmObjectSymbol::Defined { name, .. }) => name == linker_name,
            _ => false,
        }
    }
}
