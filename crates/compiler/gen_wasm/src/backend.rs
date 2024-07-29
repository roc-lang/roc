use bitvec::vec::BitVec;
use bumpalo::collections::{String, Vec};

use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_collections::all::MutMap;
use roc_error_macros::{internal_error, todo_lambda_erasure};
use roc_module::low_level::{LowLevel, LowLevelWrapperType};
use roc_module::symbol::{Interns, Symbol};
use roc_mono::code_gen_help::{CodeGenHelp, HelperOp, REFCOUNT_MAX};
use roc_mono::ir::{
    BranchInfo, CallType, CrashTag, Expr, JoinPointId, ListLiteralElement, Literal, ModifyRc,
    Param, Proc, ProcLayout, Stmt,
};
use roc_mono::layout::{
    Builtin, InLayout, Layout, LayoutIds, LayoutInterner, LayoutRepr, STLayoutInterner,
    TagIdIntType, UnionLayout,
};
use roc_std::RocDec;

use roc_wasm_module::linking::{DataSymbol, WasmObjectSymbol};
use roc_wasm_module::sections::{
    ConstExpr, DataMode, DataSegment, Export, Global, GlobalType, Import, ImportDesc, Limits,
    MemorySection, NameSection,
};
use roc_wasm_module::{
    round_up_to_alignment, Align, ExportType, LocalId, Signature, SymInfo, ValueType, WasmModule,
};

use crate::code_builder::CodeBuilder;
use crate::layout::{ReturnMethod, WasmLayout};
use crate::low_level::{call_higher_order_lowlevel, LowLevelCall};
use crate::storage::{AddressValue, Storage, StoredValue, StoredVarKind};
use crate::{
    copy_memory, CopyMemoryConfig, Env, DEBUG_SETTINGS, MEMORY_NAME, PTR_SIZE, PTR_TYPE, TARGET,
};

#[derive(Clone, Copy, Debug)]
pub enum ProcSource {
    Roc,
    Helper,
    /// Wrapper function for higher-order calls from Zig to Roc
    HigherOrderCompare(usize),
}

#[derive(Debug)]
pub struct ProcLookupData<'a> {
    pub name: Symbol,
    pub layout: ProcLayout<'a>,
    pub source: ProcSource,
}

pub struct WasmBackend<'a, 'r> {
    pub env: &'r Env<'a>,
    pub(crate) layout_interner: &'r mut STLayoutInterner<'a>,
    interns: &'r mut Interns,

    // Module-level data
    module: WasmModule<'a>,
    layout_ids: LayoutIds<'a>,
    pub fn_index_offset: u32,
    import_fn_count: u32,
    called_fns: BitVec<usize>,
    pub proc_lookup: Vec<'a, ProcLookupData<'a>>,
    host_lookup: Vec<'a, (&'a str, u32)>,
    helper_proc_gen: CodeGenHelp<'a>,
    can_relocate_heap: bool,

    // Function-level data
    pub code_builder: CodeBuilder<'a>,
    pub storage: Storage<'a>,

    /// how many blocks deep are we (used for jumps)
    block_depth: u32,
    joinpoint_label_map: MutMap<JoinPointId, (u32, Vec<'a, StoredValue>)>,
}

impl<'a, 'r> WasmBackend<'a, 'r> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        env: &'r Env<'a>,
        layout_interner: &'r mut STLayoutInterner<'a>,
        interns: &'r mut Interns,
        layout_ids: LayoutIds<'a>,
        proc_lookup: Vec<'a, ProcLookupData<'a>>,
        host_to_app_map: Vec<'a, (&'a str, u32)>,
        mut module: WasmModule<'a>,
        fn_index_offset: u32,
        helper_proc_gen: CodeGenHelp<'a>,
    ) -> Self {
        let has_heap_base = module.linking.find_internal_symbol("__heap_base").is_ok();
        let has_heap_end = module.linking.find_internal_symbol("__heap_end").is_ok();

        // We don't want to import any Memory or Tables
        module.import.imports.retain(|import| {
            !matches!(
                import.description,
                ImportDesc::Mem { .. } | ImportDesc::Table { .. }
            )
        });

        // Relocate calls from host to app
        // This will change function indices in the host, so we need to do it before get_host_function_lookup
        module.link_host_to_app_calls(env.arena, host_to_app_map);

        let host_lookup = module.get_host_function_lookup(env.arena);

        if module.names.function_names.is_empty() {
            module.names = NameSection::from_imports_and_linking_data(
                env.arena,
                &module.import,
                &module.linking,
            )
        }

        let import_fn_count = module.import.function_count();
        let host_function_count = import_fn_count
            + module.code.dead_import_dummy_count as usize
            + module.code.function_count as usize;
        let mut called_fns = BitVec::repeat(false, host_function_count);
        called_fns.extend(std::iter::repeat(true).take(proc_lookup.len()));

        WasmBackend {
            env,
            layout_interner,
            interns,

            // Module-level data
            module,
            layout_ids,
            fn_index_offset,
            import_fn_count: import_fn_count as u32,
            called_fns,
            proc_lookup,
            host_lookup,
            helper_proc_gen,
            can_relocate_heap: has_heap_base && has_heap_end,

            // Function-level data
            block_depth: 0,
            joinpoint_label_map: MutMap::default(),
            code_builder: CodeBuilder::new(env.arena),
            storage: Storage::new(env.arena),
        }
    }

    /// A Wasm module's memory is all in one contiguous block, unlike native executables.
    /// The standard layout is: constant data, then stack, then heap.
    /// Since they're all in one block, they can't grow independently. Only the highest one can grow.
    /// Also, there's no "invalid region" below the stack, so stack overflow will overwrite constants!
    /// TODO: Detect stack overflow in function prologue... at least in Roc code...
    fn set_memory_layout(&mut self, stack_size: u32) {
        let mut stack_heap_boundary = self.module.data.end_addr + stack_size;
        stack_heap_boundary = round_up_to_alignment!(stack_heap_boundary, MemorySection::PAGE_SIZE);

        // Stack pointer
        // This should be an imported global in the host
        // In the final binary, it's an internally defined global
        let sp_type = GlobalType {
            value_type: ValueType::I32,
            is_mutable: true,
        };
        {
            // Check that __stack_pointer is the only imported global
            // If there were more, we'd have to relocate them, and we don't
            let imported_globals = Vec::from_iter_in(
                self.module
                    .import
                    .imports
                    .iter()
                    .filter(|import| matches!(import.description, ImportDesc::Global { .. })),
                self.env.arena,
            );
            if imported_globals.len() != 1
                || imported_globals[0]
                    != &(Import {
                        module: "env",
                        name: "__stack_pointer",
                        description: ImportDesc::Global { ty: sp_type },
                    })
            {
                panic!("I can't link this host file. I expected it to have one imported Global called env.__stack_pointer")
            }
        }
        self.module
            .import
            .imports
            .retain(|import| !matches!(import.description, ImportDesc::Global { .. }));

        self.module.global.append(Global {
            ty: sp_type,
            init: ConstExpr::I32(stack_heap_boundary as i32),
        });

        // Set the initial size of the memory
        self.module.memory = MemorySection::new(
            self.env.arena,
            stack_heap_boundary + MemorySection::PAGE_SIZE,
        );

        // Export the memory so that JS can interact with it
        self.module.export.append(Export {
            name: MEMORY_NAME,
            ty: ExportType::Mem,
            index: 0,
        });

        // Set the constant that malloc uses to know where the heap begins
        // this should be done after we know how much constant data we have (e.g. string literals)
        if self.can_relocate_heap {
            self.module
                .relocate_internal_symbol("__heap_base", stack_heap_boundary)
                .unwrap();
            self.module
                .relocate_internal_symbol(
                    "__heap_end",
                    stack_heap_boundary + MemorySection::PAGE_SIZE,
                )
                .unwrap();
        }
    }

    /// If the host has some `extern` global variables, we need to create them in the final binary
    /// and make them visible to JavaScript by exporting them
    fn export_globals(&mut self) {
        for (sym_index, sym) in self.module.linking.symbol_table.iter().enumerate() {
            match sym {
                SymInfo::Data(DataSymbol::Imported { name, .. })
                    if *name != "__heap_base" && *name != "__heap_end" =>
                {
                    let global_value_addr = self.module.data.end_addr;
                    self.module.data.end_addr += PTR_SIZE;

                    self.module.reloc_code.apply_relocs_u32(
                        &mut self.module.code.bytes,
                        sym_index as u32,
                        global_value_addr,
                    );

                    let global_index = self.module.global.count;
                    self.module.global.append(Global {
                        ty: GlobalType {
                            value_type: ValueType::I32,
                            is_mutable: false,
                        },
                        init: ConstExpr::I32(global_value_addr as i32),
                    });

                    self.module.export.append(Export {
                        name,
                        ty: ExportType::Global,
                        index: global_index,
                    });
                }
                _ => {}
            }
        }
    }

    pub fn get_helpers(&mut self) -> Vec<'a, Proc<'a>> {
        self.helper_proc_gen.take_procs()
    }

    pub fn register_helper_proc(
        &mut self,
        symbol: Symbol,
        layout: ProcLayout<'a>,
        source: ProcSource,
    ) -> u32 {
        let proc_index = self.proc_lookup.len();
        let wasm_fn_index = self.fn_index_offset + proc_index as u32;

        let name = self
            .layout_ids
            .get_toplevel(symbol, &layout)
            .to_symbol_string(symbol, self.interns);
        let name = String::from_str_in(&name, self.env.arena).into_bump_str();

        self.proc_lookup.push(ProcLookupData {
            name: symbol,
            layout,
            source,
        });

        self.called_fns.push(true);

        let linker_symbol = SymInfo::Function(WasmObjectSymbol::ExplicitlyNamed {
            flags: 0,
            index: wasm_fn_index,
            name,
        });
        self.module.linking.symbol_table.push(linker_symbol);

        wasm_fn_index
    }

    pub fn finalize(mut self) -> (WasmModule<'a>, BitVec<usize>) {
        self.set_memory_layout(self.env.stack_bytes);
        self.export_globals();

        self.maybe_call_host_main();
        let fn_table_size = 1 + self.module.element.max_table_index();
        self.module.table.function_table.limits = Limits::MinMax(fn_table_size, fn_table_size);
        (self.module, self.called_fns)
    }

    /// If the host has a `main` function then we need to insert a `_start` to call it.
    /// This is something linkers do, and this backend is also a linker!
    fn maybe_call_host_main(&mut self) {
        const START: &str = "_start";

        // If _start exists, just export it. Trust it to call main.
        if let Ok(start_sym_index) = self.module.linking.find_internal_symbol(START) {
            let start_fn_index = match self.module.linking.symbol_table[start_sym_index] {
                SymInfo::Function(WasmObjectSymbol::ExplicitlyNamed { index, .. }) => index,
                _ => panic!("linker symbol `{START}` is not a function"),
            };
            self.module.export.append(Export {
                name: START,
                ty: ExportType::Func,
                index: start_fn_index,
            });
            return;
        }

        // _start doesn't exist. Check for a `main` and create a _start that calls it.
        // Note: if `main` is prefixed with some other module name, we won't find it!
        let main_symbol_index = match self.module.linking.find_internal_symbol("main") {
            Ok(x) => x,
            Err(_) => return,
        };

        let main_fn_index: u32 = match &self.module.linking.symbol_table[main_symbol_index] {
            SymInfo::Function(WasmObjectSymbol::ExplicitlyNamed { index, .. }) => *index,
            _ => {
                return;
            }
        };

        self.module.add_function_signature(Signature {
            param_types: bumpalo::vec![in self.env.arena],
            ret_type: None,
        });

        self.module.export.append(Export {
            name: START,
            ty: ExportType::Func,
            index: self.module.code.function_count,
        });

        self.code_builder.i32_const(0); // argc=0
        self.code_builder.i32_const(0); // argv=NULL
        self.code_builder.call(main_fn_index);
        self.code_builder.drop_();
        self.code_builder.build_fn_header_and_footer(&[], 0, None);
        self.reset();

        self.called_fns.set(main_fn_index as usize, true);
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

    pub fn get_fn_ptr(&mut self, fn_index: u32) -> i32 {
        self.module.element.get_or_insert_fn(fn_index)
    }

    /// Create an IR Symbol for an anonymous value (such as ListLiteral)
    pub fn create_symbol(&mut self, debug_name: &str) -> Symbol {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        let ident_id = ident_ids.add_str(debug_name);
        Symbol::new(self.env.module_id, ident_id)
    }

    /// Reset function-level data
    fn reset(&mut self) {
        self.code_builder.insert_into_module(&mut self.module);
        self.code_builder.clear();
        self.storage.clear();
        self.joinpoint_label_map.clear();
        assert_eq!(self.block_depth, 0);
    }

    /**********************************************************

            PROCEDURE

    ***********************************************************/

    pub fn build_proc(&mut self, proc: &Proc<'a>) {
        if DEBUG_SETTINGS.proc_start_end {
            println!("\ngenerating procedure {:?}\n", proc.name);
        }

        self.append_proc_debug_name(proc.name.name());

        self.start_proc(proc);

        self.stmt(&proc.body);

        self.finalize_proc();
        self.reset();

        if DEBUG_SETTINGS.proc_start_end {
            println!("\nfinished generating {:?}\n", proc.name);
        }
    }

    fn start_proc(&mut self, proc: &Proc<'a>) {
        use ReturnMethod::*;
        let ret_layout = WasmLayout::new(self.layout_interner, proc.ret_layout);

        let ret_type = match ret_layout.return_method() {
            Primitive(ty, _) => Some(ty),
            NoReturnValue => None,
            WriteToPointerArg => {
                self.storage.arg_types.push(PTR_TYPE);
                None
            }
        };

        // Create a block so we can exit the function without skipping stack frame "pop" code.
        // We never use the `return` instruction. Instead, we break from this block.
        self.start_block();

        self.storage.allocate_args(
            self.layout_interner,
            proc.args,
            &mut self.code_builder,
            self.env.arena,
        );

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

        if DEBUG_SETTINGS.storage_map {
            println!("\nStorage:");
            for (sym, storage) in self.storage.symbol_storage_map.iter() {
                println!("{sym:?} => {storage:?}");
            }
        }
    }

    fn append_proc_debug_name(&mut self, sym: Symbol) {
        let proc_index = self
            .proc_lookup
            .iter()
            .position(|ProcLookupData { name, .. }| *name == sym)
            .unwrap();
        let wasm_fn_index = self.fn_index_offset + proc_index as u32;

        let name = String::from_str_in(sym.as_str(self.interns), self.env.arena).into_bump_str();
        self.module.names.append_function(wasm_fn_index, name);
    }

    /// Build a wrapper around a Roc comparison proc so that it can be called from higher-order Zig builtins.
    /// Comparison procedure signature is: closure_data, a, b -> Order (u8)
    ///
    /// The generic Zig code passes *pointers* to all of the argument values (e.g. on the heap in a List).
    /// Numbers up to 64 bits are passed by value, so we need to load them from the provided pointer.
    /// Everything else is passed by reference, so we can just pass the pointer through.
    pub fn build_higher_order_compare(
        &mut self,
        wrapper_lookup_idx: usize,
        inner_lookup_idx: usize,
    ) {
        use ValueType::*;

        let ProcLookupData {
            name: wrapper_name,
            layout: wrapper_proc_layout,
            ..
        } = self.proc_lookup[wrapper_lookup_idx];
        let closure_data_layout = wrapper_proc_layout.arguments[0];
        let value_layout = wrapper_proc_layout.arguments[1];

        if self.layout_interner.stack_size(closure_data_layout) > 0 {
            self.code_builder.get_local(LocalId(0));
        }

        let inner_layout = match self.layout_interner.get_repr(value_layout) {
            LayoutRepr::Ptr(inner) => inner,
            x => internal_error!("Expected a Ptr layout, got {:?}", x),
        };
        self.code_builder.get_local(LocalId(1));
        self.dereference_boxed_value(inner_layout);
        self.code_builder.get_local(LocalId(2));
        self.dereference_boxed_value(inner_layout);

        // Call the wrapped inner function
        let inner_wasm_fn_index = self.fn_index_offset + inner_lookup_idx as u32;
        self.code_builder.call(inner_wasm_fn_index);

        // Write empty function header (local variables array with zero length)
        self.code_builder.build_fn_header_and_footer(&[], 0, None);

        self.module.add_function_signature(Signature {
            param_types: bumpalo::vec![in self.env.arena; I32; 3],
            ret_type: Some(ValueType::I32),
        });

        self.append_proc_debug_name(wrapper_name);
        self.reset();
    }

    fn dereference_boxed_value(&mut self, inner: InLayout) {
        use Align::*;

        match self.layout_interner.get_repr(inner) {
            LayoutRepr::Builtin(Builtin::Int(IntWidth::U8 | IntWidth::I8)) => {
                self.code_builder.i32_load8_u(Bytes1, 0);
            }
            LayoutRepr::Builtin(Builtin::Int(IntWidth::U16 | IntWidth::I16)) => {
                self.code_builder.i32_load16_u(Bytes2, 0);
            }
            LayoutRepr::Builtin(Builtin::Int(IntWidth::U32 | IntWidth::I32)) => {
                self.code_builder.i32_load(Bytes4, 0);
            }
            LayoutRepr::Builtin(Builtin::Int(IntWidth::U64 | IntWidth::I64)) => {
                self.code_builder.i64_load(Bytes8, 0);
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)) => {
                self.code_builder.f32_load(Bytes4, 0);
            }
            LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                self.code_builder.f64_load(Bytes8, 0);
            }
            LayoutRepr::Builtin(Builtin::Bool) => {
                self.code_builder.i32_load8_u(Bytes1, 0);
            }
            _ => {
                // Any other layout is a pointer, which we've already loaded. Nothing to do!
            }
        }
    }

    /**********************************************************

            STATEMENTS

    ***********************************************************/

    fn stmt(&mut self, stmt: &Stmt<'a>) {
        match stmt {
            Stmt::Let(_, _, _, _) => self.stmt_let(stmt),

            Stmt::Ret(sym) => self.stmt_ret(*sym),

            Stmt::Switch {
                cond_symbol,
                cond_layout,
                branches,
                default_branch,
                ret_layout: _,
            } => self.stmt_switch(*cond_symbol, *cond_layout, branches, default_branch),

            Stmt::Join {
                id,
                parameters,
                body,
                remainder,
            } => self.stmt_join(*id, parameters, body, remainder),

            Stmt::Jump(id, arguments) => self.stmt_jump(*id, arguments),

            Stmt::Refcounting(modify, following) => match modify {
                ModifyRc::Free(symbol) => self.stmt_refcounting_free(*symbol, following),
                _ => self.stmt_refcounting(modify, following),
            },

            Stmt::Dbg { .. } => todo!("dbg is not implemented in the wasm backend"),
            Stmt::Expect { .. } => todo!("expect is not implemented in the wasm backend"),
            Stmt::ExpectFx { .. } => todo!("expect-fx is not implemented in the wasm backend"),

            Stmt::Crash(sym, tag) => self.stmt_crash(*sym, *tag),
        }
    }

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

    fn stmt_let(&mut self, stmt: &Stmt<'a>) {
        let mut current_stmt = stmt;
        while let Stmt::Let(sym, expr, layout, following) = current_stmt {
            if DEBUG_SETTINGS.let_stmt_ir {
                print!("\nlet {:?} = {}", sym, expr.to_pretty(200, true));
            }

            let kind = match following {
                Stmt::Ret(ret_sym) if *sym == *ret_sym => StoredVarKind::ReturnValue,
                _ => StoredVarKind::Variable,
            };

            self.stmt_let_store_expr(*sym, *layout, expr, kind);

            current_stmt = *following;
        }

        self.stmt(current_stmt);
    }

    fn stmt_let_store_expr(
        &mut self,
        sym: Symbol,
        layout: InLayout<'a>,
        expr: &Expr<'a>,
        kind: StoredVarKind,
    ) {
        let sym_storage = self
            .storage
            .allocate_var(self.layout_interner, layout, sym, kind);

        self.expr(sym, expr, layout, &sym_storage);

        if let StoredValue::Local { local_id, .. } = sym_storage {
            if !self.code_builder.is_set(local_id) {
                self.code_builder.set_local(local_id);
            }
        }
    }

    fn stmt_ret(&mut self, sym: Symbol) {
        use crate::storage::StoredValue::*;

        match self.storage.get(&sym) {
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
                self.storage.load_symbols(&mut self.code_builder, &[sym]);

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

    fn stmt_switch(
        &mut self,
        cond_symbol: Symbol,
        cond_layout: InLayout<'a>,
        branches: &'a [(u64, BranchInfo<'a>, Stmt<'a>)],
        default_branch: &(BranchInfo<'a>, &'a Stmt<'a>),
    ) {
        // NOTE currently implemented as a series of conditional jumps
        // We may be able to improve this in the future with `Select`
        // or `BrTable`

        // create a block for each branch except the default
        for _ in 0..branches.len() {
            self.start_block()
        }

        let is_bool = matches!(cond_layout, Layout::BOOL);
        let cond_type = WasmLayout::new(self.layout_interner, cond_layout).arg_types()[0];

        // then, we jump whenever the value under scrutiny is equal to the value of a branch
        for (i, (value, _, _)) in branches.iter().enumerate() {
            // put the cond_symbol on the top of the stack
            self.storage
                .load_symbols(&mut self.code_builder, &[cond_symbol]);

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
                        self.code_builder.f64_const(f64::from_bits(*value));
                        self.code_builder.f64_eq();
                    }
                }
            }

            // "break" out of `i` surrounding blocks
            self.code_builder.br_if(i as u32);
        }

        // if we never jumped because a value matched, we're in the default case
        self.stmt(default_branch.1);

        // now put in the actual body of each branch in order
        // (the first branch would have broken out of 1 block,
        // hence we must generate its code first)
        for (_, _, branch) in branches.iter() {
            self.end_block();

            self.stmt(branch);
        }
    }

    fn stmt_join(
        &mut self,
        id: JoinPointId,
        parameters: &'a [Param<'a>],
        body: &'a Stmt<'a>,
        remainder: &'a Stmt<'a>,
    ) {
        // make locals for join pointer parameters
        let mut jp_param_storages = Vec::with_capacity_in(parameters.len(), self.env.arena);
        for parameter in parameters.iter() {
            let param_storage = self.storage.allocate_var(
                self.layout_interner,
                parameter.layout,
                parameter.symbol,
                StoredVarKind::Variable,
            );
            jp_param_storages.push(param_storage);
        }

        self.start_block();

        self.joinpoint_label_map
            .insert(id, (self.block_depth, jp_param_storages));

        self.stmt(remainder);

        self.end_block();
        self.start_loop();

        self.stmt(body);

        // ends the loop
        self.end_block();
    }

    fn stmt_jump(&mut self, id: JoinPointId, arguments: &'a [Symbol]) {
        let (target, param_storages) = self.joinpoint_label_map[&id].clone();

        for (arg_symbol, param_storage) in arguments.iter().zip(param_storages.iter()) {
            let arg_storage = self.storage.get(arg_symbol).clone();
            self.storage
                .clone_value(&mut self.code_builder, param_storage, &arg_storage);
        }

        // jump
        let levels = self.block_depth - target;
        self.code_builder.br(levels);
    }

    fn stmt_refcounting(&mut self, modify: &ModifyRc, following: &'a Stmt<'a>) {
        let value = modify.get_symbol();
        let layout = self.storage.symbol_layouts[&value];

        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        let (rc_stmt, new_specializations) = self.helper_proc_gen.expand_refcount_stmt(
            ident_ids,
            self.layout_interner,
            layout,
            modify,
            following,
        );

        if false {
            self.register_symbol_debug_names();
            println!(
                "## rc_stmt:\n{}\n{:?}",
                rc_stmt.to_pretty(self.layout_interner, 200, true),
                rc_stmt
            );
        }

        // If any new specializations were created, register their symbol data
        for (spec_sym, spec_layout) in new_specializations.into_iter() {
            self.register_helper_proc(spec_sym, spec_layout, ProcSource::Helper);
        }

        self.stmt(rc_stmt);
    }

    fn stmt_refcounting_free(&mut self, value: Symbol, following: &'a Stmt<'a>) {
        let layout = self.storage.symbol_layouts[&value];
        debug_assert!(!matches!(self.layout_interner.get_repr(layout), LayoutRepr::Builtin(Builtin::List(_))), "List are no longer safe to refcount through pointer alone. They must go through the zig bitcode functions");

        let alignment = self.layout_interner.allocation_alignment_bytes(layout);

        // Get pointer and offset
        let value_storage = self.storage.get(&value).to_owned();
        let (tag_local_id, tag_offset) = match value_storage {
            StoredValue::StackMemory { location, .. } => {
                location.local_and_offset(self.storage.stack_frame_pointer)
            }
            StoredValue::Local { local_id, .. } => (local_id, 0),
        };

        // load pointer, and add the offset to the pointer
        self.code_builder.get_local(tag_local_id);

        if tag_offset > 0 {
            self.code_builder.i32_const(tag_offset as i32);
            self.code_builder.i32_add();
        }

        // NOTE: UTILS_FREE_DATA_PTR clears any tag id bits

        // push the allocation's alignment
        self.code_builder.i32_const(alignment as i32);

        // elems_refcounted (always false except for list which are refcounted differently)
        self.code_builder.i32_const(false as i32);

        self.call_host_fn_after_loading_args(bitcode::UTILS_FREE_DATA_PTR);

        self.stmt(following);
    }

    pub fn stmt_internal_error(&mut self, msg: &'a str) {
        let msg_sym = self.create_symbol("panic_str");
        let msg_storage = self.storage.allocate_var(
            self.layout_interner,
            Layout::STR,
            msg_sym,
            StoredVarKind::Variable,
        );

        // Store the message as a RocStr on the stack
        let (local_id, offset) = match msg_storage {
            StoredValue::StackMemory { location, .. } => {
                location.local_and_offset(self.storage.stack_frame_pointer)
            }
            _ => internal_error!("String must always have stack memory"),
        };
        self.expr_string_literal(msg, local_id, offset);

        self.stmt_crash(msg_sym, CrashTag::Roc);
    }

    pub fn stmt_crash(&mut self, msg: Symbol, tag: CrashTag) {
        // load the pointer
        self.storage.load_symbols(&mut self.code_builder, &[msg]);
        self.code_builder.i32_const(tag as _);
        self.call_host_fn_after_loading_args("roc_panic");

        self.code_builder.unreachable_();
    }

    /**********************************************************

            EXPRESSIONS

    ***********************************************************/

    fn expr(&mut self, sym: Symbol, expr: &Expr<'a>, layout: InLayout<'a>, storage: &StoredValue) {
        match expr {
            Expr::Literal(lit) => self.expr_literal(lit, storage),

            Expr::NullPointer => self.expr_null_pointer(),

            Expr::Call(roc_mono::ir::Call {
                call_type,
                arguments,
            }) => self.expr_call(call_type, arguments, sym, layout, storage),

            Expr::Struct(fields) => self.expr_struct(sym, layout, storage, fields),

            Expr::StructAtIndex {
                index,
                field_layouts,
                structure,
            } => self.expr_struct_at_index(sym, *index, field_layouts, *structure),

            Expr::Array { elems, elem_layout } => {
                self.expr_array(sym, storage, *elem_layout, elems)
            }

            Expr::EmptyArray => self.expr_empty_array(sym, storage),

            Expr::Tag {
                tag_layout: union_layout,
                tag_id,
                arguments,
                reuse,
            } => {
                let reuse = reuse.map(|ru| ru.symbol);
                self.expr_tag(union_layout, *tag_id, arguments, storage, reuse)
            }

            Expr::GetTagId {
                structure,
                union_layout,
            } => self.expr_get_tag_id(*structure, union_layout, storage),

            Expr::UnionAtIndex {
                structure,
                tag_id,
                union_layout,
                index,
            } => self.expr_union_at_index(*structure, *tag_id, union_layout, *index, sym),

            Expr::GetElementPointer {
                structure,
                union_layout,
                indices,
                ..
            } => {
                debug_assert!(indices.len() >= 2);
                self.expr_union_field_ptr_at_index(
                    *structure,
                    indices[0] as u16,
                    union_layout,
                    indices[1],
                    storage,
                )
            }

            Expr::FunctionPointer { .. } => todo_lambda_erasure!(),
            Expr::ErasedMake { .. } => todo_lambda_erasure!(),
            Expr::ErasedLoad { .. } => todo_lambda_erasure!(),

            Expr::Reset { symbol: arg, .. } => self.expr_reset(*arg, sym, storage),

            Expr::ResetRef { symbol: arg, .. } => self.expr_resetref(*arg, sym, storage),

            Expr::Alloca {
                initializer,
                element_layout,
            } => self.expr_alloca(*initializer, *element_layout, storage),

            Expr::RuntimeErrorFunction(_) => {
                todo!("Expression `{}`", expr.to_pretty(100, false))
            }
        }
    }

    /*******************************************************************
     * Literals
     *******************************************************************/

    fn expr_literal(&mut self, lit: &Literal<'a>, storage: &StoredValue) {
        let invalid_error = || {
            internal_error!(
                "Literal value {:?} implements invalid storage {:?}",
                lit,
                storage
            )
        };

        match storage {
            StoredValue::Local {
                value_type,
                local_id,
                ..
            } => {
                match (lit, value_type) {
                    (Literal::Float(x), ValueType::F64) => self.code_builder.f64_const(*x),
                    (Literal::Float(x), ValueType::F32) => self.code_builder.f32_const(*x as f32),
                    (Literal::Int(x), ValueType::I64) => {
                        self.code_builder.i64_const(i128::from_ne_bytes(*x) as i64)
                    }
                    (Literal::Int(x), ValueType::I32) => {
                        self.code_builder.i32_const(i128::from_ne_bytes(*x) as i32)
                    }
                    (Literal::Bool(x), ValueType::I32) => self.code_builder.i32_const(*x as i32),
                    (Literal::Byte(x), ValueType::I32) => self.code_builder.i32_const(*x as i32),
                    _ => invalid_error(),
                };
                self.code_builder.set_local(*local_id);
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
                    Literal::Decimal(bytes) => {
                        let (upper_bits, lower_bits) = RocDec::from_ne_bytes(*bytes).as_bits();
                        write128(lower_bits as i64, upper_bits);
                    }
                    Literal::Int(x) | Literal::U128(x) => {
                        let lower_bits = (i128::from_ne_bytes(*x) & 0xffff_ffff_ffff_ffff) as i64;
                        let upper_bits = (i128::from_ne_bytes(*x) >> 64) as i64;
                        write128(lower_bits, upper_bits);
                    }
                    Literal::Float(_) => {
                        // Also not implemented in LLVM backend (nor in Rust!)
                        todo!("f128 type");
                    }
                    Literal::Str(string) => {
                        let (local_id, offset) =
                            location.local_and_offset(self.storage.stack_frame_pointer);

                        self.expr_string_literal(string, local_id, offset);
                    }
                    // Bools and bytes should not be stored in the stack frame
                    Literal::Bool(_) | Literal::Byte(_) => invalid_error(),
                }
            }
        };
    }

    fn expr_string_literal(&mut self, string: &str, local_id: LocalId, offset: u32) {
        let len = string.len();
        if len < 12 {
            // Construct the bytes of the small string
            let mut bytes = [0; 12];
            bytes[0..len].clone_from_slice(string.as_bytes());
            bytes[11] = 0x80 | (len as u8);

            // Transform into two integers, to minimise number of instructions
            let bytes_split: &([u8; 8], [u8; 4]) = unsafe { std::mem::transmute(&bytes) };
            let int64 = i64::from_le_bytes(bytes_split.0);
            let int32 = i32::from_le_bytes(bytes_split.1);

            // Write the integers to memory
            self.code_builder.get_local(local_id);
            self.code_builder.i64_const(int64);
            self.code_builder.i64_store(Align::Bytes4, offset);
            self.code_builder.get_local(local_id);
            self.code_builder.i32_const(int32);
            self.code_builder.i32_store(Align::Bytes4, offset + 8);
        } else {
            let bytes = string.as_bytes();
            let elements_addr = self.store_bytes_in_data_section(bytes);

            // ptr
            self.code_builder.get_local(local_id);
            self.code_builder.i32_const(elements_addr as i32);
            self.code_builder.i32_store(Align::Bytes4, offset);

            // len
            self.code_builder.get_local(local_id);
            self.code_builder.i32_const(string.len() as i32);
            self.code_builder.i32_store(Align::Bytes4, offset + 4);

            // capacity
            self.code_builder.get_local(local_id);
            self.code_builder.i32_const(string.len() as i32);
            self.code_builder.i32_store(Align::Bytes4, offset + 8);
        };
    }

    /// Create a string constant in the module data section
    /// Return the data we need for code gen: linker symbol index and memory address
    fn store_bytes_in_data_section(&mut self, bytes: &[u8]) -> u32 {
        // Place the segment at a 4-byte aligned offset
        let segment_addr = round_up_to_alignment!(self.module.data.end_addr, PTR_SIZE);
        let elements_addr = segment_addr + PTR_SIZE;
        let length_with_refcount = 4 + bytes.len();
        self.module.data.end_addr = segment_addr + length_with_refcount as u32;

        let mut segment = DataSegment {
            mode: DataMode::active_at(segment_addr),
            init: Vec::with_capacity_in(length_with_refcount, self.env.arena),
        };

        // Prefix the string bytes with "infinite" refcount
        let refcount_max_bytes: [u8; 4] = (REFCOUNT_MAX as i32).to_le_bytes();
        segment.init.extend_from_slice(&refcount_max_bytes);
        segment.init.extend_from_slice(bytes);

        self.module.data.append_segment(segment);

        elements_addr
    }

    fn expr_null_pointer(&mut self) {
        self.code_builder.i32_const(0);
    }

    /*******************************************************************
     * Call expressions
     *******************************************************************/

    fn expr_call(
        &mut self,
        call_type: &CallType<'a>,
        arguments: &'a [Symbol],
        ret_sym: Symbol,
        ret_layout: InLayout<'a>,
        ret_storage: &StoredValue,
    ) {
        match call_type {
            CallType::ByName {
                name: func_sym,
                arg_layouts,
                ret_layout: result,
                ..
            } => {
                let proc_layout = ProcLayout {
                    arguments: arg_layouts,
                    result: *result,
                    niche: func_sym.niche(),
                };
                self.expr_call_by_name(
                    func_sym.name(),
                    &proc_layout,
                    arguments,
                    ret_sym,
                    ret_layout,
                    ret_storage,
                )
            }

            CallType::ByPointer { .. } => {
                todo_lambda_erasure!()
            }

            CallType::LowLevel { op: lowlevel, .. } => {
                self.expr_call_low_level(*lowlevel, arguments, ret_sym, ret_layout, ret_storage)
            }

            CallType::HigherOrder(higher_order_lowlevel) => {
                call_higher_order_lowlevel(self, ret_sym, &ret_layout, higher_order_lowlevel)
            }

            CallType::Foreign {
                foreign_symbol,
                ret_layout,
            } => {
                let name = foreign_symbol.as_str();
                let wasm_layout = WasmLayout::new(self.layout_interner, *ret_layout);
                self.storage.load_symbols_for_call(
                    &mut self.code_builder,
                    arguments,
                    ret_sym,
                    &wasm_layout,
                );
                self.call_host_fn_after_loading_args(name)
            }
        }
    }

    fn expr_call_by_name(
        &mut self,
        func_sym: Symbol,
        proc_layout: &ProcLayout<'a>,
        arguments: &'a [Symbol],
        ret_sym: Symbol,
        ret_layout: InLayout<'a>,
        ret_storage: &StoredValue,
    ) {
        let wasm_layout = WasmLayout::new(self.layout_interner, ret_layout);

        // If this function is just a lowlevel wrapper, then inline it
        if let LowLevelWrapperType::CanBeReplacedBy(lowlevel) =
            LowLevelWrapperType::from_symbol(func_sym)
        {
            return self.expr_call_low_level(lowlevel, arguments, ret_sym, ret_layout, ret_storage);
        }

        self.storage.load_symbols_for_call(
            &mut self.code_builder,
            arguments,
            ret_sym,
            &wasm_layout,
        );

        let roc_proc_index = self
            .proc_lookup
            .iter()
            .position(|lookup| lookup.name == func_sym && &lookup.layout == proc_layout)
            .unwrap_or_else(|| {
                internal_error!(
                    "Could not find procedure {:?} with proc_layout:\n{:#?}\nKnown procedures:\n{:#?}",
                    func_sym,
                    proc_layout,
                    self.proc_lookup
                );
            });

        let wasm_fn_index = self.fn_index_offset + roc_proc_index as u32;

        self.code_builder.call(wasm_fn_index);
    }

    fn expr_call_low_level(
        &mut self,
        lowlevel: LowLevel,
        arguments: &'a [Symbol],
        ret_symbol: Symbol,
        ret_layout: InLayout<'a>,
        ret_storage: &StoredValue,
    ) {
        let low_level_call = LowLevelCall {
            lowlevel,
            arguments,
            ret_symbol,
            ret_layout,
            ret_layout_raw: self.layout_interner.get_repr(ret_layout),
            ret_storage: ret_storage.to_owned(),
        };
        low_level_call.generate(self);
    }

    /// Generate a call instruction to a host function or Zig builtin.
    pub fn call_host_fn_after_loading_args(&mut self, name: &str) {
        let (_, fn_index) = self
            .host_lookup
            .iter()
            .find(|(fn_name, _)| *fn_name == name)
            .unwrap_or_else(|| panic!("The Roc app tries to call `{name}` but I can't find it!"));

        self.called_fns.set(*fn_index as usize, true);

        if *fn_index < self.import_fn_count {
            self.code_builder.call_import(*fn_index);
        } else {
            self.code_builder.call(*fn_index);
        }
    }

    /// Call a helper procedure that implements `==` for a data structure (not numbers or Str)
    /// If this is the first call for this Layout, it will generate the IR for the procedure.
    /// Call stack is expr_call_low_level -> LowLevelCall::generate -> call_eq_specialized
    /// It's a bit circuitous, but the alternative is to give low_level.rs `pub` access to
    /// interns, helper_proc_gen, and expr(). That just seemed all wrong.
    pub fn call_eq_specialized(
        &mut self,
        arguments: &'a [Symbol],
        arg_layout: InLayout<'a>,
        ret_symbol: Symbol,
        ret_storage: &StoredValue,
    ) {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        // Get an IR expression for the call to the specialized procedure
        let (specialized_call_expr, new_specializations) = self
            .helper_proc_gen
            .call_specialized_equals(ident_ids, self.layout_interner, arg_layout, arguments);

        // If any new specializations were created, register their symbol data
        for (spec_sym, spec_layout) in new_specializations.into_iter() {
            self.register_helper_proc(spec_sym, spec_layout, ProcSource::Helper);
        }

        // Generate Wasm code for the IR call expression
        self.expr(
            ret_symbol,
            self.env.arena.alloc(specialized_call_expr),
            Layout::BOOL,
            ret_storage,
        );
    }

    /*******************************************************************
     * Structs
     *******************************************************************/

    fn expr_struct(
        &mut self,
        sym: Symbol,
        layout: InLayout<'a>,
        storage: &StoredValue,
        fields: &'a [Symbol],
    ) {
        match self.layout_interner.get_repr(layout) {
            LayoutRepr::Struct { .. } => {
                match storage {
                    StoredValue::StackMemory { location, size, .. } => {
                        if *size > 0 {
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
                    _ => {
                        internal_error!("Cannot create struct {:?} with storage {:?}", sym, storage)
                    }
                };
            }
            LayoutRepr::LambdaSet(lambdaset) => {
                self.expr_struct(sym, lambdaset.runtime_representation(), storage, fields)
            }
            _ => {
                if !fields.is_empty() {
                    // Struct expression but not Struct layout => single element. Copy it.
                    let field_storage = self.storage.get(&fields[0]).to_owned();
                    self.storage
                        .clone_value(&mut self.code_builder, storage, &field_storage);
                } else {
                    // Empty record. Nothing to do.
                }
            }
        }
    }

    fn expr_struct_at_index(
        &mut self,
        sym: Symbol,
        index: u64,
        field_layouts: &'a [InLayout<'a>],
        structure: Symbol,
    ) {
        let (from_addr_val, mut offset) = match self.storage.get(&structure) {
            StoredValue::StackMemory { location, .. } => {
                let (local_id, offset) =
                    location.local_and_offset(self.storage.stack_frame_pointer);
                (AddressValue::NotLoaded(local_id), offset)
            }

            StoredValue::Local {
                value_type,
                local_id,
                ..
            } => {
                debug_assert!(matches!(value_type, ValueType::I32));
                (AddressValue::NotLoaded(*local_id), 0)
            }
        };
        for field in field_layouts.iter().take(index as usize) {
            offset += self.layout_interner.stack_size(*field);
        }
        self.storage
            .copy_value_from_memory(&mut self.code_builder, sym, from_addr_val, offset);
    }

    /*******************************************************************
     * Arrays
     *******************************************************************/

    pub fn expr_array(
        &mut self,
        sym: Symbol,
        storage: &StoredValue,
        elem_layout: InLayout<'a>,
        elems: &'a [ListLiteralElement<'a>],
    ) {
        if let StoredValue::StackMemory { location, .. } = storage {
            let size = self.layout_interner.stack_size(elem_layout) * (elems.len() as u32);

            // Allocate heap space and store its address in a local variable
            let heap_local_id = self.storage.create_anonymous_local(PTR_TYPE);
            let heap_alignment = self.layout_interner.alignment_bytes(elem_layout);
            let elems_refcounted = self.layout_interner.contains_refcounted(elem_layout);
            self.allocate_with_refcount(size, heap_alignment, elems_refcounted);
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
            self.code_builder
                .i32_store(Align::Bytes4, stack_offset + 4 * Builtin::WRAPPER_LEN);

            // capacity of the list
            self.code_builder.get_local(stack_local_id);
            self.code_builder.i32_const(elems.len() as i32);
            self.code_builder
                .i32_store(Align::Bytes4, stack_offset + 4 * Builtin::WRAPPER_CAPACITY);

            let mut elem_offset = 0;

            for (i, elem) in elems.iter().enumerate() {
                let elem_sym = match elem {
                    ListLiteralElement::Literal(lit) => {
                        // This has no Symbol but our storage methods expect one.
                        // Let's just pretend it was defined in a `Let`.
                        let debug_name = format!("{sym:?}_{i}");
                        let elem_sym = self.create_symbol(&debug_name);
                        let expr = Expr::Literal(*lit);

                        self.stmt_let_store_expr(
                            elem_sym,
                            elem_layout,
                            &expr,
                            StoredVarKind::Variable,
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

    fn expr_empty_array(&mut self, sym: Symbol, storage: &StoredValue) {
        if let StoredValue::StackMemory { location, .. } = storage {
            let (local_id, offset) = location.local_and_offset(self.storage.stack_frame_pointer);

            // Store 12 bytes of zeros { elements: null, length: 0, capacity: 0 }
            debug_assert_eq!(Builtin::LIST_WORDS, 3);
            self.code_builder.get_local(local_id);
            self.code_builder.i64_const(0);
            self.code_builder.i64_store(Align::Bytes4, offset);
            self.code_builder.get_local(local_id);
            self.code_builder.i32_const(0);
            self.code_builder.i32_store(Align::Bytes4, offset + 8);
        } else {
            internal_error!("Unexpected storage for {:?}", sym)
        }
    }

    /*******************************************************************
     * Tag Unions
     *******************************************************************/

    fn expr_tag(
        &mut self,
        union_layout: &UnionLayout<'a>,
        tag_id: TagIdIntType,
        arguments: &'a [Symbol],
        stored: &StoredValue,
        maybe_reused: Option<Symbol>,
    ) {
        if union_layout.tag_is_null(tag_id) {
            self.code_builder.i32_const(0);
            return;
        }

        let stores_tag_id_as_data = union_layout.stores_tag_id_as_data(TARGET);
        let stores_tag_id_in_pointer = union_layout.stores_tag_id_in_pointer(TARGET);
        let (data_size, data_alignment) =
            union_layout.data_size_and_alignment(self.layout_interner);

        let (local_id, data_offset) = match stored {
            StoredValue::StackMemory { location, .. } => {
                location.local_and_offset(self.storage.stack_frame_pointer)
            }
            StoredValue::Local { local_id, .. } => {
                // Tag is stored as a heap pointer.
                if let Some(reused) = maybe_reused {
                    // Reuse an existing heap allocation, if one is available (not NULL at runtime)
                    self.storage.load_symbols(&mut self.code_builder, &[reused]);
                    self.code_builder.if_();
                    {
                        self.storage.load_symbols(&mut self.code_builder, &[reused]);
                        self.code_builder.set_local(*local_id);
                    }
                    self.code_builder.else_();
                    {
                        self.allocate_with_refcount(data_size, data_alignment, false);
                        self.code_builder.set_local(*local_id);
                    }
                    self.code_builder.end();
                } else {
                    // Call the allocator to get a memory address.
                    self.allocate_with_refcount(data_size, data_alignment, false);
                    self.code_builder.set_local(*local_id);
                }
                (*local_id, 0)
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
            let id_offset = data_offset + union_layout.tag_id_offset(self.layout_interner).unwrap();

            let id_align = union_layout.discriminant().alignment_bytes();
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
        } else if stores_tag_id_in_pointer && tag_id != 0 {
            self.code_builder.get_local(local_id);
            self.code_builder.i32_const(tag_id as i32);
            self.code_builder.i32_or();
            self.code_builder.set_local(local_id);
        }
    }

    fn expr_get_tag_id(
        &mut self,
        structure: Symbol,
        union_layout: &UnionLayout<'a>,
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
                let local_id = match stored_value {
                    StoredValue::Local { local_id, .. } => *local_id,
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

        if union_layout.stores_tag_id_as_data(TARGET) {
            let id_offset = union_layout.tag_id_offset(self.layout_interner).unwrap();

            let id_align = union_layout.discriminant().alignment_bytes();
            let id_align = Align::from(id_align);

            self.storage
                .load_symbols(&mut self.code_builder, &[structure]);

            use roc_mono::layout::Discriminant::*;
            match union_layout.discriminant() {
                U0 | U1 | U8 => self.code_builder.i32_load8_u(id_align, id_offset),
                U16 => self.code_builder.i32_load16_u(id_align, id_offset),
            }
        } else if union_layout.stores_tag_id_in_pointer(TARGET) {
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

    fn expr_union_at_index(
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
            .map(|field_layout| self.layout_interner.stack_size(*field_layout))
            .sum();

        // Get pointer and offset to the tag's data
        let structure_storage = self.storage.get(&structure).to_owned();
        let (tag_local_id, tag_offset) = match structure_storage {
            StoredValue::StackMemory { location, .. } => {
                location.local_and_offset(self.storage.stack_frame_pointer)
            }
            StoredValue::Local { local_id, .. } => (local_id, 0),
        };

        let stores_tag_id_in_pointer = union_layout.stores_tag_id_in_pointer(TARGET);

        let from_addr_val = if stores_tag_id_in_pointer {
            self.code_builder.get_local(tag_local_id);
            self.code_builder.i32_const(-4); // 11111111...1100
            self.code_builder.i32_and();
            AddressValue::Loaded
        } else {
            AddressValue::NotLoaded(tag_local_id)
        };

        let from_offset = tag_offset + field_offset;
        self.storage.copy_value_from_memory(
            &mut self.code_builder,
            symbol,
            from_addr_val,
            from_offset,
        );
    }

    fn expr_union_field_ptr_at_index(
        &mut self,
        structure: Symbol,
        tag_id: TagIdIntType,
        union_layout: &UnionLayout<'a>,
        index: u64,
        storage: &StoredValue,
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
            .map(|field_layout| self.layout_interner.stack_size(*field_layout))
            .sum();

        // Get pointer and offset to the tag's data
        let (tag_local_id, tag_offset) = match self.storage.get(&structure) {
            StoredValue::StackMemory { location, .. } => {
                location.local_and_offset(self.storage.stack_frame_pointer)
            }
            StoredValue::Local { local_id, .. } => (*local_id, 0),
        };

        let stores_tag_id_in_pointer = union_layout.stores_tag_id_in_pointer(TARGET);

        let from_offset = tag_offset + field_offset;

        self.code_builder.get_local(tag_local_id);

        if stores_tag_id_in_pointer {
            self.code_builder.i32_const(-4); // 11111111...1100
            self.code_builder.i32_and();
        }

        self.code_builder.i32_const(from_offset as _);
        self.code_builder.i32_add();

        let symbol_local = match storage {
            StoredValue::Local { local_id, .. } => *local_id,
            _ => internal_error!("A heap pointer will always be an i32"),
        };

        self.code_builder.set_local(symbol_local);
    }

    /*******************************************************************
     * Box
     *******************************************************************/

    pub(crate) fn ptr_load(&mut self, ret_sym: Symbol, arg_sym: Symbol) {
        let (from_addr_val, from_offset) = match self.storage.get(&arg_sym) {
            StoredValue::Local { local_id, .. } => (AddressValue::NotLoaded(*local_id), 0),
            StoredValue::StackMemory { location, .. } => {
                let (local_id, offset) =
                    location.local_and_offset(self.storage.stack_frame_pointer);
                (AddressValue::NotLoaded(local_id), offset)
            }
        };

        // Copy the value
        self.storage.copy_value_from_memory(
            &mut self.code_builder,
            ret_sym,
            from_addr_val,
            from_offset,
        );
    }

    /*******************************************************************
     * Refcounting & Heap allocation
     *******************************************************************/

    /// Allocates heap space and write an initial refcount of 1.
    /// Leaves the *data* address on the VM stack
    ///
    /// elements_refcounted should only ever be set for lists.
    fn allocate_with_refcount(
        &mut self,
        data_size: u32,
        alignment_bytes: u32,
        elements_refcounted: bool,
    ) {
        if !self.can_relocate_heap {
            // This will probably only happen for test hosts.
            panic!("The app tries to allocate heap memory but the host doesn't support that. It needs to export symbols __heap_base and __heap_end");
        }

        // Zig arguments              Wasm types
        //  data_bytes: usize          i32
        //  element_alignment: u32     i32
        //  element_refcounted: bool   i32

        self.code_builder.i32_const(data_size as i32);
        self.code_builder.i32_const(alignment_bytes as i32);
        self.code_builder.i32_const(elements_refcounted as i32);

        self.call_host_fn_after_loading_args(bitcode::UTILS_ALLOCATE_WITH_REFCOUNT);
    }

    fn expr_reset(&mut self, argument: Symbol, ret_symbol: Symbol, ret_storage: &StoredValue) {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        // Get an IR expression for the call to the specialized procedure
        let layout = self.storage.symbol_layouts[&argument];
        let (specialized_call_expr, new_specializations) = self
            .helper_proc_gen
            .call_reset_refcount(ident_ids, self.layout_interner, layout, argument);

        // If any new specializations were created, register their symbol data
        for (spec_sym, spec_layout) in new_specializations.into_iter() {
            self.register_helper_proc(spec_sym, spec_layout, ProcSource::Helper);
        }

        // Generate Wasm code for the IR call expression
        self.expr(
            ret_symbol,
            self.env.arena.alloc(specialized_call_expr),
            Layout::BOOL,
            ret_storage,
        );
    }

    fn expr_resetref(&mut self, argument: Symbol, ret_symbol: Symbol, ret_storage: &StoredValue) {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        // Get an IR expression for the call to the specialized procedure
        let layout = self.storage.symbol_layouts[&argument];
        let (specialized_call_expr, new_specializations) = self
            .helper_proc_gen
            .call_resetref_refcount(ident_ids, self.layout_interner, layout, argument);

        // If any new specializations were created, register their symbol data
        for (spec_sym, spec_layout) in new_specializations.into_iter() {
            self.register_helper_proc(spec_sym, spec_layout, ProcSource::Helper);
        }

        // Generate Wasm code for the IR call expression
        self.expr(
            ret_symbol,
            self.env.arena.alloc(specialized_call_expr),
            Layout::BOOL,
            ret_storage,
        );
    }

    fn expr_alloca(
        &mut self,
        initializer: Option<Symbol>,
        element_layout: InLayout<'a>,
        ret_storage: &StoredValue,
    ) {
        // Alloca : a -> Ptr a
        let (size, alignment_bytes) = self
            .layout_interner
            .stack_size_and_alignment(element_layout);

        let (frame_ptr, offset) = self
            .storage
            .allocate_anonymous_stack_memory(size, alignment_bytes);

        // write the default value into the stack memory
        if let Some(initializer) = initializer {
            self.storage.copy_value_to_memory(
                &mut self.code_builder,
                frame_ptr,
                offset,
                initializer,
            );
        }

        // create a local variable for the pointer
        let ptr_local_id = match ret_storage {
            StoredValue::Local { local_id, .. } => *local_id,
            _ => internal_error!("A pointer will always be an i32"),
        };

        self.code_builder.get_local(frame_ptr);
        self.code_builder.i32_const(offset as i32);
        self.code_builder.i32_add();
        self.code_builder.set_local(ptr_local_id);
    }

    /// Generate a refcount helper procedure and return a pointer (table index) to it
    /// This allows it to be indirectly called from Zig code
    pub fn get_refcount_fn_index(&mut self, layout: InLayout<'a>, op: HelperOp) -> u32 {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        let (proc_symbol, new_specializations) =
            self.helper_proc_gen
                .gen_refcount_proc(ident_ids, self.layout_interner, layout, op);

        // If any new specializations were created, register their symbol data
        for (spec_sym, spec_layout) in new_specializations.into_iter() {
            self.register_helper_proc(spec_sym, spec_layout, ProcSource::Helper);
        }

        self.get_existing_helper_fn_index(proc_symbol, layout, op)
    }

    /// Generate a copy helper procedure and return a pointer (table index) to it
    /// This allows it to be indirectly called from Zig code
    pub fn get_copy_fn_index(&mut self, layout: InLayout<'a>) -> u32 {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        let (proc_symbol, new_specializations) =
            self.helper_proc_gen
                .gen_copy_proc(ident_ids, self.layout_interner, layout);

        // If any new specializations were created, register their symbol data
        for (spec_sym, spec_layout) in new_specializations.into_iter() {
            self.register_helper_proc(spec_sym, spec_layout, ProcSource::Helper);
        }

        self.get_existing_helper_fn_index(proc_symbol, layout, HelperOp::IndirectCopy)
    }

    /// return a pointer (table index) to a refcount helper procedure.
    /// This allows it to be indirectly called from Zig code
    pub fn get_existing_helper_fn_index(
        &mut self,
        proc_symbol: Symbol,
        layout: InLayout<'a>,
        op: HelperOp,
    ) -> u32 {
        let layout_repr = self.layout_interner.runtime_representation(layout);
        let same_layout = |layout| {
            if op.is_indirect() {
                if let LayoutRepr::Ptr(inner) = self.layout_interner.runtime_representation(layout)
                {
                    self.layout_interner.runtime_representation(inner) == layout_repr
                } else {
                    false
                }
            } else {
                self.layout_interner.runtime_representation(layout) == layout_repr
            }
        };
        let proc_index = self
            .proc_lookup
            .iter()
            .position(|lookup| {
                lookup.name == proc_symbol && same_layout(lookup.layout.arguments[0])
            })
            .unwrap();

        self.fn_index_offset + proc_index as u32
    }
}
