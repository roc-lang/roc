use bitvec::vec::BitVec;
use bumpalo::collections::{String, Vec};

use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::MutMap;
use roc_error_macros::internal_error;
use roc_module::low_level::{LowLevel, LowLevelWrapperType};
use roc_module::symbol::{Interns, Symbol};
use roc_mono::code_gen_help::{CodeGenHelp, HelperOp, REFCOUNT_MAX};
use roc_mono::ir::{
    BranchInfo, CallType, CrashTag, Expr, JoinPointId, ListLiteralElement, Literal, ModifyRc,
    Param, Proc, ProcLayout, Stmt,
};
use roc_mono::layout::{Builtin, Layout, LayoutIds, TagIdIntType, UnionLayout};
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
use crate::layout::{CallConv, ReturnMethod, WasmLayout};
use crate::low_level::{call_higher_order_lowlevel, LowLevelCall};
use crate::storage::{AddressValue, Storage, StoredValue, StoredVarKind};
use crate::{
    copy_memory, CopyMemoryConfig, Env, DEBUG_SETTINGS, MEMORY_NAME, PTR_SIZE, PTR_TYPE,
    TARGET_INFO,
};

#[derive(Clone, Copy, Debug)]
pub enum ProcSource {
    Roc,
    Helper,
    /// Wrapper function for higher-order calls from Zig to Roc
    HigherOrderMapper(usize),
    HigherOrderCompare(usize),
}

#[derive(Debug)]
pub struct ProcLookupData<'a> {
    pub name: Symbol,
    pub layout: ProcLayout<'a>,
    pub source: ProcSource,
}

pub struct WasmBackend<'a> {
    pub env: &'a Env<'a>,
    interns: &'a mut Interns,

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

impl<'a> WasmBackend<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        env: &'a Env<'a>,
        interns: &'a mut Interns,
        layout_ids: LayoutIds<'a>,
        proc_lookup: Vec<'a, ProcLookupData<'a>>,
        host_to_app_map: Vec<'a, (&'a str, u32)>,
        mut module: WasmModule<'a>,
        fn_index_offset: u32,
        helper_proc_gen: CodeGenHelp<'a>,
    ) -> Self {
        let can_relocate_heap = module.linking.find_internal_symbol("__heap_base").is_ok();

        // We don't want to import any Memory or Tables
        module.import.imports.retain(|import| {
            !matches!(
                import.description,
                ImportDesc::Mem { .. } | ImportDesc::Table { .. }
            )
        });

        let host_lookup = module.get_host_function_lookup(env.arena);

        if module.names.function_names.is_empty() {
            module.names = NameSection::from_imports_and_linking_data(
                env.arena,
                &module.import,
                &module.linking,
            )
        }

        module.link_host_to_app_calls(env.arena, host_to_app_map);
        let import_fn_count = module.import.function_count();
        let host_function_count = import_fn_count
            + module.code.dead_import_dummy_count as usize
            + module.code.function_count as usize;
        let mut called_fns = BitVec::repeat(false, host_function_count);
        called_fns.extend(std::iter::repeat(true).take(proc_lookup.len()));

        WasmBackend {
            env,
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
            can_relocate_heap,

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
        }
    }

    /// If the host has some `extern` global variables, we need to create them in the final binary
    /// and make them visible to JavaScript by exporting them
    fn export_globals(&mut self) {
        for (sym_index, sym) in self.module.linking.symbol_table.iter().enumerate() {
            match sym {
                SymInfo::Data(DataSymbol::Imported { name, .. }) if *name != "__heap_base" => {
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

        const START: &str = "_start";

        if let Ok(sym_index) = self.module.linking.find_internal_symbol(START) {
            let fn_index = match self.module.linking.symbol_table[sym_index] {
                SymInfo::Function(WasmObjectSymbol::ExplicitlyNamed { index, .. }) => index,
                _ => panic!("linker symbol `{}` is not a function", START),
            };
            self.module.export.append(Export {
                name: START,
                ty: ExportType::Func,
                index: fn_index,
            });
            return;
        }

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
        self.code_builder.call(main_fn_index, 2, true);
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
        let ret_layout = WasmLayout::new(self.env.layout_interner, &proc.ret_layout);

        let ret_type = match ret_layout.return_method(CallConv::C) {
            Primitive(ty, _) => Some(ty),
            NoReturnValue => None,
            WriteToPointerArg => {
                self.storage.arg_types.push(PTR_TYPE);
                None
            }
            ZigPackedStruct => {
                internal_error!("C calling convention does not return Zig packed structs")
            }
        };

        // Create a block so we can exit the function without skipping stack frame "pop" code.
        // We never use the `return` instruction. Instead, we break from this block.
        self.start_block();

        self.storage.allocate_args(
            self.env.layout_interner,
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
                println!("{:?} => {:?}", sym, storage);
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

    /// Build a wrapper around a Roc procedure so that it can be called from Zig builtins List.map*
    ///
    /// The generic Zig code passes *pointers* to all of the argument values (e.g. on the heap in a List).
    /// Numbers up to 64 bits are passed by value, so we need to load them from the provided pointer.
    /// Everything else is passed by reference, so we can just pass the pointer through.
    ///
    /// NOTE: If the builtins expected the return pointer first and closure data last, we could eliminate the wrapper
    /// when all args are pass-by-reference and non-zero size. But currently we need it to swap those around.
    pub fn build_higher_order_mapper(
        &mut self,
        wrapper_lookup_idx: usize,
        inner_lookup_idx: usize,
    ) {
        use Align::*;
        use ValueType::*;

        let ProcLookupData {
            name: wrapper_name,
            layout: wrapper_proc_layout,
            ..
        } = self.proc_lookup[wrapper_lookup_idx];
        let wrapper_arg_layouts = wrapper_proc_layout.arguments;

        // Our convention is that the last arg of the wrapper is the heap return pointer
        let heap_return_ptr_id = LocalId(wrapper_arg_layouts.len() as u32 - 1);
        let inner_ret_layout = match wrapper_arg_layouts.last() {
            Some(Layout::Boxed(inner)) => WasmLayout::new(self.env.layout_interner, inner),
            x => internal_error!("Higher-order wrapper: invalid return layout {:?}", x),
        };

        let mut n_inner_wasm_args = 0;
        let ret_type_and_size = match inner_ret_layout.return_method(CallConv::C) {
            ReturnMethod::NoReturnValue => None,
            ReturnMethod::Primitive(ty, size) => {
                // If the inner function returns a primitive, load the address to store it at
                // After the call, it will be under the call result in the value stack
                self.code_builder.get_local(heap_return_ptr_id);
                Some((ty, size))
            }
            ReturnMethod::WriteToPointerArg => {
                // If the inner function writes to a return pointer, load its address
                self.code_builder.get_local(heap_return_ptr_id);
                n_inner_wasm_args += 1;
                None
            }
            x => internal_error!("A Roc function should never use ReturnMethod {:?}", x),
        };

        // Load all the arguments for the inner function
        for (i, wrapper_arg) in wrapper_arg_layouts.iter().enumerate() {
            let is_closure_data = i == 0; // Skip closure data (first for wrapper, last for inner). We'll handle it below.
            let is_return_pointer = i == wrapper_arg_layouts.len() - 1; // Skip return pointer (may not be an arg for inner. And if it is, swaps from end to start)
            if is_closure_data || is_return_pointer {
                continue;
            }

            let inner_layout = match wrapper_arg {
                Layout::Boxed(inner) => inner,
                x => internal_error!("Expected a Boxed layout, got {:?}", x),
            };
            if inner_layout.stack_size(self.env.layout_interner, TARGET_INFO) == 0 {
                continue;
            }

            // Load the argument pointer. If it's a primitive value, dereference it too.
            n_inner_wasm_args += 1;
            self.code_builder.get_local(LocalId(i as u32));
            self.dereference_boxed_value(inner_layout);
        }

        // If the inner function has closure data, it's the last arg of the inner fn
        let closure_data_layout = wrapper_arg_layouts[0];
        if closure_data_layout.stack_size(self.env.layout_interner, TARGET_INFO) > 0 {
            // The closure data exists, and will have been passed in to the wrapper as a
            // one-element struct.
            let inner_closure_data_layout = match closure_data_layout {
                Layout::Struct {
                    field_layouts: [inner],
                    ..
                } => inner,
                other => internal_error!(
                    "Expected a boxed layout for wrapped closure data, got {:?}",
                    other
                ),
            };
            self.code_builder.get_local(LocalId(0));
            // Since the closure data is wrapped in a one-element struct, we've been passed in the
            // pointer to that struct in the stack memory. To get the closure data we just need to
            // dereference the pointer.
            self.dereference_boxed_value(inner_closure_data_layout);
        }

        // Call the wrapped inner function
        let inner_wasm_fn_index = self.fn_index_offset + inner_lookup_idx as u32;
        let has_return_val = ret_type_and_size.is_some();
        self.code_builder
            .call(inner_wasm_fn_index, n_inner_wasm_args, has_return_val);

        // If the inner function returns a primitive, store it to the address we loaded at the very beginning
        if let Some((ty, size)) = ret_type_and_size {
            match (ty, size) {
                (I64, 8) => self.code_builder.i64_store(Bytes8, 0),
                (I32, 4) => self.code_builder.i32_store(Bytes4, 0),
                (I32, 2) => self.code_builder.i32_store16(Bytes2, 0),
                (I32, 1) => self.code_builder.i32_store8(Bytes1, 0),
                (F32, 4) => self.code_builder.f32_store(Bytes4, 0),
                (F64, 8) => self.code_builder.f64_store(Bytes8, 0),
                _ => {
                    internal_error!("Cannot store {:?} with alignment of {:?}", ty, size);
                }
            }
        }

        // Write empty function header (local variables array with zero length)
        self.code_builder.build_fn_header_and_footer(&[], 0, None);

        self.module.add_function_signature(Signature {
            param_types: bumpalo::vec![in self.env.arena; I32; wrapper_arg_layouts.len()],
            ret_type: None,
        });

        self.append_proc_debug_name(wrapper_name);
        self.reset();
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

        let mut n_inner_args = 2;
        if closure_data_layout.stack_size(self.env.layout_interner, TARGET_INFO) > 0 {
            self.code_builder.get_local(LocalId(0));
            n_inner_args += 1;
        }

        let inner_layout = match value_layout {
            Layout::Boxed(inner) => inner,
            x => internal_error!("Expected a Boxed layout, got {:?}", x),
        };
        self.code_builder.get_local(LocalId(1));
        self.dereference_boxed_value(inner_layout);
        self.code_builder.get_local(LocalId(2));
        self.dereference_boxed_value(inner_layout);

        // Call the wrapped inner function
        let inner_wasm_fn_index = self.fn_index_offset + inner_lookup_idx as u32;
        self.code_builder
            .call(inner_wasm_fn_index, n_inner_args, true);

        // Write empty function header (local variables array with zero length)
        self.code_builder.build_fn_header_and_footer(&[], 0, None);

        self.module.add_function_signature(Signature {
            param_types: bumpalo::vec![in self.env.arena; I32; 3],
            ret_type: Some(ValueType::I32),
        });

        self.append_proc_debug_name(wrapper_name);
        self.reset();
    }

    fn dereference_boxed_value(&mut self, inner: &Layout) {
        use Align::*;

        match inner {
            Layout::Builtin(Builtin::Int(IntWidth::U8 | IntWidth::I8)) => {
                self.code_builder.i32_load8_u(Bytes1, 0);
            }
            Layout::Builtin(Builtin::Int(IntWidth::U16 | IntWidth::I16)) => {
                self.code_builder.i32_load16_u(Bytes2, 0);
            }
            Layout::Builtin(Builtin::Int(IntWidth::U32 | IntWidth::I32)) => {
                self.code_builder.i32_load(Bytes4, 0);
            }
            Layout::Builtin(Builtin::Int(IntWidth::U64 | IntWidth::I64)) => {
                self.code_builder.i64_load(Bytes8, 0);
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F32)) => {
                self.code_builder.f32_load(Bytes4, 0);
            }
            Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                self.code_builder.f64_load(Bytes8, 0);
            }
            Layout::Builtin(Builtin::Bool) => {
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
            } => self.stmt_switch(*cond_symbol, cond_layout, branches, default_branch),

            Stmt::Join {
                id,
                parameters,
                body,
                remainder,
            } => self.stmt_join(*id, parameters, body, remainder),

            Stmt::Jump(id, arguments) => self.stmt_jump(*id, arguments),

            Stmt::Refcounting(modify, following) => self.stmt_refcounting(modify, following),

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
                print!("\nlet {:?} = {}", sym, expr.to_pretty(200));
            }

            let kind = match following {
                Stmt::Ret(ret_sym) if *sym == *ret_sym => StoredVarKind::ReturnValue,
                _ => StoredVarKind::Variable,
            };

            self.stmt_let_store_expr(*sym, layout, expr, kind);

            current_stmt = *following;
        }

        self.stmt(current_stmt);
    }

    fn stmt_let_store_expr(
        &mut self,
        sym: Symbol,
        layout: &Layout<'a>,
        expr: &Expr<'a>,
        kind: StoredVarKind,
    ) {
        let sym_storage = self
            .storage
            .allocate_var(self.env.layout_interner, *layout, sym, kind);

        self.expr(sym, expr, layout, &sym_storage);

        // If this value is stored in the VM stack, we need code_builder to track it
        // (since every instruction can change the VM stack)
        if let Some(StoredValue::VirtualMachineStack { vm_state, .. }) =
            self.storage.symbol_storage_map.get_mut(&sym)
        {
            *vm_state = self.code_builder.set_top_symbol(sym);
        }
    }

    fn stmt_ret(&mut self, sym: Symbol) {
        use crate::storage::StoredValue::*;

        let storage = self.storage.symbol_storage_map.get(&sym).unwrap();

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
        cond_layout: &Layout<'a>,
        branches: &'a [(u64, BranchInfo<'a>, Stmt<'a>)],
        default_branch: &(BranchInfo<'a>, &'a Stmt<'a>),
    ) {
        // NOTE currently implemented as a series of conditional jumps
        // We may be able to improve this in the future with `Select`
        // or `BrTable`

        // Ensure the condition value is not stored only in the VM stack
        // Otherwise we can't reach it from inside the block
        let cond_storage = self.storage.get(&cond_symbol).to_owned();
        self.storage
            .ensure_value_has_local(&mut self.code_builder, cond_symbol, cond_storage);

        // create a block for each branch except the default
        for _ in 0..branches.len() {
            self.start_block()
        }

        let is_bool = matches!(cond_layout, Layout::Builtin(Builtin::Bool));
        let cond_type =
            WasmLayout::new(self.env.layout_interner, cond_layout).arg_types(CallConv::C)[0];

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
                        self.code_builder.f64_const(f64::from_bits(*value as u64));
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
            let mut param_storage = self.storage.allocate_var(
                self.env.layout_interner,
                parameter.layout,
                parameter.symbol,
                StoredVarKind::Variable,
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

    fn stmt_refcounting(&mut self, modify: &ModifyRc, following: &'a Stmt<'a>) {
        let value = modify.get_symbol();
        let layout = self.storage.symbol_layouts[&value];

        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        let (rc_stmt, new_specializations) = self
            .helper_proc_gen
            .expand_refcount_stmt(ident_ids, layout, modify, following);

        if false {
            self.register_symbol_debug_names();
            println!(
                "## rc_stmt:\n{}\n{:?}",
                rc_stmt.to_pretty(self.env.layout_interner, 200),
                rc_stmt
            );
        }

        // If any new specializations were created, register their symbol data
        for (spec_sym, spec_layout) in new_specializations.into_iter() {
            self.register_helper_proc(spec_sym, spec_layout, ProcSource::Helper);
        }

        self.stmt(rc_stmt);
    }

    pub fn stmt_internal_error(&mut self, msg: &'a str) {
        let msg_sym = self.create_symbol("panic_str");
        let msg_storage = self.storage.allocate_var(
            self.env.layout_interner,
            Layout::Builtin(Builtin::Str),
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
        self.call_host_fn_after_loading_args("roc_panic", 2, false);

        self.code_builder.unreachable_();
    }

    /**********************************************************

            EXPRESSIONS

    ***********************************************************/

    fn expr(&mut self, sym: Symbol, expr: &Expr<'a>, layout: &Layout<'a>, storage: &StoredValue) {
        match expr {
            Expr::Literal(lit) => self.expr_literal(lit, storage),

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

            Expr::Array { elems, elem_layout } => self.expr_array(sym, storage, elem_layout, elems),

            Expr::EmptyArray => self.expr_empty_array(sym, storage),

            Expr::Tag {
                tag_layout: union_layout,
                tag_id,
                arguments,
                ..
            } => self.expr_tag(union_layout, *tag_id, arguments, sym, storage, None),

            Expr::GetTagId {
                structure,
                union_layout,
            } => self.expr_get_tag_id(*structure, union_layout, sym, storage),

            Expr::UnionAtIndex {
                structure,
                tag_id,
                union_layout,
                index,
            } => self.expr_union_at_index(*structure, *tag_id, union_layout, *index, sym),

            Expr::ExprBox { symbol: arg_sym } => self.expr_box(sym, *arg_sym, layout, storage),

            Expr::ExprUnbox { symbol: arg_sym } => self.expr_unbox(sym, *arg_sym),

            Expr::Reuse {
                tag_layout,
                tag_id,
                arguments,
                symbol: reused,
                ..
            } => self.expr_tag(tag_layout, *tag_id, arguments, sym, storage, Some(*reused)),

            Expr::Reset { symbol: arg, .. } => self.expr_reset(*arg, sym, storage),

            Expr::RuntimeErrorFunction(_) => {
                todo!("Expression `{}`", expr.to_pretty(100))
            }
        }
    }

    /*******************************************************************
     * Literals
     *******************************************************************/

    fn expr_literal(&mut self, lit: &Literal<'a>, storage: &StoredValue) {
        let invalid_error =
            || internal_error!("Literal value {:?} has invalid storage {:?}", lit, storage);

        match storage {
            StoredValue::VirtualMachineStack { value_type, .. } => {
                match (lit, value_type) {
                    (Literal::Float(x), ValueType::F64) => self.code_builder.f64_const(*x as f64),
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

            _ => invalid_error(),
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

    /*******************************************************************
     * Call expressions
     *******************************************************************/

    fn expr_call(
        &mut self,
        call_type: &CallType<'a>,
        arguments: &'a [Symbol],
        ret_sym: Symbol,
        ret_layout: &Layout<'a>,
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
                    result: **result,
                    captures_niche: func_sym.captures_niche(),
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

            CallType::LowLevel { op: lowlevel, .. } => {
                self.expr_call_low_level(*lowlevel, arguments, ret_sym, ret_layout, ret_storage)
            }

            CallType::HigherOrder(higher_order_lowlevel) => {
                call_higher_order_lowlevel(self, ret_sym, ret_layout, higher_order_lowlevel)
            }

            CallType::Foreign {
                foreign_symbol,
                ret_layout,
            } => {
                let name = foreign_symbol.as_str();
                let wasm_layout = WasmLayout::new(self.env.layout_interner, ret_layout);
                let (num_wasm_args, has_return_val, ret_zig_packed_struct) =
                    self.storage.load_symbols_for_call(
                        self.env.arena,
                        &mut self.code_builder,
                        arguments,
                        ret_sym,
                        &wasm_layout,
                        CallConv::C,
                    );
                debug_assert!(!ret_zig_packed_struct); // only true in another place where we use the same helper fn
                self.call_host_fn_after_loading_args(name, num_wasm_args, has_return_val)
            }
        }
    }

    fn expr_call_by_name(
        &mut self,
        func_sym: Symbol,
        proc_layout: &ProcLayout<'a>,
        arguments: &'a [Symbol],
        ret_sym: Symbol,
        ret_layout: &Layout<'a>,
        ret_storage: &StoredValue,
    ) {
        let wasm_layout = WasmLayout::new(self.env.layout_interner, ret_layout);

        // If this function is just a lowlevel wrapper, then inline it
        if let LowLevelWrapperType::CanBeReplacedBy(lowlevel) =
            LowLevelWrapperType::from_symbol(func_sym)
        {
            return self.expr_call_low_level(lowlevel, arguments, ret_sym, ret_layout, ret_storage);
        }

        let (num_wasm_args, has_return_val, ret_zig_packed_struct) =
            self.storage.load_symbols_for_call(
                self.env.arena,
                &mut self.code_builder,
                arguments,
                ret_sym,
                &wasm_layout,
                CallConv::C,
            );
        debug_assert!(!ret_zig_packed_struct);

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

        self.code_builder
            .call(wasm_fn_index, num_wasm_args, has_return_val);
    }

    fn expr_call_low_level(
        &mut self,
        lowlevel: LowLevel,
        arguments: &'a [Symbol],
        ret_symbol: Symbol,
        ret_layout: &Layout<'a>,
        ret_storage: &StoredValue,
    ) {
        let low_level_call = LowLevelCall {
            lowlevel,
            arguments,
            ret_symbol,
            ret_layout: ret_layout.to_owned(),
            ret_storage: ret_storage.to_owned(),
        };
        low_level_call.generate(self);
    }

    /// Generate a call instruction to a host function or Zig builtin.
    pub fn call_host_fn_after_loading_args(
        &mut self,
        name: &str,
        num_wasm_args: usize,
        has_return_val: bool,
    ) {
        let (_, fn_index) = self
            .host_lookup
            .iter()
            .find(|(fn_name, _)| *fn_name == name)
            .unwrap_or_else(|| panic!("The Roc app tries to call `{}` but I can't find it!", name));

        self.called_fns.set(*fn_index as usize, true);

        if *fn_index < self.import_fn_count {
            self.code_builder
                .call_import(*fn_index, num_wasm_args, has_return_val);
        } else {
            self.code_builder
                .call(*fn_index, num_wasm_args, has_return_val);
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
        arg_layout: &Layout<'a>,
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
            .call_specialized_equals(ident_ids, arg_layout, arguments);

        // If any new specializations were created, register their symbol data
        for (spec_sym, spec_layout) in new_specializations.into_iter() {
            self.register_helper_proc(spec_sym, spec_layout, ProcSource::Helper);
        }

        // Generate Wasm code for the IR call expression
        self.expr(
            ret_symbol,
            self.env.arena.alloc(specialized_call_expr),
            &Layout::Builtin(Builtin::Bool),
            ret_storage,
        );
    }

    /*******************************************************************
     * Structs
     *******************************************************************/

    fn expr_struct(
        &mut self,
        sym: Symbol,
        layout: &Layout<'a>,
        storage: &StoredValue,
        fields: &'a [Symbol],
    ) {
        match layout {
            Layout::Struct { .. } => {
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
            Layout::LambdaSet(lambdaset) => self.expr_struct(
                sym,
                &lambdaset.runtime_representation(self.env.layout_interner),
                storage,
                fields,
            ),
            _ => {
                if !fields.is_empty() {
                    // Struct expression but not Struct layout => single element. Copy it.
                    let field_storage = self.storage.get(&fields[0]).to_owned();
                    self.storage.clone_value(
                        &mut self.code_builder,
                        storage,
                        &field_storage,
                        fields[0],
                    );
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
        field_layouts: &'a [Layout<'a>],
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

            StoredValue::VirtualMachineStack { .. } => {
                self.storage
                    .load_symbols(&mut self.code_builder, &[structure]);
                (AddressValue::Loaded, 0)
            }
        };
        for field in field_layouts.iter().take(index as usize) {
            offset += field.stack_size(self.env.layout_interner, TARGET_INFO);
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
        elem_layout: &Layout<'a>,
        elems: &'a [ListLiteralElement<'a>],
    ) {
        if let StoredValue::StackMemory { location, .. } = storage {
            let size = elem_layout.stack_size(self.env.layout_interner, TARGET_INFO)
                * (elems.len() as u32);

            // Allocate heap space and store its address in a local variable
            let heap_local_id = self.storage.create_anonymous_local(PTR_TYPE);
            let heap_alignment = elem_layout.alignment_bytes(self.env.layout_interner, TARGET_INFO);
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
                        let debug_name = format!("{:?}_{}", sym, i);
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
        symbol: Symbol,
        stored: &StoredValue,
        maybe_reused: Option<Symbol>,
    ) {
        if union_layout.tag_is_null(tag_id) {
            self.code_builder.i32_const(0);
            return;
        }

        let stores_tag_id_as_data = union_layout.stores_tag_id_as_data(TARGET_INFO);
        let stores_tag_id_in_pointer = union_layout.stores_tag_id_in_pointer(TARGET_INFO);
        let (data_size, data_alignment) =
            union_layout.data_size_and_alignment(self.env.layout_interner, TARGET_INFO);

        // We're going to use the pointer many times, so put it in a local variable
        let stored_with_local =
            self.storage
                .ensure_value_has_local(&mut self.code_builder, symbol, stored.to_owned());

        let (local_id, data_offset) = match stored_with_local {
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
                        self.code_builder.set_local(local_id);
                    }
                    self.code_builder.else_();
                    {
                        self.allocate_with_refcount(Some(data_size), data_alignment, 1);
                        self.code_builder.set_local(local_id);
                    }
                    self.code_builder.end();
                } else {
                    // Call the allocator to get a memory address.
                    self.allocate_with_refcount(Some(data_size), data_alignment, 1);
                    self.code_builder.set_local(local_id);
                }
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
            let id_offset = data_offset
                + union_layout
                    .tag_id_offset(self.env.layout_interner, TARGET_INFO)
                    .unwrap();

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

        if union_layout.stores_tag_id_as_data(TARGET_INFO) {
            let id_offset = union_layout
                .tag_id_offset(self.env.layout_interner, TARGET_INFO)
                .unwrap();

            let id_align = union_layout.discriminant().alignment_bytes();
            let id_align = Align::from(id_align);

            self.storage
                .load_symbols(&mut self.code_builder, &[structure]);

            use roc_mono::layout::Discriminant::*;
            match union_layout.discriminant() {
                U0 | U1 | U8 => self.code_builder.i32_load8_u(id_align, id_offset),
                U16 => self.code_builder.i32_load16_u(id_align, id_offset),
            }
        } else if union_layout.stores_tag_id_in_pointer(TARGET_INFO) {
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
            .map(|field_layout| field_layout.stack_size(self.env.layout_interner, TARGET_INFO))
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

        let stores_tag_id_in_pointer = union_layout.stores_tag_id_in_pointer(TARGET_INFO);

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

    /*******************************************************************
     * Box
     *******************************************************************/

    fn expr_box(
        &mut self,
        ret_sym: Symbol,
        arg_sym: Symbol,
        layout: &Layout<'a>,
        storage: &StoredValue,
    ) {
        // create a local variable for the heap pointer
        let ptr_local_id = match self.storage.ensure_value_has_local(
            &mut self.code_builder,
            ret_sym,
            storage.clone(),
        ) {
            StoredValue::Local { local_id, .. } => local_id,
            _ => internal_error!("A heap pointer will always be an i32"),
        };

        // allocate heap memory and load its data address onto the value stack
        let arg_layout = match layout {
            Layout::Boxed(arg) => *arg,
            _ => internal_error!("ExprBox should always produce a Boxed layout"),
        };
        let (size, alignment) =
            arg_layout.stack_size_and_alignment(self.env.layout_interner, TARGET_INFO);
        self.allocate_with_refcount(Some(size), alignment, 1);

        // store the pointer value from the value stack into the local variable
        self.code_builder.set_local(ptr_local_id);

        // copy the argument to the pointer address
        self.storage
            .copy_value_to_memory(&mut self.code_builder, ptr_local_id, 0, arg_sym);
    }

    fn expr_unbox(&mut self, ret_sym: Symbol, arg_sym: Symbol) {
        let (from_addr_val, from_offset) = match self.storage.get(&arg_sym) {
            StoredValue::VirtualMachineStack { .. } => {
                self.storage
                    .load_symbols(&mut self.code_builder, &[arg_sym]);
                (AddressValue::Loaded, 0)
            }
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
        if !self.can_relocate_heap {
            // This will probably only happen for test hosts.
            panic!("The app tries to allocate heap memory but the host doesn't support that. It needs to export __heap_base");
        }
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
        self.call_host_fn_after_loading_args("roc_alloc", 2, true);

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
            .call_reset_refcount(ident_ids, layout, argument);

        // If any new specializations were created, register their symbol data
        for (spec_sym, spec_layout) in new_specializations.into_iter() {
            self.register_helper_proc(spec_sym, spec_layout, ProcSource::Helper);
        }

        // Generate Wasm code for the IR call expression
        self.expr(
            ret_symbol,
            self.env.arena.alloc(specialized_call_expr),
            &Layout::Builtin(Builtin::Bool),
            ret_storage,
        );
    }

    /// Generate a refcount helper procedure and return a pointer (table index) to it
    /// This allows it to be indirectly called from Zig code
    pub fn get_refcount_fn_index(&mut self, layout: Layout<'a>, op: HelperOp) -> u32 {
        let ident_ids = self
            .interns
            .all_ident_ids
            .get_mut(&self.env.module_id)
            .unwrap();

        let (proc_symbol, new_specializations) = self
            .helper_proc_gen
            .gen_refcount_proc(ident_ids, layout, op);

        // If any new specializations were created, register their symbol data
        for (spec_sym, spec_layout) in new_specializations.into_iter() {
            self.register_helper_proc(spec_sym, spec_layout, ProcSource::Helper);
        }

        let proc_index = self
            .proc_lookup
            .iter()
            .position(|lookup| lookup.name == proc_symbol && lookup.layout.arguments[0] == layout)
            .unwrap();

        self.fn_index_offset + proc_index as u32
    }
}
