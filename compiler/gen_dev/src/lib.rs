#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant, clippy::upper_case_acronyms)]

use bumpalo::{collections::Vec, Bump};
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_collections::all::{MutMap, MutSet};
use roc_error_macros::internal_error;
use roc_module::ident::{ModuleName, TagName};
use roc_module::low_level::{LowLevel, LowLevelWrapperType};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::code_gen_help::CodeGenHelp;
use roc_mono::ir::{
    BranchInfo, CallType, Expr, JoinPointId, ListLiteralElement, Literal, Param, Proc, ProcLayout,
    SelfRecursive, Stmt,
};
use roc_mono::layout::{Builtin, Layout, LayoutId, LayoutIds};

mod generic64;
mod object_builder;
pub use object_builder::build_module;
mod run_roc;

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub module_id: ModuleId,
    pub exposed_to_host: MutSet<Symbol>,
    pub lazy_literals: bool,
    pub generate_allocators: bool,
}

// These relocations likely will need a length.
// They may even need more definition, but this should be at least good enough for how we will use elf.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Relocation {
    LocalData {
        offset: u64,
        // This should probably technically be a bumpalo::Vec.
        // The problem is that it currently is built in a place that can't access the arena.
        data: std::vec::Vec<u8>,
    },
    LinkedFunction {
        offset: u64,
        name: String,
    },
    LinkedData {
        offset: u64,
        name: String,
    },
    JmpToReturn {
        inst_loc: u64,
        inst_size: u64,
        offset: u64,
    },
}

trait Backend<'a> {
    fn env(&self) -> &Env<'a>;
    fn interns(&self) -> &Interns;

    // This method is suboptimal, but it seems to be the only way to make rust understand
    // that all of these values can be mutable at the same time. By returning them together,
    // rust understands that they are part of a single use of mutable self.
    fn env_interns_helpers_mut(&mut self) -> (&Env<'a>, &mut Interns, &mut CodeGenHelp<'a>);

    fn symbol_to_string(&self, symbol: Symbol, layout_id: LayoutId) -> String {
        layout_id.to_symbol_string(symbol, self.interns())
    }

    fn defined_in_app_module(&self, symbol: Symbol) -> bool {
        symbol
            .module_string(self.interns())
            .starts_with(ModuleName::APP)
    }

    fn helper_proc_gen_mut(&mut self) -> &mut CodeGenHelp<'a>;

    fn helper_proc_symbols_mut(&mut self) -> &mut Vec<'a, (Symbol, ProcLayout<'a>)>;

    fn helper_proc_symbols(&self) -> &Vec<'a, (Symbol, ProcLayout<'a>)>;

    /// reset resets any registers or other values that may be occupied at the end of a procedure.
    /// It also passes basic procedure information to the builder for setup of the next function.
    fn reset(&mut self, name: String, is_self_recursive: SelfRecursive);

    /// finalize does any setup and cleanup that should happen around the procedure.
    /// finalize does setup because things like stack size and jump locations are not know until the function is written.
    /// For example, this can store the frame pointer and setup stack space.
    /// finalize is run at the end of build_proc when all internal code is finalized.
    fn finalize(&mut self) -> (Vec<u8>, Vec<Relocation>);

    // load_args is used to let the backend know what the args are.
    // The backend should track these args so it can use them as needed.
    fn load_args(&mut self, args: &'a [(Layout<'a>, Symbol)], ret_layout: &Layout<'a>);

    /// Used for generating wrappers for malloc/realloc/free
    fn build_wrapped_jmp(&mut self) -> (&'a [u8], u64);

    /// build_proc creates a procedure and outputs it to the wrapped object writer.
    /// Returns the procedure bytes, its relocations, and the names of the refcounting functions it references.
    fn build_proc(
        &mut self,
        proc: Proc<'a>,
        layout_ids: &mut LayoutIds<'a>,
    ) -> (Vec<u8>, Vec<Relocation>, Vec<'a, (Symbol, String)>) {
        let layout_id = layout_ids.get(proc.name, &proc.ret_layout);
        let proc_name = self.symbol_to_string(proc.name, layout_id);
        self.reset(proc_name, proc.is_self_recursive);
        self.load_args(proc.args, &proc.ret_layout);
        for (layout, sym) in proc.args {
            self.set_layout_map(*sym, layout);
        }
        self.scan_ast(&proc.body);
        self.create_free_map();
        self.build_stmt(&proc.body, &proc.ret_layout);
        let mut helper_proc_names = bumpalo::vec![in self.env().arena];
        helper_proc_names.reserve(self.helper_proc_symbols().len());
        for (rc_proc_sym, rc_proc_layout) in self.helper_proc_symbols() {
            let name = layout_ids
                .get_toplevel(*rc_proc_sym, rc_proc_layout)
                .to_symbol_string(*rc_proc_sym, self.interns());

            helper_proc_names.push((*rc_proc_sym, name));
        }
        let (bytes, relocs) = self.finalize();
        (bytes, relocs, helper_proc_names)
    }

    /// build_stmt builds a statement and outputs at the end of the buffer.
    fn build_stmt(&mut self, stmt: &Stmt<'a>, ret_layout: &Layout<'a>) {
        match stmt {
            Stmt::Let(sym, expr, layout, following) => {
                self.build_expr(sym, expr, layout);
                self.set_layout_map(*sym, layout);
                self.free_symbols(stmt);
                self.build_stmt(following, ret_layout);
            }
            Stmt::Ret(sym) => {
                self.load_literal_symbols(&[*sym]);
                self.return_symbol(sym, ret_layout);
                self.free_symbols(stmt);
            }
            Stmt::Refcounting(modify, following) => {
                let sym = modify.get_symbol();
                let layout = *self.layout_map().get(&sym).unwrap();

                // Expand the Refcounting statement into more detailed IR with a function call
                // If this layout requires a new RC proc, we get enough info to create a linker symbol
                // for it. Here we don't create linker symbols at this time, but in Wasm backend, we do.
                let (rc_stmt, new_specializations) = {
                    let (env, interns, rc_proc_gen) = self.env_interns_helpers_mut();
                    let module_id = env.module_id;
                    let ident_ids = interns.all_ident_ids.get_mut(&module_id).unwrap();

                    rc_proc_gen.expand_refcount_stmt(ident_ids, layout, modify, *following)
                };

                for spec in new_specializations.into_iter() {
                    self.helper_proc_symbols_mut().push(spec);
                }

                self.build_stmt(rc_stmt, ret_layout)
            }
            Stmt::Switch {
                cond_symbol,
                cond_layout,
                branches,
                default_branch,
                ret_layout,
            } => {
                self.load_literal_symbols(&[*cond_symbol]);
                self.build_switch(
                    cond_symbol,
                    cond_layout,
                    branches,
                    default_branch,
                    ret_layout,
                );
                self.free_symbols(stmt);
            }
            Stmt::Join {
                id,
                parameters,
                body,
                remainder,
            } => {
                for param in parameters.iter() {
                    self.set_layout_map(param.symbol, &param.layout);
                }
                self.build_join(id, parameters, body, remainder, ret_layout);
                self.free_symbols(stmt);
            }
            Stmt::Jump(id, args) => {
                let mut arg_layouts: bumpalo::collections::Vec<Layout<'a>> =
                    bumpalo::vec![in self.env().arena];
                arg_layouts.reserve(args.len());
                let layout_map = self.layout_map();
                for arg in *args {
                    if let Some(layout) = layout_map.get(arg) {
                        arg_layouts.push(*layout);
                    } else {
                        internal_error!("the argument, {:?}, has no know layout", arg);
                    }
                }
                self.build_jump(id, args, arg_layouts.into_bump_slice(), ret_layout);
                self.free_symbols(stmt);
            }
            x => todo!("the statement, {:?}", x),
        }
    }
    // build_switch generates a instructions for a switch statement.
    fn build_switch(
        &mut self,
        cond_symbol: &Symbol,
        cond_layout: &Layout<'a>,
        branches: &'a [(u64, BranchInfo<'a>, Stmt<'a>)],
        default_branch: &(BranchInfo<'a>, &'a Stmt<'a>),
        ret_layout: &Layout<'a>,
    );

    // build_join generates a instructions for a join statement.
    fn build_join(
        &mut self,
        id: &JoinPointId,
        parameters: &'a [Param<'a>],
        body: &'a Stmt<'a>,
        remainder: &'a Stmt<'a>,
        ret_layout: &Layout<'a>,
    );

    // build_jump generates a instructions for a jump statement.
    fn build_jump(
        &mut self,
        id: &JoinPointId,
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    );

    /// build_expr builds the expressions for the specified symbol.
    /// The builder must keep track of the symbol because it may be referred to later.
    fn build_expr(&mut self, sym: &Symbol, expr: &Expr<'a>, layout: &Layout<'a>) {
        match expr {
            Expr::Literal(lit) => {
                if self.env().lazy_literals {
                    self.literal_map().insert(*sym, (lit, layout));
                } else {
                    self.load_literal(sym, layout, lit);
                }
            }
            Expr::Call(roc_mono::ir::Call {
                call_type,
                arguments,
            }) => {
                match call_type {
                    CallType::ByName {
                        name: func_sym,
                        arg_layouts,
                        ret_layout,
                        ..
                    } => {
                        if let LowLevelWrapperType::CanBeReplacedBy(lowlevel) =
                            LowLevelWrapperType::from_symbol(*func_sym)
                        {
                            self.build_run_low_level(
                                sym,
                                &lowlevel,
                                arguments,
                                arg_layouts,
                                ret_layout,
                            )
                        } else if self.defined_in_app_module(*func_sym) {
                            let layout_id = LayoutIds::default().get(*func_sym, layout);
                            let fn_name = self.symbol_to_string(*func_sym, layout_id);
                            // Now that the arguments are needed, load them if they are literals.
                            self.load_literal_symbols(arguments);
                            self.build_fn_call(sym, fn_name, arguments, arg_layouts, ret_layout)
                        } else {
                            self.build_inline_builtin(
                                sym,
                                *func_sym,
                                arguments,
                                arg_layouts,
                                ret_layout,
                            )
                        }
                    }

                    CallType::LowLevel { op: lowlevel, .. } => {
                        let mut arg_layouts: bumpalo::collections::Vec<Layout<'a>> =
                            bumpalo::vec![in self.env().arena];
                        arg_layouts.reserve(arguments.len());
                        let layout_map = self.layout_map();
                        for arg in *arguments {
                            if let Some(layout) = layout_map.get(arg) {
                                arg_layouts.push(*layout);
                            } else {
                                internal_error!("the argument, {:?}, has no know layout", arg);
                            }
                        }
                        self.build_run_low_level(
                            sym,
                            lowlevel,
                            arguments,
                            arg_layouts.into_bump_slice(),
                            layout,
                        )
                    }
                    x => todo!("the call type, {:?}", x),
                }
            }
            Expr::Struct(fields) => {
                self.load_literal_symbols(fields);
                self.create_struct(sym, layout, fields);
            }
            Expr::StructAtIndex {
                index,
                field_layouts,
                structure,
            } => {
                self.load_struct_at_index(sym, structure, *index, field_layouts);
            }
            x => todo!("the expression, {:?}", x),
        }
    }

    /// build_run_low_level builds the low level opertation and outputs to the specified symbol.
    /// The builder must keep track of the symbol because it may be referred to later.
    fn build_run_low_level(
        &mut self,
        sym: &Symbol,
        lowlevel: &LowLevel,
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    ) {
        // Now that the arguments are needed, load them if they are literals.
        self.load_literal_symbols(args);
        match lowlevel {
            LowLevel::NumAbs => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "NumAbs: expected to have exactly one argument"
                );
                debug_assert_eq!(
                    arg_layouts[0], *ret_layout,
                    "NumAbs: expected to have the same argument and return layout"
                );
                self.build_num_abs(sym, &args[0], ret_layout)
            }
            LowLevel::NumAdd => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NumAdd: expected to have exactly two argument"
                );
                debug_assert_eq!(
                    arg_layouts[0], arg_layouts[1],
                    "NumAdd: expected all arguments of to have the same layout"
                );
                debug_assert_eq!(
                    arg_layouts[0], *ret_layout,
                    "NumAdd: expected to have the same argument and return layout"
                );
                self.build_num_add(sym, &args[0], &args[1], ret_layout)
            }
            LowLevel::NumAcos => self.build_fn_call(
                sym,
                bitcode::NUM_ACOS[FloatWidth::F64].to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::NumAsin => self.build_fn_call(
                sym,
                bitcode::NUM_ASIN[FloatWidth::F64].to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::NumAtan => self.build_fn_call(
                sym,
                bitcode::NUM_ATAN[FloatWidth::F64].to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::NumMul => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NumMul: expected to have exactly two argument"
                );
                debug_assert_eq!(
                    arg_layouts[0], arg_layouts[1],
                    "NumMul: expected all arguments of to have the same layout"
                );
                debug_assert_eq!(
                    arg_layouts[0], *ret_layout,
                    "NumMul: expected to have the same argument and return layout"
                );
                self.build_num_mul(sym, &args[0], &args[1], ret_layout)
            }
            LowLevel::NumNeg => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "NumNeg: expected to have exactly one argument"
                );
                debug_assert_eq!(
                    arg_layouts[0], *ret_layout,
                    "NumNeg: expected to have the same argument and return layout"
                );
                self.build_num_neg(sym, &args[0], ret_layout)
            }
            LowLevel::NumPowInt => self.build_fn_call(
                sym,
                bitcode::NUM_POW_INT[IntWidth::I64].to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::NumSub => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NumSub: expected to have exactly two argument"
                );
                debug_assert_eq!(
                    arg_layouts[0], arg_layouts[1],
                    "NumSub: expected all arguments of to have the same layout"
                );
                debug_assert_eq!(
                    arg_layouts[0], *ret_layout,
                    "NumSub: expected to have the same argument and return layout"
                );
                self.build_num_sub(sym, &args[0], &args[1], ret_layout)
            }
            LowLevel::Eq => {
                debug_assert_eq!(2, args.len(), "Eq: expected to have exactly two argument");
                debug_assert_eq!(
                    arg_layouts[0], arg_layouts[1],
                    "Eq: expected all arguments of to have the same layout"
                );
                debug_assert_eq!(
                    Layout::Builtin(Builtin::Bool),
                    *ret_layout,
                    "Eq: expected to have return layout of type Bool"
                );
                self.build_eq(sym, &args[0], &args[1], &arg_layouts[0])
            }
            LowLevel::NotEq => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NotEq: expected to have exactly two argument"
                );
                debug_assert_eq!(
                    arg_layouts[0], arg_layouts[1],
                    "NotEq: expected all arguments of to have the same layout"
                );
                debug_assert_eq!(
                    Layout::Builtin(Builtin::Bool),
                    *ret_layout,
                    "NotEq: expected to have return layout of type Bool"
                );
                self.build_neq(sym, &args[0], &args[1], &arg_layouts[0])
            }
            LowLevel::NumLt => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NumLt: expected to have exactly two argument"
                );
                debug_assert_eq!(
                    arg_layouts[0], arg_layouts[1],
                    "NumLt: expected all arguments of to have the same layout"
                );
                debug_assert_eq!(
                    Layout::Builtin(Builtin::Bool),
                    *ret_layout,
                    "NumLt: expected to have return layout of type Bool"
                );
                self.build_num_lt(sym, &args[0], &args[1], &arg_layouts[0])
            }
            LowLevel::NumToFloat => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "NumToFloat: expected to have exactly one argument"
                );

                debug_assert!(
                    matches!(
                        *ret_layout,
                        Layout::Builtin(Builtin::Float(FloatWidth::F32 | FloatWidth::F64)),
                    ),
                    "NumToFloat: expected to have return layout of type Float"
                );
                self.build_num_to_float(sym, &args[0], &arg_layouts[0], ret_layout)
            }
            LowLevel::NumGte => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NumGte: expected to have exactly two argument"
                );
                debug_assert_eq!(
                    arg_layouts[0], arg_layouts[1],
                    "NumGte: expected all arguments of to have the same layout"
                );
                debug_assert_eq!(
                    Layout::Builtin(Builtin::Bool),
                    *ret_layout,
                    "NumGte: expected to have return layout of type Bool"
                );
                self.build_num_gte(sym, &args[0], &args[1], &arg_layouts[0])
            }
            LowLevel::NumRound => self.build_fn_call(
                sym,
                bitcode::NUM_ROUND[FloatWidth::F64].to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrConcat => self.build_fn_call(
                sym,
                bitcode::STR_CONCAT.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::PtrCast => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "RefCountGetPtr: expected to have exactly one argument"
                );
                self.build_ptr_cast(sym, &args[0])
            }
            LowLevel::RefCountDec => self.build_fn_call(
                sym,
                bitcode::UTILS_DECREF.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::RefCountInc => self.build_fn_call(
                sym,
                bitcode::UTILS_INCREF.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            x => todo!("low level, {:?}", x),
        }
    }

    // inlines simple builtin functions that do not map directly to a low level
    fn build_inline_builtin(
        &mut self,
        sym: &Symbol,
        func_sym: Symbol,
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    ) {
        self.load_literal_symbols(args);
        match func_sym {
            Symbol::NUM_IS_ZERO => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "NumIsZero: expected to have exactly one argument"
                );
                debug_assert_eq!(
                    Layout::Builtin(Builtin::Bool),
                    *ret_layout,
                    "NumIsZero: expected to have return layout of type Bool"
                );

                self.load_literal(&Symbol::DEV_TMP, &arg_layouts[0], &Literal::Int(0));
                self.build_eq(sym, &args[0], &Symbol::DEV_TMP, &arg_layouts[0]);
                self.free_symbol(&Symbol::DEV_TMP)
            }
            _ => todo!("the function, {:?}", func_sym),
        }
    }

    /// build_fn_call creates a call site for a function.
    /// This includes dealing with things like saving regs and propagating the returned value.
    fn build_fn_call(
        &mut self,
        dst: &Symbol,
        fn_name: String,
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    );

    /// build_num_abs stores the absolute value of src into dst.
    fn build_num_abs(&mut self, dst: &Symbol, src: &Symbol, layout: &Layout<'a>);

    /// build_num_add stores the sum of src1 and src2 into dst.
    fn build_num_add(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &Layout<'a>);

    /// build_num_mul stores `src1 * src2` into dst.
    fn build_num_mul(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &Layout<'a>);

    /// build_num_neg stores the negated value of src into dst.
    fn build_num_neg(&mut self, dst: &Symbol, src: &Symbol, layout: &Layout<'a>);

    /// build_num_sub stores the `src1 - src2` difference into dst.
    fn build_num_sub(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &Layout<'a>);

    /// build_eq stores the result of `src1 == src2` into dst.
    fn build_eq(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, arg_layout: &Layout<'a>);

    /// build_neq stores the result of `src1 != src2` into dst.
    fn build_neq(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, arg_layout: &Layout<'a>);

    /// build_num_lt stores the result of `src1 < src2` into dst.
    fn build_num_lt(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, arg_layout: &Layout<'a>);

    /// build_num_to_float convert Number to Float
    fn build_num_to_float(
        &mut self,
        dst: &Symbol,
        src: &Symbol,
        arg_layout: &Layout<'a>,
        ret_layout: &Layout<'a>,
    );

    /// build_num_gte stores the result of `src1 >= src2` into dst.
    fn build_num_gte(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &Layout<'a>,
    );

    /// build_refcount_getptr loads the pointer to the reference count of src into dst.
    fn build_ptr_cast(&mut self, dst: &Symbol, src: &Symbol);

    /// literal_map gets the map from symbol to literal and layout, used for lazy loading and literal folding.
    fn literal_map(&mut self) -> &mut MutMap<Symbol, (*const Literal<'a>, *const Layout<'a>)>;

    fn load_literal_symbols(&mut self, syms: &[Symbol]) {
        if self.env().lazy_literals {
            for sym in syms {
                if let Some((lit, layout)) = self.literal_map().remove(sym) {
                    // This operation is always safe but complicates lifetimes.
                    // The map is reset when building a procedure and then used for that single procedure.
                    // Since the lifetime is shorter than the entire backend, we need to use a pointer.
                    let (lit, layout) = unsafe { (*lit, *layout) };
                    self.load_literal(sym, &layout, &lit);
                }
            }
        }
    }

    /// load_literal sets a symbol to be equal to a literal.
    fn load_literal(&mut self, sym: &Symbol, layout: &Layout<'a>, lit: &Literal<'a>);

    /// create_struct creates a struct with the elements specified loaded into it as data.
    fn create_struct(&mut self, sym: &Symbol, layout: &Layout<'a>, fields: &'a [Symbol]);

    /// load_struct_at_index loads into `sym` the value at `index` in `structure`.
    fn load_struct_at_index(
        &mut self,
        sym: &Symbol,
        structure: &Symbol,
        index: u64,
        field_layouts: &'a [Layout<'a>],
    );

    /// return_symbol moves a symbol to the correct return location for the backend and adds a jump to the end of the function.
    fn return_symbol(&mut self, sym: &Symbol, layout: &Layout<'a>);

    /// free_symbols will free all symbols for the given statement.
    fn free_symbols(&mut self, stmt: &Stmt<'a>) {
        if let Some(syms) = self.free_map().remove(&(stmt as *const Stmt<'a>)) {
            for sym in syms {
                // println!("Freeing symbol: {:?}", sym);
                self.free_symbol(&sym);
            }
        }
    }

    /// free_symbol frees any registers or stack space used to hold a symbol.
    fn free_symbol(&mut self, sym: &Symbol);

    /// set_last_seen sets the statement a symbol was last seen in.
    fn set_last_seen(&mut self, sym: Symbol, stmt: &Stmt<'a>) {
        self.last_seen_map().insert(sym, stmt);
    }

    /// last_seen_map gets the map from symbol to when it is last seen in the function.
    fn last_seen_map(&mut self) -> &mut MutMap<Symbol, *const Stmt<'a>>;

    /// set_layout_map sets the layout for a specific symbol.
    fn set_layout_map(&mut self, sym: Symbol, layout: &Layout<'a>) {
        if let Some(old_layout) = self.layout_map().insert(sym, *layout) {
            // Layout map already contains the symbol. We should never need to overwrite.
            // If the layout is not the same, that is a bug.
            if &old_layout != layout {
                internal_error!(
                    "Overwriting layout for symbol, {:?}: got {:?}, want {:?}",
                    sym,
                    layout,
                    old_layout
                )
            }
        }
    }

    /// layout_map gets the map from symbol to layout.
    fn layout_map(&mut self) -> &mut MutMap<Symbol, Layout<'a>>;

    fn create_free_map(&mut self) {
        let mut free_map = MutMap::default();
        let arena = self.env().arena;
        for (sym, stmt) in self.last_seen_map() {
            let vals = free_map
                .entry(*stmt)
                .or_insert_with(|| bumpalo::vec![in arena]);
            vals.push(*sym);
        }
        self.set_free_map(free_map);
    }

    /// free_map gets the map statement to the symbols that are free after they run.
    fn free_map(&mut self) -> &mut MutMap<*const Stmt<'a>, Vec<'a, Symbol>>;

    /// set_free_map sets the free map to the given map.
    fn set_free_map(&mut self, map: MutMap<*const Stmt<'a>, Vec<'a, Symbol>>);

    /// scan_ast runs through the ast and fill the last seen map.
    /// This must iterate through the ast in the same way that build_stmt does. i.e. then before else.
    fn scan_ast(&mut self, stmt: &Stmt<'a>) {
        // Join map keeps track of join point parameters so that we can keep them around while they still might be jumped to.
        let mut join_map: MutMap<JoinPointId, &'a [Param<'a>]> = MutMap::default();
        match stmt {
            Stmt::Let(sym, expr, _, following) => {
                self.set_last_seen(*sym, stmt);
                match expr {
                    Expr::Literal(_) => {}

                    Expr::Call(call) => self.scan_ast_call(call, stmt),

                    Expr::Tag { arguments, .. } => {
                        for sym in *arguments {
                            self.set_last_seen(*sym, stmt);
                        }
                    }
                    Expr::Struct(syms) => {
                        for sym in *syms {
                            self.set_last_seen(*sym, stmt);
                        }
                    }
                    Expr::StructAtIndex { structure, .. } => {
                        self.set_last_seen(*structure, stmt);
                    }
                    Expr::GetTagId { structure, .. } => {
                        self.set_last_seen(*structure, stmt);
                    }
                    Expr::UnionAtIndex { structure, .. } => {
                        self.set_last_seen(*structure, stmt);
                    }
                    Expr::Array { elems, .. } => {
                        for elem in *elems {
                            if let ListLiteralElement::Symbol(sym) = elem {
                                self.set_last_seen(*sym, stmt);
                            }
                        }
                    }
                    Expr::Reuse {
                        symbol,
                        arguments,
                        tag_name,
                        ..
                    } => {
                        self.set_last_seen(*symbol, stmt);
                        match tag_name {
                            TagName::Closure(sym) => {
                                self.set_last_seen(*sym, stmt);
                            }
                            TagName::Private(sym) => {
                                self.set_last_seen(*sym, stmt);
                            }
                            TagName::Global(_) => {}
                        }
                        for sym in *arguments {
                            self.set_last_seen(*sym, stmt);
                        }
                    }
                    Expr::Reset { symbol, .. } => {
                        self.set_last_seen(*symbol, stmt);
                    }
                    Expr::EmptyArray => {}
                    Expr::RuntimeErrorFunction(_) => {}
                }
                self.scan_ast(following);
            }

            Stmt::Switch {
                cond_symbol,
                branches,
                default_branch,
                ..
            } => {
                self.set_last_seen(*cond_symbol, stmt);
                for (_, _, branch) in *branches {
                    self.scan_ast(branch);
                }
                self.scan_ast(default_branch.1);
            }
            Stmt::Ret(sym) => {
                self.set_last_seen(*sym, stmt);
            }
            Stmt::Refcounting(modify, following) => {
                let sym = modify.get_symbol();

                self.set_last_seen(sym, stmt);
                self.scan_ast(following);
            }
            Stmt::Join {
                parameters,
                body: continuation,
                remainder,
                id,
                ..
            } => {
                join_map.insert(*id, parameters);
                for param in *parameters {
                    self.set_last_seen(param.symbol, stmt);
                }
                self.scan_ast(continuation);
                self.scan_ast(remainder);
            }
            Stmt::Jump(JoinPointId(sym), symbols) => {
                if let Some(parameters) = join_map.get(&JoinPointId(*sym)) {
                    // Keep the parameters around. They will be overwritten when jumping.
                    for param in *parameters {
                        self.set_last_seen(param.symbol, stmt);
                    }
                }
                self.set_last_seen(*sym, stmt);
                for sym in *symbols {
                    self.set_last_seen(*sym, stmt);
                }
            }
            Stmt::RuntimeError(_) => {}
        }
    }

    fn scan_ast_call(&mut self, call: &roc_mono::ir::Call, stmt: &roc_mono::ir::Stmt<'a>) {
        let roc_mono::ir::Call {
            call_type,
            arguments,
        } = call;

        for sym in *arguments {
            self.set_last_seen(*sym, stmt);
        }

        match call_type {
            CallType::ByName { .. } => {}
            CallType::LowLevel { .. } => {}
            CallType::HigherOrder { .. } => {}
            CallType::Foreign { .. } => {}
        }
    }
}
