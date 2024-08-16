//! Provides the compiler backend to generate Roc binaries fast, for a nice
//! developer experience. See [README.md](./compiler/gen_dev/README.md) for
//! more information.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant, clippy::upper_case_acronyms)]

use std::collections::hash_map::Entry;

use bumpalo::{collections::Vec, Bump};
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_collections::all::{MutMap, MutSet};
use roc_error_macros::{internal_error, todo_lambda_erasure};
use roc_module::ident::ModuleName;
use roc_module::low_level::{LowLevel, LowLevelWrapperType};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::code_gen_help::{CallerProc, CodeGenHelp};
use roc_mono::ir::{
    BranchInfo, CallType, CrashTag, Expr, HigherOrderLowLevel, JoinPointId, ListLiteralElement,
    Literal, ModifyRc, Param, Proc, ProcLayout, SelfRecursive, Stmt,
};
use roc_mono::layout::{
    Builtin, InLayout, LambdaName, Layout, LayoutIds, LayoutInterner, LayoutRepr, STLayoutInterner,
    TagIdIntType, UnionLayout,
};
use roc_mono::list_element_layout;

mod generic64;
mod object_builder;
pub use object_builder::build_module;
use roc_target::Target;
mod run_roc;

#[derive(Debug, Clone, Copy)]
pub enum AssemblyBackendMode {
    /// Assumes primitives (roc_alloc, roc_panic, etc) are provided by the host
    Binary,
    /// Provides a testing implementation of primitives (roc_alloc, roc_panic, etc)
    Test,
    /// Provides a testing implementation of primitives (roc_alloc, roc_panic, etc)
    Repl,
}

impl AssemblyBackendMode {
    fn generate_allocators(self) -> bool {
        match self {
            AssemblyBackendMode::Binary => false,
            AssemblyBackendMode::Test => true,
            AssemblyBackendMode::Repl => true,
        }
    }

    fn generate_roc_panic(self) -> bool {
        match self {
            AssemblyBackendMode::Binary => false,
            AssemblyBackendMode::Test => true,
            AssemblyBackendMode::Repl => true,
        }
    }

    fn generate_roc_dbg(self) -> bool {
        match self {
            AssemblyBackendMode::Binary => false,
            AssemblyBackendMode::Test => true,
            AssemblyBackendMode::Repl => true,
        }
    }
}

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub module_id: ModuleId,
    pub exposed_to_host: MutSet<Symbol>,
    pub lazy_literals: bool,
    pub mode: AssemblyBackendMode,
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

#[repr(u8)]
enum UpdateMode {
    Immutable = 0,
}

struct ListArgument<'a> {
    element_layout: InLayout<'a>,

    alignment: Symbol,
    element_width: Symbol,
    element_refcounted: Symbol,
}

// Track when a variable is last used (and hence when it can be disregarded). This is non-trivial
// in the presence of join points. Consider this example:
//
// let len = 3
//
// joinpoint f = \a ->
//     joinpoint g = \b ->
//         # len is used here
//     in
//         ...
// in
//     ...
//
// we have to keep `len` alive until after the joinpoint goes out of scope!
#[derive(Debug, Default)]
struct LastSeenMap<'a> {
    last_seen: MutMap<Symbol, *const Stmt<'a>>,
    join_map: MutMap<JoinPointId, &'a [Param<'a>]>,
}

impl<'a> LastSeenMap<'a> {
    fn set_last_seen(&mut self, symbol: Symbol, stmt: &'a Stmt<'a>) {
        self.last_seen.insert(symbol, stmt);
    }

    /// scan_ast runs through the ast and fill the last seen map.
    /// This must iterate through the ast in the same way that build_stmt does. i.e. then before else.
    fn scan_ast(root: &'a Stmt<'a>) -> MutMap<Symbol, *const Stmt<'a>> {
        let mut this: Self = Default::default();

        this.scan_ast_help(root);

        this.last_seen
    }

    fn scan_ast_help(&mut self, stmt: &'a Stmt<'a>) {
        match stmt {
            Stmt::Let(sym, expr, _, following) => {
                self.set_last_seen(*sym, stmt);
                match expr {
                    Expr::Literal(_) => {}
                    Expr::NullPointer => {}

                    Expr::Call(call) => self.scan_ast_call(call, stmt),

                    Expr::Tag {
                        arguments, reuse, ..
                    } => {
                        if let Some(ru) = reuse {
                            self.set_last_seen(ru.symbol, stmt);
                        }

                        for sym in *arguments {
                            self.set_last_seen(*sym, stmt);
                        }
                    }
                    Expr::ErasedMake { value, callee } => {
                        value.map(|v| self.set_last_seen(v, stmt));
                        self.set_last_seen(*callee, stmt);
                    }
                    Expr::ErasedLoad { symbol, field: _ } => {
                        self.set_last_seen(*symbol, stmt);
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
                    Expr::GetElementPointer { structure, .. } => {
                        self.set_last_seen(*structure, stmt);
                    }
                    Expr::Array { elems, .. } => {
                        for elem in *elems {
                            if let ListLiteralElement::Symbol(sym) = elem {
                                self.set_last_seen(*sym, stmt);
                            }
                        }
                    }
                    Expr::Reset { symbol, .. } | Expr::ResetRef { symbol, .. } => {
                        self.set_last_seen(*symbol, stmt);
                    }
                    Expr::Alloca { initializer, .. } => {
                        if let Some(initializer) = initializer {
                            self.set_last_seen(*initializer, stmt);
                        }
                    }
                    Expr::RuntimeErrorFunction(_) => {}
                    Expr::FunctionPointer { .. } => todo_lambda_erasure!(),
                    Expr::EmptyArray => {}
                }
                self.scan_ast_help(following);
            }

            Stmt::Switch {
                cond_symbol,
                branches,
                default_branch,
                ..
            } => {
                self.set_last_seen(*cond_symbol, stmt);
                for (_, _, branch) in *branches {
                    self.scan_ast_help(branch);
                }
                self.scan_ast_help(default_branch.1);
            }
            Stmt::Ret(sym) => {
                self.set_last_seen(*sym, stmt);
            }
            Stmt::Refcounting(modify, following) => {
                let sym = modify.get_symbol();

                self.set_last_seen(sym, stmt);
                self.scan_ast_help(following);
            }
            Stmt::Join {
                parameters,
                body: continuation,
                remainder,
                id: JoinPointId(sym),
                ..
            } => {
                self.set_last_seen(*sym, stmt);
                self.join_map.insert(JoinPointId(*sym), parameters);
                self.scan_ast_help(remainder);

                for (symbol, symbol_stmt) in Self::scan_ast(continuation) {
                    match self.last_seen.entry(symbol) {
                        Entry::Occupied(mut occupied) => {
                            // lives for the joinpoint
                            occupied.insert(stmt);
                        }
                        Entry::Vacant(vacant) => {
                            // lives for some time within the continuation
                            vacant.insert(symbol_stmt);
                        }
                    }
                }

                for param in *parameters {
                    self.set_last_seen(param.symbol, stmt);
                }
            }
            Stmt::Jump(JoinPointId(sym), symbols) => {
                if let Some(parameters) = self.join_map.get(&JoinPointId(*sym)) {
                    // Keep the parameters around. They will be overwritten when jumping.
                    for param in *parameters {
                        self.set_last_seen(param.symbol, stmt);
                    }
                }
                for sym in *symbols {
                    self.set_last_seen(*sym, stmt);
                }
            }

            Stmt::Dbg { .. } => todo!("dbg not implemented in the dev backend"),
            Stmt::Expect { .. } => todo!("expect is not implemented in the dev backend"),
            Stmt::ExpectFx { .. } => todo!("expect-fx is not implemented in the dev backend"),

            Stmt::Crash(msg, _crash_tag) => {
                self.set_last_seen(*msg, stmt);
            }
        }
    }

    fn scan_ast_call(&mut self, call: &roc_mono::ir::Call, stmt: &'a roc_mono::ir::Stmt<'a>) {
        let roc_mono::ir::Call {
            call_type,
            arguments,
        } = call;

        for sym in *arguments {
            self.set_last_seen(*sym, stmt);
        }

        match call_type {
            CallType::ByName { .. } => {}
            CallType::ByPointer { .. } => {}
            CallType::LowLevel { .. } => {}
            CallType::HigherOrder { .. } => {}
            CallType::Foreign { .. } => {}
        }
    }
}

trait Backend<'a> {
    fn env(&self) -> &Env<'a>;
    fn interns(&self) -> &Interns;
    fn interns_mut(&mut self) -> &mut Interns;
    fn interner(&self) -> &STLayoutInterner<'a>;
    fn relocations_mut(&mut self) -> &mut Vec<'a, Relocation>;
    fn target(&self) -> Target;

    fn interner_mut(&mut self) -> &mut STLayoutInterner<'a> {
        self.module_interns_helpers_mut().1
    }

    fn debug_symbol(&mut self, name: &str) -> Symbol {
        let module_id = self.env().module_id;

        self.debug_symbol_in(module_id, name)
    }

    fn debug_symbol_in(&mut self, module_id: ModuleId, name: &str) -> Symbol {
        let ident_ids = self
            .interns_mut()
            .all_ident_ids
            .get_mut(&module_id)
            .unwrap();

        let ident_id = ident_ids.add_str(name);
        Symbol::new(module_id, ident_id)
    }

    // This method is suboptimal, but it seems to be the only way to make rust understand
    // that all of these values can be mutable at the same time. By returning them together,
    // rust understands that they are part of a single use of mutable self.
    fn module_interns_helpers_mut(
        &mut self,
    ) -> (
        ModuleId,
        &mut STLayoutInterner<'a>,
        &mut Interns,
        &mut CodeGenHelp<'a>,
        &mut Vec<'a, CallerProc<'a>>,
    );

    fn lambda_name_to_string<'b, I>(
        &self,
        name: LambdaName,
        arguments: I,
        _lambda_set: Option<InLayout>,
        result: InLayout,
    ) -> String
    where
        I: Iterator<Item = InLayout<'b>>,
    {
        use std::hash::{BuildHasher, Hash, Hasher};

        let symbol = name.name();

        // NOTE: due to randomness, this will not be consistent between runs
        let mut state = roc_collections::all::BuildHasher::default().build_hasher();
        for a in arguments {
            a.hash(&mut state);
        }

        // lambda set should not matter; it should already be added as an argument
        // but the niche of the lambda name may be the only thing differentiating two different
        // implementations of a function with the same symbol
        name.niche().hash(&mut state);
        result.hash(&mut state);

        let interns = self.interns();
        let ident_string = symbol.as_str(interns);
        let module_string = interns.module_ids.get_name(symbol.module_id()).unwrap();

        // the functions from the generates #help module (refcounting, equality) is always suffixed
        // with 1. That is fine, they are always unique anyway.
        if ident_string.contains("#help") {
            format!("{module_string}_{ident_string}_1")
        } else {
            format!("{}_{}_{}", module_string, ident_string, state.finish())
        }
    }

    fn defined_in_app_module(&self, symbol: Symbol) -> bool {
        symbol
            .module_string(self.interns())
            .starts_with(ModuleName::APP)
    }

    fn list_argument(&mut self, list_layout: InLayout<'a>) -> ListArgument<'a> {
        let element_layout = match self.interner().get_repr(list_layout) {
            LayoutRepr::Builtin(Builtin::List(e)) => e,
            _ => unreachable!(),
        };

        let (element_width_int, alignment_int) =
            self.interner().stack_size_and_alignment(element_layout);

        let alignment = self.debug_symbol("alignment");
        self.load_literal_i32(&alignment, Ord::max(alignment_int, 8) as i32);

        let element_width = self.debug_symbol("element_width");
        self.load_literal_i64(&element_width, element_width_int as i64);

        let element_refcounted = self.debug_symbol("element_refcounted");
        let refcounted = self.interner().contains_refcounted(element_layout);
        self.load_literal(
            &element_refcounted,
            &Layout::BOOL,
            &Literal::Bool(refcounted),
        );

        ListArgument {
            element_layout,
            alignment,
            element_width,
            element_refcounted,
        }
    }

    fn increment_fn_pointer(&mut self, layout: InLayout<'a>) -> Symbol {
        let box_layout = self
            .interner_mut()
            .insert_direct_no_semantic(LayoutRepr::Ptr(layout));

        let element_increment = self.debug_symbol("element_increment");
        let element_increment_symbol = self.build_indirect_inc(layout);

        let element_increment_string = self.lambda_name_to_string(
            LambdaName::no_niche(element_increment_symbol),
            [box_layout].into_iter(),
            None,
            Layout::UNIT,
        );

        self.build_fn_pointer(&element_increment, element_increment_string);

        element_increment
    }

    fn increment_n_fn_pointer(&mut self, layout: InLayout<'a>) -> Symbol {
        let box_layout = self
            .interner_mut()
            .insert_direct_no_semantic(LayoutRepr::Ptr(layout));

        let element_increment = self.debug_symbol("element_increment_n");
        let element_increment_symbol = self.build_indirect_inc_n(layout);

        let element_increment_string = self.lambda_name_to_string(
            LambdaName::no_niche(element_increment_symbol),
            [box_layout, Layout::I64].into_iter(),
            None,
            Layout::UNIT,
        );

        self.build_fn_pointer(&element_increment, element_increment_string);

        element_increment
    }

    fn decrement_fn_pointer(&mut self, layout: InLayout<'a>) -> Symbol {
        let box_layout = self
            .interner_mut()
            .insert_direct_no_semantic(LayoutRepr::Ptr(layout));

        let element_decrement = self.debug_symbol("element_decrement");
        let element_decrement_symbol = self.build_indirect_dec(layout);

        let element_decrement_string = self.lambda_name_to_string(
            LambdaName::no_niche(element_decrement_symbol),
            [box_layout].into_iter(),
            None,
            Layout::UNIT,
        );

        self.build_fn_pointer(&element_decrement, element_decrement_string);

        element_decrement
    }

    fn copy_fn_pointer(&mut self, layout: InLayout<'a>) -> Symbol {
        let box_layout = self
            .interner_mut()
            .insert_direct_no_semantic(LayoutRepr::Ptr(layout));

        let element_copy = self.debug_symbol("element_copy");
        let element_copy_symbol = self.build_indirect_copy(layout);

        let element_copy_string = self.lambda_name_to_string(
            LambdaName::no_niche(element_copy_symbol),
            [box_layout, box_layout].into_iter(),
            None,
            Layout::UNIT,
        );

        self.build_fn_pointer(&element_copy, element_copy_string);

        element_copy
    }

    fn helper_proc_gen_mut(&mut self) -> &mut CodeGenHelp<'a>;

    fn helper_proc_symbols_mut(&mut self) -> &mut Vec<'a, (Symbol, ProcLayout<'a>)>;

    fn helper_proc_symbols(&self) -> &Vec<'a, (Symbol, ProcLayout<'a>)>;
    fn caller_procs(&self) -> &Vec<'a, CallerProc<'a>>;

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
    fn load_args(&mut self, args: &'a [(InLayout<'a>, Symbol)], ret_layout: &InLayout<'a>);

    /// Used for generating wrappers for malloc/realloc/free
    fn build_wrapped_jmp(&mut self) -> (&'a [u8], u64);

    // use for roc_panic
    fn build_roc_setjmp(&mut self) -> &'a [u8];
    fn build_roc_longjmp(&mut self) -> &'a [u8];
    fn build_roc_panic(&mut self) -> (&'a [u8], Vec<'a, Relocation>);

    /// build_proc creates a procedure and outputs it to the wrapped object writer.
    /// Returns the procedure bytes, its relocations, and the names of the refcounting functions it references.
    fn build_proc(
        &mut self,
        proc: Proc<'a>,
        layout_ids: &mut LayoutIds<'a>,
    ) -> (Vec<u8>, Vec<Relocation>, Vec<'a, (Symbol, String)>) {
        let proc_name = self.lambda_name_to_string(
            proc.name,
            proc.args.iter().map(|t| t.0),
            proc.closure_data_layout,
            proc.ret_layout,
        );

        let body = self.env().arena.alloc(proc.body);

        self.reset(proc_name, proc.is_self_recursive);
        self.load_args(proc.args, &proc.ret_layout);
        for (layout, sym) in proc.args {
            self.set_layout_map(*sym, layout);
        }
        self.scan_ast(body);
        self.create_free_map();
        self.build_stmt(layout_ids, body, &proc.ret_layout);

        let mut helper_proc_names = bumpalo::vec![in self.env().arena];
        helper_proc_names.reserve(self.helper_proc_symbols().len());
        for (rc_proc_sym, rc_proc_layout) in self.helper_proc_symbols() {
            let name = layout_ids
                .get_toplevel(*rc_proc_sym, rc_proc_layout)
                .to_symbol_string(*rc_proc_sym, self.interns());

            helper_proc_names.push((*rc_proc_sym, name));
        }

        for caller_proc in self.caller_procs() {
            let proc_layout = caller_proc.proc_layout;
            let proc_symbol = caller_proc.proc_symbol;
            let name = layout_ids
                .get_toplevel(proc_symbol, &proc_layout)
                .to_symbol_string(proc_symbol, self.interns());

            helper_proc_names.push((proc_symbol, name));
        }

        let (bytes, relocs) = self.finalize();
        (bytes, relocs, helper_proc_names)
    }

    /// build_stmt builds a statement and outputs at the end of the buffer.
    fn build_stmt(
        &mut self,
        layout_ids: &mut LayoutIds<'a>,
        stmt: &Stmt<'a>,
        ret_layout: &InLayout<'a>,
    ) {
        match stmt {
            Stmt::Let(sym, expr, layout, following) => {
                self.build_expr(sym, expr, layout);
                self.set_layout_map(*sym, layout);
                self.free_symbols(stmt);
                self.build_stmt(layout_ids, following, ret_layout);
            }
            Stmt::Ret(sym) => {
                self.load_literal_symbols(&[*sym]);
                self.return_symbol(sym, ret_layout);
                self.free_symbols(stmt);
            }
            Stmt::Refcounting(ModifyRc::Free(symbol), following) => {
                let dst = Symbol::DEV_TMP;

                let layout = *self.layout_map().get(symbol).unwrap();
                debug_assert!(!matches!(self.interner().get_repr(layout), LayoutRepr::Builtin(Builtin::List(_))), "List are no longer safe to refcount through pointer alone. They must go through the zig bitcode functions");

                let alignment_bytes = self.interner().allocation_alignment_bytes(layout);
                let alignment = self.debug_symbol("alignment");
                self.load_literal_i32(&alignment, alignment_bytes as i32);

                // elems_refcounted (always false except for list which are refcounted differently)
                let elems_refcounted = self.debug_symbol("elems_refcounted");
                self.load_literal(&elems_refcounted, &Layout::BOOL, &Literal::Bool(false));

                // NOTE: UTILS_FREE_DATA_PTR clears any tag id bits

                self.build_fn_call(
                    &dst,
                    bitcode::UTILS_FREE_DATA_PTR.to_string(),
                    &[*symbol, alignment, elems_refcounted],
                    &[Layout::I64, Layout::I32, Layout::BOOL],
                    &Layout::UNIT,
                );

                self.free_symbol(&dst);
                self.free_symbol(&alignment);
                self.free_symbol(&elems_refcounted);

                self.build_stmt(layout_ids, following, ret_layout)
            }
            Stmt::Refcounting(modify, following) => {
                let sym = modify.get_symbol();
                let layout = *self.layout_map().get(&sym).unwrap();

                // Expand the Refcounting statement into more detailed IR with a function call
                // If this layout requires a new RC proc, we get enough info to create a linker symbol
                // for it. Here we don't create linker symbols at this time, but in Wasm backend, we do.
                let (rc_stmt, new_specializations) = {
                    let (module_id, layout_interner, interns, rc_proc_gen, _) =
                        self.module_interns_helpers_mut();
                    let ident_ids = interns.all_ident_ids.get_mut(&module_id).unwrap();

                    rc_proc_gen.expand_refcount_stmt(
                        ident_ids,
                        layout_interner,
                        layout,
                        modify,
                        following,
                    )
                };

                for spec in new_specializations.into_iter() {
                    self.helper_proc_symbols_mut().push(spec);
                }

                self.build_stmt(layout_ids, rc_stmt, ret_layout)
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
                    layout_ids,
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
                self.build_join(layout_ids, id, parameters, body, remainder, ret_layout);
                self.free_symbols(stmt);
            }
            Stmt::Jump(id, args) => {
                self.load_literal_symbols(args);
                let mut arg_layouts: bumpalo::collections::Vec<InLayout<'a>> =
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
            Stmt::Crash(msg, crash_tag) => self.roc_panic(*msg, *crash_tag),
            x => todo!("the statement, {:?}", x),
        }
    }

    fn roc_panic(&mut self, msg: Symbol, crash_tag: CrashTag) {
        let error_message = self.debug_symbol("error_message");

        self.load_literal(
            &error_message,
            &Layout::U32,
            &Literal::Int((crash_tag as u128).to_ne_bytes()),
        );

        // this function gets a RocStr, but the roc_panic defined by a platform expects a `*RocStr`.
        // we store the value in a global variable and then use a pointer to this global
        let panic_msg_ptr = self.debug_symbol("panic_msg_ptr");
        let ignored = self.debug_symbol("ignored");
        self.build_data_pointer(&panic_msg_ptr, "panic_msg".to_string());
        self.load_literal_symbols(&[msg]);
        self.build_ptr_store(ignored, panic_msg_ptr, msg, Layout::STR);

        // Now that the arguments are needed, load them if they are literals.
        let arguments = &[panic_msg_ptr, error_message];
        self.load_literal_symbols(arguments);
        self.build_fn_call(
            &Symbol::DEV_TMP2,
            String::from("roc_panic"),
            arguments,
            &[Layout::U64, Layout::U32],
            &Layout::UNIT,
        );

        self.free_symbol(&error_message);
        self.free_symbol(&Symbol::DEV_TMP2);
    }

    // build_switch generates a instructions for a switch statement.
    fn build_switch(
        &mut self,
        layout_ids: &mut LayoutIds<'a>,
        cond_symbol: &Symbol,
        cond_layout: &InLayout<'a>,
        branches: &'a [(u64, BranchInfo<'a>, Stmt<'a>)],
        default_branch: &(BranchInfo<'a>, &'a Stmt<'a>),
        ret_layout: &InLayout<'a>,
    );

    // build_join generates a instructions for a join statement.
    fn build_join(
        &mut self,
        layout_ids: &mut LayoutIds<'a>,
        id: &JoinPointId,
        parameters: &'a [Param<'a>],
        body: &'a Stmt<'a>,
        remainder: &'a Stmt<'a>,
        ret_layout: &InLayout<'a>,
    );

    // build_jump generates a instructions for a jump statement.
    fn build_jump(
        &mut self,
        id: &JoinPointId,
        args: &[Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    );

    /// build_expr builds the expressions for the specified symbol.
    /// The builder must keep track of the symbol because it may be referred to later.
    fn build_expr(&mut self, sym: &Symbol, expr: &Expr<'a>, layout: &InLayout<'a>) {
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
                            LowLevelWrapperType::from_symbol(func_sym.name())
                        {
                            return self.build_run_low_level(
                                sym,
                                &lowlevel,
                                arguments,
                                arg_layouts,
                                ret_layout,
                            );
                        } else if func_sym.name().is_builtin() {
                            // These builtins can be built through `build_fn_call` as well, but the
                            // implementation in `build_builtin` inlines some of the symbols.
                            return self.build_builtin(
                                sym,
                                *func_sym,
                                arguments,
                                arg_layouts,
                                ret_layout,
                            );
                        }

                        let fn_name = self.lambda_name_to_string(
                            *func_sym,
                            arg_layouts.iter().copied(),
                            None,
                            *ret_layout,
                        );

                        // Now that the arguments are needed, load them if they are literals.
                        self.load_literal_symbols(arguments);
                        self.build_fn_call(sym, fn_name, arguments, arg_layouts, ret_layout)
                    }

                    CallType::ByPointer { .. } => {
                        todo_lambda_erasure!()
                    }

                    CallType::LowLevel { op: lowlevel, .. } => {
                        let mut arg_layouts: bumpalo::collections::Vec<InLayout<'a>> =
                            bumpalo::vec![in self.env().arena];
                        arg_layouts.reserve(arguments.len());
                        let layout_map = self.layout_map();
                        for arg in *arguments {
                            if let Some(layout) = layout_map.get(arg) {
                                arg_layouts.push(*layout);
                            } else if matches!(lowlevel, LowLevel::ListDecref) {
                                // The last arg of ListDecref has no layout. It is a proc symbol.
                                continue;
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
                    CallType::HigherOrder(higher_order) => {
                        self.build_higher_order_lowlevel(sym, higher_order, *layout)
                    }
                    CallType::Foreign {
                        foreign_symbol,
                        ret_layout,
                    } => {
                        let mut arg_layouts: bumpalo::collections::Vec<InLayout<'a>> =
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

                        self.load_literal_symbols(arguments);
                        self.build_fn_call(
                            sym,
                            foreign_symbol.as_str().to_string(),
                            arguments,
                            arg_layouts.into_bump_slice(),
                            ret_layout,
                        );
                    }
                }
            }
            Expr::EmptyArray => {
                self.create_empty_array(sym);
            }
            Expr::Array { elem_layout, elems } => {
                let mut syms = bumpalo::vec![in self.env().arena];
                for sym in elems.iter().filter_map(|x| match x {
                    ListLiteralElement::Symbol(sym) => Some(sym),
                    _ => None,
                }) {
                    syms.push(*sym);
                }
                self.create_array(sym, elem_layout, elems);
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
            Expr::UnionAtIndex {
                structure,
                tag_id,
                union_layout,
                index,
            } => {
                self.load_union_at_index(sym, structure, *tag_id, *index, union_layout);
            }
            Expr::GetElementPointer {
                structure,
                union_layout,
                indices,
                ..
            } => {
                debug_assert!(indices.len() >= 2);
                self.load_union_field_ptr_at_index(
                    sym,
                    structure,
                    indices[0] as u16,
                    indices[1],
                    union_layout,
                );
            }
            Expr::GetTagId {
                structure,
                union_layout,
            } => {
                self.get_tag_id(sym, structure, union_layout);
            }
            Expr::Tag {
                tag_layout,
                tag_id,
                arguments,
                reuse,
            } => {
                self.load_literal_symbols(arguments);
                let reuse = reuse.map(|ru| ru.symbol);
                self.tag(sym, arguments, tag_layout, *tag_id, reuse);
            }
            Expr::NullPointer => {
                self.load_literal_i64(sym, 0);
            }
            Expr::FunctionPointer { .. } => todo_lambda_erasure!(),
            Expr::ErasedMake { .. } => todo_lambda_erasure!(),
            Expr::ErasedLoad { .. } => todo_lambda_erasure!(),
            Expr::Reset { symbol, .. } => {
                let layout = *self.layout_map().get(symbol).unwrap();

                // Expand the Refcounting statement into more detailed IR with a function call
                // If this layout requires a new RC proc, we get enough info to create a linker symbol
                // for it. Here we don't create linker symbols at this time, but in Wasm backend, we do.
                let (new_expr, new_specializations) = {
                    let (module_id, layout_interner, interns, rc_proc_gen, _) =
                        self.module_interns_helpers_mut();
                    let ident_ids = interns.all_ident_ids.get_mut(&module_id).unwrap();

                    rc_proc_gen.call_reset_refcount(ident_ids, layout_interner, layout, *symbol)
                };

                for spec in new_specializations.into_iter() {
                    self.helper_proc_symbols_mut().push(spec);
                }

                self.build_expr(sym, &new_expr, &Layout::BOOL)
            }
            Expr::ResetRef { symbol, .. } => {
                let layout = *self.layout_map().get(symbol).unwrap();

                // Expand the Refcounting statement into more detailed IR with a function call
                // If this layout requires a new RC proc, we get enough info to create a linker symbol
                // for it. Here we don't create linker symbols at this time, but in Wasm backend, we do.
                let (new_expr, new_specializations) = {
                    let (module_id, layout_interner, interns, rc_proc_gen, _) =
                        self.module_interns_helpers_mut();
                    let ident_ids = interns.all_ident_ids.get_mut(&module_id).unwrap();

                    rc_proc_gen.call_resetref_refcount(ident_ids, layout_interner, layout, *symbol)
                };

                for spec in new_specializations.into_iter() {
                    self.helper_proc_symbols_mut().push(spec);
                }

                self.build_expr(sym, &new_expr, &Layout::BOOL)
            }
            Expr::Alloca {
                initializer,
                element_layout,
            } => {
                self.build_alloca(*sym, *initializer, *element_layout);
            }
            Expr::RuntimeErrorFunction(_) => todo!(),
        }
    }

    /// build_run_low_level builds the low level opertation and outputs to the specified symbol.
    /// The builder must keep track of the symbol because it may be referred to later.
    fn build_run_low_level(
        &mut self,
        sym: &Symbol,
        lowlevel: &LowLevel,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
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
            LowLevel::NumAddSaturated => {
                self.build_num_add_saturated(*sym, args[0], args[1], *ret_layout);
            }
            LowLevel::NumAddWrap => {
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
                self.build_num_add_wrap(sym, &args[0], &args[1], ret_layout)
            }
            LowLevel::NumAddChecked => {
                self.build_num_add_checked(sym, &args[0], &args[1], &arg_layouts[0], ret_layout)
            }
            LowLevel::NumSubChecked => {
                self.build_num_sub_checked(sym, &args[0], &args[1], &arg_layouts[0], ret_layout)
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
            LowLevel::NumMul => self.build_num_mul(sym, &args[0], &args[1], ret_layout),
            LowLevel::NumMulWrap => self.build_num_mul_wrap(sym, &args[0], &args[1], ret_layout),
            LowLevel::NumMulSaturated => {
                self.build_num_mul_saturated(*sym, args[0], args[1], *ret_layout);
            }
            LowLevel::NumMulChecked => {
                self.build_num_mul_checked(sym, &args[0], &args[1], &arg_layouts[0], ret_layout)
            }
            LowLevel::NumDivTruncUnchecked | LowLevel::NumDivFrac => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NumDiv: expected to have exactly two argument"
                );
                debug_assert_eq!(
                    arg_layouts[0], arg_layouts[1],
                    "NumDiv: expected all arguments of to have the same layout"
                );
                debug_assert_eq!(
                    arg_layouts[0], *ret_layout,
                    "NumDiv: expected to have the same argument and return layout"
                );
                self.build_num_div(sym, &args[0], &args[1], ret_layout)
            }

            LowLevel::NumDivCeilUnchecked => {
                self.build_num_div_ceil(sym, &args[0], &args[1], ret_layout)
            }
            LowLevel::NumRemUnchecked => self.build_num_rem(sym, &args[0], &args[1], ret_layout),
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
            LowLevel::NumPowInt => {
                let repr = self.interner().get_repr(arg_layouts[0]);
                let LayoutRepr::Builtin(Builtin::Int(int_width)) = repr else {
                    unreachable!("invalid layout for NumPowInt")
                };

                self.build_fn_call(
                    sym,
                    bitcode::NUM_POW_INT[int_width].to_string(),
                    args,
                    arg_layouts,
                    ret_layout,
                )
            }
            LowLevel::NumPow => {
                let intrinsic = match self.interner().get_repr(arg_layouts[0]) {
                    LayoutRepr::Builtin(Builtin::Float(float_width)) => {
                        &bitcode::NUM_POW[float_width]
                    }
                    LayoutRepr::DEC => bitcode::DEC_POW,
                    _ => unreachable!("invalid layout for NumPow"),
                };

                self.build_fn_call(sym, intrinsic.to_string(), args, arg_layouts, ret_layout)
            }

            LowLevel::NumRound => {
                let repr = self.interner().get_repr(*ret_layout);
                let LayoutRepr::Builtin(Builtin::Int(int_width)) = repr else {
                    unreachable!("invalid return layout for NumRound")
                };

                let intrinsic = match arg_layouts[0] {
                    Layout::F32 => &bitcode::NUM_ROUND_F32[int_width],
                    Layout::F64 => &bitcode::NUM_ROUND_F64[int_width],
                    Layout::DEC => &bitcode::DEC_ROUND[int_width],
                    _ => unreachable!("invalid layout for NumRound"),
                };

                self.build_fn_call(sym, intrinsic.to_string(), args, arg_layouts, ret_layout)
            }

            LowLevel::NumFloor => {
                let repr = self.interner().get_repr(*ret_layout);
                let LayoutRepr::Builtin(Builtin::Int(int_width)) = repr else {
                    unreachable!("invalid return layout for NumFloor")
                };

                let intrinsic = match arg_layouts[0] {
                    Layout::F32 => &bitcode::NUM_FLOOR_F32[int_width],
                    Layout::F64 => &bitcode::NUM_FLOOR_F64[int_width],
                    Layout::DEC => &bitcode::DEC_FLOOR[int_width],
                    _ => unreachable!("invalid layout for NumFloor"),
                };

                self.build_fn_call(sym, intrinsic.to_string(), args, arg_layouts, ret_layout)
            }

            LowLevel::NumCeiling => {
                let repr = self.interner().get_repr(*ret_layout);
                let LayoutRepr::Builtin(Builtin::Int(int_width)) = repr else {
                    unreachable!("invalid return layout for NumCeiling")
                };

                let intrinsic = match arg_layouts[0] {
                    Layout::F32 => &bitcode::NUM_CEILING_F32[int_width],
                    Layout::F64 => &bitcode::NUM_CEILING_F64[int_width],
                    Layout::DEC => &bitcode::DEC_CEILING[int_width],
                    _ => unreachable!("invalid layout for NumCeiling"),
                };

                self.build_fn_call(sym, intrinsic.to_string(), args, arg_layouts, ret_layout)
            }

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
            LowLevel::NumSubWrap => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NumSubWrap: expected to have exactly two argument"
                );
                debug_assert_eq!(
                    arg_layouts[0], arg_layouts[1],
                    "NumSubWrap: expected all arguments of to have the same layout"
                );
                debug_assert_eq!(
                    arg_layouts[0], *ret_layout,
                    "NumSubWrap: expected to have the same argument and return layout"
                );
                self.build_num_sub_wrap(sym, &args[0], &args[1], ret_layout)
            }
            LowLevel::NumSubSaturated => match self.interner().get_repr(*ret_layout) {
                LayoutRepr::Builtin(Builtin::Int(int_width)) => self.build_fn_call(
                    sym,
                    bitcode::NUM_SUB_SATURATED_INT[int_width].to_string(),
                    args,
                    arg_layouts,
                    ret_layout,
                ),
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)) => {
                    self.build_num_sub(sym, &args[0], &args[1], ret_layout)
                }
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                    // saturated sub is just normal sub
                    self.build_num_sub(sym, &args[0], &args[1], ret_layout)
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    // self.load_args_and_call_zig(backend, bitcode::DEC_SUB_SATURATED)
                    todo!()
                }
                _ => internal_error!("invalid return type"),
            },
            LowLevel::NumBitwiseAnd => {
                if let LayoutRepr::Builtin(Builtin::Int(int_width)) =
                    self.interner().get_repr(*ret_layout)
                {
                    self.build_int_bitwise_and(sym, &args[0], &args[1], int_width)
                } else {
                    internal_error!("bitwise and on a non-integer")
                }
            }
            LowLevel::NumBitwiseOr => {
                if let LayoutRepr::Builtin(Builtin::Int(int_width)) =
                    self.interner().get_repr(*ret_layout)
                {
                    self.build_int_bitwise_or(sym, &args[0], &args[1], int_width)
                } else {
                    internal_error!("bitwise or on a non-integer")
                }
            }
            LowLevel::NumBitwiseXor => {
                if let LayoutRepr::Builtin(Builtin::Int(int_width)) =
                    self.interner().get_repr(*ret_layout)
                {
                    self.build_int_bitwise_xor(sym, &args[0], &args[1], int_width)
                } else {
                    internal_error!("bitwise xor on a non-integer")
                }
            }
            LowLevel::And => {
                if let LayoutRepr::Builtin(Builtin::Bool) = self.interner().get_repr(*ret_layout) {
                    self.build_int_bitwise_and(sym, &args[0], &args[1], IntWidth::U8)
                } else {
                    internal_error!("bitwise and on a non-integer")
                }
            }
            LowLevel::Or => {
                if let LayoutRepr::Builtin(Builtin::Bool) = self.interner().get_repr(*ret_layout) {
                    self.build_int_bitwise_or(sym, &args[0], &args[1], IntWidth::U8)
                } else {
                    internal_error!("bitwise or on a non-integer")
                }
            }
            LowLevel::NumShiftLeftBy => {
                if let LayoutRepr::Builtin(Builtin::Int(int_width)) =
                    self.interner().get_repr(*ret_layout)
                {
                    self.build_int_shift_left(sym, &args[0], &args[1], int_width)
                } else {
                    internal_error!("shift left on a non-integer")
                }
            }
            LowLevel::NumShiftRightBy => {
                if let LayoutRepr::Builtin(Builtin::Int(int_width)) =
                    self.interner().get_repr(*ret_layout)
                {
                    self.build_int_shift_right(sym, &args[0], &args[1], int_width)
                } else {
                    internal_error!("shift right on a non-integer")
                }
            }
            LowLevel::NumShiftRightZfBy => {
                if let LayoutRepr::Builtin(Builtin::Int(int_width)) =
                    self.interner().get_repr(*ret_layout)
                {
                    self.build_int_shift_right_zero_fill(sym, &args[0], &args[1], int_width)
                } else {
                    internal_error!("shift right zero-fill on a non-integer")
                }
            }
            LowLevel::Eq => {
                debug_assert_eq!(2, args.len(), "Eq: expected to have exactly two argument");

                let a = Layout::runtime_representation_in(arg_layouts[0], self.interner());
                let b = Layout::runtime_representation_in(arg_layouts[1], self.interner());

                debug_assert!(
                    self.interner().eq_repr(a, b),
                    "Eq: expected all arguments to have the same layout, but {} != {}",
                    self.interner().dbg(a),
                    self.interner().dbg(b),
                );

                self.build_eq(sym, &args[0], &args[1], &arg_layouts[0])
            }
            LowLevel::NotEq => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NotEq: expected to have exactly two argument"
                );

                let a = Layout::runtime_representation_in(arg_layouts[0], self.interner());
                let b = Layout::runtime_representation_in(arg_layouts[1], self.interner());

                debug_assert!(
                    self.interner().eq_repr(a, b),
                    "NotEq: expected all arguments to have the same layout, but {} != {}",
                    self.interner().dbg(a),
                    self.interner().dbg(b),
                );
                debug_assert!(
                    self.interner().eq_repr(Layout::BOOL, *ret_layout,),
                    "NotEq: expected to have return layout of type Bool"
                );
                self.build_neq(sym, &args[0], &args[1], &arg_layouts[0])
            }
            LowLevel::Not => {
                debug_assert_eq!(1, args.len(), "Not: expected to have exactly one argument");
                debug_assert!(
                    self.interner().eq_repr(Layout::BOOL, *ret_layout,),
                    "Not: expected to have return layout of type Bool"
                );
                self.build_not(sym, &args[0], &arg_layouts[0])
            }
            LowLevel::NumLt => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NumLt: expected to have exactly two argument"
                );
                debug_assert!(
                    self.interner().eq_repr(arg_layouts[0], arg_layouts[1],),
                    "NumLt: expected all arguments of to have the same layout"
                );
                debug_assert!(
                    self.interner().eq_repr(Layout::BOOL, *ret_layout,),
                    "NumLt: expected to have return layout of type Bool"
                );
                self.build_num_lt(sym, &args[0], &args[1], &arg_layouts[0])
            }
            LowLevel::NumGt => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NumGt: expected to have exactly two argument"
                );
                debug_assert!(
                    self.interner().eq_repr(arg_layouts[0], arg_layouts[1],),
                    "NumGt: expected all arguments of to have the same layout"
                );
                debug_assert!(
                    self.interner().eq_repr(Layout::BOOL, *ret_layout,),
                    "NumGt: expected to have return layout of type Bool"
                );
                self.build_num_gt(sym, &args[0], &args[1], &arg_layouts[0])
            }
            LowLevel::NumToFrac => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "NumToFrac: expected to have exactly one argument"
                );

                debug_assert!(
                    matches!(*ret_layout, Layout::F32 | Layout::F64 | Layout::DEC),
                    "NumToFrac: expected to have return layout of type Float"
                );
                self.build_num_to_frac(sym, &args[0], &arg_layouts[0], ret_layout)
            }
            LowLevel::NumIsNan => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "NumIsNan: expected to have exactly one argument"
                );

                debug_assert!(
                    self.interner().eq_repr(Layout::BOOL, *ret_layout,),
                    "NumIsNan: expected to have return layout of type Bool"
                );
                self.build_num_is_nan(sym, &args[0], &arg_layouts[0])
            }
            LowLevel::NumIsInfinite => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "NumIsInfinite: expected to have exactly one argument"
                );

                debug_assert!(
                    self.interner().eq_repr(Layout::BOOL, *ret_layout,),
                    "NumIsInfinite: expected to have return layout of type Bool"
                );
                self.build_num_is_infinite(sym, &args[0], &arg_layouts[0])
            }
            LowLevel::NumIsFinite => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "NumIsFinite: expected to have exactly one argument"
                );

                debug_assert!(
                    self.interner().eq_repr(Layout::BOOL, *ret_layout,),
                    "NumIsFinite: expected to have return layout of type Bool"
                );
                self.build_num_is_finite(sym, &args[0], &arg_layouts[0])
            }
            LowLevel::NumLte => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "NumLte: expected to have exactly two argument"
                );
                debug_assert_eq!(
                    arg_layouts[0], arg_layouts[1],
                    "NumLte: expected all arguments of to have the same layout"
                );
                debug_assert!(
                    self.interner().eq_repr(Layout::BOOL, *ret_layout,),
                    "NumLte: expected to have return layout of type Bool"
                );
                self.build_num_lte(sym, &args[0], &args[1], &arg_layouts[0])
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
                debug_assert!(
                    self.interner().eq_repr(Layout::BOOL, *ret_layout,),
                    "NumGte: expected to have return layout of type Bool"
                );
                self.build_num_gte(sym, &args[0], &args[1], &arg_layouts[0])
            }
            LowLevel::NumLogUnchecked => {
                let float_width = match arg_layouts[0] {
                    Layout::F64 => FloatWidth::F64,
                    Layout::F32 => FloatWidth::F32,
                    _ => unreachable!("invalid layout for sqrt"),
                };

                self.build_fn_call(
                    sym,
                    bitcode::NUM_LOG[float_width].to_string(),
                    args,
                    arg_layouts,
                    ret_layout,
                )
            }
            LowLevel::NumSqrtUnchecked => {
                let float_width = match arg_layouts[0] {
                    Layout::F64 => FloatWidth::F64,
                    Layout::F32 => FloatWidth::F32,
                    _ => unreachable!("invalid layout for sqrt"),
                };

                self.build_num_sqrt(*sym, args[0], float_width);
            }
            LowLevel::NumSin => {
                let intrinsic = match arg_layouts[0] {
                    Layout::F64 => &bitcode::NUM_SIN[FloatWidth::F64],
                    Layout::F32 => &bitcode::NUM_SIN[FloatWidth::F32],
                    Layout::DEC => bitcode::DEC_SIN,
                    _ => unreachable!("invalid layout for sin"),
                };

                self.build_fn_call(sym, intrinsic.to_string(), args, arg_layouts, ret_layout)
            }
            LowLevel::NumCos => {
                let intrinsic = match arg_layouts[0] {
                    Layout::F64 => &bitcode::NUM_COS[FloatWidth::F64],
                    Layout::F32 => &bitcode::NUM_COS[FloatWidth::F32],
                    Layout::DEC => bitcode::DEC_COS,
                    _ => unreachable!("invalid layout for cos"),
                };

                self.build_fn_call(sym, intrinsic.to_string(), args, arg_layouts, ret_layout)
            }
            LowLevel::NumTan => {
                let intrinsic = match arg_layouts[0] {
                    Layout::F64 => &bitcode::NUM_TAN[FloatWidth::F64],
                    Layout::F32 => &bitcode::NUM_TAN[FloatWidth::F32],
                    Layout::DEC => bitcode::DEC_TAN,
                    _ => unreachable!("invalid layout for tan"),
                };

                self.build_fn_call(sym, intrinsic.to_string(), args, arg_layouts, ret_layout)
            }
            LowLevel::ListLenU64 => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "ListLenU64: expected to have exactly one argument"
                );
                self.build_list_len_u64(sym, &args[0])
            }
            LowLevel::ListLenUsize => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "ListLenUsize: expected to have exactly one argument"
                );
                self.build_list_len_usize(sym, &args[0])
            }
            LowLevel::ListWithCapacity => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "ListWithCapacity: expected to have exactly one argument"
                );
                let elem_layout = list_element_layout!(self.interner(), *ret_layout);
                self.build_list_with_capacity(sym, args[0], arg_layouts[0], elem_layout, ret_layout)
            }
            LowLevel::ListClone => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "ListClone: expected to have exactly one argument"
                );
                let elem_layout = list_element_layout!(self.interner(), *ret_layout);
                self.build_list_clone(*sym, args[0], elem_layout, *ret_layout)
            }
            LowLevel::ListReserve => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "ListReserve: expected to have exactly two arguments"
                );
                self.build_list_reserve(sym, args, arg_layouts, ret_layout)
            }
            LowLevel::ListAppendUnsafe => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "ListAppendUnsafe: expected to have exactly two arguments"
                );
                self.build_list_append_unsafe(sym, args, arg_layouts, ret_layout)
            }
            LowLevel::ListGetUnsafe => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "ListGetUnsafe: expected to have exactly two arguments"
                );
                self.build_list_get_unsafe(sym, &args[0], &args[1], ret_layout)
            }
            LowLevel::ListReplaceUnsafe => {
                debug_assert_eq!(
                    3,
                    args.len(),
                    "ListReplaceUnsafe: expected to have exactly three arguments"
                );
                self.build_list_replace_unsafe(sym, args, arg_layouts, ret_layout)
            }
            LowLevel::ListConcat => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "ListConcat: expected to have exactly two arguments"
                );
                let elem_layout = list_element_layout!(self.interner(), *ret_layout);
                self.build_list_concat(sym, args, arg_layouts, elem_layout, ret_layout)
            }
            LowLevel::ListPrepend => {
                debug_assert_eq!(
                    2,
                    args.len(),
                    "ListPrepend: expected to have exactly two arguments"
                );
                self.build_list_prepend(sym, args, arg_layouts, ret_layout)
            }
            LowLevel::StrConcat => self.build_fn_call(
                sym,
                bitcode::STR_CONCAT.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrJoinWith => self.build_fn_call(
                sym,
                bitcode::STR_JOIN_WITH.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrSplit => self.build_fn_call(
                sym,
                bitcode::STR_SPLIT.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrStartsWith => self.build_fn_call(
                sym,
                bitcode::STR_STARTS_WITH.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrEndsWith => self.build_fn_call(
                sym,
                bitcode::STR_ENDS_WITH.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrSubstringUnsafe => self.build_fn_call(
                sym,
                bitcode::STR_SUBSTRING_UNSAFE.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrToUtf8 => self.build_fn_call(
                sym,
                bitcode::STR_TO_UTF8.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrCountUtf8Bytes => self.build_fn_call(
                sym,
                bitcode::STR_COUNT_UTF8_BYTES.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrFromUtf8 => {
                let update_mode = self.debug_symbol("update_mode");

                // In dev builds, always use UpdateMode::Immutable
                self.load_literal_i8(&update_mode, UpdateMode::Immutable as i8);

                self.build_fn_call(
                    sym,
                    bitcode::STR_FROM_UTF8.to_string(),
                    &[args[0], update_mode],
                    &[arg_layouts[0], Layout::U8],
                    ret_layout,
                )
            }
            LowLevel::StrRepeat => self.build_fn_call(
                sym,
                bitcode::STR_REPEAT.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrTrim => self.build_fn_call(
                sym,
                bitcode::STR_TRIM.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrTrimStart => self.build_fn_call(
                sym,
                bitcode::STR_TRIM_START.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrTrimEnd => self.build_fn_call(
                sym,
                bitcode::STR_TRIM_END.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrReserve => self.build_fn_call(
                sym,
                bitcode::STR_RESERVE.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrWithCapacity => self.build_fn_call(
                sym,
                bitcode::STR_WITH_CAPACITY.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrGetUnsafe => self.build_fn_call(
                sym,
                bitcode::STR_GET_UNSAFE.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::StrToNum => {
                let number_layout = match self.interner().get_repr(*ret_layout) {
                    LayoutRepr::Struct(field_layouts) => field_layouts[0], // TODO: why is it sometimes a struct?
                    _ => unreachable!(),
                };

                // match on the return layout to figure out which zig builtin we need
                let intrinsic = match self.interner().get_repr(number_layout) {
                    LayoutRepr::Builtin(Builtin::Int(int_width)) => &bitcode::STR_TO_INT[int_width],
                    LayoutRepr::Builtin(Builtin::Float(float_width)) => {
                        &bitcode::STR_TO_FLOAT[float_width]
                    }
                    LayoutRepr::Builtin(Builtin::Decimal) => bitcode::DEC_FROM_STR,
                    _ => unreachable!(),
                };

                self.build_fn_call(sym, intrinsic.to_string(), args, arg_layouts, ret_layout)
            }
            LowLevel::ListConcatUtf8 => self.build_fn_call(
                sym,
                bitcode::LIST_CONCAT_UTF8.to_string(),
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

                debug_assert_eq!(
                    self.interner().stack_size_and_alignment(arg_layouts[0]),
                    (8, 8),
                    "cannot pointer cast from source: {}",
                    self.interner().dbg(arg_layouts[0])
                );

                debug_assert_eq!(
                    self.interner().stack_size_and_alignment(*ret_layout),
                    (8, 8),
                    "cannot pointer cast to target: {}",
                    self.interner().dbg(*ret_layout)
                );

                self.build_ptr_cast(sym, &args[0])
            }
            LowLevel::PtrStore => {
                let args0 = args[0];
                let args1 = args[1];

                let element_layout = match self.interner().get_repr(arg_layouts[0]) {
                    LayoutRepr::Ptr(inner) => inner,
                    _ => unreachable!(
                        "cannot write to {:?} in *{args0:?} = {args1:?}",
                        self.interner().dbg(arg_layouts[0])
                    ),
                };

                self.build_ptr_store(*sym, args0, args1, element_layout);
            }
            LowLevel::PtrLoad => {
                self.build_ptr_load(*sym, args[0], *ret_layout);
            }

            LowLevel::PtrClearTagId => {
                self.build_ptr_clear_tag_id(*sym, args[0]);
            }

            LowLevel::RefCountDecRcPtr => self.build_fn_call(
                sym,
                bitcode::UTILS_DECREF_RC_PTR.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::RefCountIncRcPtr => self.build_fn_call(
                sym,
                bitcode::UTILS_INCREF_RC_PTR.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::RefCountDecDataPtr => self.build_fn_call(
                sym,
                bitcode::UTILS_DECREF_DATA_PTR.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::RefCountIncDataPtr => self.build_fn_call(
                sym,
                bitcode::UTILS_INCREF_DATA_PTR.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::RefCountIsUnique => self.build_fn_call(
                sym,
                bitcode::UTILS_IS_UNIQUE.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::SetJmp => self.build_fn_call(
                sym,
                String::from("roc_setjmp"),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::LongJmp => self.build_fn_call(
                sym,
                String::from("roc_longjmp"),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::SetLongJmpBuffer => {
                self.build_data_pointer(sym, String::from("setlongjmp_buffer"));
            }
            LowLevel::DictPseudoSeed => self.build_fn_call(
                sym,
                bitcode::UTILS_DICT_PSEUDO_SEED.to_string(),
                args,
                arg_layouts,
                ret_layout,
            ),
            LowLevel::NumToStr => {
                let arg_layout = arg_layouts[0];
                let intrinsic = match self.interner().get_repr(arg_layout) {
                    LayoutRepr::Builtin(Builtin::Int(width)) => &bitcode::STR_FROM_INT[width],
                    LayoutRepr::Builtin(Builtin::Float(width)) => &bitcode::STR_FROM_FLOAT[width],
                    LayoutRepr::Builtin(Builtin::Decimal) => bitcode::DEC_TO_STR,
                    x => internal_error!("NumToStr is not defined for {:?}", x),
                };

                self.build_fn_call(sym, intrinsic.to_string(), args, arg_layouts, ret_layout)
            }
            LowLevel::StrIsEmpty => {
                let intrinsic = bitcode::STR_IS_EMPTY.to_string();
                self.build_fn_call(sym, intrinsic, args, arg_layouts, ret_layout);
            }
            LowLevel::NumIntCast => {
                let source_width = match self.interner().get_repr(arg_layouts[0]) {
                    LayoutRepr::Builtin(Builtin::Int(width)) => width,
                    _ => unreachable!(),
                };

                let target_width = match self.interner().get_repr(*ret_layout) {
                    LayoutRepr::Builtin(Builtin::Int(width)) => width,
                    _ => unreachable!(),
                };

                self.build_num_int_cast(sym, &args[0], source_width, target_width)
            }
            LowLevel::NumIsMultipleOf => {
                let int_width = arg_layouts[0].try_int_width().unwrap();
                let intrinsic = bitcode::NUM_IS_MULTIPLE_OF[int_width].to_string();
                self.build_fn_call(sym, intrinsic, args, arg_layouts, ret_layout);
            }
            LowLevel::NumCountLeadingZeroBits => {
                let int_width = arg_layouts[0].try_int_width().unwrap();
                let intrinsic = bitcode::NUM_COUNT_LEADING_ZERO_BITS[int_width].to_string();
                self.build_fn_call(sym, intrinsic, args, arg_layouts, ret_layout);
            }
            LowLevel::NumCountTrailingZeroBits => {
                let int_width = arg_layouts[0].try_int_width().unwrap();
                let intrinsic = bitcode::NUM_COUNT_TRAILING_ZERO_BITS[int_width].to_string();
                self.build_fn_call(sym, intrinsic, args, arg_layouts, ret_layout);
            }
            LowLevel::NumCountOneBits => {
                let int_width = arg_layouts[0].try_int_width().unwrap();
                let intrinsic = bitcode::NUM_COUNT_ONE_BITS[int_width].to_string();
                self.build_fn_call(sym, intrinsic, args, arg_layouts, ret_layout);
            }
            LowLevel::ListSublist => {
                //    list: RocList,
                //    alignment: u32,
                //    element_width: usize,
                //    start: u64,
                //    len: u64,
                //    dec: Dec,

                let list = args[0];
                let start = args[1];
                let len = args[2];

                let list_layout = arg_layouts[0];
                let list_argument = self.list_argument(list_layout);
                let element_layout = list_argument.element_layout;

                let args = [
                    list,
                    list_argument.alignment,
                    list_argument.element_width,
                    list_argument.element_refcounted,
                    start,
                    len,
                    self.decrement_fn_pointer(element_layout),
                ];

                let layout_usize = Layout::U64;

                let arg_layouts = [
                    arg_layouts[0],
                    Layout::U32,
                    layout_usize,
                    Layout::BOOL,
                    Layout::U64,
                    Layout::U64,
                    layout_usize,
                ];

                let intrinsic = bitcode::LIST_SUBLIST.to_string();
                self.build_fn_call(sym, intrinsic, &args, &arg_layouts, &list_layout);
            }
            LowLevel::ListSwap => {
                let list = args[0];
                let i = args[1];
                let j = args[2];

                let list_layout = arg_layouts[0];
                let list_argument = self.list_argument(list_layout);

                let update_mode = self.debug_symbol("update_mode");
                self.load_literal_i8(&update_mode, UpdateMode::Immutable as i8);

                let inc_fn_ptr = self.increment_fn_pointer(list_argument.element_layout);
                let dec_fn_ptr = self.decrement_fn_pointer(list_argument.element_layout);
                let copy_fn_ptr = self.copy_fn_pointer(list_argument.element_layout);

                let layout_usize = Layout::U64;

                //    list: RocList,
                //    alignment: u32,
                //    element_width: usize,
                //    index_1: u64,
                //    index_2: u64,
                //    element_refcounted: bool,
                //    inc: Inc
                //    dec: Dec
                //    update_mode: UpdateMode,
                //    copy: CopyFn

                self.build_fn_call(
                    sym,
                    bitcode::LIST_SWAP.to_string(),
                    &[
                        list,
                        list_argument.alignment,
                        list_argument.element_width,
                        i,
                        j,
                        list_argument.element_refcounted,
                        inc_fn_ptr,
                        dec_fn_ptr,
                        update_mode,
                        copy_fn_ptr,
                    ],
                    &[
                        list_layout,
                        Layout::U32,
                        layout_usize,
                        Layout::U64,
                        Layout::U64,
                        Layout::BOOL,
                        layout_usize,
                        layout_usize,
                        Layout::U8,
                        layout_usize,
                    ],
                    ret_layout,
                );
            }
            LowLevel::ListReleaseExcessCapacity => {
                let list = args[0];

                let list_layout = arg_layouts[0];
                let list_argument = self.list_argument(list_layout);

                let update_mode = self.debug_symbol("update_mode");
                self.load_literal_i8(&update_mode, UpdateMode::Immutable as i8);

                let inc_fn_ptr = self.increment_fn_pointer(list_argument.element_layout);
                let dec_fn_ptr = self.decrement_fn_pointer(list_argument.element_layout);

                let layout_usize = Layout::U64;

                //    list: RocList,
                //    alignment: u32,
                //    element_width: usize,
                //    element_refcounted: bool,
                //    inc_fn_ptr: Inc,
                //    dec_fn_ptr: Dec,
                //    update_mode: UpdateMode,

                self.build_fn_call(
                    sym,
                    bitcode::LIST_RELEASE_EXCESS_CAPACITY.to_string(),
                    &[
                        list,
                        list_argument.alignment,
                        list_argument.element_width,
                        list_argument.element_refcounted,
                        inc_fn_ptr,
                        dec_fn_ptr,
                        update_mode,
                    ],
                    &[
                        list_layout,
                        Layout::U32,
                        layout_usize,
                        Layout::BOOL,
                        layout_usize,
                        layout_usize,
                        Layout::U8,
                    ],
                    ret_layout,
                );
            }

            LowLevel::ListIncref => {
                let list = args[0];

                let list_layout = arg_layouts[0];
                let list_argument = self.list_argument(list_layout);

                let layout_isize = Layout::I64;

                let amount = if args.len() == 2 {
                    // amount explicitly specified,
                    args[1]
                } else {
                    // amount implicit 1.
                    let sym = self.debug_symbol("amount");
                    self.load_literal_i64(&sym, 1);
                    sym
                };

                //    list: RocList,
                //    amount: isize,
                //    element_refcounted: bool,

                self.build_fn_call(
                    sym,
                    bitcode::LIST_INCREF.to_string(),
                    &[list, amount, list_argument.element_refcounted],
                    &[list_layout, layout_isize, Layout::BOOL],
                    ret_layout,
                );
            }

            LowLevel::ListDecref => {
                let list = args[0];

                let list_layout = arg_layouts[0];
                let list_argument = self.list_argument(list_layout);

                let dec_fn_ptr = self.decrement_fn_pointer(list_argument.element_layout);

                let layout_usize = Layout::U64;

                //    list: RocList,
                //    alignment: u32,
                //    element_width: usize,
                //    element_refcounted: bool,
                //    dec_fn_ptr: Dec,

                self.build_fn_call(
                    sym,
                    bitcode::LIST_DECREF.to_string(),
                    &[
                        list,
                        list_argument.alignment,
                        list_argument.element_width,
                        list_argument.element_refcounted,
                        dec_fn_ptr,
                    ],
                    &[
                        list_layout,
                        Layout::U32,
                        layout_usize,
                        Layout::BOOL,
                        layout_usize,
                    ],
                    ret_layout,
                );
            }

            LowLevel::ListDropAt => {
                let list = args[0];
                let drop_index = args[1];

                let list_layout = arg_layouts[0];
                let list_argument = self.list_argument(list_layout);
                let element_layout = list_argument.element_layout;

                let update_mode = self.debug_symbol("update_mode");
                self.load_literal_i8(&update_mode, UpdateMode::Immutable as i8);

                let layout_usize = Layout::U64;
                let element_increment = self.increment_fn_pointer(element_layout);
                let element_decrement = self.decrement_fn_pointer(element_layout);

                //    list: RocList,
                //    alignment: u32,
                //    element_width: usize,
                //    element_refcounted: bool,
                //    drop_index: u64,
                //    inc: Inc,
                //    dec: Dec,

                self.build_fn_call(
                    sym,
                    bitcode::LIST_DROP_AT.to_string(),
                    &[
                        list,
                        list_argument.alignment,
                        list_argument.element_width,
                        list_argument.element_refcounted,
                        drop_index,
                        element_increment,
                        element_decrement,
                    ],
                    &[
                        list_layout,
                        Layout::U32,
                        layout_usize,
                        Layout::BOOL,
                        Layout::U64,
                        layout_usize,
                        layout_usize,
                    ],
                    ret_layout,
                );
            }

            LowLevel::NumCompare => {
                self.build_num_cmp(sym, &args[0], &args[1], &arg_layouts[0]);
            }

            LowLevel::NumToFloatCast => {
                let float_width = match *ret_layout {
                    Layout::F64 => FloatWidth::F64,
                    Layout::F32 => FloatWidth::F32,
                    _ => unreachable!("invalid return layout for NumToFloatCast"),
                };

                match arg_layouts[0].try_to_int_width() {
                    Some(int_width) => {
                        self.build_int_to_float_cast(sym, &args[0], int_width, float_width);
                    }
                    None => {
                        self.build_num_to_frac(sym, &args[0], &arg_layouts[0], ret_layout);
                    }
                }
            }

            LowLevel::NumWithoutDecimalPoint => {
                let intrinsic = bitcode::DEC_TO_I128.to_string();
                self.build_fn_call(sym, intrinsic, args, arg_layouts, ret_layout)
            }

            LowLevel::NumWithDecimalPoint => {
                let intrinsic = bitcode::DEC_FROM_I128.to_string();
                self.build_fn_call(sym, intrinsic, args, arg_layouts, ret_layout)
            }

            LowLevel::NumF32ToParts => {
                let intrinsic = bitcode::NUM_F32_TO_PARTS.to_string();
                self.build_fn_call(sym, intrinsic, args, arg_layouts, ret_layout)
            }

            LowLevel::NumF64ToParts => {
                let intrinsic = bitcode::NUM_F64_TO_PARTS.to_string();
                self.build_fn_call(sym, intrinsic, args, arg_layouts, ret_layout)
            }

            LowLevel::NumF32FromParts => {
                let intrinsic = bitcode::NUM_F32_FROM_PARTS.to_string();
                self.build_fn_call(sym, intrinsic, args, arg_layouts, ret_layout)
            }

            LowLevel::NumF64FromParts => {
                let intrinsic = bitcode::NUM_F64_FROM_PARTS.to_string();
                self.build_fn_call(sym, intrinsic, args, arg_layouts, ret_layout)
            }

            x => todo!("low level, {:?}", x),
        }
    }

    /// Builds a builtin functions that do not map directly to a low level
    /// If the builtin is simple enough, it will be inlined.
    fn build_builtin(
        &mut self,
        sym: &Symbol,
        func_name: LambdaName,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    ) {
        match func_name.name() {
            Symbol::NUM_IS_ZERO => {
                debug_assert_eq!(
                    1,
                    args.len(),
                    "NumIsZero: expected to have exactly one argument"
                );
                debug_assert!(
                    self.interner().eq_repr(Layout::BOOL, *ret_layout,),
                    "NumIsZero: expected to have return layout of type Bool"
                );

                let literal = match self.interner().get_repr(arg_layouts[0]) {
                    LayoutRepr::Builtin(Builtin::Int(_)) => Literal::Int(0i128.to_ne_bytes()),
                    LayoutRepr::Builtin(Builtin::Float(_)) => Literal::Float(0.0),
                    LayoutRepr::DEC => Literal::Decimal([0; 16]),
                    _ => unreachable!("invalid layout for sin"),
                };

                self.load_literal_symbols(args);
                self.load_literal(&Symbol::DEV_TMP, &arg_layouts[0], &literal);
                self.build_eq(sym, &args[0], &Symbol::DEV_TMP, &arg_layouts[0]);
                self.free_symbol(&Symbol::DEV_TMP)
            }
            Symbol::LIST_GET
            | Symbol::LIST_SET
            | Symbol::LIST_REPLACE
            | Symbol::LIST_APPEND
            | Symbol::LIST_APPEND_IF_OK
            | Symbol::LIST_PREPEND_IF_OK => {
                // TODO: This is probably simple enough to be worth inlining.
                let fn_name = self.lambda_name_to_string(
                    func_name,
                    arg_layouts.iter().copied(),
                    None,
                    *ret_layout,
                );
                // Now that the arguments are needed, load them if they are literals.
                self.load_literal_symbols(args);
                self.build_fn_call(sym, fn_name, args, arg_layouts, ret_layout)
            }
            Symbol::BOOL_TRUE => {
                const LITERAL: &Literal<'static> = &Literal::Bool(true);
                const BOOL_LAYOUT: &InLayout<'static> = &Layout::BOOL;

                if self.env().lazy_literals {
                    self.literal_map().insert(*sym, (LITERAL, BOOL_LAYOUT));
                } else {
                    self.load_literal(sym, BOOL_LAYOUT, LITERAL);
                }
            }
            Symbol::BOOL_FALSE => {
                const LITERAL: &Literal<'static> = &Literal::Bool(false);
                const BOOL_LAYOUT: &InLayout<'static> = &Layout::BOOL;

                if self.env().lazy_literals {
                    self.literal_map().insert(*sym, (LITERAL, BOOL_LAYOUT));
                } else {
                    self.load_literal(sym, BOOL_LAYOUT, LITERAL);
                }
            }
            _other => {
                // just call the function
                let fn_name = self.lambda_name_to_string(
                    func_name,
                    arg_layouts.iter().copied(),
                    None,
                    *ret_layout,
                );
                // Now that the arguments are needed, load them if they are literals.
                self.load_literal_symbols(args);
                self.build_fn_call(sym, fn_name, args, arg_layouts, ret_layout)
            }
        }
    }

    /// build_fn_call creates a call site for a function.
    /// This includes dealing with things like saving regs and propagating the returned value.
    fn build_fn_call(
        &mut self,
        dst: &Symbol,
        fn_name: String,
        args: &[Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    );

    fn build_fn_pointer(&mut self, dst: &Symbol, fn_name: String);
    fn build_data_pointer(&mut self, dst: &Symbol, data_name: String);

    /// Move a returned value into `dst`
    fn move_return_value(&mut self, dst: &Symbol, ret_layout: &InLayout<'a>);

    /// build_num_abs stores the absolute value of src into dst.
    fn build_num_int_cast(
        &mut self,
        dst: &Symbol,
        src: &Symbol,
        source: IntWidth,
        target: IntWidth,
    );

    fn build_int_to_float_cast(
        &mut self,
        dst: &Symbol,
        src: &Symbol,
        int_width: IntWidth,
        float_width: FloatWidth,
    );

    /// build_num_abs stores the absolute value of src into dst.
    fn build_num_abs(&mut self, dst: &Symbol, src: &Symbol, layout: &InLayout<'a>);

    /// build_num_add stores the sum of src1 and src2 into dst.
    fn build_num_add(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &InLayout<'a>);

    /// build_num_add_saturated stores the sum of src1 and src2 into dst.
    fn build_num_add_saturated(
        &mut self,
        dst: Symbol,
        src1: Symbol,
        src2: Symbol,
        layout: InLayout<'a>,
    );

    /// build_num_add_checked stores the sum of src1 and src2 into dst.
    fn build_num_add_checked(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        num_layout: &InLayout<'a>,
        return_layout: &InLayout<'a>,
    );

    fn build_num_add_wrap(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        layout: &InLayout<'a>,
    );

    /// build_num_sub_checked stores the sum of src1 and src2 into dst.
    fn build_num_sub_checked(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        num_layout: &InLayout<'a>,
        return_layout: &InLayout<'a>,
    );

    /// build_num_mul stores `src1 * src2` into dst.
    fn build_num_mul(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &InLayout<'a>);

    /// build_num_mul_wrap stores `src1 * src2` into dst.
    fn build_num_mul_wrap(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        layout: &InLayout<'a>,
    );

    fn build_num_mul_saturated(
        &mut self,
        dst: Symbol,
        src1: Symbol,
        src2: Symbol,
        layout: InLayout<'a>,
    );

    fn build_num_mul_checked(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        num_layout: &InLayout<'a>,
        return_layout: &InLayout<'a>,
    );

    /// build_num_mul stores `src1 / src2` into dst.
    fn build_num_div(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &InLayout<'a>);

    fn build_num_div_ceil(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        layout: &InLayout<'a>,
    );

    /// build_num_mul stores `src1 % src2` into dst.
    fn build_num_rem(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &InLayout<'a>);

    /// build_num_neg stores the negated value of src into dst.
    fn build_num_neg(&mut self, dst: &Symbol, src: &Symbol, layout: &InLayout<'a>);

    /// build_num_sub stores the `src1 - src2` difference into dst.
    fn build_num_sub(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, layout: &InLayout<'a>);

    /// build_num_sub_wrap stores the `src1 - src2` difference into dst.
    fn build_num_sub_wrap(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        layout: &InLayout<'a>,
    );

    /// stores the `src1 & src2` into dst.
    fn build_int_bitwise_and(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    );

    /// stores the `src1 | src2` into dst.
    fn build_int_bitwise_or(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    );

    /// stores the `src1 ^ src2` into dst.
    fn build_int_bitwise_xor(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    );

    /// stores the `Num.shiftLeftBy src1 src2` into dst.
    fn build_int_shift_left(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    );

    /// stores the `Num.shiftRightBy src1 src2` into dst.
    fn build_int_shift_right(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    );

    /// stores the `Num.shiftRightZfBy src1 src2` into dst.
    fn build_int_shift_right_zero_fill(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        int_width: IntWidth,
    );

    /// build_eq stores the result of `src1 == src2` into dst.
    fn build_eq(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, arg_layout: &InLayout<'a>);

    /// build_neq stores the result of `src1 != src2` into dst.
    fn build_neq(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol, arg_layout: &InLayout<'a>);

    /// build_not stores the result of `!src` into dst.
    fn build_not(&mut self, dst: &Symbol, src: &Symbol, arg_layout: &InLayout<'a>);

    fn build_num_cmp(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &InLayout<'a>,
    );

    /// build_num_lt stores the result of `src1 < src2` into dst.
    fn build_num_lt(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &InLayout<'a>,
    );

    /// build_num_gt stores the result of `src1 > src2` into dst.
    fn build_num_gt(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &InLayout<'a>,
    );

    /// build_num_to_frac convert Number to Frac
    fn build_num_to_frac(
        &mut self,
        dst: &Symbol,
        src: &Symbol,
        arg_layout: &InLayout<'a>,
        ret_layout: &InLayout<'a>,
    );

    /// build_num_is_nan check is a Frac is NaN
    fn build_num_is_nan(&mut self, dst: &Symbol, src: &Symbol, arg_layout: &InLayout<'a>);

    /// build_num_is_infinite check is a Frac is infinite
    fn build_num_is_infinite(&mut self, dst: &Symbol, src: &Symbol, arg_layout: &InLayout<'a>);

    /// build_num_is_finite check is a Frac is finite
    fn build_num_is_finite(&mut self, dst: &Symbol, src: &Symbol, arg_layout: &InLayout<'a>);

    /// build_num_lte stores the result of `src1 <= src2` into dst.
    fn build_num_lte(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &InLayout<'a>,
    );

    /// build_num_gte stores the result of `src1 >= src2` into dst.
    fn build_num_gte(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
        arg_layout: &InLayout<'a>,
    );

    /// build_sqrt stores the result of `sqrt(src)` into dst.
    fn build_num_sqrt(&mut self, dst: Symbol, src: Symbol, float_width: FloatWidth);

    /// build_list_len_usize returns the length of a list as a usize. This is for internal use only.
    fn build_list_len_usize(&mut self, dst: &Symbol, list: &Symbol);

    /// build_list_len_u64 returns the length of a list and casts it from usize to u64. This is for the public List.len.
    fn build_list_len_u64(&mut self, dst: &Symbol, list: &Symbol);

    /// generate a call to a higher-order lowlevel
    fn build_higher_order_lowlevel(
        &mut self,
        dst: &Symbol,
        holl: &HigherOrderLowLevel<'a>,
        ret_layout: InLayout<'a>,
    );

    fn build_indirect_inc(&mut self, layout: InLayout<'a>) -> Symbol;
    fn build_indirect_inc_n(&mut self, layout: InLayout<'a>) -> Symbol;
    fn build_indirect_dec(&mut self, layout: InLayout<'a>) -> Symbol;
    fn build_indirect_copy(&mut self, layout: InLayout<'a>) -> Symbol;

    fn build_list_clone(
        &mut self,
        dst: Symbol,
        input_list: Symbol,
        elem_layout: InLayout<'a>,
        ret_layout: InLayout<'a>,
    );

    /// build_list_with_capacity creates and returns a list with the given capacity.
    fn build_list_with_capacity(
        &mut self,
        dst: &Symbol,
        capacity: Symbol,
        capacity_layout: InLayout<'a>,
        elem_layout: InLayout<'a>,
        ret_layout: &InLayout<'a>,
    );

    /// build_list_reserve enlarges a list to at least accommodate the given capacity.
    fn build_list_reserve(
        &mut self,
        dst: &Symbol,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    );

    /// build_list_append_unsafe returns a new list with a given element appended.
    fn build_list_append_unsafe(
        &mut self,
        dst: &Symbol,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    );

    /// build_list_get_unsafe loads the element from the list at the index.
    fn build_list_get_unsafe(
        &mut self,
        dst: &Symbol,
        list: &Symbol,
        index: &Symbol,
        ret_layout: &InLayout<'a>,
    );

    /// build_list_replace_unsafe returns the old element and new list with the list having the new element inserted.
    fn build_list_replace_unsafe(
        &mut self,
        dst: &Symbol,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    );

    /// build_list_concat returns a new list containing the two argument lists concatenated.
    fn build_list_concat(
        &mut self,
        dst: &Symbol,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        elem_layout: InLayout<'a>,
        ret_layout: &InLayout<'a>,
    );

    /// build_list_prepend returns a new list with a given element prepended.
    fn build_list_prepend(
        &mut self,
        dst: &Symbol,
        args: &'a [Symbol],
        arg_layouts: &[InLayout<'a>],
        ret_layout: &InLayout<'a>,
    );

    /// build_refcount_getptr loads the pointer to the reference count of src into dst.
    fn build_ptr_cast(&mut self, dst: &Symbol, src: &Symbol);

    fn build_ptr_store(
        &mut self,
        sym: Symbol,
        ptr: Symbol,
        value: Symbol,
        element_layout: InLayout<'a>,
    );

    fn build_ptr_load(&mut self, sym: Symbol, ptr: Symbol, element_layout: InLayout<'a>);

    fn build_ptr_clear_tag_id(&mut self, sym: Symbol, ptr: Symbol);

    fn build_alloca(&mut self, sym: Symbol, value: Option<Symbol>, element_layout: InLayout<'a>);

    /// literal_map gets the map from symbol to literal and layout, used for lazy loading and literal folding.
    fn literal_map(&mut self) -> &mut MutMap<Symbol, (*const Literal<'a>, *const InLayout<'a>)>;

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
    fn load_literal(&mut self, sym: &Symbol, layout: &InLayout<'a>, lit: &Literal<'a>);

    fn load_literal_i64(&mut self, sym: &Symbol, value: i64) {
        let literal = Literal::Int((value as i128).to_ne_bytes());

        self.load_literal(sym, &Layout::I64, &literal)
    }

    fn load_literal_i32(&mut self, sym: &Symbol, value: i32) {
        let literal = Literal::Int((value as i128).to_ne_bytes());

        self.load_literal(sym, &Layout::I32, &literal)
    }

    fn load_literal_i16(&mut self, sym: &Symbol, value: i16) {
        let literal = Literal::Int((value as i128).to_ne_bytes());

        self.load_literal(sym, &Layout::I16, &literal)
    }

    fn load_literal_i8(&mut self, sym: &Symbol, value: i8) {
        let literal = Literal::Int((value as i128).to_ne_bytes());

        self.load_literal(sym, &Layout::I8, &literal)
    }

    /// create_empty_array creates an empty array with nullptr, zero length, and zero capacity.
    fn create_empty_array(&mut self, sym: &Symbol);

    /// create_array creates an array filling it with the specified objects.
    fn create_array(
        &mut self,
        sym: &Symbol,
        elem_layout: &InLayout<'a>,
        elems: &'a [ListLiteralElement<'a>],
    );

    /// create_struct creates a struct with the elements specified loaded into it as data.
    fn create_struct(&mut self, sym: &Symbol, layout: &InLayout<'a>, fields: &'a [Symbol]);

    /// load_struct_at_index loads into `sym` the value at `index` in `structure`.
    fn load_struct_at_index(
        &mut self,
        sym: &Symbol,
        structure: &Symbol,
        index: u64,
        field_layouts: &'a [InLayout<'a>],
    );

    /// load_union_at_index loads into `sym` the value at `index` for `tag_id`.
    fn load_union_at_index(
        &mut self,
        sym: &Symbol,
        structure: &Symbol,
        tag_id: TagIdIntType,
        index: u64,
        union_layout: &UnionLayout<'a>,
    );

    /// load_union_at_index loads into `sym` the value at `index` for `tag_id`.
    fn load_union_field_ptr_at_index(
        &mut self,
        sym: &Symbol,
        structure: &Symbol,
        tag_id: TagIdIntType,
        index: u64,
        union_layout: &UnionLayout<'a>,
    );

    /// get_tag_id loads the tag id from a the union.
    fn get_tag_id(&mut self, sym: &Symbol, structure: &Symbol, union_layout: &UnionLayout<'a>);

    /// tag sets the tag for a union.
    fn tag(
        &mut self,
        sym: &Symbol,
        args: &'a [Symbol],
        tag_layout: &UnionLayout<'a>,
        tag_id: TagIdIntType,
        reuse: Option<Symbol>,
    );

    /// load a value from a pointer
    fn expr_unbox(&mut self, sym: Symbol, ptr: Symbol, element_layout: InLayout<'a>);

    /// store a refcounted value on the heap
    fn expr_box(
        &mut self,
        sym: Symbol,
        value: Symbol,
        element_layout: InLayout<'a>,
        reuse: Option<Symbol>,
    );

    /// return_symbol moves a symbol to the correct return location for the backend and adds a jump to the end of the function.
    fn return_symbol(&mut self, sym: &Symbol, layout: &InLayout<'a>);

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

    /// last_seen_map gets the map from symbol to when it is last seen in the function.
    fn last_seen_map(&mut self) -> &mut MutMap<Symbol, *const Stmt<'a>>;

    /// set_layout_map sets the layout for a specific symbol.
    fn set_layout_map(&mut self, sym: Symbol, layout: &InLayout<'a>) {
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
    fn layout_map(&mut self) -> &mut MutMap<Symbol, InLayout<'a>>;

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
    fn scan_ast(&mut self, stmt: &'a Stmt<'a>) {
        *self.last_seen_map() = LastSeenMap::scan_ast(stmt);
    }
}
