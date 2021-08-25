#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant, clippy::upper_case_acronyms)]

use bumpalo::{collections::Vec, Bump};
use roc_builtins::bitcode;
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::{ModuleName, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::ir::{BranchInfo, CallType, Expr, JoinPointId, Literal, Proc, Stmt};
use roc_mono::layout::{Builtin, Layout, LayoutIds};
use target_lexicon::Triple;

mod generic64;
mod object_builder;
pub use object_builder::build_module;
mod run_roc;

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub interns: Interns,
    pub exposed_to_host: MutSet<Symbol>,
    pub lazy_literals: bool,
    pub generate_allocators: bool,
}

// These relocations likely will need a length.
// They may even need more definition, but this should be at least good enough for how we will use elf.
#[derive(Debug)]
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
}

trait Backend<'a>
where
    Self: Sized,
{
    /// new creates a new backend that will output to the specific Object.
    fn new(env: &'a Env, target: &Triple) -> Result<Self, String>;

    fn env(&self) -> &'a Env<'a>;

    /// reset resets any registers or other values that may be occupied at the end of a procedure.
    fn reset(&mut self);

    /// finalize does any setup and cleanup that should happen around the procedure.
    /// finalize does setup because things like stack size and jump locations are not know until the function is written.
    /// For example, this can store the frame pionter and setup stack space.
    /// finalize is run at the end of build_proc when all internal code is finalized.
    fn finalize(&mut self) -> Result<(&'a [u8], &[Relocation]), String>;

    // load_args is used to let the backend know what the args are.
    // The backend should track these args so it can use them as needed.
    fn load_args(
        &mut self,
        args: &'a [(Layout<'a>, Symbol)],
        ret_layout: &Layout<'a>,
    ) -> Result<(), String>;

    /// Used for generating wrappers for malloc/realloc/free
    fn build_wrapped_jmp(&mut self) -> Result<(&'a [u8], u64), String>;

    /// build_proc creates a procedure and outputs it to the wrapped object writer.
    fn build_proc(&mut self, proc: Proc<'a>) -> Result<(&'a [u8], &[Relocation]), String> {
        self.reset();
        self.load_args(proc.args, &proc.ret_layout)?;
        // let start = std::time::Instant::now();
        self.scan_ast(&proc.body);
        self.create_free_map();
        // let duration = start.elapsed();
        // println!("Time to calculate lifetimes: {:?}", duration);
        // println!("{:?}", self.last_seen_map());
        self.build_stmt(&proc.body, &proc.ret_layout)?;
        self.finalize()
    }

    /// build_stmt builds a statement and outputs at the end of the buffer.
    fn build_stmt(&mut self, stmt: &Stmt<'a>, ret_layout: &Layout<'a>) -> Result<(), String> {
        match stmt {
            Stmt::Let(sym, expr, layout, following) => {
                self.build_expr(sym, expr, layout)?;
                self.free_symbols(stmt);
                self.build_stmt(following, ret_layout)?;
                Ok(())
            }
            Stmt::Ret(sym) => {
                self.load_literal_symbols(&[*sym])?;
                self.return_symbol(sym, ret_layout)?;
                self.free_symbols(stmt);
                Ok(())
            }
            Stmt::Switch {
                cond_symbol,
                cond_layout,
                branches,
                default_branch,
                ret_layout,
            } => {
                self.load_literal_symbols(&[*cond_symbol])?;
                self.build_switch(
                    cond_symbol,
                    cond_layout,
                    branches,
                    default_branch,
                    ret_layout,
                )?;
                self.free_symbols(stmt);
                Ok(())
            }
            x => Err(format!("the statement, {:?}, is not yet implemented", x)),
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
    ) -> Result<(), String>;

    /// build_expr builds the expressions for the specified symbol.
    /// The builder must keep track of the symbol because it may be referred to later.
    fn build_expr(
        &mut self,
        sym: &Symbol,
        expr: &Expr<'a>,
        layout: &Layout<'a>,
    ) -> Result<(), String> {
        match expr {
            Expr::Literal(lit) => {
                if self.env().lazy_literals {
                    self.literal_map().insert(*sym, lit.clone());
                } else {
                    self.load_literal(sym, lit)?;
                }
                Ok(())
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
                        // For most builtins instead of calling a function, we can just inline the low level.
                        match *func_sym {
                            Symbol::NUM_ABS => {
                                self.build_run_low_level(sym, &LowLevel::NumAbs, arguments, layout)
                            }
                            Symbol::NUM_ADD => {
                                self.build_run_low_level(sym, &LowLevel::NumAdd, arguments, layout)
                            }
                            Symbol::NUM_ACOS => {
                                self.build_run_low_level(sym, &LowLevel::NumAcos, arguments, layout)
                            }
                            Symbol::NUM_ASIN => {
                                self.build_run_low_level(sym, &LowLevel::NumAsin, arguments, layout)
                            }
                            Symbol::NUM_ATAN => {
                                self.build_run_low_level(sym, &LowLevel::NumAtan, arguments, layout)
                            }
                            Symbol::NUM_MUL => {
                                self.build_run_low_level(sym, &LowLevel::NumMul, arguments, layout)
                            }
                            Symbol::NUM_POW_INT => self.build_run_low_level(
                                sym,
                                &LowLevel::NumPowInt,
                                arguments,
                                layout,
                            ),
                            Symbol::NUM_SUB => {
                                self.build_run_low_level(sym, &LowLevel::NumSub, arguments, layout)
                            }
                            Symbol::NUM_ROUND => self.build_run_low_level(
                                sym,
                                &LowLevel::NumRound,
                                arguments,
                                layout,
                            ),
                            Symbol::BOOL_EQ => {
                                self.build_run_low_level(sym, &LowLevel::Eq, arguments, layout)
                            }
                            x if x
                                .module_string(&self.env().interns)
                                .starts_with(ModuleName::APP) =>
                            {
                                let fn_name = LayoutIds::default()
                                    .get(*func_sym, layout)
                                    .to_symbol_string(*func_sym, &self.env().interns);
                                // Now that the arguments are needed, load them if they are literals.
                                self.load_literal_symbols(arguments)?;
                                self.build_fn_call(sym, fn_name, arguments, arg_layouts, ret_layout)
                            }
                            x => Err(format!("the function, {:?}, is not yet implemented", x)),
                        }
                    }

                    CallType::LowLevel { op: lowlevel, .. } => {
                        self.build_run_low_level(sym, lowlevel, arguments, layout)
                    }
                    x => Err(format!("the call type, {:?}, is not yet implemented", x)),
                }
            }
            Expr::Struct(fields) => {
                self.load_literal_symbols(fields)?;
                self.create_struct(sym, layout, fields)
            }
            Expr::StructAtIndex {
                index,
                field_layouts,
                structure,
            } => self.load_struct_at_index(sym, structure, *index, field_layouts),
            x => Err(format!("the expression, {:?}, is not yet implemented", x)),
        }
    }

    /// build_run_low_level builds the low level opertation and outputs to the specified symbol.
    /// The builder must keep track of the symbol because it may be referred to later.
    fn build_run_low_level(
        &mut self,
        sym: &Symbol,
        lowlevel: &LowLevel,
        args: &'a [Symbol],
        layout: &Layout<'a>,
    ) -> Result<(), String> {
        // Now that the arguments are needed, load them if they are literals.
        self.load_literal_symbols(args)?;
        match lowlevel {
            LowLevel::NumAbs => {
                // TODO: when this is expanded to floats. deal with typecasting here, and then call correct low level method.
                match layout {
                    Layout::Builtin(Builtin::Int64) => self.build_num_abs_i64(sym, &args[0]),
                    Layout::Builtin(Builtin::Float64) => self.build_num_abs_f64(sym, &args[0]),
                    x => Err(format!("layout, {:?}, not implemented yet", x)),
                }
            }
            LowLevel::NumAdd => {
                // TODO: when this is expanded to floats. deal with typecasting here, and then call correct low level method.
                match layout {
                    Layout::Builtin(Builtin::Int64) => {
                        self.build_num_add_i64(sym, &args[0], &args[1])
                    }
                    Layout::Builtin(Builtin::Float64) => {
                        self.build_num_add_f64(sym, &args[0], &args[1])
                    }
                    x => Err(format!("layout, {:?}, not implemented yet", x)),
                }
            }
            LowLevel::NumAcos => {
                self.build_fn_call(sym, bitcode::NUM_ACOS.to_string(), args, &[*layout], layout)
            }
            LowLevel::NumAsin => {
                self.build_fn_call(sym, bitcode::NUM_ASIN.to_string(), args, &[*layout], layout)
            }
            LowLevel::NumAtan => {
                self.build_fn_call(sym, bitcode::NUM_ATAN.to_string(), args, &[*layout], layout)
            }
            LowLevel::NumMul => {
                // TODO: when this is expanded to floats. deal with typecasting here, and then call correct low level method.
                match layout {
                    Layout::Builtin(Builtin::Int64) => {
                        self.build_num_mul_i64(sym, &args[0], &args[1])
                    }
                    x => Err(format!("layout, {:?}, not implemented yet", x)),
                }
            }
            LowLevel::NumPowInt => self.build_fn_call(
                sym,
                bitcode::NUM_POW_INT.to_string(),
                args,
                &[*layout, *layout],
                layout,
            ),
            LowLevel::NumSub => {
                // TODO: when this is expanded to floats. deal with typecasting here, and then call correct low level method.
                match layout {
                    Layout::Builtin(Builtin::Int64) => {
                        self.build_num_sub_i64(sym, &args[0], &args[1])
                    }
                    x => Err(format!("layout, {:?}, not implemented yet", x)),
                }
            }
            LowLevel::Eq => match layout {
                Layout::Builtin(Builtin::Int1) => self.build_eq_i64(sym, &args[0], &args[1]),
                // Should we panic?
                x => Err(format!("wrong layout, {:?}, for LowLevel::Eq", x)),
            },
            LowLevel::NumRound => self.build_fn_call(
                sym,
                bitcode::NUM_ROUND.to_string(),
                args,
                &[Layout::Builtin(Builtin::Float64)],
                layout,
            ),
            x => Err(format!("low level, {:?}. is not yet implemented", x)),
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
    ) -> Result<(), String>;

    /// build_num_abs_i64 stores the absolute value of src into dst.
    /// It only deals with inputs and outputs of i64 type.
    fn build_num_abs_i64(&mut self, dst: &Symbol, src: &Symbol) -> Result<(), String>;

    /// build_num_abs_f64 stores the absolute value of src into dst.
    /// It only deals with inputs and outputs of f64 type.
    fn build_num_abs_f64(&mut self, dst: &Symbol, src: &Symbol) -> Result<(), String>;

    /// build_num_add_i64 stores the sum of src1 and src2 into dst.
    /// It only deals with inputs and outputs of i64 type.
    fn build_num_add_i64(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
    ) -> Result<(), String>;

    /// build_num_add_f64 stores the sum of src1 and src2 into dst.
    /// It only deals with inputs and outputs of f64 type.
    fn build_num_add_f64(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
    ) -> Result<(), String>;

    /// build_num_mul_i64 stores `src1 * src2` into dst.
    /// It only deals with inputs and outputs of i64 type.
    fn build_num_mul_i64(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
    ) -> Result<(), String>;

    /// build_num_sub_i64 stores the `src1 - src2` difference into dst.
    /// It only deals with inputs and outputs of i64 type.
    fn build_num_sub_i64(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
    ) -> Result<(), String>;

    /// build_eq_i64 stores the result of `src1 == src2` into dst.
    /// It only deals with inputs and outputs of i64 type.
    fn build_eq_i64(&mut self, dst: &Symbol, src1: &Symbol, src2: &Symbol) -> Result<(), String>;

    /// literal_map gets the map from symbol to literal, used for lazy loading and literal folding.
    fn literal_map(&mut self) -> &mut MutMap<Symbol, Literal<'a>>;

    fn load_literal_symbols(&mut self, syms: &[Symbol]) -> Result<(), String> {
        if self.env().lazy_literals {
            for sym in syms {
                if let Some(lit) = self.literal_map().remove(sym) {
                    self.load_literal(sym, &lit)?;
                }
            }
        }
        Ok(())
    }

    /// create_struct creates a struct with the elements specified loaded into it as data.
    fn create_struct(
        &mut self,
        sym: &Symbol,
        layout: &Layout<'a>,
        fields: &'a [Symbol],
    ) -> Result<(), String>;

    /// load_struct_at_index loads into `sym` the value at `index` in `structure`.
    fn load_struct_at_index(
        &mut self,
        sym: &Symbol,
        structure: &Symbol,
        index: u64,
        field_layouts: &'a [Layout<'a>],
    ) -> Result<(), String>;

    /// load_literal sets a symbol to be equal to a literal.
    fn load_literal(&mut self, sym: &Symbol, lit: &Literal<'a>) -> Result<(), String>;

    /// return_symbol moves a symbol to the correct return location for the backend.
    fn return_symbol(&mut self, sym: &Symbol, layout: &Layout<'a>) -> Result<(), String>;

    /// free_symbols will free all symbols for the given statement.
    fn free_symbols(&mut self, stmt: &Stmt<'a>) {
        if let Some(syms) = self.free_map().remove(&(stmt as *const Stmt<'a>)) {
            for sym in syms {
                //println!("Freeing symbol: {:?}", sym);
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
                        for sym in *elems {
                            self.set_last_seen(*sym, stmt);
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
                    Expr::Reset(sym) => {
                        self.set_last_seen(*sym, stmt);
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
                ..
            } => {
                for param in *parameters {
                    self.set_last_seen(param.symbol, stmt);
                }
                self.scan_ast(continuation);
                self.scan_ast(remainder);
            }
            Stmt::Jump(JoinPointId(sym), symbols) => {
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
            CallType::HigherOrderLowLevel { .. } => {}
            CallType::Foreign { .. } => {}
        }
    }
}
