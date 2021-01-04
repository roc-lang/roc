#![warn(clippy::all, clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

use bumpalo::{collections::Vec, Bump};
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::TagName;
use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::ir::{CallType, Expr, JoinPointId, Literal, Proc, Stmt};
use roc_mono::layout::{Builtin, Layout};
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
}

// INLINED_SYMBOLS is a set of all of the functions we automatically inline if seen.
const INLINED_SYMBOLS: [Symbol; 2] = [Symbol::NUM_ABS, Symbol::NUM_ADD];

// These relocations likely will need a length.
// They may even need more definition, but this should be at least good enough for how we will use elf.
#[allow(dead_code)]
enum Relocation<'a> {
    LocalData { offset: u64, data: &'a [u8] },
    LinkedFunction { offset: u64, name: &'a str },
    LinkedData { offset: u64, name: &'a str },
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

    /// build_proc creates a procedure and outputs it to the wrapped object writer.
    fn build_proc(&mut self, proc: Proc<'a>) -> Result<(&'a [u8], &[Relocation]), String> {
        self.reset();
        // TODO: let the backend know of all the arguments.
        // let start = std::time::Instant::now();
        self.scan_ast(&proc.body);
        self.create_free_map();
        // let duration = start.elapsed();
        // println!("Time to calculate lifetimes: {:?}", duration);
        // println!("{:?}", self.last_seen_map());
        self.build_stmt(&proc.body)?;
        self.finalize()
    }

    /// build_stmt builds a statement and outputs at the end of the buffer.
    fn build_stmt(&mut self, stmt: &Stmt<'a>) -> Result<(), String> {
        match stmt {
            Stmt::Let(sym, expr, layout, following) => {
                self.build_expr(sym, expr, layout)?;
                self.free_symbols(stmt);
                self.build_stmt(following)?;
                Ok(())
            }
            Stmt::Ret(sym) => {
                self.load_literal_symbols(&[*sym])?;
                self.return_symbol(sym)?;
                self.free_symbols(stmt);
                Ok(())
            }
            Stmt::Invoke {
                symbol,
                layout,
                call,
                pass,
                fail: _,
            } => {
                // for now, treat invoke as a normal call

                let stmt = Stmt::Let(*symbol, Expr::Call(call.clone()), layout.clone(), pass);
                self.build_stmt(&stmt)
            }
            x => Err(format!("the statement, {:?}, is not yet implemented", x)),
        }
    }

    /// build_expr builds the expressions for the specified symbol.
    /// The builder must keep track of the symbol because it may be refered to later.
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
                    CallType::ByName { name: func_sym, .. } => {
                        match *func_sym {
                            Symbol::NUM_ABS => {
                                // Instead of calling the function, just inline it.
                                self.build_run_low_level(sym, &LowLevel::NumAbs, arguments, layout)
                            }
                            Symbol::NUM_ADD => {
                                // Instead of calling the function, just inline it.
                                self.build_run_low_level(sym, &LowLevel::NumAdd, arguments, layout)
                            }
                            x => Err(format!("the function, {:?}, is not yet implemented", x)),
                        }
                    }

                    CallType::LowLevel { op: lowlevel } => {
                        self.build_run_low_level(sym, lowlevel, arguments, layout)
                    }
                    x => Err(format!("the call type, {:?}, is not yet implemented", x)),
                }
            }
            x => Err(format!("the expression, {:?}, is not yet implemented", x)),
        }
    }

    /// build_run_low_level builds the low level opertation and outputs to the specified symbol.
    /// The builder must keep track of the symbol because it may be refered to later.
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
                    x => Err(format!("layout, {:?}, not implemented yet", x)),
                }
            }
            LowLevel::NumAdd => {
                // TODO: when this is expanded to floats. deal with typecasting here, and then call correct low level method.
                match layout {
                    Layout::Builtin(Builtin::Int64) => {
                        self.build_num_add_i64(sym, &args[0], &args[1])
                    }
                    x => Err(format!("layout, {:?}, not implemented yet", x)),
                }
            }
            x => Err(format!("low level, {:?}. is not yet implemented", x)),
        }
    }

    /// build_num_abs_i64 stores the absolute value of src into dst.
    /// It only deals with inputs and outputs of i64 type.
    fn build_num_abs_i64(&mut self, dst: &Symbol, src: &Symbol) -> Result<(), String>;

    /// build_num_add_i64 stores the absolute value of src into dst.
    /// It only deals with inputs and outputs of i64 type.
    fn build_num_add_i64(
        &mut self,
        dst: &Symbol,
        src1: &Symbol,
        src2: &Symbol,
    ) -> Result<(), String>;

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

    /// load_literal sets a symbol to be equal to a literal.
    fn load_literal(&mut self, sym: &Symbol, lit: &Literal<'a>) -> Result<(), String>;

    /// return_symbol moves a symbol to the correct return location for the backend.
    fn return_symbol(&mut self, sym: &Symbol) -> Result<(), String>;

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

    /// set_not_leaf_function lets the backend know that it is not a leaf function.
    fn set_not_leaf_function(&mut self);

    /// scan_ast runs through the ast and fill the last seen map.
    /// It also checks if the function is a leaf function or not.
    /// This must iterate through the ast in the same way that build_stmt does. i.e. then before else.
    fn scan_ast(&mut self, stmt: &Stmt<'a>) {
        match stmt {
            Stmt::Let(sym, expr, _, following) => {
                self.set_last_seen(*sym, stmt);
                match expr {
                    Expr::Literal(_) => {}
                    Expr::FunctionPointer(sym, _) => self.set_last_seen(*sym, stmt),

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
                    Expr::AccessAtIndex { structure, .. } => {
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

            Stmt::Invoke {
                symbol,
                layout,
                call,
                pass,
                fail: _,
            } => {
                // for now, treat invoke as a normal call

                let stmt = Stmt::Let(*symbol, Expr::Call(call.clone()), layout.clone(), pass);
                self.scan_ast(&stmt);
            }

            Stmt::Switch {
                cond_symbol,
                branches,
                default_branch,
                ..
            } => {
                self.set_last_seen(*cond_symbol, stmt);
                for (_, branch) in *branches {
                    self.scan_ast(branch);
                }
                self.scan_ast(default_branch);
            }
            Stmt::Ret(sym) => {
                self.set_last_seen(*sym, stmt);
            }
            Stmt::Unreachable => {}
            Stmt::Inc(sym, following) => {
                self.set_last_seen(*sym, stmt);
                self.scan_ast(following);
            }
            Stmt::Dec(sym, following) => {
                self.set_last_seen(*sym, stmt);
                self.scan_ast(following);
            }
            Stmt::Join {
                parameters,
                continuation,
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
            CallType::ByName { name: sym, .. } => {
                // For functions that we won't inline, we should not be a leaf function.
                if !INLINED_SYMBOLS.contains(sym) {
                    self.set_not_leaf_function();
                }
            }
            CallType::ByPointer { name: sym, .. } => {
                self.set_not_leaf_function();
                self.set_last_seen(*sym, stmt);
            }
            CallType::LowLevel { .. } => {}
            CallType::Foreign { .. } => self.set_not_leaf_function(),
        }
    }
}
