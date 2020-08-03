use crate::expr::InProgressProc::*;
use crate::expr::Pattern;
use crate::expr::RecordDestruct;
use crate::layout::{list_layout_from_elem, Builtin, Layout, LayoutCache, LayoutProblem};
use crate::pattern::{Ctor, Guard, RenderAs, TagId};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{default_hasher, MutMap, MutSet};
use roc_module::ident::{Ident, Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_problem::can::RuntimeError;
use roc_region::all::{Located, Region};
use roc_types::subs::{Content, FlatType, Subs, Variable};
use std::collections::HashMap;
use ven_pretty::{BoxAllocator, DocAllocator, DocBuilder};

/*

#[derive(Clone, Debug)]
pub enum MonoProblem {
    PatternProblem(crate::pattern::Error),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PartialProc<'a> {
    pub annotation: Variable,
    pub pattern_symbols: Vec<'a, Symbol>,
    pub body: roc_can::expr::Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PendingSpecialization<'a> {
    pub fn_var: Variable,
    pub ret_var: Variable,
    pub pattern_vars: Vec<'a, Variable>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Proc<'a> {
    pub name: Symbol,
    pub args: &'a [(Layout<'a>, Symbol)],
    pub body: Expr<'a>,
    pub closes_over: Layout<'a>,
    pub ret_layout: Layout<'a>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Procs<'a> {
    pub partial_procs: MutMap<Symbol, PartialProc<'a>>,
    pub module_thunks: MutSet<Symbol>,
    pub pending_specializations:
        Option<MutMap<Symbol, MutMap<Layout<'a>, PendingSpecialization<'a>>>>,
    pub specialized: MutMap<(Symbol, Layout<'a>), InProgressProc<'a>>,
    pub runtime_errors: MutMap<Symbol, &'a str>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum InProgressProc<'a> {
    InProgress,
    Done(Proc<'a>),
}

impl<'a> Procs<'a> {
    // TODO trim down these arguments!
    #[allow(clippy::too_many_arguments)]
    pub fn insert_named(
        &mut self,
        env: &mut Env<'a, '_>,
        layout_cache: &mut LayoutCache<'a>,
        name: Symbol,
        annotation: Variable,
        loc_args: std::vec::Vec<(Variable, Located<roc_can::pattern::Pattern>)>,
        loc_body: Located<roc_can::expr::Expr>,
        ret_var: Variable,
    ) {
        match patterns_to_when(env, self, layout_cache, loc_args, ret_var, loc_body) {
            Ok((_, pattern_symbols, body)) => {
                // a named closure. Since these aren't specialized by the surrounding
                // context, we can't add pending specializations for them yet.
                // (If we did, all named polymorphic functions would immediately error
                // on trying to convert a flex var to a Layout.)
                self.partial_procs.insert(
                    name,
                    PartialProc {
                        annotation,
                        pattern_symbols,
                        body: body.value,
                    },
                );
            }

            Err(error) => {
                // If the function has invalid patterns in its arguments,
                // its call sites will code gen to runtime errors. This happens
                // at the call site so we don't have to try to define the
                // function LLVM, which would be difficult considering LLVM
                // wants to know what symbols each argument corresponds to,
                // and in this case the patterns were invalid, so we don't know
                // what the symbols ought to be.

                let error_msg = format!("TODO generate a RuntimeError message for {:?}", error);

                self.runtime_errors.insert(name, env.arena.alloc(error_msg));
            }
        }
    }

    // TODO trim these down
    #[allow(clippy::too_many_arguments)]
    pub fn insert_anonymous(
        &mut self,
        env: &mut Env<'a, '_>,
        symbol: Symbol,
        annotation: Variable,
        loc_args: std::vec::Vec<(Variable, Located<roc_can::pattern::Pattern>)>,
        loc_body: Located<roc_can::expr::Expr>,
        ret_var: Variable,
        layout_cache: &mut LayoutCache<'a>,
    ) -> Result<Layout<'a>, RuntimeError> {
        match patterns_to_when(env, self, layout_cache, loc_args, ret_var, loc_body) {
            Ok((pattern_vars, pattern_symbols, body)) => {
                // an anonymous closure. These will always be specialized already
                // by the surrounding context, so we can add pending specializations
                // for them immediately.
                let layout = layout_cache
                    .from_var(env.arena, annotation, env.subs, env.pointer_size)
                    .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

                // if we've already specialized this one, no further work is needed.
                //
                // NOTE: this #[allow(clippy::map_entry)] here is for correctness!
                // Changing it to use .entry() would necessarily make it incorrect.
                #[allow(clippy::map_entry)]
                if !self.specialized.contains_key(&(symbol, layout.clone())) {
                    let pending = PendingSpecialization {
                        ret_var,
                        fn_var: annotation,
                        pattern_vars,
                    };

                    match &mut self.pending_specializations {
                        Some(pending_specializations) => {
                            // register the pending specialization, so this gets code genned later
                            add_pending(pending_specializations, symbol, layout.clone(), pending);

                            debug_assert!(!self.partial_procs.contains_key(&symbol), "Procs was told to insert a value for symbol {:?}, but there was already an entry for that key! Procs should never attempt to insert duplicates.", symbol);

                            self.partial_procs.insert(
                                symbol,
                                PartialProc {
                                    annotation,
                                    pattern_symbols,
                                    body: body.value,
                                },
                            );
                        }
                        None => {
                            // TODO should pending_procs hold a Rc<Proc>?
                            let partial_proc = PartialProc {
                                annotation,
                                pattern_symbols,
                                body: body.value,
                            };

                            // Mark this proc as in-progress, so if we're dealing with
                            // mutually recursive functions, we don't loop forever.
                            // (We had a bug around this before this system existed!)
                            self.specialized
                                .insert((symbol, layout.clone()), InProgress);

                            match specialize(env, self, symbol, layout_cache, pending, partial_proc)
                            {
                                Ok(proc) => {
                                    self.specialized
                                        .insert((symbol, layout.clone()), Done(proc));
                                }
                                Err(error) => {
                                    let error_msg = format!(
                                        "TODO generate a RuntimeError message for {:?}",
                                        error
                                    );
                                    self.runtime_errors
                                        .insert(symbol, env.arena.alloc(error_msg));
                                }
                            }
                        }
                    }
                }

                Ok(layout)
            }
            Err(loc_error) => Err(loc_error.value),
        }
    }
}

fn add_pending<'a>(
    pending_specializations: &mut MutMap<Symbol, MutMap<Layout<'a>, PendingSpecialization<'a>>>,
    symbol: Symbol,
    layout: Layout<'a>,
    pending: PendingSpecialization<'a>,
) {
    let all_pending = pending_specializations
        .entry(symbol)
        .or_insert_with(|| HashMap::with_capacity_and_hasher(1, default_hasher()));

    all_pending.insert(layout, pending);
}

#[derive(Default)]
pub struct Specializations<'a> {
    by_symbol: MutMap<Symbol, MutMap<Layout<'a>, Proc<'a>>>,
    runtime_errors: MutSet<Symbol>,
}

impl<'a> Specializations<'a> {
    pub fn insert(&mut self, symbol: Symbol, layout: Layout<'a>, proc: Proc<'a>) {
        let procs_by_layout = self
            .by_symbol
            .entry(symbol)
            .or_insert_with(|| HashMap::with_capacity_and_hasher(1, default_hasher()));

        // If we already have an entry for this, it should be no different
        // from what we're about to insert.
        debug_assert!(
            !procs_by_layout.contains_key(&layout) || procs_by_layout.get(&layout) == Some(&proc)
        );

        // We shouldn't already have a runtime error recorded for this symbol
        debug_assert!(!self.runtime_errors.contains(&symbol));

        procs_by_layout.insert(layout, proc);
    }

    pub fn runtime_error(&mut self, symbol: Symbol) {
        // We shouldn't already have a normal proc recorded for this symbol
        debug_assert!(!self.by_symbol.contains_key(&symbol));

        self.runtime_errors.insert(symbol);
    }

    pub fn into_owned(self) -> (MutMap<Symbol, MutMap<Layout<'a>, Proc<'a>>>, MutSet<Symbol>) {
        (self.by_symbol, self.runtime_errors)
    }

    pub fn len(&self) -> usize {
        let runtime_errors: usize = self.runtime_errors.len();
        let specializations: usize = self.by_symbol.len();

        runtime_errors + specializations
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
*/

pub type MonoProblem = crate::expr::MonoProblem;
pub type PendingSpecialization<'a> = crate::expr::PendingSpecialization<'a>;
pub type PartialProc<'a> = crate::expr::PartialProc<'a>;
pub type Proc<'a> = crate::expr::Proc<'a>;
pub type Procs<'a> = crate::expr::Procs<'a>;
pub type Env<'a, 'i> = crate::expr::Env<'a, 'i>;

/*
pub struct Env<'a, 'i> {
    pub arena: &'a Bump,
    pub subs: &'a mut Subs,
    pub problems: &'i mut std::vec::Vec<MonoProblem>,
    pub home: ModuleId,
    pub ident_ids: &'i mut IdentIds,
    pub pointer_size: u32,
    pub jump_counter: &'a mut u64,
}

impl<'a, 'i> Env<'a, 'i> {
    pub fn unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        self.home.register_debug_idents(&self.ident_ids);

        Symbol::new(self.home, ident_id)
    }
}
*/

#[derive(Clone, Debug, PartialEq, Copy)]
pub struct JoinPointId(Symbol);

pub type Stores<'a> = &'a [(Symbol, Layout<'a>, Expr<'a>)];
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<'a> {
    Let(Symbol, Expr<'a>, Layout<'a>, &'a Stmt<'a>),
    Switch {
        /// This *must* stand for an integer, because Switch potentially compiles to a jump table.
        cond_symbol: Symbol,
        cond_layout: Layout<'a>,
        /// The u64 in the tuple will be compared directly to the condition Expr.
        /// If they are equal, this branch will be taken.
        branches: &'a [(u64, Stmt<'a>)],
        /// If no other branches pass, this default branch will be taken.
        default_branch: &'a Stmt<'a>,
        /// Each branch must return a value of this type.
        ret_layout: Layout<'a>,
    },
    Cond {
        // The left-hand side of the conditional comparison and the right-hand side.
        // These are stored separately because there are different machine instructions
        // for e.g. "compare float and jump" vs. "compare integer and jump"

        // symbol storing the original expression that we branch on, e.g. `Ok 42`
        // required for RC logic
        cond_symbol: Symbol,
        cond_layout: Layout<'a>,

        // symbol storing the value that we branch on, e.g. `1` representing the `Ok` tag
        branching_symbol: Symbol,
        branching_layout: Layout<'a>,

        // What to do if the condition either passes or fails
        pass: &'a Stmt<'a>,
        fail: &'a Stmt<'a>,
        ret_layout: Layout<'a>,
    },
    Ret(Symbol),
    Inc(Symbol, &'a Stmt<'a>),
    Dec(Symbol, &'a Stmt<'a>),
    Join {
        id: JoinPointId,
        arguments: &'a [Symbol],
        result: &'a Stmt<'a>,
        continuation: &'a Stmt<'a>,
    },
    Jump(JoinPointId, &'a [Symbol]),
    RuntimeError(&'a str),
}
#[derive(Clone, Debug, PartialEq)]
pub enum Literal<'a> {
    // Literals
    Int(i64),
    Float(f64),
    Str(&'a str),
    /// Closed tag unions containing exactly two (0-arity) tags compile to Expr::Bool,
    /// so they can (at least potentially) be emitted as 1-bit machine bools.
    ///
    /// So [ True, False ] compiles to this, and so do [ A, B ] and [ Foo, Bar ].
    /// However, a union like [ True, False, Other Int ] would not.
    Bool(bool),
    /// Closed tag unions containing between 3 and 256 tags (all of 0 arity)
    /// compile to bytes, e.g. [ Blue, Black, Red, Green, White ]
    Byte(u8),
}
#[derive(Clone, Debug, PartialEq, Copy)]
pub enum CallType {
    ByName(Symbol),
    ByPointer(Symbol),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    Literal(Literal<'a>),

    // Functions
    FunctionPointer(Symbol, Layout<'a>),
    FunctionCall {
        call_type: CallType,
        layout: Layout<'a>,
        args: &'a [Symbol],
    },
    RunLowLevel(LowLevel, &'a [Symbol]),

    Tag {
        tag_layout: Layout<'a>,
        tag_name: TagName,
        tag_id: u8,
        union_size: u8,
        arguments: &'a [Symbol],
    },
    Struct(&'a [Symbol]),
    AccessAtIndex {
        index: u64,
        field_layouts: &'a [Layout<'a>],
        structure: Symbol,
        is_unwrapped: bool,
    },

    Array {
        elem_layout: Layout<'a>,
        elems: &'a [Symbol],
    },
    EmptyArray,

    RuntimeErrorFunction(&'a str),
}

impl<'a> Literal<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D, parens: bool) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use Literal::*;

        match self {
            Int(lit) => alloc.text(format!("{}i64", lit)),
            Float(lit) => alloc.text(format!("{}f64", lit)),
            Bool(lit) => alloc.text(format!("{}", lit)),
            Byte(lit) => alloc.text(format!("{}u8", lit)),
            Str(lit) => alloc.text(format!("{:?}", lit)),
        }
    }
}

fn symbol_to_doc<'b, D, A>(alloc: &'b D, symbol: Symbol) -> DocBuilder<'b, D, A>
where
    D: DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    alloc.text(format!("{}", symbol))
}

impl<'a> Expr<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D, parens: bool) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use Expr::*;

        match self {
            Literal(lit) => lit.to_doc(alloc, false),

            FunctionPointer(symbol, _) => symbol_to_doc(alloc, *symbol),

            FunctionCall {
                call_type, args, ..
            } => match call_type {
                CallType::ByName(name) => {
                    let it = std::iter::once(name)
                        .chain(args.iter())
                        .map(|s| symbol_to_doc(alloc, *s));

                    alloc.text("CallByName ").append(alloc.intersperse(it, " "))
                }
                CallType::ByPointer(name) => {
                    let it = std::iter::once(name)
                        .chain(args.iter())
                        .map(|s| symbol_to_doc(alloc, *s));

                    alloc
                        .text("CallByPointer ")
                        .append(alloc.intersperse(it, " "))
                }
            },
            RunLowLevel(lowlevel, args) => {
                let it = args.iter().map(|s| symbol_to_doc(alloc, *s));

                alloc
                    .text(format!("lowlevel {:?} ", lowlevel))
                    .append(alloc.intersperse(it, " "))
            }
            Tag {
                tag_name,
                arguments,
                ..
            } => {
                let doc_tag = match tag_name {
                    TagName::Global(s) => alloc.text(s.as_str()),
                    TagName::Private(s) => alloc.text(format!("{}", s)),
                };

                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s));

                doc_tag
                    .append(alloc.space())
                    .append(alloc.intersperse(it, " "))
            }
            Struct(args) => {
                let it = args.iter().map(|s| symbol_to_doc(alloc, *s));

                alloc
                    .text("Struct {")
                    .append(alloc.intersperse(it, ", "))
                    .append(alloc.text("}"))
            }
            Array { elems, .. } => {
                let it = elems.iter().map(|s| symbol_to_doc(alloc, *s));

                alloc
                    .text("Array [")
                    .append(alloc.intersperse(it, ", "))
                    .append(alloc.text("]"))
            }
            EmptyArray => alloc.text("Array []"),

            AccessAtIndex {
                index, structure, ..
            } => alloc
                .text(format!("Index {} ", index))
                .append(symbol_to_doc(alloc, *structure)),

            RuntimeErrorFunction(s) => alloc.text(format!("ErrorFunction {}", s)),
        }
    }
}

impl<'a> Stmt<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D, parens: bool) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use Stmt::*;

        match self {
            Let(symbol, expr, _, cont) => alloc
                .text("let ")
                .append(symbol_to_doc(alloc, *symbol))
                .append(" = ")
                .append(expr.to_doc(alloc, false))
                .append(";")
                .append(alloc.hardline())
                .append(cont.to_doc(alloc, false)),

            Ret(symbol) => alloc
                .text("ret ")
                .append(symbol_to_doc(alloc, *symbol))
                .append(";"),

            Switch {
                cond_symbol,
                branches,
                default_branch,
                ..
            } => {
                let default_doc = alloc
                    .text("default:")
                    .append(alloc.hardline())
                    .append(default_branch.to_doc(alloc, false).indent(4))
                    .indent(4);

                let branches_docs = branches
                    .iter()
                    .map(|(tag, expr)| {
                        alloc
                            .text(format!("case {}:", tag))
                            .append(alloc.hardline())
                            .append(expr.to_doc(alloc, false).indent(4))
                            .indent(4)
                    })
                    .chain(std::iter::once(default_doc));
                //
                alloc
                    .text(format!("switch {}:", cond_symbol))
                    .append(alloc.hardline())
                    .append(
                        alloc.intersperse(branches_docs, alloc.hardline().append(alloc.hardline())),
                    )
                    .append(alloc.hardline())
            }

            Cond {
                branching_symbol,
                pass,
                fail,
                ..
            } => alloc
                .text(format!("if {} then", branching_symbol))
                .append(alloc.hardline())
                .append(pass.to_doc(alloc, false).indent(4))
                .append(alloc.hardline())
                .append(alloc.text("else"))
                .append(alloc.hardline())
                .append(fail.to_doc(alloc, false).indent(4)),
            RuntimeError(s) => alloc.text(format!("Error {}", s)),
            _ => todo!(),
        }
        /*
        Inc(Symbol, &'a Stmt<'a>),
        Dec(Symbol, &'a Stmt<'a>),
        Join {
            id: JoinPointId,
            arguments: &'a [Symbol],
            result: &'a Stmt<'a>,
            continuation: &'a Stmt<'a>,
        },
        Jump(JoinPointId, &'a [Symbol]),
             */
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let allocator = BoxAllocator;
        let mut w = std::vec::Vec::new();
        self.to_doc::<_, ()>(&allocator, false)
            .1
            .render(width, &mut w)
            .unwrap();
        w.push(b'\n');
        String::from_utf8(w).unwrap()
    }
}

fn patterns_to_when<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    patterns: std::vec::Vec<(Variable, Located<roc_can::pattern::Pattern>)>,
    body_var: Variable,
    body: Located<roc_can::expr::Expr>,
) -> Result<
    (
        Vec<'a, Variable>,
        Vec<'a, Symbol>,
        Located<roc_can::expr::Expr>,
    ),
    Located<RuntimeError>,
> {
    todo!()
}

fn specialize<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    proc_name: Symbol,
    layout_cache: &mut LayoutCache<'a>,
    pending: PendingSpecialization<'a>,
    partial_proc: PartialProc<'a>,
) -> Result<Proc<'a>, LayoutProblem> {
    todo!()
}

pub fn with_hole<'a>(
    env: &mut Env<'a, '_>,
    can_expr: roc_can::expr::Expr,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
) -> Box<dyn FnOnce(&'a Stmt<'a>) -> Stmt<'a> + 'a> {
    use crate::expr::num_argument_to_int_or_float;
    use crate::expr::IntOrFloat;
    use roc_can::expr::Expr::*;

    let arena = env.arena;

    match can_expr {
        Int(_, num) => Box::new(move |hole| {
            Stmt::Let(
                assigned,
                Expr::Literal(Literal::Int(num)),
                Layout::Builtin(Builtin::Int64),
                hole,
            )
        }),

        Float(_, num) => Box::new(move |hole| {
            Stmt::Let(
                assigned,
                Expr::Literal(Literal::Float(num)),
                Layout::Builtin(Builtin::Float64),
                hole,
            )
        }),

        Str(string) | BlockStr(string) => Box::new(move |hole| {
            Stmt::Let(
                assigned,
                Expr::Literal(Literal::Str(arena.alloc(string))),
                Layout::Builtin(Builtin::Str),
                hole,
            )
        }),

        Num(var, num) => match num_argument_to_int_or_float(env.subs, var) {
            IntOrFloat::IntType => Box::new(move |hole| {
                Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Int(num)),
                    Layout::Builtin(Builtin::Int64),
                    hole,
                )
            }),
            IntOrFloat::FloatType => Box::new(move |hole| {
                Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Float(num as f64)),
                    Layout::Builtin(Builtin::Float64),
                    hole,
                )
            }),
        },
        LetNonRec(def, cont, _, _) => {
            if let roc_can::pattern::Pattern::Identifier(symbol) = def.loc_pattern.value {
                let expr = with_hole(env, def.loc_expr.value, procs, layout_cache, symbol);
                let stmt = with_hole(env, cont.value, procs, layout_cache, assigned);

                Box::new(move |hole| expr(arena.alloc(stmt(hole))))
            } else {
                todo!()
            }
        }
        Var(symbol) => Box::new(move |_| Stmt::Ret(symbol)),

        Tag {
            variant_var,
            name: tag_name,
            arguments: args,
            ..
        } => {
            use crate::layout::UnionVariant::*;
            let arena = env.arena;

            let variant = crate::layout::union_sorted_tags(
                env.arena,
                variant_var,
                env.subs,
                env.pointer_size,
            );

            match variant {
                Never => unreachable!("The `[]` type has no constructors"),
                Unit => Box::new(move |hole| {
                    Stmt::Let(
                        assigned,
                        Expr::Struct(&[]),
                        Layout::Builtin(Builtin::Float64),
                        hole,
                    )
                }),
                BoolUnion { ttrue, .. } => Box::new(move |hole| {
                    Stmt::Let(
                        assigned,
                        Expr::Literal(Literal::Bool(tag_name == ttrue)),
                        Layout::Builtin(Builtin::Int1),
                        hole,
                    )
                }),
                ByteUnion(tag_names) => {
                    let tag_id = tag_names
                        .iter()
                        .position(|key| key == &tag_name)
                        .expect("tag must be in its own type");

                    Box::new(move |hole| {
                        Stmt::Let(
                            assigned,
                            Expr::Literal(Literal::Byte(tag_id as u8)),
                            Layout::Builtin(Builtin::Int8),
                            hole,
                        )
                    })
                }

                Unwrapped(field_layouts) => {
                    let mut field_symbols = Vec::with_capacity_in(field_layouts.len(), env.arena);

                    let field_exprs = args
                        .into_iter()
                        .map(|(_, arg)| {
                            let symbol = env.unique_symbol();
                            field_symbols.push(symbol);
                            with_hole(env, arg.value, procs, layout_cache, symbol)
                        })
                        .collect::<std::vec::Vec<_>>();

                    let layout = Layout::Struct(field_layouts.into_bump_slice());

                    Box::new(move |hole| {
                        let mut stmt = Stmt::Let(
                            assigned,
                            Expr::Struct(field_symbols.into_bump_slice()),
                            layout,
                            hole,
                        );

                        for expr in field_exprs.into_iter().rev() {
                            stmt = expr(arena.alloc(stmt));
                        }

                        stmt
                    })
                }
                Wrapped(sorted_tag_layouts) => {
                    let union_size = sorted_tag_layouts.len() as u8;
                    let (tag_id, (_, argument_layouts)) = sorted_tag_layouts
                        .iter()
                        .enumerate()
                        .find(|(_, (key, _))| key == &tag_name)
                        .expect("tag must be in its own type");

                    let mut arguments: Vec<Symbol> = Vec::with_capacity_in(args.len(), arena);
                    let tag_id_symbol = env.unique_symbol();
                    arguments.push(tag_id_symbol);

                    let expr_it = args
                        .into_iter()
                        .map(|(_, arg): (_, Located<roc_can::expr::Expr>)| {
                            let symbol = env.unique_symbol();
                            arguments.push(symbol);
                            with_hole(env, arg.value, procs, layout_cache, symbol)
                        })
                        .collect::<std::vec::Vec<_>>();

                    let layout_it = argument_layouts.iter();

                    let mut layouts: Vec<&'a [Layout<'a>]> =
                        Vec::with_capacity_in(sorted_tag_layouts.len(), env.arena);

                    for (_, arg_layouts) in sorted_tag_layouts.into_iter() {
                        layouts.push(arg_layouts);
                    }

                    let layout = Layout::Union(layouts.into_bump_slice());
                    let tag = Expr::Tag {
                        tag_layout: layout.clone(),
                        tag_name,
                        tag_id: tag_id as u8,
                        union_size,
                        arguments: arguments.into_bump_slice(),
                    };

                    Box::new(move |hole| {
                        let mut stmt = Stmt::Let(assigned, tag, layout, hole);

                        for expr in expr_it.into_iter().rev() {
                            stmt = expr(arena.alloc(stmt));
                        }

                        // define the tag id
                        stmt = Stmt::Let(
                            tag_id_symbol,
                            Expr::Literal(Literal::Int(tag_id as i64)),
                            Layout::Builtin(Builtin::Int64),
                            arena.alloc(stmt),
                        );

                        stmt
                    })
                }
            }
        }

        Record {
            record_var,
            mut fields,
            ..
        } => {
            let arena = env.arena;

            let sorted_fields = crate::layout::sort_record_fields(
                env.arena,
                record_var,
                env.subs,
                env.pointer_size,
            );

            /*
            let mut field_tuples = Vec::with_capacity_in(sorted_fields.len(), arena);

            for (label, layout) in sorted_fields {
                let field = fields.remove(&label).unwrap();
                let expr = from_can(env, field.loc_expr.value, procs, layout_cache);

                field_tuples.push((expr, layout));
            }

            let record = Expr::Struct(field_tuples.into_bump_slice());
            */

            let mut field_symbols = Vec::with_capacity_in(fields.len(), env.arena);
            let mut field_layouts = Vec::with_capacity_in(fields.len(), env.arena);

            let field_exprs = sorted_fields
                .into_iter()
                .map(|(label, layout)| {
                    let symbol = env.unique_symbol();
                    field_symbols.push(symbol);
                    field_layouts.push(layout);

                    let field = fields.remove(&label).unwrap();

                    with_hole(env, field.loc_expr.value, procs, layout_cache, symbol)
                })
                .collect::<std::vec::Vec<_>>();

            let layout = Layout::Struct(field_layouts.into_bump_slice());

            Box::new(move |hole| {
                let mut stmt = Stmt::Let(
                    assigned,
                    Expr::Struct(field_symbols.into_bump_slice()),
                    layout,
                    hole,
                );

                for expr in field_exprs.into_iter().rev() {
                    stmt = expr(arena.alloc(stmt));
                }

                stmt
            })
        }

        EmptyRecord => {
            Box::new(move |hole| Stmt::Let(assigned, Expr::Struct(&[]), Layout::Struct(&[]), hole))
        }

        When { .. } | If { .. } => todo!("when or if in expression requires join points"),

        List { .. } => todo!("list"),
        LetRec(_, _, _, _) | LetNonRec(_, _, _, _) => todo!("lets"),

        Access { .. } | Accessor { .. } | Update { .. } => todo!("record access/accessor/update"),

        Closure(_, _, _, _, _) => todo!("call"),
        Call(boxed, loc_args, _) => {
            let (fn_var, loc_expr, ret_var) = *boxed;

            // match from_can(env, loc_expr.value, procs, layout_cache) {
            match loc_expr.value {
                roc_can::expr::Expr::Var(proc_name) => Box::new(move |hole| {
                    call_by_name(
                        env,
                        procs,
                        fn_var,
                        ret_var,
                        proc_name,
                        loc_args,
                        layout_cache,
                        assigned,
                        hole,
                    )
                }),
                _ => {
                    let function_symbol = env.unique_symbol();
                    let ptr = with_hole(env, loc_expr.value, procs, layout_cache, function_symbol);
                    // Call by pointer - the closure was anonymous, e.g.
                    //
                    // ((\a -> a) 5)
                    //
                    // It might even be the anonymous result of a conditional:
                    //
                    // ((if x > 0 then \a -> a else \_ -> 0) 5)
                    //
                    // It could be named too:
                    //
                    // ((if x > 0 then foo else bar) 5)
                    let mut args = Vec::with_capacity_in(loc_args.len(), env.arena);
                    let mut arg_symbols = Vec::with_capacity_in(loc_args.len(), env.arena);

                    for (_, loc_arg) in loc_args {
                        let symbol = env.unique_symbol();
                        arg_symbols.push(symbol);
                        args.push(with_hole(env, loc_arg.value, procs, layout_cache, symbol));
                    }

                    let layout = layout_cache
                        .from_var(env.arena, fn_var, env.subs, env.pointer_size)
                        .unwrap_or_else(|err| {
                            panic!("TODO turn fn_var into a RuntimeError {:?}", err)
                        });

                    Box::new(move |mut hole| {
                        // Expr::CallByPointer(&*env.arena.alloc(ptr), args.into_bump_slice(), layout)
                        let mut result = Stmt::Let(
                            assigned,
                            Expr::FunctionCall {
                                call_type: CallType::ByPointer(function_symbol),
                                layout: layout.clone(),
                                args: arg_symbols.into_bump_slice(),
                            },
                            layout,
                            arena.alloc(hole),
                        );
                        result = ptr(arena.alloc(result));

                        for arg in args {
                            result = arg(arena.alloc(result));
                        }

                        result
                    })
                }
            }
        }
        RunLowLevel { .. } => todo!("run lowlevel"),
        RuntimeError(_) => todo!("runtime error"),
    }
}

pub fn from_can<'a>(
    env: &mut Env<'a, '_>,
    can_expr: roc_can::expr::Expr,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
) -> Stmt<'a> {
    use roc_can::expr::Expr::*;

    match can_expr {
        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => {
            let mut expr = from_can(env, final_else.value, procs, layout_cache);
            let arena = env.arena;

            let ret_layout = layout_cache
                .from_var(env.arena, branch_var, env.subs, env.pointer_size)
                .expect("invalid ret_layout");
            let cond_layout = layout_cache
                .from_var(env.arena, cond_var, env.subs, env.pointer_size)
                .expect("invalid cond_layout");

            for (loc_cond, loc_then) in branches.into_iter().rev() {
                let branching_symbol = env.unique_symbol();
                let cond = with_hole(env, loc_cond.value, procs, layout_cache, branching_symbol);
                let then = from_can(env, loc_then.value, procs, layout_cache);

                let cond_stmt = Stmt::Cond {
                    cond_symbol: branching_symbol,
                    branching_symbol,
                    cond_layout: cond_layout.clone(),
                    branching_layout: cond_layout.clone(),
                    pass: env.arena.alloc(then),
                    fail: env.arena.alloc(expr),
                    ret_layout: ret_layout.clone(),
                };

                expr = cond(env.arena.alloc(cond_stmt));
            }

            expr
        }
        When {
            cond_var,
            expr_var,
            region,
            loc_cond,
            branches,
        } => {
            let cond_symbol = if let roc_can::expr::Expr::Var(symbol) = loc_cond.value {
                symbol
            } else {
                env.unique_symbol()
            };

            let mono_when = from_can_when(
                env,
                cond_var,
                expr_var,
                region,
                cond_symbol,
                branches,
                layout_cache,
                procs,
            );

            let mono_cond = with_hole(env, loc_cond.value, procs, layout_cache, cond_symbol);

            mono_cond(env.arena.alloc(mono_when))
        }
        _ => {
            let symbol = env.unique_symbol();
            let mut holed = with_hole(env, can_expr, procs, layout_cache, symbol);

            holed(env.arena.alloc(Stmt::Ret(symbol)))
        }
    }
}

fn from_can_when<'a>(
    env: &mut Env<'a, '_>,
    cond_var: Variable,
    expr_var: Variable,
    region: Region,
    cond_symbol: Symbol,
    mut branches: std::vec::Vec<roc_can::expr::WhenBranch>,
    layout_cache: &mut LayoutCache<'a>,
    procs: &mut Procs<'a>,
) -> Stmt<'a> {
    if branches.is_empty() {
        // A when-expression with no branches is a runtime error.
        // We can't know what to return!
        Stmt::RuntimeError("Hit a 0-branch when expression")
    } else if branches.len() == 1 && branches[0].patterns.len() == 1 && branches[0].guard.is_none()
    {
        let first = branches.remove(0);
        // A when-expression with exactly 1 branch is essentially a LetNonRec.
        // As such, we can compile it direcly to a Store.
        let arena = env.arena;
        let mut stored = Vec::with_capacity_in(1, arena);

        let bound_symbols = first
            .patterns
            .iter()
            .map(|pat| roc_can::pattern::symbols_from_pattern(&pat.value))
            .flatten()
            .collect::<std::vec::Vec<_>>();

        let loc_when_pattern = &first.patterns[0];

        let mono_pattern =
            crate::expr::from_can_pattern(env, procs, layout_cache, &loc_when_pattern.value);

        // record pattern matches can have 1 branch and typecheck, but may still not be exhaustive
        let guard = if first.guard.is_some() {
            Guard::HasGuard
        } else {
            Guard::NoGuard
        };

        let context = crate::pattern::Context::BadCase;
        match crate::pattern::check(
            region,
            &[(
                Located::at(loc_when_pattern.region, mono_pattern.clone()),
                guard,
            )],
            context,
        ) {
            Ok(_) => {}
            Err(errors) => {
                for error in errors {
                    env.problems.push(MonoProblem::PatternProblem(error))
                }

                // panic!("generate runtime error, should probably also optimize this");
            }
        }

        let cond_layout = layout_cache
            .from_var(env.arena, cond_var, env.subs, env.pointer_size)
            .unwrap_or_else(|err| panic!("TODO turn this into a RuntimeError {:?}", err));

        // NOTE this will still store shadowed names.
        // that's fine: the branch throws a runtime error anyway
        let mut ret = match store_pattern(env, &mono_pattern, cond_symbol, cond_layout, &mut stored)
        {
            Ok(_) => from_can(env, first.value.value, procs, layout_cache),
            Err(message) => Stmt::RuntimeError(env.arena.alloc(message)),
        };

        for (symbol, layout, expr) in stored.iter().rev().cloned() {
            ret = Stmt::Let(symbol, expr, layout, env.arena.alloc(ret));
        }

        ret
    } else {
        let cond_layout = layout_cache
            .from_var(env.arena, cond_var, env.subs, env.pointer_size)
            .unwrap_or_else(|err| panic!("TODO turn this into a RuntimeError {:?}", err));

        let mut loc_branches = std::vec::Vec::new();
        let mut opt_branches = std::vec::Vec::new();

        for when_branch in branches {
            let mono_expr = from_can(env, when_branch.value.value, procs, layout_cache);

            let exhaustive_guard = if when_branch.guard.is_some() {
                Guard::HasGuard
            } else {
                Guard::NoGuard
            };

            for loc_pattern in when_branch.patterns {
                let mono_pattern =
                    crate::expr::from_can_pattern(env, procs, layout_cache, &loc_pattern.value);

                loc_branches.push((
                    Located::at(loc_pattern.region, mono_pattern.clone()),
                    exhaustive_guard.clone(),
                ));

                let mut stores = Vec::with_capacity_in(1, env.arena);

                let (mono_guard, stores, expr) = match store_pattern(
                    env,
                    &mono_pattern,
                    cond_symbol,
                    cond_layout.clone(),
                    &mut stores,
                ) {
                    Ok(_) => {
                        // if the branch is guarded, the guard can use variables bound in the
                        // pattern. They must be available, so we give the stores to the
                        // decision_tree. A branch with guard can only be entered with the guard
                        // evaluated, so variables will also be loaded in the branch's body expr.
                        //
                        // otherwise, we modify the branch's expression to include the stores
                        if let Some(loc_guard) = when_branch.guard.clone() {
                            let expr = from_can(env, loc_guard.value, procs, layout_cache);
                            (
                                crate::decision_tree2::Guard::Guard {
                                    stores: stores.into_bump_slice(),
                                    expr,
                                },
                                &[] as &[_],
                                mono_expr.clone(),
                            )
                        } else {
                            (
                                crate::decision_tree2::Guard::NoGuard,
                                stores.into_bump_slice(),
                                mono_expr.clone(),
                            )
                        }
                    }
                    Err(message) => {
                        // when the pattern is invalid, a guard must give a runtime error too
                        if when_branch.guard.is_some() {
                            (
                                crate::decision_tree2::Guard::Guard {
                                    stores: &[],
                                    expr: Stmt::RuntimeError(env.arena.alloc(message)),
                                },
                                &[] as &[_],
                                // we can never hit this
                                Stmt::RuntimeError(&"invalid pattern with guard: unreachable"),
                            )
                        } else {
                            (
                                crate::decision_tree2::Guard::NoGuard,
                                &[] as &[_],
                                Stmt::RuntimeError(env.arena.alloc(message)),
                            )
                        }
                    }
                };

                opt_branches.push((mono_pattern, mono_guard, stores, expr));
            }
        }

        let context = crate::pattern::Context::BadCase;
        match crate::pattern::check(region, &loc_branches, context) {
            Ok(_) => {}
            Err(errors) => {
                use crate::pattern::Error::*;
                let mut is_not_exhaustive = false;
                let mut overlapping_branches = std::vec::Vec::new();

                for error in errors {
                    match &error {
                        Incomplete(_, _, _) => {
                            is_not_exhaustive = true;
                        }
                        Redundant { index, .. } => {
                            overlapping_branches.push(index.to_zero_based());
                        }
                    }
                    env.problems.push(MonoProblem::PatternProblem(error))
                }

                overlapping_branches.sort();

                for i in overlapping_branches.into_iter().rev() {
                    opt_branches.remove(i);
                }

                if is_not_exhaustive {
                    opt_branches.push((
                        Pattern::Underscore,
                        crate::decision_tree2::Guard::NoGuard,
                        &[],
                        Stmt::RuntimeError("non-exhaustive pattern match"),
                    ));
                }
            }
        }

        let ret_layout = layout_cache
            .from_var(env.arena, expr_var, env.subs, env.pointer_size)
            .unwrap_or_else(|err| panic!("TODO turn this into a RuntimeError {:?}", err));

        crate::decision_tree2::optimize_when(
            env,
            cond_symbol,
            cond_layout.clone(),
            ret_layout,
            opt_branches,
        )
    }
}

fn store_pattern<'a>(
    env: &mut Env<'a, '_>,
    can_pat: &Pattern<'a>,
    outer_symbol: Symbol,
    layout: Layout<'a>,
    stored: &mut Vec<'a, (Symbol, Layout<'a>, Expr<'a>)>,
) -> Result<(), String> {
    use Pattern::*;

    match can_pat {
        Identifier(symbol) => {
            // let load = Expr::Load(outer_symbol);
            // stored.push((*symbol, layout, load))
            todo!()
        }
        Underscore => {
            // Since _ is never read, it's safe to reassign it.
            // stored.push((Symbol::UNDERSCORE, layout, Expr::Load(outer_symbol)))
        }
        IntLiteral(_) | FloatLiteral(_) | EnumLiteral { .. } | BitLiteral { .. } => {}
        AppliedTag {
            union, arguments, ..
        } => {
            let is_unwrapped = union.alternatives.len() == 1;

            let mut arg_layouts = Vec::with_capacity_in(arguments.len(), env.arena);

            if !is_unwrapped {
                // add an element for the tag discriminant
                arg_layouts.push(Layout::Builtin(Builtin::Int64));
            }

            for (_, layout) in arguments {
                arg_layouts.push(layout.clone());
            }

            for (index, (argument, arg_layout)) in arguments.iter().enumerate() {
                let load = Expr::AccessAtIndex {
                    is_unwrapped,
                    index: (!is_unwrapped as usize + index) as u64,
                    field_layouts: arg_layouts.clone().into_bump_slice(),
                    structure: outer_symbol,
                };
                match argument {
                    Identifier(symbol) => {
                        // store immediately in the given symbol
                        stored.push((*symbol, arg_layout.clone(), load));
                    }
                    Underscore => {
                        // ignore
                    }
                    IntLiteral(_) | FloatLiteral(_) | EnumLiteral { .. } | BitLiteral { .. } => {}
                    _ => {
                        // store the field in a symbol, and continue matching on it
                        let symbol = env.unique_symbol();
                        stored.push((symbol, arg_layout.clone(), load));

                        store_pattern(env, argument, symbol, arg_layout.clone(), stored)?;
                    }
                }
            }
        }
        RecordDestructure(destructs, Layout::Struct(sorted_fields)) => {
            for (index, destruct) in destructs.iter().enumerate() {
                store_record_destruct(
                    env,
                    destruct,
                    index as u64,
                    outer_symbol,
                    sorted_fields,
                    stored,
                )?;
            }
        }

        Shadowed(region, ident) => {
            return Err(format!(
                "The pattern at {:?} shadows variable {:?}",
                region, ident
            ));
        }
        _ => {
            panic!("TODO store_pattern for {:?}", can_pat);
        }
    }

    Ok(())
}

fn store_record_destruct<'a>(
    env: &mut Env<'a, '_>,
    destruct: &RecordDestruct<'a>,
    index: u64,
    outer_symbol: Symbol,
    sorted_fields: &'a [Layout<'a>],
    stored: &mut Vec<'a, (Symbol, Layout<'a>, Expr<'a>)>,
) -> Result<(), String> {
    use crate::expr::DestructType;
    use Pattern::*;

    let load = Expr::AccessAtIndex {
        index,
        field_layouts: sorted_fields,
        structure: outer_symbol,
        is_unwrapped: true,
    };

    match &destruct.typ {
        DestructType::Required => {
            stored.push((destruct.symbol, destruct.layout.clone(), load));
        }
        DestructType::Optional(_expr) => {
            todo!("TODO monomorphize optional field destructure's default expr");
        }
        DestructType::Guard(guard_pattern) => match &guard_pattern {
            Identifier(symbol) => {
                stored.push((*symbol, destruct.layout.clone(), load));
            }
            Underscore => {
                // important that this is special-cased to do nothing: mono record patterns will extract all the
                // fields, but those not bound in the source code are guarded with the underscore
                // pattern. So given some record `{ x : a, y : b }`, a match
                //
                // { x } -> ...
                //
                // is actually
                //
                // { x, y: _ } -> ...
                //
                // internally. But `y` is never used, so we must make sure it't not stored/loaded.
            }
            IntLiteral(_) | FloatLiteral(_) | EnumLiteral { .. } | BitLiteral { .. } => {}
            _ => {
                let symbol = env.unique_symbol();
                stored.push((symbol, destruct.layout.clone(), load));

                store_pattern(env, guard_pattern, symbol, destruct.layout.clone(), stored)?;
            }
        },
    }

    Ok(())
}

fn call_by_name<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    fn_var: Variable,
    ret_var: Variable,
    proc_name: Symbol,
    loc_args: std::vec::Vec<(Variable, Located<roc_can::expr::Expr>)>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    // Register a pending_specialization for this function
    match layout_cache.from_var(env.arena, fn_var, env.subs, env.pointer_size) {
        Ok(layout) => {
            // Build the CallByName node
            let arena = env.arena;
            let mut args = Vec::with_capacity_in(loc_args.len(), arena);
            let mut pattern_vars = Vec::with_capacity_in(loc_args.len(), arena);

            for (var, loc_arg) in loc_args {
                match layout_cache.from_var(&env.arena, var, &env.subs, env.pointer_size) {
                    Ok(layout) => {
                        pattern_vars.push(var);
                        args.push((from_can(env, loc_arg.value, procs, layout_cache), layout));
                    }
                    Err(_) => {
                        // One of this function's arguments code gens to a runtime error,
                        // so attempting to call it will immediately crash.
                        return Stmt::RuntimeError("TODO runtime error for invalid layout");
                    }
                }
            }

            // If we've already specialized this one, no further work is needed.
            if procs.specialized.contains_key(&(proc_name, layout.clone())) {
                let call = Expr::FunctionCall {
                    call_type: CallType::ByName(proc_name),
                    layout,
                    args: &[],
                };

                Stmt::Let(assigned, call, layout, hole)
            } else {
                let pending = PendingSpecialization {
                    pattern_vars,
                    ret_var,
                    fn_var,
                };

                // When requested (that is, when procs.pending_specializations is `Some`),
                // store a pending specialization rather than specializing immediately.
                //
                // We do this so that we can do specialization in two passes: first,
                // build the mono_expr with all the specialized calls in place (but
                // no specializations performed yet), and then second, *after*
                // de-duplicating requested specializations (since multiple modules
                // which could be getting monomorphized in parallel might request
                // the same specialization independently), we work through the
                // queue of pending specializations to complete each specialization
                // exactly once.
                match &mut procs.pending_specializations {
                    Some(pending_specializations) => {
                        // register the pending specialization, so this gets code genned later
                        add_pending(pending_specializations, proc_name, layout.clone(), pending);

                        let call = Expr::FunctionCall {
                            call_type: CallType::ByName(proc_name),
                            layout,
                            args: &[],
                        };

                        Stmt::Let(assigned, call, layout, hole)
                    }
                    None => {
                        let opt_partial_proc = procs.partial_procs.get(&proc_name);

                        match opt_partial_proc {
                            Some(partial_proc) => {
                                // TODO should pending_procs hold a Rc<Proc> to avoid this .clone()?
                                let partial_proc = partial_proc.clone();

                                // Mark this proc as in-progress, so if we're dealing with
                                // mutually recursive functions, we don't loop forever.
                                // (We had a bug around this before this system existed!)
                                procs
                                    .specialized
                                    .insert((proc_name, layout.clone()), InProgress);

                                match specialize(
                                    env,
                                    procs,
                                    proc_name,
                                    layout_cache,
                                    pending,
                                    partial_proc,
                                ) {
                                    Ok(proc) => {
                                        procs
                                            .specialized
                                            .insert((proc_name, layout.clone()), Done(proc));

                                        let call = Expr::FunctionCall {
                                            call_type: CallType::ByName(proc_name),
                                            layout,
                                            args: &[],
                                        };

                                        Stmt::Let(assigned, call, layout, hole)
                                    }
                                    Err(error) => {
                                        let error_msg = env.arena.alloc(format!(
                                            "TODO generate a RuntimeError message for {:?}",
                                            error
                                        ));

                                        procs.runtime_errors.insert(proc_name, error_msg);

                                        Stmt::RuntimeError(error_msg)
                                    }
                                }
                            }

                            None => {
                                // This must have been a runtime error.
                                let error = procs.runtime_errors.get(&proc_name).unwrap();

                                Stmt::RuntimeError(error)
                            }
                        }
                    }
                }
            }
        }
        Err(_) => {
            // This function code gens to a runtime error,
            // so attempting to call it will immediately crash.
            Stmt::RuntimeError("")
        }
    }
}
