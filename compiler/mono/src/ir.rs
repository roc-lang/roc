use self::InProgressProc::*;
use crate::exhaustive::{Ctor, Guard, RenderAs, TagId};
use crate::layout::{
    list_layout_from_elem, Builtin, Layout, LayoutCache, LayoutProblem, Ownership,
};
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

#[derive(Clone, Debug)]
pub enum MonoProblem {
    PatternProblem(crate::exhaustive::Error),
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
    pub body: Stmt<'a>,
    pub closes_over: Layout<'a>,
    pub ret_layout: Layout<'a>,
}

impl<'a> Proc<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D, _parens: bool) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        let args_doc = self
            .args
            .iter()
            .map(|(_, symbol)| alloc.text(format!("{}", symbol)));

        alloc
            .text(format!("procedure {} (", self.name))
            .append(alloc.intersperse(args_doc, ", "))
            .append("):")
            .append(alloc.hardline())
            .append(self.body.to_doc(alloc, false).indent(4))
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

#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
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
        arguments: &'a [(Symbol, Layout<'a>)],
        /// does not contain jumps to this id
        continuation: &'a Stmt<'a>,
        /// contains the jumps to this id
        remainder: &'a Stmt<'a>,
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

    /// A symbol will alias this symbol
    /// in the long term we should get rid of this using copy propagation
    Alias(Symbol),

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

fn join_point_to_doc<'b, D, A>(alloc: &'b D, symbol: JoinPointId) -> DocBuilder<'b, D, A>
where
    D: DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    alloc.text(format!("{}", symbol.0))
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
            Alias(symbol) => alloc.text("alias ").append(symbol_to_doc(alloc, *symbol)),

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
    pub fn new(
        env: &mut Env<'a, '_>,
        can_expr: roc_can::expr::Expr,
        procs: &mut Procs<'a>,
    ) -> Self {
        let mut layout_cache = LayoutCache::default();

        from_can(env, can_expr, procs, &mut layout_cache)
    }
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

            Join {
                id,
                arguments,
                continuation,
                remainder,
            } => {
                let it = arguments.iter().map(|(s, _)| symbol_to_doc(alloc, *s));

                alloc.intersperse(
                    vec![
                        remainder.to_doc(alloc, false),
                        alloc
                            .text("joinpoint ")
                            .append(join_point_to_doc(alloc, *id))
                            .append(" ".repeat(arguments.len().min(1)))
                            .append(alloc.intersperse(it, alloc.space()))
                            .append(":"),
                        continuation.to_doc(alloc, false).indent(4),
                    ],
                    alloc.hardline(),
                )
            }
            Jump(id, arguments) => {
                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s));

                alloc
                    .text("jump ")
                    .append(join_point_to_doc(alloc, *id))
                    .append(" ".repeat(arguments.len().min(1)))
                    .append(alloc.intersperse(it, alloc.space()))
                    .append(";")
            }

            _ => todo!(),
        }
        /*
        Inc(Symbol, &'a Stmt<'a>),
        Dec(Symbol, &'a Stmt<'a>),
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

/// turn record/tag patterns into a when expression, e.g.
///
/// foo = \{ x } -> body
///
/// becomes
///
/// foo = \r -> when r is { x } -> body
///
/// conversion of one-pattern when expressions will do the most optimal thing
#[allow(clippy::type_complexity)]
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
    let mut arg_vars = Vec::with_capacity_in(patterns.len(), env.arena);
    let mut symbols = Vec::with_capacity_in(patterns.len(), env.arena);
    let mut body = Ok(body);

    // patterns that are not yet in a when (e.g. in let or function arguments) must be irrefutable
    // to pass type checking. So the order in which we add them to the body does not matter: there
    // are only stores anyway, no branches.
    for (pattern_var, pattern) in patterns.into_iter() {
        let context = crate::exhaustive::Context::BadArg;
        let mono_pattern = from_can_pattern(env, layout_cache, &pattern.value);

        match crate::exhaustive::check(
            pattern.region,
            &[(
                Located::at(pattern.region, mono_pattern.clone()),
                crate::exhaustive::Guard::NoGuard,
            )],
            context,
        ) {
            Ok(_) => {
                // Replace the body with a new one, but only if it was Ok.
                if let Ok(unwrapped_body) = body {
                    let (new_symbol, new_body) =
                        pattern_to_when(env, pattern_var, pattern, body_var, unwrapped_body);

                    symbols.push(new_symbol);
                    arg_vars.push(pattern_var);

                    body = Ok(new_body)
                }
            }
            Err(errors) => {
                for error in errors {
                    env.problems.push(MonoProblem::PatternProblem(error))
                }

                let value = RuntimeError::UnsupportedPattern(pattern.region);

                // Even if the body was Ok, replace it with this Err.
                // If it was already an Err, leave it at that Err, so the first
                // RuntimeError we encountered remains the first.
                body = body.and_then(|_| {
                    Err(Located {
                        region: pattern.region,
                        value,
                    })
                });
            }
        }
    }

    match body {
        Ok(body) => Ok((arg_vars, symbols, body)),
        Err(loc_error) => Err(loc_error),
    }
}

/// turn irrefutable patterns into when. For example
///
/// foo = \{ x } -> body
///
/// Assuming the above program typechecks, the pattern match cannot fail
/// (it is irrefutable). It becomes
///
/// foo = \r ->
///      when r is
///          { x } -> body
///
/// conversion of one-pattern when expressions will do the most optimal thing
fn pattern_to_when<'a>(
    env: &mut Env<'a, '_>,
    pattern_var: Variable,
    pattern: Located<roc_can::pattern::Pattern>,
    body_var: Variable,
    body: Located<roc_can::expr::Expr>,
) -> (Symbol, Located<roc_can::expr::Expr>) {
    use roc_can::expr::Expr::*;
    use roc_can::expr::WhenBranch;
    use roc_can::pattern::Pattern::*;

    match &pattern.value {
        Identifier(symbol) => (*symbol, body),
        Underscore => {
            // for underscore we generate a dummy Symbol
            (env.unique_symbol(), body)
        }
        Shadowed(region, loc_ident) => {
            let error = roc_problem::can::RuntimeError::Shadowing {
                original_region: *region,
                shadow: loc_ident.clone(),
            };
            (env.unique_symbol(), Located::at_zero(RuntimeError(error)))
        }

        UnsupportedPattern(region) => {
            // create the runtime error here, instead of delegating to When.
            // UnsupportedPattern should then never occcur in When
            let error = roc_problem::can::RuntimeError::UnsupportedPattern(*region);
            (env.unique_symbol(), Located::at_zero(RuntimeError(error)))
        }

        MalformedPattern(problem, region) => {
            // create the runtime error here, instead of delegating to When.
            let error = roc_problem::can::RuntimeError::MalformedPattern(*problem, *region);
            (env.unique_symbol(), Located::at_zero(RuntimeError(error)))
        }

        AppliedTag { .. } | RecordDestructure { .. } => {
            let symbol = env.unique_symbol();

            let wrapped_body = When {
                cond_var: pattern_var,
                expr_var: body_var,
                region: Region::zero(),
                loc_cond: Box::new(Located::at_zero(Var(symbol))),
                branches: vec![WhenBranch {
                    patterns: vec![pattern],
                    value: body,
                    guard: None,
                }],
            };

            (symbol, Located::at_zero(wrapped_body))
        }

        IntLiteral(_) | NumLiteral(_, _) | FloatLiteral(_) | StrLiteral(_) => {
            // These patters are refutable, and thus should never occur outside a `when` expression
            // They should have been replaced with `UnsupportedPattern` during canonicalization
            unreachable!("refutable pattern {:?} where irrefutable pattern is expected. This should never happen!", pattern.value)
        }
    }
}

pub fn specialize_all<'a>(
    env: &mut Env<'a, '_>,
    mut procs: Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
) -> Procs<'a> {
    let mut pending_specializations = procs.pending_specializations.unwrap_or_default();

    // When calling from_can, pending_specializations should be unavailable.
    // This must be a single pass, and we must not add any more entries to it!
    procs.pending_specializations = None;

    for (name, mut by_layout) in pending_specializations.drain() {
        // Use the function's symbol's home module as the home module
        // when doing canonicalization. This will be important to determine
        // whether or not it's safe to defer specialization.
        env.home = name.module_id();

        for (layout, pending) in by_layout.drain() {
            // If we've already seen this (Symbol, Layout) combination before,
            // don't try to specialize it again. If we do, we'll loop forever!
            //
            // NOTE: this #[allow(clippy::map_entry)] here is for correctness!
            // Changing it to use .entry() would necessarily make it incorrect.
            #[allow(clippy::map_entry)]
            if !procs.specialized.contains_key(&(name, layout.clone())) {
                // TODO should pending_procs hold a Rc<Proc>?
                let partial_proc = procs
                    .partial_procs
                    .get(&name)
                    .unwrap_or_else(|| panic!("Could not find partial_proc for {:?}", name))
                    .clone();

                // Mark this proc as in-progress, so if we're dealing with
                // mutually recursive functions, we don't loop forever.
                // (We had a bug around this before this system existed!)
                procs.specialized.insert((name, layout.clone()), InProgress);

                match specialize(env, &mut procs, name, layout_cache, pending, partial_proc) {
                    Ok(proc) => {
                        procs.specialized.insert((name, layout), Done(proc));
                    }
                    Err(error) => {
                        let error_msg = env.arena.alloc(format!(
                            "TODO generate a RuntimeError message for {:?}",
                            error
                        ));

                        procs.runtime_errors.insert(name, error_msg);
                    }
                }
            }
        }
    }

    procs
}

fn specialize<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    proc_name: Symbol,
    layout_cache: &mut LayoutCache<'a>,
    pending: PendingSpecialization<'a>,
    partial_proc: PartialProc<'a>,
) -> Result<Proc<'a>, LayoutProblem> {
    let PendingSpecialization {
        ret_var,
        fn_var,
        pattern_vars,
    } = pending;

    let PartialProc {
        annotation,
        pattern_symbols,
        body,
    } = partial_proc;

    // unify the called function with the specialized signature, then specialize the function body
    let snapshot = env.subs.snapshot();
    let unified = roc_unify::unify::unify(env.subs, annotation, fn_var);

    debug_assert!(matches!(unified, roc_unify::unify::Unified::Success(_)));

    let ret_symbol = env.unique_symbol();
    let hole = env.arena.alloc(Stmt::Ret(ret_symbol));
    let specialized_body = with_hole(env, body, procs, layout_cache, ret_symbol, hole);

    // reset subs, so we don't get type errors when specializing for a different signature
    env.subs.rollback_to(snapshot);

    let mut proc_args = Vec::with_capacity_in(pattern_vars.len(), &env.arena);

    debug_assert_eq!(
        &pattern_vars.len(),
        &pattern_symbols.len(),
        "Tried to zip two vecs with different lengths!"
    );

    for (arg_var, arg_name) in pattern_vars.iter().zip(pattern_symbols.iter()) {
        let layout = layout_cache.from_var(&env.arena, *arg_var, env.subs, env.pointer_size)?;

        proc_args.push((layout, *arg_name));
    }

    let ret_layout = layout_cache
        .from_var(&env.arena, ret_var, env.subs, env.pointer_size)
        .unwrap_or_else(|err| panic!("TODO handle invalid function {:?}", err));

    // TODO WRONG
    let closes_over_layout = Layout::Struct(&[]);

    let proc = Proc {
        name: proc_name,
        args: proc_args.into_bump_slice(),
        body: specialized_body,
        closes_over: closes_over_layout,
        ret_layout,
    };

    Ok(proc)
}

pub fn with_hole<'a>(
    env: &mut Env<'a, '_>,
    can_expr: roc_can::expr::Expr,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    use roc_can::expr::Expr::*;

    let arena = env.arena;

    match can_expr {
        Int(_, num) => Stmt::Let(
            assigned,
            Expr::Literal(Literal::Int(num)),
            Layout::Builtin(Builtin::Int64),
            hole,
        ),

        Float(_, num) => Stmt::Let(
            assigned,
            Expr::Literal(Literal::Float(num)),
            Layout::Builtin(Builtin::Float64),
            hole,
        ),

        Str(string) | BlockStr(string) => Stmt::Let(
            assigned,
            Expr::Literal(Literal::Str(arena.alloc(string))),
            Layout::Builtin(Builtin::Str),
            hole,
        ),

        Num(var, num) => match num_argument_to_int_or_float(env.subs, var) {
            IntOrFloat::IntType => Stmt::Let(
                assigned,
                Expr::Literal(Literal::Int(num)),
                Layout::Builtin(Builtin::Int64),
                hole,
            ),
            IntOrFloat::FloatType => Stmt::Let(
                assigned,
                Expr::Literal(Literal::Float(num as f64)),
                Layout::Builtin(Builtin::Float64),
                hole,
            ),
        },
        LetNonRec(def, cont, _, _) => {
            // WRONG! this is introduces new control flow, and should call `from_can` again
            if let roc_can::pattern::Pattern::Identifier(symbol) = def.loc_pattern.value {
                let stmt = with_hole(env, cont.value, procs, layout_cache, assigned, hole);
                let expr = with_hole(
                    env,
                    def.loc_expr.value,
                    procs,
                    layout_cache,
                    symbol,
                    env.arena.alloc(stmt),
                );

                expr
            } else {
                todo!()
            }
        }
        Var(symbol) => {
            if procs.module_thunks.contains(&symbol) {
                let partial_proc = procs.partial_procs.get(&symbol).unwrap();
                let fn_var = partial_proc.annotation;
                let ret_var = fn_var; // These are the same for a thunk.

                // This is a top-level declaration, which will code gen to a 0-arity thunk.
                let result = call_by_name(
                    env,
                    procs,
                    fn_var,
                    ret_var,
                    symbol,
                    std::vec::Vec::new(),
                    layout_cache,
                    assigned,
                    env.arena.alloc(Stmt::Ret(assigned)),
                );

                return result;
            }

            // A bit ugly, but it does the job
            match hole {
                Stmt::Jump(id, _) => Stmt::Jump(*id, env.arena.alloc([symbol])),
                Stmt::Ret(s) => {
                    //
                    Stmt::Ret(symbol)
                }
                _ => {
                    // if you see this, there is variable aliasing going on
                    Stmt::Ret(symbol)
                }
            }
        }
        // Var(symbol) => panic!("reached Var {}", symbol),
        // assigned,
        // Stmt::Ret(symbol),
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
                Unit => Stmt::Let(assigned, Expr::Struct(&[]), Layout::Struct(&[]), hole),
                BoolUnion { ttrue, .. } => Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Bool(tag_name == ttrue)),
                    Layout::Builtin(Builtin::Int1),
                    hole,
                ),
                ByteUnion(tag_names) => {
                    let tag_id = tag_names
                        .iter()
                        .position(|key| key == &tag_name)
                        .expect("tag must be in its own type");

                    Stmt::Let(
                        assigned,
                        Expr::Literal(Literal::Byte(tag_id as u8)),
                        Layout::Builtin(Builtin::Int8),
                        hole,
                    )
                }

                Unwrapped(field_layouts) => {
                    let mut field_symbols = Vec::with_capacity_in(field_layouts.len(), env.arena);

                    for (_, arg) in args.iter() {
                        if let roc_can::expr::Expr::Var(symbol) = arg.value {
                            field_symbols.push(symbol);
                        } else {
                            field_symbols.push(env.unique_symbol());
                        }
                    }

                    // Layout will unpack this unwrapped tack if it only has one (non-zero-sized) field
                    let layout = layout_cache
                        .from_var(env.arena, variant_var, env.subs, env.pointer_size)
                        .unwrap_or_else(|err| {
                            panic!("TODO turn fn_var into a RuntimeError {:?}", err)
                        });

                    // even though this was originally a Tag, we treat it as a Struct from now on
                    let mut stmt = Stmt::Let(
                        assigned,
                        Expr::Struct(field_symbols.clone().into_bump_slice()),
                        layout,
                        hole,
                    );

                    for ((_, arg), symbol) in args.into_iter().rev().zip(field_symbols.iter().rev())
                    {
                        // if this argument is already a symbol, we don't need to re-define it
                        if let roc_can::expr::Expr::Var(_) = arg.value {
                            continue;
                        }
                        stmt = with_hole(
                            env,
                            arg.value,
                            procs,
                            layout_cache,
                            *symbol,
                            env.arena.alloc(stmt),
                        );
                    }

                    stmt
                }
                Wrapped(sorted_tag_layouts) => {
                    let union_size = sorted_tag_layouts.len() as u8;
                    let (tag_id, (_, argument_layouts)) = sorted_tag_layouts
                        .iter()
                        .enumerate()
                        .find(|(_, (key, _))| key == &tag_name)
                        .expect("tag must be in its own type");

                    let mut field_symbols: Vec<Symbol> = Vec::with_capacity_in(args.len(), arena);
                    let tag_id_symbol = env.unique_symbol();
                    field_symbols.push(tag_id_symbol);

                    for (_, arg) in args.iter() {
                        if let roc_can::expr::Expr::Var(symbol) = arg.value {
                            field_symbols.push(symbol);
                        } else {
                            field_symbols.push(env.unique_symbol());
                        }
                    }

                    let layout_it = argument_layouts.iter();

                    let mut layouts: Vec<&'a [Layout<'a>]> =
                        Vec::with_capacity_in(sorted_tag_layouts.len(), env.arena);

                    for (_, arg_layouts) in sorted_tag_layouts.into_iter() {
                        layouts.push(arg_layouts);
                    }

                    let field_symbols = field_symbols.into_bump_slice();
                    let layout = Layout::Union(layouts.into_bump_slice());
                    let tag = Expr::Tag {
                        tag_layout: layout.clone(),
                        tag_name,
                        tag_id: tag_id as u8,
                        union_size,
                        arguments: field_symbols,
                    };

                    let mut stmt = Stmt::Let(assigned, tag, layout, hole);

                    for ((_, arg), symbol) in args.into_iter().rev().zip(field_symbols.iter().rev())
                    {
                        // if this argument is already a symbol, we don't need to re-define it
                        if let roc_can::expr::Expr::Var(_) = arg.value {
                            continue;
                        }

                        stmt = with_hole(
                            env,
                            arg.value,
                            procs,
                            layout_cache,
                            *symbol,
                            env.arena.alloc(stmt),
                        );
                    }

                    // define the tag id
                    stmt = Stmt::Let(
                        tag_id_symbol,
                        Expr::Literal(Literal::Int(tag_id as i64)),
                        Layout::Builtin(Builtin::Int64),
                        arena.alloc(stmt),
                    );

                    stmt
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

            let mut field_symbols = Vec::with_capacity_in(fields.len(), env.arena);
            let mut field_layouts = Vec::with_capacity_in(fields.len(), env.arena);
            let mut can_fields = Vec::with_capacity_in(fields.len(), env.arena);

            for (label, layout) in sorted_fields.into_iter() {
                field_layouts.push(layout);

                let field = fields.remove(&label).unwrap();
                let field_symbol = if let roc_can::expr::Expr::Var(symbol) = field.loc_expr.value {
                    field_symbols.push(symbol);
                    can_fields.push(None);
                } else {
                    field_symbols.push(env.unique_symbol());
                    can_fields.push(Some(field));
                };
            }

            // creating a record from the var will unpack it if it's just a single field.
            let layout = layout_cache
                .from_var(env.arena, record_var, env.subs, env.pointer_size)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

            let field_symbols = field_symbols.into_bump_slice();
            let mut stmt = Stmt::Let(assigned, Expr::Struct(field_symbols), layout, hole);

            for (opt_field, symbol) in can_fields.into_iter().rev().zip(field_symbols.iter().rev())
            {
                if let Some(field) = opt_field {
                    stmt = with_hole(
                        env,
                        field.loc_expr.value,
                        procs,
                        layout_cache,
                        *symbol,
                        env.arena.alloc(stmt),
                    );
                }
            }

            stmt
        }

        EmptyRecord => Stmt::Let(assigned, Expr::Struct(&[]), Layout::Struct(&[]), hole),

        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => {
            let arena = env.arena;

            let ret_layout = layout_cache
                .from_var(env.arena, branch_var, env.subs, env.pointer_size)
                .expect("invalid ret_layout");
            let cond_layout = layout_cache
                .from_var(env.arena, cond_var, env.subs, env.pointer_size)
                .expect("invalid cond_layout");

            let id = JoinPointId(env.unique_symbol());
            let jump = env.arena.alloc(Stmt::Jump(id, env.arena.alloc([assigned])));

            let mut stmt = with_hole(env, final_else.value, procs, layout_cache, assigned, jump);

            for (loc_cond, loc_then) in branches.into_iter().rev() {
                let branching_symbol = env.unique_symbol();
                let then = with_hole(env, loc_then.value, procs, layout_cache, assigned, jump);

                stmt = Stmt::Cond {
                    cond_symbol: branching_symbol,
                    branching_symbol,
                    cond_layout: cond_layout.clone(),
                    branching_layout: cond_layout.clone(),
                    pass: env.arena.alloc(then),
                    fail: env.arena.alloc(stmt),
                    ret_layout: ret_layout.clone(),
                };

                // add condition
                stmt = with_hole(
                    env,
                    loc_cond.value,
                    procs,
                    layout_cache,
                    branching_symbol,
                    env.arena.alloc(stmt),
                );
            }

            let layout = layout_cache
                .from_var(env.arena, branch_var, env.subs, env.pointer_size)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

            Stmt::Join {
                id,
                arguments: env.arena.alloc([(assigned, layout)]),
                remainder: env.arena.alloc(stmt),
                continuation: hole,
            }
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

            let id = JoinPointId(env.unique_symbol());

            let mut stmt = from_can_when(
                env,
                cond_var,
                expr_var,
                region,
                cond_symbol,
                branches,
                layout_cache,
                procs,
                Some(id),
            );

            // define the `when` condition
            if let roc_can::expr::Expr::Var(_) = loc_cond.value {
                // do nothing
            } else {
                stmt = with_hole(
                    env,
                    loc_cond.value,
                    procs,
                    layout_cache,
                    cond_symbol,
                    env.arena.alloc(stmt),
                );
            };

            let layout = layout_cache
                .from_var(env.arena, expr_var, env.subs, env.pointer_size)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

            Stmt::Join {
                id,
                arguments: bumpalo::vec![in env.arena; (assigned, layout)].into_bump_slice(),
                remainder: env.arena.alloc(stmt),
                continuation: env.arena.alloc(hole),
            }
        }

        List { loc_elems, .. } if loc_elems.is_empty() => {
            // because an empty list has an unknown element type, it is handled differently
            let expr = Expr::EmptyArray;
            Stmt::Let(assigned, expr, Layout::Builtin(Builtin::EmptyList), hole)
        }

        List {
            elem_var,
            loc_elems,
        } => {
            let mut arg_symbols = Vec::with_capacity_in(loc_elems.len(), env.arena);
            for arg_expr in loc_elems.iter() {
                if let roc_can::expr::Expr::Var(symbol) = arg_expr.value {
                    arg_symbols.push(symbol);
                } else {
                    arg_symbols.push(env.unique_symbol());
                }
            }
            let arg_symbols = arg_symbols.into_bump_slice();

            let elem_layout = layout_cache
                .from_var(env.arena, elem_var, env.subs, env.pointer_size)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

            let expr = Expr::Array {
                elem_layout: elem_layout.clone(),
                elems: arg_symbols,
            };
            let mut stmt = Stmt::Let(
                assigned,
                expr,
                Layout::Builtin(Builtin::List(
                    Ownership::Owned,
                    env.arena.alloc(elem_layout),
                )),
                hole,
            );

            for (arg_expr, symbol) in loc_elems.into_iter().rev().zip(arg_symbols.iter().rev()) {
                // if this argument is already a symbol, we don't need to re-define it
                if let roc_can::expr::Expr::Var(_) = arg_expr.value {
                    continue;
                }

                stmt = with_hole(
                    env,
                    arg_expr.value,
                    procs,
                    layout_cache,
                    *symbol,
                    env.arena.alloc(stmt),
                );
            }

            stmt
        }
        LetRec(_, _, _, _) | LetNonRec(_, _, _, _) => todo!("lets"),

        Access {
            record_var,
            field_var,
            field,
            loc_expr,
            ..
        } => {
            let arena = env.arena;

            let sorted_fields = crate::layout::sort_record_fields(
                env.arena,
                record_var,
                env.subs,
                env.pointer_size,
            );

            let mut index = None;
            let mut field_layouts = Vec::with_capacity_in(sorted_fields.len(), env.arena);

            for (current, (label, field_layout)) in sorted_fields.into_iter().enumerate() {
                field_layouts.push(field_layout);

                if label == field {
                    index = Some(current);
                }
            }

            let record_symbol = if let roc_can::expr::Expr::Var(symbol) = loc_expr.value {
                symbol
            } else {
                env.unique_symbol()
            };

            let expr = Expr::AccessAtIndex {
                index: index.expect("field not in its own type") as u64,
                field_layouts: field_layouts.into_bump_slice(),
                structure: record_symbol,
                is_unwrapped: true,
            };

            let layout = layout_cache
                .from_var(env.arena, field_var, env.subs, env.pointer_size)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

            let mut stmt = Stmt::Let(assigned, expr, layout, hole);

            if let roc_can::expr::Expr::Var(symbol) = loc_expr.value {
                // do nothing
            } else {
                stmt = with_hole(
                    env,
                    loc_expr.value,
                    procs,
                    layout_cache,
                    record_symbol,
                    env.arena.alloc(stmt),
                );
            };

            stmt
        }

        Accessor { .. } | Update { .. } => todo!("record access/accessor/update"),

        Closure(ann, name, _, loc_args, boxed_body) => {
            let (loc_body, ret_var) = *boxed_body;

            match procs.insert_anonymous(env, name, ann, loc_args, loc_body, ret_var, layout_cache)
            {
                Ok(layout) => {
                    // TODO should the let have layout Pointer?
                    Stmt::Let(
                        assigned,
                        Expr::FunctionPointer(name, layout.clone()),
                        layout,
                        hole,
                    )
                }

                Err(_error) => Stmt::RuntimeError(
                    "TODO convert anonymous function error to a RuntimeError string",
                ),
            }
        }

        Call(boxed, loc_args, _) => {
            let (fn_var, loc_expr, ret_var) = *boxed;

            /*
            Var(symbol) => {
                if procs.module_thunks.contains(&symbol) {
                    let partial_proc = procs.partial_procs.get(&symbol).unwrap();
                    let fn_var = partial_proc.annotation;
                    let ret_var = fn_var; // These are the same for a thunk.

                    // This is a top-level declaration, which will code gen to a 0-arity thunk.
                    call_by_name(
                        env,
                        procs,
                        fn_var,
                        ret_var,
                        symbol,
                        std::vec::Vec::new(),
                        layout_cache,
                    )
                } else {
                    // NOTE Load will always increment the refcount
                    Expr::Load(symbol)
                }
            }
                */

            // match from_can(env, loc_expr.value, procs, layout_cache) {
            match loc_expr.value {
                roc_can::expr::Expr::Var(proc_name) if procs.module_thunks.contains(&proc_name) => {
                    todo!()
                }
                roc_can::expr::Expr::Var(proc_name) => call_by_name(
                    env,
                    procs,
                    fn_var,
                    ret_var,
                    proc_name,
                    loc_args,
                    layout_cache,
                    assigned,
                    hole,
                ),
                _ => {
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
                    let mut arg_symbols = Vec::with_capacity_in(loc_args.len(), env.arena);

                    for _ in 0..loc_args.len() {
                        arg_symbols.push(env.unique_symbol());
                    }

                    let layout = layout_cache
                        .from_var(env.arena, fn_var, env.subs, env.pointer_size)
                        .unwrap_or_else(|err| {
                            panic!("TODO turn fn_var into a RuntimeError {:?}", err)
                        });

                    let ret_layout = layout_cache
                        .from_var(env.arena, ret_var, env.subs, env.pointer_size)
                        .unwrap_or_else(|err| {
                            panic!("TODO turn fn_var into a RuntimeError {:?}", err)
                        });

                    let function_symbol = env.unique_symbol();
                    let arg_symbols = arg_symbols.into_bump_slice();
                    let mut result = Stmt::Let(
                        assigned,
                        Expr::FunctionCall {
                            call_type: CallType::ByPointer(function_symbol),
                            layout,
                            args: arg_symbols,
                        },
                        ret_layout,
                        arena.alloc(hole),
                    );

                    // let ptr = with_hole(env, loc_expr.value, procs, layout_cache, function_symbol);
                    result = with_hole(
                        env,
                        loc_expr.value,
                        procs,
                        layout_cache,
                        function_symbol,
                        env.arena.alloc(result),
                    );

                    for ((_, loc_arg), symbol) in
                        loc_args.into_iter().rev().zip(arg_symbols.iter().rev())
                    {
                        result = with_hole(
                            env,
                            loc_arg.value,
                            procs,
                            layout_cache,
                            *symbol,
                            env.arena.alloc(result),
                        );
                    }

                    result
                }
            }
        }

        RunLowLevel { op, args, ret_var } => {
            let op = optimize_low_level(env.subs, op, &args);

            let mut arg_symbols = Vec::with_capacity_in(args.len(), env.arena);

            for (_, arg_expr) in args.iter() {
                if let roc_can::expr::Expr::Var(symbol) = arg_expr {
                    arg_symbols.push(*symbol);
                } else {
                    arg_symbols.push(env.unique_symbol());
                }
            }
            let arg_symbols = arg_symbols.into_bump_slice();

            // layout of the return type
            let layout = layout_cache
                .from_var(env.arena, ret_var, env.subs, env.pointer_size)
                .unwrap_or_else(|err| todo!("TODO turn fn_var into a RuntimeError {:?}", err));

            let mut result = Stmt::Let(assigned, Expr::RunLowLevel(op, arg_symbols), layout, hole);

            for ((_arg_var, arg_expr), symbol) in
                args.into_iter().rev().zip(arg_symbols.iter().rev())
            {
                // if this argument is already a symbol, we don't need to re-define it
                if let roc_can::expr::Expr::Var(_) = arg_expr {
                    continue;
                }

                result = with_hole(
                    env,
                    arg_expr,
                    procs,
                    layout_cache,
                    *symbol,
                    env.arena.alloc(result),
                );
            }

            result
        }
        RuntimeError(e) => Stmt::RuntimeError(env.arena.alloc(format!("{:?}", e))),
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
        LetRec(defs, cont, _, _) => {
            // because Roc is strict, only functions can be recursive!
            for def in defs.into_iter() {
                if let roc_can::pattern::Pattern::Identifier(symbol) = &def.loc_pattern.value {
                    // Now that we know for sure it's a closure, get an owned
                    // version of these variant args so we can use them properly.
                    match def.loc_expr.value {
                        Closure(ann, _, _, loc_args, boxed_body) => {
                            // Extract Procs, but discard the resulting Expr::Load.
                            // That Load looks up the pointer, which we won't use here!

                            let (loc_body, ret_var) = *boxed_body;

                            procs.insert_named(
                                env,
                                layout_cache,
                                *symbol,
                                ann,
                                loc_args,
                                loc_body,
                                ret_var,
                            );

                            continue;
                        }
                        _ => unreachable!("recursive value is not a function"),
                    }
                }
                unreachable!("recursive value does not have Identifier pattern")
            }

            from_can(env, cont.value, procs, layout_cache)
        }
        LetNonRec(def, cont, xvar, _) => {
            if let roc_can::pattern::Pattern::Identifier(symbol) = &def.loc_pattern.value {
                if let Closure(_, _, _, _, _) = &def.loc_expr.value {
                    // Now that we know for sure it's a closure, get an owned
                    // version of these variant args so we can use them properly.
                    match def.loc_expr.value {
                        Closure(ann, _, _, loc_args, boxed_body) => {
                            // Extract Procs, but discard the resulting Expr::Load.
                            // That Load looks up the pointer, which we won't use here!

                            let (loc_body, ret_var) = *boxed_body;

                            procs.insert_named(
                                env,
                                layout_cache,
                                *symbol,
                                ann,
                                loc_args,
                                loc_body,
                                ret_var,
                            );

                            return from_can(env, cont.value, procs, layout_cache);
                        }
                        _ => unreachable!(),
                    }
                }
                let rest = from_can(env, cont.value, procs, layout_cache);
                return with_hole(
                    env,
                    def.loc_expr.value,
                    procs,
                    layout_cache,
                    *symbol,
                    env.arena.alloc(rest),
                );
            }

            // this may be a destructure pattern
            let mono_pattern = from_can_pattern(env, layout_cache, &def.loc_pattern.value);

            if let Pattern::Identifier(symbol) = mono_pattern {
                let hole = env
                    .arena
                    .alloc(from_can(env, cont.value, procs, layout_cache));
                with_hole(env, def.loc_expr.value, procs, layout_cache, symbol, hole)
            } else {
                let context = crate::exhaustive::Context::BadDestruct;
                match crate::exhaustive::check(
                    def.loc_pattern.region,
                    &[(
                        Located::at(def.loc_pattern.region, mono_pattern.clone()),
                        crate::exhaustive::Guard::NoGuard,
                    )],
                    context,
                ) {
                    Ok(_) => {}
                    Err(errors) => {
                        for error in errors {
                            env.problems.push(MonoProblem::PatternProblem(error))
                        }
                    } // TODO make all variables bound in the pattern evaluate to a runtime error
                      // return Stmt::RuntimeError("TODO non-exhaustive pattern");
                }

                let layout = layout_cache
                    .from_var(env.arena, def.expr_var, env.subs, env.pointer_size)
                    .expect("invalid layout");

                let mut stores = Vec::new_in(env.arena);
                let outer_symbol = env.unique_symbol();
                store_pattern(env, &mono_pattern, outer_symbol, layout, &mut stores);

                // convert the continuation
                let mut stmt = from_can(env, cont.value, procs, layout_cache);

                // unpack the body of the def based on the pattern
                for (symbol, layout, expr) in stores.iter().rev() {
                    stmt = Stmt::Let(*symbol, expr.clone(), layout.clone(), env.arena.alloc(stmt));
                }

                // convert the def body, store in outer_symbol
                with_hole(
                    env,
                    def.loc_expr.value,
                    procs,
                    layout_cache,
                    outer_symbol,
                    env.arena.alloc(stmt),
                )
            }
        }

        _ => {
            let symbol = env.unique_symbol();
            let hole = env.arena.alloc(Stmt::Ret(symbol));
            with_hole(env, can_expr, procs, layout_cache, symbol, hole)
        }
    }
}

fn to_opt_branches<'a>(
    env: &mut Env<'a, '_>,
    cond_var: Variable,
    expr_var: Variable,
    region: Region,
    cond_symbol: Symbol,
    mut branches: std::vec::Vec<roc_can::expr::WhenBranch>,
    layout_cache: &mut LayoutCache<'a>,
) -> std::vec::Vec<(
    Pattern<'a>,
    Option<Located<roc_can::expr::Expr>>,
    roc_can::expr::Expr,
)> {
    debug_assert!(!branches.is_empty());

    let cond_layout = layout_cache
        .from_var(env.arena, cond_var, env.subs, env.pointer_size)
        .unwrap_or_else(|err| panic!("TODO turn this into a RuntimeError {:?}", err));

    let mut loc_branches = std::vec::Vec::new();
    let mut opt_branches = std::vec::Vec::new();

    for when_branch in branches {
        let exhaustive_guard = if when_branch.guard.is_some() {
            Guard::HasGuard
        } else {
            Guard::NoGuard
        };

        for loc_pattern in when_branch.patterns {
            let mono_pattern = from_can_pattern(env, layout_cache, &loc_pattern.value);

            loc_branches.push((
                Located::at(loc_pattern.region, mono_pattern.clone()),
                exhaustive_guard.clone(),
            ));

            // TODO remove clone?
            opt_branches.push((
                mono_pattern,
                when_branch.guard.clone(),
                when_branch.value.value.clone(),
            ));
        }
    }

    // NOTE exhaustiveness is checked after the construction of all the branches
    // In contrast to elm (currently), we still do codegen even if a pattern is non-exhaustive.
    // So we not only report exhaustiveness errors, but also correct them
    let context = crate::exhaustive::Context::BadCase;
    match crate::exhaustive::check(region, &loc_branches, context) {
        Ok(_) => {}
        Err(errors) => {
            use crate::exhaustive::Error::*;
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
                    None,
                    roc_can::expr::Expr::RuntimeError(
                        roc_problem::can::RuntimeError::NonExhaustivePattern,
                    ),
                ));
            }
        }
    }

    opt_branches
}

fn from_can_when<'a>(
    env: &mut Env<'a, '_>,
    cond_var: Variable,
    expr_var: Variable,
    region: Region,
    cond_symbol: Symbol,
    branches: std::vec::Vec<roc_can::expr::WhenBranch>,
    layout_cache: &mut LayoutCache<'a>,
    procs: &mut Procs<'a>,
    join_point: Option<JoinPointId>,
) -> Stmt<'a> {
    if branches.is_empty() {
        // A when-expression with no branches is a runtime error.
        // We can't know what to return!
        return Stmt::RuntimeError("Hit a 0-branch when expression");
    }
    let opt_branches = to_opt_branches(
        env,
        cond_var,
        expr_var,
        region,
        cond_symbol,
        branches,
        layout_cache,
    );

    let cond_layout = layout_cache
        .from_var(env.arena, cond_var, env.subs, env.pointer_size)
        .unwrap_or_else(|err| panic!("TODO turn this into a RuntimeError {:?}", err));

    let ret_layout = layout_cache
        .from_var(env.arena, expr_var, env.subs, env.pointer_size)
        .unwrap_or_else(|err| panic!("TODO turn this into a RuntimeError {:?}", err));

    let arena = env.arena;
    let it = opt_branches
        .into_iter()
        .map(|(pattern, opt_guard, can_expr)| {
            let mut stores = Vec::with_capacity_in(1, env.arena);
            let res_stores =
                store_pattern(env, &pattern, cond_symbol, cond_layout.clone(), &mut stores);
            let mut stmt = match join_point {
                None => from_can(env, can_expr, procs, layout_cache),
                Some(id) => {
                    let symbol = env.unique_symbol();
                    let arguments = bumpalo::vec![in env.arena; symbol].into_bump_slice();
                    let jump = env.arena.alloc(Stmt::Jump(id, arguments));

                    with_hole(env, can_expr, procs, layout_cache, symbol, jump)
                }
            };

            use crate::decision_tree2::Guard;
            match res_stores {
                Ok(_) => {
                    for (symbol, layout, expr) in stores.iter().rev() {
                        stmt =
                            Stmt::Let(*symbol, expr.clone(), layout.clone(), env.arena.alloc(stmt));
                    }

                    let guard = if let Some(loc_expr) = opt_guard {
                        let id = JoinPointId(env.unique_symbol());
                        let symbol = env.unique_symbol();
                        let jump = env.arena.alloc(Stmt::Jump(id, env.arena.alloc([symbol])));

                        let mut stmt =
                            with_hole(env, loc_expr.value, procs, layout_cache, symbol, jump);

                        // guard must have access to bound values
                        for (symbol, layout, expr) in stores.into_iter().rev() {
                            stmt = Stmt::Let(symbol, expr, layout, env.arena.alloc(stmt));
                        }

                        Guard::Guard { id, symbol, stmt }
                    } else {
                        Guard::NoGuard
                    };

                    (pattern, guard, stmt)
                }
                Err(msg) => (
                    Pattern::Underscore,
                    Guard::NoGuard,
                    Stmt::RuntimeError(env.arena.alloc(msg)),
                ),
            }
        });
    let mono_branches = Vec::from_iter_in(it, arena);

    crate::decision_tree2::optimize_when(
        env,
        cond_symbol,
        cond_layout.clone(),
        ret_layout,
        mono_branches,
    )
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
            // TODO surely something should happen here?
            stored.push((*symbol, layout, Expr::Alias(outer_symbol)));
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
            let mut pattern_vars = Vec::with_capacity_in(loc_args.len(), arena);

            let mut field_symbols = Vec::with_capacity_in(loc_args.len(), env.arena);

            for (_, arg_expr) in loc_args.iter() {
                if let roc_can::expr::Expr::Var(symbol) = arg_expr.value {
                    field_symbols.push(symbol);
                } else {
                    field_symbols.push(env.unique_symbol());
                }
            }
            let field_symbols = field_symbols.into_bump_slice();

            for (var, loc_arg) in loc_args.clone() {
                match layout_cache.from_var(&env.arena, var, &env.subs, env.pointer_size) {
                    Ok(layout) => {
                        pattern_vars.push(var);
                    }
                    Err(_) => {
                        // One of this function's arguments code gens to a runtime error,
                        // so attempting to call it will immediately crash.
                        return Stmt::RuntimeError("TODO runtime error for invalid layout");
                    }
                }
            }

            // TODO does this work?
            let layout = if let Layout::FunctionPointer(_, rlayout) = layout {
                rlayout
            } else {
                &layout
            };

            // If we've already specialized this one, no further work is needed.
            if procs.specialized.contains_key(&(proc_name, layout.clone())) {
                let call = Expr::FunctionCall {
                    call_type: CallType::ByName(proc_name),
                    layout: layout.clone(),
                    args: field_symbols,
                };

                let mut result = Stmt::Let(assigned, call, layout.clone(), hole);

                for ((_, loc_arg), symbol) in
                    loc_args.into_iter().rev().zip(field_symbols.iter().rev())
                {
                    // if this argument is already a symbol, we don't need to re-define it
                    if let roc_can::expr::Expr::Var(_) = loc_arg.value {
                        continue;
                    }
                    result = with_hole(
                        env,
                        loc_arg.value,
                        procs,
                        layout_cache,
                        *symbol,
                        env.arena.alloc(result),
                    );
                }

                result
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
                            layout: layout.clone(),
                            args: field_symbols,
                        };

                        let mut result = Stmt::Let(assigned, call, layout.clone(), hole);

                        for ((_, loc_arg), symbol) in
                            loc_args.into_iter().rev().zip(field_symbols.iter().rev())
                        {
                            // if this argument is already a symbol, we don't need to re-define it
                            if let roc_can::expr::Expr::Var(_) = loc_arg.value {
                                continue;
                            }
                            result = with_hole(
                                env,
                                loc_arg.value,
                                procs,
                                layout_cache,
                                *symbol,
                                env.arena.alloc(result),
                            );
                        }

                        result
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
                                            layout: layout.clone(),
                                            args: field_symbols,
                                        };

                                        let mut result =
                                            Stmt::Let(assigned, call, layout.clone(), hole);

                                        for ((_, loc_arg), symbol) in loc_args
                                            .into_iter()
                                            .rev()
                                            .zip(field_symbols.iter().rev())
                                        {
                                            // if this argument is already a symbol, we don't need to re-define it
                                            if let roc_can::expr::Expr::Var(_) = loc_arg.value {
                                                continue;
                                            }
                                            result = with_hole(
                                                env,
                                                loc_arg.value,
                                                procs,
                                                layout_cache,
                                                *symbol,
                                                env.arena.alloc(result),
                                            );
                                        }

                                        result
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
                                match procs.runtime_errors.get(&proc_name) {
                                    Some(error) => Stmt::RuntimeError(error),
                                    None => unreachable!("Proc name {:?} is invalid", proc_name),
                                }
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

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'a> {
    Identifier(Symbol),
    Underscore,

    IntLiteral(i64),
    FloatLiteral(u64),
    BitLiteral {
        value: bool,
        tag_name: TagName,
        union: crate::exhaustive::Union,
    },
    EnumLiteral {
        tag_id: u8,
        tag_name: TagName,
        union: crate::exhaustive::Union,
    },
    StrLiteral(Box<str>),

    RecordDestructure(Vec<'a, RecordDestruct<'a>>, Layout<'a>),
    AppliedTag {
        tag_name: TagName,
        tag_id: u8,
        arguments: Vec<'a, (Pattern<'a>, Layout<'a>)>,
        layout: Layout<'a>,
        union: crate::exhaustive::Union,
    },

    // Runtime Exceptions
    Shadowed(Region, Located<Ident>),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordDestruct<'a> {
    pub label: Lowercase,
    pub layout: Layout<'a>,
    pub symbol: Symbol,
    pub typ: DestructType<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DestructType<'a> {
    Required,
    Optional(roc_can::expr::Expr),
    Guard(Pattern<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhenBranch<'a> {
    pub patterns: Vec<'a, Pattern<'a>>,
    pub value: Expr<'a>,
    pub guard: Option<Stmt<'a>>,
}

pub fn from_can_pattern<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    can_pattern: &roc_can::pattern::Pattern,
) -> Pattern<'a> {
    use roc_can::pattern::Pattern::*;
    match can_pattern {
        Underscore => Pattern::Underscore,
        Identifier(symbol) => Pattern::Identifier(*symbol),
        IntLiteral(v) => Pattern::IntLiteral(*v),
        FloatLiteral(v) => Pattern::FloatLiteral(f64::to_bits(*v)),
        StrLiteral(v) => Pattern::StrLiteral(v.clone()),
        Shadowed(region, ident) => Pattern::Shadowed(*region, ident.clone()),
        UnsupportedPattern(region) => Pattern::UnsupportedPattern(*region),
        MalformedPattern(_problem, region) => {
            // TODO preserve malformed problem information here?
            Pattern::UnsupportedPattern(*region)
        }
        NumLiteral(var, num) => match num_argument_to_int_or_float(env.subs, *var) {
            IntOrFloat::IntType => Pattern::IntLiteral(*num),
            IntOrFloat::FloatType => Pattern::FloatLiteral(*num as u64),
        },

        AppliedTag {
            whole_var,
            tag_name,
            arguments,
            ..
        } => {
            use crate::exhaustive::Union;
            use crate::layout::UnionVariant::*;

            let variant =
                crate::layout::union_sorted_tags(env.arena, *whole_var, env.subs, env.pointer_size);

            match variant {
                Never => unreachable!("there is no pattern of type `[]`"),
                Unit => Pattern::EnumLiteral {
                    tag_id: 0,
                    tag_name: tag_name.clone(),
                    union: Union {
                        render_as: RenderAs::Tag,
                        alternatives: vec![Ctor {
                            tag_id: TagId(0),
                            name: tag_name.clone(),
                            arity: 0,
                        }],
                    },
                },
                BoolUnion { ttrue, ffalse } => Pattern::BitLiteral {
                    value: tag_name == &ttrue,
                    tag_name: tag_name.clone(),
                    union: Union {
                        render_as: RenderAs::Tag,
                        alternatives: vec![
                            Ctor {
                                tag_id: TagId(0),
                                name: ffalse,
                                arity: 0,
                            },
                            Ctor {
                                tag_id: TagId(1),
                                name: ttrue,
                                arity: 0,
                            },
                        ],
                    },
                },
                ByteUnion(tag_names) => {
                    let tag_id = tag_names
                        .iter()
                        .position(|key| key == tag_name)
                        .expect("tag must be in its own type");

                    let mut ctors = std::vec::Vec::with_capacity(tag_names.len());
                    for (i, tag_name) in tag_names.iter().enumerate() {
                        ctors.push(Ctor {
                            tag_id: TagId(i as u8),
                            name: tag_name.clone(),
                            arity: 0,
                        })
                    }

                    let union = crate::exhaustive::Union {
                        render_as: RenderAs::Tag,
                        alternatives: ctors,
                    };

                    Pattern::EnumLiteral {
                        tag_id: tag_id as u8,
                        tag_name: tag_name.clone(),
                        union,
                    }
                }
                Unwrapped(field_layouts) => {
                    let union = crate::exhaustive::Union {
                        render_as: RenderAs::Tag,
                        alternatives: vec![Ctor {
                            tag_id: TagId(0),
                            name: tag_name.clone(),
                            arity: field_layouts.len(),
                        }],
                    };

                    let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);
                    for ((_, loc_pat), layout) in arguments.iter().zip(field_layouts.iter()) {
                        mono_args.push((
                            from_can_pattern(env, layout_cache, &loc_pat.value),
                            layout.clone(),
                        ));
                    }

                    let layout = Layout::Struct(field_layouts.into_bump_slice());

                    Pattern::AppliedTag {
                        tag_name: tag_name.clone(),
                        tag_id: 0,
                        arguments: mono_args,
                        union,
                        layout,
                    }
                }
                Wrapped(tags) => {
                    let mut ctors = std::vec::Vec::with_capacity(tags.len());
                    for (i, (tag_name, args)) in tags.iter().enumerate() {
                        ctors.push(Ctor {
                            tag_id: TagId(i as u8),
                            name: tag_name.clone(),
                            // don't include tag discriminant in arity
                            arity: args.len() - 1,
                        })
                    }

                    let union = crate::exhaustive::Union {
                        render_as: RenderAs::Tag,
                        alternatives: ctors,
                    };

                    let (tag_id, (_, argument_layouts)) = tags
                        .iter()
                        .enumerate()
                        .find(|(_, (key, _))| key == tag_name)
                        .expect("tag must be in its own type");

                    let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);
                    // disregard the tag discriminant layout
                    let it = argument_layouts[1..].iter();
                    for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                        mono_args.push((
                            from_can_pattern(env, layout_cache, &loc_pat.value),
                            layout.clone(),
                        ));
                    }

                    let mut layouts: Vec<&'a [Layout<'a>]> =
                        Vec::with_capacity_in(tags.len(), env.arena);

                    for (_, arg_layouts) in tags.into_iter() {
                        layouts.push(arg_layouts);
                    }

                    let layout = Layout::Union(layouts.into_bump_slice());

                    Pattern::AppliedTag {
                        tag_name: tag_name.clone(),
                        tag_id: tag_id as u8,
                        arguments: mono_args,
                        union,
                        layout,
                    }
                }
            }
        }

        RecordDestructure {
            whole_var,
            destructs,
            ..
        } => {
            let mut mono_destructs = Vec::with_capacity_in(destructs.len(), env.arena);
            let mut destructs = destructs.clone();
            destructs.sort_by(|a, b| a.value.label.cmp(&b.value.label));

            let mut it = destructs.iter();
            let mut opt_destruct = it.next();

            let sorted_fields = crate::layout::sort_record_fields(
                env.arena,
                *whole_var,
                env.subs,
                env.pointer_size,
            );

            let mut field_layouts = Vec::with_capacity_in(sorted_fields.len(), env.arena);

            for (label, field_layout) in sorted_fields.into_iter() {
                if let Some(destruct) = opt_destruct {
                    if destruct.value.label == label {
                        opt_destruct = it.next();

                        mono_destructs.push(from_can_record_destruct(
                            env,
                            layout_cache,
                            &destruct.value,
                            field_layout.clone(),
                        ));
                    } else {
                        // insert underscore pattern
                        mono_destructs.push(RecordDestruct {
                            label: label.clone(),
                            symbol: env.unique_symbol(),
                            layout: field_layout.clone(),
                            typ: DestructType::Guard(Pattern::Underscore),
                        });
                    }
                } else {
                    // insert underscore pattern
                    mono_destructs.push(RecordDestruct {
                        label: label.clone(),
                        symbol: env.unique_symbol(),
                        layout: field_layout.clone(),
                        typ: DestructType::Guard(Pattern::Underscore),
                    });
                }
                field_layouts.push(field_layout);
            }

            Pattern::RecordDestructure(
                mono_destructs,
                Layout::Struct(field_layouts.into_bump_slice()),
            )
        }
    }
}

fn from_can_record_destruct<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    can_rd: &roc_can::pattern::RecordDestruct,
    field_layout: Layout<'a>,
) -> RecordDestruct<'a> {
    RecordDestruct {
        label: can_rd.label.clone(),
        symbol: can_rd.symbol,
        layout: field_layout,
        typ: match &can_rd.typ {
            roc_can::pattern::DestructType::Required => DestructType::Required,
            roc_can::pattern::DestructType::Optional(_, loc_expr) => {
                DestructType::Optional(loc_expr.value.clone())
            }
            roc_can::pattern::DestructType::Guard(_, loc_pattern) => {
                DestructType::Guard(from_can_pattern(env, layout_cache, &loc_pattern.value))
            }
        },
    }
}

/// Potentially translate LowLevel operations into more efficient ones based on
/// uniqueness type info.
///
/// For example, turning LowLevel::ListSet to LowLevel::ListSetInPlace if the
/// list is Unique.
fn optimize_low_level(
    subs: &Subs,
    op: LowLevel,
    args: &[(Variable, roc_can::expr::Expr)],
) -> LowLevel {
    match op {
        LowLevel::ListSet => {
            // The first arg is the one with the List in it.
            // List.set : List elem, Int, elem -> List elem
            let list_arg_var = args[0].0;
            let content = subs.get_without_compacting(list_arg_var).content;

            match content {
                Content::Structure(FlatType::Apply(Symbol::ATTR_ATTR, attr_args)) => {
                    debug_assert_eq!(attr_args.len(), 2);

                    // If the first argument (the List) is unique,
                    // then we can safely upgrade to List.set_in_place
                    let attr_arg_content = subs.get_without_compacting(attr_args[0]).content;

                    if attr_arg_content.is_unique(subs) {
                        LowLevel::ListSetInPlace
                    } else {
                        LowLevel::ListSet
                    }
                }
                _ => op,
            }
        }
        _ => op,
    }
}

pub enum IntOrFloat {
    IntType,
    FloatType,
}

/// Given the `a` in `Num a`, determines whether it's an int or a float
pub fn num_argument_to_int_or_float(subs: &Subs, var: Variable) -> IntOrFloat {
    match subs.get_without_compacting(var).content {
        Content::Alias(Symbol::NUM_INTEGER, args, _) => {
            debug_assert!(args.is_empty());
            IntOrFloat::IntType
        }
        Content::FlexVar(_) => {
            // If this was still a (Num *), assume compiling it to an Int
            IntOrFloat::IntType
        }
        Content::Alias(Symbol::NUM_FLOATINGPOINT, args, _) => {
            debug_assert!(args.is_empty());
            IntOrFloat::FloatType
        }
        Content::Structure(FlatType::Apply(Symbol::ATTR_ATTR, attr_args)) => {
            debug_assert!(attr_args.len() == 2);

            // Recurse on the second argument
            num_argument_to_int_or_float(subs, attr_args[1])
        }
        other => {
            panic!(
                "Unrecognized Num type argument for var {:?} with Content: {:?}",
                var, other
            );
        }
    }
}
