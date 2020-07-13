use crate::layout::{list_layout_from_elem, Builtin, Layout, LayoutCache, LayoutProblem};
use crate::pattern::{Ctor, Guard, RenderAs, TagId};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{default_hasher, MutMap, MutSet};
use roc_module::ident::{Ident, Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_region::all::{Located, Region};
use roc_types::subs::{Content, FlatType, Subs, Variable};
use std::collections::HashMap;
use std::hash::Hash;

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
    pub specialized: MutMap<(Symbol, Layout<'a>), Proc<'a>>,
    pub runtime_errors: MutSet<Symbol>,
}

impl<'a> Procs<'a> {
    pub fn insert_named(
        &mut self,
        env: &mut Env<'a, '_>,
        name: Symbol,
        annotation: Variable,
        loc_args: std::vec::Vec<(Variable, Located<roc_can::pattern::Pattern>)>,
        loc_body: Located<roc_can::expr::Expr>,
        ret_var: Variable,
    ) {
        let (_, pattern_symbols, body) = patterns_to_when(env, loc_args, ret_var, loc_body);

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
    ) -> Layout<'a> {
        let (pattern_vars, pattern_symbols, body) =
            patterns_to_when(env, loc_args, ret_var, loc_body);

        // an anonymous closure. These will always be specialized already
        // by the surrounding context, so we can add pending specializations
        // for them immediately.
        let layout = layout_cache
            .from_var(env.arena, annotation, env.subs, env.pointer_size)
            .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

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

                match specialize(env, self, symbol, layout_cache, pending, partial_proc) {
                    Ok(proc) => {
                        self.specialized.insert((symbol, layout.clone()), proc);
                    }
                    Err(_) => {
                        self.runtime_errors.insert(symbol);
                    }
                }
            }
        }

        layout
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

pub type Stores<'a> = &'a [(Symbol, Layout<'a>, Expr<'a>)];

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
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

    // Load/Store
    Load(Symbol),
    Store(&'a [(Symbol, Layout<'a>, Expr<'a>)], &'a Expr<'a>),

    // Functions
    FunctionPointer(Symbol, Layout<'a>),
    RuntimeErrorFunction(&'a str),
    CallByName {
        name: Symbol,
        layout: Layout<'a>,
        args: &'a [(Expr<'a>, Layout<'a>)],
    },
    CallByPointer(&'a Expr<'a>, &'a [Expr<'a>], Layout<'a>),
    RunLowLevel(LowLevel, &'a [(Expr<'a>, Layout<'a>)]),

    // Exactly two conditional branches, e.g. if/else
    Cond {
        // The left-hand side of the conditional comparison and the right-hand side.
        // These are stored separately because there are different machine instructions
        // for e.g. "compare float and jump" vs. "compare integer and jump"

        // symbol storing the original expression that we branch on, e.g. `Ok 42`
        // required for RC logic
        cond_symbol: Symbol,

        // symbol storing the value that we branch on, e.g. `1` representing the `Ok` tag
        branch_symbol: Symbol,

        cond_layout: Layout<'a>,
        // What to do if the condition either passes or fails
        pass: (Stores<'a>, &'a Expr<'a>),
        fail: (Stores<'a>, &'a Expr<'a>),
        ret_layout: Layout<'a>,
    },
    /// Conditional branches for integers. These are more efficient.
    Switch {
        /// This *must* be an integer, because Switch potentially compiles to a jump table.
        cond: &'a Expr<'a>,
        cond_layout: Layout<'a>,
        /// The u64 in the tuple will be compared directly to the condition Expr.
        /// If they are equal, this branch will be taken.
        branches: &'a [(u64, Stores<'a>, Expr<'a>)],
        /// If no other branches pass, this default branch will be taken.
        default_branch: (Stores<'a>, &'a Expr<'a>),
        /// Each branch must return a value of this type.
        ret_layout: Layout<'a>,
    },
    Tag {
        tag_layout: Layout<'a>,
        tag_name: TagName,
        tag_id: u8,
        union_size: u8,
        arguments: &'a [(Expr<'a>, Layout<'a>)],
    },
    Struct(&'a [(Expr<'a>, Layout<'a>)]),
    AccessAtIndex {
        index: u64,
        field_layouts: &'a [Layout<'a>],
        expr: &'a Expr<'a>,
        is_unwrapped: bool,
    },

    Array {
        elem_layout: Layout<'a>,
        elems: &'a [Expr<'a>],
    },
    EmptyArray,

    RuntimeError(&'a str),
}

#[derive(Clone, Debug)]
pub enum MonoProblem {
    PatternProblem(crate::pattern::Error),
}

#[allow(clippy::too_many_arguments)]
impl<'a> Expr<'a> {
    pub fn new(
        env: &mut Env<'a, '_>,
        can_expr: roc_can::expr::Expr,
        procs: &mut Procs<'a>,
    ) -> Self {
        let mut layout_cache = LayoutCache::default();

        from_can(env, can_expr, procs, &mut layout_cache)
    }
}

enum IntOrFloat {
    IntType,
    FloatType,
}

/// Given the `a` in `Num a`, determines whether it's an int or a float
fn num_argument_to_int_or_float(subs: &Subs, var: Variable) -> IntOrFloat {
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

/// turn record/tag patterns into a when expression, e.g.
///
/// foo = \{ x } -> body
///
/// becomes
///
/// foo = \r -> when r is { x } -> body
///
/// conversion of one-pattern when expressions will do the most optimal thing
fn patterns_to_when<'a>(
    env: &mut Env<'a, '_>,
    patterns: std::vec::Vec<(Variable, Located<roc_can::pattern::Pattern>)>,
    body_var: Variable,
    mut body: Located<roc_can::expr::Expr>,
) -> (
    Vec<'a, Variable>,
    Vec<'a, Symbol>,
    Located<roc_can::expr::Expr>,
) {
    let mut arg_vars = Vec::with_capacity_in(patterns.len(), env.arena);
    let mut symbols = Vec::with_capacity_in(patterns.len(), env.arena);

    // patterns that are not yet in a when (e.g. in let or function arguments) must be irrefutable
    // to pass type checking. So the order in which we add them to the body does not matter: there
    // are only stores anyway, no branches.
    for (pattern_var, pattern) in patterns.into_iter() {
        let context = crate::pattern::Context::BadArg;
        let mono_pattern = from_can_pattern(env, &pattern.value);

        match crate::pattern::check(
            pattern.region,
            &[(
                Located::at(pattern.region, mono_pattern.clone()),
                crate::pattern::Guard::NoGuard,
            )],
            context,
        ) {
            Ok(_) => {
                let (new_symbol, new_body) =
                    pattern_to_when(env, pattern_var, pattern, body_var, body);

                symbols.push(new_symbol);
                body = new_body;
            }
            Err(errors) => {
                for error in errors {
                    env.problems.push(MonoProblem::PatternProblem(error))
                }

                let error = roc_problem::can::RuntimeError::UnsupportedPattern(pattern.region);
                body = Located::at(pattern.region, roc_can::expr::Expr::RuntimeError(error));
            }
        }

        arg_vars.push(pattern_var);
    }

    (arg_vars, symbols, body)
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

#[allow(clippy::cognitive_complexity)]
fn from_can<'a>(
    env: &mut Env<'a, '_>,
    can_expr: roc_can::expr::Expr,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
) -> Expr<'a> {
    use roc_can::expr::Expr::*;

    match can_expr {
        Num(var, num) => match num_argument_to_int_or_float(env.subs, var) {
            IntOrFloat::IntType => Expr::Int(num),
            IntOrFloat::FloatType => Expr::Float(num as f64),
        },
        Int(_, num) => Expr::Int(num),
        Float(_, num) => Expr::Float(num),
        Str(string) | BlockStr(string) => Expr::Str(env.arena.alloc(string)),
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
                Expr::Load(symbol)
            }
        }
        LetRec(defs, ret_expr, _, _) => from_can_defs(env, defs, *ret_expr, layout_cache, procs),
        LetNonRec(def, ret_expr, _, _) => {
            from_can_defs(env, vec![*def], *ret_expr, layout_cache, procs)
        }

        Closure(ann, name, _, loc_args, boxed_body) => {
            let (loc_body, ret_var) = *boxed_body;
            let layout =
                procs.insert_anonymous(env, name, ann, loc_args, loc_body, ret_var, layout_cache);

            Expr::FunctionPointer(name, layout)
        }

        RunLowLevel { op, args, .. } => {
            let op = optimize_low_level(env.subs, op, &args);
            let mut mono_args = Vec::with_capacity_in(args.len(), env.arena);

            for (arg_var, arg_expr) in args {
                let arg = from_can(env, arg_expr, procs, layout_cache);
                let layout = layout_cache
                    .from_var(env.arena, arg_var, env.subs, env.pointer_size)
                    .unwrap_or_else(|err| todo!("TODO turn fn_var into a RuntimeError {:?}", err));

                mono_args.push((arg, layout));
            }

            Expr::RunLowLevel(op, mono_args.into_bump_slice())
        }

        Call(boxed, loc_args, _) => {
            let (fn_var, loc_expr, ret_var) = *boxed;

            match from_can(env, loc_expr.value, procs, layout_cache) {
                Expr::Load(proc_name) => {
                    // Some functions can potentially mutate in-place.
                    // If we have one of those, switch to the in-place version if appropriate.
                    call_by_name(
                        env,
                        procs,
                        fn_var,
                        ret_var,
                        proc_name,
                        loc_args,
                        layout_cache,
                    )
                }
                ptr => {
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

                    for (_, loc_arg) in loc_args {
                        args.push(from_can(env, loc_arg.value, procs, layout_cache));
                    }

                    let layout = layout_cache
                        .from_var(env.arena, fn_var, env.subs, env.pointer_size)
                        .unwrap_or_else(|err| {
                            panic!("TODO turn fn_var into a RuntimeError {:?}", err)
                        });
                    Expr::CallByPointer(&*env.arena.alloc(ptr), args.into_bump_slice(), layout)
                }
            }
        }

        When {
            cond_var,
            expr_var,
            region,
            loc_cond,
            branches,
        } => from_can_when(
            env,
            cond_var,
            expr_var,
            region,
            *loc_cond,
            branches,
            layout_cache,
            procs,
        ),

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
                let cond = from_can(env, loc_cond.value, procs, layout_cache);
                let then = from_can(env, loc_then.value, procs, layout_cache);

                let branch_symbol = env.unique_symbol();

                let cond_expr = Expr::Cond {
                    cond_symbol: branch_symbol,
                    branch_symbol,
                    cond_layout: cond_layout.clone(),
                    pass: (&[], env.arena.alloc(then)),
                    fail: (&[], env.arena.alloc(expr)),
                    ret_layout: ret_layout.clone(),
                };

                expr = Expr::Store(
                    bumpalo::vec![in arena; (branch_symbol, Layout::Builtin(Builtin::Int1), cond)]
                        .into_bump_slice(),
                    env.arena.alloc(cond_expr),
                );
            }

            expr
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

            let mut field_tuples = Vec::with_capacity_in(sorted_fields.len(), arena);

            for (label, layout) in sorted_fields {
                let field = fields.remove(&label).unwrap();
                let expr = from_can(env, field.loc_expr.value, procs, layout_cache);

                field_tuples.push((expr, layout));
            }

            Expr::Struct(field_tuples.into_bump_slice())
        }

        EmptyRecord => Expr::Struct(&[]),

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
                Unit => Expr::Struct(&[]),
                BoolUnion { ttrue, .. } => Expr::Bool(tag_name == ttrue),
                ByteUnion(tag_names) => {
                    let tag_id = tag_names
                        .iter()
                        .position(|key| key == &tag_name)
                        .expect("tag must be in its own type");

                    Expr::Byte(tag_id as u8)
                }
                Unwrapped(field_layouts) => {
                    let field_exprs = args
                        .into_iter()
                        .map(|(_, arg)| from_can(env, arg.value, procs, layout_cache));

                    let mut field_tuples = Vec::with_capacity_in(field_layouts.len(), arena);

                    for tuple in field_exprs.zip(field_layouts.into_iter()) {
                        field_tuples.push(tuple)
                    }

                    Expr::Struct(field_tuples.into_bump_slice())
                }
                Wrapped(sorted_tag_layouts) => {
                    let union_size = sorted_tag_layouts.len() as u8;
                    let (tag_id, (_, argument_layouts)) = sorted_tag_layouts
                        .iter()
                        .enumerate()
                        .find(|(_, (key, _))| key == &tag_name)
                        .expect("tag must be in its own type");

                    let mut arguments = Vec::with_capacity_in(args.len(), arena);

                    let it = std::iter::once(Expr::Int(tag_id as i64)).chain(
                        args.into_iter()
                            .map(|(_, arg)| from_can(env, arg.value, procs, layout_cache)),
                    );

                    for (arg_layout, arg_expr) in argument_layouts.iter().zip(it) {
                        arguments.push((arg_expr, arg_layout.clone()));
                    }

                    let mut layouts: Vec<&'a [Layout<'a>]> =
                        Vec::with_capacity_in(sorted_tag_layouts.len(), env.arena);

                    for (_, arg_layouts) in sorted_tag_layouts.into_iter() {
                        layouts.push(arg_layouts);
                    }

                    let layout = Layout::Union(layouts.into_bump_slice());

                    Expr::Tag {
                        tag_layout: layout,
                        tag_name,
                        tag_id: tag_id as u8,
                        union_size,
                        arguments: arguments.into_bump_slice(),
                    }
                }
            }
        }

        Access {
            record_var,
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

            let record = arena.alloc(from_can(env, loc_expr.value, procs, layout_cache));

            Expr::AccessAtIndex {
                index: index.expect("field not in its own type") as u64,
                field_layouts: field_layouts.into_bump_slice(),
                expr: record,
                is_unwrapped: true,
            }
        }

        List {
            elem_var,
            loc_elems,
        } => {
            let arena = env.arena;
            let subs = &env.subs;

            match list_layout_from_elem(arena, subs, elem_var, env.pointer_size) {
                Ok(Layout::Builtin(Builtin::EmptyList)) => Expr::EmptyArray,
                Ok(Layout::Builtin(Builtin::List(elem_layout))) => {
                    let mut elems = Vec::with_capacity_in(loc_elems.len(), arena);

                    for loc_elem in loc_elems {
                        elems.push(from_can(env, loc_elem.value, procs, layout_cache));
                    }

                    Expr::Array {
                        elem_layout: elem_layout.clone(),
                        elems: elems.into_bump_slice(),
                    }
                }
                Ok(_) => {
                    unreachable!();
                }
                Err(problem) => {
                    todo!(
                        "gracefully handle List with element layout problem: {:?}",
                        problem
                    );
                }
            }
        }
        Accessor { .. } => todo!("record accessor"),
        Update { .. } => todo!("record update"),
        RuntimeError(error) => Expr::RuntimeError(env.arena.alloc(format!("{:?}", error))),
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
            let load = Expr::Load(outer_symbol);
            stored.push((*symbol, layout, load))
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
                    expr: env.arena.alloc(Expr::Load(outer_symbol)),
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

    let record = env.arena.alloc(Expr::Load(outer_symbol));
    let load = Expr::AccessAtIndex {
        index,
        field_layouts: sorted_fields,
        expr: record,
        is_unwrapped: true,
    };

    match &destruct.guard {
        None => {
            stored.push((destruct.symbol, destruct.layout.clone(), load));
        }
        Some(guard_pattern) => match &guard_pattern {
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

fn from_can_defs<'a>(
    env: &mut Env<'a, '_>,
    defs: std::vec::Vec<roc_can::def::Def>,
    ret_expr: Located<roc_can::expr::Expr>,
    layout_cache: &mut LayoutCache<'a>,
    procs: &mut Procs<'a>,
) -> Expr<'a> {
    use roc_can::expr::Expr::*;
    use roc_can::pattern::Pattern::*;

    let arena = env.arena;
    let mut stored = Vec::with_capacity_in(defs.len(), arena);

    for def in defs {
        let loc_pattern = def.loc_pattern;
        let loc_expr = def.loc_expr;
        // If we're defining a named closure, insert it into Procs and then
        // remove the Let. When code gen later goes to look it up, it'll be in Procs!
        //
        // Before:
        //
        //     identity = \a -> a
        //
        //     identity 5
        //
        // After: (`identity` is now in Procs)
        //
        //     identity 5
        //
        if let Identifier(symbol) = &loc_pattern.value {
            if let Closure(_, _, _, _, _) = &loc_expr.value {
                // Now that we know for sure it's a closure, get an owned
                // version of these variant args so we can use them properly.
                match loc_expr.value {
                    Closure(ann, _, _, loc_args, boxed_body) => {
                        // Extract Procs, but discard the resulting Expr::Load.
                        // That Load looks up the pointer, which we won't use here!

                        let (loc_body, ret_var) = *boxed_body;

                        procs.insert_named(env, *symbol, ann, loc_args, loc_body, ret_var);

                        continue;
                    }
                    _ => unreachable!(),
                }
            }
        }

        // If it wasn't specifically an Identifier & Closure, proceed as normal.
        let mono_pattern = from_can_pattern(env, &loc_pattern.value);

        let layout = layout_cache
            .from_var(env.arena, def.expr_var, env.subs, env.pointer_size)
            .expect("invalid layout");

        match &mono_pattern {
            Pattern::Identifier(symbol) => {
                stored.push((
                    *symbol,
                    layout.clone(),
                    from_can(env, loc_expr.value, procs, layout_cache),
                ));
            }
            _ => {
                let context = crate::pattern::Context::BadDestruct;
                match crate::pattern::check(
                    loc_pattern.region,
                    &[(
                        Located::at(loc_pattern.region, mono_pattern.clone()),
                        crate::pattern::Guard::NoGuard,
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

                let symbol = env.unique_symbol();
                stored.push((
                    symbol,
                    layout.clone(),
                    from_can(env, loc_expr.value, procs, layout_cache),
                ));

                match store_pattern(env, &mono_pattern, symbol, layout, &mut stored) {
                    Ok(()) => {}
                    Err(message) => todo!(
                        "generate runtime error, the pattern was invalid: {:?}",
                        message
                    ),
                }
            }
        }
    }
    // At this point, it's safe to assume we aren't assigning a Closure to a def.
    // Extract Procs from the def body and the ret expression, and return the result!
    let ret = from_can(env, ret_expr.value, procs, layout_cache);

    if stored.is_empty() {
        ret
    } else {
        Expr::Store(stored.into_bump_slice(), arena.alloc(ret))
    }
}

// TODO trim these down
#[allow(clippy::too_many_arguments)]
fn from_can_when<'a>(
    env: &mut Env<'a, '_>,
    cond_var: Variable,
    expr_var: Variable,
    region: Region,
    loc_cond: Located<roc_can::expr::Expr>,
    mut branches: std::vec::Vec<roc_can::expr::WhenBranch>,
    layout_cache: &mut LayoutCache<'a>,
    procs: &mut Procs<'a>,
) -> Expr<'a> {
    if branches.is_empty() {
        // A when-expression with no branches is a runtime error.
        // We can't know what to return!
        Expr::RuntimeError("Hit a 0-branch when expression")
    } else if branches.len() == 1 && branches[0].patterns.len() == 1 && branches[0].guard.is_none()
    {
        let first = branches.remove(0);
        // A when-expression with exactly 1 branch is essentially a LetNonRec.
        // As such, we can compile it direcly to a Store.
        let arena = env.arena;
        let mut stored = Vec::with_capacity_in(1, arena);

        let loc_when_pattern = &first.patterns[0];

        let mono_pattern = from_can_pattern(env, &loc_when_pattern.value);

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
        let cond_symbol = env.unique_symbol();
        let cond = from_can(env, loc_cond.value, procs, layout_cache);
        stored.push((cond_symbol, cond_layout.clone(), cond));

        // NOTE this will still store shadowed names.
        // that's fine: the branch throws a runtime error anyway
        let ret = match store_pattern(env, &mono_pattern, cond_symbol, cond_layout, &mut stored) {
            Ok(_) => from_can(env, first.value.value, procs, layout_cache),
            Err(message) => Expr::RuntimeError(env.arena.alloc(message)),
        };

        Expr::Store(stored.into_bump_slice(), arena.alloc(ret))
    } else {
        let cond_layout = layout_cache
            .from_var(env.arena, cond_var, env.subs, env.pointer_size)
            .unwrap_or_else(|err| panic!("TODO turn this into a RuntimeError {:?}", err));

        let cond = from_can(env, loc_cond.value, procs, layout_cache);
        let cond_symbol = env.unique_symbol();

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
                let mono_pattern = from_can_pattern(env, &loc_pattern.value);

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
                                crate::decision_tree::Guard::Guard {
                                    stores: stores.into_bump_slice(),
                                    expr,
                                },
                                &[] as &[_],
                                mono_expr.clone(),
                            )
                        } else {
                            (
                                crate::decision_tree::Guard::NoGuard,
                                stores.into_bump_slice(),
                                mono_expr.clone(),
                            )
                        }
                    }
                    Err(message) => {
                        // when the pattern is invalid, a guard must give a runtime error too
                        if when_branch.guard.is_some() {
                            (
                                crate::decision_tree::Guard::Guard {
                                    stores: &[],
                                    expr: Expr::RuntimeError(env.arena.alloc(message)),
                                },
                                &[] as &[_],
                                // we can never hit this
                                Expr::RuntimeError(&"invalid pattern with guard: unreachable"),
                            )
                        } else {
                            (
                                crate::decision_tree::Guard::NoGuard,
                                &[] as &[_],
                                Expr::RuntimeError(env.arena.alloc(message)),
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
                        crate::decision_tree::Guard::NoGuard,
                        &[],
                        Expr::RuntimeError("non-exhaustive pattern match"),
                    ));
                }
            }
        }

        let ret_layout = layout_cache
            .from_var(env.arena, expr_var, env.subs, env.pointer_size)
            .unwrap_or_else(|err| panic!("TODO turn this into a RuntimeError {:?}", err));

        let branching = crate::decision_tree::optimize_when(
            env,
            cond_symbol,
            cond_layout.clone(),
            ret_layout,
            opt_branches,
        );

        let stores = env.arena.alloc([(cond_symbol, cond_layout, cond)]);

        Expr::Store(stores, env.arena.alloc(branching))
    }
}

fn call_by_name<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    fn_var: Variable,
    ret_var: Variable,
    proc_name: Symbol,
    loc_args: std::vec::Vec<(Variable, Located<roc_can::expr::Expr>)>,
    layout_cache: &mut LayoutCache<'a>,
) -> Expr<'a> {
    // Register a pending_specialization for this function
    match layout_cache.from_var(env.arena, fn_var, env.subs, env.pointer_size) {
        Ok(layout) => {
            // Build the CallByName node
            let arena = env.arena;
            let mut args = Vec::with_capacity_in(loc_args.len(), arena);
            let mut pattern_vars = Vec::with_capacity_in(loc_args.len(), arena);

            for (var, loc_arg) in loc_args {
                pattern_vars.push(var);

                match layout_cache.from_var(&env.arena, var, &env.subs, env.pointer_size) {
                    Ok(layout) => {
                        args.push((from_can(env, loc_arg.value, procs, layout_cache), layout));
                    }
                    Err(_) => {
                        // One of this function's arguments code gens to a runtime error,
                        // so attempting to call it will immediately crash.
                        return Expr::RuntimeError("");
                    }
                }
            }

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
                }
                None => {
                    // TODO should pending_procs hold a Rc<Proc>?
                    let partial_proc = procs
                        .partial_procs
                        .get(&proc_name)
                        .unwrap_or_else(|| {
                            panic!("Could not find partial_proc for {:?}", proc_name)
                        })
                        .clone();

                    match specialize(env, procs, proc_name, layout_cache, pending, partial_proc) {
                        Ok(proc) => {
                            procs.specialized.insert((proc_name, layout.clone()), proc);
                        }
                        Err(_) => {
                            procs.runtime_errors.insert(proc_name);
                        }
                    }
                }
            }

            Expr::CallByName {
                name: proc_name,
                layout,
                args: args.into_bump_slice(),
            }
        }
        Err(_) => {
            // This function code gens to a runtime error,
            // so attempting to call it will immediately crash.
            Expr::RuntimeError("")
        }
    }
}

pub fn specialize_all<'a>(
    env: &mut Env<'a, '_>,
    mut procs: Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
) -> Procs<'a> {
    let mut pending_specializations = procs.pending_specializations.unwrap_or(MutMap::default());

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
            if !procs.specialized.contains_key(&(name, layout.clone())) {
                // TODO should pending_procs hold a Rc<Proc>?
                let partial_proc = procs
                    .partial_procs
                    .get(&name)
                    .unwrap_or_else(|| panic!("Could not find partial_proc for {:?}", name))
                    .clone();

                match specialize(env, &mut procs, name, layout_cache, pending, partial_proc) {
                    Ok(proc) => {
                        procs.specialized.insert((name, layout), proc);
                    }
                    Err(_) => {
                        procs.runtime_errors.insert(name);
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

    let specialized_body = from_can(env, body, procs, layout_cache);
    // reset subs, so we don't get type errors when specializing for a different signature
    env.subs.rollback_to(snapshot);

    let mut proc_args = Vec::with_capacity_in(pattern_vars.len(), &env.arena);

    debug_assert!(pattern_vars.len() == pattern_symbols.len());

    for (arg_var, arg_name) in pattern_vars.iter().zip(pattern_symbols.iter()) {
        let layout = layout_cache.from_var(&env.arena, *arg_var, env.subs, env.pointer_size)?;

        proc_args.push((layout, *arg_name));
    }

    let ret_layout = layout_cache
        .from_var(&env.arena, ret_var, env.subs, env.pointer_size)
        .unwrap_or_else(|err| panic!("TODO handle invalid function {:?}", err));

    let proc = Proc {
        name: proc_name,
        args: proc_args.into_bump_slice(),
        body: specialized_body,
        closes_over: Layout::Struct(&[]),
        ret_layout,
    };

    Ok(proc)
}

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Pattern<'a> {
    Identifier(Symbol),
    Underscore,

    IntLiteral(i64),
    FloatLiteral(u64),
    BitLiteral {
        value: bool,
        tag_name: TagName,
        union: crate::pattern::Union,
    },
    EnumLiteral {
        tag_id: u8,
        tag_name: TagName,
        union: crate::pattern::Union,
    },
    StrLiteral(Box<str>),

    RecordDestructure(Vec<'a, RecordDestruct<'a>>, Layout<'a>),
    AppliedTag {
        tag_name: TagName,
        tag_id: u8,
        arguments: Vec<'a, (Pattern<'a>, Layout<'a>)>,
        layout: Layout<'a>,
        union: crate::pattern::Union,
    },

    // Runtime Exceptions
    Shadowed(Region, Located<Ident>),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RecordDestruct<'a> {
    pub label: Lowercase,
    pub layout: Layout<'a>,
    pub symbol: Symbol,
    pub guard: Option<Pattern<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhenBranch<'a> {
    pub patterns: Vec<'a, Pattern<'a>>,
    pub value: Expr<'a>,
    pub guard: Option<Expr<'a>>,
}

fn from_can_pattern<'a>(
    env: &mut Env<'a, '_>,
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
            use crate::layout::UnionVariant::*;
            use crate::pattern::Union;

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

                    let union = crate::pattern::Union {
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
                    let union = crate::pattern::Union {
                        render_as: RenderAs::Tag,
                        alternatives: vec![Ctor {
                            tag_id: TagId(0),
                            name: tag_name.clone(),
                            arity: field_layouts.len(),
                        }],
                    };

                    let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);
                    for ((_, loc_pat), layout) in arguments.iter().zip(field_layouts.iter()) {
                        mono_args.push((from_can_pattern(env, &loc_pat.value), layout.clone()));
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

                    let union = crate::pattern::Union {
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
                        mono_args.push((from_can_pattern(env, &loc_pat.value), layout.clone()));
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
                            &destruct.value,
                            field_layout.clone(),
                        ));
                    } else {
                        // insert underscore pattern
                        mono_destructs.push(RecordDestruct {
                            label: label.clone(),
                            symbol: env.unique_symbol(),
                            layout: field_layout.clone(),
                            guard: Some(Pattern::Underscore),
                        });
                    }
                } else {
                    // insert underscore pattern
                    mono_destructs.push(RecordDestruct {
                        label: label.clone(),
                        symbol: env.unique_symbol(),
                        layout: field_layout.clone(),
                        guard: Some(Pattern::Underscore),
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
    can_rd: &roc_can::pattern::RecordDestruct,
    field_layout: Layout<'a>,
) -> RecordDestruct<'a> {
    RecordDestruct {
        label: can_rd.label.clone(),
        symbol: can_rd.symbol,
        layout: field_layout,
        guard: match &can_rd.guard {
            None => None,
            Some((_, loc_pattern)) => Some(from_can_pattern(env, &loc_pattern.value)),
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
