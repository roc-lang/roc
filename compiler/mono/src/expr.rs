use crate::layout::{Builtin, Layout};
use crate::pattern::{Ctor, Guard};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_can;
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::{Ident, Lowercase, TagName};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_region::all::{Located, Region};
use roc_types::subs::{Content, ContentHash, FlatType, Subs, Variable};
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Procs<'a> {
    user_defined: MutMap<Symbol, PartialProc<'a>>,
    anonymous: MutMap<Symbol, Option<Proc<'a>>>,
    builtin: MutSet<Symbol>,
}

impl<'a> Procs<'a> {
    fn insert_user_defined(&mut self, symbol: Symbol, partial_proc: PartialProc<'a>) {
        self.user_defined.insert(symbol, partial_proc);
    }

    fn insert_anonymous(&mut self, symbol: Symbol, proc: Option<Proc<'a>>) {
        self.anonymous.insert(symbol, proc);
    }

    fn insert_specialization(
        &mut self,
        symbol: Symbol,
        hash: ContentHash,
        spec_name: Symbol,
        proc: Option<Proc<'a>>,
    ) {
        self.user_defined
            .get_mut(&symbol)
            .map(|partial_proc| partial_proc.specializations.insert(hash, (spec_name, proc)));
    }

    fn get_user_defined(&self, symbol: Symbol) -> Option<&PartialProc<'a>> {
        self.user_defined.get(&symbol)
    }

    pub fn len(&self) -> usize {
        let anonymous: usize = self.anonymous.len();
        let user_defined: usize = self
            .user_defined
            .values()
            .map(|v| v.specializations.len())
            .sum();

        anonymous + user_defined
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn insert_builtin(&mut self, symbol: Symbol) {
        self.builtin.insert(symbol);
    }

    pub fn as_map(&self) -> MutMap<Symbol, Option<Proc<'a>>> {
        let mut result = MutMap::default();

        for partial_proc in self.user_defined.values() {
            for (_, (symbol, opt_proc)) in partial_proc.specializations.clone().into_iter() {
                result.insert(symbol, opt_proc);
            }
        }

        for (symbol, proc) in self.anonymous.clone().into_iter() {
            result.insert(symbol, proc);
        }

        for symbol in self.builtin.iter() {
            result.insert(*symbol, None);
        }

        result
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PartialProc<'a> {
    pub annotation: Variable,
    pub patterns: Vec<'a, Symbol>,
    pub body: roc_can::expr::Expr,
    pub specializations: MutMap<ContentHash, (Symbol, Option<Proc<'a>>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Proc<'a> {
    pub name: Symbol,
    pub args: &'a [(Layout<'a>, Symbol)],
    pub body: Expr<'a>,
    pub closes_over: Layout<'a>,
    pub ret_layout: Layout<'a>,
}

pub struct Env<'a, 'i> {
    pub arena: &'a Bump,
    pub subs: &'a mut Subs,
    pub problems: &'i mut std::vec::Vec<MonoProblem>,
    pub home: ModuleId,
    pub ident_ids: &'i mut IdentIds,
    pub pointer_size: u32,
    symbol_counter: usize,
    pub jump_counter: &'a mut u64,
}

impl<'a, 'i> Env<'a, 'i> {
    pub fn fresh_symbol(&mut self) -> Symbol {
        let ident_id = self
            .ident_ids
            .add(format!("_{}", self.symbol_counter).into());
        self.symbol_counter += 1;

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
    FunctionPointer(Symbol),
    CallByName(Symbol, &'a [(Expr<'a>, Layout<'a>)]),
    CallByPointer(&'a Expr<'a>, &'a [Expr<'a>], Layout<'a>),

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

    RuntimeError(&'a str),
}

#[derive(Clone, Debug)]
pub enum MonoProblem {
    PatternProblem(crate::pattern::Error),
}

#[allow(clippy::too_many_arguments)]
impl<'a> Expr<'a> {
    pub fn new(
        arena: &'a Bump,
        subs: &'a mut Subs,
        problems: &mut std::vec::Vec<MonoProblem>,
        can_expr: roc_can::expr::Expr,
        procs: &mut Procs<'a>,
        home: ModuleId,
        ident_ids: &mut IdentIds,
        pointer_size: u32,
    ) -> Self {
        let mut env = Env {
            arena,
            subs,
            problems,
            home,
            ident_ids,
            pointer_size,
            symbol_counter: 0,
            jump_counter: arena.alloc(0),
        };

        from_can(&mut env, can_expr, procs, None)
    }
}

enum IntOrFloat {
    IntType,
    FloatType,
}

fn to_int_or_float(subs: &Subs, var: Variable) -> IntOrFloat {
    match subs.get_without_compacting(var).content {
        Content::Alias(Symbol::INT_INTEGER, args, _) => {
            debug_assert!(args.is_empty());
            IntOrFloat::IntType
        }
        Content::FlexVar(_) => {
            // If this was still a (Num *), assume compiling it to an Int
            IntOrFloat::IntType
        }
        Content::Alias(Symbol::FLOAT_FLOATINGPOINT, args, _) => {
            debug_assert!(args.is_empty());
            IntOrFloat::FloatType
        }
        Content::Alias(Symbol::NUM_NUM, args, _) => {
            debug_assert!(args.len() == 1);

            match subs.get_without_compacting(args[0].1).content {
                Content::Alias(Symbol::INT_INTEGER, args, _) => {
                    debug_assert!(args.is_empty());
                    IntOrFloat::IntType
                }
                Content::FlexVar(_) => {
                    // If this was still a (Num *), assume compiling it to an Int
                    IntOrFloat::IntType
                }
                Content::Alias(Symbol::FLOAT_FLOATINGPOINT, args, _) => {
                    debug_assert!(args.is_empty());
                    IntOrFloat::FloatType
                }
                Content::Structure(FlatType::Apply(Symbol::ATTR_ATTR, attr_args)) => {
                    debug_assert!(attr_args.len() == 2);

                    // Recurse on the second argument
                    to_int_or_float(subs, attr_args[1])
                }
                other => panic!(
                    "Unrecognized Num.Num alias type argument Content: {:?}",
                    other
                ),
            }
        }
        Content::Structure(FlatType::Apply(Symbol::ATTR_ATTR, attr_args)) => {
            debug_assert!(attr_args.len() == 2);

            // Recurse on the second argument
            to_int_or_float(subs, attr_args[1])
        }
        other => panic!("Unrecognized Num type argument Content: {:?}", other),
    }
}

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
            (env.fresh_symbol(), body)
        }
        Shadowed(region, loc_ident) => {
            let error = roc_problem::can::RuntimeError::Shadowing {
                original_region: *region,
                shadow: loc_ident.clone(),
            };
            (env.fresh_symbol(), Located::at_zero(RuntimeError(error)))
        }

        UnsupportedPattern(region) => {
            // create the runtime error here, instead of delegating to When.
            // UnsupportedPattern should then never occcur in When
            let error = roc_problem::can::RuntimeError::UnsupportedPattern(*region);
            (env.fresh_symbol(), Located::at_zero(RuntimeError(error)))
        }

        AppliedTag { .. } | RecordDestructure { .. } => {
            let symbol = env.fresh_symbol();

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
    name: Option<Symbol>,
) -> Expr<'a> {
    use roc_can::expr::Expr::*;

    match can_expr {
        Num(var, num) => match to_int_or_float(env.subs, var) {
            IntOrFloat::IntType => Expr::Int(num),
            IntOrFloat::FloatType => Expr::Float(num as f64),
        },
        Int(_, num) => Expr::Int(num),
        Float(_, num) => Expr::Float(num),
        Str(string) | BlockStr(string) => Expr::Str(env.arena.alloc(string)),
        Var(symbol) => Expr::Load(symbol),
        LetRec(defs, ret_expr, _, _) => from_can_defs(env, defs, *ret_expr, procs),
        LetNonRec(def, ret_expr, _, _) => from_can_defs(env, vec![*def], *ret_expr, procs),

        Closure(annotation, _, _, loc_args, boxed_body) => {
            let (loc_body, ret_var) = *boxed_body;

            // turn record/tag patterns into a when expression, e.g.
            //
            // foo = \{ x } -> body
            //
            // becomes
            //
            // foo = \r -> when r is { x } -> body
            //
            // conversion of one-pattern when expressions will do the most optimal thing

            let (arg_vars, arg_symbols, body) = patterns_to_when(env, loc_args, ret_var, loc_body);

            let symbol = match name {
                Some(symbol) => {
                    // a named closure
                    procs.insert_user_defined(
                        symbol,
                        PartialProc {
                            annotation,
                            patterns: arg_symbols,
                            body: body.value,
                            specializations: MutMap::default(),
                        },
                    );
                    symbol
                }
                None => {
                    // an anonymous closure. These will always be specialized already
                    // by the surrounding context
                    let symbol = env.fresh_symbol();

                    let opt_proc = specialize_proc_body(
                        env,
                        procs,
                        annotation,
                        ret_var,
                        symbol,
                        &arg_vars,
                        &arg_symbols,
                        annotation,
                        body.value,
                    )
                    .ok();

                    procs.insert_anonymous(symbol, opt_proc);

                    symbol
                }
            };

            Expr::FunctionPointer(symbol)
        }

        Call(boxed, loc_args, _) => {
            use IntOrFloat::*;

            let (fn_var, loc_expr, ret_var) = *boxed;

            let specialize_builtin_functions = {
                |env: &mut Env<'a, '_>, symbol: Symbol| {
                    if !symbol.module_id().is_builtin() {
                        // return unchanged
                        symbol
                    } else {
                        match symbol {
                            Symbol::NUM_ADD => match to_int_or_float(env.subs, ret_var) {
                                FloatType => Symbol::FLOAT_ADD,
                                IntType => Symbol::INT_ADD,
                            },
                            Symbol::NUM_SUB => match to_int_or_float(env.subs, ret_var) {
                                FloatType => Symbol::FLOAT_SUB,
                                IntType => Symbol::INT_SUB,
                            },
                            // TODO make this work for more than just int/float
                            Symbol::BOOL_EQ => {
                                match Layout::from_var(
                                    env.arena,
                                    loc_args[0].0,
                                    env.subs,
                                    env.pointer_size,
                                ) {
                                    Ok(Layout::Builtin(builtin)) => match builtin {
                                        Builtin::Int64 => Symbol::INT_EQ_I64,
                                        Builtin::Float64 => Symbol::FLOAT_EQ,
                                        Builtin::Bool => Symbol::INT_EQ_I1,
                                        Builtin::Byte => Symbol::INT_EQ_I8,
                                        _ => panic!("Equality not implemented for {:?}", builtin),
                                    },
                                    Ok(complex) => panic!(
                                        "TODO support equality on complex layouts like {:?}",
                                        complex
                                    ),
                                    Err(()) => panic!("Invalid layout"),
                                }
                            }
                            _ => symbol,
                        }
                    }
                }
            };

            match from_can(env, loc_expr.value, procs, None) {
                Expr::Load(proc_name) => {
                    // Some functions can potentially mutate in-place.
                    // If we have one of those, switch to the in-place version if appropriate.
                    match specialize_builtin_functions(env, proc_name) {
                        Symbol::LIST_SET => {
                            let subs = &env.subs;
                            // The first arg is the one with the List in it.
                            // List.set : List elem, Int, elem -> List elem
                            let (list_arg_var, _) = loc_args.get(0).unwrap();

                            let content = subs.get_without_compacting(*list_arg_var).content;

                            match content {
                                Content::Structure(FlatType::Apply(
                                    Symbol::ATTR_ATTR,
                                    attr_args,
                                )) => {
                                    debug_assert!(attr_args.len() == 2);

                                    // If the first argument (the List) is unique,
                                    // then we can safely upgrade to List.set_in_place
                                    let attr_arg_content =
                                        subs.get_without_compacting(attr_args[0]).content;

                                    let new_name = if attr_arg_content.is_unique(subs) {
                                        Symbol::LIST_SET_IN_PLACE
                                    } else {
                                        Symbol::LIST_SET
                                    };

                                    call_by_name(env, procs, fn_var, ret_var, new_name, loc_args)
                                }
                                _ => call_by_name(env, procs, fn_var, ret_var, proc_name, loc_args),
                            }
                        }
                        specialized_proc_symbol => call_by_name(
                            env,
                            procs,
                            fn_var,
                            ret_var,
                            specialized_proc_symbol,
                            loc_args,
                        ),
                    }
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
                        args.push(from_can(env, loc_arg.value, procs, None));
                    }

                    let layout = Layout::from_var(env.arena, fn_var, env.subs, env.pointer_size)
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
        } => from_can_when(env, cond_var, expr_var, region, *loc_cond, branches, procs),

        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => {
            let mut expr = from_can(env, final_else.value, procs, None);

            let ret_layout = Layout::from_var(env.arena, branch_var, env.subs, env.pointer_size)
                .expect("invalid ret_layout");
            let cond_layout = Layout::from_var(env.arena, cond_var, env.subs, env.pointer_size)
                .expect("invalid cond_layout");

            for (loc_cond, loc_then) in branches.into_iter().rev() {
                let cond = from_can(env, loc_cond.value, procs, None);
                let then = from_can(env, loc_then.value, procs, None);

                let branch_symbol = env.fresh_symbol();

                let cond_expr = Expr::Cond {
                    cond_symbol: branch_symbol,
                    branch_symbol,
                    cond_layout: cond_layout.clone(),
                    pass: (&[], env.arena.alloc(then)),
                    fail: (&[], env.arena.alloc(expr)),
                    ret_layout: ret_layout.clone(),
                };

                expr = Expr::Store(
                    env.arena
                        .alloc(vec![(branch_symbol, Layout::Builtin(Builtin::Bool), cond)]),
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

            let btree = crate::layout::record_fields_btree(
                env.arena,
                record_var,
                env.subs,
                env.pointer_size,
            );

            let mut field_tuples = Vec::with_capacity_in(btree.len(), arena);

            for (label, layout) in btree {
                let field = fields.remove(&label).unwrap();
                let expr = from_can(env, field.loc_expr.value, procs, None);

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
                        .map(|(_, arg)| from_can(env, arg.value, procs, None));

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
                            .map(|(_, arg)| from_can(env, arg.value, procs, None)),
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

            let btree = crate::layout::record_fields_btree(
                env.arena,
                record_var,
                env.subs,
                env.pointer_size,
            );

            let mut index = None;
            let mut field_layouts = Vec::with_capacity_in(btree.len(), env.arena);

            for (current, (label, field_layout)) in btree.into_iter().enumerate() {
                field_layouts.push(field_layout);

                if label == field {
                    index = Some(current);
                }
            }

            let record = arena.alloc(from_can(env, loc_expr.value, procs, None));

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
            let elem_content = subs.get_without_compacting(elem_var).content;
            let elem_layout = match elem_content {
                // We have to special-case the empty list, because trying to
                // compute a layout for an unbound var won't work.
                Content::FlexVar(_) => Layout::Builtin(Builtin::EmptyList),
                content => match Layout::from_content(arena, content, env.subs, env.pointer_size) {
                    Ok(layout) => layout,
                    Err(()) => {
                        panic!("TODO gracefully handle List with invalid element layout");
                    }
                },
            };

            let mut elems = Vec::with_capacity_in(loc_elems.len(), arena);

            for loc_elem in loc_elems {
                elems.push(from_can(env, loc_elem.value, procs, None));
            }

            Expr::Array {
                elem_layout,
                elems: elems.into_bump_slice(),
            }
        }
        RuntimeError(error) => Expr::RuntimeError(env.arena.alloc(format!("{:?}", error))),
        other => panic!("TODO convert canonicalized {:?} to mono::Expr", other),
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
                        let symbol = env.fresh_symbol();
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
                let symbol = env.fresh_symbol();
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
                // Extract Procs, but discard the resulting Expr::Load.
                // That Load looks up the pointer, which we won't use here!
                from_can(env, loc_expr.value, procs, Some(*symbol));

                continue;
            }
        }

        // If it wasn't specifically an Identifier & Closure, proceed as normal.
        let mono_pattern = from_can_pattern(env, &loc_pattern.value);

        let layout = Layout::from_var(env.arena, def.expr_var, env.subs, env.pointer_size)
            .expect("invalid layout");

        match &mono_pattern {
            Pattern::Identifier(symbol) => {
                stored.push((
                    *symbol,
                    layout.clone(),
                    from_can(env, loc_expr.value, procs, None),
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

                let symbol = env.fresh_symbol();
                stored.push((
                    symbol,
                    layout.clone(),
                    from_can(env, loc_expr.value, procs, None),
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
    let ret = from_can(env, ret_expr.value, procs, None);

    if stored.is_empty() {
        ret
    } else {
        Expr::Store(stored.into_bump_slice(), arena.alloc(ret))
    }
}

fn from_can_when<'a>(
    env: &mut Env<'a, '_>,
    cond_var: Variable,
    expr_var: Variable,
    region: Region,
    loc_cond: Located<roc_can::expr::Expr>,
    mut branches: std::vec::Vec<roc_can::expr::WhenBranch>,
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

        let cond_layout = Layout::from_var(env.arena, cond_var, env.subs, env.pointer_size)
            .unwrap_or_else(|err| panic!("TODO turn this into a RuntimeError {:?}", err));
        let cond_symbol = env.fresh_symbol();
        let cond = from_can(env, loc_cond.value, procs, None);
        stored.push((cond_symbol, cond_layout.clone(), cond));

        // NOTE this will still store shadowed names.
        // that's fine: the branch throws a runtime error anyway
        let ret = match store_pattern(env, &mono_pattern, cond_symbol, cond_layout, &mut stored) {
            Ok(_) => from_can(env, first.value.value, procs, None),
            Err(message) => Expr::RuntimeError(env.arena.alloc(message)),
        };

        Expr::Store(stored.into_bump_slice(), arena.alloc(ret))
    } else {
        let cond_layout = Layout::from_var(env.arena, cond_var, env.subs, env.pointer_size)
            .unwrap_or_else(|err| panic!("TODO turn this into a RuntimeError {:?}", err));

        let cond = from_can(env, loc_cond.value, procs, None);
        let cond_symbol = env.fresh_symbol();

        let mut loc_branches = std::vec::Vec::new();
        let mut opt_branches = std::vec::Vec::new();

        for when_branch in branches {
            let mono_expr = from_can(env, when_branch.value.value, procs, None);

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
                            let expr = from_can(env, loc_guard.value, procs, None);
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

        let ret_layout = Layout::from_var(env.arena, expr_var, env.subs, env.pointer_size)
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
) -> Expr<'a> {
    // create specialized procedure to call

    // If we need to specialize the body, this will get populated with the info
    // we need to do that. This is defined outside the procs.get_user_defined(...) call
    // because if we tried to specialize the body inside that match, we would
    // get a borrow checker error about trying to borrow `procs` as mutable
    // while there is still an active immutable borrow.
    #[allow(clippy::type_complexity)]
    let opt_specialize_body: Option<(
        ContentHash,
        Variable,
        roc_can::expr::Expr,
        Vec<'a, Symbol>,
    )>;

    let specialized_proc_name = if let Some(partial_proc) = procs.get_user_defined(proc_name) {
        let content_hash = ContentHash::from_var(fn_var, env.subs);

        if let Some(specialization) = partial_proc.specializations.get(&content_hash) {
            opt_specialize_body = None;

            // a specialization with this type hash already exists, use its symbol
            specialization.0
        } else {
            opt_specialize_body = Some((
                content_hash,
                partial_proc.annotation,
                partial_proc.body.clone(),
                partial_proc.patterns.clone(),
            ));

            // generate a symbol for this specialization
            env.fresh_symbol()
        }
    } else {
        opt_specialize_body = None;

        // This happens for built-in symbols (they are never defined as a Closure)
        procs.insert_builtin(proc_name);
        proc_name
    };

    if let Some((content_hash, annotation, body, loc_patterns)) = opt_specialize_body {
        // register proc, so specialization doesn't loop infinitely
        procs.insert_specialization(proc_name, content_hash, specialized_proc_name, None);

        let arg_vars = loc_args.iter().map(|v| v.0).collect::<std::vec::Vec<_>>();

        let proc = specialize_proc_body(
            env,
            procs,
            fn_var,
            ret_var,
            specialized_proc_name,
            &arg_vars,
            &loc_patterns,
            annotation,
            body,
        )
        .ok();

        procs.insert_specialization(proc_name, content_hash, specialized_proc_name, proc);
    }

    // generate actual call
    let mut args = Vec::with_capacity_in(loc_args.len(), env.arena);

    for (var, loc_arg) in loc_args {
        let layout = Layout::from_var(&env.arena, var, &env.subs, env.pointer_size)
            .unwrap_or_else(|err| panic!("TODO gracefully handle bad layout: {:?}", err));

        args.push((from_can(env, loc_arg.value, procs, None), layout));
    }

    Expr::CallByName(specialized_proc_name, args.into_bump_slice())
}

#[allow(clippy::too_many_arguments)]
fn specialize_proc_body<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    fn_var: Variable,
    ret_var: Variable,
    proc_name: Symbol,
    loc_args: &[Variable],
    pattern_symbols: &[Symbol],
    annotation: Variable,
    body: roc_can::expr::Expr,
) -> Result<Proc<'a>, ()> {
    // unify the called function with the specialized signature, then specialize the function body
    let snapshot = env.subs.snapshot();
    let unified = roc_unify::unify::unify(env.subs, annotation, fn_var);
    debug_assert!(matches!(unified, roc_unify::unify::Unified::Success(_)));
    let specialized_body = from_can(env, body, procs, None);
    // reset subs, so we don't get type errors when specializing for a different signature
    env.subs.rollback_to(snapshot);

    let mut proc_args = Vec::with_capacity_in(loc_args.len(), &env.arena);

    for (arg_var, arg_name) in loc_args.iter().zip(pattern_symbols.iter()) {
        let layout = Layout::from_var(&env.arena, *arg_var, env.subs, env.pointer_size)?;
        proc_args.push((layout, *arg_name));
    }

    let ret_layout = Layout::from_var(&env.arena, ret_var, env.subs, env.pointer_size)
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

        NumLiteral(var, num) => match to_int_or_float(env.subs, *var) {
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
                        alternatives: vec![Ctor {
                            name: tag_name.clone(),
                            arity: 0,
                        }],
                    },
                },
                BoolUnion { ttrue, ffalse } => Pattern::BitLiteral {
                    value: tag_name == &ttrue,
                    tag_name: tag_name.clone(),
                    union: Union {
                        alternatives: vec![
                            Ctor {
                                name: ttrue,
                                arity: 0,
                            },
                            Ctor {
                                name: ffalse,
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
                    for tag_name in &tag_names {
                        ctors.push(Ctor {
                            name: tag_name.clone(),
                            arity: 0,
                        })
                    }

                    let union = crate::pattern::Union {
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
                        alternatives: vec![Ctor {
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
                    for (tag_name, args) in &tags {
                        ctors.push(Ctor {
                            name: tag_name.clone(),
                            // don't include tag discriminant in arity
                            arity: args.len() - 1,
                        })
                    }

                    let union = crate::pattern::Union {
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

            let btree = crate::layout::record_fields_btree(
                env.arena,
                *whole_var,
                env.subs,
                env.pointer_size,
            );

            let mut field_layouts = Vec::with_capacity_in(btree.len(), env.arena);

            for (label, field_layout) in btree.into_iter() {
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
                            symbol: env.fresh_symbol(),
                            layout: field_layout.clone(),
                            guard: Some(Pattern::Underscore),
                        });
                    }
                } else {
                    // insert underscore pattern
                    mono_destructs.push(RecordDestruct {
                        label: label.clone(),
                        symbol: env.fresh_symbol(),
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

pub fn specialize_equality<'a>(
    arena: &'a Bump,
    lhs: Expr<'a>,
    rhs: Expr<'a>,
    layout: Layout<'a>,
) -> Expr<'a> {
    let symbol = match &layout {
        Layout::Builtin(builtin) => match builtin {
            Builtin::Int64 => Symbol::INT_EQ_I64,
            Builtin::Float64 => Symbol::FLOAT_EQ,
            Builtin::Byte => Symbol::INT_EQ_I8,
            Builtin::Bool => Symbol::INT_EQ_I1,
            other => todo!("Cannot yet compare for equality {:?}", other),
        },
        other => todo!("Cannot yet compare for equality {:?}", other),
    };

    Expr::CallByName(
        symbol,
        arena.alloc([(lhs, layout.clone()), (rhs, layout.clone())]),
    )
}
