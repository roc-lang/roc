use crate::layout::{Builtin, Layout};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_can;
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::{Ident, Lowercase, TagName};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_region::all::{Located, Region};
use roc_types::subs::{Content, ContentHash, FlatType, Subs, Variable};

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

struct Env<'a, 'i> {
    pub arena: &'a Bump,
    pub subs: &'a mut Subs,
    pub home: ModuleId,
    pub ident_ids: &'i mut IdentIds,
    pub pointer_size: u32,
    symbol_counter: usize,
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
        cond: &'a Expr<'a>,
        cond_layout: Layout<'a>,
        // What to do if the condition either passes or fails
        pass: &'a Expr<'a>,
        fail: &'a Expr<'a>,
        ret_layout: Layout<'a>,
    },
    /// More than two conditional branches, e.g. a 3-way when-expression
    Branches {
        /// The left-hand side of the conditional. We compile this to LLVM once,
        /// then reuse it to test against each different compiled cond_rhs value.
        cond: &'a Expr<'a>,
        /// ( cond_rhs, pass, fail )
        branches: &'a [(Expr<'a>, Expr<'a>, Expr<'a>)],
        default: &'a Expr<'a>,
        ret_layout: Layout<'a>,
    },
    /// Conditional branches for integers. These are more efficient.
    Switch {
        /// This *must* be an integer, because Switch potentially compiles to a jump table.
        cond: &'a Expr<'a>,
        cond_layout: Layout<'a>,
        /// The u64 in the tuple will be compared directly to the condition Expr.
        /// If they are equal, this branch will be taken.
        branches: &'a [(u64, Expr<'a>)],
        /// If no other branches pass, this default branch will be taken.
        default_branch: &'a Expr<'a>,
        /// Each branch must return a value of this type.
        ret_layout: Layout<'a>,
    },
    Tag {
        tag_layout: Layout<'a>,
        name: TagName,
        arguments: &'a [Expr<'a>],
    },
    Struct {
        fields: &'a [(Lowercase, Expr<'a>)],
        layout: Layout<'a>,
    },
    Access {
        label: Lowercase,
        field_layout: Layout<'a>,
        struct_layout: Layout<'a>,
        record: &'a Expr<'a>,
    },

    Array {
        elem_layout: Layout<'a>,
        elems: &'a [Expr<'a>],
    },

    RuntimeError(&'a str),
}

impl<'a> Expr<'a> {
    pub fn new(
        arena: &'a Bump,
        subs: &'a mut Subs,
        can_expr: roc_can::expr::Expr,
        procs: &mut Procs<'a>,
        home: ModuleId,
        ident_ids: &mut IdentIds,
        pointer_size: u32,
    ) -> Self {
        let mut env = Env {
            arena,
            subs,
            home,
            ident_ids,
            pointer_size,
            symbol_counter: 0,
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

    for (pattern_var, pattern) in patterns.into_iter().rev() {
        let (new_symbol, new_body) = pattern_to_when(env, pattern_var, pattern, body_var, body);
        body = new_body;
        symbols.push(new_symbol);
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
    use roc_can::pattern::Pattern::*;

    match &pattern.value {
        Identifier(symbol) => (*symbol, body),
        Underscore => {
            // for underscore we generate a dummy Symbol
            (env.fresh_symbol(), body)
        }

        AppliedTag {..} | RecordDestructure {..} | Shadowed(_, _) | UnsupportedPattern(_) => {
            let symbol = env.fresh_symbol();

            let wrapped_body = When {
                cond_var: pattern_var,
                expr_var: body_var,
                loc_cond: Box::new(Located::at_zero(Var(symbol))),
                branches: vec![(pattern, body)],
            };

            (symbol, Located::at_zero(wrapped_body))
        }

        // These patters are refutable, and thus should never occur outside a `when` expression
        IntLiteral(_) | NumLiteral(_,_) | FloatLiteral(_) | StrLiteral(_) => {
            unreachable!("refutable pattern {:?} where irrefutable pattern is expected. This should never happen!", pattern.value)
        }

    }
}

fn from_can<'a>(
    env: &mut Env<'a, '_>,
    can_expr: roc_can::expr::Expr,
    procs: &mut Procs<'a>,
    name: Option<Symbol>,
) -> Expr<'a> {
    use roc_can::expr::Expr::*;
    use roc_can::pattern::Pattern::*;

    match can_expr {
        Num(var, num) => match to_int_or_float(env.subs, var) {
            IntOrFloat::IntType => Expr::Int(num),
            IntOrFloat::FloatType => Expr::Float(num as f64),
        },
        Int(_, num) => Expr::Int(num),
        Float(_, num) => Expr::Float(num),
        Str(string) | BlockStr(string) => Expr::Str(env.arena.alloc(string)),
        Var(symbol) => Expr::Load(symbol),
        LetNonRec(def, ret_expr, _, _) => {
            let arena = env.arena;
            let loc_pattern = def.loc_pattern;
            let loc_expr = def.loc_expr;
            let mut stored = Vec::with_capacity_in(1, arena);

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

                    // Discard this LetNonRec by replacing it with its ret_expr.
                    return from_can(env, ret_expr.value, procs, None);
                }
            }

            // If it wasn't specifically an Identifier & Closure, proceed as normal.
            let mono_pattern = from_can_pattern(env, loc_pattern.value);
            store_pattern(
                env,
                mono_pattern,
                loc_expr.value,
                def.expr_var,
                procs,
                &mut stored,
            );

            // At this point, it's safe to assume we aren't assigning a Closure to a def.
            // Extract Procs from the def body and the ret expression, and return the result!
            let ret = from_can(env, ret_expr.value, procs, None);

            Expr::Store(stored.into_bump_slice(), arena.alloc(ret))
        }

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
                    );

                    procs.insert_anonymous(symbol, opt_proc);

                    symbol
                }
            };

            Expr::FunctionPointer(symbol)
        }

        Call(boxed, loc_args, _) => {
            use IntOrFloat::*;

            let (fn_var, loc_expr, ret_var) = *boxed;

            // Optimization: have a cheap "is_builtin" check, that looks at the
            // module ID to see if it's possibly a builting symbol
            let specialize_builtin_functions = {
                |env: &mut Env<'a, '_>, symbol| match symbol {
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
                        match Layout::from_var(env.arena, loc_args[0].0, env.subs, env.pointer_size)
                        {
                            Ok(Layout::Builtin(builtin)) => match builtin {
                                Builtin::Int64 => Symbol::INT_EQ_I64,
                                Builtin::Float64 => Symbol::FLOAT_EQ,
                                Builtin::Bool(_, _) => Symbol::INT_EQ_I1,
                                Builtin::Byte(_) => Symbol::INT_EQ_I8,
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
            loc_cond,
            branches,
        } => from_can_when(env, cond_var, expr_var, *loc_cond, branches, procs),

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
                expr = Expr::Cond {
                    cond: env.arena.alloc(cond),
                    cond_layout: cond_layout.clone(),
                    pass: env.arena.alloc(then),
                    fail: env.arena.alloc(expr),
                    ret_layout: ret_layout.clone(),
                };
            }

            expr
        }

        Record {
            record_var, fields, ..
        } => {
            let arena = env.arena;
            let mut field_bodies = Vec::with_capacity_in(fields.len(), arena);

            for (label, field) in fields {
                let expr = from_can(env, field.loc_expr.value, procs, None);

                field_bodies.push((label, expr));
            }

            let struct_layout =
                match Layout::from_var(arena, record_var, env.subs, env.pointer_size) {
                    Ok(layout) => layout,
                    Err(()) => {
                        // Invalid field!
                        panic!("TODO gracefully handle Record with invalid struct_layout");
                    }
                };

            Expr::Struct {
                fields: field_bodies.into_bump_slice(),
                layout: struct_layout,
            }
        }

        Tag {
            variant_var,
            name,
            arguments: args,
            ..
        } => {
            let arena = env.arena;

            match Layout::from_var(arena, variant_var, &env.subs, env.pointer_size) {
                Ok(Layout::Builtin(Builtin::Bool(_smaller, larger))) => Expr::Bool(name == larger),
                Ok(Layout::Builtin(Builtin::Byte(tags))) => match tags.get(&name) {
                    Some(v) => Expr::Byte(*v),
                    None => panic!("Tag name is not part of the type"),
                },
                Ok(layout) => {
                    let mut arguments = Vec::with_capacity_in(args.len(), arena);

                    for (_, arg) in args {
                        arguments.push(from_can(env, arg.value, procs, None));
                    }

                    Expr::Tag {
                        tag_layout: layout,
                        name,
                        arguments: arguments.into_bump_slice(),
                    }
                }
                Err(()) => {
                    // Invalid field!
                    panic!("TODO gracefully handle Access with invalid struct_layout");
                }
            }
        }

        Access {
            record_var,
            field_var,
            field,
            loc_expr,
            ..
        } => {
            let arena = env.arena;

            let struct_layout =
                match Layout::from_var(arena, record_var, env.subs, env.pointer_size) {
                    Ok(layout) => layout,
                    Err(()) => {
                        // Invalid field!
                        panic!("TODO gracefully handle Access with invalid struct_layout");
                    }
                };

            let field_layout = match Layout::from_var(arena, field_var, env.subs, env.pointer_size)
            {
                Ok(layout) => layout,
                Err(()) => {
                    // Invalid field!
                    panic!("TODO gracefully handle Access with invalid field_layout");
                }
            };

            let record = arena.alloc(from_can(env, loc_expr.value, procs, None));

            Expr::Access {
                label: field,
                field_layout,
                struct_layout,
                record,
            }
        }

        List {
            elem_var,
            loc_elems,
        } => {
            let arena = env.arena;
            let elem_layout = match Layout::from_var(arena, elem_var, env.subs, env.pointer_size) {
                Ok(layout) => layout,
                Err(()) => {
                    panic!("TODO gracefully handle List with invalid element layout");
                }
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
        other => panic!("TODO convert canonicalized {:?} to mono::Expr", other),
    }
}

fn store_pattern<'a>(
    env: &mut Env<'a, '_>,
    can_pat: Pattern,
    can_expr: roc_can::expr::Expr,
    var: Variable,
    procs: &mut Procs<'a>,
    stored: &mut Vec<'a, (Symbol, Layout<'a>, Expr<'a>)>,
) {
    use Pattern::*;

    let layout = match Layout::from_var(env.arena, var, env.subs, env.pointer_size) {
        Ok(layout) => layout,
        Err(()) => {
            panic!("TODO gen a runtime error here");
        }
    };

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
    match can_pat {
        Identifier(symbol) => stored.push((symbol, layout, from_can(env, can_expr, procs, None))),
        Underscore => {
            // Since _ is never read, it's safe to reassign it.
            stored.push((
                Symbol::UNDERSCORE,
                layout,
                from_can(env, can_expr, procs, None),
            ))
        }
        _ => {
            panic!("TODO store_pattern for {:?}", can_pat);
        }
    }
}

fn from_can_when<'a>(
    env: &mut Env<'a, '_>,
    cond_var: Variable,
    expr_var: Variable,
    loc_cond: Located<roc_can::expr::Expr>,
    branches: std::vec::Vec<(
        Located<roc_can::pattern::Pattern>,
        Located<roc_can::expr::Expr>,
    )>,
    procs: &mut Procs<'a>,
) -> Expr<'a> {
    use Pattern::*;

    match branches.len() {
        0 => {
            // A when-expression with no branches is a runtime error.
            // We can't know what to return!
            panic!("TODO compile a 0-branch when-expression to a RuntimeError");
        }
        1 => {
            // A when-expression with exactly 1 branch is essentially a LetNonRec.
            // As such, we can compile it direcly to a Store.
            let arena = env.arena;
            let mut stored = Vec::with_capacity_in(1, arena);
            let (loc_when_pattern, loc_branch) = branches.into_iter().next().unwrap();

            let mono_pattern = from_can_pattern(env, loc_when_pattern.value);
            store_pattern(
                env,
                mono_pattern,
                loc_cond.value,
                cond_var,
                procs,
                &mut stored,
            );

            let ret = from_can(env, loc_branch.value, procs, None);

            Expr::Store(stored.into_bump_slice(), arena.alloc(ret))
        }
        2 => {
            // A when-expression with exactly 2 branches compiles to a Cond.
            let arena = env.arena;
            let mut iter = branches.into_iter();
            let (can_loc_when_pat1, loc_then) = iter.next().unwrap();
            let (can_loc_when_pat2, loc_else) = iter.next().unwrap();

            let when_pat1 = from_can_pattern(env, can_loc_when_pat1.value);
            let when_pat2 = from_can_pattern(env, can_loc_when_pat2.value);

            let cond_layout = Layout::Builtin(Builtin::Bool(
                TagName::Global("False".into()),
                TagName::Global("True".into()),
            ));

            match (&when_pat1, &when_pat2) {
                (IntLiteral(int), Underscore) => {
                    let cond_lhs = from_can(env, loc_cond.value, procs, None);
                    let cond_rhs = Expr::Int(*int);

                    let cond = arena.alloc(Expr::CallByName(
                        Symbol::INT_EQ_I64,
                        arena.alloc([
                            (cond_lhs, Layout::Builtin(Builtin::Int64)),
                            (cond_rhs, Layout::Builtin(Builtin::Int64)),
                        ]),
                    ));

                    let pass = arena.alloc(from_can(env, loc_then.value, procs, None));
                    let fail = arena.alloc(from_can(env, loc_else.value, procs, None));
                    let ret_layout = Layout::from_var(arena, expr_var, env.subs, env.pointer_size)
                        .unwrap_or_else(|err| {
                            panic!("TODO turn this into a RuntimeError {:?}", err)
                        });

                    Expr::Cond {
                        cond_layout,
                        cond,
                        pass,
                        fail,
                        ret_layout,
                    }
                }
                (FloatLiteral(float), Underscore) => {
                    let cond_lhs = from_can(env, loc_cond.value, procs, None);
                    let cond_rhs = Expr::Float(*float);

                    let cond = arena.alloc(Expr::CallByName(
                        Symbol::FLOAT_EQ,
                        arena.alloc([
                            (cond_lhs, Layout::Builtin(Builtin::Float64)),
                            (cond_rhs, Layout::Builtin(Builtin::Float64)),
                        ]),
                    ));

                    let pass = arena.alloc(from_can(env, loc_then.value, procs, None));
                    let fail = arena.alloc(from_can(env, loc_else.value, procs, None));
                    let ret_layout = Layout::from_var(arena, expr_var, env.subs, env.pointer_size)
                        .unwrap_or_else(|err| {
                            panic!("TODO turn this into a RuntimeError {:?}", err)
                        });

                    Expr::Cond {
                        cond_layout,
                        cond,
                        pass,
                        fail,
                        ret_layout,
                    }
                }
                _ => {
                    panic!("TODO handle more conds");
                }
            }
        }
        _ => {
            // This is a when-expression with 3+ branches.
            let arena = env.arena;
            let cond = from_can(env, loc_cond.value, procs, None);
            let subs = &env.subs;
            let layout = Layout::from_var(arena, cond_var, subs, env.pointer_size)
                .unwrap_or_else(|_| panic!("TODO generate a runtime error in from_can_when here!"));

            // We can Switch on integers and tags, because they both have
            // representations that work as integer values.
            //
            // TODO we can also Switch on record fields if we're pattern matching
            // on a record field that's also Switchable.
            //
            // TODO we can also convert floats to integer representations.
            let is_switchable = match layout {
                Layout::Builtin(Builtin::Int64) => true,
                Layout::Builtin(Builtin::Bool(_, _)) => true,
                Layout::Builtin(Builtin::Byte(_)) => true,
                _ => false,
            };

            // If the condition is an Int or Float, we can potentially use
            // a Switch for more efficiency.
            if is_switchable {
                // These are integer literals or underscore patterns,
                // so they're eligible for user in a jump table.
                let mut jumpable_branches = Vec::with_capacity_in(branches.len(), arena);
                let mut opt_default_branch = None;

                for (loc_when_pat, loc_expr) in branches {
                    let mono_expr = from_can(env, loc_expr.value, procs, None);
                    let when_pat = from_can_pattern(env, loc_when_pat.value);

                    match &when_pat {
                        IntLiteral(int) => {
                            // Switch only compares the condition to the
                            // alternatives based on their bit patterns,
                            // so casting from i64 to u64 makes no difference here.
                            jumpable_branches.push((*int as u64, mono_expr));
                        }
                        BitLiteral(v) => jumpable_branches.push((*v as u64, mono_expr)),
                        EnumLiteral(v) => jumpable_branches.push((*v as u64, mono_expr)),
                        Identifier(symbol) => {
                            // Since this is an ident, it must be
                            // the last pattern in the `when`.
                            // We can safely treat this like an `_`
                            // except that we need to wrap this branch
                            // in a `Store` so the identifier is in scope!

                            // TODO does this evaluate `cond` twice?
                            let mono_with_store = Expr::Store(
                                arena.alloc([(*symbol, layout.clone(), cond.clone())]),
                                arena.alloc(mono_expr),
                            );

                            opt_default_branch = Some(arena.alloc(mono_with_store));
                        }
                        Underscore => {
                            // We should always have exactly one default branch!
                            debug_assert!(opt_default_branch.is_none());

                            opt_default_branch = Some(arena.alloc(mono_expr));
                        }
                        Shadowed(_, _) => {
                            panic!("TODO runtime error for shadowing in a pattern");
                        }
                        // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
                        UnsupportedPattern(_region) => {
                            panic!("TODO runtime error for unsupported pattern");
                        }
                        AppliedTag(_, _, _)
                        | StrLiteral(_)
                        | RecordDestructure(_, _)
                        | FloatLiteral(_) => {
                            // The type checker should have converted these mismatches into RuntimeErrors already!
                            if cfg!(debug_assertions) {
                                panic!("A type mismatch in a pattern was not converted to a runtime error: {:?}", when_pat);
                            } else {
                                unreachable!();
                            }
                        }
                    }
                }

                // If the default branch was never set, that means
                // our canonical Expr didn't have one. An earlier
                // step in the compilation process should have
                // ruled this out!
                debug_assert!(opt_default_branch.is_some());
                let default_branch = opt_default_branch.unwrap();

                let cond_layout = Layout::from_var(arena, cond_var, env.subs, env.pointer_size)
                    .unwrap_or_else(|err| {
                        panic!("TODO turn cond_layout into a RuntimeError {:?}", err)
                    });

                let ret_layout = Layout::from_var(arena, expr_var, env.subs, env.pointer_size)
                    .unwrap_or_else(|err| {
                        panic!("TODO turn ret_layout into a RuntimeError {:?}", err)
                    });

                Expr::Switch {
                    cond: arena.alloc(cond),
                    branches: jumpable_branches.into_bump_slice(),
                    default_branch,
                    ret_layout,
                    cond_layout,
                }
            } else {
                // /// More than two conditional branches, e.g. a 3-way when-expression
                // Expr::Branches {
                //     /// The left-hand side of the conditional. We compile this to LLVM once,
                //     /// then reuse it to test against each different compiled cond_rhs value.
                //     cond_lhs: &'a Expr<'a>,
                //     /// ( cond_rhs, pass, fail )
                //     branches: &'a [(Expr<'a>, Expr<'a>, Expr<'a>)],
                //     ret_var: Variable,
                // },
                panic!(
                    "TODO support when-expressions of 3+ branches whose conditions aren't integers."
                );
            }
        }
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
        );

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
) -> Option<Proc<'a>> {
    // unify the called function with the specialized signature, then specialize the function body
    let snapshot = env.subs.snapshot();
    let unified = roc_unify::unify::unify(env.subs, annotation, fn_var);
    debug_assert!(unified.mismatches.is_empty());
    let specialized_body = from_can(env, body, procs, None);
    // reset subs, so we don't get type errors when specializing for a different signature
    env.subs.rollback_to(snapshot);

    let mut proc_args = Vec::with_capacity_in(loc_args.len(), &env.arena);

    for (arg_var, arg_name) in loc_args.iter().zip(pattern_symbols.iter()) {
        let layout = match Layout::from_var(&env.arena, *arg_var, env.subs, env.pointer_size) {
            Ok(layout) => layout,
            Err(()) => {
                // Invalid closure!
                return None;
            }
        };

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

    Some(proc)
}

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'a> {
    Identifier(Symbol),
    AppliedTag(TagName, Vec<'a, Pattern<'a>>, Layout<'a>),
    BitLiteral(bool),
    EnumLiteral(u8),
    IntLiteral(i64),
    FloatLiteral(f64),
    StrLiteral(Box<str>),
    RecordDestructure(Vec<'a, RecordDestruct<'a>>, Layout<'a>),
    Underscore,

    // Runtime Exceptions
    Shadowed(Region, Located<Ident>),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordDestruct<'a> {
    pub label: Lowercase,
    pub symbol: Symbol,
    pub guard: Option<Pattern<'a>>,
}

fn from_can_pattern<'a>(
    env: &mut Env<'a, '_>,
    can_pattern: roc_can::pattern::Pattern,
) -> Pattern<'a> {
    use roc_can::pattern::Pattern::*;
    match can_pattern {
        Underscore => Pattern::Underscore,
        Identifier(symbol) => Pattern::Identifier(symbol),
        IntLiteral(v) => Pattern::IntLiteral(v),
        FloatLiteral(v) => Pattern::FloatLiteral(v),
        StrLiteral(v) => Pattern::StrLiteral(v),
        Shadowed(region, ident) => Pattern::Shadowed(region, ident),
        UnsupportedPattern(region) => Pattern::UnsupportedPattern(region),

        NumLiteral(var, num) => match to_int_or_float(env.subs, var) {
            IntOrFloat::IntType => Pattern::IntLiteral(num),
            IntOrFloat::FloatType => Pattern::FloatLiteral(num as f64),
        },

        AppliedTag {
            whole_var,
            tag_name,
            arguments,
            ..
        } => match Layout::from_var(env.arena, whole_var, env.subs, env.pointer_size) {
            Ok(Layout::Builtin(Builtin::Bool(_bottom, top))) => {
                Pattern::BitLiteral(tag_name == top)
            }
            Ok(Layout::Builtin(Builtin::Byte(conversion))) => match conversion.get(&tag_name) {
                Some(index) => Pattern::EnumLiteral(*index),
                None => unreachable!("Tag must be in its own type"),
            },
            Ok(layout) => {
                let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);
                for (_, loc_pat) in arguments {
                    mono_args.push(from_can_pattern(env, loc_pat.value));
                }

                Pattern::AppliedTag(tag_name, mono_args, layout)
            }
            Err(()) => panic!("Invalid layout"),
        },

        RecordDestructure {
            whole_var,
            destructs,
            ..
        } => match Layout::from_var(env.arena, whole_var, env.subs, env.pointer_size) {
            Ok(layout) => {
                let mut mono_destructs = Vec::with_capacity_in(destructs.len(), env.arena);
                for loc_rec_des in destructs {
                    mono_destructs.push(from_can_record_destruct(env, loc_rec_des.value));
                }

                Pattern::RecordDestructure(mono_destructs, layout)
            }
            Err(()) => panic!("Invalid layout"),
        },
    }
}

fn from_can_record_destruct<'a>(
    env: &mut Env<'a, '_>,
    can_rd: roc_can::pattern::RecordDestruct,
) -> RecordDestruct<'a> {
    RecordDestruct {
        label: can_rd.label,
        symbol: can_rd.symbol,
        guard: match can_rd.guard {
            None => None,
            Some((_, loc_pattern)) => Some(from_can_pattern(env, loc_pattern.value)),
        },
    }
}
