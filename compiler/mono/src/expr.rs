use crate::layout::{Builtin, Layout};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_can;
use roc_can::pattern::Pattern;
use roc_collections::all::MutMap;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_region::all::Located;
use roc_types::subs::{Content, FlatType, Subs, Variable};

pub type Procs<'a> = MutMap<Symbol, Option<Proc<'a>>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Proc<'a> {
    pub args: &'a [(Layout<'a>, Symbol)],
    pub body: Expr<'a>,
    pub closes_over: Layout<'a>,
    pub ret_layout: Layout<'a>,
}

struct Env<'a, 'i> {
    pub arena: &'a Bump,
    pub subs: &'a Subs,
    pub home: ModuleId,
    pub ident_ids: &'i mut IdentIds,
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
        cond_lhs: &'a Expr<'a>,
        cond_rhs: &'a Expr<'a>,
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
        ext_layout: Layout<'a>,
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
        subs: &'a Subs,
        can_expr: roc_can::expr::Expr,
        procs: &mut Procs<'a>,
        home: ModuleId,
        ident_ids: &mut IdentIds,
    ) -> Self {
        let mut env = Env {
            arena,
            subs,
            home,
            ident_ids,
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
            store_pattern(
                env,
                loc_pattern.value,
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

        Closure(_, _, _, loc_args, boxed_body) => {
            let (loc_body, ret_var) = *boxed_body;
            let symbol =
                name.unwrap_or_else(|| gen_closure_name(procs, &mut env.ident_ids, env.home));

            add_closure(env, symbol, loc_body.value, ret_var, &loc_args, procs)
        }

        Call(boxed, loc_args, _) => {
            let (fn_var, loc_expr, _) = *boxed;

            match from_can(env, loc_expr.value, procs, None) {
                Expr::Load(proc_name) => {
                    // Some functions can potentially mutate in-place.
                    // If we have one of those, switch to the in-place version if appropriate.
                    match proc_name {
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

                                    call_by_name(env, procs, new_name, loc_args)
                                }
                                _ => call_by_name(env, procs, proc_name, loc_args),
                            }
                        }
                        _ => call_by_name(env, procs, proc_name, loc_args),
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

                    let layout =
                        Layout::from_var(env.arena, fn_var, env.subs).unwrap_or_else(|err| {
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

        Record(ext_var, fields) => {
            let subs = env.subs;
            let arena = env.arena;
            let mut field_bodies = Vec::with_capacity_in(fields.len(), arena);

            for (label, field) in fields {
                let expr = from_can(env, field.loc_expr.value, procs, None);

                field_bodies.push((label, expr));
            }

            let struct_layout = match Layout::from_var(arena, ext_var, subs) {
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

        Access {
            ext_var,
            field_var,
            field,
            ..
        } => {
            let subs = env.subs;
            let arena = env.arena;

            let struct_layout = match Layout::from_var(arena, ext_var, subs) {
                Ok(layout) => layout,
                Err(()) => {
                    // Invalid field!
                    panic!("TODO gracefully handle Access with invalid struct_layout");
                }
            };

            let field_layout = match Layout::from_var(arena, field_var, subs) {
                Ok(layout) => layout,
                Err(()) => {
                    // Invalid field!
                    panic!("TODO gracefully handle Access with invalid field_layout");
                }
            };

            Expr::Access {
                label: field,
                field_layout,
                struct_layout,
            }
        }

        List {
            elem_var,
            loc_elems,
        } => {
            let subs = env.subs;
            let arena = env.arena;
            let elem_layout = match Layout::from_var(arena, elem_var, subs) {
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

fn add_closure<'a>(
    env: &mut Env<'a, '_>,
    symbol: Symbol,
    can_body: roc_can::expr::Expr,
    ret_var: Variable,
    loc_args: &[(Variable, Located<Pattern>)],
    procs: &mut Procs<'a>,
) -> Expr<'a> {
    let subs = &env.subs;
    let arena = env.arena;
    let mut proc_args = Vec::with_capacity_in(loc_args.len(), arena);

    for (arg_var, loc_arg) in loc_args.iter() {
        let layout = match Layout::from_var(arena, *arg_var, subs) {
            Ok(layout) => layout,
            Err(()) => {
                // Invalid closure!
                procs.insert(symbol, None);

                return Expr::FunctionPointer(symbol);
            }
        };

        let arg_name: Symbol = match &loc_arg.value {
            Pattern::Identifier(symbol) => *symbol,
            _ => {
                panic!("TODO determine arg_name for pattern {:?}", loc_arg.value);
            }
        };

        proc_args.push((layout, arg_name));
    }

    let ret_layout = Layout::from_var(arena, ret_var, subs)
        .unwrap_or_else(|err| panic!("TODO handle invalid function {:?}", err));

    let proc = Proc {
        args: proc_args.into_bump_slice(),
        body: from_can(env, can_body, procs, None),
        closes_over: Layout::Struct(&[]),
        ret_layout,
    };

    procs.insert(symbol, Some(proc));

    Expr::FunctionPointer(symbol)
}

fn store_pattern<'a>(
    env: &mut Env<'a, '_>,
    can_pat: Pattern,
    can_expr: roc_can::expr::Expr,
    var: Variable,
    procs: &mut Procs<'a>,
    stored: &mut Vec<'a, (Symbol, Layout<'a>, Expr<'a>)>,
) {
    use roc_can::pattern::Pattern::*;

    let layout = match Layout::from_var(env.arena, var, env.subs) {
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

fn gen_closure_name(procs: &Procs<'_>, ident_ids: &mut IdentIds, home: ModuleId) -> Symbol {
    let ident_id = ident_ids.add(format!("_{}", procs.len()).into());

    Symbol::new(home, ident_id)
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
    use roc_can::pattern::Pattern::*;

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

            store_pattern(
                env,
                loc_when_pattern.value,
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
            let (loc_when_pat1, loc_then) = iter.next().unwrap();
            let (loc_when_pat2, loc_else) = iter.next().unwrap();

            match (&loc_when_pat1.value, &loc_when_pat2.value) {
                (NumLiteral(var, num), NumLiteral(_, _)) | (NumLiteral(var, num), Underscore) => {
                    let cond_lhs = arena.alloc(from_can(env, loc_cond.value, procs, None));
                    let (builtin, cond_rhs_expr) = match to_int_or_float(env.subs, *var) {
                        IntOrFloat::IntType => (Builtin::Int64, Expr::Int(*num)),
                        IntOrFloat::FloatType => (Builtin::Float64, Expr::Float(*num as f64)),
                    };

                    let cond_rhs = arena.alloc(cond_rhs_expr);
                    let pass = arena.alloc(from_can(env, loc_then.value, procs, None));
                    let fail = arena.alloc(from_can(env, loc_else.value, procs, None));
                    let ret_layout =
                        Layout::from_var(arena, expr_var, env.subs).unwrap_or_else(|err| {
                            panic!("TODO turn this into a RuntimeError {:?}", err)
                        });

                    Expr::Cond {
                        cond_layout: Layout::Builtin(builtin),
                        cond_lhs,
                        cond_rhs,
                        pass,
                        fail,
                        ret_layout,
                    }
                }
                (IntLiteral(int), IntLiteral(_)) | (IntLiteral(int), Underscore) => {
                    let cond_lhs = arena.alloc(from_can(env, loc_cond.value, procs, None));
                    let cond_rhs = arena.alloc(Expr::Int(*int));
                    let pass = arena.alloc(from_can(env, loc_then.value, procs, None));
                    let fail = arena.alloc(from_can(env, loc_else.value, procs, None));
                    let ret_layout =
                        Layout::from_var(arena, expr_var, env.subs).unwrap_or_else(|err| {
                            panic!("TODO turn this into a RuntimeError {:?}", err)
                        });

                    Expr::Cond {
                        cond_layout: Layout::Builtin(Builtin::Int64),
                        cond_lhs,
                        cond_rhs,
                        pass,
                        fail,
                        ret_layout,
                    }
                }
                (FloatLiteral(float), FloatLiteral(_)) | (FloatLiteral(float), Underscore) => {
                    let cond_lhs = arena.alloc(from_can(env, loc_cond.value, procs, None));
                    let cond_rhs = arena.alloc(Expr::Float(*float));
                    let pass = arena.alloc(from_can(env, loc_then.value, procs, None));
                    let fail = arena.alloc(from_can(env, loc_else.value, procs, None));
                    let ret_layout =
                        Layout::from_var(arena, expr_var, env.subs).unwrap_or_else(|err| {
                            panic!("TODO turn this into a RuntimeError {:?}", err)
                        });

                    Expr::Cond {
                        cond_layout: Layout::Builtin(Builtin::Float64),
                        cond_lhs,
                        cond_rhs,
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
            let layout = Layout::from_var(arena, cond_var, subs)
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

                    match &loc_when_pat.value {
                        NumLiteral(var, num) => {
                            // This is jumpable iff it's an int
                            match to_int_or_float(env.subs, *var) {
                                IntOrFloat::IntType => {
                                    jumpable_branches.push((*num as u64, mono_expr));
                                }
                                IntOrFloat::FloatType => {
                                    // The type checker should have converted these mismatches into RuntimeErrors already!
                                    if cfg!(debug_assertions) {
                                        panic!("A type mismatch in a pattern was not converted to a runtime error: {:?}", loc_when_pat);
                                    } else {
                                        unreachable!();
                                    }
                                }
                            };
                        }
                        IntLiteral(int) => {
                            // Switch only compares the condition to the
                            // alternatives based on their bit patterns,
                            // so casting from i64 to u64 makes no difference here.
                            jumpable_branches.push((*int as u64, mono_expr));
                        }
                        Identifier(_symbol) => {
                            // Since this is an ident, it must be
                            // the last pattern in the `when`.
                            // We can safely treat this like an `_`
                            // except that we need to wrap this branch
                            // in a `Store` so the identifier is in scope!

                            opt_default_branch = Some(arena.alloc(if true {
                                // Using `if true` for this TODO panic to avoid a warning
                                panic!("TODO wrap this expr in an Expr::Store: {:?}", mono_expr)
                            } else {
                                mono_expr
                            }));
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
                                panic!("A type mismatch in a pattern was not converted to a runtime error: {:?}", loc_when_pat);
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

                let cond_layout =
                    Layout::from_var(arena, cond_var, env.subs).unwrap_or_else(|err| {
                        panic!("TODO turn cond_layout into a RuntimeError {:?}", err)
                    });
                let ret_layout =
                    Layout::from_var(arena, expr_var, env.subs).unwrap_or_else(|err| {
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
    proc_name: Symbol,
    loc_args: std::vec::Vec<(Variable, Located<roc_can::expr::Expr>)>,
) -> Expr<'a> {
    let mut args = Vec::with_capacity_in(loc_args.len(), env.arena);
    let subs = env.subs;
    let arena = env.arena;

    for (var, loc_arg) in loc_args {
        let layout = Layout::from_var(arena, var, subs)
            .unwrap_or_else(|err| panic!("TODO gracefully handle bad layout: {:?}", err));

        args.push((from_can(env, loc_arg.value, procs, None), layout));
    }

    Expr::CallByName(proc_name, args.into_bump_slice())
}
