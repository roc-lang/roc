use crate::can::pattern::Pattern;
use crate::can::{
    self,
    ident::{Lowercase, TagName},
};
use crate::collections::MutMap;
use crate::module::symbol::Symbol;
use crate::mono::layout::{Builtin, Layout};
use crate::region::Located;
use crate::subs::{Subs, Variable};
use bumpalo::collections::Vec;
use bumpalo::Bump;

pub type Procs<'a> = MutMap<Symbol, Option<Proc<'a>>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Proc<'a> {
    pub args: &'a [(Layout<'a>, Symbol, Variable)],
    pub body: Expr<'a>,
    pub closes_over: Layout<'a>,
    pub ret_var: Variable,
}

struct Env<'a> {
    pub arena: &'a Bump,
    pub subs: &'a Subs,
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
    Store(&'a [(Symbol, Variable, Expr<'a>)], &'a Expr<'a>),

    // Functions
    FunctionPointer(Symbol),
    CallByName(Symbol, &'a [Expr<'a>]),
    CallByPointer(&'a Expr<'a>, &'a [Expr<'a>], Variable),

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
        ret_var: Variable,
    },
    /// More than two conditional branches, e.g. a 3-way when-expression
    Branches {
        /// The left-hand side of the conditional. We compile this to LLVM once,
        /// then reuse it to test against each different compiled cond_rhs value.
        cond: &'a Expr<'a>,
        /// ( cond_rhs, pass, fail )
        branches: &'a [(Expr<'a>, Expr<'a>, Expr<'a>)],
        default: &'a Expr<'a>,
        ret_var: Variable,
    },
    /// Conditional branches for integers. These are more efficient.
    Switch {
        /// This *must* be an integer, because Switch potentially compiles to a jump table.
        cond: &'a Expr<'a>,
        cond_var: Variable,
        /// The u64 in the tuple will be compared directly to the condition Expr.
        /// If they are equal, this branch will be taken.
        branches: &'a [(u64, Expr<'a>)],
        /// If no other branches pass, this default branch will be taken.
        default_branch: &'a Expr<'a>,
        /// Each branch must return a value of this type.
        ret_var: Variable,
    },
    Tag {
        variant_var: Variable,
        ext_var: Variable,
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

    RuntimeError(&'a str),
}

impl<'a> Expr<'a> {
    pub fn new(
        arena: &'a Bump,
        subs: &'a Subs,
        can_expr: can::expr::Expr,
        procs: &mut Procs<'a>,
    ) -> Self {
        let env = Env { arena, subs };

        from_can(&env, can_expr, procs, None)
    }
}

fn from_can<'a>(
    env: &Env<'a>,
    can_expr: can::expr::Expr,
    procs: &mut Procs<'a>,
    name: Option<Symbol>,
) -> Expr<'a> {
    use crate::can::expr::Expr::*;
    use crate::can::pattern::Pattern::*;

    match can_expr {
        Int(_, val) => Expr::Int(val),
        Float(_, val) => Expr::Float(val),
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

        Closure(_, _symbol, _, loc_args, boxed_body) => {
            let (loc_body, ret_var) = *boxed_body;
            let name = name.unwrap_or_else(|| gen_closure_name(procs));

            add_closure(env, name, loc_body.value, ret_var, &loc_args, procs)
        }

        Call(boxed, loc_args, _) => {
            let (fn_var, loc_expr, _) = *boxed;
            let mut args = Vec::with_capacity_in(loc_args.len(), env.arena);

            for (_, loc_arg) in loc_args {
                args.push(from_can(env, loc_arg.value, procs, None));
            }

            match from_can(env, loc_expr.value, procs, None) {
                Expr::Load(proc_name) => Expr::CallByName(proc_name, args.into_bump_slice()),
                ptr => {
                    // Call by pointer - the closure was anonymous, e.g.
                    //
                    // ((\a -> a) 5)
                    //
                    // It might even be the anonymous result of a conditional:
                    //
                    // ((if x > 0 then \a -> a else \_ -> 0) 5)
                    Expr::CallByPointer(&*env.arena.alloc(ptr), args.into_bump_slice(), fn_var)
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

            let struct_content = subs.get_without_compacting(ext_var).content;
            let struct_layout = match Layout::from_content(arena, struct_content, subs) {
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

            let struct_content = subs.get_without_compacting(ext_var).content;
            let struct_layout = match Layout::from_content(arena, struct_content, subs) {
                Ok(layout) => layout,
                Err(()) => {
                    // Invalid field!
                    panic!("TODO gracefully handle Access with invalid struct_layout");
                }
            };

            let field_content = subs.get_without_compacting(field_var).content;
            let field_layout = match Layout::from_content(arena, field_content, subs) {
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

        other => panic!("TODO convert canonicalized {:?} to ll::Expr", other),
    }
}

fn add_closure<'a>(
    env: &Env<'a>,
    name: Symbol,
    can_body: can::expr::Expr,
    ret_var: Variable,
    loc_args: &[(Variable, Located<Pattern>)],
    procs: &mut Procs<'a>,
) -> Expr<'a> {
    let subs = &env.subs;
    let arena = env.arena;
    let mut proc_args = Vec::with_capacity_in(loc_args.len(), arena);

    for (arg_var, loc_arg) in loc_args.iter() {
        let content = subs.get_without_compacting(*arg_var).content;

        let layout = match Layout::from_content(arena, content, subs) {
            Ok(layout) => layout,
            Err(()) => {
                // Invalid closure!
                procs.insert(name.clone(), None);

                return Expr::FunctionPointer(name);
            }
        };

        let arg_name: Symbol = match &loc_arg.value {
            Pattern::Identifier(symbol) => *symbol,
            _ => {
                panic!("TODO determine arg_name for pattern {:?}", loc_arg.value);
            }
        };

        proc_args.push((layout, arg_name, *arg_var));
    }

    let proc = Proc {
        args: proc_args.into_bump_slice(),
        body: from_can(env, can_body, procs, None),
        closes_over: Layout::Struct(&[]),
        ret_var,
    };

    procs.insert(name.clone(), Some(proc));

    Expr::FunctionPointer(name)
}

fn store_pattern<'a>(
    env: &Env<'a>,
    can_pat: Pattern,
    can_expr: can::expr::Expr,
    var: Variable,
    procs: &mut Procs<'a>,
    stored: &mut Vec<'a, (Symbol, Variable, Expr<'a>)>,
) {
    use crate::can::pattern::Pattern::*;

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
        Identifier(symbol) => stored.push((symbol, var, from_can(env, can_expr, procs, None))),
        Underscore => {
            // Since _ is never read, it's safe to reassign it.
            stored.push((
                Symbol::UNDERSCORE,
                var,
                from_can(env, can_expr, procs, None),
            ))
        }
        _ => {
            panic!("TODO store_pattern for {:?}", can_pat);
        }
    }
}

fn gen_closure_name(procs: &Procs<'_>) -> Symbol {
    // Give the closure a name like "_0" or "_1".
    // We know procs.len() will be unique!
    panic!(
        "TODO generate a unique closure symbol, presumably by keeping the current module and generating a new unique IdentId for it. Previously was: format!(\"_{}\", procs.len()).into();"
    );
}

fn from_can_when<'a>(
    env: &Env<'a>,
    cond_var: Variable,
    expr_var: Variable,
    loc_cond: Located<can::expr::Expr>,
    branches: std::vec::Vec<(Located<can::pattern::Pattern>, Located<can::expr::Expr>)>,
    procs: &mut Procs<'a>,
) -> Expr<'a> {
    use crate::can::pattern::Pattern::*;

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
                (IntLiteral(int), IntLiteral(_)) | (IntLiteral(int), Underscore) => {
                    let cond_lhs = arena.alloc(from_can(env, loc_cond.value, procs, None));
                    let cond_rhs = arena.alloc(Expr::Int(*int));
                    let pass = arena.alloc(from_can(env, loc_then.value, procs, None));
                    let fail = arena.alloc(from_can(env, loc_else.value, procs, None));

                    Expr::Cond {
                        cond_layout: Layout::Builtin(Builtin::Int64),
                        cond_lhs,
                        cond_rhs,
                        pass,
                        fail,
                        ret_var: expr_var,
                    }
                }
                (FloatLiteral(float), FloatLiteral(_)) | (FloatLiteral(float), Underscore) => {
                    let cond_lhs = arena.alloc(from_can(env, loc_cond.value, procs, None));
                    let cond_rhs = arena.alloc(Expr::Float(*float));
                    let pass = arena.alloc(from_can(env, loc_then.value, procs, None));
                    let fail = arena.alloc(from_can(env, loc_else.value, procs, None));

                    Expr::Cond {
                        cond_layout: Layout::Builtin(Builtin::Float64),
                        cond_lhs,
                        cond_rhs,
                        pass,
                        fail,
                        ret_var: expr_var,
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
            let content = subs.get_without_compacting(cond_var).content;
            let layout = Layout::from_content(arena, content, subs)
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

                Expr::Switch {
                    cond: arena.alloc(cond),
                    branches: jumpable_branches.into_bump_slice(),
                    default_branch,
                    ret_var: expr_var,
                    cond_var,
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
