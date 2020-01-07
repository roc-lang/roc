use crate::can;
use crate::can::pattern::Pattern;
use crate::collections::MutMap;
use crate::gen::convert::content_to_basic_type;
use crate::ll::layout::Layout;
use crate::region::Located;
use crate::subs::{Subs, Variable};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::FunctionValue;
use inlinable_string::InlinableString;

pub type Procs<'a, 'ctx> = MutMap<InlinableString, (Option<Proc<'a>>, FunctionValue<'ctx>)>;

#[derive(Clone, Debug, PartialEq)]
pub struct Proc<'a> {
    pub args: &'a [(Layout<'a>, InlinableString, Variable)],
    pub body: Expr<'a>,
    pub closes_over: Layout<'a>,
    pub ret_var: Variable,
}

struct Env<'a, 'ctx> {
    arena: &'a Bump,
    subs: &'a Subs,
    module: &'ctx Module<'ctx>,
    context: &'ctx Context,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    // Literals
    Int(i64),
    Float(f64),
    Str(&'a str),

    // Load/Store
    Load(InlinableString),
    Store(&'a [(InlinableString, Variable, Expr<'a>)], &'a Expr<'a>),

    // Functions
    FunctionPointer(InlinableString),
    CallByPointer(&'a Expr<'a>, &'a [Expr<'a>]),
    CallByName(InlinableString, &'a [Expr<'a>]),

    // Exactly two conditional branches, e.g. if/else
    Cond {
        // The left-hand side of the conditional comparison and the right-hand side.
        // These are stored separately because there are different machine instructions
        // for e.g. "compare float and jump" vs. "compare integer and jump"
        cond_lhs: &'a Expr<'a>,
        cond_rhs: &'a Expr<'a>,
        // What to do if the condition either passes or fails
        pass: &'a Expr<'a>,
        fail: &'a Expr<'a>,
        ret_var: Variable,
    },
    /// More than two conditional branches, e.g. a 3-way when-expression
    Branches {
        /// The left-hand side of the conditional. We compile this to LLVM once,
        /// then reuse it to test against each different compiled cond_rhs value.
        cond_lhs: &'a Expr<'a>,
        /// ( cond_rhs, pass, fail )
        branches: &'a [(Expr<'a>, Expr<'a>, Expr<'a>)],
        ret_var: Variable,
    },

    Struct(&'a [(InlinableString, Expr<'a>)]),

    RuntimeError(&'a str),
}

impl<'a> Expr<'a> {
    pub fn new<'ctx>(
        arena: &'a Bump,
        subs: &'a Subs,
        module: &'ctx Module<'ctx>,
        context: &'ctx Context,
        can_expr: can::expr::Expr,
        procs: &mut Procs<'a, 'ctx>,
    ) -> Self {
        let env = Env {
            arena,
            subs,
            module,
            context,
        };

        from_can(&env, can_expr, procs, None)
    }
}

fn from_can<'a, 'ctx>(
    env: &Env<'a, 'ctx>,
    can_expr: can::expr::Expr,
    procs: &mut Procs<'a, 'ctx>,
    name: Option<InlinableString>,
) -> Expr<'a> {
    use crate::can::expr::Expr::*;
    use crate::can::pattern::Pattern::*;

    match can_expr {
        Int(_, val) => Expr::Int(val),
        Float(_, val) => Expr::Float(val),
        Str(string) | BlockStr(string) => Expr::Str(env.arena.alloc(string)),
        Var {
            resolved_symbol, ..
        } => Expr::Load(resolved_symbol.into()),
        LetNonRec(def, ret_expr, _) => {
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
            if let Identifier(name) = &loc_pattern.value {
                if let Closure(_, _, _, _, _) = &loc_expr.value {
                    // Extract Procs, but discard the resulting Expr::Load.
                    // That Load looks up the pointer, which we won't use here!
                    from_can(env, loc_expr.value, procs, Some(name.clone().into()));

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
            let name = name.unwrap_or_else(||
                    // Give the closure a name like "_0" or "_1".
                    // We know procs.len() will be unique!
                    format!("_{}", procs.len()).into());

            add_closure(env, name, loc_body.value, ret_var, &loc_args, procs)
        }

        Call(boxed, loc_args, _) => {
            let (_, loc_expr, _) = *boxed;
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
                    Expr::CallByPointer(&*env.arena.alloc(ptr), args.into_bump_slice())
                }
            }
        }

        When {
            cond_var,
            expr_var,
            loc_cond,
            branches,
        } => {
            debug_assert!(!branches.is_empty());

            if branches.len() == 2 {
                let arena = env.arena;
                let mut iter = branches.into_iter();
                let (loc_pat1, loc_then) = iter.next().unwrap();
                let (loc_pat2, loc_else) = iter.next().unwrap();

                match (&loc_pat1.value, &loc_pat2.value) {
                    (IntLiteral(int), IntLiteral(_)) | (IntLiteral(int), Underscore) => {
                        let cond_lhs = arena.alloc(from_can(env, loc_cond.value, procs, None));
                        let cond_rhs = arena.alloc(Expr::Int(*int));
                        let pass = arena.alloc(from_can(env, loc_then.value, procs, None));
                        let fail = arena.alloc(from_can(env, loc_else.value, procs, None));

                        Expr::Cond {
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
            } else if branches.len() == 1 {
                // A when-expression with exactly 1 branch is essentially a LetNonRec.
                // As such, we can compile it direcly to a Store.
                let arena = env.arena;
                let mut stored = Vec::with_capacity_in(1, arena);
                let (loc_pattern, loc_branch) = branches.into_iter().next().unwrap();

                store_pattern(
                    env,
                    loc_pattern.value,
                    loc_cond.value,
                    cond_var,
                    procs,
                    &mut stored,
                );

                let ret = from_can(env, loc_branch.value, procs, None);

                Expr::Store(stored.into_bump_slice(), arena.alloc(ret))
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
                panic!("TODO support when-expressions of more than 2 branches.");
            }
        }

        other => panic!("TODO convert canonicalized {:?} to ll::Expr", other),
    }
}

fn add_closure<'a, 'ctx>(
    env: &Env<'a, 'ctx>,
    name: InlinableString,
    can_body: can::expr::Expr,
    ret_var: Variable,
    loc_args: &[(Variable, Located<Pattern>)],
    procs: &mut Procs<'a, 'ctx>,
) -> Expr<'a> {
    let subs = &env.subs;
    let context = env.context;
    let arena = env.arena;
    let ret_content = subs.get_without_compacting(ret_var).content;
    let ret_type = content_to_basic_type(&ret_content, subs, context).unwrap_or_else(|err| {
        panic!(
            "Error converting function return value content to basic type: {:?}",
            err
        )
    });

    let mut arg_names = Vec::with_capacity_in(loc_args.len(), arena);
    let mut arg_basic_types = Vec::with_capacity_in(loc_args.len(), arena);
    let mut proc_args = Vec::with_capacity_in(loc_args.len(), arena);

    for (arg_var, loc_arg) in loc_args.iter() {
        let content = subs.get_without_compacting(*arg_var).content;

        arg_basic_types.push(
            content_to_basic_type(&content, subs, context).unwrap_or_else(|err| {
                panic!(
                    "Error converting function arg content to basic type: {:?}",
                    err
                )
            }),
        );

        let layout = match Layout::from_content(arena, content, subs) {
            Ok(layout) => layout,
            Err(()) => {
                return invalid_closure(env, name, ret_type, procs);
            }
        };

        let arg_name: InlinableString = match &loc_arg.value {
            Pattern::Identifier(name) => name.as_str().into(),
            _ => {
                panic!("TODO determine arg_name for pattern {:?}", loc_arg.value);
            }
        };

        arg_names.push(arg_name.clone());
        proc_args.push((layout, arg_name, *arg_var));
    }

    let fn_type = ret_type.fn_type(arg_basic_types.into_bump_slice(), false);
    let fn_val = env.module.add_function(&name, fn_type, None);
    let proc = Proc {
        args: proc_args.into_bump_slice(),
        body: from_can(env, can_body, procs, None),
        closes_over: Layout::Struct(&[]),
        ret_var,
    };

    procs.insert(name.clone(), (Some(proc), fn_val));

    Expr::FunctionPointer(name)
}

fn invalid_closure<'a, 'ctx>(
    env: &Env<'a, 'ctx>,
    name: InlinableString,
    ret_type: BasicTypeEnum<'ctx>,
    procs: &mut Procs<'a, 'ctx>,
) -> Expr<'a> {
    let fn_type = ret_type.fn_type(&[], false);
    let fn_val = env.module.add_function(&name, fn_type, None);

    procs.insert(name.clone(), (None, fn_val));

    Expr::FunctionPointer(name)
}

fn store_pattern<'a, 'ctx>(
    env: &Env<'a, 'ctx>,
    can_pat: Pattern,
    can_expr: can::expr::Expr,
    var: Variable,
    procs: &mut Procs<'a, 'ctx>,
    stored: &mut Vec<'a, (InlinableString, Variable, Expr<'a>)>,
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
        Identifier(name) => stored.push((name.into(), var, from_can(env, can_expr, procs, None))),
        Underscore => {
            // Since _ is never read, it's safe to reassign it.
            stored.push(("_".into(), var, from_can(env, can_expr, procs, None)))
        }
        _ => {
            panic!("TODO store_pattern for {:?}", can_pat);
        }
    }
}
