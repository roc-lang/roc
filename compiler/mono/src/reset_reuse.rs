use crate::inc_dec::LocalContext;
use crate::ir::{Expr, Literal, Stmt};
use crate::layout::{Builtin, Layout};
use crate::live_vars;

use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::MutSet;
use roc_module::symbol::Symbol;

struct Env<'a, 'b> {
    env: crate::ir::Env<'a, 'b>,
    ctx: LocalContext<'a>,
}

fn may_reuse<'a>(x: Layout<'a>, y: Layout<'a>) -> bool {
    // a heuristic; we really only want the same "type" to be reused.
    // we could also compare actual stack size.
    x == y
}

fn try_function_s<'a>(
    env: &mut Env<'a, '_>,
    x: Symbol,
    layout: Layout<'a>,
    stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    let arena = env.env.arena;

    let w = env.env.unique_symbol();

    match function_s(env, w, stmt) {
        None => stmt,
        Some(new) => arena.alloc(Stmt::Let(w, Expr::Reset(x), layout, new)),
    }
}

fn function_s<'a>(env: &mut Env<'a, '_>, w: Symbol, stmt: &'a Stmt<'a>) -> Option<&'a Stmt<'a>> {
    use Stmt::*;

    let arena = env.env.arena;

    match stmt {
        _ => todo!(),
    }
}

fn function_d_main<'a>(
    env: &mut Env<'a, '_>,
    x: Symbol,
    layout: Layout<'a>,
    stmt: &'a Stmt<'a>,
) -> (Option<&'a Stmt<'a>>, bool) {
    /*
        let c = layout;

        use Stmt::*;

        let arena = env.env.arena;

        match stmt {
            Switch {
                cond_symbol,
                cond_layout,
                branches,
                default_branch,
                ret_layout,
            } => {
                // TODO only conditionally re-build the expression
                let live = live_vars::collect_stmt(stmt, env.ctx)

                let branches = Vec::from_iter_in(
                    branches.iter().map(|(label, branch)| {
                        let branch = function_r(env, branch).unwrap_or(branch);
                        let branch = function_d(env, *cond_symbol, cond_layout.clone(), branch)
                            .unwrap_or(branch);

                        (*label, branch.clone())
                    }),
                    arena,
                )
                .into_bump_slice();

                let default_branch = function_r(env, default_branch).unwrap_or(default_branch);
                let default_branch = function_d(env, *cond_symbol, cond_layout.clone(), default_branch)
                    .unwrap_or(default_branch);

                let switch = Switch {
                    cond_symbol: *cond_symbol,
                    branches,
                    default_branch,
                    cond_layout: cond_layout.clone(),
                    ret_layout: ret_layout.clone(),
                };

                Some(arena.alloc(switch))
            }

            Join { .. } => todo!(),

            Ret(_) | Jump(_, _) | RuntimeError(_) => None,

            Inc(x, b) => match function_r(env, b) {
                None => None,
                Some(new_b) => Some(arena.alloc(Inc(*x, new_b))),
            },

            Dec(x, b) => match function_r(env, b) {
                None => None,
                Some(new_b) => Some(arena.alloc(Dec(*x, new_b))),
            },

            Let(x, v, l, b) => match function_r(env, b) {
                None => None,
                Some(new_b) => Some(arena.alloc(Let(*x, v.clone(), l.clone(), new_b))),
            },

            Cond {
                cond_symbol,
                cond_layout,
                branching_symbol,
                branching_layout,
                pass,
                fail,
                ret_layout,
            } => {
                // TODO only conditionally re-build the expression

                let pass = function_r(env, pass).unwrap_or(pass);
                let pass = function_d(env, *cond_symbol, cond_layout.clone(), pass).unwrap_or(pass);

                let fail = function_r(env, fail).unwrap_or(fail);
                let fail = function_d(env, *cond_symbol, cond_layout.clone(), fail).unwrap_or(fail);

                let stmt = Cond {
                    cond_symbol: *cond_symbol,
                    cond_layout: cond_layout.clone(),
                    branching_symbol: *branching_symbol,
                    branching_layout: branching_layout.clone(),
                    pass,
                    fail,
                    ret_layout: ret_layout.clone(),
                };

                Some(arena.alloc(stmt))
            }
        }
    */
    panic!();
}

fn function_d<'a>(
    env: &mut Env<'a, '_>,
    x: Symbol,
    layout: Layout<'a>,
    stmt: &'a Stmt<'a>,
) -> Option<&'a Stmt<'a>> {
    let c = layout;
    todo!();
}

fn function_r<'a>(env: &mut Env<'a, '_>, stmt: &'a Stmt<'a>) -> Option<&'a Stmt<'a>> {
    use Stmt::*;

    let arena = env.env.arena;

    match stmt {
        Join { .. } => todo!(),

        Ret(_) | Jump(_, _) | RuntimeError(_) => None,

        Inc(x, b) => match function_r(env, b) {
            None => None,
            Some(new_b) => Some(arena.alloc(Inc(*x, new_b))),
        },

        Dec(x, b) => match function_r(env, b) {
            None => None,
            Some(new_b) => Some(arena.alloc(Dec(*x, new_b))),
        },

        Let(x, v, l, b) => match function_r(env, b) {
            None => None,
            Some(new_b) => Some(arena.alloc(Let(*x, v.clone(), l.clone(), new_b))),
        },

        Cond {
            cond_symbol,
            cond_layout,
            branching_symbol,
            branching_layout,
            pass,
            fail,
            ret_layout,
        } => {
            // TODO only conditionally re-build the expression

            let pass = function_r(env, pass).unwrap_or(pass);
            let pass = function_d(env, *cond_symbol, cond_layout.clone(), pass).unwrap_or(pass);

            let fail = function_r(env, fail).unwrap_or(fail);
            let fail = function_d(env, *cond_symbol, cond_layout.clone(), fail).unwrap_or(fail);

            let stmt = Cond {
                cond_symbol: *cond_symbol,
                cond_layout: cond_layout.clone(),
                branching_symbol: *branching_symbol,
                branching_layout: branching_layout.clone(),
                pass,
                fail,
                ret_layout: ret_layout.clone(),
            };

            Some(arena.alloc(stmt))
        }
        Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            // TODO only conditionally re-build the expression

            let branches = Vec::from_iter_in(
                branches.iter().map(|(label, branch)| {
                    let branch = function_r(env, branch).unwrap_or(branch);
                    let branch = function_d(env, *cond_symbol, cond_layout.clone(), branch)
                        .unwrap_or(branch);

                    (*label, branch.clone())
                }),
                arena,
            )
            .into_bump_slice();

            let default_branch = function_r(env, default_branch).unwrap_or(default_branch);
            let default_branch = function_d(env, *cond_symbol, cond_layout.clone(), default_branch)
                .unwrap_or(default_branch);

            let switch = Switch {
                cond_symbol: *cond_symbol,
                branches,
                default_branch,
                cond_layout: cond_layout.clone(),
                ret_layout: ret_layout.clone(),
            };

            Some(arena.alloc(switch))
        }
    }
}
