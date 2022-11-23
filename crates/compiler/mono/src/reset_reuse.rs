//! This module inserts reset/reuse statements into the mono IR. These statements provide an
//! opportunity to reduce memory pressure by reusing memory slots of non-shared values. From the
//! introduction of the relevant paper:
//!
//! > [We] have added two additional instructions to our IR: `let y = reset x` and
//! > `let z = (reuse y in ctor_i w)`. The two instructions are used together; if `x`
//! > is a shared value, then `y` is set to a special reference, and the reuse instruction
//! > just allocates a new constructor value `ctor_i w`. If `x` is not shared, then reset
//! > decrements the reference counters of the components of `x`, and `y` is set to `x`.
//! > Then, reuse reuses the memory cell used by `x` to store the constructor value `ctor_i w`.
//!
//! See also
//! - [Counting Immutable Beans](https://arxiv.org/pdf/1908.05647.pdf) (Ullrich and Moura, 2020)
//! - [The lean implementation](https://github.com/leanprover/lean4/blob/master/src/Lean/Compiler/IR/ResetReuse.lean)

use crate::inc_dec::{collect_stmt, occurring_variables_expr, JPLiveVarMap, LiveVarSet};
use crate::ir::{
    BranchInfo, Call, Expr, ListLiteralElement, Proc, Stmt, UpdateModeId, UpdateModeIds,
};
use crate::layout::{Layout, TagIdIntType, UnionLayout};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::MutSet;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

pub fn insert_reset_reuse<'a, 'i>(
    arena: &'a Bump,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    update_mode_ids: &'i mut UpdateModeIds,
    mut proc: Proc<'a>,
) -> Proc<'a> {
    let mut env = Env {
        arena,
        home,
        ident_ids,
        update_mode_ids,
        jp_live_vars: Default::default(),
    };

    let new_body = function_r(&mut env, arena.alloc(proc.body));
    proc.body = new_body.clone();

    proc
}

#[derive(Debug)]
struct CtorInfo<'a> {
    id: TagIdIntType,
    layout: UnionLayout<'a>,
}

fn may_reuse(tag_layout: UnionLayout, tag_id: TagIdIntType, other: &CtorInfo) -> bool {
    if tag_layout != other.layout {
        return false;
    }

    // we should not get here if the tag we matched on is represented as NULL
    debug_assert!(!tag_layout.tag_is_null(other.id as _));

    // furthermore, we can only use the memory if the tag we're creating is non-NULL
    !tag_layout.tag_is_null(tag_id)
}

#[derive(Debug)]
struct Env<'a, 'i> {
    arena: &'a Bump,

    /// required for creating new `Symbol`s
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    update_mode_ids: &'i mut UpdateModeIds,

    jp_live_vars: JPLiveVarMap,
}

impl<'a, 'i> Env<'a, 'i> {
    fn unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        Symbol::new(self.home, ident_id)
    }
}

fn function_s<'a, 'i>(
    env: &mut Env<'a, 'i>,
    w: Opportunity,
    c: &CtorInfo<'a>,
    stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    use Stmt::*;

    let arena = env.arena;

    match stmt {
        Let(symbol, expr, layout, continuation) => match expr {
            Expr::Tag {
                tag_layout,
                tag_id,
                arguments,
            } if may_reuse(*tag_layout, *tag_id, c) => {
                // for now, always overwrite the tag ID just to be sure
                let update_tag_id = true;

                let new_expr = Expr::Reuse {
                    symbol: w.symbol,
                    update_mode: w.update_mode,
                    update_tag_id,
                    tag_layout: *tag_layout,
                    tag_id: *tag_id,
                    arguments,
                };
                let new_stmt = Let(*symbol, new_expr, *layout, continuation);

                arena.alloc(new_stmt)
            }
            _ => {
                let rest = function_s(env, w, c, continuation);
                let new_stmt = Let(*symbol, expr.clone(), *layout, rest);

                arena.alloc(new_stmt)
            }
        },
        Join {
            id,
            parameters,
            body,
            remainder,
        } => {
            let id = *id;
            let body: &Stmt = body;
            let new_body = function_s(env, w, c, body);

            let new_join = if std::ptr::eq(body, new_body) || body == new_body {
                // the join point body will consume w
                Join {
                    id,
                    parameters,
                    body: new_body,
                    remainder,
                }
            } else {
                let new_remainder = function_s(env, w, c, remainder);

                Join {
                    id,
                    parameters,
                    body,
                    remainder: new_remainder,
                }
            };

            arena.alloc(new_join)
        }
        Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            let mut new_branches = Vec::with_capacity_in(branches.len(), arena);
            new_branches.extend(branches.iter().map(|(tag, info, body)| {
                let new_body = function_s(env, w, c, body);

                (*tag, info.clone(), new_body.clone())
            }));

            let new_default = function_s(env, w, c, default_branch.1);

            let new_switch = Switch {
                cond_symbol: *cond_symbol,
                cond_layout: *cond_layout,
                branches: new_branches.into_bump_slice(),
                default_branch: (default_branch.0.clone(), new_default),
                ret_layout: *ret_layout,
            };

            arena.alloc(new_switch)
        }
        Refcounting(op, continuation) => {
            let continuation: &Stmt = continuation;
            let new_continuation = function_s(env, w, c, continuation);

            if std::ptr::eq(continuation, new_continuation) || continuation == new_continuation {
                stmt
            } else {
                let new_refcounting = Refcounting(*op, new_continuation);

                arena.alloc(new_refcounting)
            }
        }

        Expect {
            condition,
            region,
            lookups,
            layouts,
            remainder,
        } => {
            let continuation: &Stmt = remainder;
            let new_continuation = function_s(env, w, c, continuation);

            if std::ptr::eq(continuation, new_continuation) || continuation == new_continuation {
                stmt
            } else {
                let new_refcounting = Expect {
                    condition: *condition,
                    region: *region,
                    lookups,
                    layouts,
                    remainder: new_continuation,
                };

                arena.alloc(new_refcounting)
            }
        }

        ExpectFx {
            condition,
            region,
            lookups,
            layouts,
            remainder,
        } => {
            let continuation: &Stmt = remainder;
            let new_continuation = function_s(env, w, c, continuation);

            if std::ptr::eq(continuation, new_continuation) || continuation == new_continuation {
                stmt
            } else {
                let new_refcounting = ExpectFx {
                    condition: *condition,
                    region: *region,
                    lookups,
                    layouts,
                    remainder: new_continuation,
                };

                arena.alloc(new_refcounting)
            }
        }

        Ret(_) | Jump(_, _) | Crash(..) => stmt,
    }
}

#[derive(Clone, Copy)]
struct Opportunity {
    symbol: Symbol,
    update_mode: UpdateModeId,
}

fn try_function_s<'a, 'i>(
    env: &mut Env<'a, 'i>,
    x: Symbol,
    c: &CtorInfo<'a>,
    stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    let w = Opportunity {
        symbol: env.unique_symbol(),
        update_mode: env.update_mode_ids.next_id(),
    };

    let new_stmt = function_s(env, w, c, stmt);

    if std::ptr::eq(stmt, new_stmt) || stmt == new_stmt {
        stmt
    } else {
        insert_reset(env, w, x, c.layout, new_stmt)
    }
}

fn insert_reset<'a>(
    env: &mut Env<'a, '_>,
    w: Opportunity,
    x: Symbol,
    union_layout: UnionLayout<'a>,
    mut stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    use crate::ir::Expr::*;

    let mut stack = vec![];

    while let Stmt::Let(symbol, expr, expr_layout, rest) = stmt {
        match &expr {
            StructAtIndex { .. } | GetTagId { .. } | UnionAtIndex { .. } => {
                stack.push((symbol, expr, expr_layout));
                stmt = rest;
            }

            ExprBox { .. } | ExprUnbox { .. } => {
                // TODO
                break;
            }

            Literal(_)
            | Call(_)
            | Tag { .. }
            | Struct(_)
            | Array { .. }
            | EmptyArray
            | Reuse { .. }
            | Reset { .. }
            | RuntimeErrorFunction(_) => break,
        }
    }

    let reset_expr = Expr::Reset {
        symbol: x,
        update_mode: w.update_mode,
    };

    let layout = Layout::Union(union_layout);

    stmt = env
        .arena
        .alloc(Stmt::Let(w.symbol, reset_expr, layout, stmt));

    for (symbol, expr, expr_layout) in stack.into_iter().rev() {
        stmt = env
            .arena
            .alloc(Stmt::Let(*symbol, expr.clone(), *expr_layout, stmt));
    }

    stmt
}

fn function_d_finalize<'a, 'i>(
    env: &mut Env<'a, 'i>,
    x: Symbol,
    c: &CtorInfo<'a>,
    output: (&'a Stmt<'a>, bool),
) -> &'a Stmt<'a> {
    let (stmt, x_live_in_stmt) = output;
    if x_live_in_stmt {
        stmt
    } else {
        try_function_s(env, x, c, stmt)
    }
}

fn function_d_main<'a, 'i>(
    env: &mut Env<'a, 'i>,
    x: Symbol,
    c: &CtorInfo<'a>,
    stmt: &'a Stmt<'a>,
) -> (&'a Stmt<'a>, bool) {
    use Stmt::*;

    let arena = env.arena;

    match stmt {
        Let(symbol, expr, layout, continuation) => {
            match expr {
                Expr::Tag { arguments, .. } if arguments.iter().any(|s| *s == x) => {
                    // If the scrutinee `x` (the one that is providing memory) is being
                    // stored in a constructor, then reuse will probably not be able to reuse memory at runtime.
                    // It may work only if the new cell is consumed, but we ignore this case.
                    (stmt, true)
                }
                _ => {
                    let (b, found) = function_d_main(env, x, c, continuation);

                    // NOTE the &b != continuation is not found in the Lean source, but is required
                    // otherwise we observe the same symbol being reset twice
                    let mut result = MutSet::default();
                    if found
                        || {
                            occurring_variables_expr(expr, &mut result);
                            !result.contains(&x)
                        }
                        || &b != continuation
                    {
                        let let_stmt = Let(*symbol, expr.clone(), *layout, b);

                        (arena.alloc(let_stmt), found)
                    } else {
                        let b = try_function_s(env, x, c, b);
                        let let_stmt = Let(*symbol, expr.clone(), *layout, b);

                        (arena.alloc(let_stmt), found)
                    }
                }
            }
        }
        Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            if has_live_var(&env.jp_live_vars, stmt, x) {
                // if `x` is live in `stmt`, we recursively process each branch
                let mut new_branches = Vec::with_capacity_in(branches.len(), arena);

                for (tag, info, body) in branches.iter() {
                    let temp = function_d_main(env, x, c, body);
                    let new_body = function_d_finalize(env, x, c, temp);

                    new_branches.push((*tag, info.clone(), new_body.clone()));
                }

                let new_default = {
                    let (info, body) = default_branch;
                    let temp = function_d_main(env, x, c, body);
                    let new_body = function_d_finalize(env, x, c, temp);

                    (info.clone(), new_body)
                };

                let new_switch = Switch {
                    cond_symbol: *cond_symbol,
                    cond_layout: *cond_layout,
                    branches: new_branches.into_bump_slice(),
                    default_branch: new_default,
                    ret_layout: *ret_layout,
                };

                (arena.alloc(new_switch), true)
            } else {
                (stmt, false)
            }
        }
        Refcounting(modify_rc, continuation) => {
            let (b, found) = function_d_main(env, x, c, continuation);

            if found || modify_rc.get_symbol() != x {
                let refcounting = Refcounting(*modify_rc, b);

                (arena.alloc(refcounting), found)
            } else {
                let b = try_function_s(env, x, c, b);
                let refcounting = Refcounting(*modify_rc, b);

                (arena.alloc(refcounting), found)
            }
        }

        Expect {
            condition,
            region,
            lookups,
            layouts,
            remainder,
        } => {
            let (b, found) = function_d_main(env, x, c, remainder);

            if found || *condition != x {
                let refcounting = Expect {
                    condition: *condition,
                    region: *region,
                    lookups,
                    layouts,
                    remainder: b,
                };

                (arena.alloc(refcounting), found)
            } else {
                let b = try_function_s(env, x, c, b);

                let refcounting = Expect {
                    condition: *condition,
                    region: *region,
                    lookups,
                    layouts,
                    remainder: b,
                };

                (arena.alloc(refcounting), found)
            }
        }
        ExpectFx {
            condition,
            region,
            lookups,
            layouts,
            remainder,
        } => {
            let (b, found) = function_d_main(env, x, c, remainder);

            if found || *condition != x {
                let refcounting = ExpectFx {
                    condition: *condition,
                    region: *region,
                    lookups,
                    layouts,
                    remainder: b,
                };

                (arena.alloc(refcounting), found)
            } else {
                let b = try_function_s(env, x, c, b);

                let refcounting = ExpectFx {
                    condition: *condition,
                    region: *region,
                    lookups,
                    layouts,
                    remainder: b,
                };

                (arena.alloc(refcounting), found)
            }
        }
        Join {
            id,
            parameters,
            body,
            remainder,
        } => {
            env.jp_live_vars.insert(*id, LiveVarSet::default());

            let body_live_vars = collect_stmt(body, &env.jp_live_vars, LiveVarSet::default());

            env.jp_live_vars.insert(*id, body_live_vars);

            let (b, found) = function_d_main(env, x, c, remainder);

            let (v, _found) = function_d_main(env, x, c, body);

            env.jp_live_vars.remove(id);

            // If `found' == true`, then `Dmain b` must also have returned `(b, true)` since
            // we assume the IR does not have dead join points. So, if `x` is live in `j` (i.e., `v`),
            // then it must also live in `b` since `j` is reachable from `b` with a `jmp`.
            // On the other hand, `x` may be live in `b` but dead in `j` (i.e., `v`). -/
            let new_join = Join {
                id: *id,
                parameters,
                body: v,
                remainder: b,
            };

            (arena.alloc(new_join), found)
        }
        Ret(_) | Jump(_, _) | Crash(..) => (stmt, has_live_var(&env.jp_live_vars, stmt, x)),
    }
}

fn function_d<'a, 'i>(
    env: &mut Env<'a, 'i>,
    x: Symbol,
    c: &CtorInfo<'a>,
    stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    let temp = function_d_main(env, x, c, stmt);

    function_d_finalize(env, x, c, temp)
}

fn function_r_branch_body<'a, 'i>(
    env: &mut Env<'a, 'i>,
    info: &BranchInfo<'a>,
    body: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    let temp = function_r(env, body);

    match info {
        BranchInfo::None => temp,
        BranchInfo::Constructor {
            scrutinee,
            layout,
            tag_id,
        } => match layout {
            Layout::Union(UnionLayout::NonRecursive(_)) => temp,
            Layout::Union(union_layout) if !union_layout.tag_is_null(*tag_id) => {
                let ctor_info = CtorInfo {
                    layout: *union_layout,
                    id: *tag_id,
                };
                function_d(env, *scrutinee, &ctor_info, temp)
            }
            _ => temp,
        },
    }
}

fn function_r<'a, 'i>(env: &mut Env<'a, 'i>, stmt: &'a Stmt<'a>) -> &'a Stmt<'a> {
    use Stmt::*;

    let arena = env.arena;

    match stmt {
        Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            let mut new_branches = Vec::with_capacity_in(branches.len(), arena);

            for (tag, info, body) in branches.iter() {
                let new_body = function_r_branch_body(env, info, body);

                new_branches.push((*tag, info.clone(), new_body.clone()));
            }

            let new_default = {
                let (info, body) = default_branch;

                let new_body = function_r_branch_body(env, info, body);

                (info.clone(), new_body)
            };

            let new_switch = Switch {
                cond_symbol: *cond_symbol,
                cond_layout: *cond_layout,
                branches: new_branches.into_bump_slice(),
                default_branch: new_default,
                ret_layout: *ret_layout,
            };

            arena.alloc(new_switch)
        }

        Join {
            id,
            parameters,
            body,
            remainder,
        } => {
            env.jp_live_vars.insert(*id, LiveVarSet::default());

            let body_live_vars = collect_stmt(body, &env.jp_live_vars, LiveVarSet::default());

            env.jp_live_vars.insert(*id, body_live_vars);

            let b = function_r(env, remainder);

            let v = function_r(env, body);

            env.jp_live_vars.remove(id);

            let join = Join {
                id: *id,
                parameters,
                body: v,
                remainder: b,
            };

            arena.alloc(join)
        }

        Let(symbol, expr, layout, continuation) => {
            let b = function_r(env, continuation);

            arena.alloc(Let(*symbol, expr.clone(), *layout, b))
        }
        Refcounting(modify_rc, continuation) => {
            let b = function_r(env, continuation);

            arena.alloc(Refcounting(*modify_rc, b))
        }

        Expect {
            condition,
            region,
            lookups,
            layouts,
            remainder,
        } => {
            let b = function_r(env, remainder);

            let expect = Expect {
                condition: *condition,
                region: *region,
                lookups,
                layouts,
                remainder: b,
            };

            arena.alloc(expect)
        }

        ExpectFx {
            condition,
            region,
            lookups,
            layouts,
            remainder,
        } => {
            let b = function_r(env, remainder);

            let expect = ExpectFx {
                condition: *condition,
                region: *region,
                lookups,
                layouts,
                remainder: b,
            };

            arena.alloc(expect)
        }

        Ret(_) | Jump(_, _) | Crash(..) => {
            // terminals
            stmt
        }
    }
}

fn has_live_var<'a>(jp_live_vars: &JPLiveVarMap, stmt: &'a Stmt<'a>, needle: Symbol) -> bool {
    use Stmt::*;

    match stmt {
        Let(s, e, _, c) => {
            debug_assert_ne!(*s, needle);
            has_live_var_expr(e, needle) || has_live_var(jp_live_vars, c, needle)
        }
        Switch { cond_symbol, .. } if *cond_symbol == needle => true,
        Switch {
            branches,
            default_branch,
            ..
        } => {
            has_live_var(jp_live_vars, default_branch.1, needle)
                || branches
                    .iter()
                    .any(|(_, _, body)| has_live_var(jp_live_vars, body, needle))
        }
        Ret(s) => *s == needle,
        Refcounting(modify_rc, cont) => {
            modify_rc.get_symbol() == needle || has_live_var(jp_live_vars, cont, needle)
        }
        Expect {
            condition,
            remainder,
            ..
        } => *condition == needle || has_live_var(jp_live_vars, remainder, needle),
        ExpectFx {
            condition,
            remainder,
            ..
        } => *condition == needle || has_live_var(jp_live_vars, remainder, needle),
        Join {
            id,
            parameters,
            body,
            remainder,
        } => {
            debug_assert!(parameters.iter().all(|p| p.symbol != needle));

            let mut jp_live_vars = jp_live_vars.clone();

            jp_live_vars.insert(*id, LiveVarSet::default());

            let body_live_vars = collect_stmt(body, &jp_live_vars, LiveVarSet::default());

            if body_live_vars.contains(&needle) {
                return true;
            }

            jp_live_vars.insert(*id, body_live_vars);

            has_live_var(&jp_live_vars, remainder, needle)
        }
        Jump(id, arguments) => {
            arguments.iter().any(|s| *s == needle) || jp_live_vars[id].contains(&needle)
        }
        Crash(m, _) => *m == needle,
    }
}

fn has_live_var_expr<'a>(expr: &'a Expr<'a>, needle: Symbol) -> bool {
    match expr {
        Expr::Literal(_) => false,
        Expr::Call(call) => has_live_var_call(call, needle),
        Expr::Array { elems: fields, .. } => {
            for element in fields.iter() {
                if let ListLiteralElement::Symbol(s) = element {
                    if *s == needle {
                        return true;
                    }
                }
            }

            false
        }
        Expr::Tag {
            arguments: fields, ..
        }
        | Expr::Struct(fields) => fields.iter().any(|s| *s == needle),
        Expr::StructAtIndex { structure, .. }
        | Expr::GetTagId { structure, .. }
        | Expr::UnionAtIndex { structure, .. } => *structure == needle,
        Expr::EmptyArray => false,
        Expr::Reuse {
            symbol, arguments, ..
        } => needle == *symbol || arguments.iter().any(|s| *s == needle),
        Expr::Reset { symbol, .. } => needle == *symbol,
        Expr::ExprBox { symbol, .. } => needle == *symbol,
        Expr::ExprUnbox { symbol, .. } => needle == *symbol,
        Expr::RuntimeErrorFunction(_) => false,
    }
}

fn has_live_var_call<'a>(call: &'a Call<'a>, needle: Symbol) -> bool {
    call.arguments.iter().any(|s| *s == needle)
}
