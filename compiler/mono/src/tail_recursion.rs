#![allow(clippy::manual_map)]

use crate::ir::{CallType, Expr, JoinPointId, Param, Stmt};
use crate::layout::Layout;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_module::symbol::Symbol;

/// Make tail calls into loops (using join points)
///
/// e.g.
///
/// > factorial n accum = if n == 1 then accum else factorial (n - 1) (n * accum)
///
/// becomes
///
/// ```elm
/// factorial n1 accum1 =
///     let joinpoint j n accum =
///             if n == 1 then
///                 accum
///             else
///                 jump j (n - 1) (n * accum)
///
///     in
///         jump j n1 accum1
/// ```
///
/// This will effectively compile into a loop in llvm, and
/// won't grow the call stack for each iteration
pub fn make_tail_recursive<'a>(
    arena: &'a Bump,
    id: JoinPointId,
    needle: Symbol,
    stmt: Stmt<'a>,
    args: &'a [(Layout<'a>, Symbol, Symbol)],
    ret_layout: Layout,
) -> Option<Stmt<'a>> {
    let allocated = arena.alloc(stmt);
    let get_arg_layouts = || args.iter().map(|l| &l.0);

    let new_stmt = insert_jumps(arena, allocated, id, needle, get_arg_layouts, ret_layout)?;

    // if we did not early-return, jumps were inserted, we must now add a join point

    let params = Vec::from_iter_in(
        args.iter().map(|(layout, symbol, _)| Param {
            symbol: *symbol,
            layout: *layout,
            borrow: true,
        }),
        arena,
    )
    .into_bump_slice();

    // TODO could this be &[]?
    let args = Vec::from_iter_in(args.iter().map(|t| t.2), arena).into_bump_slice();

    let jump = arena.alloc(Stmt::Jump(id, args));

    let join = Stmt::Join {
        id,
        remainder: jump,
        parameters: params,
        body: new_stmt,
    };

    Some(join)
}

fn fn_eq<'a>(
    needle: Symbol,
    needle_args: impl ExactSizeIterator<Item = &'a Layout<'a>>,
    needle_ret: Layout,
    f: Symbol,
    f_args: &[Layout<'a>],
    f_ret: Layout,
) -> bool {
    needle == f && needle_args.eq(f_args.iter()) && needle_ret == f_ret
}

fn insert_jumps<'a, I, F>(
    arena: &'a Bump,
    stmt: &'a Stmt<'a>,
    goal_id: JoinPointId,
    needle: Symbol,
    get_needle_args: F,
    needle_ret: Layout,
) -> Option<&'a Stmt<'a>>
where
    I: ExactSizeIterator<Item = &'a Layout<'a>>,
    F: Fn() -> I + Copy,
{
    use Stmt::*;

    match stmt {
        Let(
            symbol,
            Expr::Call(crate::ir::Call {
                call_type:
                    CallType::ByName {
                        name: fsym,
                        ret_layout,
                        arg_layouts,
                        ..
                    },
                arguments,
            }),
            _,
            Stmt::Ret(rsym),
        ) if fn_eq(
            needle,
            get_needle_args(),
            needle_ret,
            *fsym,
            arg_layouts,
            **ret_layout,
        ) && symbol == rsym =>
        {
            // replace the call and return with a jump

            let jump = Stmt::Jump(goal_id, arguments);

            Some(arena.alloc(jump))
        }

        Let(symbol, expr, layout, cont) => {
            let opt_cont = insert_jumps(arena, cont, goal_id, needle, get_needle_args, needle_ret);

            if opt_cont.is_some() {
                let cont = opt_cont.unwrap_or(cont);

                Some(arena.alloc(Let(*symbol, expr.clone(), *layout, cont)))
            } else {
                None
            }
        }

        Join {
            id,
            parameters,
            remainder,
            body: continuation,
        } => {
            let opt_remainder = insert_jumps(
                arena,
                remainder,
                goal_id,
                needle,
                get_needle_args,
                needle_ret,
            );
            let opt_continuation = insert_jumps(
                arena,
                continuation,
                goal_id,
                needle,
                get_needle_args,
                needle_ret,
            );

            if opt_remainder.is_some() || opt_continuation.is_some() {
                let remainder = opt_remainder.unwrap_or(remainder);
                let continuation = opt_continuation.unwrap_or(*continuation);

                Some(arena.alloc(Join {
                    id: *id,
                    parameters,
                    remainder,
                    body: continuation,
                }))
            } else {
                None
            }
        }
        Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            let opt_default = insert_jumps(
                arena,
                default_branch.1,
                goal_id,
                needle,
                get_needle_args,
                needle_ret,
            );

            let mut did_change = false;

            let opt_branches = Vec::from_iter_in(
                branches.iter().map(|(label, info, branch)| {
                    match insert_jumps(arena, branch, goal_id, needle, get_needle_args, needle_ret)
                    {
                        None => None,
                        Some(branch) => {
                            did_change = true;
                            Some((*label, info.clone(), branch.clone()))
                        }
                    }
                }),
                arena,
            );

            if opt_default.is_some() || did_change {
                let default_branch = (
                    default_branch.0.clone(),
                    opt_default.unwrap_or(default_branch.1),
                );

                let branches = if did_change {
                    let new = Vec::from_iter_in(
                        opt_branches.into_iter().zip(branches.iter()).map(
                            |(opt_branch, branch)| match opt_branch {
                                None => branch.clone(),
                                Some(new_branch) => new_branch,
                            },
                        ),
                        arena,
                    );

                    new.into_bump_slice()
                } else {
                    branches
                };

                Some(arena.alloc(Switch {
                    cond_symbol: *cond_symbol,
                    cond_layout: *cond_layout,
                    default_branch,
                    branches,
                    ret_layout: *ret_layout,
                }))
            } else {
                None
            }
        }
        Refcounting(modify, cont) => {
            match insert_jumps(arena, cont, goal_id, needle, get_needle_args, needle_ret) {
                Some(cont) => Some(arena.alloc(Refcounting(*modify, cont))),
                None => None,
            }
        }

        Expect {
            condition,
            region,
            lookups,
            layouts,
            remainder,
        } => match insert_jumps(
            arena,
            remainder,
            goal_id,
            needle,
            get_needle_args,
            needle_ret,
        ) {
            Some(cont) => Some(arena.alloc(Expect {
                condition: *condition,
                region: *region,
                lookups,
                layouts,
                remainder: cont,
            })),
            None => None,
        },

        Ret(_) => None,
        Jump(_, _) => None,
        RuntimeError(_) => None,
    }
}
