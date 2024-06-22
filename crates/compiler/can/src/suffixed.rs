#![allow(clippy::manual_map)]

use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_error_macros::internal_error;
use roc_module::called_via::CalledVia;
use roc_module::ident::ModuleName;
use roc_parse::ast::Expr::{self, *};
use roc_parse::ast::{is_expr_suffixed, Pattern, ValueDef, WhenBranch};
use roc_region::all::{Loc, Region};
use std::cell::Cell;

thread_local! {
    // we use a thread_local here so that tests consistently give the same pattern
    static SUFFIXED_ANSWER_COUNTER: Cell<usize> = Cell::new(0);
}

/// Provide an intermediate answer expression and pattern when unwrapping a
/// (sub) expression
///
/// e.g. `x = foo (bar!)` unwraps to `x = Task.await (bar) \#!a0 -> foo #!a0`
fn next_suffixed_answer_pattern(arena: &Bump) -> (Expr, Pattern) {
    // Use the thread-local counter
    SUFFIXED_ANSWER_COUNTER.with(|counter| {
        let count = counter.get();
        counter.set(count + 1);

        let answer_ident = arena.alloc(format!("#!a{}", count));

        (
            Expr::Var {
                module_name: "",
                ident: answer_ident,
            },
            Pattern::Identifier {
                ident: answer_ident.as_str(),
            },
        )
    })
}

#[derive(Debug)]
pub enum EUnwrapped<'a> {
    UnwrappedDefExpr(&'a Loc<Expr<'a>>),

    UnwrappedSubExpr {
        /// the unwrapped expression argument for Task.await
        sub_arg: &'a Loc<Expr<'a>>,

        /// the pattern for the closure
        sub_pat: &'a Loc<Pattern<'a>>,

        /// the expression to replace the unwrapped
        sub_new: &'a Loc<Expr<'a>>,
    },

    Malformed,
}

fn init_unwrapped_err<'a>(
    arena: &'a Bump,
    unwrapped_expr: &'a Loc<Expr<'a>>,
    maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match maybe_def_pat {
        Some(..) => {
            // we have a def pattern, so no need to generate a new pattern
            // as this should only be created in the first call from a def
            Err(EUnwrapped::UnwrappedDefExpr(unwrapped_expr))
        }
        None => {
            let (answer_var, answer_pat) = next_suffixed_answer_pattern(arena);
            let sub_new = arena.alloc(Loc::at(unwrapped_expr.region, answer_var));
            let sub_pat = arena.alloc(Loc::at(unwrapped_expr.region, answer_pat));

            Err(EUnwrapped::UnwrappedSubExpr {
                sub_arg: unwrapped_expr,
                sub_pat,
                sub_new,
            })
        }
    }
}

/// Descend through the AST and unwrap each suffixed expression
/// when an expression is unwrapped, we apply a `Task.await` and
/// then descend through the AST again until there are no more suffixed
/// expressions, or we hit an error
pub fn unwrap_suffixed_expression<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    let unwrapped_expression = {
        match loc_expr.value {
            Expr::TaskAwaitBang(sub_expr) => {
                let unwrapped_sub_expr = arena.alloc(Loc::at(loc_expr.region, *sub_expr));

                init_unwrapped_err(arena, unwrapped_sub_expr, maybe_def_pat)
            }

            Expr::Defs(..) => unwrap_suffixed_expression_defs_help(arena, loc_expr, maybe_def_pat),

            Expr::Apply(..) => {
                unwrap_suffixed_expression_apply_help(arena, loc_expr, maybe_def_pat)
            }

            Expr::When(..) => unwrap_suffixed_expression_when_help(arena, loc_expr, maybe_def_pat),

            Expr::If(..) => {
                unwrap_suffixed_expression_if_then_else_help(arena, loc_expr, maybe_def_pat)
            }

            Expr::Closure(..) => {
                unwrap_suffixed_expression_closure_help(arena, loc_expr, maybe_def_pat)
            }

            Expr::ParensAround(..) => {
                unwrap_suffixed_expression_parens_help(arena, loc_expr, maybe_def_pat)
            }

            Expr::SpaceBefore(..) | Expr::SpaceAfter(..) => {
                internal_error!(
                    "SpaceBefore and SpaceAfter should have been removed in desugar_expr"
                );
            }

            Expr::BinOps(..) => {
                internal_error!("BinOps should have been desugared in desugar_expr");
            }

            Expr::LowLevelDbg(dbg_src, arg, rest) => {
                if is_expr_suffixed(&arg.value) {
                    // we cannot unwrap a suffixed expression within dbg
                    // e.g. dbg (foo! "bar")
                    return Err(EUnwrapped::Malformed);
                }

                match unwrap_suffixed_expression(arena, rest, maybe_def_pat) {
                    Ok(unwrapped_expr) => {
                        let new_dbg = arena.alloc(Loc::at(
                            loc_expr.region,
                            LowLevelDbg(dbg_src, arg, unwrapped_expr),
                        ));
                        return Ok(new_dbg);
                    }
                    Err(EUnwrapped::UnwrappedDefExpr(unwrapped_expr)) => {
                        let new_dbg = arena.alloc(Loc::at(
                            loc_expr.region,
                            LowLevelDbg(dbg_src, arg, unwrapped_expr),
                        ));
                        Err(EUnwrapped::UnwrappedDefExpr(new_dbg))
                    }
                    Err(EUnwrapped::UnwrappedSubExpr {
                        sub_arg: unwrapped_expr,
                        sub_pat,
                        sub_new,
                    }) => {
                        let new_dbg = arena.alloc(Loc::at(
                            loc_expr.region,
                            LowLevelDbg(dbg_src, arg, unwrapped_expr),
                        ));
                        Err(EUnwrapped::UnwrappedSubExpr {
                            sub_arg: new_dbg,
                            sub_pat,
                            sub_new,
                        })
                    }
                    Err(EUnwrapped::Malformed) => Err(EUnwrapped::Malformed),
                }
            }

            Expr::Expect(condition, continuation) => {
                if is_expr_suffixed(&condition.value) {
                    // we cannot unwrap a suffixed expression within expect
                    // e.g. expect (foo! "bar")
                    return Err(EUnwrapped::Malformed);
                }

                match unwrap_suffixed_expression(arena, continuation, maybe_def_pat) {
                    Ok(unwrapped_expr) => {
                        let new_expect = arena
                            .alloc(Loc::at(loc_expr.region, Expect(condition, unwrapped_expr)));
                        return Ok(new_expect);
                    }
                    Err(EUnwrapped::UnwrappedDefExpr(unwrapped_expr)) => {
                        let new_expect = arena
                            .alloc(Loc::at(loc_expr.region, Expect(condition, unwrapped_expr)));
                        Err(EUnwrapped::UnwrappedDefExpr(new_expect))
                    }
                    Err(EUnwrapped::UnwrappedSubExpr {
                        sub_arg: unwrapped_expr,
                        sub_pat,
                        sub_new,
                    }) => {
                        let new_expect = arena
                            .alloc(Loc::at(loc_expr.region, Expect(condition, unwrapped_expr)));
                        Err(EUnwrapped::UnwrappedSubExpr {
                            sub_arg: new_expect,
                            sub_pat,
                            sub_new,
                        })
                    }
                    Err(EUnwrapped::Malformed) => Err(EUnwrapped::Malformed),
                }
            }

            // we only need to unwrap some expressions, leave the rest as is
            _ => Ok(loc_expr),
        }
    };

    // KEEP THIS HERE FOR DEBUGGING
    // USEFUL TO SEE THE UNWRAPPING
    // OF AST NODES AS THEY DESCEND
    // if is_expr_suffixed(&loc_expr.value) {
    //     dbg!(&maybe_def_pat, &loc_expr, &unwrapped_expression);
    // }

    unwrapped_expression
}

pub fn unwrap_suffixed_expression_parens_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    _maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::ParensAround(sub_loc_expr) => {
            // note we use `None` here as we always want to generate a new pattern from child expressions
            match unwrap_suffixed_expression(arena, arena.alloc(Loc::at_zero(*sub_loc_expr)), None)
            {
                Ok(new_expr) => {
                    let new_parens = arena.alloc(Loc::at(
                        loc_expr.region,
                        ParensAround(arena.alloc(new_expr.value)),
                    ));
                    Ok(new_parens)
                }
                Err(EUnwrapped::UnwrappedDefExpr(..)) => {
                    internal_error!("unreachable, child expressions from ParensAround should generate UnwrappedSubExpr instead");
                }
                Err(EUnwrapped::UnwrappedSubExpr {
                    sub_arg,
                    sub_pat,
                    sub_new,
                }) => {
                    let new_parens = arena.alloc(Loc::at(
                        loc_expr.region,
                        ParensAround(arena.alloc(sub_new.value)),
                    ));
                    Err(EUnwrapped::UnwrappedSubExpr {
                        sub_arg,
                        sub_pat,
                        sub_new: new_parens,
                    })
                }
                Err(err) => Err(err),
            }
        }
        _ => internal_error!("unreachable, expected a ParensAround node to be passed into unwrap_suffixed_expression_parens_help"),
    }
}

pub fn unwrap_suffixed_expression_closure_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    _maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::Closure(closure_args, closure_loc_ret) => {
            // note we use `None` here as we don't want to pass a DefExpr up and
            // unwrap the definition pattern for the closure
            match unwrap_suffixed_expression(arena, closure_loc_ret, None) {
                Ok(unwrapped_expr) => {
                    let new_closure = arena.alloc(Loc::at(loc_expr.region, Expr::Closure(closure_args, unwrapped_expr)));
                    Ok(new_closure)
                }
                Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                    let new_closure_loc_ret = apply_task_await(arena, loc_expr.region, sub_arg, sub_pat, sub_new);
                    let new_closure = arena.alloc(Loc::at(loc_expr.region, Expr::Closure(closure_args, new_closure_loc_ret)));
                    Ok(new_closure)
                }
                Err(err) => {
                    debug_assert!(false,"the closure Defs was malformd, got {:#?}", err);
                    Err(EUnwrapped::Malformed)
                }
            }
        }
        _ => internal_error!("unreachable, expected a Closure node to be passed into unwrap_suffixed_expression_closure_help"),
    }
}

pub fn unwrap_suffixed_expression_apply_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::Apply(function, apply_args, called_via) => {

            // Any suffixed arguments will be innermost, therefore we unwrap those first
            let local_args = arena.alloc_slice_copy(apply_args);
            for arg in local_args.iter_mut() {
                // Args are always expressions, don't pass `maybe_def_pat`
                match unwrap_suffixed_expression(arena, arg, None) {
                    Ok(new_arg) => {
                        *arg = new_arg;
                    }
                    Err(EUnwrapped::UnwrappedDefExpr(..)) => {
                        internal_error!("unreachable, unwrapped arg cannot be def expression as `None` was passed as pattern");
                    }
                    Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new: new_arg }) => {

                        *arg = new_arg;

                        let new_apply = arena.alloc(Loc::at(loc_expr.region, Apply(function, local_args, called_via)));
                        return Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new: new_apply});
                    }
                    Err(err) => return Err(err),
                }
            }

            // special case for when our Apply function is a suffixed Var (but not multiple suffixed)
            if let Expr::TaskAwaitBang(sub_expr) = function.value {
                let unwrapped_function = arena.alloc(Loc::at(
                    loc_expr.region,
                    *sub_expr,
                ));

                let new_apply = arena.alloc(Loc::at(loc_expr.region, Expr::Apply(unwrapped_function, local_args, called_via)));

                return init_unwrapped_err(arena, new_apply, maybe_def_pat);
            }

            // function is another expression
            match unwrap_suffixed_expression(arena, function, maybe_def_pat) {
                Ok(new_function) => {
                    let new_apply = arena.alloc(Loc::at(loc_expr.region, Expr::Apply(new_function, local_args, called_via)));
                    Ok(new_apply)
                }
                Err(EUnwrapped::UnwrappedDefExpr(unwrapped_function)) => {
                    let new_apply = arena.alloc(Loc::at(loc_expr.region, Expr::Apply(unwrapped_function, local_args, called_via)));
                    Err(EUnwrapped::UnwrappedDefExpr(new_apply))
                }
                Err(EUnwrapped::UnwrappedSubExpr { sub_arg: unwrapped_function, sub_pat, sub_new }) => {

                    let new_apply = arena.alloc(Loc::at(loc_expr.region, Expr::Apply(sub_new, local_args, called_via)));

                    Err(EUnwrapped::UnwrappedSubExpr { sub_arg: unwrapped_function, sub_pat, sub_new:new_apply})
                }
                Err(err) => Err(err)
            }
        }
        _ => internal_error!("unreachable, expected an Apply node to be passed into unwrap_suffixed_expression_apply_help"),
    }
}

/// Unwrap if-then-else statements
pub fn unwrap_suffixed_expression_if_then_else_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::If(if_thens, final_else_branch) => {
            for (index, if_then) in if_thens.iter().enumerate() {
                let (current_if_then_statement, current_if_then_expression) = if_then;

                // unwrap suffixed (innermost) expressions e.g. `if true then doThing! then ...`
                if is_expr_suffixed(&current_if_then_expression.value) {
                    // split if_thens around the current index
                    let (before, after) = roc_parse::ast::split_around(if_thens, index);

                    match unwrap_suffixed_expression(arena, current_if_then_expression, None) {
                        Ok(unwrapped_expression) => {
                            let mut new_if_thens = Vec::new_in(arena);

                            new_if_thens.extend(before);
                            new_if_thens.push((*current_if_then_statement, *unwrapped_expression));
                            new_if_thens.extend(after);

                            let new_if = arena.alloc(Loc::at(
                                loc_expr.region,
                                Expr::If(
                                    arena.alloc_slice_copy(new_if_thens.as_slice()),
                                    final_else_branch,
                                ),
                            ));

                            return unwrap_suffixed_expression(arena, new_if, maybe_def_pat);
                        }
                        Err(EUnwrapped::UnwrappedDefExpr(..)) => {
                            internal_error!("unexpected, unwrapped if-then-else Def expr should have intermediate answer as `None` was passed as pattern");
                        }
                        Err(EUnwrapped::UnwrappedSubExpr {
                            sub_arg,
                            sub_pat,
                            sub_new,
                        }) => {
                            let unwrapped_expression =
                                apply_task_await(arena, sub_arg.region, sub_arg, sub_pat, sub_new);

                            let mut new_if_thens = Vec::new_in(arena);

                            new_if_thens.extend(before);
                            new_if_thens.push((*current_if_then_statement, *unwrapped_expression));
                            new_if_thens.extend(after);

                            let new_if = arena.alloc(Loc::at(
                                loc_expr.region,
                                Expr::If(
                                    arena.alloc_slice_copy(new_if_thens.as_slice()),
                                    final_else_branch,
                                ),
                            ));

                            return unwrap_suffixed_expression(arena, new_if, maybe_def_pat);
                        }
                        Err(EUnwrapped::Malformed) => return Err(EUnwrapped::Malformed),
                    }
                }

                // unwrap suffixed statements e.g. `if isThing! then ...`
                // note we want to split and nest if-then's so we only run Task's
                // that are required
                if is_expr_suffixed(&current_if_then_statement.value) {
                    // split if_thens around the current index
                    let (before, after) = roc_parse::ast::split_around(if_thens, index);

                    match unwrap_suffixed_expression(arena, current_if_then_statement, None) {
                        Ok(unwrapped_statement) => {
                            let mut new_if_thens = Vec::new_in(arena);

                            new_if_thens.push((*unwrapped_statement, *current_if_then_expression));
                            new_if_thens.extend(after);

                            let new_if = arena.alloc(Loc::at(
                                loc_expr.region,
                                Expr::If(
                                    arena.alloc_slice_copy(new_if_thens.as_slice()),
                                    final_else_branch,
                                ),
                            ));

                            return unwrap_suffixed_expression(arena, new_if, maybe_def_pat);
                        }
                        Err(EUnwrapped::UnwrappedDefExpr(..)) => {
                            internal_error!("unexpected, unwrapped if-then-else Def expr should have intermediate answer as `None` was passed as pattern");
                        }
                        Err(EUnwrapped::UnwrappedSubExpr {
                            sub_arg,
                            sub_pat,
                            sub_new,
                        }) => {
                            if before.is_empty() {
                                let mut new_if_thens = Vec::new_in(arena);

                                new_if_thens.extend(before);
                                new_if_thens.push((*sub_new, *current_if_then_expression));
                                new_if_thens.extend(after);

                                let new_if = arena.alloc(Loc::at(
                                    loc_expr.region,
                                    Expr::If(
                                        arena.alloc_slice_copy(new_if_thens.as_slice()),
                                        final_else_branch,
                                    ),
                                ));

                                let unwrapped_if_then = apply_task_await(
                                    arena,
                                    sub_arg.region,
                                    sub_arg,
                                    sub_pat,
                                    new_if,
                                );

                                return unwrap_suffixed_expression(
                                    arena,
                                    unwrapped_if_then,
                                    maybe_def_pat,
                                );
                            } else {
                                let mut after_if_thens = Vec::new_in(arena);

                                after_if_thens.push((*sub_new, *current_if_then_expression));
                                after_if_thens.extend(after);

                                let after_if = arena.alloc(Loc::at(
                                    loc_expr.region,
                                    Expr::If(
                                        arena.alloc_slice_copy(after_if_thens.as_slice()),
                                        final_else_branch,
                                    ),
                                ));

                                let after_if_then = apply_task_await(
                                    arena,
                                    sub_arg.region,
                                    sub_arg,
                                    sub_pat,
                                    after_if,
                                );

                                let before_if_then = arena.alloc(Loc::at(
                                    loc_expr.region,
                                    Expr::If(before, after_if_then),
                                ));

                                return unwrap_suffixed_expression(
                                    arena,
                                    before_if_then,
                                    maybe_def_pat,
                                );
                            }
                        }
                        Err(EUnwrapped::Malformed) => return Err(EUnwrapped::Malformed),
                    }
                }
            }

            // check the final_else_branch
            match unwrap_suffixed_expression(arena, final_else_branch, None) {
                Ok(unwrapped_final_else) => {
                    return Ok(arena.alloc(Loc::at(
                        loc_expr.region,
                        Expr::If(if_thens, unwrapped_final_else),
                    )));
                }
                Err(EUnwrapped::UnwrappedDefExpr(..)) => {
                    internal_error!("unexpected, unwrapped if-then-else Def expr should have intermediate answer as `None` was passed as pattern");
                }
                Err(EUnwrapped::UnwrappedSubExpr {
                    sub_arg,
                    sub_pat,
                    sub_new,
                }) => {
                    let unwrapped_final_else =
                        apply_task_await(arena, sub_arg.region, sub_arg, sub_pat, sub_new);

                    let new_if = arena.alloc(Loc::at(
                        loc_expr.region,
                        Expr::If(if_thens, unwrapped_final_else),
                    ));

                    return unwrap_suffixed_expression(arena, new_if, maybe_def_pat);
                }
                Err(EUnwrapped::Malformed) => Err(EUnwrapped::Malformed),
            }
        }
        _ => internal_error!("unreachable, expected an If expression to desugar"),
    }
}

pub fn unwrap_suffixed_expression_when_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::When(condition, branches) => {

            // first unwrap any when branches values
            // e.g.
            // when foo is
            //     [] -> line! "bar"
            //      _ -> line! "baz"
            for (branch_index, WhenBranch{value: branch_loc_expr,patterns, guard}) in branches.iter().enumerate() {

                // if the branch isn't suffixed we can leave it alone
                if is_expr_suffixed(&branch_loc_expr.value) {
                    let unwrapped_branch_value = match unwrap_suffixed_expression(arena, branch_loc_expr, None) {
                        Ok(unwrapped_branch_value) => unwrapped_branch_value,
                        Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => apply_task_await(arena, branch_loc_expr.region, sub_arg, sub_pat, sub_new),
                        Err(..) => return Err(EUnwrapped::Malformed),
                    };

                    // TODO: unwrap guard

                    let new_branch = WhenBranch{value: *unwrapped_branch_value, patterns, guard: *guard};
                    let mut new_branches = Vec::new_in(arena);
                    let (before, rest) = branches.split_at(branch_index);
                    let after = &rest[1..];

                    new_branches.extend_from_slice(before);
                    new_branches.push(arena.alloc(new_branch));
                    new_branches.extend_from_slice(after);

                    let new_when = arena.alloc(Loc::at(loc_expr.region, Expr::When(condition, arena.alloc_slice_copy(new_branches.as_slice()))));

                    return unwrap_suffixed_expression(arena, new_when, maybe_def_pat);
                }
            }

            // then unwrap the when condition
            match unwrap_suffixed_expression(arena, condition, None) {
                Ok(unwrapped_condition) => {
                    let new_when = arena.alloc(Loc::at(loc_expr.region, Expr::When(unwrapped_condition, branches)));
                    Ok(new_when)
                }
                Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                    let new_when = arena.alloc(Loc::at(loc_expr.region, Expr::When(sub_new, branches)));
                    let applied_task_await = apply_task_await(arena,loc_expr.region,sub_arg,sub_pat,new_when);
                    Ok(applied_task_await)
                }
                Err(EUnwrapped::UnwrappedDefExpr(..))
                | Err(EUnwrapped::Malformed) => Err(EUnwrapped::Malformed)
            }

        }
        _ => internal_error!("unreachable, expected a When node to be passed into unwrap_suffixed_expression_defs_help"),
    }
}

pub fn unwrap_suffixed_expression_defs_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::Defs(defs, loc_ret) => {

            let mut local_defs = defs.clone();
            let tags = local_defs.tags.clone();

            // try an unwrap each def, if none can be unwrapped, then try to unwrap the loc_ret
            for (tag_index, type_or_value_def_index) in tags.iter().enumerate() {
                use ValueDef::*;

                let mut current_value_def = match type_or_value_def_index.split() {
                    Ok(..) => {
                        // ignore type definitions
                        continue;
                    },
                    Err(value_index) => *local_defs.value_defs.get(value_index.index()).unwrap(),
                };

                let maybe_suffixed_value_def = match current_value_def {
                    Annotation(..) | Dbg{..} | Expect{..} | ExpectFx{..} | Stmt(..) | ModuleImport{..} | IngestedFileImport(_) => None,
                    AnnotatedBody { body_pattern, body_expr, .. } => Some((body_pattern, body_expr)),
                    Body (def_pattern, def_expr, .. ) => Some((def_pattern, def_expr)),
                };

                match maybe_suffixed_value_def {
                    None => {
                        // We can't unwrap this def type, continue
                    },
                    Some((def_pattern, def_expr)) => {
                        match unwrap_suffixed_expression(arena, def_expr, Some(def_pattern)) {
                            Ok(unwrapped_def) => {
                                current_value_def.replace_expr(unwrapped_def);
                                local_defs.replace_with_value_def(tag_index, current_value_def, def_expr.region);
                            }
                            Err(EUnwrapped::UnwrappedDefExpr(unwrapped_expr)) => {
                                let split_defs = local_defs.split_defs_around(tag_index);
                                let before_empty = split_defs.before.is_empty();
                                let after_empty = split_defs.after.is_empty();
                                if before_empty && after_empty {
                                    // NIL before, NIL after -> SINGLE DEF
                                    let next_expr = match unwrap_suffixed_expression(arena,loc_ret,maybe_def_pat) {
                                        Ok(next_expr) => next_expr,
                                        Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                                            // We need to apply Task.ok here as the defs final expression was unwrapped
                                            apply_task_await(arena,def_expr.region,sub_arg,sub_pat,sub_new)
                                        }
                                        Err(EUnwrapped::UnwrappedDefExpr(..)) | Err(EUnwrapped::Malformed) => {
                                            // TODO handle case when we have maybe_def_pat so can return an unwrapped up
                                            return Err(EUnwrapped::Malformed);
                                        },
                                    };
                                    return unwrap_suffixed_expression(arena, apply_task_await(arena,def_expr.region,unwrapped_expr,def_pattern,next_expr), maybe_def_pat);
                                } else if before_empty {
                                    // NIL before, SOME after -> FIRST DEF
                                    let new_defs = arena.alloc(Loc::at(def_expr.region, Defs(arena.alloc(split_defs.after), loc_ret)));

                                    let next_expr = match unwrap_suffixed_expression(arena,new_defs,maybe_def_pat){
                                        Ok(next_expr) => next_expr,
                                        Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                                            apply_task_await(arena, def_expr.region, sub_arg, sub_pat, sub_new)
                                        }
                                        Err(EUnwrapped::UnwrappedDefExpr(..)) | Err(EUnwrapped::Malformed) => {
                                            // TODO handle case when we have maybe_def_pat so can return an unwrapped up
                                            return Err(EUnwrapped::Malformed);
                                        },
                                    };

                                    return unwrap_suffixed_expression(arena, apply_task_await(arena,def_expr.region,unwrapped_expr,def_pattern,next_expr), maybe_def_pat);
                                } else if after_empty {
                                    // SOME before, NIL after -> LAST DEF
                                    match unwrap_suffixed_expression(arena,loc_ret,maybe_def_pat){
                                        Ok(new_loc_ret) => {
                                            let applied_task_await = apply_task_await(arena, loc_expr.region, unwrapped_expr, def_pattern, new_loc_ret);
                                            let new_defs = arena.alloc(Loc::at(loc_expr.region,Defs(arena.alloc(split_defs.before), applied_task_await)));
                                            return unwrap_suffixed_expression(arena, new_defs, maybe_def_pat);
                                        },
                                        Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                                            let new_loc_ret = apply_task_await(arena,def_expr.region,sub_arg,sub_pat,sub_new);
                                            let applied_task_await = apply_task_await(arena, loc_expr.region, unwrapped_expr, def_pattern, new_loc_ret);
                                            let new_defs = arena.alloc(Loc::at(loc_expr.region,Defs(arena.alloc(split_defs.before), applied_task_await)));
                                            return unwrap_suffixed_expression(arena, new_defs, maybe_def_pat);
                                        }
                                        Err(EUnwrapped::UnwrappedDefExpr(..)) => {
                                            // TODO confirm this is correct with test case
                                            return Err(EUnwrapped::Malformed);
                                        }
                                        Err(EUnwrapped::Malformed) => {
                                            return Err(EUnwrapped::Malformed);
                                        },
                                    }
                                } else {
                                    // SOME before, SOME after -> MIDDLE DEF
                                    let after_defs = arena.alloc(Loc::at(def_expr.region, Defs(arena.alloc(split_defs.after), loc_ret)));

                                    match unwrap_suffixed_expression(arena,after_defs,maybe_def_pat){
                                        Ok(new_loc_ret) => {
                                            let applied_await = apply_task_await(arena, loc_expr.region, unwrapped_expr, def_pattern, new_loc_ret);
                                            let new_defs = arena.alloc(Loc::at(loc_expr.region,Defs(arena.alloc(split_defs.before), applied_await)));
                                            return unwrap_suffixed_expression(arena, new_defs, maybe_def_pat);
                                        },
                                        Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                                            let new_loc_ret = apply_task_await(arena, def_expr.region, sub_arg, sub_pat, sub_new);
                                            let applied_await = apply_task_await(arena, loc_expr.region, unwrapped_expr, def_pattern, new_loc_ret);
                                            let new_defs = arena.alloc(Loc::at(loc_expr.region,Defs(arena.alloc(split_defs.before), applied_await)));
                                            return unwrap_suffixed_expression(arena, new_defs, maybe_def_pat);
                                        }
                                        Err(EUnwrapped::UnwrappedDefExpr(..)) | Err(EUnwrapped::Malformed) => {
                                            // TODO handle case when we have maybe_def_pat so can return an unwrapped up
                                            return Err(EUnwrapped::Malformed);
                                        },
                                    };
                                }
                            }
                            Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                                let new_body_def = ValueDef::Body(def_pattern, sub_new);
                                local_defs.replace_with_value_def(tag_index,new_body_def, sub_new.region);
                                let new_defs_expr = arena.alloc(Loc::at(def_expr.region,Defs(arena.alloc(local_defs), loc_ret)));
                                let replaced_def = apply_task_await(arena,def_expr.region,sub_arg,sub_pat,new_defs_expr);
                                return unwrap_suffixed_expression(arena,replaced_def,maybe_def_pat);
                            }
                            Err(err) => return Err(err)
                        }
                    }
                }
            }

            // try to unwrap the loc_ret
            match unwrap_suffixed_expression(arena,loc_ret,maybe_def_pat){
                Ok(new_loc_ret) => {
                            Ok(arena.alloc(Loc::at(loc_expr.region,Defs(arena.alloc(local_defs), new_loc_ret))))
                },
                Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                    let new_loc_ret = apply_task_await(arena, loc_expr.region,sub_arg,sub_pat,sub_new);
                    let new_defs = arena.alloc(Loc::at(loc_expr.region,Defs(arena.alloc(local_defs), new_loc_ret)));
                    unwrap_suffixed_expression(arena, new_defs, maybe_def_pat)
                }
                Err(EUnwrapped::UnwrappedDefExpr(..)) => {
                    // TODO confirm this is correct with test case
                    Err(EUnwrapped::Malformed)
                }
                Err(EUnwrapped::Malformed) => {
                    Err(EUnwrapped::Malformed)
                },
            }
        },
        _ => internal_error!("unreachable, expected a Defs node to be passed into unwrap_suffixed_expression_defs_help"),
    }
}

/// Helper for `Task.await (loc_arg) \loc_pat -> loc_new`
pub fn apply_task_await<'a>(
    arena: &'a Bump,
    region: Region,
    loc_arg: &'a Loc<Expr<'a>>,
    loc_pat: &'a Loc<Pattern<'a>>,
    loc_new: &'a Loc<Expr<'a>>,
) -> &'a Loc<Expr<'a>> {
    // If the pattern and the new are the same then we don't need to unwrap anything
    // e.g. `Task.await foo \{} -> Task.ok {}` is the same as `foo`
    if is_matching_empty_record(loc_pat, loc_new) {
        return loc_arg;
    }

    // If the pattern and the new are matching answers then we don't need to unwrap anything
    // e.g. `Task.await foo \#!a1 -> Task.ok #!a1` is the same as `foo`
    if is_matching_intermediate_answer(loc_pat, loc_new) {
        return loc_arg;
    }

    let mut task_await_apply_args: Vec<&'a Loc<Expr<'a>>> = Vec::new_in(arena);

    // apply the unwrapped suffixed expression
    task_await_apply_args.push(loc_arg);

    // apply the closure
    let mut closure_pattern = Vec::new_in(arena);
    closure_pattern.push(*loc_pat);
    task_await_apply_args.push(arena.alloc(Loc::at(
        region,
        Closure(arena.alloc_slice_copy(closure_pattern.as_slice()), loc_new),
    )));

    arena.alloc(Loc::at(
        region,
        Apply(
            arena.alloc(Loc {
                region,
                value: Var {
                    module_name: ModuleName::TASK,
                    ident: "await",
                },
            }),
            arena.alloc(task_await_apply_args),
            CalledVia::BangSuffix,
        ),
    ))
}

fn extract_wrapped_task_ok_value<'a>(loc_expr: &'a Loc<Expr<'a>>) -> Option<&'a Loc<Expr<'a>>> {
    match loc_expr.value {
        Expr::Apply(function, arguments, _) => match function.value {
            Var {
                module_name, ident, ..
            } if module_name == ModuleName::TASK && ident == "ok" => arguments.first().copied(),
            _ => None,
        },
        _ => None,
    }
}

fn is_matching_empty_record<'a>(
    loc_pat: &'a Loc<Pattern<'a>>,
    loc_expr: &'a Loc<Expr<'a>>,
) -> bool {
    let is_empty_record = match extract_wrapped_task_ok_value(loc_expr) {
        Some(task_expr) => match task_expr.value {
            Expr::Record(collection) => collection.is_empty(),
            _ => false,
        },
        None => false,
    };

    let is_pattern_empty_record = match loc_pat.value {
        Pattern::RecordDestructure(collection) => collection.is_empty(),
        _ => false,
    };

    is_empty_record && is_pattern_empty_record
}

pub fn is_matching_intermediate_answer<'a>(
    loc_pat: &'a Loc<Pattern<'a>>,
    loc_new: &'a Loc<Expr<'a>>,
) -> bool {
    let pat_ident = match loc_pat.value {
        Pattern::Identifier { ident, .. } => Some(ident),
        _ => None,
    };
    let exp_ident = match loc_new.value {
        Expr::Var {
            module_name, ident, ..
        } if module_name.is_empty() && ident.starts_with('#') => Some(ident),
        _ => None,
    };
    let exp_ident_in_task = match extract_wrapped_task_ok_value(loc_new) {
        Some(task_expr) => match task_expr.value {
            Expr::Var {
                module_name, ident, ..
            } if module_name.is_empty() && ident.starts_with('#') => Some(ident),
            _ => None,
        },
        None => None,
    };
    match (pat_ident, exp_ident, exp_ident_in_task) {
        (Some(a), Some(b), None) => a == b,
        (Some(a), None, Some(b)) => a == b,
        _ => false,
    }
}
