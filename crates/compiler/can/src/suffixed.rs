#![allow(clippy::manual_map)]

use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_error_macros::internal_error;
use roc_module::called_via::CalledVia;
use roc_module::ident::ModuleName;
use roc_parse::ast::Expr::{self, *};
use roc_parse::ast::{Pattern, ValueDef};
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
                suffixed: 0,
            },
            Pattern::Identifier {
                ident: answer_ident.as_str(),
                suffixed: 0,
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
    // dbg!("unwrap_suffixed_expression", &loc_expr);

    match loc_expr.value {
        Expr::Var { suffixed, .. } if suffixed == 0 => Ok(loc_expr),

        Expr::Var {
            module_name,
            ident,
            suffixed,
        } if suffixed > 0 => {
            let unwrapped_var = arena.alloc(Loc::at(
                loc_expr.region,
                Expr::Var {
                    module_name,
                    ident,
                    suffixed: suffixed.saturating_sub(1),
                },
            ));

            init_unwrapped_err(arena, unwrapped_var, maybe_def_pat)
        }

        Expr::Defs(..) => unwrap_suffixed_expression_defs_help(arena, loc_expr, maybe_def_pat),

        Expr::Apply(..) => unwrap_suffixed_expression_apply_help(arena, loc_expr, maybe_def_pat),

        Expr::When(..) => unwrap_suffixed_expression_when_help(arena, loc_expr, maybe_def_pat),

        Expr::If(..) => unwrap_suffixed_expression_if_then_else_help(arena, loc_expr, maybe_def_pat),

        Expr::Closure(..) => unwrap_suffixed_expression_closure_help(arena, loc_expr, maybe_def_pat),

        Expr::ParensAround(..) => unwrap_suffixed_expression_parens_help(arena, loc_expr, maybe_def_pat),

        Expr::SpaceBefore(..) | Expr::SpaceAfter(..) => internal_error!("SpaceBefore and SpaceAfter should have been removed in desugar_expr"),

        Expr::BinOps(..) => internal_error!("BinOps should have been desugared in desugar_expr"),

        // we only need to unwrap some expressions, leave the rest alone
        _ =>Ok(loc_expr),
    }
}

pub fn unwrap_suffixed_expression_parens_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::ParensAround(sub_loc_expr) => {
            // dbg!(&loc_expr, &maybe_def_pat);
            match unwrap_suffixed_expression(arena, arena.alloc(Loc::at_zero(*sub_loc_expr)), None)
            {
                Ok(new_expr) => {
                    let new_parens = arena.alloc(Loc::at(
                        loc_expr.region,
                        ParensAround(arena.alloc(new_expr.value)),
                    ));
                    return Ok(new_parens);
                }
                Err(EUnwrapped::UnwrappedDefExpr(unwrapped_parens)) => {
                    // dbg!(&unwrapped_parens, maybe_def_pat);
                    if let Some(..) = maybe_def_pat {
                        let new_parens = arena.alloc(Loc::at(
                            loc_expr.region,
                            ParensAround(&unwrapped_parens.value),
                        ));
                        return Err(EUnwrapped::UnwrappedDefExpr(new_parens));
                    } else {
                        return Err(EUnwrapped::Malformed);
                    }
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
                    // dbg!(loc_expr, sub_arg, sub_pat, sub_new, &new_parens);
                    return Err(EUnwrapped::UnwrappedSubExpr {
                        sub_arg,
                        sub_pat,
                        sub_new: new_parens,
                    });
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
    maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::Closure(args, return_expr) => {
            let count_suffixed_args = args
                .iter()
                .filter(|loc_pat| loc_pat.value.is_suffixed())
                .count();

            if count_suffixed_args > 0 {
                debug_assert!(false,"closure arguments should not be suffixed");
                return Err(EUnwrapped::Malformed);
            }

            // check the return expression
            match unwrap_suffixed_expression(arena, return_expr, None) {
                Ok(new_expr) => {
                    let new_closure = arena.alloc(Loc::at(loc_expr.region, Expr::Closure(args, new_expr)));
                    Ok(new_closure)
                }
                Err(EUnwrapped::UnwrappedDefExpr(unwrapped_return)) => {
                    if let Some(..) = maybe_def_pat {
                        let new_closure = arena.alloc(Loc::at(loc_expr.region, Expr::Closure(args, unwrapped_return)));
                        return Err(EUnwrapped::UnwrappedDefExpr(new_closure));
                    } else {
                        return Err(EUnwrapped::Malformed);
                    }
                }
                Err(EUnwrapped::UnwrappedSubExpr {
                    sub_arg,
                    sub_pat,
                    sub_new,
                }) => {
                    let new_closure =
                        arena.alloc(Loc::at(loc_expr.region, Expr::Closure(args, sub_new)));
                    return Err(EUnwrapped::UnwrappedSubExpr {
                        sub_arg,
                        sub_pat,
                        sub_new: new_closure,
                    });
                }
                Err(err) => Err(err),
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
    // dbg!(&loc_expr, maybe_def_pat);
    match loc_expr.value {
        Expr::Apply(function, apply_args, called_via) => {

            // Any suffixed arguments will be innermost, therefore we unwrap those first
            let local_args = arena.alloc_slice_copy(apply_args);
            for (_, arg) in local_args.iter_mut().enumerate() {
                match unwrap_suffixed_expression(arena, arg, None) {
                    Ok(new_arg) => {
                        *arg = new_arg;
                    }
                    Err(EUnwrapped::UnwrappedDefExpr(unwrapped_arg)) => {
                        // dbg!(&unwrapped_arg, maybe_def_pat);
                        if let Some(..) = maybe_def_pat {
                            *arg = unwrapped_arg;

                            let new_apply = arena.alloc(Loc::at(loc_expr.region, Apply(function, local_args, called_via)));

                            return Err(EUnwrapped::UnwrappedDefExpr(new_apply));
                        } else {
                            return Err(EUnwrapped::Malformed);
                        }
                    }
                    Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new: new_arg }) => {

                        *arg = new_arg;

                        let new_apply = arena.alloc(Loc::at(loc_expr.region, Apply(function, local_args, called_via)));
                        // dbg!(&loc_expr, &sub_arg, &sub_pat, &new_arg);
                        // return dbg!(Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new: new_apply}));
                        return Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new: new_apply});
                    }
                    Err(err) => return Err(err),
                }
            }

            // function is another expression
            match unwrap_suffixed_expression(arena, function, None) {
                Ok(new_function) => {
                    let new_apply = arena.alloc(Loc::at(loc_expr.region, Expr::Apply(new_function, local_args, called_via)));
                    return Ok(new_apply);
                }
                Err(EUnwrapped::UnwrappedDefExpr(unwrapped_function)) => {
                    if let Some(..) = maybe_def_pat {
                        let new_apply = arena.alloc(Loc::at(loc_expr.region, Expr::Apply(unwrapped_function, local_args, called_via)));
                        return Err(EUnwrapped::UnwrappedDefExpr(new_apply));
                    } else {
                        return Err(EUnwrapped::Malformed);
                    }
                }
                Err(EUnwrapped::UnwrappedSubExpr { sub_arg: unwrapped_function, sub_pat, sub_new }) => {

                    let new_apply = arena.alloc(Loc::at(loc_expr.region, Expr::Apply(unwrapped_function, local_args, called_via)));

                    return Err(EUnwrapped::UnwrappedSubExpr { sub_arg: new_apply, sub_pat, sub_new});

                }
                Err(err) => Err(err)
            }
        }
        _ => internal_error!("unreachable, expected an Apply node to be passed into unwrap_suffixed_expression_apply_help"),
    }
}

pub fn unwrap_suffixed_expression_if_then_else_help<'a>(
    _arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    _maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    Ok(loc_expr)

    // consider each if-statement, if it is suffixed we need to desugar e.g.
    // ```
    // if isFalse! then
    //     "fail"
    // else
    //     if isTrue! then
    //         "success"
    //     else
    //         "fail"
    // ```
    // desugars to
    // ```
    // Task.await (isFalse) \isAnswer0 ->
    //     if isAnswer0 then
    //         "fail"
    //     else
    //         Task.await (isTrue) \isAnswer1 ->
    //             if isAnswer1 then
    //                 "success"
    //             else
    //                 "fail"
    // ```
    //
    // Note there are four possible combinations that must be considered
    // 1. NIL if_thens before the first suffixed, and NIL after e.g. `if y! then "y" else "n"`
    // 2. NIL if_thens before the first suffixed, and SOME after e.g. `if n! then "n" else if y! "y" else "n"`
    // 3. SOME if_thens before the first suffixed, and NIL after e.g. `if n then "n" else if y! then "y" else "n"`
    // 4. SOME if_thens before the first suffixed, and SOME after e.g. `if n then "n" else if y! then "y" else if n then "n"`
    // fn desugar_if_node_suffixed<'a>(arena: &'a Bump, loc_expr: &'a Loc<Expr<'a>>) -> &'a Loc<Expr<'a>> {
    //     match loc_expr.value {
    //         Expr::If(if_thens, final_else_branch) => {
    //             // Search for the first suffixied expression e.g. `if isThing! then ...`
    //             for (index, if_then) in if_thens.iter().enumerate() {
    //                 let (current_if_then_statement, current_if_then_expression) = if_then;

    //                 if is_loc_expr_suffixed(current_if_then_statement) {
    //                     // split if_thens around the current index
    //                     let (before, after) = roc_parse::ast::split_around(if_thens, index);

    //                     // increment our global counter for ident suffixes
    //                     // this should be the only place this counter is referenced
    //                     SUFFIXED_IF_COUNTER.fetch_add(1, Ordering::SeqCst);
    //                     let count = SUFFIXED_IF_COUNTER.load(Ordering::SeqCst);

    //                     // create a unique identifier for our answer
    //                     let answer_ident = arena.alloc(format!("#if!{}", count));
    //                     let pattern = Loc::at(
    //                         current_if_then_statement.region,
    //                         Pattern::Identifier {
    //                             ident: answer_ident,
    //                             suffixed: 0,
    //                         },
    //                     );

    //                     // if we have any after the current index, we will recurse on these as they may also be suffixed
    //                     let remaining_loc_expr = if after.is_empty() {
    //                         final_else_branch
    //                     } else {
    //                         let after_if = arena
    //                             .alloc(Loc::at(loc_expr.region, Expr::If(after, final_else_branch)));

    //                         desugar_if_node_suffixed(arena, after_if)
    //                     };

    //                     let closure_expr = Closure(
    //                         arena.alloc([pattern]),
    //                         arena.alloc(Loc::at(
    //                             current_if_then_statement.region,
    //                             If(
    //                                 arena.alloc_slice_clone(&[(
    //                                     Loc::at(
    //                                         current_if_then_statement.region,
    //                                         Var {
    //                                             module_name: "",
    //                                             ident: answer_ident,
    //                                             suffixed: 0,
    //                                         },
    //                                     ),
    //                                     *current_if_then_expression,
    //                                 )]),
    //                                 remaining_loc_expr,
    //                             ),
    //                         )),
    //                     );

    //                     // Apply arguments to Task.await, first is the unwrapped Suffix expr second is the Closure
    //                     let mut task_await_apply_args: Vec<&'a Loc<Expr<'a>>> = Vec::new_in(arena);

    //                     task_await_apply_args.push(current_if_then_statement);
    //                     task_await_apply_args.push(arena.alloc(Loc::at(loc_expr.region, closure_expr)));

    //                     let applied_closure = arena.alloc(Loc::at(
    //                         loc_expr.region,
    //                         Apply(
    //                             arena.alloc(Loc {
    //                                 region: loc_expr.region,
    //                                 value: Var {
    //                                     module_name: ModuleName::TASK,
    //                                     ident: "await",
    //                                     suffixed: 0,
    //                                 },
    //                             }),
    //                             arena.alloc(task_await_apply_args),
    //                             CalledVia::BangSuffix,
    //                         ),
    //                     ));

    //                     if before.is_empty() {
    //                         return applied_closure;
    //                     } else {
    //                         return arena
    //                             .alloc(Loc::at(loc_expr.region, Expr::If(before, applied_closure)));
    //                     }
    //                 }
    //             }

    //             // nothing was suffixed, so just return the original if-statement
    //             loc_expr
    //         }
    //         _ => internal_error!("unreachable, expected an If expression to desugar"),
    //     }
    // }
}

pub fn unwrap_suffixed_expression_when_help<'a>(
    _arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    _maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    Ok(loc_expr)
}

pub fn unwrap_suffixed_expression_defs_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    maybe_def_pat: Option<&'a Loc<Pattern<'a>>>,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::Defs(defs, loc_ret) => {

            // try an unwrap each def, if none can be unwrapped, then try to unwrap the loc_ret
            for (tag_index, type_or_value_def) in defs.defs().enumerate() {
                use ValueDef::*;

                let maybe_suffixed_value_def = match type_or_value_def.err() {
                    None | Some(Annotation(..)) | Some(Dbg{..}) | Some(Expect{..}) | Some(ExpectFx{..}) | Some(Stmt(..))=> None,
                    Some(AnnotatedBody { body_pattern, body_expr, .. }) => Some((body_pattern, body_expr)),
                    Some(Body (def_pattern, def_expr, .. )) => Some((def_pattern, def_expr)),
                };

                match maybe_suffixed_value_def {
                    None => {
                        // We can't unwrap this def type, continue
                        continue;
                    },
                    Some((def_pattern, def_expr)) => {
                        match unwrap_suffixed_expression(arena, def_expr, Some(*def_pattern)) {
                            Ok(..) => { 
                                // Nothing further to unwrap with this def, continue
                                continue;
                            }
                            Err(EUnwrapped::UnwrappedDefExpr(unwrapped_expr)) => {
                                let local_defs = defs.clone();
                                let split_defs = local_defs.split_defs_around(tag_index);
                                let before_empty = split_defs.before.is_empty();
                                let after_empty = split_defs.after.is_empty();
                                if before_empty && after_empty {
                                    // NIL before, NIL after -> SINGLE
                                    let next_expr = match unwrap_suffixed_expression(arena,loc_ret,None) {
                                        Ok(next_expr) => next_expr,
                                        Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                                            apply_task_await(arena,def_expr.region,sub_arg,sub_pat,sub_new)
                                        }
                                        Err(EUnwrapped::UnwrappedDefExpr(..)) | Err(EUnwrapped::Malformed) => {
                                            // TODO handle case when we have maybe_def_pat so can return an unwrapped up
                                            return Err(EUnwrapped::Malformed);
                                        },
                                    };

                                    return unwrap_suffixed_expression(arena, apply_task_await(arena,def_expr.region,unwrapped_expr,*def_pattern,next_expr), None);
                                } else if before_empty {
                                    // NIL before, SOME after -> FIRST
                                    let new_defs = arena.alloc(Loc::at(def_expr.region, Defs(arena.alloc(split_defs.after), loc_ret)));

                                    let next_expr = match unwrap_suffixed_expression(arena,new_defs,None){
                                        Ok(next_expr) => next_expr,
                                        Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                                            apply_task_await(arena, def_expr.region, sub_arg, sub_pat, sub_new)
                                        }
                                        Err(EUnwrapped::UnwrappedDefExpr(..)) | Err(EUnwrapped::Malformed) => {
                                            // TODO handle case when we have maybe_def_pat so can return an unwrapped up
                                            return Err(EUnwrapped::Malformed);
                                        },
                                    };

                                    return unwrap_suffixed_expression(arena, apply_task_await(arena,def_expr.region,unwrapped_expr,*def_pattern,next_expr), None);
                                } else {
                                    // SOME before, NIL after -> LAST
                                    debug_assert!(after_empty);
                                    let new_defs = arena.alloc(Loc::at(loc_expr.region,Defs(arena.alloc(split_defs.before), loc_ret)));
                                    let next_expr = match unwrap_suffixed_expression(arena,new_defs,None){
                                        Ok(next_expr) => next_expr,
                                        Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                                            apply_task_await(arena,def_expr.region,sub_arg,sub_pat,sub_new)
                                        }
                                        Err(EUnwrapped::UnwrappedDefExpr(..)) | Err(EUnwrapped::Malformed) => {
                                            // TODO handle case when we have maybe_def_pat so can return an unwrapped up
                                            return Err(EUnwrapped::Malformed);
                                        },
                                    
                                    };

                                    return unwrap_suffixed_expression(arena, apply_task_await(arena,def_expr.region,unwrapped_expr,*def_pattern,next_expr), None);
                                }
                            }
                            Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {

                                // dbg!(&sub_arg, &sub_pat, &sub_new);

                                let mut local_defs = defs.clone();
                                let new_body_def = ValueDef::Body(def_pattern, sub_new);
                                local_defs.replace_with_value_def(tag_index,new_body_def,def_expr.region);
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
            match unwrap_suffixed_expression(arena, loc_ret, None) {
                Ok(new_loc_ret) => {

                    // dbg!(&new_loc_ret);

                    Ok(arena.alloc(Loc::at(loc_expr.region, Defs(arena.alloc(defs), new_loc_ret))))
                }
                Err(EUnwrapped::UnwrappedDefExpr(new_loc_ret)) => {
                    // the loc_ret was unwrapped, replace and return new expression
                    internal_error!("is this even possible? we are in the loc_ret of a Def");
                    // Err(EUnwrapped::UnwrappedDefExpr(arena.alloc(Loc::at(loc_expr.region, Defs(defs, new_loc_ret)))))
                }
                Err(EUnwrapped::UnwrappedSubExpr { sub_arg, sub_pat, sub_new }) => {
                    // the loc_ret was unwrapped
                    internal_error!("is this even possible? we are in the loc_ret of a Def");
                    // let new_loc_ret = apply_task_await(arena, loc_ret.region, sub_arg, sub_pat, sub_new);
                    // Err(EUnwrapped::UnwrappedDefExpr(arena.alloc(Loc::at(loc_expr.region, Defs(defs, new_loc_ret)))))
                }
                Err(err) => Err(err)
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
                    suffixed: 0,
                },
            }),
            arena.alloc(task_await_apply_args),
            CalledVia::BangSuffix,
        ),
    ))
}
