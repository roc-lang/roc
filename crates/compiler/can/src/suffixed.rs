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
    let result = {
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
                )
            }

            Expr::BinOps(..) => {
                internal_error!("BinOps should have been desugared in desugar_expr")
            }

            // we only need to unwrap some expressions, leave the rest as is
            _ => Ok(loc_expr),
        }
    };

    #[cfg(all(test, debug_assertions))]
    {
        // only run in tests
        dbg!(
            "unwrap_suffixed_expression",
            &loc_expr,
            maybe_def_pat,
            &result
        );
    }

    result
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

            // Check to make sure that arguments are not suffixed
            let suffixed_arg_count = closure_args
                .iter()
                .filter(|loc_pat| loc_pat.value.is_suffixed())
                .count();

            if suffixed_arg_count > 0 {
                debug_assert!(false,"closure arguments should not be suffixed");
                return Err(EUnwrapped::Malformed);
            }

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
            for (_, arg) in local_args.iter_mut().enumerate() {
                match unwrap_suffixed_expression(arena, arg, maybe_def_pat) {
                    Ok(new_arg) => {
                        *arg = new_arg;
                    }
                    Err(EUnwrapped::UnwrappedDefExpr(unwrapped_arg)) => {
                        *arg = unwrapped_arg;

                        let new_apply = arena.alloc(Loc::at(loc_expr.region, Apply(function, local_args, called_via)));

                        return Err(EUnwrapped::UnwrappedDefExpr(new_apply));
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
            if let Expr::Var { module_name, ident, suffixed } = function.value {
                if suffixed == 1 {
                    let unwrapped_function = arena.alloc(Loc::at(
                        loc_expr.region,
                        Expr::Var {
                            module_name,
                            ident,
                            suffixed: suffixed - 1,
                        },
                    ));

                    let new_apply = arena.alloc(Loc::at(loc_expr.region, Expr::Apply(unwrapped_function, local_args, called_via)));

                    return init_unwrapped_err(arena, new_apply, maybe_def_pat);
                }
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
                    Annotation(..) | Dbg{..} | Expect{..} | ExpectFx{..} | Stmt(..) => None,
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
                                } else {
                                    // SOME before, NIL after -> LAST DEF
                                    debug_assert!(after_empty);

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
