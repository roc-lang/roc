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
    /// The expression was unwrapped, consumes the def and following expression
    /// e.g. `x = foo!` unwrapped `Task.await (foo) \x -> ...`
    UnwrappedExpr(&'a Loc<Expr<'a>>),

    /// The expression had a (sub) expression unwrapped, doesn't affect the def or following expression
    /// e.g. `x = bar! (foo!)` unwrapped `x = Task.await (foo) \#answer1 -> bar! #answer1`
    UnwrappedSubExpr {
        /// this is the unwrapped suffixed which will be applied to the Task.await
        arg: &'a Loc<Expr<'a>>,

        /// this pattern will be used in the closure
        pat: Loc<Pattern<'a>>,

        /// this expression will replace the suffixed in the parent
        new: &'a Loc<Expr<'a>>,
    },
}

/// Descend through the AST and unwrap each suffixed expression
/// when an expression is unwrapped, we apply a `Task.await` and
/// then descend through the AST again until there are no more suffixed
/// expressions, or we hit an error
pub fn unwrap_suffixed_expression<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,

    // was this function called from e.g. ValueDef::Body
    // we need this to handle the case where we cannot unwrap the expression
    called_from_def: bool,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::Var { suffixed, .. } if suffixed == 0 => Ok(loc_expr),

        Expr::Var {
            module_name,
            ident,
            suffixed,
        } if suffixed > 0 => {
            if suffixed > 1 {
                /*
                ## Example with multiple suffix
                foo!!
                bar

                Task.await (foo) \#!a1 ->
                    (#!a1)!
                    bar

                Task.await (foo) \#!a1 ->
                    Task.await #!a1 \{} -> bar
                */

                let (mut answer_var, answer_pat) = next_suffixed_answer_pattern(arena);

                let arg = arena.alloc(Loc::at(
                    loc_expr.region,
                    Var {
                        module_name,
                        ident,
                        suffixed: 0,
                    },
                ));

                let pat = Loc::at(loc_expr.region, answer_pat);

                // we increment the suffix of the answer variable as there may be more than 1 additional suffix
                answer_var.increment_var_suffix(suffixed.saturating_sub(1));

                let new = arena.alloc(Loc::at(loc_expr.region, answer_var));

                return Err(EUnwrapped::UnwrappedSubExpr { arg, pat, new });
            }

            if called_from_def {
                Err(EUnwrapped::UnwrappedExpr(arena.alloc(Loc::at(
                    loc_expr.region,
                    Expr::Var {
                        module_name,
                        ident,
                        suffixed: suffixed.saturating_sub(1),
                    },
                ))))
            } else {
                internal_error!("unwrapping Var {:?} within a sub expression... this should have been handled at the previous level", loc_expr);
            }
        }

        Expr::Defs(..) => unwrap_suffixed_expression_defs_help(arena, loc_expr, called_from_def),

        Expr::Apply(..) => unwrap_suffixed_expression_apply_help(arena, loc_expr, called_from_def),

        Expr::SpaceBefore(..) | Expr::SpaceAfter(..) => {
            internal_error!("unexpected SpaceBefore or SpaceAfter in unwrap_suffixed_expression, should have been removed in desugar_expr before this");
        }

        Expr::When(..) => unwrap_suffixed_expression_when_help(arena, loc_expr, called_from_def),

        Expr::If(..) => {
            unwrap_suffixed_expression_if_then_else_help(arena, loc_expr, called_from_def)
        }

        Expr::Closure(args, return_expr) => {
            let count_suffixed_args = args
                .iter()
                .filter(|loc_pat| loc_pat.value.is_suffixed())
                .count();

            if count_suffixed_args > 0 {
                // TODO make this a nice error report
                internal_error!("closure arguments should not be suffixed");
            }

            // check the return expression
            match unwrap_suffixed_expression(arena, return_expr, false) {
                Ok(new_expr) => {
                    Ok(arena.alloc(Loc::at(loc_expr.region, Expr::Closure(args, new_expr))))
                }
                Err(..) => {
                    // TODO make this a nicer error

                    /*
                    Can we have suffixed expressions in a closure return expression?

                    x = \msg -> line! msg

                    x "hi"

                     */

                    internal_error!(
                        "unexpected unwrapped closure return expression, how is this possible?"
                    );
                }
            }
        }

        Expr::ParensAround(sub_loc_expr) => {
            match unwrap_suffixed_expression(
                arena,
                arena.alloc(Loc::at_zero(*sub_loc_expr)),
                called_from_def,
            ) {
                Ok(new_expr) => Ok(arena.alloc(Loc::at(
                    loc_expr.region,
                    ParensAround(arena.alloc(new_expr.value)),
                ))),
                Err(EUnwrapped::UnwrappedExpr(new_expr)) => {
                    Err(EUnwrapped::UnwrappedExpr(arena.alloc(Loc::at(
                        loc_expr.region,
                        ParensAround(arena.alloc(new_expr.value)),
                    ))))
                }
                Err(EUnwrapped::UnwrappedSubExpr { arg, pat, new }) => {
                    Err(EUnwrapped::UnwrappedSubExpr {
                        arg,
                        pat,
                        new: arena.alloc(Loc::at(
                            loc_expr.region,
                            ParensAround(arena.alloc(new.value)),
                        )),
                    })
                }
            }
        }

        _ => {
            // we only need to unwrap some expressions, leave the rest alone
            Ok(loc_expr)
        }
    }
}

pub fn unwrap_suffixed_expression_apply_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    called_from_def: bool,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::Apply(function, apply_args, called_via) => {

            let local_args = arena.alloc_slice_copy(apply_args);

            for (_, arg) in local_args.iter_mut().enumerate() {
                /*
                ## Example of suffixed function within an Apply
                x = bar (foo! "hello")
                baz x
                
                x = Task.await (foo "hello") \#!a -> bar (#!a)
                baz x
                */

                // try to unwrap the argument
                match unwrap_suffixed_expression(arena, arg, false) {
                    Ok(new_arg) => {
                        *arg = new_arg;
                    },
                    Err(..) => {
                         todo!();
                    }
                }
            }

            // try to unwrap the function

            // function is a Var
            if let Loc { value: Var{module_name, ident,suffixed},region } = function {
                match suffixed {
                    0 => {
                        // nothing to unwrap, move on to the return expression
                        return Ok(arena.alloc(Loc::at(loc_expr.region, Expr::Apply(function, local_args, called_via))));
                    },
                    1 => {
                        if called_from_def {
                            let new_apply = Loc::at(
                                loc_expr.region,
                                Expr::Apply(
                                    arena.alloc(Loc::at(
                                        loc_expr.region,
                                        Var {
                                            module_name,
                                            ident,
                                            suffixed: 0,
                                        },
                                    )),
                                    local_args,
                                    called_via,
                                ),
                            );

                            return Err(EUnwrapped::UnwrappedExpr(arena.alloc(new_apply)));
                        } else {

                            /*
                            ## Example of suffixed function within an Apply
                            x = (foo! "bar") "hello"
                                ^^^^^^^^^^^^ is our suffixed function
                            baz x
                            
                            Task.await (foo "bar") \#!a1 -> 
                                x = #!a1 "hello"
                                baz x
                            */

                            let (answer_var, answer_pat) = next_suffixed_answer_pattern(arena);
                            let arg = arena.alloc(Loc::at(loc_expr.region, Apply(
                                arena.alloc(Loc::at(
                                    *region,
                                    Var {
                                        module_name,
                                        ident,
                                        suffixed: 0,
                                    },
                                )),
                                local_args,
                                called_via,
                            )));

                            let pat = Loc::at(loc_expr.region, answer_pat);

                            let new = arena.alloc(Loc::at(loc_expr.region, answer_var));

                            return Err(EUnwrapped::UnwrappedSubExpr { arg, pat, new });
                        }
                    }
                    _ => {
                        internal_error!("TODO support multiple suffixes");
                    }
                }
            }

            // handle other cases
            match unwrap_suffixed_expression(arena, function, false) {
                Ok(new_function) => {
                    return Ok(arena.alloc(Loc::at(loc_expr.region, Expr::Apply(new_function, local_args, called_via))));
                },
                Err(EUnwrapped::UnwrappedExpr(..)) => {
                    todo!();
                }
                Err(EUnwrapped::UnwrappedSubExpr { arg, pat, new }) => {

                    return Err(EUnwrapped::UnwrappedSubExpr {
                        arg,
                        pat,
                        new: arena.alloc(Loc::at(loc_expr.region, Expr::Apply(new, local_args, called_via)))
                    });

                }
            }
        }
        _ => internal_error!("unreachable, expected an Apply node to be passed into unwrap_suffixed_expression_apply_help"),
    }
}

pub fn unwrap_suffixed_expression_if_then_else_help<'a>(
    _arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    _called_from_def: bool, // TODO do we need this? pretty sure we do
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
    _called_from_def: bool, // TODO do we need this? pretty sure we do
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    Ok(loc_expr)
}

pub fn unwrap_suffixed_expression_defs_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    _called_from_def: bool, // TODO do we need this? pretty sure we do
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    match loc_expr.value {
        Expr::Defs(defs, loc_ret) => {

            let mut local_defs = defs.clone();

            // try an unwrap each def, if none can be unwrapped, then try to unwrap the loc_ret
            for (tag_index, type_or_value_def) in defs.defs().enumerate() {
                use ValueDef::*;

                let maybe_pattern_expr = match type_or_value_def.err() {
                    None | Some(Annotation(..)) | Some(Dbg{..}) | Some(Expect{..}) | Some(ExpectFx{..}) | Some(Stmt(..))=> None,
                    Some(AnnotatedBody { body_pattern, body_expr, .. }) => Some((body_pattern, body_expr)),
                    Some(Body (def_pattern, def_expr, .. )) => Some((def_pattern, def_expr)),
                };

                match maybe_pattern_expr {
                    None => {
                        // nothing to unwrap, move on to the next def
                        continue;
                    },
                    Some((def_pattern, def_expr)) => {
                        match unwrap_suffixed_expression(arena, def_expr, true) {
                            Ok(new_def_expr) => {

                                let new_value_def = match type_or_value_def.err() {
                                    None | Some(Annotation(..)) | Some(Dbg{..}) | Some(Expect{..}) | Some(ExpectFx{..}) | Some(Stmt(..))=> internal_error!("unexpected ValueDef type"),
                                    Some(AnnotatedBody{ann_pattern,ann_type,comment,..}) => ValueDef::AnnotatedBody{ann_pattern,ann_type,comment: *comment,body_pattern: def_pattern, body_expr:new_def_expr},
                                    Some(Body ( .. )) => ValueDef::Body(def_pattern, new_def_expr),
                                };

                                // do nothing, move on to check the next def
                                local_defs.replace_with_value_def(tag_index, new_value_def, loc_expr.region);
                            }
                            Err(EUnwrapped::UnwrappedExpr(new)) => {
                                let split_defs = defs.split_defs_around(tag_index);
                                let before_empty = split_defs.before.is_empty();
                                let after_empty = split_defs.after.is_empty();
                                if before_empty && after_empty {
                                    // NIL before, NIL after -> SINGLE
                                    return unwrap_suffixed_expression(
                                        arena,
                                        apply_task_await(
                                            arena,
                                            def_expr.region,
                                            new,
                                            **def_pattern,
                                            loc_ret,
                                        ),
                                        false,
                                    );
                                } else if before_empty {
                                    // NIL before, SOME after -> FIRST
                                    return unwrap_suffixed_expression(
                                        arena,
                                        apply_task_await(arena, loc_expr.region, new, **def_pattern, arena.alloc(Loc::at(def_expr.region, Defs(arena.alloc(split_defs.after), loc_ret)))),
                                        false,
                                    );
                                } else {
                                    // SOME before, NIL after -> LAST
                                    debug_assert!(after_empty);
                                    /*
                                    ## Example with multiple defs, last suffixed
                                    msg = "hello"
                                    x = foo! msg <-- e.g. we've just unwrapped this def
                                    bar x
    
                                    msg = "hello"
                                    Task.await (foo msg) \x -> bar x
                                    */
                                    let new_loc_ret = apply_task_await(
                                        arena,
                                        loc_expr.region,
                                        new,
                                        **def_pattern,
                                        loc_ret,
                                    );
                                    return unwrap_suffixed_expression(
                                        arena,
                                        arena.alloc(Loc::at(loc_expr.region,
                                            Defs(arena.alloc(split_defs.before), new_loc_ret))),
                                             false,
                                            );
                                }
                            }
                            Err(EUnwrapped::UnwrappedSubExpr { arg, pat, new }) => {
                                let new_value_def = ValueDef::Body(def_pattern, new);
                                let mut new_defs = defs.clone();
                                new_defs.replace_with_value_def(
                                    tag_index,
                                    new_value_def,
                                    def_expr.region,
                                );
                                return unwrap_suffixed_expression(
                                    arena,
                                    apply_task_await(
                                        arena,
                                        def_expr.region,
                                        arg,
                                        pat,
                                        arena.alloc(Loc::at(
                                            def_expr.region,
                                            Defs(arena.alloc(new_defs), loc_ret),
                                        )),
                                    ),
                                    false,
                                );
                            }
                        }
                    }
                }
            }

            // try to unwrap the loc_ret
            match unwrap_suffixed_expression(arena, loc_ret, false) {
                Ok(new_loc_ret) => {
                    Ok(arena.alloc(Loc::at(loc_expr.region, Defs(arena.alloc(local_defs), new_loc_ret))))
                },
                Err(EUnwrapped::UnwrappedExpr(new_loc_ret)) => {
                    // the loc_ret was unwrapped, replace and return new expression
                    Err(EUnwrapped::UnwrappedExpr(arena.alloc(Loc::at(loc_expr.region, Defs(defs, new_loc_ret)))))
                }
                Err(EUnwrapped::UnwrappedSubExpr { arg, pat, new }) => {
                    // the loc_ret was unwrapped
                    let new_loc_ret = apply_task_await(arena, loc_ret.region, arg, pat, new);
                    Err(EUnwrapped::UnwrappedExpr(arena.alloc(Loc::at(loc_expr.region, Defs(defs, new_loc_ret)))))
                },
            }
        },
        _ => internal_error!("unreachable, expected a Defs node to be passed into unwrap_suffixed_expression_defs_help"),
    }
}

fn apply_task_await<'a>(
    arena: &'a Bump,
    region: Region,
    arg_loc_expr: &'a Loc<Expr<'a>>,
    loc_pat: Loc<Pattern<'a>>,
    new: &'a Loc<Expr<'a>>,
) -> &'a Loc<Expr<'a>> {
    let mut task_await_apply_args: Vec<&'a Loc<Expr<'a>>> = Vec::new_in(arena);

    // apply the unwrapped suffixed expression
    task_await_apply_args.push(arg_loc_expr);

    // apply the closure
    let mut closure_pattern = Vec::new_in(arena);
    closure_pattern.push(loc_pat);
    task_await_apply_args.push(arena.alloc(Loc::at(
        region,
        Closure(arena.alloc_slice_copy(closure_pattern.as_slice()), new),
    )));

    // e.g. `Task.await (arg_loc_expr) \pattern -> new`
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

#[cfg(test)]
mod unwrap_suffixed {

    use crate::desugar::desugar_defs_node_values;
    use bumpalo::Bump;
    use roc_parse::test_helpers::parse_defs_with;
    use roc_test_utils::assert_multiline_str_eq;

    #[test]
    fn multi_defs_stmts() {
        /*
        line! "Ahoy"
        {} = "There" |> Stdout.line!

        Task.ok {}

        # desugared
        Task.await (line "Ahoy") \{} ->
            Task.await ("There" |> Stdout.line) \{} ->
                Task.ok {}
        */

        let arena = &Bump::new();

        let src = r#"
            main = 
                line! "Ahoy"
                {} = "There" |> Stdout.line!
                
                Task.ok {}
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r#"Defs { tags: [Index(2147483648)], regions: [@0-36], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @0-36 Apply(@0-36 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@24-36 Apply(@24-36 Var { module_name: "", ident: "line", suffixed: 0 }, [@30-36 Str(PlainLine("Ahoy"))], Space), @0-36 Closure([@24-36 RecordDestructure([])], @58-81 Apply(@58-81 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@58-81 Apply(@58-81 Var { module_name: "Stdout", ident: "line", suffixed: 0 }, [@58-65 Str(PlainLine("There"))], BinOp(Pizza)), @58-81 Closure([@53-55 RecordDestructure([])], @115-125 Apply(@115-122 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@123-125 Record([])], Space))], BangSuffix))], BangSuffix))] }"#;

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    #[test]
    fn simple_pizza() {
        /*
        "hello" |> line!

        Task.ok {}

        # desugared
        Task.await ("hello" |> line) \{} -> Task.ok {}
        */

        let arena = &Bump::new();

        let src = r#"
            main = 
                "hello" |> line!

                Task.ok {}
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r#"Defs { tags: [Index(2147483648)], regions: [@0-40], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @24-40 Apply(@24-40 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@24-40 Apply(@24-40 Var { module_name: "", ident: "line", suffixed: 0 }, [@24-31 Str(PlainLine("hello"))], BinOp(Pizza)), @24-40 Closure([@24-40 RecordDestructure([])], @58-68 Apply(@58-65 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@66-68 Record([])], Space))], BangSuffix))] }"#;

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    #[test]
    fn single_suffix() {
        /*
        ## Example with single suffix
        x = foo!
        bar x

        # desugared
        Task.await (foo) \x -> bar x
        */

        let arena = &Bump::new();

        let src = r#"
            main = 
                x = foo!
                bar x
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r#"Defs { tags: [Index(2147483648)], regions: [@0-54], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @28-32 Apply(@28-32 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@28-32 Var { module_name: "", ident: "foo", suffixed: 0 }, @28-32 Closure([@24-25 Identifier { ident: "x", suffixed: 0 }], @49-54 Apply(@49-52 Var { module_name: "", ident: "bar", suffixed: 0 }, [@53-54 Var { module_name: "", ident: "x", suffixed: 0 }], Space))], BangSuffix))] }"#;

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    #[test]
    fn multiple_suffix() {
        /*
        ## Example with multiple suffix
        foo!!
        bar

        # step 1
        Task.await (foo) \#!a1 ->
            (#!a1)!
            bar

        # desugared
        Task.await (foo) \#!a1 ->
            Task.await #!a1 \{} -> bar
        */

        let arena = &Bump::new();

        let src = r#"
            main = 
                foo!!
                bar
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        // TODO fix this... the value_defs is incorrect here in a harmless way because
        // roc_parse::ast::Defs::split_defs_around is not completely correct
        // so we have the value_def in the vec in Defs, but there is no tag pointing to it
        let expected = r##"Defs { tags: [Index(2147483648)], regions: [@0-29], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @24-29 Apply(@24-29 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@24-29 Var { module_name: "", ident: "foo", suffixed: 0 }, @24-29 Closure([@24-29 Identifier { ident: "#!a0", suffixed: 0 }], @24-29 Apply(@24-29 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@24-29 Var { module_name: "", ident: "#!a0", suffixed: 0 }, @24-29 Closure([@24-29 RecordDestructure([])], @46-49 Var { module_name: "", ident: "bar", suffixed: 0 })], BangSuffix))], BangSuffix))] }"##;

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    #[test]
    fn apply_function_suffixed_sub_expr() {
        /*
        ## Example of suffixed function within an Apply
        x = (foo! "bar") "hello"
            ^^^^^^^^^^^^ is our suffixed function
        baz x

        # desugared
        Task.await (foo "bar") \#!a1 ->
            x = #!a1 "hello"
            bar x
        */

        let arena = &Bump::new();

        let src = r#"
            main = 
                x = (foo! "bar") "hello"
                baz x
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        // TODO fix this... the value_defs is incorrect here in a harmless way because
        // roc_parse::ast::Defs::split_defs_around is not completely correct
        // so we have the value_def in the vec in Defs, but there is no tag pointing to it
        let expected = r##"Defs { tags: [Index(2147483648)], regions: [@0-70], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @28-48 Apply(@28-48 Var { module_name: "Task", ident: "await", suffixed: 0 }, [Apply(@29-33 Var { module_name: "", ident: "foo", suffixed: 0 }, [@34-39 Str(PlainLine("bar"))], Space), @28-48 Closure([Identifier { ident: "#!a0", suffixed: 0 }], @28-48 Defs(Defs { tags: [Index(2147483650)], regions: [@28-48], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-48 Apply(@29-39 ParensAround(Apply(@29-33 Var { module_name: "", ident: "foo", suffixed: 1 }, [@34-39 Str(PlainLine("bar"))], Space)), [@41-48 Str(PlainLine("hello"))], Space)), Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-48 Apply(@29-39 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), [@41-48 Str(PlainLine("hello"))], Space)), Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-48 Apply(@29-39 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), [@41-48 Str(PlainLine("hello"))], Space))] }, @65-70 Apply(@65-68 Var { module_name: "", ident: "baz", suffixed: 0 }, [@69-70 Var { module_name: "", ident: "x", suffixed: 0 }], Space)))], BangSuffix))] }"##;

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    #[test]
    fn multiple_def_first_suffixed() {
        /*
        ## Example with multiple defs, first suffixed
        msg = "hello"
        x = foo! msg
        bar x

        # desugared
        msg = "hello"
        Task.await (foo msg) \x -> bar x
        */

        let arena = &Bump::new();

        let src = r#"
            main = 
                msg = "hello"
                x = foo! msg
                bar x
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        // TODO fix this... the value_defs is incorrect here in a harmless way because
        // roc_parse::ast::Defs::split_defs_around is not completely correct
        // so we have the value_def in the vec in Defs, but there is no tag pointing to it
        let expected = r#"Defs { tags: [Index(2147483648)], regions: [@0-88], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @24-88 Defs(Defs { tags: [Index(2147483650)], regions: [@24-88, @54-66], space_before: [Slice(start = 0, length = 0), Slice(start = 0, length = 1)], space_after: [Slice(start = 0, length = 0), Slice(start = 1, length = 0)], spaces: [Newline], type_defs: [], value_defs: [Body(@24-27 Identifier { ident: "msg", suffixed: 0 }, @30-37 Str(PlainLine("hello"))), Body(@54-55 Identifier { ident: "x", suffixed: 0 }, @58-66 Apply(@58-62 Var { module_name: "", ident: "foo", suffixed: 1 }, [@63-66 Var { module_name: "", ident: "msg", suffixed: 0 }], Space)), Body(@24-27 Identifier { ident: "msg", suffixed: 0 }, @30-37 Str(PlainLine("hello")))] }, @24-88 Apply(@24-88 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@58-66 Apply(@58-66 Var { module_name: "", ident: "foo", suffixed: 0 }, [@63-66 Var { module_name: "", ident: "msg", suffixed: 0 }], Space), @24-88 Closure([@54-55 Identifier { ident: "x", suffixed: 0 }], @83-88 Apply(@83-86 Var { module_name: "", ident: "bar", suffixed: 0 }, [@87-88 Var { module_name: "", ident: "x", suffixed: 0 }], Space))], BangSuffix)))] }"#;

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    #[test]
    fn closure_with_annotations() {
        /*
        x : Str -> Task _ _
        x = \msg ->

            y : Task {} _
            y = line! msg
            y

        x "foo"

        # desugared
        x = \msg ->
            Task.await (line msg) \y -> y

        x "foo"
        */

        let arena = &Bump::new();

        let src = r#"
            main = 
                x : Str -> Task _ _
                x = \msg ->

                    y : Task {} _
                    y = line! msg
                    y

                x "foo"
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        // TODO fix this... the value_defs is incorrect here in a harmless way because
        // roc_parse::ast::Defs::split_defs_around is not completely correct
        // so we have the value_def in the vec in Defs, but there is no tag pointing to it
        let expected = r#"Defs { tags: [Index(2147483648)], regions: [@0-187], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @24-187 Defs(Defs { tags: [Index(2147483650)], regions: [@24-187], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Annotation(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred]))), AnnotatedBody { ann_pattern: @24-25 Identifier { ident: "x", suffixed: 0 }, ann_type: @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred])), comment: None, body_pattern: @60-61 Identifier { ident: "x", suffixed: 0 }, body_expr: @64-162 Closure([@65-68 Identifier { ident: "msg", suffixed: 0 }], @93-162 Defs(Defs { tags: [Index(2147483649)], regions: [@93-140], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Annotation(@93-94 Identifier { ident: "y", suffixed: 0 }, @97-106 Apply("", "Task", [@102-104 Record { fields: [], ext: None }, @105-106 Inferred])), AnnotatedBody { ann_pattern: @93-94 Identifier { ident: "y", suffixed: 0 }, ann_type: @97-106 Apply("", "Task", [@102-104 Record { fields: [], ext: None }, @105-106 Inferred]), comment: None, body_pattern: @127-128 Identifier { ident: "y", suffixed: 0 }, body_expr: @131-140 Apply(@131-136 Var { module_name: "", ident: "line", suffixed: 1 }, [@137-140 Var { module_name: "", ident: "msg", suffixed: 0 }], Space) }] }, @161-162 Var { module_name: "", ident: "y", suffixed: 0 })) }, AnnotatedBody { ann_pattern: @24-25 Identifier { ident: "x", suffixed: 0 }, ann_type: @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred])), comment: None, body_pattern: @60-61 Identifier { ident: "x", suffixed: 0 }, body_expr: @64-162 Closure([@65-68 Identifier { ident: "msg", suffixed: 0 }], @131-140 Apply(@131-140 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@131-140 Apply(@131-140 Var { module_name: "", ident: "line", suffixed: 0 }, [@137-140 Var { module_name: "", ident: "msg", suffixed: 0 }], Space), @131-140 Closure([@127-128 Identifier { ident: "y", suffixed: 0 }], @161-162 Var { module_name: "", ident: "y", suffixed: 0 })], BangSuffix)) }] }, @180-187 Apply(@180-181 Var { module_name: "", ident: "x", suffixed: 0 }, [@182-187 Str(PlainLine("foo"))], Space)))] }"#;

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }
}
