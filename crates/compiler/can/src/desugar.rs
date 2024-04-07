#![allow(clippy::manual_map)]

use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_error_macros::internal_error;
use roc_module::called_via::BinOp::Pizza;
use roc_module::called_via::{self, BinOp, CalledVia};
use roc_module::ident::ModuleName;
use roc_parse::ast::Expr::{self, *};
use roc_parse::ast::{
    AssignedField, Collection, Pattern, RecordBuilderField, StrLiteral,
    StrSegment, ValueDef, WhenBranch,
    split_loc_exprs_around,
};
use roc_region::all::{LineInfo, Loc, Region};

use std::fmt::Arguments;
use std::sync::atomic::{AtomicUsize, Ordering};

static SUFFIXED_ANSWER_COUNTER: AtomicUsize = AtomicUsize::new(0);

// use a global counter to ensure that each suffixed answer pattern has a unique suffix
// once it is desugared into a closure e.g. a!0, a!1, a!2, etc.
fn next_suffixed_answer_pattern<'a>(arena: &'a Bump) -> Pattern<'a> {
    // increment our global counter for ident suffixes
    // this should be the only place this counter is referenced
    SUFFIXED_ANSWER_COUNTER.fetch_add(1, Ordering::SeqCst);

    let count = SUFFIXED_ANSWER_COUNTER.load(Ordering::SeqCst);
    let answer_ident = arena.alloc(format!("#!a{}", count));

   Pattern::Identifier { ident: answer_ident.as_str(), suffixed: 0 }
}

// BinOp precedence logic adapted from Gluon by Markus Westerlind
// https://github.com/gluon-lang/gluon - license information can be found in
// the LEGAL_DETAILS file in the root directory of this distribution.
//
// Thank you, Markus!

fn new_op_call_expr<'a>(
    arena: &'a Bump,
    left: &'a Loc<Expr<'a>>,
    loc_op: Loc<BinOp>,
    right: &'a Loc<Expr<'a>>,
) -> Loc<Expr<'a>> {
    let region = Region::span_across(&left.region, &right.region);

    let value = match loc_op.value {
        Pizza => {
            // Rewrite the Pizza operator into an Apply

            match &right.value {
                Apply(function, arguments, _called_via) => {
                    let mut args = Vec::with_capacity_in(1 + arguments.len(), arena);

                    args.push(left);
                    args.extend(arguments.iter());

                    let args = args.into_bump_slice();

                    Apply(function, args, CalledVia::BinOp(Pizza))
                }
                _ => {
                    // e.g. `1 |> (if b then (\a -> a) else (\c -> c))`
                    Apply(right, arena.alloc([left]), CalledVia::BinOp(Pizza))
                }
            }
        }
        binop => {
            // This is a normal binary operator like (+), so desugar it
            // into the appropriate function call.
            let (module_name, ident) = binop_to_function(binop);

            let args = arena.alloc([left, right]);

            let loc_expr = arena.alloc(Loc {
                value: Expr::Var {
                    module_name,
                    ident,
                    suffixed: 0,
                },
                region: loc_op.region,
            });

            Apply(loc_expr, args, CalledVia::BinOp(binop))
        }
    };

    Loc { region, value }
}

fn desugar_value_def<'a>(
    arena: &'a Bump,
    def: &'a ValueDef<'a>,
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
) -> ValueDef<'a> {
    use ValueDef::*;

    match def {
        Body(loc_pattern, loc_expr) => {
            // let first = desugar_expr(arena, loc_expr, src, line_info, module_path);

            // if dbg!(module_path.contains("test.roc")) {
            //     dbg!(&first);
            // }

            // let result = unwrap_suffixed_expression(arena, first, false, module_path);

            // if dbg!(module_path.contains("test.roc")) {
            //     dbg!(&result);
            // }

            // match result {
            //     Unwrapped::NothingUnwrapped(loc_expr) | Unwrapped::UnwrappedExpr(loc_expr) => Body(
            //         desugar_loc_pattern(arena, loc_pattern, src, line_info, module_path),
            //         desugar_expr(arena, loc_expr, src, line_info, module_path),
            //     ),
            //     Unwrapped::UnwrappedSubExpr { .. } => {
            //         internal_error!("this should have been desugared elsewhere...")
            //     }
            // }

            Body(
            desugar_loc_pattern(arena, loc_pattern, src, line_info, module_path),
            desugar_expr(arena, loc_expr, src, line_info, module_path),
            )
        }
        ann @ Annotation(_, _) => *ann,
        AnnotatedBody {
            ann_pattern,
            ann_type,
            comment,
            body_pattern,
            body_expr,
        } => {
            // let first = desugar_expr(arena, body_expr, src, line_info, module_path);
            // let result = unwrap_suffixed_expression(arena, first, false,module_path);

            // if module_path.contains("test.roc") {
            //     dbg!(top_level_def, &first, &result);
            // }

            // match result {
            //     Unwrapped::NothingUnwrapped(loc_expr) | Unwrapped::UnwrappedExpr(loc_expr) => AnnotatedBody {
            //         ann_pattern,
            //         ann_type,
            //         comment: *comment,
            //         body_pattern,
            //         body_expr: loc_expr,
            //     },
            //     Unwrapped::UnwrappedSubExpr { .. } => {
            //         internal_error!("this should have been desugared elsewhere...")
            //     }
            // }
            AnnotatedBody {
            ann_pattern,
            ann_type,
            comment: *comment,
            body_pattern,
                body_expr,
            }
        }

        Dbg {
            condition,
            preceding_comment,
        } => {
            let desugared_condition =
                &*arena.alloc(desugar_expr(arena, condition, src, line_info, module_path));
            Dbg {
                condition: desugared_condition,
                preceding_comment: *preceding_comment,
            }
        }
        Expect {
            condition,
            preceding_comment,
        } => {
            let desugared_condition =
                &*arena.alloc(desugar_expr(arena, condition, src, line_info, module_path));
            Expect {
                condition: desugared_condition,
                preceding_comment: *preceding_comment,
            }
        }
        ExpectFx {
            condition,
            preceding_comment,
        } => {
            let desugared_condition =
                &*arena.alloc(desugar_expr(arena, condition, src, line_info, module_path));
            ExpectFx {
                condition: desugared_condition,
                preceding_comment: *preceding_comment,
            }
        }

        Stmt(stmt_expr) => {
            // let first = desugar_expr(arena, stmt_expr, src, line_info, module_path);
            // let result = unwrap_suffixed_expression(arena, first, false, module_path);

            // if module_path.contains("test.roc") {
            //     dbg!(top_level_def, &first, &result);
            // }

            // match result {
            //     Unwrapped::NothingUnwrapped(loc_expr) | Unwrapped::UnwrappedExpr(loc_expr) => Stmt(loc_expr),
            //     Unwrapped::UnwrappedSubExpr { .. } => {
            //         internal_error!("this should have been desugared elsewhere...")
            //     }
            // }

            todo!();
        }
    }
}

pub fn desugar_defs_node_values<'a>(
    arena: &'a Bump,
    defs: &mut roc_parse::ast::Defs<'a>,
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
    top_level_def: bool,
) {
    for value_def in defs.value_defs.iter_mut() {
        *value_def = desugar_value_def(
            arena,
            arena.alloc(*value_def),
            src,
            line_info,
            module_path,
        );
    }

    // we only want to unwrap suffixed nodes if this is a top level def
    // this function will be called recursively in desugar_expr and we only 
    // unwrap expression after they have completed desugaring to simplify the analysis
    if top_level_def {

        for value_def in defs.value_defs.iter_mut() {
            
            // unwrap any suffixed defs
            *value_def = desugar_defs_node_values_suffixed(
                arena,
                *value_def,
                src,
                line_info,
                module_path,
            );
        }
    }
}

pub fn desugar_defs_node_values_suffixed<'a>(
    arena: &'a Bump,
    value_def: ValueDef<'a>,
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
) -> ValueDef<'a>{
    use ValueDef::*;

    match value_def {
        Body(loc_pattern, loc_expr) => {
          
            // note called_from_def is passed as `false` as this is a top_level_def
            let result = unwrap_suffixed_expression(arena, loc_expr, false, module_path);

            if dbg!(module_path.contains("test.roc")) {
                dbg!(&result);
            }

            match result {
                Unwrapped::NothingUnwrapped(loc_expr) | Unwrapped::UnwrappedExpr(loc_expr) => Body(
                    desugar_loc_pattern(arena, loc_pattern, src, line_info, module_path),
                    desugar_expr(arena, loc_expr, src, line_info, module_path),
                ),
                Unwrapped::UnwrappedSubExpr { .. } => {
                    internal_error!("this should have been desugared elsewhere...")
                }
            }

        }
        ann @ Annotation(_, _) => ann,
        AnnotatedBody {
            ann_pattern,
            ann_type,
            comment,
            body_pattern,
            body_expr,
        } => {

            // note called_from_def is passed as `false` as this is a top_level_def
            let result = unwrap_suffixed_expression(arena, body_expr, false,module_path);

            if module_path.contains("test.roc") {
                dbg!(&result);
            }

            match result {
                Unwrapped::NothingUnwrapped(loc_expr) | Unwrapped::UnwrappedExpr(loc_expr) => AnnotatedBody {
                    ann_pattern,
                    ann_type,
                    comment: comment,
                    body_pattern,
                    body_expr: loc_expr,
                },
                Unwrapped::UnwrappedSubExpr { .. } => {
                    internal_error!("this should have been desugared elsewhere...")
                }
            }
        }

        // TODO support desugaring of Dbg, Expect, and ExpectFx
        Dbg {.. } | Expect {..} | ExpectFx {.. } => value_def,

        Stmt(stmt_expr) => {
            // let first = desugar_expr(arena, stmt_expr, src, line_info, module_path);
            // let result = unwrap_suffixed_expression(arena, first, false, module_path);

            // if module_path.contains("test.roc") {
            //     dbg!(top_level_def, &first, &result);
            // }

            // match result {
            //     Unwrapped::NothingUnwrapped(loc_expr) | Unwrapped::UnwrappedExpr(loc_expr) => Stmt(loc_expr),
            //     Unwrapped::UnwrappedSubExpr { .. } => {
            //         internal_error!("this should have been desugared elsewhere...")
            //     }
            // }

            todo!();
        }
    }
}

// fn desugar_defs_node_suffixed<'a>(
//     arena: &'a Bump,
//     loc_expr: &'a Loc<Expr<'a>>,
// ) -> &'a Loc<Expr<'a>> {
//     match loc_expr.value {
//         Defs(defs, loc_ret) => {
//             match defs.search_suffixed_defs() {
//                 None => loc_expr,
//                 Some((tag_index, value_index)) => {
//                     if defs.value_defs.len() == 1 {
//                         // We have only one value_def and it must be Suffixed
//                         // replace Defs with an Apply(Task.await) and Closure of loc_return

//                         debug_assert!(
//                             value_index == 0,
//                             "we have only one value_def and so it must be Suffixed "
//                         );

//                         // Unwrap Suffixed def within Apply, and the pattern so we can use in the call to Task.await
//                         let (suffixed_sub_loc_expr, pattern) =
//                             unwrap_suffixed_value_def(arena, defs.value_defs[0]);

//                         // Create Closure for the result of the recursion,
//                         // use the pattern from our Suffixed Def as closure argument
//                         let closure_expr = Closure(arena.alloc([*pattern]), loc_ret);

//                         // Apply arguments to Task.await, first is the unwrapped Suffix expr second is the Closure
//                         let mut task_await_apply_args: Vec<&'a Loc<Expr<'a>>> = Vec::new_in(arena);

//                         task_await_apply_args.push(suffixed_sub_loc_expr);
//                         task_await_apply_args
//                             .push(arena.alloc(Loc::at(loc_expr.region, closure_expr)));

//                         arena.alloc(Loc::at(
//                             loc_expr.region,
//                             Apply(
//                                 arena.alloc(Loc {
//                                     region: loc_expr.region,
//                                     value: Var {
//                                         module_name: ModuleName::TASK,
//                                         ident: "await",
//                                         suffixed: 0,
//                                     },
//                                 }),
//                                 arena.alloc(task_await_apply_args),
//                                 CalledVia::BangSuffix,
//                             ),
//                         ))
//                     } else if value_index == 0 {
//                         // We have a Suffixed in first index, and also other nodes in Defs
//                         // pop the first Suffixed and recurse on Defs (without first) to handle any other Suffixed
//                         // the result will be wrapped in an Apply(Task.await) and Closure

//                         debug_assert!(
//                             defs.value_defs.len() > 1,
//                             "we know we have other Defs that will need to be considered"
//                         );

//                         // Unwrap Suffixed def within Apply, and the pattern so we can use in the call to Task.await
//                         let (suffixed_sub_loc_expr, pattern) =
//                             unwrap_suffixed_value_def(arena, defs.value_defs[0]);

//                         // Get a mutable copy of the defs
//                         let mut copied_defs = defs.clone();

//                         // Remove the suffixed def
//                         copied_defs.remove_value_def(tag_index);

//                         // Recurse using new Defs to get new expression
//                         let new_loc_expr = desugar_defs_node_suffixed(
//                             arena,
//                             arena.alloc(Loc::at(
//                                 loc_expr.region,
//                                 Defs(arena.alloc(copied_defs), loc_ret),
//                             )),
//                         );

//                         // Create Closure for the result of the recursion,
//                         // use the pattern from our Suffixed Def as closure argument
//                         let closure_expr = Closure(arena.alloc([*pattern]), new_loc_expr);

//                         // Apply arguments to Task.await, first is the unwrapped Suffix expr second is the Closure
//                         let mut task_await_apply_args: Vec<&'a Loc<Expr<'a>>> = Vec::new_in(arena);

//                         task_await_apply_args.push(suffixed_sub_loc_expr);
//                         task_await_apply_args
//                             .push(arena.alloc(Loc::at(loc_expr.region, closure_expr)));

//                         arena.alloc(Loc::at(
//                             loc_expr.region,
//                             Apply(
//                                 arena.alloc(Loc {
//                                     region: loc_expr.region,
//                                     value: Var {
//                                         module_name: ModuleName::TASK,
//                                         ident: "await",
//                                         suffixed: 0,
//                                     },
//                                 }),
//                                 arena.alloc(task_await_apply_args),
//                                 CalledVia::BangSuffix,
//                             ),
//                         ))
//                     } else {
//                         // The first Suffixed is in the middle of our Defs
//                         // We will keep the defs before the Suffixed in our Defs node
//                         // We take the defs after the Suffixed and create a new Defs node using the current loc_return
//                         // Then recurse on the new Defs node, wrap the result in an Apply(Task.await) and Closure,
//                         // which will become the new loc_return

//                         let (before, after) = {
//                             let values = defs.split_values_either_side_of(tag_index);
//                             (values.before, values.after)
//                         };

//                         // If there are no defs after, then just use loc_ret as we dont need a Defs node
//                         let defs_after_suffixed_desugared = {
//                             if !after.is_empty() {
//                                 desugar_defs_node_suffixed(
//                                     arena,
//                                     arena.alloc(Loc::at(
//                                         loc_expr.region,
//                                         Defs(arena.alloc(after), loc_ret),
//                                     )),
//                                 )
//                             } else {
//                                 loc_ret
//                             }
//                         };

//                         // Unwrap Suffixed def within Apply, and the pattern so we can use in the call to Task.await
//                         let (suffixed_sub_loc_expr, pattern) =
//                             unwrap_suffixed_value_def(arena, defs.value_defs[value_index]);

//                         // Create Closure for the result of the recursion,
//                         // use the pattern from our Suffixed Def as closure argument
//                         let closure_expr =
//                             Closure(arena.alloc([*pattern]), defs_after_suffixed_desugared);

//                         // Apply arguments to Task.await, first is the unwrapped Suffix expr second is the Closure
//                         let mut task_await_apply_args: Vec<&'a Loc<Expr<'a>>> = Vec::new_in(arena);

//                         task_await_apply_args.push(suffixed_sub_loc_expr);
//                         task_await_apply_args
//                             .push(arena.alloc(Loc::at(loc_expr.region, closure_expr)));

//                         let new_loc_return = arena.alloc(Loc::at(
//                             loc_expr.region,
//                             Apply(
//                                 arena.alloc(Loc {
//                                     region: loc_expr.region,
//                                     value: Var {
//                                         module_name: ModuleName::TASK,
//                                         ident: "await",
//                                         suffixed: 0,
//                                     },
//                                 }),
//                                 arena.alloc(task_await_apply_args),
//                                 CalledVia::BangSuffix,
//                             ),
//                         ));

//                         arena.alloc(Loc::at(
//                             loc_expr.region,
//                             Defs(arena.alloc(before), new_loc_return),
//                         ))
//                     }
//                 }
//             }
//         }
//         _ => unreachable!(
//             "should only be passed a Defs node as it is called from within desugar_expr for Defs"
//         ),
//     }
// }

// Unwrap suffixed value_def so we can use in a call to Task.await
// fn unwrap_suffixed_value_def<'a>(
//     arena: &'a Bump,
//     value_def: ValueDef<'a>,
// ) -> (
//     &'a Loc<roc_parse::ast::Expr<'a>>,
//     &'a Loc<roc_parse::ast::Pattern<'a>>,
// ) {
//     match value_def {
//         ValueDef::Stmt(_) => {
//             internal_error!("this should have been desugared elswhere...")
//         }
//         ValueDef::Body(loc_pattern, loc_expr) => {
//             (unwrap_suffixed_loc_expr(arena, loc_expr), loc_pattern)
//         }
//         _ => unreachable!("should have a suffixed Body value_def"),
//     }
// }

// fn unwrap_suffixed_loc_expr<'a>(
//     arena: &'a Bump,
//     loc_expr: &Loc<Expr<'a>>,
// ) -> &'a Loc<roc_parse::ast::Expr<'a>> {
//     match loc_expr.value {
//         // Arguments applied e.g. `Stdout.line! "Hello World"`
//         Apply(
//             Loc {
//                 value:
//                     Var {
//                         suffixed,
//                         module_name,
//                         ident,
//                     },
//                 ..
//             },
//             args,
//             called_via,
//         ) if suffixed > &0 => arena.alloc(Loc::at(
//             loc_expr.region,
//             Apply(
//                 arena.alloc(Loc::at(
//                     loc_expr.region,
//                     Var {
//                         module_name,
//                         ident,
//                         suffixed: 0,
//                     },
//                 )),
//                 args,
//                 called_via,
//             ),
//         )),
//         // NIL arguments applied e.g. `Stdin.line!`
//         Var {
//             suffixed,
//             module_name,
//             ident,
//         } if suffixed > 0 => arena.alloc(Loc::at(
//             loc_expr.region,
//             Var {
//                 module_name,
//                 ident,
//                 suffixed: 0,
//             },
//         )),
//         _ => {
//             unreachable!("should have a suffixed Var inside a Body value_def")
//         }
//     }
// }

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

/// Reorder the expression tree based on operator precedence and associativity rules,
/// then replace the BinOp nodes with Apply nodes. Also drop SpaceBefore and SpaceAfter nodes.
pub fn desugar_expr<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
) -> &'a Loc<Expr<'a>> {
    match &loc_expr.value {
        Float(..)
        | Num(..)
        | NonBase10Int { .. }
        | SingleQuote(_)
        | AccessorFunction(_)
        | Var { .. }
        | Underscore { .. }
        | MalformedIdent(_, _)
        | MalformedClosure
        | PrecedenceConflict { .. }
        | MultipleRecordBuilders { .. }
        | UnappliedRecordBuilder { .. }
        | Tag(_)
        | OpaqueRef(_)
        | IngestedFile(_, _)
        | Crash => loc_expr,

        Str(str_literal) => match str_literal {
            StrLiteral::PlainLine(_) => loc_expr,
            StrLiteral::Line(segments) => {
                let region = loc_expr.region;
                let value = Str(StrLiteral::Line(desugar_str_segments(
                    arena,
                    segments,
                    src,
                    line_info,
                    module_path,
                )));

                arena.alloc(Loc { region, value })
            }
            StrLiteral::Block(lines) => {
                let region = loc_expr.region;
                let new_lines = Vec::from_iter_in(
                    lines.iter().map(|segments| {
                        desugar_str_segments(arena, segments, src, line_info, module_path)
                    }),
                    arena,
                );
                let value = Str(StrLiteral::Block(new_lines.into_bump_slice()));

                arena.alloc(Loc { region, value })
            }
        },

        TupleAccess(sub_expr, paths) => {
            let region = loc_expr.region;
            let loc_sub_expr = Loc {
                region,
                value: **sub_expr,
            };
            let value = TupleAccess(
                &desugar_expr(
                    arena,
                    arena.alloc(loc_sub_expr),
                    src,
                    line_info,
                    module_path,
                )
                .value,
                paths,
            );

            arena.alloc(Loc { region, value })
        }
        RecordAccess(sub_expr, paths) => {
            let region = loc_expr.region;
            let loc_sub_expr = Loc {
                region,
                value: **sub_expr,
            };
            let value = RecordAccess(
                &desugar_expr(
                    arena,
                    arena.alloc(loc_sub_expr),
                    src,
                    line_info,
                    module_path,
                )
                .value,
                paths,
            );

            arena.alloc(Loc { region, value })
        }
        List(items) => {
            let mut new_items = Vec::with_capacity_in(items.len(), arena);

            for item in items.iter() {
                new_items.push(desugar_expr(arena, item, src, line_info, module_path));
            }
            let new_items = new_items.into_bump_slice();
            let value: Expr<'a> = List(items.replace_items(new_items));

            arena.alloc(Loc {
                region: loc_expr.region,
                value,
            })
        }
        Record(fields) => {
            let mut allocated = Vec::with_capacity_in(fields.len(), arena);
            for field in fields.iter() {
                let value = desugar_field(arena, &field.value, src, line_info, module_path);
                allocated.push(Loc {
                    value,
                    region: field.region,
                });
            }
            let fields = fields.replace_items(allocated.into_bump_slice());
            arena.alloc(Loc {
                region: loc_expr.region,
                value: Record(fields),
            })
        }
        Tuple(fields) => {
            let mut allocated = Vec::with_capacity_in(fields.len(), arena);
            for field in fields.iter() {
                let expr = desugar_expr(arena, field, src, line_info, module_path);
                allocated.push(expr);
            }
            let fields = fields.replace_items(allocated.into_bump_slice());
            arena.alloc(Loc {
                region: loc_expr.region,
                value: Tuple(fields),
            })
        }
        RecordUpdate { fields, update } => {
            // NOTE the `update` field is always a `Var { .. }`, we only desugar it to get rid of
            // any spaces before/after
            let new_update = desugar_expr(arena, update, src, line_info, module_path);

            let mut allocated = Vec::with_capacity_in(fields.len(), arena);
            for field in fields.iter() {
                let value = desugar_field(arena, &field.value, src, line_info, module_path);
                allocated.push(Loc {
                    value,
                    region: field.region,
                });
            }
            let new_fields = fields.replace_items(allocated.into_bump_slice());

            arena.alloc(Loc {
                region: loc_expr.region,
                value: RecordUpdate {
                    update: new_update,
                    fields: new_fields,
                },
            })
        }
        Closure(loc_patterns, loc_ret) => arena.alloc(Loc {
            region: loc_expr.region,
            value: Closure(
                desugar_loc_patterns(arena, loc_patterns, src, line_info, module_path),
                desugar_expr(arena, loc_ret, src, line_info, module_path),
            ),
        }),
        Backpassing(loc_patterns, loc_body, loc_ret) => {
            // loc_patterns <- loc_body
            //
            // loc_ret

            // first desugar the body, because it may contain |>
            let desugared_body = desugar_expr(arena, loc_body, src, line_info, module_path);

            let desugared_ret = desugar_expr(arena, loc_ret, src, line_info, module_path);
            let desugared_loc_patterns =
                desugar_loc_patterns(arena, loc_patterns, src, line_info, module_path);
            let closure = Expr::Closure(desugared_loc_patterns, desugared_ret);
            let loc_closure = Loc::at(loc_expr.region, closure);

            match &desugared_body.value {
                Expr::Apply(function, arguments, called_via) => {
                    let mut new_arguments: Vec<'a, &'a Loc<Expr<'a>>> =
                        Vec::with_capacity_in(arguments.len() + 1, arena);
                    new_arguments.extend(arguments.iter());
                    new_arguments.push(arena.alloc(loc_closure));

                    let call = Expr::Apply(function, new_arguments.into_bump_slice(), *called_via);
                    let loc_call = Loc::at(loc_expr.region, call);

                    arena.alloc(loc_call)
                }
                _ => {
                    // e.g. `x <- (if b then (\a -> a) else (\c -> c))`
                    let call = Expr::Apply(
                        desugared_body,
                        arena.alloc([&*arena.alloc(loc_closure)]),
                        CalledVia::Space,
                    );
                    let loc_call = Loc::at(loc_expr.region, call);

                    arena.alloc(loc_call)
                }
            }
        }
        RecordBuilder(_) => arena.alloc(Loc {
            value: UnappliedRecordBuilder(loc_expr),
            region: loc_expr.region,
        }),
        BinOps(lefts, right) => desugar_bin_ops(
            arena,
            loc_expr.region,
            lefts,
            right,
            src,
            line_info,
            module_path,
        ),
        Defs(defs, loc_ret) => {
            let mut defs = (*defs).clone();
            desugar_defs_node_values(arena, &mut defs, src, line_info, module_path, false);
            let loc_ret = desugar_expr(arena, loc_ret, src, line_info, module_path);

            // Desugar any suffixed nodes, such as `foo = bar!`
            // desugar_defs_node_suffixed(
            //     arena,
            //     arena.alloc(Loc::at(loc_expr.region, Defs(arena.alloc(defs), loc_ret))),
            // )

            arena.alloc(Loc::at(loc_expr.region, Defs(arena.alloc(defs), loc_ret)))
        }
        Apply(loc_fn, loc_args, called_via) => {
            let mut desugared_args = Vec::with_capacity_in(loc_args.len(), arena);
            let mut builder_apply_exprs = None;

            for loc_arg in loc_args.iter() {
                let mut current = loc_arg.value;
                let arg = loop {
                    match current {
                        RecordBuilder(fields) => {
                            if builder_apply_exprs.is_some() {
                                return arena.alloc(Loc {
                                    value: MultipleRecordBuilders(loc_expr),
                                    region: loc_expr.region,
                                });
                            }

                            let builder_arg = record_builder_arg(arena, loc_arg.region, fields);
                            builder_apply_exprs = Some(builder_arg.apply_exprs);

                            break builder_arg.closure;
                        }
                        SpaceBefore(expr, _) | SpaceAfter(expr, _) => {
                            current = *expr;
                        }
                        _ => break loc_arg,
                    }
                };

                desugared_args.push(desugar_expr(arena, arg, src, line_info, module_path));
            }

            let desugared_args = desugared_args.into_bump_slice();

            let mut apply: &Loc<Expr> = arena.alloc(Loc {
                value: Apply(
                    desugar_expr(arena, loc_fn, src, line_info, module_path),
                    desugared_args,
                    *called_via,
                ),
                region: loc_expr.region,
            });

            match builder_apply_exprs {
                None => {}

                Some(apply_exprs) => {
                    for expr in apply_exprs {
                        let desugared_expr = desugar_expr(arena, expr, src, line_info, module_path);

                        let args = std::slice::from_ref(arena.alloc(apply));

                        apply = arena.alloc(Loc {
                            value: Apply(desugared_expr, args, CalledVia::RecordBuilder),
                            region: loc_expr.region,
                        });
                    }
                }
            }

            apply
        }
        When(loc_cond_expr, branches) => {
            let loc_desugared_cond = &*arena.alloc(desugar_expr(
                arena,
                loc_cond_expr,
                src,
                line_info,
                module_path,
            ));
            let mut desugared_branches = Vec::with_capacity_in(branches.len(), arena);

            for branch in branches.iter() {
                let desugared_expr =
                    desugar_expr(arena, &branch.value, src, line_info, module_path);
                let desugared_patterns =
                    desugar_loc_patterns(arena, branch.patterns, src, line_info, module_path);

                let desugared_guard = if let Some(guard) = &branch.guard {
                    Some(*desugar_expr(arena, guard, src, line_info, module_path))
                } else {
                    None
                };

                desugared_branches.push(&*arena.alloc(WhenBranch {
                    patterns: desugared_patterns,
                    value: *desugared_expr,
                    guard: desugared_guard,
                }));
            }

            let desugared_branches = desugared_branches.into_bump_slice();

            arena.alloc(Loc {
                value: When(loc_desugared_cond, desugared_branches),
                region: loc_expr.region,
            })
        }
        UnaryOp(loc_arg, loc_op) => {
            use roc_module::called_via::UnaryOp::*;

            let region = loc_op.region;
            let op = loc_op.value;
            // TODO desugar this in canonicalization instead, so we can work
            // in terms of integers exclusively and not need to create strings
            // which canonicalization then needs to look up, check if they're exposed, etc
            let value = match op {
                Negate => Var {
                    module_name: ModuleName::NUM,
                    ident: "neg",
                    suffixed: 0,
                },
                Not => Var {
                    module_name: ModuleName::BOOL,
                    ident: "not",
                    suffixed: 0,
                },
            };
            let loc_fn_var = arena.alloc(Loc { region, value });
            let desugared_args =
                arena.alloc([desugar_expr(arena, loc_arg, src, line_info, module_path)]);

            arena.alloc(Loc {
                value: Apply(loc_fn_var, desugared_args, CalledVia::UnaryOp(op)),
                region: loc_expr.region,
            })
        }
        SpaceBefore(expr, _) | SpaceAfter(expr, _) => {
            // Since we've already begun canonicalization, spaces and parens
            // are no longer needed and should be dropped.
            desugar_expr(
                arena,
                arena.alloc(Loc {
                    value: **expr,
                    region: loc_expr.region,
                }),
                src,
                line_info,
                module_path,
            )
        }
        ParensAround(expr) => {
            let desugared = desugar_expr(
                arena,
                arena.alloc(Loc {
                    value: **expr,
                    region: loc_expr.region,
                }),
                src,
                line_info,
                module_path,
            );

            arena.alloc(Loc {
                value: ParensAround(&desugared.value),
                region: loc_expr.region,
            })
        }
        If(if_thens, final_else_branch) => {
            // If does not get desugared into `when` so we can give more targeted error messages during type checking.
            let desugared_final_else = &*arena.alloc(desugar_expr(
                arena,
                final_else_branch,
                src,
                line_info,
                module_path,
            ));

            let mut desugared_if_thens = Vec::with_capacity_in(if_thens.len(), arena);

            for (condition, then_branch) in if_thens.iter() {
                desugared_if_thens.push((
                    *desugar_expr(arena, condition, src, line_info, module_path),
                    *desugar_expr(arena, then_branch, src, line_info, module_path),
                ));
            }

                arena.alloc(Loc {
                    value: If(desugared_if_thens.into_bump_slice(), desugared_final_else),
                    region: loc_expr.region,
            })

            // Desugar any suffixed nodes, such as `if isTrue! then ...`
            // desugar_if_node_suffixed(
            //     arena,
            //     arena.alloc(Loc {
            //         value: If(desugared_if_thens.into_bump_slice(), desugared_final_else),
            //         region: loc_expr.region,
            //     }),
            // )
        }
        Expect(condition, continuation) => {
            let desugared_condition =
                &*arena.alloc(desugar_expr(arena, condition, src, line_info, module_path));
            let desugared_continuation = &*arena.alloc(desugar_expr(
                arena,
                continuation,
                src,
                line_info,
                module_path,
            ));
            arena.alloc(Loc {
                value: Expect(desugared_condition, desugared_continuation),
                region: loc_expr.region,
            })
        }
        Dbg(condition, continuation) => {
            // Desugars a `dbg x` statement into essentially
            // Inspect.toStr x |> LowLevelDbg
            let desugared_continuation = &*arena.alloc(desugar_expr(
                arena,
                continuation,
                src,
                line_info,
                module_path,
            ));

            let region = condition.region;
            // Inspect.toStr x
            let inspect_fn = Var {
                module_name: ModuleName::INSPECT,
                ident: "toStr",
                suffixed: 0,
            };
            let loc_inspect_fn_var = arena.alloc(Loc {
                value: inspect_fn,
                region,
            });
            let desugared_inspect_args =
                arena.alloc([desugar_expr(arena, condition, src, line_info, module_path)]);

            let dbg_str = arena.alloc(Loc {
                value: Apply(loc_inspect_fn_var, desugared_inspect_args, CalledVia::Space),
                region,
            });

            // line_info is an option so that we can lazily calculate it.
            // That way it there are no `dbg` statements, we never pay the cast of scanning the source an extra time.
            if line_info.is_none() {
                *line_info = Some(LineInfo::new(src));
            }
            let line_col = line_info.as_ref().unwrap().convert_pos(region.start());

            let dbg_src = src
                .split_at(region.start().offset as usize)
                .1
                .split_at((region.end().offset - region.start().offset) as usize)
                .0;

            // |> LowLevelDbg
            arena.alloc(Loc {
                value: LowLevelDbg(
                    arena.alloc((
                        &*arena.alloc_str(&format!("{}:{}", module_path, line_col.line + 1)),
                        &*arena.alloc_str(dbg_src),
                    )),
                    dbg_str,
                    desugared_continuation,
                ),
                region: loc_expr.region,
            })
        }
        LowLevelDbg(_, _, _) => unreachable!("Only exists after desugaring"),
    }
}

#[derive(Debug)]
pub enum Unwrapped<'a> {
    // the expression has nothing further to unwrap
    NothingUnwrapped(&'a Loc<Expr<'a>>),

    // the expression was unwrapped, consumes the def and following expression
    // e.g. `x = foo!` unwrapped `Task.await (foo) \x -> ...`
    UnwrappedExpr(&'a Loc<Expr<'a>>),

    // the expression had a (sub) expression unwrapped, doesn't affect the def or following expression
    // e.g. `x = bar! (foo!)` unwrapped `x = Task.await (foo) \#answer1 -> bar! #answer1`
    UnwrappedSubExpr {
        // this is the unwrapped suffixed wich will be applied to the Task.await
        arg: &'a Loc<Expr<'a>>,

        // this pattern will be used in the closure
        pat: Loc<Pattern<'a>>,

        // this expression will replace the suffixed in the parent
        new: &'a Loc<Expr<'a>>,
    },
}

impl Unwrapped<'_> {
    pub fn get<'a>(&'a mut self) -> &'a Loc<Expr<'a>> {
        match self {
            Unwrapped::NothingUnwrapped(loc_expr) => loc_expr,
            Unwrapped::UnwrappedExpr(loc_expr) => loc_expr,
            Unwrapped::UnwrappedSubExpr { .. } => {
                internal_error!("expected an unwrapped expression");
            }
        }
    }
}

pub fn unwrap_suffixed_expression<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,

    // None -> we will need to generate a pattern for the closure
    // Some -> first call from a def, we may have a pattern such as "a" or "#answer2" to use
    // maybe_pattern_expr: Option<&'a Loc<Expr<'a>>>,

    called_from_def: bool,

    // USEFUL HERE FOR DEBUGGING
    // e.g.
    // if module_path.contains("test.roc"){
    //     dbg!("unwrap_suffixed_expression", loc_expr);
    // }
    module_path: &str,
) -> Unwrapped<'a> {
    // if module_path.contains("test.roc") {
    //     dbg!("unwrap_suffixed_expression");
    // }

    dbg!("unwrap_suffixed_expression", &loc_expr);

    match loc_expr.value {
        Float(..) | Num(..) | NonBase10Int { .. } | Str(..) | SingleQuote(..) => {
            Unwrapped::NothingUnwrapped(loc_expr)
        }
        Expr::Var { suffixed, .. } if suffixed == 0 => Unwrapped::NothingUnwrapped(loc_expr),

        Expr::Var {
            module_name,
            ident,
            suffixed,
        } if suffixed > 0 => {

            if suffixed > 1 {
                /*
                ## Example with multiple suffix
                {} = foo!!
                bar

                Task.await (foo) \answer1 ->
                    {} = (answer1)!
                    bar

                Task.await (foo) \answer1 ->
                    Task.await (answer1) \{} -> bar
                */
                internal_error!("TODO support multiple suffixes");
            }

            if called_from_def {
                Unwrapped::UnwrappedExpr(arena.alloc(Loc::at(
                                    loc_expr.region,
                    Expr::Var {
                        module_name,
                        ident,
                        suffixed: suffixed.saturating_sub(1),
                    },
                )))
            } else {
                internal_error!("unwrapping Var {:?} within a sub expression... this should have been handled at the previous level", loc_expr);
            }
        },

        Expr::Defs(..) => {
            unwrap_suffixed_expression_defs_help(arena, loc_expr, called_from_def, module_path)
        }


        Expr::Apply(function, apply_args, called_via) => {
            
            for (arg_index, arg) in apply_args.iter().enumerate() {
                /*
                ## Example of suffixed function within an Apply
                x = bar (foo! "hello")
                baz x
                
                x = Task.await (foo "hello") \#!a -> bar (#!a)
                baz x
                */

                // try to unwrap the argument
                match unwrap_suffixed_expression(arena, *arg, false,module_path) {
                    Unwrapped::NothingUnwrapped(..) => {
                        todo!();
                    },
                    Unwrapped::UnwrappedExpr(new_arg) => {
                        /*
                        foo! (
                            x = bar!
                            x
                        ) "hello"
                        */
                        todo!();
                        // replace the argument with the unwrapped version
                        // let (before, after) = split_loc_exprs_around(apply_args, arg_index);

                        // let mut new_args = Vec::new_in(arena);
                        // new_args.extend(before.iter());
                        // new_args.push(new_arg);
                        // new_args.extend(after.iter());

                        // let new_apply = Loc::at(
                        //     *region,
                        //     Expr::Apply(
                        //         arena.alloc(Loc::at(
                        //             *region,
                        //             Var {
                        //                 module_name,
                        //                 ident,
                        //                 suffixed: 0,
                        //             },
                        //         )),
                        //         new_args.into_bump_slice(),
                        //         called_via,
                        //     ),
                        // );

                        // return unwrap_suffixed_expression(arena, arena.alloc(new_apply), module_path);
                    },
                    Unwrapped::UnwrappedSubExpr { arg, pat, new } => {
                        todo!();
                    }
                }
            }

            // try to unwrap the function
            match unwrap_suffixed_expression(arena, function, false, module_path) {
                Unwrapped::NothingUnwrapped(..) => Unwrapped::NothingUnwrapped(loc_expr),
                Unwrapped::UnwrappedExpr(new_function) => {
                    /*
                    ## Example of suffixed function within an Apply
                    x = (foo! "bar") "hello"
                        ^^^^^^^^^^^^ is our suffixed function
                    baz x
                    
                    Task.await (foo "bar") \#!a1 -> 
                        x = #!a1 "hello"
                        bar x
                    */
                    
                    let arg = Loc::at(loc_expr.region, Apply(
                        arena.alloc(new_function), apply_args, called_via));

                    let new = Loc::at(loc_expr.region, Apply(
                        arena.alloc(new_function), apply_args, called_via));

                    return Unwrapped::UnwrappedSubExpr {
                        arg: arena.alloc(arg),
                        pat: Loc::at(loc_expr.region, next_suffixed_answer_pattern(arena)),
                        new: arena.alloc(new),
                    };
                }
                Unwrapped::UnwrappedSubExpr { arg, pat, new } => { 
                return Unwrapped::UnwrappedSubExpr {
                    arg,
                    pat,
                    new: arena.alloc(Loc::at(
                        loc_expr.region,
                            Expr::Apply(new, apply_args, called_via),
                    )),
                };
            }
            }
        }

        // Expr::Var {
        //     module_name,
        //     ident,
        //     suffixed,
        // } if suffixed > 0 => {
        //     /*
        //     ## Example with single suffix
        //     x = foo!
        //     bar x

        //     Task.await (foo) \x -> bar x

        //     // must have a next expression to progress
        //     let next_loc_expr = match maybe_next_expr {
        //         None => return Err(UnwrappedError::MissingNextInVar),
        //         Some(next_loc_expr) => next_loc_expr,
        //     };

        //     // use the pattern from the parent expression, or create a unit pattern
        //     // e.g. ("{}", None) or ("x",Some(Var{"x"}))
        //     let (loc_pattern, maybe_ident) =
        //         pattern_thing.unwrap_or_else(|| (
        //             arena.alloc([Loc::at(
        //                 loc_expr.region,
        //                 Pattern::RecordDestructure(Collection::empty()),
        //                 )]),
        //             None,
        //         ));

        //     // recurse to get the next expression
        //     let loc_expr_to_wrap = unwrap_innermost_suffixed(
        //         arena,
        //         next_loc_expr(maybe_ident),

        //         // we are in a Expr::Var, so we cannot have a pattern or sub expression
        //         None,
        //         None,
        //     )?;

        //     Ok(apply_task_await(
        //         arena,
        //         loc_expr.region,

        //         // we have desugared a suffixed Var, the argument to Task.await
        //         // will be the base identifier without a suffix
        //         arena.alloc(Loc::at(
        //             loc_expr.region,
        //             Var {
        //                 module_name,
        //                 ident,
        //                 suffixed: 0,
        //             },
        //         )),

        //         // the pattern to use in the closure will be from parent like "x" or "{}"
        //         loc_pattern,

        //         // the expression we have just wrapped in Task.await
        //         loc_expr_to_wrap,
        //     ))
        // }

        // Expr::Defs(defs, loc_ret) => {
        //     for (tag_index, type_or_value_def) in defs.defs().enumerate() {
        //         if let Some(ValueDef::Body(def_pattern, def_expr)) = type_or_value_def.err() {

        //             // if we unwrap this def, we will use this pattern in the closure
        //             // the second Some(ident) will be used in place of the
        //             // TODO can we simplify this and not have the first Option?
        //             let def_pattern_expression: Option<(&'a [Loc<Pattern<'a>>], Option<&'a Loc<Expr<'a>>>)> = match def_pattern {
        //                 Loc { value: Pattern::RecordDestructure(_), .. } => Some((&[**def_pattern], None)),
        //                 Loc {  value: Pattern::Identifier { ident, .. }, .. } => Some((&[**def_pattern], Some(arena.alloc(Loc::at(
        //                     loc_expr.region,
        //                     Expr::Var {
        //                         module_name: "",
        //                         ident,
        //                         suffixed: 0,
        //                     },
        //                 ))))),
        //                 _ => internal_error!("expected a RecordDestructure e.g. `{{}} =` or Identifier e.g. `x =` pattern in the LHS of a definition"),
        //             };

        //             // try unwrap this def
        //             match unwrap_innermost_suffixed(
        //                 arena,
        //                 def_expr,
        //                 def_pattern_expression,
        //                 next_defs_expr_fn_help(arena, defs, tag_index, loc_ret),
        //             ) {
        //                 Err(UnwrappedError::NothingToUnwrap) => {
        //                     // do nothing, move on to next def
        //                 }
        //                 Err(err) => return Err(err),
        //                 Ok(new_def_expr) => {
        //                     return Ok(new_def_expr);
        //                 }
        //             }
        //         }

        //         if let Some(ValueDef::Stmt(def_expr)) = type_or_value_def.err() {
        //             let def_pattern = arena.alloc(Loc::at(
        //                 loc_expr.region,
        //                 Pattern::RecordDestructure(Collection::empty()),
        //             ));

        //             todo!();
        //         }
        //     }

        //     // nothing was unwrapped
        //     Ok(loc_expr)
        // }

        // Expr::Apply(function, arguments, called_via) => {
        //     // first descend into the arguments as they will get unwrapped first
        //     for (index, arg) in arguments.iter().enumerate() {
        //     }
        // }

        // Expr::Apply(function, arguments, called_via) => {
        //     // first descend into the arguments as they will get unwrapped first
        //     for (index, arg) in arguments.iter().enumerate() {
        //         // check if this argument can be unwrapped
        //         if let unwrapped_result =
        //             unwrap_innermost_suffixed(arena, arg, src, line_info, module_path)?
        //         {
        //             debug_assert!(unwrapped_result.is_unwrapped_sub_expr());

        //             // an argument was unwrapped, so we need to replace the argument with the new expression
        //             let mut new_arguments = Vec::new_in(arena);

        //             // args before
        //             new_arguments.extend_from_slice(&arguments[..index]);

        //             // our replacement arg
        //             new_arguments.extend_from_slice(&[unwrapped_result.get_new()]);

        //             if index + 1 < arguments.len() {
        //                 // args after
        //                 new_arguments.extend_from_slice(&arguments[index + 1..]);
        //             }

        //             return Ok(unwrapped_result.set_new(
        //                 arena,
        //                 arena.alloc(Loc::at(
        //                     loc_expr.region,
        //                     Apply(
        //                         function,
        //                         arena.alloc_slice_copy(new_arguments.as_slice()),
        //                         called_via,
        //                     ),
        //                 )),
        //             ));
        //         }
        //     }

        //     // then check the function call itself
        //     if let unwrapped_result =
        //         unwrap_innermost_suffixed(arena, function, src, line_info, module_path)?
        //     {
        //         debug_assert!(unwrapped_result.is_unwrapped_sub_expr());

        //         return Ok(unwrapped_result.set_new(
        //             arena,
        //             arena.alloc(Loc::at(
        //                 loc_expr.region,
        //                 Apply(unwrapped_result.get_new(), arguments, called_via),
        //             )),
        //         ));
        //     }

        //     // nothing was unwrapped
        //     Ok(NoChange)
        // }

        // Expr::Defs(defs, loc_ret) => {
        //     // first descend into each def in sequence,
        //     // if we have any suffixed expressions to unwrap do these first
        //     for (tag_index, type_or_value_def) in defs.defs().enumerate() {
        //         // we only care about ValueDefs
        //         if let Some(ValueDef::Body(def_pattern, def_expr)) = type_or_value_def.err() {
        //             // check if the def expression can be unwrapped
        //             let unwrapped_result =
        //                 unwrap_innermost_suffixed(arena, def_expr, src, line_info, module_path)?;

        //             let mut new_defs = defs.clone();

        //             new_defs.replace_with_value_def(
        //                 tag_index,
        //                 ValueDef::Body(def_pattern, unwrapped_result.get_new()),
        //                 loc_expr.region,
        //             );

        //             return Ok(unwrapped_result.set_new(
        //                 arena,
        //                 arena.alloc(Loc::at(loc_expr.region, Defs(&new_defs, loc_ret))),
        //             ));

        //             // THIS IS WRONG I THINK
        //             // let split_defs = defs.split_values_either_side_of(tag_index);

        //             // // TODO check if the type annotations stuff things up here...
        //             // let empty_before = split_defs.before.is_empty();
        //             // let empty_after = split_defs.after.is_empty();

        //             // // NIL before, NIL after -> SINGLE
        //             // if empty_before && empty_after {
        //             //     /*
        //             //     ## Example

        //             //     x = foo!                <- single suffixed ValueDef::Body
        //             //     bar x                   <- loc_ret

        //             //     ## Desguared

        //             //     Task.await foo \x ->    <- apply_task_await
        //             //         bar x               <- new expression
        //             //     */

        //             //     // replace our Defs node with the wrapped Task.await expression
        //             //     return Ok(Done(apply_task_await(
        //             //         arena,
        //             //         loc_expr.region,
        //             //         unwrapped_result.get_arg(),
        //             //         unwrapped_result.get_pat(),
        //             //         unwrap_innermost_suffixed(
        //             //             arena,
        //             //             unwrapped_result.replace_rep(arena, new),
        //             //             src,
        //             //             line_info,
        //             //             module_path,
        //             //         ),
        //             //     )));

        //             // NIL before, SOME after -> FIRST
        //             // SOME before, NIL after -> LAST
        //             // SOME before, SOME after -> MIDDLE
        //         }

        //         // we only care about ValueDefs
        //         if let Some(ValueDef::Stmt(def_expr)) = type_or_value_def.err() {
        //             // pattern will be `{}`
        //             todo!();
        //         }
        //     }

        //     // check the def return expression, which shouldn't have any suffixed expressions
        //     if let Err(unwrapped) =
        //         unwrap_innermost_suffixed(arena, loc_ret, src, line_info, module_path)
        //     {
        //         let sub_loc_expr =
        //             arena.alloc(Loc::at(loc_expr.region, Defs(defs, unwrapped.sub_loc_expr)));

        //         return Some(unwrapped.replace_sub_loc_expr(arena, sub_loc_expr));
        //     }
        Expr::SpaceBefore(..) | Expr::SpaceAfter(..) => {
            internal_error!("should have been desugared before unwrapping");
        }

        //     // nothing was suffixed in the defs, so just return
        //     Ok(loc_expr)
        // }
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
            match unwrap_suffixed_expression(arena, return_expr, false, module_path) {
                Unwrapped::NothingUnwrapped(_) => return Unwrapped::NothingUnwrapped(loc_expr),
                Unwrapped::UnwrappedExpr(_) | Unwrapped::UnwrappedSubExpr { .. } => {
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

        _ => {
            if module_path.contains("test.roc") {
                dbg!(loc_expr.value);
                panic!("TODO");
            }
            Unwrapped::NothingUnwrapped(loc_expr)
        }
    }
}

pub fn unwrap_suffixed_expression_defs_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    called_from_def: bool, // TODO do we need this?
    module_path: &str,
) -> Unwrapped<'a> { 
    dbg!("unwrap_suffixed_expression_defs_help", &loc_expr);
    match loc_expr.value {
        Expr::Defs(defs, loc_ret) => {
            // try an unwrap each def, if none can be unwrapped, then try to unwrap the loc_ret
            for (tag_index, type_or_value_def) in defs.defs().enumerate() {
                if let Some(ValueDef::Body(def_pattern, def_expr)) = type_or_value_def.err() {
                    match unwrap_suffixed_expression(arena, def_expr, true, module_path) {
                        Unwrapped::NothingUnwrapped(_) => {
                            // do nothing, move on to check the next def
                        }
                        Unwrapped::UnwrappedExpr(new) => {

                            let split_defs = defs.split_defs_around(tag_index);
                            let before_empty = split_defs.before.is_empty();
                            let after_empty = split_defs.after.is_empty();

                            if before_empty && after_empty {
                                // NIL before, NIL after -> SINGLE
                                // We have only one value_def and it must be Suffixed
                                // replace Defs with an Apply(Task.await) and Closure of loc_return
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
                                    module_path,
                                );
                            } else {
                                todo!()
                            }
                    

                            // NIL before, SOME after -> FIRST
                            // SOME before, NIL after -> LAST
                            // SOME before, SOME after -> MIDDLE

                            

                            // We have a Suffixed in first index, and also other nodes in Defs
                            // pop the first Suffixed and recurse on Defs (without first) to handle any other Suffixed
                            // the result will be wrapped in an Apply(Task.await) and Closure

                            // The first Suffixed is in the middle of our Defs
                            // We will keep the defs before the Suffixed in our Defs node
                            // We take the defs after the Suffixed and create a new Defs node using the current loc_return
                            // Then recurse on the new Defs node, wrap the result in an Apply(Task.await) and Closure,
                            // which will become the new loc_return

                            
                        }
                        Unwrapped::UnwrappedSubExpr { arg, pat, new } => {
                            // the new unwrapped expression replaces the def expression
                            // doesn't affect other defs or consume the ret expression
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
                                module_path,
                            );
                        }
                    }
                }

                if let Some(ValueDef::Stmt(_)) = type_or_value_def.err() {
                    internal_error!("handle Stmt");
                }
            }

            // try to unwrap the loc_ret
            match unwrap_suffixed_expression(arena, loc_ret, false, module_path) {
                Unwrapped::NothingUnwrapped(_) => {
                    // nothing in the Expr::Defs was unwrapped, return the original loc_expr
                    Unwrapped::NothingUnwrapped(loc_expr)
                },
                Unwrapped::UnwrappedExpr(new_loc_ret) => {
                    // the loc_ret was unwrapped, replace and return new expression
                    Unwrapped::UnwrappedExpr(arena.alloc(Loc::at(loc_expr.region, Defs(defs, new_loc_ret))))
                }
                Unwrapped::UnwrappedSubExpr { arg, pat, new } => {
                    // the loc_ret was unwrapped
                    let new_loc_ret = apply_task_await(arena, loc_ret.region, arg, pat, new);
                    Unwrapped::UnwrappedExpr(arena.alloc(Loc::at(loc_expr.region, Defs(defs, new_loc_ret))))
                },
            } 
        },
        _ => internal_error!("unreachable, expected a Defs node to be passed into unwrap_suffixed_expression_defs_help"),
    }
}

#[cfg(test)]
mod unwrap_suffixed_expression_tests {
    use roc_parse::test_helpers::parse_defs_with;
    use crate::desugar::unwrap_suffixed_expression;
    use crate::desugar::Bump;
    use crate::desugar::desugar_defs_node_values;
    use bumpalo::collections::Vec;
    use roc_module::called_via::CalledVia;
    // use roc_parse::ast::Collection;
    use roc_parse::ast::Expr;
    use roc_parse::ast::Pattern;
    use roc_parse::ast::ValueDef;
    use roc_region::all::Loc;
    use roc_region::all::Region;
    use roc_test_utils::assert_multiline_str_eq;

    #[test]
    fn single_suffix() {
        /*
        ## Example with single suffix
        x = foo!
        bar x

        Task.await (foo) \x -> bar x
        */

        let arena = &Bump::new();

        let src = 
            r#"
            main = 
                x = foo!
                bar x
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();
        
        dbg!("BEFORE", &defs);
        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);
        dbg!("AFTER", &defs);

        // let arena = Bump::new();

        // let defs = arena.alloc(roc_parse::ast::Defs::default());

        // defs.push_value_def(
        //     ValueDef::Body(
        //         arena.alloc(Loc::at_zero(Pattern::Identifier {
        //             ident: "x",
        //             suffixed: 0,
        //         })),
        //         arena.alloc(Loc::at_zero(Expr::Var {
        //             module_name: "",
        //             ident: "foo",
        //             suffixed: 1,
        //         })),
        //     ),
        //     Region::zero(),
        //     &[],
        //     &[],
        // );

        // let asdf = Loc::at_zero(Expr::Var {
        //     module_name: "",
        //     ident: "x",
        //     suffixed: 0,
        // });

        // let mut arguments = Vec::new_in(&arena);

        // arguments.push(&asdf);

        // let input = arena.alloc(Loc::at_zero(Expr::Defs(
        //     defs,
        //     arena.alloc(Loc::at_zero(Expr::Apply(
        //         arena.alloc(Loc::at_zero(Expr::Var {
        //             module_name: "",
        //             ident: "bar",
        //             suffixed: 0,
        //         })),
        //         arguments.as_slice(),
        //         CalledVia::Space,
        //     ))),
        // )));

        // // dbg!(&input);

        // let output = unwrap_suffixed_expression(&arena, input, "test.roc");

        // // dbg!(&output);

        // let expected = r#"NothingUnwrapped(Apply(Var { module_name: "Task", ident: "await", suffixed: 0 }, [Var { module_name: "", ident: "foo", suffixed: 0 }, Closure([Identifier { ident: "x", suffixed: 0 }], Apply(Var { module_name: "", ident: "bar", suffixed: 0 }, [Var { module_name: "", ident: "x", suffixed: 0 }], Space))], BangSuffix))"#;

        // assert_multiline_str_eq!(format!("{:?}", output).as_str(), expected);
    }

    #[test]
    fn multiple_suffix() {
        /*
        ## Example with multiple suffix
        {} = foo!!
        bar

        Task.await (foo) \answer1 ->
            {} = (answer1)!
            bar

        Task.await (foo) \answer1 ->
            Task.await (answer1) \{} -> bar
        */
        todo!();
    }

    #[test]
    fn multiple_def_first_suffixed() {
        /*
        ## Example with multiple defs, first suffixed
        msg = "hello"
        x = foo! msg
        bar x

        msg = "hello"
        Task.await (foo msg) \x -> bar x
        */
        let arena = &Bump::new();

        let src = 
            r#"
            main = 
                line! "hello"
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();
        
        dbg!("BEFORE", &defs);
        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);
        dbg!("AFTER", &defs);

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
    task_await_apply_args.push(arena.alloc(Loc::at(region, Closure(arena.alloc_slice_copy(closure_pattern.as_slice()), new))));

    // e.g. `Task.await (arg_loc_expr) \pattern -> new`
    arena.alloc(Loc::at(
        region,
        Apply(
            arena.alloc(Loc {
                region: region,
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

fn desugar_str_segments<'a>(
    arena: &'a Bump,
    segments: &'a [StrSegment<'a>],
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
) -> &'a [StrSegment<'a>] {
    Vec::from_iter_in(
        segments.iter().map(|segment| match segment {
            StrSegment::Plaintext(_) | StrSegment::Unicode(_) | StrSegment::EscapedChar(_) => {
                *segment
            }
            StrSegment::DeprecatedInterpolated(loc_expr) => {
                let loc_desugared = desugar_expr(
                    arena,
                    arena.alloc(Loc {
                        region: loc_expr.region,
                        value: *loc_expr.value,
                    }),
                    src,
                    line_info,
                    module_path,
                );
                StrSegment::DeprecatedInterpolated(Loc {
                    region: loc_desugared.region,
                    value: arena.alloc(loc_desugared.value),
                })
            }
            StrSegment::Interpolated(loc_expr) => {
                let loc_desugared = desugar_expr(
                    arena,
                    arena.alloc(Loc {
                        region: loc_expr.region,
                        value: *loc_expr.value,
                    }),
                    src,
                    line_info,
                    module_path,
                );
                StrSegment::Interpolated(Loc {
                    region: loc_desugared.region,
                    value: arena.alloc(loc_desugared.value),
                })
            }
        }),
        arena,
    )
    .into_bump_slice()
}

fn desugar_field<'a>(
    arena: &'a Bump,
    field: &'a AssignedField<'a, Expr<'a>>,
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
) -> AssignedField<'a, Expr<'a>> {
    use roc_parse::ast::AssignedField::*;

    match field {
        RequiredValue(loc_str, spaces, loc_expr) => RequiredValue(
            Loc {
                value: loc_str.value,
                region: loc_str.region,
            },
            spaces,
            desugar_expr(arena, loc_expr, src, line_info, module_path),
        ),
        OptionalValue(loc_str, spaces, loc_expr) => OptionalValue(
            Loc {
                value: loc_str.value,
                region: loc_str.region,
            },
            spaces,
            desugar_expr(arena, loc_expr, src, line_info, module_path),
        ),
        LabelOnly(loc_str) => {
            // Desugar { x } into { x: x }
            let loc_expr = Loc {
                value: Var {
                    module_name: "",
                    ident: loc_str.value,
                    suffixed: 0,
                },
                region: loc_str.region,
            };

            RequiredValue(
                Loc {
                    value: loc_str.value,
                    region: loc_str.region,
                },
                &[],
                desugar_expr(arena, arena.alloc(loc_expr), src, line_info, module_path),
            )
        }
        SpaceBefore(field, _spaces) => desugar_field(arena, field, src, line_info, module_path),
        SpaceAfter(field, _spaces) => desugar_field(arena, field, src, line_info, module_path),

        Malformed(string) => Malformed(string),
    }
}

fn desugar_loc_patterns<'a>(
    arena: &'a Bump,
    loc_patterns: &'a [Loc<Pattern<'a>>],
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
) -> &'a [Loc<Pattern<'a>>] {
    Vec::from_iter_in(
        loc_patterns.iter().map(|loc_pattern| Loc {
            region: loc_pattern.region,
            value: desugar_pattern(arena, loc_pattern.value, src, line_info, module_path),
        }),
        arena,
    )
    .into_bump_slice()
}

fn desugar_loc_pattern<'a>(
    arena: &'a Bump,
    loc_pattern: &'a Loc<Pattern<'a>>,
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
) -> &'a Loc<Pattern<'a>> {
    arena.alloc(Loc {
        region: loc_pattern.region,
        value: desugar_pattern(arena, loc_pattern.value, src, line_info, module_path),
    })
}

fn desugar_pattern<'a>(
    arena: &'a Bump,
    pattern: Pattern<'a>,
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
) -> Pattern<'a> {
    use roc_parse::ast::Pattern::*;

    match pattern {
        Identifier { .. }
        | Tag(_)
        | OpaqueRef(_)
        | NumLiteral(_)
        | NonBase10Literal { .. }
        | FloatLiteral(_)
        | StrLiteral(_)
        | Underscore(_)
        | SingleQuote(_)
        | ListRest(_)
        | Malformed(_)
        | MalformedIdent(_, _)
        | QualifiedIdentifier { .. } => pattern,

        Apply(tag, arg_patterns) => {
            // Skip desugaring the tag, it should either be a Tag or OpaqueRef
            let desugared_arg_patterns = Vec::from_iter_in(
                arg_patterns.iter().map(|arg_pattern| Loc {
                    region: arg_pattern.region,
                    value: desugar_pattern(arena, arg_pattern.value, src, line_info, module_path),
                }),
                arena,
            )
            .into_bump_slice();

            Apply(tag, desugared_arg_patterns)
        }
        RecordDestructure(field_patterns) => {
            let mut allocated = Vec::with_capacity_in(field_patterns.len(), arena);
            for field_pattern in field_patterns.iter() {
                let value =
                    desugar_pattern(arena, field_pattern.value, src, line_info, module_path);
                allocated.push(Loc {
                    value,
                    region: field_pattern.region,
                });
            }
            let field_patterns = field_patterns.replace_items(allocated.into_bump_slice());

            RecordDestructure(field_patterns)
        }
        RequiredField(name, field_pattern) => RequiredField(
            name,
            desugar_loc_pattern(arena, field_pattern, src, line_info, module_path),
        ),
        OptionalField(name, expr) => {
            OptionalField(name, desugar_expr(arena, expr, src, line_info, module_path))
        }
        Tuple(patterns) => {
            let mut allocated = Vec::with_capacity_in(patterns.len(), arena);
            for pattern in patterns.iter() {
                let value = desugar_pattern(arena, pattern.value, src, line_info, module_path);
                allocated.push(Loc {
                    value,
                    region: pattern.region,
                });
            }
            let patterns = patterns.replace_items(allocated.into_bump_slice());

            Tuple(patterns)
        }
        List(patterns) => {
            let mut allocated = Vec::with_capacity_in(patterns.len(), arena);
            for pattern in patterns.iter() {
                let value = desugar_pattern(arena, pattern.value, src, line_info, module_path);
                allocated.push(Loc {
                    value,
                    region: pattern.region,
                });
            }
            let patterns = patterns.replace_items(allocated.into_bump_slice());

            List(patterns)
        }
        As(sub_pattern, symbol) => As(
            desugar_loc_pattern(arena, sub_pattern, src, line_info, module_path),
            symbol,
        ),
        SpaceBefore(sub_pattern, _spaces) => {
            desugar_pattern(arena, *sub_pattern, src, line_info, module_path)
        }
        SpaceAfter(sub_pattern, _spaces) => {
            desugar_pattern(arena, *sub_pattern, src, line_info, module_path)
        }
    }
}

struct RecordBuilderArg<'a> {
    closure: &'a Loc<Expr<'a>>,
    apply_exprs: Vec<'a, &'a Loc<Expr<'a>>>,
}

fn record_builder_arg<'a>(
    arena: &'a Bump,
    region: Region,
    fields: Collection<'a, Loc<RecordBuilderField<'a>>>,
) -> RecordBuilderArg<'a> {
    let mut record_fields = Vec::with_capacity_in(fields.len(), arena);
    let mut apply_exprs = Vec::with_capacity_in(fields.len(), arena);
    let mut apply_field_names = Vec::with_capacity_in(fields.len(), arena);

    // Build the record that the closure will return and gather apply expressions

    for field in fields.iter() {
        let mut current = field.value;

        let new_field = loop {
            match current {
                RecordBuilderField::Value(label, spaces, expr) => {
                    break AssignedField::RequiredValue(label, spaces, expr)
                }
                RecordBuilderField::ApplyValue(label, _, _, expr) => {
                    apply_field_names.push(label);
                    apply_exprs.push(expr);

                    let var = arena.alloc(Loc {
                        region: label.region,
                        value: Expr::Var {
                            module_name: "",
                            ident: arena.alloc("#".to_owned() + label.value),
                            suffixed: 0,
                        },
                    });

                    break AssignedField::RequiredValue(label, &[], var);
                }
                RecordBuilderField::LabelOnly(label) => break AssignedField::LabelOnly(label),
                RecordBuilderField::SpaceBefore(sub_field, _) => {
                    current = *sub_field;
                }
                RecordBuilderField::SpaceAfter(sub_field, _) => {
                    current = *sub_field;
                }
                RecordBuilderField::Malformed(malformed) => {
                    break AssignedField::Malformed(malformed)
                }
            }
        };

        record_fields.push(Loc {
            value: new_field,
            region: field.region,
        });
    }

    let record_fields = fields.replace_items(record_fields.into_bump_slice());

    let mut body = arena.alloc(Loc {
        value: Record(record_fields),
        region,
    });

    // Construct the builder's closure
    //
    // { x: #x, y: #y, z: 3 }
    // \#y -> { x: #x, y: #y, z: 3 }
    // \#x -> \#y -> { x: #x, y: #y, z: 3 }

    for label in apply_field_names.iter().rev() {
        let name = arena.alloc("#".to_owned() + label.value);
        let ident = roc_parse::ast::Pattern::Identifier {
            ident: name,
            suffixed: 0,
        };

        let arg_pattern = arena.alloc(Loc {
            value: ident,
            region: label.region,
        });

        body = arena.alloc(Loc {
            value: Closure(std::slice::from_ref(arg_pattern), body),
            region,
        });
    }

    RecordBuilderArg {
        closure: body,
        apply_exprs,
    }
}

// TODO move this desugaring to canonicalization, so we can use Symbols instead of strings
#[inline(always)]
fn binop_to_function(binop: BinOp) -> (&'static str, &'static str) {
    use self::BinOp::*;

    match binop {
        Caret => (ModuleName::NUM, "pow"),
        Star => (ModuleName::NUM, "mul"),
        Slash => (ModuleName::NUM, "div"),
        DoubleSlash => (ModuleName::NUM, "divTrunc"),
        Percent => (ModuleName::NUM, "rem"),
        Plus => (ModuleName::NUM, "add"),
        Minus => (ModuleName::NUM, "sub"),
        Equals => (ModuleName::BOOL, "isEq"),
        NotEquals => (ModuleName::BOOL, "isNotEq"),
        LessThan => (ModuleName::NUM, "isLt"),
        GreaterThan => (ModuleName::NUM, "isGt"),
        LessThanOrEq => (ModuleName::NUM, "isLte"),
        GreaterThanOrEq => (ModuleName::NUM, "isGte"),
        And => (ModuleName::BOOL, "and"),
        Or => (ModuleName::BOOL, "or"),
        Pizza => unreachable!("Cannot desugar the |> operator"),
        Assignment => unreachable!("Cannot desugar the = operator"),
        IsAliasType => unreachable!("Cannot desugar the : operator"),
        IsOpaqueType => unreachable!("Cannot desugar the := operator"),
        Backpassing => unreachable!("Cannot desugar the <- operator"),
    }
}

fn desugar_bin_ops<'a>(
    arena: &'a Bump,
    whole_region: Region,
    lefts: &'a [(Loc<Expr<'_>>, Loc<BinOp>)],
    right: &'a Loc<Expr<'_>>,
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
) -> &'a Loc<Expr<'a>> {
    let mut arg_stack: Vec<&'a Loc<Expr>> = Vec::with_capacity_in(lefts.len() + 1, arena);
    let mut op_stack: Vec<Loc<BinOp>> = Vec::with_capacity_in(lefts.len(), arena);

    for (loc_expr, loc_op) in lefts {
        arg_stack.push(desugar_expr(arena, loc_expr, src, line_info, module_path));
        match run_binop_step(arena, whole_region, &mut arg_stack, &mut op_stack, *loc_op) {
            Err(problem) => return problem,
            Ok(()) => continue,
        }
    }

    let mut expr = desugar_expr(arena, right, src, line_info, module_path);

    for (left, loc_op) in arg_stack.into_iter().zip(op_stack.into_iter()).rev() {
        expr = arena.alloc(new_op_call_expr(arena, left, loc_op, expr));
    }

    expr
}

enum Step<'a> {
    Error(&'a Loc<Expr<'a>>),
    Push(Loc<BinOp>),
    Skip,
}

fn run_binop_step<'a>(
    arena: &'a Bump,
    whole_region: Region,
    arg_stack: &mut Vec<&'a Loc<Expr<'a>>>,
    op_stack: &mut Vec<Loc<BinOp>>,
    next_op: Loc<BinOp>,
) -> Result<(), &'a Loc<Expr<'a>>> {
    use Step::*;

    match binop_step(arena, whole_region, arg_stack, op_stack, next_op) {
        Error(problem) => Err(problem),
        Push(loc_op) => run_binop_step(arena, whole_region, arg_stack, op_stack, loc_op),
        Skip => Ok(()),
    }
}

fn binop_step<'a>(
    arena: &'a Bump,
    whole_region: Region,
    arg_stack: &mut Vec<&'a Loc<Expr<'a>>>,
    op_stack: &mut Vec<Loc<BinOp>>,
    next_op: Loc<BinOp>,
) -> Step<'a> {
    use roc_module::called_via::Associativity::*;
    use std::cmp::Ordering;

    match op_stack.pop() {
        Some(stack_op) => {
            match next_op.value.cmp(&stack_op.value) {
                Ordering::Less => {
                    // Inline
                    let right = arg_stack.pop().unwrap();
                    let left = arg_stack.pop().unwrap();

                    arg_stack.push(arena.alloc(new_op_call_expr(arena, left, stack_op, right)));

                    Step::Push(next_op)
                }

                Ordering::Greater => {
                    // Swap
                    op_stack.push(stack_op);
                    op_stack.push(next_op);

                    Step::Skip
                }

                Ordering::Equal => {
                    match (
                        next_op.value.associativity(),
                        stack_op.value.associativity(),
                    ) {
                        (LeftAssociative, LeftAssociative) => {
                            // Inline
                            let right = arg_stack.pop().unwrap();
                            let left = arg_stack.pop().unwrap();

                            arg_stack
                                .push(arena.alloc(new_op_call_expr(arena, left, stack_op, right)));

                            Step::Push(next_op)
                        }

                        (RightAssociative, RightAssociative) => {
                            // Swap
                            op_stack.push(stack_op);
                            op_stack.push(next_op);

                            Step::Skip
                        }

                        (NonAssociative, NonAssociative) => {
                            // Both operators were non-associative, e.g. (True == False == False).
                            // We should tell the author to disambiguate by grouping them with parens.
                            let bad_op = next_op;
                            let right = arg_stack.pop().unwrap();
                            let left = arg_stack.pop().unwrap();
                            let broken_expr =
                                arena.alloc(new_op_call_expr(arena, left, stack_op, right));
                            let region = broken_expr.region;
                            let data = roc_parse::ast::PrecedenceConflict {
                                whole_region,
                                binop1_position: stack_op.region.start(),
                                binop1: stack_op.value,
                                binop2_position: bad_op.region.start(),
                                binop2: bad_op.value,
                                expr: arena.alloc(broken_expr),
                            };
                            let value = Expr::PrecedenceConflict(arena.alloc(data));

                            Step::Error(arena.alloc(Loc { region, value }))
                        }

                        _ => {
                            // The operators had the same precedence but different associativity.
                            //
                            // In many languages, this case can happen due to (for example) <| and |> having the same
                            // precedence but different associativity. Languages which support custom operators with
                            // (e.g. Haskell) can potentially have arbitrarily many of these cases.
                            //
                            // By design, Roc neither allows custom operators nor has any built-in operators with
                            // the same precedence and different associativity, so this should never happen!
                            internal_error!("BinOps had the same associativity, but different precedence. This should never happen!");
                        }
                    }
                }
            }
        }
        None => {
            op_stack.push(next_op);
            Step::Skip
        }
    }
}
