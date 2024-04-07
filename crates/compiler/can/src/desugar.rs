#![allow(clippy::manual_map)]

use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_error_macros::internal_error;
use roc_module::called_via::BinOp::Pizza;
use roc_module::called_via::{BinOp, CalledVia};
use roc_module::ident::ModuleName;
use roc_parse::ast::Expr::{self, *};
use roc_parse::ast::{
    AssignedField, Collection, Pattern, RecordBuilderField, StrLiteral, StrSegment, ValueDef,
    WhenBranch,
};
use roc_region::all::{LineInfo, Loc, Region};
use std::cell::Cell;

thread_local! {
    static SUFFIXED_ANSWER_COUNTER: Cell<usize> = Cell::new(0);
}

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
        Body(loc_pattern, loc_expr) => Body(
            desugar_loc_pattern(arena, loc_pattern, src, line_info, module_path),
            desugar_expr(arena, loc_expr, src, line_info, module_path),
        ),
        ann @ Annotation(_, _) => *ann,
        AnnotatedBody {
            ann_pattern,
            ann_type,
            comment,
            body_pattern,
            body_expr,
        } => AnnotatedBody {
            ann_pattern,
            ann_type,
            comment: *comment,
            body_pattern: desugar_loc_pattern(arena, body_pattern, src, line_info, module_path),
            body_expr: desugar_expr(arena, body_expr, src, line_info, module_path),
        },

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
            // desugar into a Body({}, stmt_expr)
            let loc_pattern = arena.alloc(Loc::at(
                stmt_expr.region,
                Pattern::RecordDestructure(Collection::empty()),
            ));
            Body(
                loc_pattern,
                desugar_expr(arena, stmt_expr, src, line_info, module_path),
            )
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
        // we only want to unwrap suffixed nodes if this is a top level def
        // this function will be called recursively in desugar_expr and we only
        // unwrap expression after they have completed desugaring to simplify the analysis
        if top_level_def {
            // first desugar, then unwrap any suffixed defs
            *value_def = desugar_defs_node_values_suffixed(
                arena,
                desugar_value_def(arena, arena.alloc(*value_def), src, line_info, module_path),
                src,
                line_info,
                module_path,
            );
        } else {
            *value_def =
                desugar_value_def(arena, arena.alloc(*value_def), src, line_info, module_path);
        }
    }
}

pub fn desugar_defs_node_values_suffixed<'a>(
    arena: &'a Bump,
    value_def: ValueDef<'a>,
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
) -> ValueDef<'a> {
    use ValueDef::*;

    match value_def {
        Body(loc_pattern, loc_expr) => {
            // note called_from_def is passed as `false` as this is a top_level_def
            let result = unwrap_suffixed_expression(arena, loc_expr, false, module_path);

            match result {
                Ok(loc_expr) => Body(
                    desugar_loc_pattern(arena, loc_pattern, src, line_info, module_path),
                    desugar_expr(arena, loc_expr, src, line_info, module_path),
                ),
                Err(err) => {
                    internal_error!("this should have been desugared elsewhere... {:?}", err)
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
            let result = unwrap_suffixed_expression(arena, body_expr, false, module_path);

            match result {
                Ok(loc_expr) => AnnotatedBody {
                    ann_pattern,
                    ann_type,
                    comment,
                    body_pattern,
                    body_expr: loc_expr,
                },
                Err(err) => {
                    internal_error!("this should have been desugared elsewhere... {:?}", err)
                }
            }
        }

        // TODO support desugaring of Dbg, Expect, and ExpectFx
        Dbg { .. } | Expect { .. } | ExpectFx { .. } => value_def,

        Stmt(..) => {
            internal_error!("this should have been desugared in desugar_expr before this call")
        }
    }
}

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

        // note this only exists after desugaring
        LowLevelDbg(_, _, _) => loc_expr,
    }
}

#[derive(Debug)]
pub enum EUnwrapped<'a> {
    // the expression was unwrapped, consumes the def and following expression
    // e.g. `x = foo!` unwrapped `Task.await (foo) \x -> ...`
    UnwrappedExpr(&'a Loc<Expr<'a>>),

    // the expression had a (sub) expression unwrapped, doesn't affect the def or following expression
    // e.g. `x = bar! (foo!)` unwrapped `x = Task.await (foo) \#answer1 -> bar! #answer1`
    UnwrappedSubExpr {
        // this is the unwrapped suffixed which will be applied to the Task.await
        arg: &'a Loc<Expr<'a>>,

        // this pattern will be used in the closure
        pat: Loc<Pattern<'a>>,

        // this expression will replace the suffixed in the parent
        new: &'a Loc<Expr<'a>>,
    },
}

pub fn unwrap_suffixed_expression<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,

    // we need to know this to handle the error case where we cannot unwrap it
    called_from_def: bool,

    // TODO remove this, just here to help debugging temporarily
    module_path: &str,
) -> Result<&'a Loc<Expr<'a>>, EUnwrapped<'a>> {
    // dbg!("unwrap_suffixed_expression", &loc_expr);

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

        Expr::Defs(..) => {
            unwrap_suffixed_expression_defs_help(arena, loc_expr, called_from_def, module_path)
        }

        Expr::Apply(..) => {
            unwrap_suffixed_expression_apply_help(arena, loc_expr, called_from_def, module_path)
        }

        Expr::SpaceBefore(..) | Expr::SpaceAfter(..) => {
            internal_error!("unexpected SpaceBefore or SpaceAfter in unwrap_suffixed_expression, should have been removed in desugar_expr before this");
        }

        Expr::When(..) => {
            // if module_path.contains("test.roc") {
            //     dbg!("TODO unwrap_suffixed_expression When");
            // }
            Ok(loc_expr)
        }

        Expr::If(..) => {
            // if module_path.contains("test.roc") {
            //     dbg!("TODO unwrap_suffixed_expression If");
            // }
            Ok(loc_expr)
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
            match unwrap_suffixed_expression(arena, return_expr, false, module_path) {
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
                module_path,
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

    // we need to know this to handle the error case where we cannot unwrap it
    called_from_def: bool,

    // TODO remove this, just here to help debugging temporarily
    module_path: &str,
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
                match unwrap_suffixed_expression(arena, arg, false,module_path) {
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
            match unwrap_suffixed_expression(arena, function, false, module_path) {
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

pub fn unwrap_suffixed_expression_defs_help<'a>(
    arena: &'a Bump,
    loc_expr: &'a Loc<Expr<'a>>,
    _called_from_def: bool, // TODO do we need this? pretty sure we do
    module_path: &str,
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
                        match unwrap_suffixed_expression(arena, def_expr, true, module_path) {
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
                                        module_path,
                                    );
                                } else if before_empty {
                                    // NIL before, SOME after -> FIRST
                                    return unwrap_suffixed_expression(
                                        arena,
                                        apply_task_await(arena, loc_expr.region, new, **def_pattern, arena.alloc(Loc::at(def_expr.region, Defs(arena.alloc(split_defs.after), loc_ret)))),
                                        false,
                                        module_path,
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
                                    return unwrap_suffixed_expression(arena, arena.alloc(Loc::at(loc_expr.region, Defs(arena.alloc(split_defs.before), new_loc_ret))), false, module_path);
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
                                    module_path,
                                );
                            }
                        }
                    }
                }
            }

            // try to unwrap the loc_ret
            match unwrap_suffixed_expression(arena, loc_ret, false, module_path) {
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

#[cfg(test)]
mod unwrap_suffixed_expression_tests {

    use crate::desugar::desugar_defs_node_values;
    use crate::desugar::Bump;
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

        dbg!(&defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
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
