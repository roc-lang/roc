#![allow(clippy::manual_map)]

use crate::suffixed::{apply_task_await, unwrap_suffixed_expression, EUnwrapped};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_error_macros::internal_error;
use roc_module::called_via::BinOp::Pizza;
use roc_module::called_via::{BinOp, CalledVia};
use roc_module::ident::ModuleName;
use roc_parse::ast::Expr::{self, *};
use roc_parse::ast::{
    AssignedField, Collection, ModuleImportParams, OldRecordBuilderField, Pattern, StrLiteral,
    StrSegment, TypeAnnotation, ValueDef, WhenBranch,
};
use roc_region::all::{LineInfo, Loc, Region};

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
                value: Expr::Var { module_name, ident },
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
        ModuleImport(roc_parse::ast::ModuleImport {
            before_name,
            name,
            params,
            alias,
            exposed,
        }) => {
            let desugared_params =
                params.map(|ModuleImportParams { before, params }| ModuleImportParams {
                    before,
                    params: desugar_field_collection(arena, params, src, line_info, module_path),
                });

            ModuleImport(roc_parse::ast::ModuleImport {
                before_name,
                name: *name,
                params: desugared_params,
                alias: *alias,
                exposed: *exposed,
            })
        }
        IngestedFileImport(_) => *def,

        Stmt(stmt_expr) => {
            // desugar `stmt_expr!` to
            // _ : {}
            // _ = stmt_expr!

            let region = stmt_expr.region;
            let new_pat = arena.alloc(Loc::at(region, Pattern::Underscore("#!stmt")));

            ValueDef::AnnotatedBody {
                ann_pattern: new_pat,
                ann_type: arena.alloc(Loc::at(
                    region,
                    TypeAnnotation::Record {
                        fields: Collection::empty(),
                        ext: None,
                    },
                )),
                comment: None,
                body_pattern: new_pat,
                body_expr: desugar_expr(arena, stmt_expr, src, line_info, module_path),
            }
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
        *value_def = desugar_value_def(arena, arena.alloc(*value_def), src, line_info, module_path);
    }

    // `desugar_defs_node_values` is called recursively in `desugar_expr`
    // and we only want to unwrap suffixed nodes if they are a top level def.
    //
    // check here first so we only unwrap the expressions once, and after they have
    // been desugared
    if top_level_def {
        for value_def in defs.value_defs.iter_mut() {
            *value_def = desugar_value_def_suffixed(arena, *value_def);
        }
    }
}

/// For each top-level ValueDef in our module, we will unwrap any suffixed
/// expressions
///
/// e.g. `say! "hi"` desugars to `Task.await (say "hi") \{} -> ...`
pub fn desugar_value_def_suffixed<'a>(arena: &'a Bump, value_def: ValueDef<'a>) -> ValueDef<'a> {
    use ValueDef::*;

    match value_def {
        Body(loc_pattern, loc_expr) => {
            // note called_from_def is passed as `false` as this is a top_level_def
            match unwrap_suffixed_expression(arena, loc_expr, None) {
                Ok(new_expr) => Body(loc_pattern, new_expr),
                Err(EUnwrapped::UnwrappedSubExpr {
                    sub_arg,
                    sub_pat,
                    sub_new,
                }) => desugar_value_def_suffixed(
                    arena,
                    Body(
                        loc_pattern,
                        apply_task_await(arena, loc_expr.region, sub_arg, sub_pat, sub_new, None),
                    ),
                ),
                Err(..) => Body(
                    loc_pattern,
                    arena.alloc(Loc::at(loc_expr.region, MalformedSuffixed(loc_expr))),
                ),
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
            match unwrap_suffixed_expression(arena, body_expr, None) {
                Ok(new_expr) => AnnotatedBody {
                    ann_pattern,
                    ann_type,
                    comment,
                    body_pattern,
                    body_expr: new_expr,
                },
                Err(EUnwrapped::UnwrappedSubExpr {
                    sub_arg,
                    sub_pat,
                    sub_new,
                }) => desugar_value_def_suffixed(
                    arena,
                    AnnotatedBody {
                        ann_pattern,
                        ann_type,
                        comment,
                        body_pattern,
                        body_expr: apply_task_await(
                            arena,
                            body_expr.region,
                            sub_arg,
                            sub_pat,
                            sub_new,
                            Some((ann_pattern, ann_type)),
                        ),
                    },
                ),
                Err(..) => AnnotatedBody {
                    ann_pattern,
                    ann_type,
                    comment,
                    body_pattern,
                    body_expr: arena.alloc(Loc::at(body_expr.region, MalformedSuffixed(body_expr))),
                },
            }
        }

        // TODO support desugaring of Dbg, Expect, and ExpectFx
        Dbg { .. } | Expect { .. } | ExpectFx { .. } => value_def,
        ModuleImport { .. } | IngestedFileImport(_) => value_def,

        Stmt(..) => {
            internal_error!(
                "this should have been desugared into a Body(..) before this call in desugar_expr"
            )
        }
    }
}

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
        | MalformedSuffixed(..)
        | PrecedenceConflict { .. }
        | MultipleOldRecordBuilders(_)
        | UnappliedOldRecordBuilder(_)
        | EmptyRecordBuilder(_)
        | SingleFieldRecordBuilder(_)
        | OptionalFieldInRecordBuilder { .. }
        | Tag(_)
        | OpaqueRef(_)
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
        // desugar the sub_expression, but leave the TaskAwaitBang as this will
        // be unwrapped later in desugar_value_def_suffixed
        TaskAwaitBang(sub_expr) => {
            let intermediate = arena.alloc(Loc::at(loc_expr.region, **sub_expr));
            let new_sub_loc_expr = desugar_expr(arena, intermediate, src, line_info, module_path);
            let new_sub_expr = arena.alloc(new_sub_loc_expr.value);

            arena.alloc(Loc::at(loc_expr.region, TaskAwaitBang(new_sub_expr)))
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
            let fields = desugar_field_collection(arena, *fields, src, line_info, module_path);
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
        OldRecordBuilder(_) => arena.alloc(Loc {
            value: UnappliedOldRecordBuilder(loc_expr),
            region: loc_expr.region,
        }),
        RecordBuilder { mapper, fields } => {
            // NOTE the `mapper` is always a `Var { .. }`, we only desugar it to get rid of
            // any spaces before/after
            let new_mapper = desugar_expr(arena, mapper, src, line_info, module_path);

            if fields.is_empty() {
                return arena.alloc(Loc {
                    value: EmptyRecordBuilder(loc_expr),
                    region: loc_expr.region,
                });
            } else if fields.len() == 1 {
                return arena.alloc(Loc {
                    value: SingleFieldRecordBuilder(loc_expr),
                    region: loc_expr.region,
                });
            }

            let mut field_names = Vec::with_capacity_in(fields.len(), arena);
            let mut field_vals = Vec::with_capacity_in(fields.len(), arena);

            for field in fields.items {
                match desugar_field(arena, &field.value, src, line_info, module_path) {
                    AssignedField::RequiredValue(loc_name, _, loc_val) => {
                        field_names.push(loc_name);
                        field_vals.push(loc_val);
                    }
                    AssignedField::LabelOnly(loc_name) => {
                        field_names.push(loc_name);
                        field_vals.push(arena.alloc(Loc {
                            region: loc_name.region,
                            value: Expr::Var {
                                module_name: "",
                                ident: loc_name.value,
                            },
                        }));
                    }
                    AssignedField::OptionalValue(loc_name, _, loc_val) => {
                        return arena.alloc(Loc {
                            region: loc_expr.region,
                            value: OptionalFieldInRecordBuilder(arena.alloc(loc_name), loc_val),
                        });
                    }
                    AssignedField::SpaceBefore(_, _) | AssignedField::SpaceAfter(_, _) => {
                        unreachable!("Should have been desugared in `desugar_field`")
                    }
                    AssignedField::Malformed(_name) => {}
                }
            }

            let closure_arg_from_field = |field: Loc<&'a str>| Loc {
                region: field.region,
                value: Pattern::Identifier {
                    ident: arena.alloc_str(&format!("#{}", field.value)),
                },
            };

            let combiner_closure_in_region = |region| {
                let closure_body = Tuple(Collection::with_items(
                    Vec::from_iter_in(
                        [
                            &*arena.alloc(Loc::at(
                                region,
                                Expr::Var {
                                    module_name: "",
                                    ident: "#record_builder_closure_arg_a",
                                },
                            )),
                            &*arena.alloc(Loc::at(
                                region,
                                Expr::Var {
                                    module_name: "",
                                    ident: "#record_builder_closure_arg_b",
                                },
                            )),
                        ],
                        arena,
                    )
                    .into_bump_slice(),
                ));

                arena.alloc(Loc::at(
                    region,
                    Closure(
                        arena.alloc_slice_copy(&[
                            Loc::at(
                                region,
                                Pattern::Identifier {
                                    ident: "#record_builder_closure_arg_a",
                                },
                            ),
                            Loc::at(
                                region,
                                Pattern::Identifier {
                                    ident: "#record_builder_closure_arg_b",
                                },
                            ),
                        ]),
                        arena.alloc(Loc::at(region, closure_body)),
                    ),
                ))
            };

            let closure_args = {
                if field_names.len() == 2 {
                    arena.alloc_slice_copy(&[
                        closure_arg_from_field(field_names[0]),
                        closure_arg_from_field(field_names[1]),
                    ])
                } else {
                    let second_to_last_arg =
                        closure_arg_from_field(field_names[field_names.len() - 2]);
                    let last_arg = closure_arg_from_field(field_names[field_names.len() - 1]);

                    let mut second_arg = Pattern::Tuple(Collection::with_items(
                        arena.alloc_slice_copy(&[second_to_last_arg, last_arg]),
                    ));
                    let mut second_arg_region =
                        Region::span_across(&second_to_last_arg.region, &last_arg.region);

                    for index in (1..(field_names.len() - 2)).rev() {
                        second_arg =
                            Pattern::Tuple(Collection::with_items(arena.alloc_slice_copy(&[
                                closure_arg_from_field(field_names[index]),
                                Loc::at(second_arg_region, second_arg),
                            ])));
                        second_arg_region =
                            Region::span_across(&field_names[index].region, &second_arg_region);
                    }

                    arena.alloc_slice_copy(&[
                        closure_arg_from_field(field_names[0]),
                        Loc::at(second_arg_region, second_arg),
                    ])
                }
            };

            let record_val = Record(Collection::with_items(
                Vec::from_iter_in(
                    field_names.iter().map(|field_name| {
                        Loc::at(
                            field_name.region,
                            AssignedField::RequiredValue(
                                Loc::at(field_name.region, field_name.value),
                                &[],
                                arena.alloc(Loc::at(
                                    field_name.region,
                                    Expr::Var {
                                        module_name: "",
                                        ident: arena.alloc_str(&format!("#{}", field_name.value)),
                                    },
                                )),
                            ),
                        )
                    }),
                    arena,
                )
                .into_bump_slice(),
            ));

            let record_combiner_closure = arena.alloc(Loc {
                region: loc_expr.region,
                value: Closure(
                    closure_args,
                    arena.alloc(Loc::at(loc_expr.region, record_val)),
                ),
            });

            if field_names.len() == 2 {
                return arena.alloc(Loc {
                    region: loc_expr.region,
                    value: Apply(
                        new_mapper,
                        arena.alloc_slice_copy(&[
                            field_vals[0],
                            field_vals[1],
                            record_combiner_closure,
                        ]),
                        CalledVia::RecordBuilder,
                    ),
                });
            }

            let mut inner_combined = arena.alloc(Loc {
                region: Region::span_across(
                    &field_vals[field_names.len() - 2].region,
                    &field_vals[field_names.len() - 1].region,
                ),
                value: Apply(
                    new_mapper,
                    arena.alloc_slice_copy(&[
                        field_vals[field_names.len() - 2],
                        field_vals[field_names.len() - 1],
                        combiner_closure_in_region(loc_expr.region),
                    ]),
                    CalledVia::RecordBuilder,
                ),
            });

            for index in (1..(field_names.len() - 2)).rev() {
                inner_combined = arena.alloc(Loc {
                    region: Region::span_across(&field_vals[index].region, &inner_combined.region),
                    value: Apply(
                        new_mapper,
                        arena.alloc_slice_copy(&[
                            field_vals[index],
                            inner_combined,
                            combiner_closure_in_region(loc_expr.region),
                        ]),
                        CalledVia::RecordBuilder,
                    ),
                });
            }

            arena.alloc(Loc {
                region: loc_expr.region,
                value: Apply(
                    new_mapper,
                    arena.alloc_slice_copy(&[
                        field_vals[0],
                        inner_combined,
                        record_combiner_closure,
                    ]),
                    CalledVia::RecordBuilder,
                ),
            })
        }
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

            arena.alloc(Loc::at(loc_expr.region, Defs(arena.alloc(defs), loc_ret)))
        }
        Apply(loc_fn, loc_args, called_via) => {
            let mut desugared_args = Vec::with_capacity_in(loc_args.len(), arena);
            let mut builder_apply_exprs = None;

            for loc_arg in loc_args.iter() {
                let mut current = loc_arg.value;
                let arg = loop {
                    match current {
                        OldRecordBuilder(fields) => {
                            if builder_apply_exprs.is_some() {
                                return arena.alloc(Loc {
                                    value: MultipleOldRecordBuilders(loc_expr),
                                    region: loc_expr.region,
                                });
                            }

                            let builder_arg = old_record_builder_arg(arena, loc_arg.region, fields);
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
                            value: Apply(desugared_expr, args, CalledVia::OldRecordBuilder),
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
                },
                Not => Var {
                    module_name: ModuleName::BOOL,
                    ident: "not",
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

fn desugar_field_collection<'a>(
    arena: &'a Bump,
    fields: Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>,
    src: &'a str,
    line_info: &mut Option<LineInfo>,
    module_path: &str,
) -> Collection<'a, Loc<AssignedField<'a, Expr<'a>>>> {
    let mut allocated = Vec::with_capacity_in(fields.len(), arena);

    for field in fields.iter() {
        let value = desugar_field(arena, &field.value, src, line_info, module_path);

        allocated.push(Loc::at(field.region, value));
    }

    fields.replace_items(allocated.into_bump_slice())
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

struct OldRecordBuilderArg<'a> {
    closure: &'a Loc<Expr<'a>>,
    apply_exprs: Vec<'a, &'a Loc<Expr<'a>>>,
}

fn old_record_builder_arg<'a>(
    arena: &'a Bump,
    region: Region,
    fields: Collection<'a, Loc<OldRecordBuilderField<'a>>>,
) -> OldRecordBuilderArg<'a> {
    let mut record_fields = Vec::with_capacity_in(fields.len(), arena);
    let mut apply_exprs = Vec::with_capacity_in(fields.len(), arena);
    let mut apply_field_names = Vec::with_capacity_in(fields.len(), arena);

    // Build the record that the closure will return and gather apply expressions

    for field in fields.iter() {
        let mut current = field.value;

        let new_field = loop {
            match current {
                OldRecordBuilderField::Value(label, spaces, expr) => {
                    break AssignedField::RequiredValue(label, spaces, expr)
                }
                OldRecordBuilderField::ApplyValue(label, _, _, expr) => {
                    apply_field_names.push(label);
                    apply_exprs.push(expr);

                    let var = arena.alloc(Loc {
                        region: label.region,
                        value: Expr::Var {
                            module_name: "",
                            ident: arena.alloc("#".to_owned() + label.value),
                        },
                    });

                    break AssignedField::RequiredValue(label, &[], var);
                }
                OldRecordBuilderField::LabelOnly(label) => break AssignedField::LabelOnly(label),
                OldRecordBuilderField::SpaceBefore(sub_field, _) => {
                    current = *sub_field;
                }
                OldRecordBuilderField::SpaceAfter(sub_field, _) => {
                    current = *sub_field;
                }
                OldRecordBuilderField::Malformed(malformed) => {
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
        let ident = roc_parse::ast::Pattern::Identifier { ident: name };

        let arg_pattern = arena.alloc(Loc {
            value: ident,
            region: label.region,
        });

        body = arena.alloc(Loc {
            value: Closure(std::slice::from_ref(arg_pattern), body),
            region,
        });
    }

    OldRecordBuilderArg {
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
