#![allow(clippy::manual_map)]

use crate::env::{Env, FxMode};
use crate::scope::Scope;
use crate::suffixed::{apply_try_function, unwrap_suffixed_expression, EUnwrapped};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_error_macros::internal_error;
use roc_module::called_via::BinOp::Pizza;
use roc_module::called_via::{BinOp, CalledVia};
use roc_module::ident::ModuleName;
use roc_parse::ast::Expr::{self, *};
use roc_parse::ast::{
    is_expr_suffixed, AssignedField, Collection, Defs, ModuleImportParams, Pattern, StrLiteral,
    StrSegment, TypeAnnotation, ValueDef, WhenBranch,
};
use roc_problem::can::Problem;
use roc_region::all::{Loc, Region};

// BinOp precedence logic adapted from Gluon by Markus Westerlind
// https://github.com/gluon-lang/gluon - license information can be found in
// the LEGAL_DETAILS file in the root directory of this distribution.
//
// Thank you, Markus!

/// Desugar a single binary operation.
///
/// When using this function, don't desugar `left` and `right` before calling so that
/// we can properly desugar `|> try` expressions!
fn new_op_call_expr<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    left: &'a Loc<Expr<'a>>,
    loc_op: Loc<BinOp>,
    right: &'a Loc<Expr<'a>>,
) -> Loc<Expr<'a>> {
    let region = Region::span_across(&left.region, &right.region);

    let value = match loc_op.value {
        // Rewrite the Pizza operator into an Apply
        Pizza => {
            // Allow `left |> try (optional)` to desugar to `try left (optional)`
            let right_without_spaces = without_spaces(&right.value);
            match right_without_spaces {
                Try => {
                    let desugared_left = desugar_expr(env, scope, left);
                    return desugar_try_expr(env, scope, desugared_left);
                }
                Apply(&Loc { value: Try, .. }, arguments, _called_via) => {
                    let try_fn = desugar_expr(env, scope, arguments.first().unwrap());

                    let mut args = Vec::with_capacity_in(arguments.len(), env.arena);
                    args.push(desugar_expr(env, scope, left));
                    args.extend(
                        arguments
                            .iter()
                            .skip(1)
                            .map(|a| desugar_expr(env, scope, a)),
                    );

                    return desugar_try_expr(
                        env,
                        scope,
                        env.arena.alloc(Loc::at(
                            right.region,
                            Expr::Apply(try_fn, args.into_bump_slice(), CalledVia::Try),
                        )),
                    );
                }
                _ => {}
            }

            let left = desugar_expr(env, scope, left);
            let right = desugar_expr(env, scope, right);

            match right.value {
                Apply(function, arguments, _called_via) => {
                    let mut args = Vec::with_capacity_in(1 + arguments.len(), env.arena);

                    args.push(left);
                    args.extend(arguments.iter());

                    let args = args.into_bump_slice();

                    Apply(function, args, CalledVia::BinOp(Pizza))
                }
                Dbg => *desugar_dbg_expr(env, scope, left, region),
                _ => {
                    // e.g. `1 |> (if b then (\a -> a) else (\c -> c))`
                    Apply(right, env.arena.alloc([left]), CalledVia::BinOp(Pizza))
                }
            }
        }
        binop => {
            let left = desugar_expr(env, scope, left);
            let right = desugar_expr(env, scope, right);

            // This is a normal binary operator like (+), so desugar it
            // into the appropriate function call.
            let (module_name, ident) = binop_to_function(binop);

            let args = env.arena.alloc([left, right]);

            let loc_expr = env.arena.alloc(Loc {
                value: Expr::Var { module_name, ident },
                region: loc_op.region,
            });

            Apply(loc_expr, args, CalledVia::BinOp(binop))
        }
    };

    Loc { region, value }
}

fn without_spaces<'a>(expr: &'a Expr<'a>) -> &'a Expr<'a> {
    match expr {
        Expr::SpaceBefore(inner, _) | Expr::SpaceAfter(inner, _) => without_spaces(inner),
        _ => expr,
    }
}

fn desugar_value_def<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    def: &'a ValueDef<'a>,
) -> ValueDef<'a> {
    use ValueDef::*;

    match def {
        Body(loc_pattern, loc_expr) => Body(
            desugar_loc_pattern(env, scope, loc_pattern),
            desugar_expr(env, scope, loc_expr),
        ),
        ann @ Annotation(_, _) => *ann,
        AnnotatedBody {
            ann_pattern,
            ann_type,
            lines_between,
            body_pattern,
            body_expr,
        } => AnnotatedBody {
            ann_pattern,
            ann_type,
            lines_between,
            body_pattern: desugar_loc_pattern(env, scope, body_pattern),
            body_expr: desugar_expr(env, scope, body_expr),
        },

        Dbg {
            condition,
            preceding_comment,
        } => {
            let desugared_condition = &*env.arena.alloc(desugar_expr(env, scope, condition));
            Dbg {
                condition: desugared_condition,
                preceding_comment: *preceding_comment,
            }
        }
        Expect {
            condition,
            preceding_comment,
        } => {
            let desugared_condition = &*env.arena.alloc(desugar_expr(env, scope, condition));
            Expect {
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
                    params: params.map(|params| desugar_field_collection(env, scope, *params)),
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

        StmtAfterExpr => internal_error!(
            "StmtAfterExpression is only created during desugaring, so it shouldn't exist here."
        ),

        Stmt(stmt_expr) => {
            if env.fx_mode == FxMode::PurityInference {
                // In purity inference mode, statements aren't fully desugared here
                // so we can provide better errors
                return Stmt(desugar_expr(env, scope, stmt_expr));
            }

            // desugar `stmt_expr!` to
            // _ : {}
            // _ = stmt_expr!

            let desugared_expr = desugar_expr(env, scope, stmt_expr);

            if !is_expr_suffixed(&desugared_expr.value) {
                env.problems.push(Problem::StmtAfterExpr(stmt_expr.region));

                return ValueDef::StmtAfterExpr;
            }

            let region = stmt_expr.region;
            let new_pat = env
                .arena
                .alloc(Loc::at(region, Pattern::Underscore("#!stmt")));

            ValueDef::AnnotatedBody {
                ann_pattern: new_pat,
                ann_type: env.arena.alloc(Loc::at(
                    region,
                    TypeAnnotation::Record {
                        fields: Collection::empty(),
                        ext: None,
                    },
                )),
                lines_between: &[],
                body_pattern: new_pat,
                body_expr: desugared_expr,
            }
        }
    }
}

pub fn desugar_defs_node_values<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    defs: &mut roc_parse::ast::Defs<'a>,
    top_level_def: bool,
) {
    for value_def in defs.value_defs.iter_mut() {
        *value_def = desugar_value_def(env, scope, env.arena.alloc(*value_def));
    }

    // `desugar_defs_node_values` is called recursively in `desugar_expr`
    // and we only want to unwrap suffixed nodes if they are a top level def.
    //
    // check here first so we only unwrap the expressions once, and after they have
    // been desugared
    if top_level_def {
        for value_def in defs.value_defs.iter_mut() {
            *value_def = desugar_value_def_suffixed(env.arena, *value_def);
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
                    target,
                }) => desugar_value_def_suffixed(
                    arena,
                    Body(
                        loc_pattern,
                        apply_try_function(
                            arena,
                            loc_expr.region,
                            sub_arg,
                            sub_pat,
                            sub_new,
                            None,
                            target,
                        ),
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
            lines_between,
            body_pattern,
            body_expr,
        } => {
            match unwrap_suffixed_expression(arena, body_expr, Some(ann_pattern)) {
                Ok(new_expr) => AnnotatedBody {
                    ann_pattern,
                    ann_type,
                    lines_between,
                    body_pattern,
                    body_expr: new_expr,
                },
                Err(EUnwrapped::UnwrappedSubExpr {
                    sub_arg,
                    sub_pat,
                    sub_new,
                    target,
                }) => desugar_value_def_suffixed(
                    arena,
                    AnnotatedBody {
                        ann_pattern,
                        ann_type,
                        lines_between,
                        body_pattern,
                        body_expr: apply_try_function(
                            arena,
                            body_expr.region,
                            sub_arg,
                            sub_pat,
                            sub_new,
                            Some((ann_pattern, ann_type)),
                            target,
                        ),
                    },
                ),
                // When the last expression is suffixed, it will try to unwrap the def, but because we
                // have an annotated def we can simply ignore the try and return it as is without
                // creating an intermediate identifier
                Err(EUnwrapped::UnwrappedDefExpr { loc_expr, .. }) => desugar_value_def_suffixed(
                    arena,
                    AnnotatedBody {
                        ann_pattern,
                        ann_type,
                        lines_between,
                        body_pattern,
                        body_expr: loc_expr,
                    },
                ),
                Err(..) => AnnotatedBody {
                    ann_pattern,
                    ann_type,
                    lines_between,
                    body_pattern,
                    body_expr: arena.alloc(Loc::at(body_expr.region, MalformedSuffixed(body_expr))),
                },
            }
        }

        Expect {
            condition,
            preceding_comment,
        } => match unwrap_suffixed_expression(arena, condition, None) {
            Ok(new_condition) => ValueDef::Expect {
                condition: new_condition,
                preceding_comment,
            },
            Err(..) => {
                internal_error!("Unable to desugar the suffix inside an Expect value def");
            }
        },

        // TODO support desugaring of Dbg
        Dbg { .. } => value_def,
        ModuleImport { .. } | IngestedFileImport(_) | StmtAfterExpr => value_def,

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
    env: &mut Env<'a>,
    scope: &mut Scope,
    loc_expr: &'a Loc<Expr<'a>>,
) -> &'a Loc<Expr<'a>> {
    match &loc_expr.value {
        Float(..)
        | Num(..)
        | NonBase10Int { .. }
        | SingleQuote(_)
        | AccessorFunction(_)
        | Underscore { .. }
        | MalformedIdent(_, _)
        | MalformedSuffixed(..)
        | PrecedenceConflict { .. }
        | EmptyRecordBuilder(_)
        | SingleFieldRecordBuilder(_)
        | OptionalFieldInRecordBuilder { .. }
        | Tag(_)
        | OpaqueRef(_)
        | Crash
        | Try => loc_expr,

        Var { module_name, ident } => {
            if env.fx_mode == FxMode::Task && ident.ends_with('!') {
                env.arena.alloc(Loc::at(
                    Region::new(loc_expr.region.start(), loc_expr.region.end().sub(1)),
                    TrySuffix {
                        expr: env.arena.alloc(Var {
                            module_name,
                            ident: ident.trim_end_matches('!'),
                        }),
                        target: roc_parse::ast::TryTarget::Task,
                    },
                ))
            } else {
                loc_expr
            }
        }

        Str(str_literal) => match str_literal {
            StrLiteral::PlainLine(_) => loc_expr,
            StrLiteral::Line(segments) => {
                let region = loc_expr.region;
                let value = Str(StrLiteral::Line(desugar_str_segments(env, scope, segments)));

                env.arena.alloc(Loc { region, value })
            }
            StrLiteral::Block(lines) => {
                let region = loc_expr.region;
                let mut new_lines = Vec::with_capacity_in(lines.len(), env.arena);
                for segments in lines.iter() {
                    new_lines.push(desugar_str_segments(env, scope, segments));
                }
                let value = Str(StrLiteral::Block(new_lines.into_bump_slice()));

                env.arena.alloc(Loc { region, value })
            }
        },

        TupleAccess(sub_expr, paths) => {
            let region = loc_expr.region;
            let loc_sub_expr = Loc {
                region,
                value: **sub_expr,
            };
            let value = TupleAccess(
                &desugar_expr(env, scope, env.arena.alloc(loc_sub_expr)).value,
                paths,
            );

            env.arena.alloc(Loc { region, value })
        }
        // desugar the sub_expression, but leave the TrySuffix as this will
        // be unwrapped later in desugar_value_def_suffixed
        TrySuffix {
            expr: sub_expr,
            target,
        } => {
            let intermediate = env.arena.alloc(Loc::at(loc_expr.region, **sub_expr));
            let new_sub_loc_expr = desugar_expr(env, scope, intermediate);
            let new_sub_expr = env.arena.alloc(new_sub_loc_expr.value);

            env.arena.alloc(Loc::at(
                loc_expr.region,
                TrySuffix {
                    expr: new_sub_expr,
                    target: *target,
                },
            ))
        }
        RecordAccess(sub_expr, paths) => {
            let region = loc_expr.region;
            let loc_sub_expr = Loc {
                region,
                value: **sub_expr,
            };
            let value = RecordAccess(
                &desugar_expr(env, scope, env.arena.alloc(loc_sub_expr)).value,
                paths,
            );

            env.arena.alloc(Loc { region, value })
        }
        List(items) => {
            let mut new_items = Vec::with_capacity_in(items.len(), env.arena);

            for item in items.iter() {
                new_items.push(desugar_expr(env, scope, item));
            }
            let new_items = new_items.into_bump_slice();
            let value: Expr<'a> = List(items.replace_items(new_items));

            env.arena.alloc(Loc {
                region: loc_expr.region,
                value,
            })
        }
        Record(fields) => {
            let fields = desugar_field_collection(env, scope, *fields);
            env.arena.alloc(Loc {
                region: loc_expr.region,
                value: Record(fields),
            })
        }
        Tuple(fields) => {
            let mut allocated = Vec::with_capacity_in(fields.len(), env.arena);
            for field in fields.iter() {
                let expr = desugar_expr(env, scope, field);
                allocated.push(expr);
            }
            let fields = fields.replace_items(allocated.into_bump_slice());
            env.arena.alloc(Loc {
                region: loc_expr.region,
                value: Tuple(fields),
            })
        }
        RecordUpdate { fields, update } => {
            // NOTE the `update` field is always a `Var { .. }`, we only desugar it to get rid of
            // any spaces before/after
            let new_update = desugar_expr(env, scope, update);

            let mut allocated = Vec::with_capacity_in(fields.len(), env.arena);
            for field in fields.iter() {
                let value = desugar_field(env, scope, &field.value);
                allocated.push(Loc {
                    value,
                    region: field.region,
                });
            }
            let new_fields = fields.replace_items(allocated.into_bump_slice());

            env.arena.alloc(Loc {
                region: loc_expr.region,
                value: RecordUpdate {
                    update: new_update,
                    fields: new_fields,
                },
            })
        }
        RecordUpdater(field_name) => {
            let region = loc_expr.region;

            let closure_body = RecordUpdate {
                update: env.arena.alloc(Loc {
                    region,
                    value: Expr::Var {
                        module_name: "",
                        ident: "#record_updater_record",
                    },
                }),
                fields: Collection::with_items(
                    Vec::from_iter_in(
                        [Loc::at(
                            region,
                            AssignedField::RequiredValue(
                                Loc::at(region, field_name),
                                &[],
                                &*env.arena.alloc(Loc {
                                    region,
                                    value: Expr::Var {
                                        module_name: "",
                                        ident: "#record_updater_field",
                                    },
                                }),
                            ),
                        )],
                        env.arena,
                    )
                    .into_bump_slice(),
                ),
            };

            env.arena.alloc(Loc {
                region,
                value: Closure(
                    env.arena.alloc_slice_copy(&[
                        Loc::at(
                            region,
                            Pattern::Identifier {
                                ident: "#record_updater_record",
                            },
                        ),
                        Loc::at(
                            region,
                            Pattern::Identifier {
                                ident: "#record_updater_field",
                            },
                        ),
                    ]),
                    env.arena.alloc(Loc::at(region, closure_body)),
                ),
            })
        }
        Closure(loc_patterns, loc_ret) => env.arena.alloc(Loc {
            region: loc_expr.region,
            value: Closure(
                desugar_loc_patterns(env, scope, loc_patterns),
                desugar_expr(env, scope, loc_ret),
            ),
        }),
        Backpassing(loc_patterns, loc_body, loc_ret) => {
            // loc_patterns <- loc_body
            //
            // loc_ret

            let problem_region = Region::span_across(
                &Region::across_all(loc_patterns.iter().map(|loc_pattern| &loc_pattern.region)),
                &loc_body.region,
            );
            env.problem(Problem::DeprecatedBackpassing(problem_region));

            // first desugar the body, because it may contain |>
            let desugared_body = desugar_expr(env, scope, loc_body);

            let desugared_ret = desugar_expr(env, scope, loc_ret);
            let desugared_loc_patterns = desugar_loc_patterns(env, scope, loc_patterns);
            let closure = Expr::Closure(desugared_loc_patterns, desugared_ret);
            let loc_closure = Loc::at(loc_expr.region, closure);

            match &desugared_body.value {
                Expr::Apply(function, arguments, called_via) => {
                    let mut new_arguments: Vec<'a, &'a Loc<Expr<'a>>> =
                        Vec::with_capacity_in(arguments.len() + 1, env.arena);
                    new_arguments.extend(arguments.iter());
                    new_arguments.push(env.arena.alloc(loc_closure));

                    let call = Expr::Apply(function, new_arguments.into_bump_slice(), *called_via);
                    let loc_call = Loc::at(loc_expr.region, call);

                    env.arena.alloc(loc_call)
                }
                _ => {
                    // e.g. `x <- (if b then (\a -> a) else (\c -> c))`
                    let call = Expr::Apply(
                        desugared_body,
                        env.arena.alloc([&*env.arena.alloc(loc_closure)]),
                        CalledVia::Space,
                    );
                    let loc_call = Loc::at(loc_expr.region, call);

                    env.arena.alloc(loc_call)
                }
            }
        }
        RecordBuilder { mapper, fields } => {
            // NOTE the `mapper` is always a `Var { .. }`, we only desugar it to get rid of
            // any spaces before/after
            let new_mapper = desugar_expr(env, scope, mapper);

            if fields.is_empty() {
                return env.arena.alloc(Loc {
                    value: EmptyRecordBuilder(loc_expr),
                    region: loc_expr.region,
                });
            } else if fields.len() == 1 {
                return env.arena.alloc(Loc {
                    value: SingleFieldRecordBuilder(loc_expr),
                    region: loc_expr.region,
                });
            }

            struct FieldData<'d> {
                name: Loc<&'d str>,
                value: &'d Loc<Expr<'d>>,
                ignored: bool,
            }

            let mut field_data = Vec::with_capacity_in(fields.len(), env.arena);

            for field in fields.items {
                let desugared_field = desugar_field(env, scope, &field.value);
                let (name, value, ignored) = match desugared_field {
                    AssignedField::RequiredValue(loc_name, _, loc_val) => {
                        (loc_name, loc_val, false)
                    }
                    AssignedField::IgnoredValue(loc_name, _, loc_val) => (loc_name, loc_val, true),
                    AssignedField::LabelOnly(loc_name) => (
                        loc_name,
                        &*env.arena.alloc(Loc {
                            region: loc_name.region,
                            value: Expr::Var {
                                module_name: "",
                                ident: loc_name.value,
                            },
                        }),
                        false,
                    ),
                    AssignedField::OptionalValue(loc_name, _, loc_val) => {
                        return env.arena.alloc(Loc {
                            region: loc_expr.region,
                            value: OptionalFieldInRecordBuilder(env.arena.alloc(loc_name), loc_val),
                        });
                    }
                    AssignedField::SpaceBefore(_, _) | AssignedField::SpaceAfter(_, _) => {
                        unreachable!("Should have been desugared in `desugar_field`")
                    }
                };

                field_data.push(FieldData {
                    name,
                    value,
                    ignored,
                });
            }

            let closure_arg_from_field =
                |FieldData {
                     name,
                     value: _,
                     ignored,
                 }: &FieldData<'a>| Loc {
                    region: name.region,
                    value: if *ignored {
                        Pattern::Underscore(name.value)
                    } else {
                        Pattern::Identifier {
                            ident: env.arena.alloc_str(&format!("#{}", name.value)),
                        }
                    },
                };

            let combiner_closure_in_region = |region| {
                let closure_body = Tuple(Collection::with_items(
                    Vec::from_iter_in(
                        [
                            &*env.arena.alloc(Loc::at(
                                region,
                                Expr::Var {
                                    module_name: "",
                                    ident: "#record_builder_closure_arg_a",
                                },
                            )),
                            &*env.arena.alloc(Loc::at(
                                region,
                                Expr::Var {
                                    module_name: "",
                                    ident: "#record_builder_closure_arg_b",
                                },
                            )),
                        ],
                        env.arena,
                    )
                    .into_bump_slice(),
                ));

                env.arena.alloc(Loc::at(
                    region,
                    Closure(
                        env.arena.alloc_slice_copy(&[
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
                        env.arena.alloc(Loc::at(region, closure_body)),
                    ),
                ))
            };

            let closure_args = {
                if field_data.len() == 2 {
                    env.arena.alloc_slice_copy(&[
                        closure_arg_from_field(&field_data[0]),
                        closure_arg_from_field(&field_data[1]),
                    ])
                } else {
                    let second_to_last_arg =
                        closure_arg_from_field(&field_data[field_data.len() - 2]);
                    let last_arg = closure_arg_from_field(&field_data[field_data.len() - 1]);

                    let mut second_arg = Pattern::Tuple(Collection::with_items(
                        env.arena.alloc_slice_copy(&[second_to_last_arg, last_arg]),
                    ));
                    let mut second_arg_region =
                        Region::span_across(&second_to_last_arg.region, &last_arg.region);

                    for index in (1..(field_data.len() - 2)).rev() {
                        second_arg =
                            Pattern::Tuple(Collection::with_items(env.arena.alloc_slice_copy(&[
                                closure_arg_from_field(&field_data[index]),
                                Loc::at(second_arg_region, second_arg),
                            ])));
                        second_arg_region =
                            Region::span_across(&field_data[index].name.region, &second_arg_region);
                    }

                    env.arena.alloc_slice_copy(&[
                        closure_arg_from_field(&field_data[0]),
                        Loc::at(second_arg_region, second_arg),
                    ])
                }
            };

            let record_val = Record(Collection::with_items(
                Vec::from_iter_in(
                    field_data
                        .iter()
                        .filter(|field| !field.ignored)
                        .map(|field| {
                            Loc::at(
                                field.name.region,
                                AssignedField::RequiredValue(
                                    field.name,
                                    &[],
                                    env.arena.alloc(Loc::at(
                                        field.name.region,
                                        Expr::Var {
                                            module_name: "",
                                            ident: env
                                                .arena
                                                .alloc_str(&format!("#{}", field.name.value)),
                                        },
                                    )),
                                ),
                            )
                        }),
                    env.arena,
                )
                .into_bump_slice(),
            ));

            let record_combiner_closure = env.arena.alloc(Loc {
                region: loc_expr.region,
                value: Closure(
                    closure_args,
                    env.arena.alloc(Loc::at(loc_expr.region, record_val)),
                ),
            });

            if field_data.len() == 2 {
                return env.arena.alloc(Loc {
                    region: loc_expr.region,
                    value: Apply(
                        new_mapper,
                        env.arena.alloc_slice_copy(&[
                            field_data[0].value,
                            field_data[1].value,
                            record_combiner_closure,
                        ]),
                        CalledVia::RecordBuilder,
                    ),
                });
            }

            let mut inner_combined = env.arena.alloc(Loc {
                region: Region::span_across(
                    &field_data[field_data.len() - 2].value.region,
                    &field_data[field_data.len() - 1].value.region,
                ),
                value: Apply(
                    new_mapper,
                    env.arena.alloc_slice_copy(&[
                        field_data[field_data.len() - 2].value,
                        field_data[field_data.len() - 1].value,
                        combiner_closure_in_region(loc_expr.region),
                    ]),
                    CalledVia::RecordBuilder,
                ),
            });

            for index in (1..(field_data.len() - 2)).rev() {
                inner_combined = env.arena.alloc(Loc {
                    region: Region::span_across(
                        &field_data[index].value.region,
                        &inner_combined.region,
                    ),
                    value: Apply(
                        new_mapper,
                        env.arena.alloc_slice_copy(&[
                            field_data[index].value,
                            inner_combined,
                            combiner_closure_in_region(loc_expr.region),
                        ]),
                        CalledVia::RecordBuilder,
                    ),
                });
            }

            env.arena.alloc(Loc {
                region: loc_expr.region,
                value: Apply(
                    new_mapper,
                    env.arena.alloc_slice_copy(&[
                        field_data[0].value,
                        inner_combined,
                        record_combiner_closure,
                    ]),
                    CalledVia::RecordBuilder,
                ),
            })
        }
        BinOps(lefts, right) => desugar_bin_ops(env, scope, loc_expr.region, lefts, right),
        Defs(defs, loc_ret) => {
            let mut defs = (*defs).clone();
            desugar_defs_node_values(env, scope, &mut defs, false);
            let loc_ret = desugar_expr(env, scope, loc_ret);

            env.arena.alloc(Loc::at(
                loc_expr.region,
                Defs(env.arena.alloc(defs), loc_ret),
            ))
        }
        Apply(Loc { value: Dbg, .. }, loc_args, _called_via) => {
            debug_assert!(!loc_args.is_empty());

            if loc_args.len() > 1 {
                let args_region = Region::span_across(
                    &loc_args.first().unwrap().region,
                    &loc_args.last().unwrap().region,
                );
                env.problem(Problem::OverAppliedDbg {
                    region: args_region,
                });

                env.arena.alloc(Loc {
                    value: *desugar_invalid_dbg_expr(env, scope, loc_expr.region),
                    region: loc_expr.region,
                })
            } else {
                let desugared_arg = desugar_expr(env, scope, loc_args.first().unwrap());

                env.arena.alloc(Loc {
                    value: *desugar_dbg_expr(env, scope, desugared_arg, loc_expr.region),
                    region: loc_expr.region,
                })
            }
        }
        Apply(
            Loc {
                value: Try,
                region: _,
            },
            loc_args,
            _called_via,
        ) => {
            let result_expr = if loc_args.len() == 1 {
                desugar_expr(env, scope, loc_args[0])
            } else {
                let function = desugar_expr(env, scope, loc_args.first().unwrap());
                let mut desugared_args = Vec::with_capacity_in(loc_args.len() - 1, env.arena);
                for loc_arg in &loc_args[1..] {
                    desugared_args.push(desugar_expr(env, scope, loc_arg));
                }

                env.arena.alloc(Loc::at(
                    loc_expr.region,
                    Expr::Apply(function, desugared_args.into_bump_slice(), CalledVia::Try),
                ))
            };

            env.arena.alloc(desugar_try_expr(env, scope, result_expr))
        }
        Apply(loc_fn, loc_args, called_via) => {
            let mut desugared_args = Vec::with_capacity_in(loc_args.len(), env.arena);

            for loc_arg in loc_args.iter() {
                let mut current = loc_arg.value;
                let arg = loop {
                    match current {
                        SpaceBefore(expr, _) | SpaceAfter(expr, _) => {
                            current = *expr;
                        }
                        _ => break loc_arg,
                    }
                };

                desugared_args.push(desugar_expr(env, scope, arg));
            }

            let desugared_args = desugared_args.into_bump_slice();

            env.arena.alloc(Loc {
                value: Apply(
                    desugar_expr(env, scope, loc_fn),
                    desugared_args,
                    *called_via,
                ),
                region: loc_expr.region,
            })
        }
        When(loc_cond_expr, branches) => {
            let loc_desugared_cond = &*env.arena.alloc(desugar_expr(env, scope, loc_cond_expr));
            let mut desugared_branches = Vec::with_capacity_in(branches.len(), env.arena);

            for branch in branches.iter() {
                let desugared_expr = desugar_expr(env, scope, &branch.value);
                let desugared_patterns = desugar_loc_patterns(env, scope, branch.patterns);

                let desugared_guard = if let Some(guard) = &branch.guard {
                    Some(*desugar_expr(env, scope, guard))
                } else {
                    None
                };

                desugared_branches.push(&*env.arena.alloc(WhenBranch {
                    patterns: desugared_patterns,
                    value: *desugared_expr,
                    guard: desugared_guard,
                }));
            }

            let desugared_branches = desugared_branches.into_bump_slice();

            env.arena.alloc(Loc {
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
            let loc_fn_var = env.arena.alloc(Loc { region, value });
            let desugared_args = env.arena.alloc([desugar_expr(env, scope, loc_arg)]);

            env.arena.alloc(Loc {
                value: Apply(loc_fn_var, desugared_args, CalledVia::UnaryOp(op)),
                region: loc_expr.region,
            })
        }
        SpaceBefore(expr, _) | SpaceAfter(expr, _) => {
            // Since we've already begun canonicalization, spaces and parens
            // are no longer needed and should be dropped.
            desugar_expr(
                env,
                scope,
                env.arena.alloc(Loc {
                    value: **expr,
                    region: loc_expr.region,
                }),
            )
        }
        ParensAround(expr) => {
            let desugared = desugar_expr(
                env,
                scope,
                env.arena.alloc(Loc {
                    value: **expr,
                    region: loc_expr.region,
                }),
            );

            env.arena.alloc(Loc {
                value: ParensAround(&desugared.value),
                region: loc_expr.region,
            })
        }
        If {
            if_thens,
            final_else,
            indented_else,
        } => {
            // If does not get desugared into `when` so we can give more targeted error messages during type checking.
            let desugared_final_else = &*env.arena.alloc(desugar_expr(env, scope, final_else));

            let mut desugared_if_thens = Vec::with_capacity_in(if_thens.len(), env.arena);

            for (condition, then_branch) in if_thens.iter() {
                let desugared_condition = *desugar_expr(env, scope, condition);
                let desugared_then_branch = *desugar_expr(env, scope, then_branch);

                desugared_if_thens.push((desugared_condition, desugared_then_branch));
            }

            env.arena.alloc(Loc {
                value: If {
                    if_thens: desugared_if_thens.into_bump_slice(),
                    final_else: desugared_final_else,
                    indented_else: *indented_else,
                },
                region: loc_expr.region,
            })
        }
        Dbg => {
            // Allow naked dbg, necessary for piping values into dbg with the `Pizza` binop
            loc_expr
        }
        DbgStmt(condition, continuation) => {
            let desugared_condition = &*env.arena.alloc(desugar_expr(env, scope, condition));
            let desugared_continuation = &*env.arena.alloc(desugar_expr(env, scope, continuation));

            env.arena.alloc(Loc {
                value: *desugar_dbg_stmt(env, desugared_condition, desugared_continuation),
                region: loc_expr.region,
            })
        }
        Return(return_value, after_return) => {
            let desugared_return_value = &*env.arena.alloc(desugar_expr(env, scope, return_value));

            env.arena.alloc(Loc {
                // Do not desugar after_return since it isn't run anyway
                value: Return(desugared_return_value, *after_return),
                region: loc_expr.region,
            })
        }

        // note this only exists after desugaring
        LowLevelDbg(_, _, _) => loc_expr,
    }
}

pub fn desugar_try_expr<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    result_expr: &'a Loc<Expr<'a>>,
) -> Loc<Expr<'a>> {
    let region = result_expr.region;
    let ok_symbol = env.arena.alloc(scope.gen_unique_symbol_name().to_string());
    let err_symbol = env.arena.alloc(scope.gen_unique_symbol_name().to_string());

    let ok_branch = env.arena.alloc(WhenBranch {
        patterns: env.arena.alloc([Loc::at(
            region,
            Pattern::Apply(
                env.arena.alloc(Loc::at(region, Pattern::Tag("Ok"))),
                env.arena
                    .alloc([Loc::at(region, Pattern::Identifier { ident: ok_symbol })]),
            ),
        )]),
        value: Loc::at(
            region,
            Expr::Var {
                module_name: "",
                ident: ok_symbol,
            },
        ),
        guard: None,
    });

    let err_branch = env.arena.alloc(WhenBranch {
        patterns: env.arena.alloc([Loc::at(
            region,
            Pattern::Apply(
                env.arena.alloc(Loc::at(region, Pattern::Tag("Err"))),
                env.arena
                    .alloc([Loc::at(region, Pattern::Identifier { ident: err_symbol })]),
            ),
        )]),
        value: Loc::at(
            region,
            Expr::Return(
                env.arena.alloc(Loc::at(
                    region,
                    Expr::Apply(
                        env.arena.alloc(Loc::at(region, Expr::Tag("Err"))),
                        &*env.arena.alloc([&*env.arena.alloc(Loc::at(
                            region,
                            Expr::Var {
                                module_name: "",
                                ident: err_symbol,
                            },
                        ))]),
                        CalledVia::Try,
                    ),
                )),
                None,
            ),
        ),
        guard: None,
    });

    Loc::at(
        region,
        Expr::When(result_expr, &*env.arena.alloc([&*ok_branch, &*err_branch])),
    )
}

fn desugar_str_segments<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    segments: &'a [StrSegment<'a>],
) -> &'a [StrSegment<'a>] {
    let mut allocated = Vec::with_capacity_in(segments.len(), env.arena);

    for segment in segments.iter() {
        allocated.push(match segment {
            StrSegment::Plaintext(_) | StrSegment::Unicode(_) | StrSegment::EscapedChar(_) => {
                *segment
            }
            StrSegment::Interpolated(loc_expr) => {
                let loc_desugared = desugar_expr(
                    env,
                    scope,
                    env.arena.alloc(Loc {
                        region: loc_expr.region,
                        value: *loc_expr.value,
                    }),
                );
                StrSegment::Interpolated(Loc {
                    region: loc_desugared.region,
                    value: env.arena.alloc(loc_desugared.value),
                })
            }
        });
    }

    allocated.into_bump_slice()
}

fn desugar_field_collection<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    fields: Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>,
) -> Collection<'a, Loc<AssignedField<'a, Expr<'a>>>> {
    let mut allocated = Vec::with_capacity_in(fields.len(), env.arena);

    for field in fields.iter() {
        let value = desugar_field(env, scope, &field.value);

        allocated.push(Loc::at(field.region, value));
    }

    fields.replace_items(allocated.into_bump_slice())
}

fn desugar_field<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    field: &'a AssignedField<'a, Expr<'a>>,
) -> AssignedField<'a, Expr<'a>> {
    use roc_parse::ast::AssignedField::*;

    match field {
        RequiredValue(loc_str, spaces, loc_expr) => RequiredValue(
            Loc {
                value: loc_str.value,
                region: loc_str.region,
            },
            spaces,
            desugar_expr(env, scope, loc_expr),
        ),
        OptionalValue(loc_str, spaces, loc_expr) => OptionalValue(
            Loc {
                value: loc_str.value,
                region: loc_str.region,
            },
            spaces,
            desugar_expr(env, scope, loc_expr),
        ),
        IgnoredValue(loc_str, spaces, loc_expr) => IgnoredValue(
            Loc {
                value: loc_str.value,
                region: loc_str.region,
            },
            spaces,
            desugar_expr(env, scope, loc_expr),
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
                desugar_expr(env, scope, env.arena.alloc(loc_expr)),
            )
        }
        SpaceBefore(field, _spaces) => desugar_field(env, scope, field),
        SpaceAfter(field, _spaces) => desugar_field(env, scope, field),
    }
}

fn desugar_loc_patterns<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    loc_patterns: &'a [Loc<Pattern<'a>>],
) -> &'a [Loc<Pattern<'a>>] {
    let mut allocated = Vec::with_capacity_in(loc_patterns.len(), env.arena);

    for loc_pattern in loc_patterns.iter() {
        allocated.push(Loc {
            region: loc_pattern.region,
            value: desugar_pattern(env, scope, loc_pattern.value),
        });
    }

    allocated.into_bump_slice()
}

fn desugar_loc_pattern<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    loc_pattern: &'a Loc<Pattern<'a>>,
) -> &'a Loc<Pattern<'a>> {
    env.arena.alloc(Loc {
        region: loc_pattern.region,
        value: desugar_pattern(env, scope, loc_pattern.value),
    })
}

fn desugar_pattern<'a>(env: &mut Env<'a>, scope: &mut Scope, pattern: Pattern<'a>) -> Pattern<'a> {
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
            let mut desugared_arg_patterns = Vec::with_capacity_in(arg_patterns.len(), env.arena);
            for arg_pattern in arg_patterns.iter() {
                desugared_arg_patterns.push(Loc {
                    region: arg_pattern.region,
                    value: desugar_pattern(env, scope, arg_pattern.value),
                });
            }

            Apply(tag, desugared_arg_patterns.into_bump_slice())
        }
        RecordDestructure(field_patterns) => {
            RecordDestructure(desugar_record_destructures(env, scope, field_patterns))
        }
        RequiredField(name, field_pattern) => {
            RequiredField(name, desugar_loc_pattern(env, scope, field_pattern))
        }
        OptionalField(name, expr) => OptionalField(name, desugar_expr(env, scope, expr)),
        Tuple(patterns) => {
            let mut allocated = Vec::with_capacity_in(patterns.len(), env.arena);
            for pattern in patterns.iter() {
                let value = desugar_pattern(env, scope, pattern.value);
                allocated.push(Loc {
                    value,
                    region: pattern.region,
                });
            }
            let patterns = patterns.replace_items(allocated.into_bump_slice());

            Tuple(patterns)
        }
        List(patterns) => {
            let mut allocated = Vec::with_capacity_in(patterns.len(), env.arena);
            for pattern in patterns.iter() {
                let value = desugar_pattern(env, scope, pattern.value);
                allocated.push(Loc {
                    value,
                    region: pattern.region,
                });
            }
            let patterns = patterns.replace_items(allocated.into_bump_slice());

            List(patterns)
        }
        As(sub_pattern, symbol) => As(desugar_loc_pattern(env, scope, sub_pattern), symbol),
        SpaceBefore(sub_pattern, _spaces) => desugar_pattern(env, scope, *sub_pattern),
        SpaceAfter(sub_pattern, _spaces) => desugar_pattern(env, scope, *sub_pattern),
    }
}

pub fn desugar_record_destructures<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    field_patterns: Collection<'a, Loc<Pattern<'a>>>,
) -> Collection<'a, Loc<Pattern<'a>>> {
    let mut allocated = Vec::with_capacity_in(field_patterns.len(), env.arena);
    for field_pattern in field_patterns.iter() {
        let value = desugar_pattern(env, scope, field_pattern.value);
        allocated.push(Loc {
            value,
            region: field_pattern.region,
        });
    }

    field_patterns.replace_items(allocated.into_bump_slice())
}

/// Desugars a `dbg expr` expression into a statement block that prints and returns the
/// value produced by `expr`. Essentially:
/// (
///     tmpVar = expr
///     LowLevelDbg (Inspect.toStr tmpVar)
///     tmpVar
/// )
fn desugar_dbg_expr<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    expr: &'a Loc<Expr<'a>>,
    outer_region: Region,
) -> &'a Expr<'a> {
    let region = expr.region;

    // tmpVar = expr
    let ident = env.arena.alloc(scope.gen_unique_symbol_name().to_string());

    let value_def = ValueDef::Body(
        env.arena.alloc(Loc {
            value: Pattern::Identifier { ident },
            region,
        }),
        expr,
    );

    let defs = env.arena.alloc(Defs::default());
    defs.push_value_def(value_def, region, &[], &[]);

    // tmpVar
    let tmp_var = env.arena.alloc(Loc {
        value: Var {
            module_name: "",
            ident,
        },
        region,
    });

    // LowLevelDbg
    let dbg_stmt = env.arena.alloc(Loc {
        value: *desugar_dbg_stmt(env, tmp_var, tmp_var),
        region: outer_region,
    });

    env.arena.alloc(Defs(defs, dbg_stmt))
}

/// Build a desugared `dbg {}` expression to act as a placeholder when the AST
/// is invalid.
pub fn desugar_invalid_dbg_expr<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    outer_region: Region,
) -> &'a Expr<'a> {
    let placeholder_expr = env.arena.alloc(Loc {
        value: Record(Collection::empty()),
        region: outer_region,
    });

    desugar_dbg_expr(env, scope, placeholder_expr, outer_region)
}

/// Desugars a `dbg x` statement into essentially `Inspect.toStr x |> LowLevelDbg`
fn desugar_dbg_stmt<'a>(
    env: &mut Env<'a>,
    condition: &'a Loc<Expr<'a>>,
    continuation: &'a Loc<Expr<'a>>,
) -> &'a Expr<'a> {
    let region = condition.region;

    let inspect_fn = Var {
        module_name: ModuleName::INSPECT,
        ident: "toStr",
    };
    let loc_inspect_fn_var = env.arena.alloc(Loc {
        value: inspect_fn,
        region,
    });
    let inspect_args = &*env.arena.alloc([condition]);

    let dbg_str = env.arena.alloc(Loc {
        value: Apply(loc_inspect_fn_var, inspect_args, CalledVia::Space),
        region,
    });

    let line_col = env.line_info().convert_pos(region.start());

    let dbg_src = env
        .src
        .split_at(region.start().offset as usize)
        .1
        .split_at((region.end().offset - region.start().offset) as usize)
        .0;

    let module_path_str = env.module_path.to_string_lossy();

    // |> LowLevelDbg
    env.arena.alloc(LowLevelDbg(
        env.arena.alloc((
            &*env
                .arena
                .alloc_str(&format!("{}:{}", module_path_str, line_col.line + 1)),
            &*env.arena.alloc_str(dbg_src),
        )),
        dbg_str,
        continuation,
    ))
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
    }
}

fn desugar_bin_ops<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    whole_region: Region,
    lefts: &'a [(Loc<Expr<'_>>, Loc<BinOp>)],
    right: &'a Loc<Expr<'_>>,
) -> &'a Loc<Expr<'a>> {
    let mut arg_stack: Vec<&'a Loc<Expr>> = Vec::with_capacity_in(lefts.len() + 1, env.arena);
    let mut op_stack: Vec<Loc<BinOp>> = Vec::with_capacity_in(lefts.len(), env.arena);

    for (loc_expr, loc_op) in lefts {
        arg_stack.push(loc_expr);
        match run_binop_step(
            env,
            scope,
            whole_region,
            &mut arg_stack,
            &mut op_stack,
            *loc_op,
        ) {
            Err(problem) => return problem,
            Ok(()) => continue,
        }
    }

    let mut expr = right;

    for (left, loc_op) in arg_stack.into_iter().zip(op_stack.into_iter()).rev() {
        expr = env
            .arena
            .alloc(new_op_call_expr(env, scope, left, loc_op, expr));
    }

    expr
}

enum Step<'a> {
    Error(&'a Loc<Expr<'a>>),
    Push(Loc<BinOp>),
    Skip,
}

fn run_binop_step<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    whole_region: Region,
    arg_stack: &mut Vec<&'a Loc<Expr<'a>>>,
    op_stack: &mut Vec<Loc<BinOp>>,
    next_op: Loc<BinOp>,
) -> Result<(), &'a Loc<Expr<'a>>> {
    use Step::*;

    match binop_step(env, scope, whole_region, arg_stack, op_stack, next_op) {
        Error(problem) => Err(problem),
        Push(loc_op) => run_binop_step(env, scope, whole_region, arg_stack, op_stack, loc_op),
        Skip => Ok(()),
    }
}

fn binop_step<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
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

                    arg_stack.push(
                        env.arena
                            .alloc(new_op_call_expr(env, scope, left, stack_op, right)),
                    );

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

                            arg_stack.push(
                                env.arena
                                    .alloc(new_op_call_expr(env, scope, left, stack_op, right)),
                            );

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
                            let broken_expr = env
                                .arena
                                .alloc(new_op_call_expr(env, scope, left, stack_op, right));
                            let region = broken_expr.region;
                            let data = roc_parse::ast::PrecedenceConflict {
                                whole_region,
                                binop1_position: stack_op.region.start(),
                                binop1: stack_op.value,
                                binop2_position: bad_op.region.start(),
                                binop2: bad_op.value,
                                expr: env.arena.alloc(broken_expr),
                            };
                            let value = Expr::PrecedenceConflict(env.arena.alloc(data));

                            Step::Error(env.arena.alloc(Loc { region, value }))
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
