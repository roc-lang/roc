use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_module::ident::ModuleName;
use roc_module::operator::BinOp::Pizza;
use roc_module::operator::{BinOp, CalledVia};
use roc_parse::ast::Expr::{self, *};
use roc_parse::ast::{AssignedField, Def, Pattern, WhenBranch};
use roc_region::all::{Located, Region};

// BinOp precedence logic adapted from Gluon by Markus Westerlind, MIT licensed
// https://github.com/gluon-lang/gluon
// Thank you, Markus!

fn new_op_call_expr<'a>(
    arena: &'a Bump,
    left: &'a Located<Expr<'a>>,
    loc_op: Located<BinOp>,
    right: &'a Located<Expr<'a>>,
) -> Located<Expr<'a>> {
    let region = Region::span_across(&left.region, &right.region);

    let value = match loc_op.value {
        Pizza => {
            // Rewrite the Pizza operator into an Apply

            match &right.value {
                Apply(function, arguments, _called_via) => {
                    let mut args = Vec::with_capacity_in(1 + arguments.len(), arena);

                    args.push(left);

                    for arg in arguments.iter() {
                        args.push(arg);
                    }

                    let args = args.into_bump_slice();

                    Apply(function, args, CalledVia::BinOp(Pizza))
                }
                _ => {
                    // e.g. `1 |> (if b then (\a -> a) else (\c -> c))`
                    let mut args = Vec::with_capacity_in(1, arena);

                    args.push(left);

                    let args = args.into_bump_slice();

                    Apply(right, args, CalledVia::BinOp(Pizza))
                }
            }
        }
        binop => {
            // This is a normal binary operator like (+), so desugar it
            // into the appropriate function call.
            let (module_name, ident) = binop_to_function(binop);

            let args = arena.alloc([left, right]);

            let loc_expr = arena.alloc(Located {
                value: Expr::Var { module_name, ident },
                region: loc_op.region,
            });

            Apply(loc_expr, args, CalledVia::BinOp(binop))
        }
    };

    Located { value, region }
}

fn desugar_defs<'a>(
    arena: &'a Bump,
    region: Region,
    defs: &'a [&'a Located<Def<'a>>],
    loc_ret: &'a Located<Expr<'a>>,
) -> &'a Located<Expr<'a>> {
    let mut desugared_defs = Vec::with_capacity_in(defs.len(), arena);

    for loc_def in defs.iter() {
        let loc_def = Located {
            value: desugar_def(arena, &loc_def.value),
            region: loc_def.region,
        };

        desugared_defs.push(&*arena.alloc(loc_def));
    }

    let desugared_defs = desugared_defs.into_bump_slice();

    arena.alloc(Located {
        value: Defs(desugared_defs, desugar_expr(arena, loc_ret)),
        region,
    })
}

pub fn desugar_def<'a>(arena: &'a Bump, def: &'a Def<'a>) -> Def<'a> {
    use roc_parse::ast::Def::*;

    match def {
        Body(loc_pattern, loc_expr) | Nested(Body(loc_pattern, loc_expr)) => {
            Body(loc_pattern, desugar_expr(arena, loc_expr))
        }
        SpaceBefore(def, _)
        | SpaceAfter(def, _)
        | Nested(SpaceBefore(def, _))
        | Nested(SpaceAfter(def, _)) => desugar_def(arena, def),
        Nested(Nested(def)) => desugar_def(arena, def),
        alias @ Alias { .. } => Nested(alias),
        Nested(alias @ Alias { .. }) => Nested(alias),
        ann @ Annotation(_, _) => Nested(ann),
        Nested(ann @ Annotation(_, _)) => Nested(ann),
        AnnotatedBody {
            ann_pattern,
            ann_type,
            comment,
            body_pattern,
            body_expr,
        }
        | Nested(AnnotatedBody {
            ann_pattern,
            ann_type,
            comment,
            body_pattern,
            body_expr,
        }) => AnnotatedBody {
            ann_pattern,
            ann_type,
            comment: *comment,
            body_pattern: *body_pattern,
            body_expr: desugar_expr(arena, body_expr),
        },
        Nested(NotYetImplemented(s)) => todo!("{}", s),
        NotYetImplemented(s) => todo!("{}", s),
    }
}

/// Reorder the expression tree based on operator precedence and associativity rules,
/// then replace the BinOp nodes with Apply nodes. Also drop SpaceBefore and SpaceAfter nodes.
pub fn desugar_expr<'a>(arena: &'a Bump, loc_expr: &'a Located<Expr<'a>>) -> &'a Located<Expr<'a>> {
    match &loc_expr.value {
        Float(_)
        | Nested(Float(_))
        | Num(_)
        | Nested(Num(_))
        | NonBase10Int { .. }
        | Nested(NonBase10Int { .. })
        | Str(_)
        | Nested(Str(_))
        | AccessorFunction(_)
        | Nested(AccessorFunction(_))
        | Var { .. }
        | Nested(Var { .. })
        | MalformedIdent(_, _)
        | Nested(MalformedIdent(_, _))
        | MalformedClosure
        | Nested(MalformedClosure)
        | PrecedenceConflict { .. }
        | Nested(PrecedenceConflict { .. })
        | GlobalTag(_)
        | Nested(GlobalTag(_))
        | PrivateTag(_)
        | Nested(PrivateTag(_)) => loc_expr,

        Access(sub_expr, paths) | Nested(Access(sub_expr, paths)) => {
            let region = loc_expr.region;
            let loc_sub_expr = Located {
                region,
                value: Nested(sub_expr),
            };
            let value = Access(&desugar_expr(arena, arena.alloc(loc_sub_expr)).value, paths);

            arena.alloc(Located { region, value })
        }
        List {
            items,
            final_comments,
        }
        | Nested(List {
            items,
            final_comments,
        }) => {
            let mut new_items = Vec::with_capacity_in(items.len(), arena);

            for item in items.iter() {
                new_items.push(desugar_expr(arena, item));
            }
            let new_items = new_items.into_bump_slice();
            let value: Expr<'a> = List {
                items: new_items,
                final_comments,
            };

            arena.alloc(Located {
                region: loc_expr.region,
                value,
            })
        }
        Record {
            fields,
            final_comments,
        }
        | Nested(Record {
            fields,
            final_comments,
        }) => {
            let mut new_fields = Vec::with_capacity_in(fields.len(), arena);

            for field in fields.iter() {
                let value = desugar_field(arena, &field.value);

                new_fields.push(Located {
                    value,
                    region: field.region,
                });
            }

            let new_fields = new_fields.into_bump_slice();

            arena.alloc(Located {
                region: loc_expr.region,
                value: Record {
                    fields: new_fields,
                    final_comments,
                },
            })
        }

        RecordUpdate {
            fields,
            update,
            final_comments,
        }
        | Nested(RecordUpdate {
            fields,
            update,
            final_comments,
        }) => {
            // NOTE the `update` field is always a `Var { .. }` and does not need to be desugared
            let mut new_fields = Vec::with_capacity_in(fields.len(), arena);

            for field in fields.iter() {
                let value = desugar_field(arena, &field.value);

                new_fields.push(Located {
                    value,
                    region: field.region,
                });
            }

            let new_fields = new_fields.into_bump_slice();

            arena.alloc(Located {
                region: loc_expr.region,
                value: RecordUpdate {
                    update: *update,
                    fields: new_fields,
                    final_comments,
                },
            })
        }
        Closure(loc_patterns, loc_ret) | Nested(Closure(loc_patterns, loc_ret)) => {
            arena.alloc(Located {
                region: loc_expr.region,
                value: Closure(loc_patterns, desugar_expr(arena, loc_ret)),
            })
        }
        Backpassing(loc_patterns, loc_body, loc_ret)
        | Nested(Backpassing(loc_patterns, loc_body, loc_ret)) => {
            // loc_patterns <- loc_body
            //
            // loc_ret

            // first desugar the body, because it may contain |>
            let desugared_body = desugar_expr(arena, loc_body);

            match &desugared_body.value {
                Expr::Apply(function, arguments, called_via) => {
                    let desugared_ret = desugar_expr(arena, loc_ret);
                    let closure = Expr::Closure(loc_patterns, desugared_ret);
                    let loc_closure = Located::at_zero(closure);

                    let mut new_arguments: Vec<'a, &'a Located<Expr<'a>>> =
                        Vec::with_capacity_in(arguments.len() + 1, arena);
                    new_arguments.extend(arguments.iter());
                    new_arguments.push(arena.alloc(loc_closure));

                    let call = Expr::Apply(function, new_arguments.into_bump_slice(), *called_via);
                    let loc_call = Located::at(loc_expr.region, call);

                    arena.alloc(loc_call)
                }
                _ => panic!(),
            }
        }
        BinOps(lefts, right) | Nested(BinOps(lefts, right)) => {
            desugar_bin_ops(arena, loc_expr.region, lefts, right)
        }
        Defs(defs, loc_ret) | Nested(Defs(defs, loc_ret)) => {
            desugar_defs(arena, loc_expr.region, *defs, loc_ret)
        }
        Apply(loc_fn, loc_args, called_via) | Nested(Apply(loc_fn, loc_args, called_via)) => {
            let mut desugared_args = Vec::with_capacity_in(loc_args.len(), arena);

            for loc_arg in loc_args.iter() {
                desugared_args.push(desugar_expr(arena, loc_arg));
            }

            let desugared_args = desugared_args.into_bump_slice();

            arena.alloc(Located {
                value: Apply(desugar_expr(arena, loc_fn), desugared_args, *called_via),
                region: loc_expr.region,
            })
        }
        When(loc_cond_expr, branches) | Nested(When(loc_cond_expr, branches)) => {
            let loc_desugared_cond = &*arena.alloc(desugar_expr(arena, &loc_cond_expr));
            let mut desugared_branches = Vec::with_capacity_in(branches.len(), arena);

            for branch in branches.iter() {
                let desugared = desugar_expr(arena, &branch.value);

                let mut alternatives = Vec::with_capacity_in(branch.patterns.len(), arena);
                for loc_pattern in branch.patterns.iter() {
                    alternatives.push(Located {
                        region: loc_pattern.region,
                        value: Pattern::Nested(&loc_pattern.value),
                    })
                }

                let desugared_guard = if let Some(guard) = &branch.guard {
                    Some(desugar_expr(arena, guard).clone())
                } else {
                    None
                };

                let alternatives = alternatives.into_bump_slice();

                desugared_branches.push(&*arena.alloc(WhenBranch {
                    patterns: alternatives,
                    value: Located {
                        region: desugared.region,
                        value: Nested(&desugared.value),
                    },
                    guard: desugared_guard,
                }));
            }

            let desugared_branches = desugared_branches.into_bump_slice();

            arena.alloc(Located {
                value: When(loc_desugared_cond, desugared_branches),
                region: loc_expr.region,
            })
        }
        UnaryOp(loc_arg, loc_op) | Nested(UnaryOp(loc_arg, loc_op)) => {
            use roc_module::operator::UnaryOp::*;

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
            let loc_fn_var = arena.alloc(Located { region, value });
            let desugared_args = bumpalo::vec![in arena; desugar_expr(arena, loc_arg)];

            let desugared_args = desugared_args.into_bump_slice();

            arena.alloc(Located {
                value: Apply(loc_fn_var, desugared_args, CalledVia::UnaryOp(op)),
                region: loc_expr.region,
            })
        }
        SpaceBefore(expr, _)
        | Nested(SpaceBefore(expr, _))
        | SpaceAfter(expr, _)
        | Nested(SpaceAfter(expr, _))
        | ParensAround(expr)
        | Nested(ParensAround(expr))
        | Nested(Nested(expr)) => {
            // Since we've already begun canonicalization, spaces and parens
            // are no longer needed and should be dropped.
            desugar_expr(
                arena,
                arena.alloc(Located {
                    value: Nested(expr),
                    region: loc_expr.region,
                }),
            )
        }
        If(if_thens, final_else_branch) | Nested(If(if_thens, final_else_branch)) => {
            // If does not get desugared into `when` so we can give more targetted error messages during type checking.
            let desugared_final_else = &*arena.alloc(desugar_expr(arena, &final_else_branch));

            let mut desugared_if_thens = Vec::with_capacity_in(if_thens.len(), arena);

            for (condition, then_branch) in if_thens.iter() {
                desugared_if_thens.push((
                    desugar_expr(arena, condition).clone(),
                    desugar_expr(arena, then_branch).clone(),
                ));
            }

            arena.alloc(Located {
                value: If(desugared_if_thens.into_bump_slice(), desugared_final_else),
                region: loc_expr.region,
            })
        }
    }
}

fn desugar_field<'a>(
    arena: &'a Bump,
    field: &'a AssignedField<'a, Expr<'a>>,
) -> AssignedField<'a, Expr<'a>> {
    use roc_parse::ast::AssignedField::*;

    match field {
        RequiredValue(loc_str, spaces, loc_expr) => RequiredValue(
            Located {
                value: loc_str.value,
                region: loc_str.region,
            },
            spaces,
            desugar_expr(arena, loc_expr),
        ),
        OptionalValue(loc_str, spaces, loc_expr) => OptionalValue(
            Located {
                value: loc_str.value,
                region: loc_str.region,
            },
            spaces,
            desugar_expr(arena, loc_expr),
        ),
        LabelOnly(loc_str) => {
            // Desugar { x } into { x: x }
            let loc_expr = Located {
                value: Var {
                    module_name: "",
                    ident: loc_str.value,
                },
                region: loc_str.region,
            };

            RequiredValue(
                Located {
                    value: loc_str.value,
                    region: loc_str.region,
                },
                &[],
                desugar_expr(arena, arena.alloc(loc_expr)),
            )
        }
        SpaceBefore(field, _spaces) => desugar_field(arena, field),
        SpaceAfter(field, _spaces) => desugar_field(arena, field),

        Malformed(string) => Malformed(string),
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
        DoubleSlash => (ModuleName::NUM, "divFloor"),
        Percent => (ModuleName::NUM, "rem"),
        DoublePercent => (ModuleName::NUM, "mod"),
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
        HasType => unreachable!("Cannot desugar the : operator"),
        Backpassing => unreachable!("Cannot desugar the <- operator"),
    }
}

fn desugar_bin_ops<'a>(
    arena: &'a Bump,
    whole_region: Region,
    lefts: &'a [(Located<Expr<'_>>, Located<BinOp>)],
    right: &'a Located<Expr<'_>>,
) -> &'a Located<Expr<'a>> {
    let mut arg_stack: Vec<&'a Located<Expr>> = Vec::with_capacity_in(lefts.len() + 1, arena);
    let mut op_stack: Vec<Located<BinOp>> = Vec::with_capacity_in(lefts.len(), arena);

    for (loc_expr, loc_op) in lefts {
        arg_stack.push(desugar_expr(arena, loc_expr));
        match run_binop_step(arena, whole_region, &mut arg_stack, &mut op_stack, *loc_op) {
            Err(problem) => return problem,
            Ok(()) => continue,
        }
    }

    arg_stack.push(desugar_expr(arena, right));

    for loc_op in op_stack.into_iter().rev() {
        let right = arg_stack.pop().unwrap();
        let left = arg_stack.pop().unwrap();

        let region = Region::span_across(&left.region, &right.region);
        let value = match loc_op.value {
            Pizza => {
                // Rewrite the Pizza operator into an Apply

                match right.value {
                    Apply(function, arguments, _called_via) => {
                        let mut args = Vec::with_capacity_in(1 + arguments.len(), arena);

                        args.push(left);

                        for arg in arguments.iter() {
                            args.push(arg);
                        }

                        let args = args.into_bump_slice();

                        Apply(function, args, CalledVia::BinOp(Pizza))
                    }
                    _ => {
                        // e.g. `1 |> (if b then (\a -> a) else (\c -> c))`
                        let mut args = Vec::with_capacity_in(1, arena);

                        args.push(left);

                        let args = args.into_bump_slice();

                        Apply(right, args, CalledVia::BinOp(Pizza))
                    }
                }
            }
            binop => {
                // This is a normal binary operator like (+), so desugar it
                // into the appropriate function call.
                let (module_name, ident) = binop_to_function(binop);
                let mut args = Vec::with_capacity_in(2, arena);

                args.push(left);
                args.push(right);

                let loc_expr = arena.alloc(Located {
                    value: Expr::Var { module_name, ident },
                    region: loc_op.region,
                });

                let args = args.into_bump_slice();

                Apply(loc_expr, args, CalledVia::BinOp(binop))
            }
        };

        arg_stack.push(arena.alloc(Located { region, value }));
    }

    assert_eq!(arg_stack.len(), 1);

    arg_stack.pop().unwrap()
}

enum Step<'a> {
    Error(&'a Located<Expr<'a>>),
    Push(Located<BinOp>),
    Skip,
}

fn run_binop_step<'a>(
    arena: &'a Bump,
    whole_region: Region,
    arg_stack: &mut Vec<&'a Located<Expr<'a>>>,
    op_stack: &mut Vec<Located<BinOp>>,
    next_op: Located<BinOp>,
) -> Result<(), &'a Located<Expr<'a>>> {
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
    arg_stack: &mut Vec<&'a Located<Expr<'a>>>,
    op_stack: &mut Vec<Located<BinOp>>,
    next_op: Located<BinOp>,
) -> Step<'a> {
    use roc_module::operator::Associativity::*;
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

                            Step::Error(arena.alloc(Located { region, value }))
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
                            panic!("BinOps had the same associativity, but different precedence. This should never happen!");
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
