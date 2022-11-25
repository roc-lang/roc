#![allow(clippy::manual_map)]

use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_module::called_via::BinOp::Pizza;
use roc_module::called_via::{BinOp, CalledVia};
use roc_module::ident::ModuleName;
use roc_parse::ast::Expr::{self, *};
use roc_parse::ast::{AssignedField, ValueDef, WhenBranch};
use roc_region::all::{Loc, Region};

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

fn desugar_value_def<'a>(arena: &'a Bump, def: &'a ValueDef<'a>) -> ValueDef<'a> {
    use ValueDef::*;

    match def {
        Body(loc_pattern, loc_expr) => Body(loc_pattern, desugar_expr(arena, loc_expr)),
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
            body_pattern,
            body_expr: desugar_expr(arena, body_expr),
        },
        Dbg {
            condition,
            preceding_comment,
        } => {
            let desugared_condition = &*arena.alloc(desugar_expr(arena, condition));
            Dbg {
                condition: desugared_condition,
                preceding_comment: *preceding_comment,
            }
        }
        Expect {
            condition,
            preceding_comment,
        } => {
            let desugared_condition = &*arena.alloc(desugar_expr(arena, condition));
            Expect {
                condition: desugared_condition,
                preceding_comment: *preceding_comment,
            }
        }
        ExpectFx {
            condition,
            preceding_comment,
        } => {
            let desugared_condition = &*arena.alloc(desugar_expr(arena, condition));
            ExpectFx {
                condition: desugared_condition,
                preceding_comment: *preceding_comment,
            }
        }
    }
}

pub fn desugar_defs<'a>(arena: &'a Bump, defs: &mut roc_parse::ast::Defs<'a>) {
    for value_def in defs.value_defs.iter_mut() {
        *value_def = desugar_value_def(arena, arena.alloc(*value_def));
    }
}

/// Reorder the expression tree based on operator precedence and associativity rules,
/// then replace the BinOp nodes with Apply nodes. Also drop SpaceBefore and SpaceAfter nodes.
pub fn desugar_expr<'a>(arena: &'a Bump, loc_expr: &'a Loc<Expr<'a>>) -> &'a Loc<Expr<'a>> {
    match &loc_expr.value {
        Float(..)
        | Num(..)
        | NonBase10Int { .. }
        | Str(_)
        | SingleQuote(_)
        | RecordAccessorFunction(_)
        | TupleAccessorFunction(_)
        | Var { .. }
        | Underscore { .. }
        | MalformedIdent(_, _)
        | MalformedClosure
        | PrecedenceConflict { .. }
        | Tag(_)
        | OpaqueRef(_)
        | Crash => loc_expr,

        TupleAccess(_sub_expr, _paths) => todo!("Handle TupleAccess"),
        RecordAccess(sub_expr, paths) => {
            let region = loc_expr.region;
            let loc_sub_expr = Loc {
                region,
                value: **sub_expr,
            };
            let value = RecordAccess(&desugar_expr(arena, arena.alloc(loc_sub_expr)).value, paths);

            arena.alloc(Loc { region, value })
        }
        List(items) => {
            let mut new_items = Vec::with_capacity_in(items.len(), arena);

            for item in items.iter() {
                new_items.push(desugar_expr(arena, item));
            }
            let new_items = new_items.into_bump_slice();
            let value: Expr<'a> = List(items.replace_items(new_items));

            arena.alloc(Loc {
                region: loc_expr.region,
                value,
            })
        }
        Record(fields) => arena.alloc(Loc {
            region: loc_expr.region,
            value: Record(fields.map_items(arena, |field| {
                let value = desugar_field(arena, &field.value);
                Loc {
                    value,
                    region: field.region,
                }
            })),
        }),
        Tuple(_fields) => {
            todo!("desugar_expr: Tuple");
        }
        RecordUpdate { fields, update } => {
            // NOTE the `update` field is always a `Var { .. }`, we only desugar it to get rid of
            // any spaces before/after
            let new_update = desugar_expr(arena, update);

            let new_fields = fields.map_items(arena, |field| {
                let value = desugar_field(arena, &field.value);
                Loc {
                    value,
                    region: field.region,
                }
            });

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
            value: Closure(loc_patterns, desugar_expr(arena, loc_ret)),
        }),
        Backpassing(loc_patterns, loc_body, loc_ret) => {
            // loc_patterns <- loc_body
            //
            // loc_ret

            // first desugar the body, because it may contain |>
            let desugared_body = desugar_expr(arena, loc_body);

            let desugared_ret = desugar_expr(arena, loc_ret);
            let closure = Expr::Closure(loc_patterns, desugared_ret);
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
        BinOps(lefts, right) => desugar_bin_ops(arena, loc_expr.region, lefts, right),
        Defs(defs, loc_ret) => {
            let mut defs = (*defs).clone();
            desugar_defs(arena, &mut defs);

            let loc_ret = desugar_expr(arena, loc_ret);

            arena.alloc(Loc::at(loc_expr.region, Defs(arena.alloc(defs), loc_ret)))
        }
        Apply(loc_fn, loc_args, called_via) => {
            let mut desugared_args = Vec::with_capacity_in(loc_args.len(), arena);

            for loc_arg in loc_args.iter() {
                desugared_args.push(desugar_expr(arena, loc_arg));
            }

            let desugared_args = desugared_args.into_bump_slice();

            arena.alloc(Loc {
                value: Apply(desugar_expr(arena, loc_fn), desugared_args, *called_via),
                region: loc_expr.region,
            })
        }
        When(loc_cond_expr, branches) => {
            let loc_desugared_cond = &*arena.alloc(desugar_expr(arena, loc_cond_expr));
            let mut desugared_branches = Vec::with_capacity_in(branches.len(), arena);

            for branch in branches.iter() {
                let desugared = desugar_expr(arena, &branch.value);

                let mut alternatives = Vec::with_capacity_in(branch.patterns.len(), arena);
                alternatives.extend(branch.patterns.iter().copied());

                let desugared_guard = if let Some(guard) = &branch.guard {
                    Some(*desugar_expr(arena, guard))
                } else {
                    None
                };

                let alternatives = alternatives.into_bump_slice();

                desugared_branches.push(&*arena.alloc(WhenBranch {
                    patterns: alternatives,
                    value: *desugared,
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
            let desugared_args = arena.alloc([desugar_expr(arena, loc_arg)]);

            arena.alloc(Loc {
                value: Apply(loc_fn_var, desugared_args, CalledVia::UnaryOp(op)),
                region: loc_expr.region,
            })
        }
        SpaceBefore(expr, _) | SpaceAfter(expr, _) | ParensAround(expr) => {
            // Since we've already begun canonicalization, spaces and parens
            // are no longer needed and should be dropped.
            desugar_expr(
                arena,
                arena.alloc(Loc {
                    value: **expr,
                    region: loc_expr.region,
                }),
            )
        }
        If(if_thens, final_else_branch) => {
            // If does not get desugared into `when` so we can give more targeted error messages during type checking.
            let desugared_final_else = &*arena.alloc(desugar_expr(arena, final_else_branch));

            let mut desugared_if_thens = Vec::with_capacity_in(if_thens.len(), arena);

            for (condition, then_branch) in if_thens.iter() {
                desugared_if_thens.push((
                    *desugar_expr(arena, condition),
                    *desugar_expr(arena, then_branch),
                ));
            }

            arena.alloc(Loc {
                value: If(desugared_if_thens.into_bump_slice(), desugared_final_else),
                region: loc_expr.region,
            })
        }
        Expect(condition, continuation) => {
            let desugared_condition = &*arena.alloc(desugar_expr(arena, condition));
            let desugared_continuation = &*arena.alloc(desugar_expr(arena, continuation));
            arena.alloc(Loc {
                value: Expect(desugared_condition, desugared_continuation),
                region: loc_expr.region,
            })
        }
        Dbg(condition, continuation) => {
            let desugared_condition = &*arena.alloc(desugar_expr(arena, condition));
            let desugared_continuation = &*arena.alloc(desugar_expr(arena, continuation));
            arena.alloc(Loc {
                value: Dbg(desugared_condition, desugared_continuation),
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
            Loc {
                value: loc_str.value,
                region: loc_str.region,
            },
            spaces,
            desugar_expr(arena, loc_expr),
        ),
        OptionalValue(loc_str, spaces, loc_expr) => OptionalValue(
            Loc {
                value: loc_str.value,
                region: loc_str.region,
            },
            spaces,
            desugar_expr(arena, loc_expr),
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
) -> &'a Loc<Expr<'a>> {
    let mut arg_stack: Vec<&'a Loc<Expr>> = Vec::with_capacity_in(lefts.len() + 1, arena);
    let mut op_stack: Vec<Loc<BinOp>> = Vec::with_capacity_in(lefts.len(), arena);

    for (loc_expr, loc_op) in lefts {
        arg_stack.push(desugar_expr(arena, loc_expr));
        match run_binop_step(arena, whole_region, &mut arg_stack, &mut op_stack, *loc_op) {
            Err(problem) => return problem,
            Ok(()) => continue,
        }
    }

    let mut expr = desugar_expr(arena, right);

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
