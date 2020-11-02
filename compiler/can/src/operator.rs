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
fn new_op_expr<'a>(
    arena: &'a Bump,
    left: Located<Expr<'a>>,
    op: Located<BinOp>,
    right: Located<Expr<'a>>,
) -> Located<Expr<'a>> {
    let new_region = Region {
        start_line: left.region.start_line,
        start_col: left.region.start_col,

        end_line: right.region.end_line,
        end_col: right.region.end_col,
    };
    let new_expr = Expr::BinOp(arena.alloc((left, op, right)));

    Located {
        value: new_expr,
        region: new_region,
    }
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
        | MalformedIdent(_)
        | Nested(MalformedIdent(_))
        | MalformedClosure
        | Nested(MalformedClosure)
        | PrecedenceConflict(_, _, _, _)
        | Nested(PrecedenceConflict(_, _, _, _))
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
        List(elems) | Nested(List(elems)) => {
            let mut new_elems = Vec::with_capacity_in(elems.len(), arena);

            for elem in elems.iter() {
                new_elems.push(desugar_expr(arena, elem));
            }
            let new_elems = new_elems.into_bump_slice();
            let value: Expr<'a> = List(new_elems);

            arena.alloc(Located {
                region: loc_expr.region,
                value,
            })
        }
        Record { fields, update } | Nested(Record { fields, update }) => {
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
                    update: *update,
                    fields: new_fields,
                },
            })
        }
        Closure(loc_patterns, loc_ret) | Nested(Closure(loc_patterns, loc_ret)) => {
            arena.alloc(Located {
                region: loc_expr.region,
                value: Closure(loc_patterns, desugar_expr(arena, loc_ret)),
            })
        }
        BinOp(_) | Nested(BinOp(_)) => desugar_bin_op(arena, loc_expr),
        Defs(defs, loc_ret) | Nested(Defs(defs, loc_ret)) => {
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
                region: loc_expr.region,
            })
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
        If(condition, then_branch, else_branch)
        | Nested(If(condition, then_branch, else_branch)) => {
            // If does not get desugared yet so we can give more targetted error messages during
            // type checking.
            let desugared_cond = &*arena.alloc(desugar_expr(arena, &condition));
            let desugared_then = &*arena.alloc(desugar_expr(arena, &then_branch));
            let desugared_else = &*arena.alloc(desugar_expr(arena, &else_branch));

            arena.alloc(Located {
                value: If(desugared_cond, desugared_then, desugared_else),
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
        SpaceBefore(field, spaces) => SpaceBefore(arena.alloc(desugar_field(arena, field)), spaces),
        SpaceAfter(field, spaces) => SpaceAfter(arena.alloc(desugar_field(arena, field)), spaces),

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
    }
}

fn desugar_bin_op<'a>(arena: &'a Bump, loc_expr: &'a Located<Expr<'_>>) -> &'a Located<Expr<'a>> {
    use roc_module::operator::Associativity::*;
    use std::cmp::Ordering;

    let mut infixes = Infixes::new(loc_expr);
    let mut arg_stack: Vec<&'a Located<Expr>> = Vec::new_in(arena);
    let mut op_stack: Vec<Located<BinOp>> = Vec::new_in(arena);

    while let Some(token) = infixes.next() {
        match token {
            InfixToken::Arg(next_expr) => arg_stack.push(next_expr),
            InfixToken::Op(next_op) => {
                match op_stack.pop() {
                    Some(stack_op) => {
                        match next_op.value.cmp(&stack_op.value) {
                            Ordering::Less => {
                                // Inline
                                let right = arg_stack.pop().unwrap();
                                let left = arg_stack.pop().unwrap();

                                infixes.next_op = Some(next_op);
                                arg_stack.push(arena.alloc(new_op_expr(
                                    arena,
                                    Located {
                                        value: Nested(&left.value),
                                        region: left.region,
                                    },
                                    stack_op,
                                    Located {
                                        value: Nested(&right.value),
                                        region: right.region,
                                    },
                                )));
                            }

                            Ordering::Greater => {
                                // Swap
                                op_stack.push(stack_op);
                                op_stack.push(next_op);
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

                                        infixes.next_op = Some(next_op);
                                        arg_stack.push(arena.alloc(new_op_expr(
                                            arena,
                                            Located {
                                                value: Nested(&left.value),
                                                region: left.region,
                                            },
                                            stack_op,
                                            Located {
                                                value: Nested(&right.value),
                                                region: right.region,
                                            },
                                        )));
                                    }

                                    (RightAssociative, RightAssociative) => {
                                        // Swap
                                        op_stack.push(stack_op);
                                        op_stack.push(next_op);
                                    }

                                    (NonAssociative, NonAssociative) => {
                                        // Both operators were non-associative, e.g. (True == False == False).
                                        // We should tell the author to disambiguate by grouping them with parens.
                                        let bad_op = next_op.clone();
                                        let right = arg_stack.pop().unwrap();
                                        let left = arg_stack.pop().unwrap();
                                        let broken_expr = new_op_expr(
                                            arena,
                                            Located {
                                                value: Nested(&left.value),
                                                region: left.region,
                                            },
                                            next_op,
                                            Located {
                                                value: Nested(&right.value),
                                                region: right.region,
                                            },
                                        );
                                        let region = broken_expr.region;
                                        let value = Expr::PrecedenceConflict(
                                            loc_expr.region,
                                            stack_op,
                                            bad_op,
                                            arena.alloc(broken_expr),
                                        );

                                        return arena.alloc(Located { region, value });
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
                    None => op_stack.push(next_op),
                };
            }
        }
    }

    for loc_op in op_stack.into_iter().rev() {
        let right = desugar_expr(arena, arg_stack.pop().unwrap());
        let left = desugar_expr(arena, arg_stack.pop().unwrap());

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
                    expr => {
                        // e.g. `1 |> (if b then (\a -> a) else (\c -> c))`
                        let mut args = Vec::with_capacity_in(1, arena);

                        args.push(left);

                        let function = arena.alloc(Located {
                            value: Nested(expr),
                            region: right.region,
                        });

                        let args = args.into_bump_slice();

                        Apply(function, args, CalledVia::BinOp(Pizza))
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

#[derive(Debug, Clone, PartialEq)]
enum InfixToken<'a> {
    Arg(&'a Located<Expr<'a>>),
    Op(Located<BinOp>),
}

/// An iterator that takes an expression that has had its operators grouped
/// with _right associativity_, and yeilds a sequence of `InfixToken`s. This
/// is useful for reparsing the operators with their correct associativies
/// and precedences.
///
/// For example, the expression:
///
/// ```text
/// (1 + (2 ^ (4 * (6 - 8))))
/// ```
///
/// Will result in the following iterations:
///
/// ```text
/// Arg:  1
/// Op:   +
/// Arg:  2
/// Op:   ^
/// Arg:  4
/// Op:   *
/// Arg:  6
/// Op:   -
/// Arg:  8
/// ```
struct Infixes<'a> {
    /// The next part of the expression that we need to flatten
    remaining_expr: Option<&'a Located<Expr<'a>>>,
    /// Cached operator from a previous iteration
    next_op: Option<Located<BinOp>>,
}

impl<'a> Infixes<'a> {
    fn new(expr: &'a Located<Expr<'a>>) -> Infixes<'a> {
        Infixes {
            remaining_expr: Some(expr),
            next_op: None,
        }
    }
}

impl<'a> Iterator for Infixes<'a> {
    type Item = InfixToken<'a>;

    fn next(&mut self) -> Option<InfixToken<'a>> {
        match self.next_op.take() {
            Some(op) => Some(InfixToken::Op(op)),
            None => self
                .remaining_expr
                .take()
                .map(|loc_expr| match loc_expr.value {
                    Expr::BinOp((left, loc_op, right))
                    | Expr::Nested(Expr::BinOp((left, loc_op, right))) => {
                        self.remaining_expr = Some(right);
                        self.next_op = Some(loc_op.clone());

                        InfixToken::Arg(left)
                    }
                    _ => InfixToken::Arg(loc_expr),
                }),
        }
    }
}
