use bumpalo::collections::Vec;
use bumpalo::Bump;
use operator::Operator::Pizza;
use operator::{CalledVia, Operator};
use parse::ast::Expr::{self, *};
use parse::ast::{AssignedField, Def};
use region::{Located, Region};
use types;

// Operator precedence logic adapted from Gluon by Markus Westerlind, MIT licensed
// https://github.com/gluon-lang/gluon
// Thank you, Markus!
fn new_op_expr<'a>(
    arena: &'a Bump,
    left: Located<Expr<'a>>,
    op: Located<Operator>,
    right: Located<Expr<'a>>,
) -> Located<Expr<'a>> {
    let new_region = Region {
        start_line: left.region.start_line,
        start_col: left.region.start_col,

        end_line: right.region.end_line,
        end_col: right.region.end_col,
    };
    let new_expr = Expr::Operator(arena.alloc((left, op, right)));

    Located {
        value: new_expr,
        region: new_region,
    }
}

/// Reorder the expression tree based on operator precedence and associativity rules,
/// then replace the Operator nodes with Apply nodes. Also drop SpaceBefore and SpaceAfter nodes.
pub fn desugar<'a>(arena: &'a Bump, loc_expr: &'a Located<Expr<'a>>) -> &'a Located<Expr<'a>> {
    use operator::Associativity::*;
    use std::cmp::Ordering;

    match &loc_expr.value {
        Float(_)
        | Int(_)
        | HexInt(_)
        | OctalInt(_)
        | BinaryInt(_)
        | Str(_)
        | BlockStr(_)
        | QualifiedField(_, _)
        | AccessorFunction(_)
        | Var(_, _)
        | MalformedIdent(_)
        | MalformedClosure
        | PrecedenceConflict(_, _, _)
        | Variant(_, _) => loc_expr,

        Field(sub_expr, paths) => arena.alloc(Located {
            region: loc_expr.region,
            value: Field(desugar(arena, sub_expr), paths.clone()),
        }),
        List(elems) => {
            let mut new_elems = Vec::with_capacity_in(elems.len(), arena);

            for elem in elems {
                new_elems.push(desugar(arena, elem));
            }
            let value: Expr<'a> = List(new_elems);

            arena.alloc(Located {
                region: loc_expr.region,
                value,
            })
        }
        Record(fields) => {
            let mut new_fields = Vec::with_capacity_in(fields.len(), arena);

            for field in fields {
                let value = desugar_field(arena, &field.value);

                new_fields.push(Located {
                    value,
                    region: field.region,
                });
            }

            arena.alloc(Located {
                region: loc_expr.region,
                value: Record(new_fields),
            })
        }
        Closure(loc_patterns, loc_ret) => arena.alloc(Located {
            region: loc_expr.region,
            value: Closure(loc_patterns, desugar(arena, loc_ret)),
        }),
        Operator(_) => {
            let mut infixes = Infixes::new(arena.alloc(loc_expr));
            let mut arg_stack: Vec<&'a Located<Expr>> = Vec::new_in(arena);
            let mut op_stack: Vec<Located<Operator>> = Vec::new_in(arena);

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
                                            left.clone(),
                                            stack_op,
                                            right.clone(),
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
                                                    left.clone(),
                                                    stack_op,
                                                    right.clone(),
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
                                                    left.clone(),
                                                    next_op,
                                                    right.clone(),
                                                );
                                                let region = broken_expr.region;
                                                let value = Expr::PrecedenceConflict(
                                                    bad_op,
                                                    stack_op,
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
                                                panic!("Operators had the same associativity, but different precedence. This should never happen!");
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
                let right = arg_stack.pop().unwrap();
                let left = arg_stack.pop().unwrap();

                let region = Region::span_across(&left.region, &right.region);
                let value = match loc_op.value {
                    Pizza => {
                        // Rewrite the Pizza operator into an Apply
                        panic!("TODO desugar |> operator into an Apply");
                    }
                    binop => {
                        // This is a normal binary operator like (+), so desugar it
                        // into the appropriate function call.
                        let (module_parts, name) = desugar_binop(&binop, arena);
                        let mut args = Vec::with_capacity_in(2, arena);

                        args.push(*arena.alloc(left));
                        args.push(*arena.alloc(right));

                        let loc_expr = arena.alloc(Located {
                            value: Expr::Var(module_parts, name),
                            region: loc_op.region,
                        });

                        Apply(loc_expr, args, CalledVia::Operator(binop))
                    }
                };

                arg_stack.push(arena.alloc(Located { region, value }));
            }

            assert_eq!(arg_stack.len(), 1);

            arg_stack.pop().unwrap()
        }

        Defs(defs, loc_ret) => {
            let mut desugared_defs = Vec::with_capacity_in(defs.len(), arena);

            for loc_def in defs.into_iter() {
                let loc_def = desugar_def(arena, &loc_def, loc_expr);

                desugared_defs.push(loc_def);
            }

            arena.alloc(Located {
                value: Defs(desugared_defs, desugar(arena, loc_ret)),
                region: loc_expr.region,
            })
        }
        Apply(loc_fn, loc_args, called_via) => {
            let mut desugared_args = Vec::with_capacity_in(loc_args.len(), arena);

            for loc_arg in loc_args {
                desugared_args.push(desugar(arena, loc_arg));
            }

            arena.alloc(Located {
                value: Apply(desugar(arena, loc_fn), desugared_args, called_via.clone()),
                region: loc_expr.region,
            })
        }
        // If(&'a (Loc<Expr<'a>>, Loc<Expr<'a>>, Loc<Expr<'a>>)),
        // Case(
        //     &'a Loc<Expr<'a>>,
        //     Vec<'a, &'a (Loc<Pattern<'a>>, Loc<Expr<'a>>)>,
        // ),
        SpaceBefore(expr, _) => {
            // Since we've already begun canonicalization, these are no longer needed
            // and should be dropped.
            desugar(
                arena,
                arena.alloc(Located {
                    // TODO FIXME performance disaster!!! Must remove this clone!
                    //
                    // This won't be easy because:
                    //
                    // * If this function takes an &'a Expr, then Infixes hits a problem.
                    // * If SpaceBefore holds a Loc<&'a Expr>, then Spaceable hits a problem.
                    // * If all the existing &'a Loc<Expr> values become Loc<&'a Expr>...who knows?
                    value: (*expr).clone(),
                    region: loc_expr.region,
                }),
            )
        }
        SpaceAfter(expr, _) => {
            // Since we've already begun canonicalization, these are no longer needed
            // and should be dropped.
            desugar(
                arena,
                arena.alloc(Located {
                    // TODO FIXME performance disaster!!! Must remove this clone! (Not easy.)
                    value: (*expr).clone(),
                    region: loc_expr.region,
                }),
            )
        }
        other => panic!("TODO desugar {:?}", other),
    }
}

fn desugar_def<'a>(
    arena: &'a Bump,
    loc_def: &'a Located<Def<'a>>,
    loc_expr: &'a Located<Expr<'a>>,
) -> &'a Located<Def<'a>> {
    match loc_def.value {
        Def::Body(ref pattern, loc_expr) => arena.alloc(Located {
            value: Def::Body(pattern.clone(), desugar(arena, loc_expr)),
            region: loc_def.region,
        }),
        Def::Annotation(_, _) => loc_def,
        Def::CustomType(_, _) => loc_def,
        Def::TypeAlias(_, _) => loc_def,
        Def::SpaceBefore(other_def, _) | Def::SpaceAfter(other_def, _) => desugar_def(
            arena,
            arena.alloc(Located {
                value: other_def.clone(),
                region: loc_def.region,
            }),
            loc_expr,
        ),
    }
}

fn desugar_field<'a>(
    arena: &'a Bump,
    field: &'a AssignedField<'a, Expr<'a>>,
) -> AssignedField<'a, Expr<'a>> {
    use parse::ast::AssignedField::*;
    match field {
        LabeledValue(ref loc_str, spaces, loc_expr) => {
            AssignedField::LabeledValue(loc_str.clone(), spaces, desugar(arena, loc_expr))
        }
        LabelOnly(ref loc_str, spaces) => LabelOnly(loc_str.clone(), spaces),
        SpaceBefore(ref field, spaces) => {
            SpaceBefore(arena.alloc(desugar_field(arena, field)), spaces)
        }
        SpaceAfter(ref field, spaces) => {
            SpaceAfter(arena.alloc(desugar_field(arena, field)), spaces)
        }

        Malformed(string) => Malformed(string),
    }
}

#[inline(always)]
fn desugar_binop<'a>(binop: &Operator, arena: &'a Bump) -> (&'a [&'a str], &'a str) {
    use self::Operator::*;

    match binop {
        Caret => (
            bumpalo::vec![ in arena; types::MOD_NUM ].into_bump_slice(),
            "pow",
        ),
        Star => (
            bumpalo::vec![ in arena; types::MOD_NUM ].into_bump_slice(),
            "mul",
        ),
        Slash => (
            bumpalo::vec![ in arena; types::MOD_FLOAT ].into_bump_slice(),
            "div",
        ),
        DoubleSlash => (
            bumpalo::vec![ in arena; types::MOD_INT ].into_bump_slice(),
            "divFloor",
        ),
        Percent => (
            bumpalo::vec![ in arena; types::MOD_NUM ].into_bump_slice(),
            "rem",
        ),
        DoublePercent => (
            bumpalo::vec![ in arena; types::MOD_NUM ].into_bump_slice(),
            "mod",
        ),
        Plus => (
            bumpalo::vec![ in arena; types::MOD_NUM ].into_bump_slice(),
            "plus",
        ),
        Minus => (
            bumpalo::vec![ in arena; types::MOD_NUM ].into_bump_slice(),
            "sub",
        ),
        Equals => (
            bumpalo::vec![ in arena; types::MOD_BOOL ].into_bump_slice(),
            "isEq",
        ),
        NotEquals => (
            bumpalo::vec![ in arena; types::MOD_BOOL ].into_bump_slice(),
            "isNotEq",
        ),
        LessThan => (
            bumpalo::vec![ in arena; types::MOD_NUM ].into_bump_slice(),
            "isLt",
        ),
        GreaterThan => (
            bumpalo::vec![ in arena; types::MOD_NUM ].into_bump_slice(),
            "isGt",
        ),
        LessThanOrEq => (
            bumpalo::vec![ in arena; types::MOD_NUM ].into_bump_slice(),
            "isLte",
        ),
        GreaterThanOrEq => (
            bumpalo::vec![ in arena; types::MOD_NUM ].into_bump_slice(),
            "isGte",
        ),
        And => (
            bumpalo::vec![ in arena; types::MOD_BOOL ].into_bump_slice(),
            "and",
        ),
        Or => (
            bumpalo::vec![ in arena; types::MOD_BOOL ].into_bump_slice(),
            "or",
        ),
        Pizza => panic!("Cannot desugar the |> operator"),
    }
}

#[derive(Debug, Clone, PartialEq)]
enum InfixToken<'a> {
    Arg(&'a Located<Expr<'a>>),
    Op(Located<Operator>),
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
    next_op: Option<Located<Operator>>,
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
                    Expr::Operator((left, op, right)) => {
                        self.remaining_expr = Some(right);
                        self.next_op = Some(op.clone());

                        InfixToken::Arg(left)
                    }
                    _ => InfixToken::Arg(loc_expr),
                }),
        }
    }
}
