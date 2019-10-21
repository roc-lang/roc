use bumpalo::collections::Vec;
use bumpalo::Bump;
use operator::Operator::Pizza;
use operator::{CalledVia, Operator};
use parse::ast::Expr::{self, *};
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
/// then replace the Operator nodes with Apply nodes.
pub fn desugar<'a>(arena: &'a Bump, expr: Located<Expr<'a>>) -> Located<Expr<'a>> {
    use operator::Associativity::*;
    use std::cmp::Ordering;

    // NOTE: A potentially nice performance optimization here would be to use
    // arena bump allocation for Infixes, arg_stack, and op_stack. As long as we
    // allocate each element inside arg_stack outside the arena, this should end
    // up being a decent bit more efficient.
    let mut infixes = Infixes::new(arena.alloc(expr));
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
                                        let region = broken_expr.region.clone();
                                        let value = Expr::PrecedenceConflict(
                                            bad_op,
                                            stack_op,
                                            arena.alloc(broken_expr),
                                        );

                                        return Located { region, value };
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
        let expr = match loc_op.value {
            Pizza => {
                // Rewrite the Pizza operator into an Apply
                panic!("TODO desugar |> operator into an Apply");
            }
            binop => {
                // This is a normal binary operator like (+), so desugar it
                // into the appropriate function call.
                let (module_parts, name) = desugar_binop(&binop, arena);
                let mut args = Vec::with_capacity_in(2, arena);

                args.push(left.clone());
                args.push(right.clone());

                let loc_expr = Located {
                    value: Expr::Var(module_parts, name),
                    region: loc_op.region,
                };

                Apply(arena.alloc((loc_expr, args, CalledVia::Operator(binop))))
            }
        };

        arg_stack.push(arena.alloc(Located {
            region,
            value: expr,
        }));
    }

    assert_eq!(arg_stack.len(), 1);

    arg_stack.pop().unwrap().clone()
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
            "div",
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
            None => self.remaining_expr.take().map(|expr| match expr.value {
                Expr::Operator((left, op, right)) => {
                    self.remaining_expr = Some(right);
                    self.next_op = Some(op.clone());

                    InfixToken::Arg(left)
                }
                _ => InfixToken::Arg(expr),
            }),
        }
    }
}
