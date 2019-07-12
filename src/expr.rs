
use operator::Operator;
use operator::Associativity::*;
use std::cmp::Ordering;
use region::{Located, Region};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Frac(i64, i64),
    EmptyStr,
    Str(String),
    InterpolatedStr(Vec<(String, Located<Ident>)>, String),
    Char(char),

    Var(Ident),
    Assign(Located<Pattern>, Box<Located<Expr>>, Box<Located<Expr>>),

    // Functions
    CallByName(Ident, Vec<Located<Expr>>),
    Apply(Box<Located<Expr>>, Vec<Located<Expr>>),
    Operator(Box<Located<Expr>>, Located<Operator>, Box<Located<Expr>>),
    Closure(Vec<Located<Pattern>>, Box<Located<Expr>>),

    // Sum Types
    ApplyVariant(String, Option<Vec<Located<Expr>>>),

    // Product Types
    EmptyRecord,

    // Conditionals
    If(Box<Located<Expr>>, Box<Located<Expr>>, Box<Located<Expr>>),
    Case(Box<Located<Expr>>, Vec<(Located<Pattern>, Box<Located<Expr>>)>),
}

pub type Ident = String;

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(String),
    Variant(String, Option<Vec<Located<Pattern>>>),
    Integer(i64),
    Fraction(i64, i64),
    EmptyRecordLiteral,
    Underscore
}

impl Expr {
    pub fn walk<F>(self: &Expr, transform: &F) -> Expr
        where F: Fn(&Expr) -> Expr
    {
        use self::Expr::*;

        let transformed = transform(self);

        match transformed {
            Int(_) | Frac(_, _) | EmptyStr | Str(_) | Char(_) | Var(_) | EmptyRecord | InterpolatedStr(_, _) => transformed,
            Assign(pattern, expr1, expr2) => {
                Assign(
                    pattern,
                    Box::new(expr1.with_value(expr1.value.walk(transform))),
                    Box::new(expr2.with_value(expr2.value.walk(transform)))
                )
            },
            CallByName(ident, args) => {
                CallByName(
                    ident,
                    args.into_iter().map(|arg| arg.with_value(arg.value.walk(transform))).collect()
                )
            },
            Apply(fn_expr, args) => {
                Apply(
                    Box::new(fn_expr.with_value(fn_expr.value.walk(transform))),
                    args.into_iter().map(|arg| arg.with_value(arg.value.walk(transform))).collect()
                )
            },
            Closure(patterns, body) => Closure(patterns, Box::new(body.with_value(body.value.walk(transform)))),
            ApplyVariant(_, None) => transformed,
            ApplyVariant(name, Some(args)) => {
                ApplyVariant(
                    name,
                    Some(
                        args.into_iter().map(|arg| arg.with_value(arg.value.walk(transform))).collect())
                    )
            },
            If(condition, if_true, if_false) => {
                If(
                    Box::new(condition.with_value(condition.value.walk(transform))),
                    Box::new(if_true.with_value(if_true.value.walk(transform))),
                    Box::new(if_false.with_value(if_false.value.walk(transform)))
                )
            },
            Case(condition, branches) => {
                Case(
                    Box::new(condition.with_value(condition.value.walk(transform))),
                    branches.into_iter().map(|( pattern, body )|
                        ( pattern, Box::new(body.with_value(body.value.walk(transform))) )
                    ).collect()
                )
            },
            Operator(loc_left, loc_op, loc_right) => {
                Operator(
                    Box::new(loc_left.with_value(loc_left.value.walk(transform))),
                    loc_op,
                    Box::new(loc_right.with_value(loc_right.value.walk(transform)))
                )
            }
        }
    }
}

// Precedence logic adapted from Gluon by Markus Westerlind, MIT licensed
// https://github.com/gluon-lang/gluon
#[derive(Clone, Debug, PartialEq)]
pub enum PrecedenceProblem {
    BothNonAssociative(Located<Operator>, Located<Operator>),
}

fn new_op_expr(left: Box<Located<Expr>>, op: Located<Operator>, right: Box<Located<Expr>>)
    -> Box<Located<Expr>> {
    let new_region = Region {
        start_line: left.region.start_line,
        start_col: left.region.start_col,

        end_line: right.region.end_line,
        end_col: right.region.end_col
    };
    let new_expr = Expr::Operator(left, op, right);

    Box::new(Located::new(new_expr, new_region))
}

/// Reorder the expression tree based on operator precedence and associativity rules.
/// In many languages, this can fail due to (for example) <| and |> having the same
/// precedence but different associativity. Languages which support custom operators with
/// user-defined precedence and associativity (e.g. Haskell) can have many such errors.
///
/// By design, Roc neither allows custom operators nor has any built-in operators with
/// the same precedence and different associativity, so this operation always succeeds
/// and can never produce any user-facing errors.
pub fn apply_precedence_and_associativity(expr: Located<Expr>)
    -> Result<Located<Expr>, PrecedenceProblem> {
    use self::PrecedenceProblem::*;

    let mut infixes = Infixes::new(expr);
    let mut arg_stack: Vec<Box<Located<Expr>>> = Vec::new();
    let mut op_stack: Vec<Located<Operator>> = Vec::new();

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
                                arg_stack.push(new_op_expr(left, stack_op, right));
                            }

                            Ordering::Greater => {
                                // Swap
                                op_stack.push(stack_op);
                                op_stack.push(next_op);
                            }

                            Ordering::Equal => {
                                match (next_op.value.associativity(), stack_op.value.associativity()) {
                                    ( LeftAssociative, LeftAssociative ) => {
                                        // Inline
                                        let right = arg_stack.pop().unwrap();
                                        let left = arg_stack.pop().unwrap();

                                        infixes.next_op = Some(next_op);
                                        arg_stack.push(new_op_expr(left, stack_op, right));
                                    },

                                    ( RightAssociative, RightAssociative ) => {
                                        // Swap
                                        op_stack.push(stack_op);
                                        op_stack.push(next_op);
                                    },

                                    ( NonAssociative, NonAssociative ) => {
                                        // Both operators were non-associative, e.g. (True == False == False).
                                        // We should tell the author to disambiguate by grouping them with parens.
                                        return Err(BothNonAssociative(next_op, stack_op));
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
                    },
                    None => op_stack.push(next_op)
                };
            }
        }
    }

    for op in op_stack.into_iter().rev() {
        let right = arg_stack.pop().unwrap();
        let left = arg_stack.pop().unwrap();

        arg_stack.push(new_op_expr(left, op, right));
    }

    assert_eq!(arg_stack.len(), 1);

    Ok(*arg_stack.pop().unwrap())
}

#[derive(Debug, Clone, PartialEq)]
enum InfixToken {
    Arg(Box<Located<Expr>>),
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
struct Infixes {
    /// The next part of the expression that we need to flatten
    remaining_expr: Option<Box<Located<Expr>>>,
    /// Cached operator from a previous iteration
    next_op: Option<Located<Operator>>,
}

impl Infixes {
    fn new(expr: Located<Expr>) -> Infixes {
        Infixes {
            remaining_expr: Some(Box::new(expr)),
            next_op: None,
        }
    }
}

impl Iterator for Infixes {
    type Item = InfixToken;

    fn next(&mut self) -> Option<InfixToken> {
        match self.next_op.take() {
            Some(op) => Some(InfixToken::Op(op)),
            None => {
                self.remaining_expr.take().map(|boxed_expr| {
                    let expr = *boxed_expr;

                    match expr.value {
                        Expr::Operator(left, op, right) => {
                            self.remaining_expr = Some(right);
                            self.next_op = Some(op);

                            InfixToken::Arg(left)
                        }
                        _ => InfixToken::Arg(Box::new(expr)),
                    }
                })
            }
        }
    }
}
