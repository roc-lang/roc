
use operator::Operator;
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
