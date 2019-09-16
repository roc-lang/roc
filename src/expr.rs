use operator::Operator;
use region::Located;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Float(f64),
    EmptyStr,
    Str(String),
    Char(char),
    List(Vec<Located<Expr>>),
    EmptyList,

    // Lookups
    Var(Ident),
    InterpolatedStr(Vec<(String, Located<Ident>)>, String),

    // Pattern Matching
    Case(Box<Located<Expr>>, Vec<(Located<Pattern>, Located<Expr>)>),
    Closure(Vec<Located<Pattern>>, Box<Located<Expr>>),
    Assign(Vec<(Located<Pattern>, Located<Expr>)>, Box<Located<Expr>>),

    // Application
    Apply(Box<Located<Expr>>, Vec<Located<Expr>>),
    ApplyVariant(VariantName, Option<Vec<Located<Expr>>>),

    // Product Types
    EmptyRecord,

    // Sugar
    If(Box<Located<Expr>>, Box<Located<Expr>>, Box<Located<Expr>>),
    Operator(Box<Located<Expr>>, Located<Operator>, Box<Located<Expr>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(String),
    Variant(Located<VariantName>, Option<Vec<Located<Pattern>>>),
    IntLiteral(i64),
    FloatLiteral(f64),
    ExactString(String),
    EmptyRecordLiteral,
    Underscore,
}

impl Expr {
    pub fn walk<F>(self: &Expr, transform: &F) -> Expr
    where
        F: Fn(&Expr) -> Expr,
    {
        use self::Expr::*;

        let transformed = transform(self);

        match transformed {
            Int(_)
            | Float(_)
            | EmptyStr
            | Str(_)
            | Char(_)
            | Var(_)
            | EmptyRecord
            | InterpolatedStr(_, _)
            | EmptyList => transformed,
            List(elems) => {
                let new_elems = elems
                    .into_iter()
                    .map(|loc_elem| loc_elem.with_value(loc_elem.value.walk(transform)))
                    .collect();

                List(new_elems)
            }
            Assign(assignments, loc_ret) => Assign(
                assignments
                    .into_iter()
                    .map(|(pattern, loc_expr)| {
                        (pattern, loc_expr.with_value(loc_expr.value.walk(transform)))
                    })
                    .collect(),
                Box::new(loc_ret.with_value(loc_ret.value.walk(transform))),
            ),
            Apply(fn_expr, args) => Apply(
                Box::new(fn_expr.with_value(fn_expr.value.walk(transform))),
                args.into_iter()
                    .map(|arg| arg.with_value(arg.value.walk(transform)))
                    .collect(),
            ),
            Closure(patterns, body) => Closure(
                patterns,
                Box::new(body.with_value(body.value.walk(transform))),
            ),
            ApplyVariant(_, None) => transformed,
            ApplyVariant(name, Some(args)) => ApplyVariant(
                name,
                Some(
                    args.into_iter()
                        .map(|arg| arg.with_value(arg.value.walk(transform)))
                        .collect(),
                ),
            ),
            If(condition, if_true, if_false) => If(
                Box::new(condition.with_value(condition.value.walk(transform))),
                Box::new(if_true.with_value(if_true.value.walk(transform))),
                Box::new(if_false.with_value(if_false.value.walk(transform))),
            ),
            Case(condition, branches) => Case(
                Box::new(condition.with_value(condition.value.walk(transform))),
                branches
                    .into_iter()
                    .map(|(pattern, body)| (pattern, body.with_value(body.value.walk(transform))))
                    .collect(),
            ),
            Operator(loc_left, loc_op, loc_right) => Operator(
                Box::new(loc_left.with_value(loc_left.value.walk(transform))),
                loc_op,
                Box::new(loc_right.with_value(loc_right.value.walk(transform))),
            ),
        }
    }
}
