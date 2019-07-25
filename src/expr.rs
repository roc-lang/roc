use operator::Operator;
use region::Located;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Frac(i64, i64),
    Approx(f64),
    EmptyStr,
    Str(String),
    Char(char),

    // Lookups
    Var(Ident),
    CallByName(Ident, Vec<Located<Expr>>),
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

/// A variant name, possibly fully-qualified with a module name
/// e.g. (Result.Ok)
/// Parameterized on a phantom marker for whether it has been canonicalized
#[derive(Clone, Debug, PartialEq)]
pub enum VariantName {
    Unqualified(String),
    Qualified((Path, String)),
}

/// An identifier, possibly fully-qualified with a module name
/// e.g. (Http.Request from http)
/// Parameterized on a phantom marker for whether it has been canonicalized
#[derive(Clone, Debug, PartialEq)]
pub enum Ident {
    Unqualified(String),
    Qualified((Path, String)),
}

/// A path to a module, which may include the package it came from.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Path(String);

impl Path {
    pub fn new(string: String) -> Path {
        Path(string)
    }

    pub fn into_string(self) -> String {
        let Path(str) = self;

        str
    }
}

impl Into<String> for Path {
    fn into(self) -> String {
        self.into_string()
    }
}

impl From<String> for Path {
    fn from(str: String) -> Self {
        Path(str)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ident::Unqualified(name) => {
                write!(f, "{}", name)
            },
            Ident::Qualified((path, name)) => {
                write!(f, "{}.{}", path.clone().into_string(), name)
            }
        }
    }
}

impl fmt::Display for VariantName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VariantName::Unqualified(name) => {
                write!(f, "{}", name)
            },
            VariantName::Qualified((path, name)) => {
                write!(f, "{}.{}", path.clone().into_string(), name)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(String),
    Variant(Located<VariantName>, Option<Vec<Located<Pattern>>>),
    Integer(i64),
    Fraction(i64, i64),
    ExactString(String),
    EmptyRecordLiteral,
    Underscore,
}

impl Expr {
    pub fn walk<F>(self: &Expr, transform: &F) -> Expr
        where F: Fn(&Expr) -> Expr
    {
        use self::Expr::*;

        let transformed = transform(self);

        match transformed {
            Int(_) | Frac(_, _) | Approx(_) | EmptyStr | Str(_) | Char(_) | Var(_) | EmptyRecord | InterpolatedStr(_, _) => transformed,
            Assign(assignments, loc_ret) => {
                Assign(
                    assignments.into_iter().map(|(pattern, loc_expr)|
                        (pattern, loc_expr.with_value(loc_expr.value.walk(transform)))
                    ).collect(),
                    Box::new(loc_ret.with_value(loc_ret.value.walk(transform)))
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
                        ( pattern, body.with_value(body.value.walk(transform)) )
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