use std::fmt;
use self::Expr::*;
use smallvec::SmallVec;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Frac(i64, u64),
    EmptyStr,
    Str(String),
    InterpolatedStr(Vec<(String, Ident)>, String),
    Char(char),

    Var(Ident),
    Let(Pattern, Box<Expr>, Box<Expr>),

    // Functions
    Func(Ident, Vec<Expr>),
    Apply(Box<Expr>, Vec<Expr>),
    Operator(Box<Expr>, Operator, Box<Expr>),
    Closure(SmallVec<[Pattern; 2]>, Box<Expr>),

    // Sum Types
    ApplyVariant(String, Option<Vec<Expr>>),

    // Product Types
    EmptyRecord,

    // Conditionals
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Case(Box<Expr>, SmallVec<[(Pattern, Box<Expr>); 2]>),

    // Error
    Error(Problem),
}

pub type Ident = String;

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // PRIMITIVES
            Int(num) => write!(f, "{}", *num),
            Frac(numerator, denominator) => {
                if *denominator == 10 {
                    write!(f, "{}", (*numerator as f64 / 10.0))
                } else {
                    write!(f, "{}/{}", numerator, denominator)
                }
            },
            Str(string) => {
                let escaped_str =
                    (*string)
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\"")
                        .replace("\t", "\\t")
                        .replace("\n", "\\n")
                        .replace("\r", "\\r");

                write!(f, "\"{}\"", escaped_str)
            },
            Char(ch) => write!(f, "'{}'", *ch),
            Closure(args, _) => write!(f, "<{}-argument function>", args.len()),
            ApplyVariant(name, opt_exprs) => {
                match opt_exprs {
                    None => write!(f, "{}", name),
                    Some(exprs) => {
                        let contents =
                            exprs.into_iter()
                                .map(|expr| format!(" {}", expr))
                                .collect::<Vec<_>>()
                                .join(",");

                        write!(f, "{}{}", name, contents)
                    }
                }
            },

            // ERRORS
            Error(Problem::UnrecognizedVarName(name)) => write!(f, "NAMING ERROR: Unrecognized var name `{}`", name),
            Error(Problem::TypeMismatch(info)) => write!(f, "TYPE ERROR: {}", info),
            Error(Problem::ReassignedVarName(name)) => write!(f, "REASSIGNED CONSTANT: {}", name),
            Error(Problem::WrongArity(expected_arity, provided_arity)) => {
                if provided_arity > expected_arity {
                  write!(f, "TOO MANY ARGUMENTS: needed {} arguments, but got {}", expected_arity, provided_arity)
                } else {
                  write!(f, "MISSING ARGUMENTS: needed {} arguments, but got {}", expected_arity, provided_arity)
                }
            }

            // UNFORMATTED
            _ => write!(f, "<partially evaluated expression>")
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Problem {
    UnrecognizedVarName(String),
    TypeMismatch(String),
    ReassignedVarName(String),
    WrongArity(u32 /* Expected */, u32 /* Provided */),
    NoBranchesMatched,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(String),
    Variant(String, Option<Vec<Pattern>>),
    EmptyRecord,
    Underscore
}


#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus, Minus, Star, Slash, DoubleSlash, Equals,
}
