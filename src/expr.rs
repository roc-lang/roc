use std::fmt;
use self::Expr::*;
use im_rc::vector::Vector;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Frac(i64, u64),
    Str(String),
    Char(char),
    Bool(bool),

    Var(String),
    Let(Pattern, Box<Expr>, Box<Expr>),

    // Functions
    Func(String, Vector<Expr>),
    Apply(Box<Expr>, Vector<Expr>),
    Operator(Box<Expr>, Operator, Box<Expr>),
    Closure(Vector<Pattern>, Box<Expr>),

    // Sum Types
    ApplyVariant(String, Option<Vector<Expr>>),

    // Conditionals
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vector<(Pattern, Box<Expr>)>),

    // Error
    Error(Problem),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // PRIMITIVES
            Int(num) => write!(f, "{} : Int", *num),
            Frac(numerator, denominator) => {
                if *denominator == 10 {
                    write!(f, "{} : Frac", (*numerator as f64 / 10.0))
                } else {
                    write!(f, "{}/{} : Frac", numerator, denominator)
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

                write!(f, "\"{}\" : String", escaped_str)
            },
            Char(ch) => write!(f, "'{}' : Char", *ch),
            Bool(true) => write!(f, "True : Bool"),
            Bool(false) => write!(f, "False : Bool"),
            Closure(args, _) => write!(f, "<{}-argument function>", args.len()),

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
    Variant(String, Option<Vector<Pattern>>),
    Underscore
}


#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus, Minus, Star, Slash, DoubleSlash, Equals,
}
