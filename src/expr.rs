
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
    Variant(String, Option<Vec<Pattern>>),
    Integer(i64),
    Fraction(i64, i64),
    EmptyRecordLiteral,
    Underscore
}


}
