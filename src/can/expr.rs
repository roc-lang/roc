use can::pattern::Pattern;
use can::problem::RuntimeError;
use can::symbol::Symbol;
use operator::Operator;
use region::Located;
use std::i64;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Float(f64),
    EmptyStr,
    Str(Box<str>),
    Char(char), // OBSOLETE
    List(Vec<Located<Expr>>),
    EmptyList,

    // Lookups
    Var(Symbol),
    /// Works the same as Var, but has an important marking purpose.
    /// See 13623e3f5f65ea2d703cf155f16650c1e8246502 for the bug this fixed.
    FunctionPointer(Symbol),
    /// We have a separate variant for this so that we can report errors
    /// (including type errors later) in the context of the sugar rather than
    /// confusingly talking about the desugared version the user can't see.
    InterpolatedStr(Vec<(Box<str>, Located<Expr>)>, Box<str>),

    // Pattern Matching
    Case(Box<Located<Expr>>, Vec<(Located<Pattern>, Located<Expr>)>),
    Assign(Vec<(Located<Pattern>, Located<Expr>)>, Box<Located<Expr>>),

    // Application
    Call(Box<Located<Expr>>, Vec<Located<Expr>>),
    ApplyVariant(Symbol, Option<Vec<Located<Expr>>>),

    // Product Types
    EmptyRecord,

    // Sugar
    If(Box<Located<Expr>>, Box<Located<Expr>>, Box<Located<Expr>>),
    Operator(Box<Located<Expr>>, Located<Operator>, Box<Located<Expr>>),

    // Compiles, but will crash if reached
    RuntimeError(RuntimeError),
}
