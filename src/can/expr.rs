use can::pattern::Pattern;
use can::problem::RuntimeError;
use can::symbol::Symbol;
use operator::CalledVia;
use region::Located;
use std::i64;
use subs::Variable;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Float(f64),
    Str(Box<str>),
    List(Variable, Vec<Located<Expr>>),

    // Lookups
    Var(Variable, Symbol),
    /// Works the same as Var, but has an important marking purpose.
    /// See 13623e3f5f65ea2d703cf155f16650c1e8246502 for the bug this fixed.
    FunctionPointer(Variable, Symbol),

    // Look up a field on a record, e.g. (expr).foo
    Field(Box<Located<Expr>>, Box<str>),

    // Pattern Matching
    Case(
        Variable,
        Box<Located<Expr>>,
        Vec<(Located<Pattern>, Located<Expr>)>,
    ),
    Defs(
        Variable,
        Vec<(Located<Pattern>, Located<Expr>)>,
        Box<Located<Expr>>,
    ),

    CallByName(Symbol, Vec<Located<Expr>>, CalledVia),
    CallPointer(Box<Expr>, Vec<Located<Expr>>, CalledVia),

    // Product Types
    Record(Variable, Vec<Located<(Box<str>, Located<Expr>)>>),
    EmptyRecord,

    // Compiles, but will crash if reached
    RuntimeError(RuntimeError),
}
