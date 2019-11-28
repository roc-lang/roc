use crate::can::pattern::Pattern;
use crate::can::problem::RuntimeError;
use crate::can::symbol::Symbol;
use crate::operator::CalledVia;
use crate::region::Located;
use crate::subs::Variable;
use std::i64;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Float(f64),
    Str(Box<str>),
    BlockStr(Box<str>),
    List(Variable, Vec<Located<Expr>>),

    // Lookups
    Var(Variable, Symbol),
    /// Works the same as Var, but has an important marking purpose.
    /// See 13623e3f5f65ea2d703cf155f16650c1e8246502 for the bug this fixed.
    FunctionPointer(Variable, Symbol),

    /// Look up exactly one field on a record, e.g. (expr).foo.
    /// Canonicalization will convert chains to single-access, e.g. foo.bar.baz to (foo.bar).baz.
    Field(Box<Located<Expr>>, Box<str>),

    // Pattern Matching
    /// Case is guaranteed to be exhaustive at this point. (If it wasn't, then
    /// a _ branch was added at the end that will throw a runtime error.)
    /// Also, `If` is desugared into `Case` matching on `False` and `_` at this point.
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

    Call(Box<Expr>, Vec<Located<Expr>>, CalledVia),

    // Product Types
    Record(Variable, Vec<Located<(Box<str>, Located<Expr>)>>),
    EmptyRecord,

    // Compiles, but will crash if reached
    RuntimeError(RuntimeError),
}
