use can::pattern::Pattern;
use can::problem::RuntimeError;
use can::symbol::Symbol;
use operator::Operator;
use region::Located;
use std::i64;
use subs::Variable;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    // Literals
    Int(Variable, i64),
    Float(Variable, f64),
    Str(Variable, &'a str),
    List(Variable, &'a [Located<Expr<'a>>]),

    // Lookups
    Var(Variable, Symbol<'a>),
    /// Works the same as Var, but has an important marking purpose.
    /// See 13623e3f5f65ea2d703cf155f16650c1e8246502 for the bug this fixed.
    FunctionPointer(Variable, Symbol<'a>),

    // Pattern Matching
    Case(
        Variable,
        &'a Located<Expr<'a>>,
        &'a [(Located<Pattern<'a>>, Located<Expr<'a>>)],
    ),
    Define(
        Variable,
        &'a [(Located<Pattern<'a>>, Located<Expr<'a>>)],
        &'a Located<Expr<'a>>,
    ),

    // Application
    Call(Variable, &'a Located<Expr<'a>>, &'a [Located<Expr<'a>>]),

    // This has to be separate from Call so we can do precedence reordering
    Operator(
        Variable,
        &'a (Located<Expr<'a>>, Located<Operator>, Located<Expr<'a>>),
    ),

    // Product Types
    Record(Variable, &'a [Located<(&'a str, Located<Expr<'a>>)>]),

    // Compiles, but will crash if reached
    RuntimeError(Variable, RuntimeError<'a>),
}
