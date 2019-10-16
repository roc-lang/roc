use bumpalo::collections::Vec;
use can::expr::Expr;
use can::pattern::{Pattern, PatternType};
use ident::{Ident, VariantName};
use operator::Operator;
use region::{Located, Region};

/// Problems that can occur in the course of canonicalization.
#[derive(Clone, Debug, PartialEq)]
pub enum Problem<'a> {
    Shadowing(Located<Ident>),
    UnrecognizedFunctionName(Located<Ident>),
    UnrecognizedConstant(Located<Ident>),
    UnrecognizedVariant(Located<VariantName>),
    UnusedAssignment(Located<Ident>),
    UnusedArgument(Located<Ident>),
    PrecedenceProblem(PrecedenceProblem),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(PatternType, Region),
    CircularAssignment(Vec<'a, Located<Ident>>),
    RuntimeError(RuntimeError<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrecedenceProblem {
    BothNonAssociative(Located<Operator>, Located<Operator>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeError<'a> {
    InvalidPrecedence(PrecedenceProblem, &'a Located<Expr<'a>>),
    UnrecognizedFunctionName(Located<Ident>),
    UnrecognizedConstant(Located<Ident>),
    UnrecognizedVariant(Located<VariantName>),
    FloatOutsideRange(&'a str),
    IntOutsideRange(&'a str),
    InvalidHex(std::num::ParseIntError, &'a str),
    InvalidOctal(std::num::ParseIntError, &'a str),
    InvalidBinary(std::num::ParseIntError, &'a str),
    CircularAssignment(
        Vec<'a, Located<Ident>>,
        Vec<'a, (Located<Pattern<'a>>, Located<Expr<'a>>)>,
        &'a Located<Expr<'a>>,
    ),

    /// When the author specifies a type annotation but no implementation
    NoImplementation,
}
