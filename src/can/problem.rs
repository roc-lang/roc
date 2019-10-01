use can::expr::Expr;
use can::pattern::{Pattern, PatternType};
use ident::{Ident, VariantName};
use operator::Operator;
use region::{Located, Region};

/// Problems that can occur in the course of canonicalization.
#[derive(Clone, Debug, PartialEq)]
pub enum Problem {
    Shadowing(Located<Ident>),
    UnrecognizedFunctionName(Located<Ident>),
    UnrecognizedConstant(Located<Ident>),
    UnrecognizedVariant(Located<VariantName>),
    UnusedAssignment(Located<Ident>),
    UnusedArgument(Located<Ident>),
    PrecedenceProblem(PrecedenceProblem),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(PatternType, Region),
    CircularAssignment(Vec<Located<Ident>>),
    RuntimeError(RuntimeError),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrecedenceProblem {
    BothNonAssociative(Located<Operator>, Located<Operator>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeError {
    InvalidPrecedence(PrecedenceProblem, Box<Located<Expr>>),
    UnrecognizedFunctionName(Located<Ident>),
    UnrecognizedConstant(Located<Ident>),
    UnrecognizedVariant(Located<VariantName>),
    FloatOutsideRange(Box<str>),
    IntOutsideRange(Box<str>),
    InvalidHex(std::num::ParseIntError, Box<str>),
    InvalidOctal(std::num::ParseIntError, Box<str>),
    InvalidBinary(std::num::ParseIntError, Box<str>),
    CircularAssignment(
        Vec<Located<Ident>>,
        Vec<(Located<Pattern>, Located<Expr>)>,
        Box<Located<Expr>>,
    ),

    /// When the author specifies a type annotation but no implementation
    NoImplementation,
}
