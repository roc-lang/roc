use crate::can::pattern::PatternType;
use crate::ident::{Ident, VariantName};
use crate::operator::BinOp;
use crate::region::{Located, Region};

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
    BothNonAssociative(Located<BinOp>, Located<BinOp>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeError {
    InvalidPrecedence(PrecedenceProblem, Region),
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
        Vec<(Region /* pattern */, Region /* expr */)>,
        Region,
    ),

    /// When the author specifies a type annotation but no implementation
    NoImplementation,
}
