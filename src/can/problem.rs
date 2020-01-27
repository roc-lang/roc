use crate::can::ident::Ident;
use crate::can::pattern::PatternType;
use crate::operator::BinOp;
use crate::region::{Located, Region};
use inlinable_string::InlinableString;

/// Problems that can occur in the course of canonicalization.
#[derive(Clone, Debug, PartialEq)]
pub enum Problem {
    UnusedDef(Located<Ident>),
    UnusedArgument(Located<Ident>),
    PrecedenceProblem(PrecedenceProblem),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(PatternType, Region),
    CircularDef(Vec<Located<Ident>>),
    RuntimeError(RuntimeError),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrecedenceProblem {
    BothNonAssociative(Located<BinOp>, Located<BinOp>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeError {
    Shadowing {
        original_region: Region,
        shadow: Located<Ident>,
    },
    UnrecognizedFunctionName(Located<InlinableString>),
    UnrecognizedLookup(Located<InlinableString>),
    ValueNotExposed {
        module_name: InlinableString,
        ident: InlinableString,
        region: Region,
    },
    ModuleNotImported {
        module_name: InlinableString,
        ident: InlinableString,
        region: Region,
    },
    InvalidPrecedence(PrecedenceProblem, Region),
    FloatOutsideRange(Box<str>),
    IntOutsideRange(Box<str>),
    InvalidHex(std::num::ParseIntError, Box<str>),
    InvalidOctal(std::num::ParseIntError, Box<str>),
    InvalidBinary(std::num::ParseIntError, Box<str>),
    QualifiedPatternIdent(InlinableString),
    CircularDef(
        Vec<Located<Ident>>,
        Vec<(Region /* pattern */, Region /* expr */)>,
    ),

    /// When the author specifies a type annotation but no implementation
    NoImplementation,
}
