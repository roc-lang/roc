use inlinable_string::InlinableString;
use roc_collections::all::MutSet;
use roc_module::ident::{Ident, Lowercase, TagName};
use roc_module::operator::BinOp;
use roc_module::symbol::{ModuleId, Symbol};
use roc_parse::ast::Base;
use roc_parse::pattern::PatternType;
use roc_region::all::{Located, Region};

/// Problems that can occur in the course of canonicalization.
#[derive(Clone, Debug, PartialEq)]
pub enum Problem {
    UnusedDef(Symbol, Region),
    UnusedImport(ModuleId, Region),
    /// First symbol is the name of the closure with that argument
    /// Second symbol is the name of the argument that is unused
    UnusedArgument(Symbol, Symbol, Region),
    PrecedenceProblem(PrecedenceProblem),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(PatternType, Region),
    ShadowingInAnnotation {
        original_region: Region,
        shadow: Located<Ident>,
    },
    CyclicAlias(Symbol, Region, Vec<Symbol>),
    PhantomTypeArgument {
        alias: Symbol,
        variable_region: Region,
        variable_name: Lowercase,
    },
    DuplicateRecordFieldValue {
        field_name: Lowercase,
        record_region: Region,
        field_region: Region,
        replaced_region: Region,
    },
    DuplicateRecordFieldType {
        field_name: Lowercase,
        record_region: Region,
        field_region: Region,
        replaced_region: Region,
    },
    DuplicateTag {
        tag_name: TagName,
        tag_union_region: Region,
        tag_region: Region,
        replaced_region: Region,
    },
    RuntimeError(RuntimeError),
    SignatureDefMismatch {
        annotation_pattern: Region,
        def_pattern: Region,
    },
    InvalidAliasRigid {
        alias_name: Symbol,
        region: Region,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrecedenceProblem {
    BothNonAssociative(Region, Located<BinOp>, Located<BinOp>),
}

/// Enum to store the various types of errors that can cause parsing an integer to fail.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntErrorKind {
    /// Value being parsed is empty.
    ///
    /// Among other causes, this variant will be constructed when parsing an empty string.
    Empty,
    /// Contains an invalid digit.
    ///
    /// Among other causes, this variant will be constructed when parsing a string that
    /// contains a letter.
    InvalidDigit,
    /// Integer is too large to store in target integer type.
    Overflow,
    /// Integer is too small to store in target integer type.
    Underflow,
}

/// Enum to store the various types of errors that can cause parsing a float to fail.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FloatErrorKind {
    /// Probably an invalid digit
    Error,
    /// the literal is too small for f64
    NegativeInfinity,
    /// the literal is too large for f64
    PositiveInfinity,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeError {
    Shadowing {
        original_region: Region,
        shadow: Located<Ident>,
    },
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
    // Example: when 1 is 1.X -> 32
    MalformedPattern(MalformedPatternProblem, Region),
    LookupNotInScope(Located<InlinableString>, MutSet<Box<str>>),
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
    MalformedIdentifier(Box<str>, Region),
    MalformedClosure(Region),
    InvalidFloat(FloatErrorKind, Region, Box<str>),
    InvalidInt(IntErrorKind, Base, Region, Box<str>),
    CircularDef(Vec<Symbol>, Vec<(Region /* pattern */, Region /* expr */)>),

    /// When the author specifies a type annotation but no implementation
    NoImplementation,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MalformedPatternProblem {
    MalformedInt,
    MalformedFloat,
    MalformedBase(Base),
    Unknown,
    QualifiedIdentifier,
}
