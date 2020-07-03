use inlinable_string::InlinableString;
use roc_collections::all::MutSet;
use roc_module::ident::{Ident, Lowercase, TagName};
use roc_module::operator::BinOp;
use roc_module::symbol::{ModuleId, Symbol};
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

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeError {
    Shadowing {
        original_region: Region,
        shadow: Located<Ident>,
    },
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
    UnrecognizedFunctionName(Located<InlinableString>),
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
    FloatOutsideRange(Box<str>),
    IntOutsideRange(Box<str>),
    InvalidHex(std::num::ParseIntError, Box<str>),
    InvalidOctal(std::num::ParseIntError, Box<str>),
    InvalidBinary(std::num::ParseIntError, Box<str>),
    QualifiedPatternIdent(InlinableString),
    CircularDef(Vec<Symbol>, Vec<(Region /* pattern */, Region /* expr */)>),

    /// When the author specifies a type annotation but no implementation
    NoImplementation,
}
