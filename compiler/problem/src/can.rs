use roc_collections::all::MutSet;
use roc_module::called_via::BinOp;
use roc_module::ident::{Ident, Lowercase, ModuleName, TagName};
use roc_module::symbol::{ModuleId, Symbol};
use roc_parse::ast::Base;
use roc_parse::pattern::PatternType;
use roc_region::all::{Loc, Region};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CycleEntry {
    pub symbol: Symbol,
    pub symbol_region: Region,
    pub expr_region: Region,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BadPattern {
    UnderscoreInDef,
    Unsupported(PatternType),
}

/// Problems that can occur in the course of canonicalization.
#[derive(Clone, Debug, PartialEq)]
pub enum Problem {
    UnusedDef(Symbol, Region),
    UnusedImport(ModuleId, Region),
    ExposedButNotDefined(Symbol),
    UnknownGeneratesWith(Loc<Ident>),
    /// First symbol is the name of the closure with that argument
    /// Second symbol is the name of the argument that is unused
    UnusedArgument(Symbol, Symbol, Region),
    PrecedenceProblem(PrecedenceProblem),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(BadPattern, Region),
    ShadowingInAnnotation {
        original_region: Region,
        shadow: Loc<Ident>,
    },
    CyclicAlias(Symbol, Region, Vec<Symbol>),
    BadRecursion(Vec<CycleEntry>),
    PhantomTypeArgument {
        typ: Symbol,
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
    InvalidOptionalValue {
        field_name: Lowercase,
        record_region: Region,
        field_region: Region,
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
    InvalidInterpolation(Region),
    InvalidHexadecimal(Region),
    InvalidUnicodeCodePt(Region),
    NestedDatatype {
        alias: Symbol,
        def_region: Region,
        differing_recursion_region: Region,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrecedenceProblem {
    BothNonAssociative(Region, Loc<BinOp>, Loc<BinOp>),
}

/// Enum to store the various types of errors that can cause parsing an integer to fail.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntErrorKind {
    /// Value being parsed is empty.
    ///
    /// Among other causes, this variant will be constructed when parsing an empty string.
    /// In roc, this can happen with non-base-10 literals, e.g. `0x` or `0b` without any digits
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
    /// This is an integer, but it has a float numeric suffix.
    FloatSuffix,
    /// The integer literal overflows the width of the suffix associated with it.
    OverflowsSuffix {
        suffix_type: &'static str,
        max_value: u128,
    },
    /// The integer literal underflows the width of the suffix associated with it.
    UnderflowsSuffix {
        suffix_type: &'static str,
        min_value: i128,
    },
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
    /// This is a float, but it has an integer numeric suffix.
    IntSuffix,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeError {
    Shadowing {
        original_region: Region,
        shadow: Loc<Ident>,
    },
    InvalidOptionalValue {
        field_name: Lowercase,
        record_region: Region,
        field_region: Region,
    },
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
    // Example: when 1 is 1.X -> 32
    MalformedPattern(MalformedPatternProblem, Region),

    UnresolvedTypeVar,
    ErroneousType,

    LookupNotInScope(Loc<Ident>, MutSet<Box<str>>),
    OpaqueNotDefined {
        usage: Loc<Ident>,
        opaques_in_scope: MutSet<Box<str>>,
        opt_defined_alias: Option<Region>,
    },
    OpaqueOutsideScope {
        opaque: Ident,
        referenced_region: Region,
        imported_region: Region,
    },
    ValueNotExposed {
        module_name: ModuleName,
        ident: Ident,
        region: Region,
        exposed_values: Vec<Lowercase>,
    },
    ModuleNotImported {
        module_name: ModuleName,
        imported_modules: MutSet<Box<str>>,
        region: Region,
    },
    InvalidPrecedence(PrecedenceProblem, Region),
    MalformedIdentifier(Box<str>, roc_parse::ident::BadIdent, Region),
    MalformedTypeName(Box<str>, Region),
    MalformedClosure(Region),
    InvalidRecordUpdate {
        region: Region,
    },
    InvalidFloat(FloatErrorKind, Region, Box<str>),
    InvalidInt(IntErrorKind, Base, Region, Box<str>),
    CircularDef(Vec<CycleEntry>),

    NonExhaustivePattern,

    InvalidInterpolation(Region),
    InvalidHexadecimal(Region),
    InvalidUnicodeCodePt(Region),

    /// When the author specifies a type annotation but no implementation
    NoImplementationNamed {
        def_symbol: Symbol,
    },
    NoImplementation,

    /// cases where the `[]` value (or equivalently, `forall a. a`) pops up
    VoidValue,

    ExposedButNotDefined(Symbol),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MalformedPatternProblem {
    MalformedInt,
    MalformedFloat,
    MalformedBase(Base),
    Unknown,
    QualifiedIdentifier,
    BadIdent(roc_parse::ident::BadIdent),
}
