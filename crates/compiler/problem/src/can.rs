use roc_collections::all::MutSet;
use roc_module::called_via::BinOp;
use roc_module::ident::{Ident, Lowercase, ModuleName, TagName};
use roc_module::symbol::{ModuleId, Symbol};
use roc_parse::ast::Base;
use roc_parse::pattern::PatternType;
use roc_region::all::{Loc, Region};
use roc_types::types::AliasKind;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CycleEntry {
    pub symbol: Symbol,
    pub symbol_region: Region,
    pub expr_region: Region,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BadPattern {
    Unsupported(PatternType),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ShadowKind {
    Variable,
    Alias(Symbol),
    Opaque(Symbol),
    Ability(Symbol),
}

/// Problems that can occur in the course of canonicalization.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Problem {
    UnusedDef(Symbol, Region),
    UnusedImport(Symbol, Region),
    UnusedModuleImport(ModuleId, Region),
    ExposedButNotDefined(Symbol),
    UnknownGeneratesWith(Loc<Ident>),
    /// First symbol is the name of the closure with that argument
    /// Bool is whether the closure is anonymous
    /// Second symbol is the name of the argument that is unused
    UnusedArgument(Symbol, bool, Symbol, Region),
    UnusedBranchDef(Symbol, Region),
    PrecedenceProblem(PrecedenceProblem),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(BadPattern, Region),
    Shadowing {
        original_region: Region,
        shadow: Loc<Ident>,
        kind: ShadowKind,
    },
    CyclicAlias(Symbol, Region, Vec<Symbol>, AliasKind),
    BadRecursion(Vec<CycleEntry>),
    PhantomTypeArgument {
        typ: Symbol,
        variable_region: Region,
        variable_name: Lowercase,
        alias_kind: AliasKind,
    },
    UnboundTypeVariable {
        typ: Symbol,
        num_unbound: usize,
        one_occurrence: Region,
        kind: AliasKind,
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
    InvalidExtensionType {
        region: Region,
        kind: ExtensionTypeKind,
    },
    AbilityHasTypeVariables {
        name: Symbol,
        variables_region: Region,
    },
    HasClauseIsNotAbility {
        region: Region,
    },
    IllegalHasClause {
        region: Region,
    },
    DuplicateHasAbility {
        ability: Symbol,
        region: Region,
    },
    AbilityMemberMissingHasClause {
        member: Symbol,
        ability: Symbol,
        region: Region,
    },
    AbilityMemberMultipleBoundVars {
        member: Symbol,
        ability: Symbol,
        span_has_clauses: Region,
        bound_var_names: Vec<Lowercase>,
    },
    AbilityNotOnToplevel {
        region: Region,
    },
    AbilityUsedAsType(Lowercase, Symbol, Region),
    NestedSpecialization(Symbol, Region),
    IllegalDerivedAbility(Region),
    ImplementationNotFound {
        member: Symbol,
        region: Region,
    },
    NotAnAbilityMember {
        ability: Symbol,
        name: String,
        region: Region,
    },
    OptionalAbilityImpl {
        ability: Symbol,
        region: Region,
    },
    QualifiedAbilityImpl {
        region: Region,
    },
    AbilityImplNotIdent {
        region: Region,
    },
    DuplicateImpl {
        original: Region,
        duplicate: Region,
    },
    NotAnAbility(Region),
    ImplementsNonRequired {
        region: Region,
        ability: Symbol,
        not_required: Vec<Symbol>,
    },
    DoesNotImplementAbility {
        region: Region,
        ability: Symbol,
        not_implemented: Vec<Symbol>,
    },
    NotBoundInAllPatterns {
        unbound_symbol: Symbol,
        region: Region,
    },
    NoIdentifiersIntroduced(Region),
    OverloadedSpecialization {
        overload: Region,
        original_opaque: Symbol,
        ability_member: Symbol,
    },
    UnnecessaryOutputWildcard {
        region: Region,
    },
    MultipleListRestPattern {
        region: Region,
    },
    BadTypeArguments {
        symbol: Symbol,
        region: Region,
        alias_needs: u8,
        type_got: u8,
        alias_kind: AliasKind,
    },
    UnappliedCrash {
        region: Region,
    },
    OverAppliedCrash {
        region: Region,
    },
}

impl Problem {
    /// Returns a Region value from the Problem, if possible.
    /// Some problems have more than one region; in those cases,
    /// this tries to pick the one that's closest to the original
    /// definition site, since that's what the REPL uses this for:
    /// filtering out errors and warnings from wrapped defs based
    /// on their Region being outside the expression currently being evaluated.
    pub fn region(&self) -> Option<Region> {
        match self {
            Problem::UnusedDef(_, region)
            | Problem::Shadowing {
                original_region: region,
                ..
            }
            | Problem::UnusedImport(_, region)
            | Problem::UnusedModuleImport(_, region)
            | Problem::UnknownGeneratesWith(Loc { region, .. })
            | Problem::UnusedArgument(_, _, _, region)
            | Problem::UnusedBranchDef(_, region)
            | Problem::PrecedenceProblem(PrecedenceProblem::BothNonAssociative(region, _, _))
            | Problem::UnsupportedPattern(_, region)
            | Problem::CyclicAlias(_, region, _, _)
            | Problem::PhantomTypeArgument {
                variable_region: region,
                ..
            }
            | Problem::UnboundTypeVariable {
                one_occurrence: region,
                ..
            }
            | Problem::DuplicateRecordFieldValue {
                record_region: region,
                ..
            }
            | Problem::DuplicateRecordFieldType {
                record_region: region,
                ..
            }
            | Problem::InvalidOptionalValue {
                record_region: region,
                ..
            }
            | Problem::DuplicateTag {
                tag_union_region: region,
                ..
            }
            | Problem::RuntimeError(RuntimeError::Shadowing {
                original_region: region,
                ..
            })
            | Problem::RuntimeError(RuntimeError::InvalidOptionalValue {
                record_region: region,
                ..
            })
            | Problem::RuntimeError(RuntimeError::UnsupportedPattern(region))
            | Problem::RuntimeError(RuntimeError::MalformedPattern(_, region))
            | Problem::RuntimeError(RuntimeError::LookupNotInScope(Loc { region, .. }, _))
            | Problem::RuntimeError(RuntimeError::OpaqueNotDefined {
                usage: Loc { region, .. },
                ..
            })
            | Problem::RuntimeError(RuntimeError::OpaqueOutsideScope {
                referenced_region: region,
                ..
            })
            | Problem::RuntimeError(RuntimeError::OpaqueNotApplied(Loc { region, .. }))
            | Problem::RuntimeError(RuntimeError::OpaqueAppliedToMultipleArgs(region))
            | Problem::RuntimeError(RuntimeError::ValueNotExposed { region, .. })
            | Problem::RuntimeError(RuntimeError::ModuleNotImported { region, .. })
            | Problem::RuntimeError(RuntimeError::InvalidPrecedence(_, region))
            | Problem::RuntimeError(RuntimeError::MalformedIdentifier(_, _, region))
            | Problem::RuntimeError(RuntimeError::MalformedTypeName(_, region))
            | Problem::RuntimeError(RuntimeError::MalformedClosure(region))
            | Problem::RuntimeError(RuntimeError::InvalidRecordUpdate { region })
            | Problem::RuntimeError(RuntimeError::InvalidFloat(_, region, _))
            | Problem::RuntimeError(RuntimeError::InvalidInt(_, _, region, _))
            | Problem::RuntimeError(RuntimeError::InvalidInterpolation(region))
            | Problem::RuntimeError(RuntimeError::InvalidHexadecimal(region))
            | Problem::RuntimeError(RuntimeError::InvalidUnicodeCodePt(region))
            | Problem::RuntimeError(RuntimeError::EmptySingleQuote(region))
            | Problem::RuntimeError(RuntimeError::MultipleCharsInSingleQuote(region))
            | Problem::RuntimeError(RuntimeError::DegenerateBranch(region))
            | Problem::InvalidAliasRigid { region, .. }
            | Problem::InvalidInterpolation(region)
            | Problem::InvalidHexadecimal(region)
            | Problem::InvalidUnicodeCodePt(region)
            | Problem::NestedDatatype {
                def_region: region, ..
            }
            | Problem::InvalidExtensionType { region, .. }
            | Problem::AbilityHasTypeVariables {
                variables_region: region,
                ..
            }
            | Problem::HasClauseIsNotAbility { region }
            | Problem::IllegalHasClause { region }
            | Problem::DuplicateHasAbility { region, .. }
            | Problem::AbilityMemberMissingHasClause { region, .. }
            | Problem::AbilityMemberMultipleBoundVars {
                span_has_clauses: region,
                ..
            }
            | Problem::AbilityNotOnToplevel { region }
            | Problem::AbilityUsedAsType(_, _, region)
            | Problem::NestedSpecialization(_, region)
            | Problem::IllegalDerivedAbility(region)
            | Problem::ImplementationNotFound { region, .. }
            | Problem::NotAnAbilityMember { region, .. }
            | Problem::OptionalAbilityImpl { region, .. }
            | Problem::QualifiedAbilityImpl { region }
            | Problem::AbilityImplNotIdent { region }
            | Problem::DuplicateImpl {
                original: region, ..
            }
            | Problem::NotAnAbility(region)
            | Problem::ImplementsNonRequired { region, .. }
            | Problem::DoesNotImplementAbility { region, .. }
            | Problem::NoIdentifiersIntroduced(region)
            | Problem::OverloadedSpecialization {
                overload: region, ..
            }
            | Problem::NotBoundInAllPatterns { region, .. }
            | Problem::SignatureDefMismatch {
                def_pattern: region,
                ..
            }
            | Problem::MultipleListRestPattern { region }
            | Problem::BadTypeArguments { region, .. }
            | Problem::UnnecessaryOutputWildcard { region }
            | Problem::OverAppliedCrash { region }
            | Problem::UnappliedCrash { region } => Some(*region),
            Problem::RuntimeError(RuntimeError::CircularDef(cycle_entries))
            | Problem::BadRecursion(cycle_entries) => {
                cycle_entries.first().map(|entry| entry.expr_region)
            }
            Problem::RuntimeError(RuntimeError::UnresolvedTypeVar)
            | Problem::RuntimeError(RuntimeError::ErroneousType)
            | Problem::RuntimeError(RuntimeError::NonExhaustivePattern)
            | Problem::RuntimeError(RuntimeError::NoImplementation)
            | Problem::RuntimeError(RuntimeError::VoidValue)
            | Problem::RuntimeError(RuntimeError::ExposedButNotDefined(_))
            | Problem::RuntimeError(RuntimeError::NoImplementationNamed { .. })
            | Problem::ExposedButNotDefined(_) => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExtensionTypeKind {
    Record,
    TagUnion,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RuntimeError {
    Shadowing {
        original_region: Region,
        shadow: Loc<Ident>,
        kind: ShadowKind,
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
    OpaqueNotApplied(Loc<Ident>),
    OpaqueAppliedToMultipleArgs(Region),
    ValueNotExposed {
        module_name: ModuleName,
        ident: Ident,
        region: Region,
        exposed_values: Vec<Lowercase>,
    },
    /// A module was referenced, but hasn't been imported anywhere in the program
    ///
    /// An example would be:
    /// ```roc
    /// app "hello"
    ///     packages { pf: "platform/main.roc" }
    ///     imports [pf.Stdout]
    ///     provides [main] to pf
    ///
    /// main : Task.Task {} [] // Task isn't imported!
    /// main = Stdout.line "I'm a Roc application!"
    /// ```
    ModuleNotImported {
        /// The name of the module that was referenced
        module_name: ModuleName,
        /// A list of modules which *have* been imported
        imported_modules: MutSet<Box<str>>,
        /// Where the problem occurred
        region: Region,
        /// Whether or not the module exists at all
        ///
        /// This is used to suggest that the user import the module, as opposed to fix a
        /// typo in the spelling.  For example, if the user typed `Task`, and the platform
        /// exposes a `Task` module that hasn't been imported, we can sugguest that they
        /// add the import statement.
        ///
        /// On the other hand, if the user typed `Tesk`, they might want to check their
        /// spelling.
        ///
        /// If unsure, this should be set to `false`
        module_exists: bool,
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

    /// where ''
    EmptySingleQuote(Region),
    /// where 'aa'
    MultipleCharsInSingleQuote(Region),

    DegenerateBranch(Region),
}

impl RuntimeError {
    pub fn runtime_message(self) -> String {
        use RuntimeError::*;

        match self {
            DegenerateBranch(region) => {
                format!(
                    "Hit a branch pattern that does not bind all symbols its body needs, at {:?}",
                    region
                )
            }
            err => format!("{:?}", err),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MalformedPatternProblem {
    MalformedInt,
    MalformedFloat,
    MalformedBase(Base),
    Unknown,
    QualifiedIdentifier,
    BadIdent(roc_parse::ident::BadIdent),
    EmptySingleQuote,
    MultipleCharsInSingleQuote,
    DuplicateListRestPattern,
}
