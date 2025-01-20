use std::io;
use std::path::PathBuf;

use roc_collections::all::MutSet;
use roc_module::called_via::BinOp;
use roc_module::ident::{Ident, Lowercase, ModuleName, TagName};
use roc_module::symbol::{ModuleId, Symbol};
use roc_parse::ast::Base;
use roc_parse::pattern::PatternType;
use roc_region::all::{Loc, Region};
use roc_types::types::{AliasKind, EarlyReturnKind};

use crate::Severity;

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
    ImportNameConflict {
        name: ModuleName,
        is_alias: bool,
        new_module_id: ModuleId,
        new_import_region: Region,
        existing_import: ScopeModuleSource,
    },
    ExplicitBuiltinImport(ModuleId, Region),
    ExplicitBuiltinTypeImport(Symbol, Region),
    ImportShadowsSymbol {
        region: Region,
        new_symbol: Symbol,
        existing_symbol_region: Region,
    },
    /// First symbol is the name of the closure with that argument
    /// Bool is whether the closure is anonymous
    /// Second symbol is the name of the argument that is unused
    UnusedArgument(Symbol, bool, Symbol, Region),
    UnusedBranchDef(Symbol, Region),
    DefsOnlyUsedInRecursion(usize, Region),
    PrecedenceProblem(PrecedenceProblem),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(BadPattern, Region),
    MultipleSpreadsInPattern {
        record_region: Region,
        first_spread: Region,
        shadow_spread: Region,
    },
    IgnoredFieldInType {
        record_region: Region,
        field_region: Region,
    },
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
    UndeclaredTypeVar {
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
    ImplementsClauseIsNotAbility {
        region: Region,
    },
    IllegalImplementsClause {
        region: Region,
    },
    DuplicateImplementsAbility {
        ability: Symbol,
        region: Region,
    },
    AbilityMemberMissingImplementsClause {
        member: Symbol,
        ability: Symbol,
        region: Region,
    },
    AbilityMemberMultipleBoundVars {
        member: Symbol,
        ability: Symbol,
        span_implements_clauses: Region,
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
    QualifiedAbilityImpl {
        region: Region,
    },
    AbilityImplNotIdent {
        region: Region,
    },
    IgnoredFieldForAbilityImpl(Region),
    DefaultValueFieldForAbilityImpl(Region),
    SpreadUsedInAbilityImpl(Region),
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
    UnappliedDbg {
        region: Region,
    },
    OverAppliedDbg {
        region: Region,
    },
    UnderAppliedTry {
        region: Region,
    },
    FileProblem {
        filename: PathBuf,
        error: io::ErrorKind,
    },
    WildcardNotAllowed {
        typ: Symbol,
        num_wildcards: usize,
        one_occurrence: Region,
        kind: AliasKind,
    },
    UnderscoreNotAllowed {
        typ: Symbol,
        num_underscores: usize,
        one_occurrence: Region,
        kind: AliasKind,
    },
    ReturnOutsideOfFunction {
        region: Region,
        return_kind: EarlyReturnKind,
    },
    StatementsAfterReturn {
        region: Region,
    },
    ReturnAtEndOfFunction {
        region: Region,
    },
    UnsuffixedEffectfulRecordField(Region),
    SuffixedPureRecordField(Region),
    EmptyTupleType(Region),
    UnboundTypeVarsInAs(Region),
    InterpolatedStringNotAllowed(Region),
    SpreadInModuleParams {
        params_region: Region,
        spread_region: Region,
    },
    SpreadInTypeNotImplemented(Region),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeModuleSource {
    Builtin,
    Current,
    Import(Region),
}

impl Problem {
    pub fn severity(&self) -> Severity {
        use Severity::{Fatal, RuntimeError, Warning};

        match self {
            Problem::UnusedDef(_, _) => Warning,
            Problem::UnusedImport(_, _) => Warning,
            Problem::UnusedModuleImport(_, _) => Warning,
            Problem::InterpolatedStringNotAllowed(_) => RuntimeError,
            Problem::ImportNameConflict { .. } => RuntimeError,
            Problem::ExplicitBuiltinImport(_, _) => Warning,
            Problem::ExplicitBuiltinTypeImport(_, _) => Warning,
            Problem::ImportShadowsSymbol { .. } => RuntimeError,
            Problem::ExposedButNotDefined(_) => RuntimeError,
            Problem::UnusedArgument(_, _, _, _) => Warning,
            Problem::UnusedBranchDef(_, _) => Warning,
            Problem::PrecedenceProblem(_) => RuntimeError,
            Problem::UnsupportedPattern(_, _) => RuntimeError,
            Problem::MultipleSpreadsInPattern { .. } => Warning,
            Problem::IgnoredFieldInType { .. } => Warning,
            Problem::Shadowing { .. } => RuntimeError,
            Problem::CyclicAlias(..) => RuntimeError,
            Problem::BadRecursion(_) => RuntimeError,
            Problem::PhantomTypeArgument { .. } => Warning,
            Problem::UndeclaredTypeVar { .. } => RuntimeError,
            Problem::WildcardNotAllowed { .. } => RuntimeError,
            Problem::UnderscoreNotAllowed { .. } => RuntimeError,
            Problem::DuplicateRecordFieldValue { .. } => Warning,
            Problem::DuplicateRecordFieldType { .. } => RuntimeError,
            Problem::DuplicateTag { .. } => RuntimeError,
            Problem::RuntimeError(_) => RuntimeError,
            Problem::SignatureDefMismatch { .. } => RuntimeError,
            Problem::InvalidAliasRigid { .. } => RuntimeError,
            Problem::InvalidInterpolation(_) => RuntimeError,
            Problem::InvalidHexadecimal(_) => RuntimeError,
            Problem::InvalidUnicodeCodePt(_) => RuntimeError,
            Problem::NestedDatatype { .. } => RuntimeError,
            Problem::InvalidExtensionType { .. } => RuntimeError,
            Problem::AbilityHasTypeVariables { .. } => RuntimeError,
            Problem::ImplementsClauseIsNotAbility { .. } => RuntimeError,
            Problem::IllegalImplementsClause { .. } => RuntimeError,
            Problem::DuplicateImplementsAbility { .. } => Warning,
            Problem::AbilityMemberMissingImplementsClause { .. } => RuntimeError,
            Problem::AbilityMemberMultipleBoundVars { .. } => RuntimeError,
            Problem::AbilityNotOnToplevel { .. } => RuntimeError, // Ideally, could be compiled
            Problem::AbilityUsedAsType(_, _, _) => RuntimeError,
            Problem::NestedSpecialization(_, _) => RuntimeError, // Ideally, could be compiled
            Problem::IllegalDerivedAbility(_) => RuntimeError,
            Problem::ImplementationNotFound { .. } => RuntimeError,
            Problem::NotAnAbilityMember { .. } => RuntimeError,
            Problem::QualifiedAbilityImpl { .. } => RuntimeError,
            Problem::AbilityImplNotIdent { .. } => RuntimeError,
            Problem::IgnoredFieldForAbilityImpl(_) => RuntimeError,
            Problem::DefaultValueFieldForAbilityImpl(_) => RuntimeError,
            Problem::SpreadUsedInAbilityImpl(_) => Warning,
            Problem::DuplicateImpl { .. } => Warning, // First impl is used at runtime
            Problem::NotAnAbility(_) => Warning,
            Problem::ImplementsNonRequired { .. } => Warning,
            Problem::DoesNotImplementAbility { .. } => RuntimeError,
            Problem::NotBoundInAllPatterns { .. } => RuntimeError,
            Problem::NoIdentifiersIntroduced(_) => Warning,
            Problem::OverloadedSpecialization { .. } => Warning, // Ideally, will compile
            Problem::UnnecessaryOutputWildcard { .. } => Warning,
            // TODO: sometimes this can just be a warning, e.g. if you have [1, .., .., 2] but we
            // don't catch that yet.
            Problem::MultipleListRestPattern { .. } => RuntimeError,
            Problem::BadTypeArguments { .. } => RuntimeError,
            // TODO: this can be a warning instead if we recover the program by
            // injecting a crash message
            Problem::UnappliedCrash { .. } => RuntimeError,
            Problem::OverAppliedCrash { .. } => RuntimeError,
            Problem::UnappliedDbg { .. } => RuntimeError,
            Problem::OverAppliedDbg { .. } => RuntimeError,
            Problem::UnderAppliedTry { .. } => Warning,
            Problem::DefsOnlyUsedInRecursion(_, _) => Warning,
            Problem::FileProblem { .. } => Fatal,
            Problem::ReturnOutsideOfFunction { .. } => Warning,
            Problem::StatementsAfterReturn { .. } => Warning,
            Problem::ReturnAtEndOfFunction { .. } => Warning,
            Problem::UnsuffixedEffectfulRecordField(_) | Problem::SuffixedPureRecordField(..) => {
                Warning
            }
            Problem::EmptyTupleType(_) => Warning,
            Problem::UnboundTypeVarsInAs(_) => Warning,
            Problem::SpreadInModuleParams { .. } => Warning,
            Problem::SpreadInTypeNotImplemented(_) => Warning,
        }
    }

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
            | Problem::ImportNameConflict {
                new_import_region: region,
                ..
            }
            | Problem::ExplicitBuiltinImport(_, region)
            | Problem::InterpolatedStringNotAllowed(region)
            | Problem::ExplicitBuiltinTypeImport(_, region)
            | Problem::ImportShadowsSymbol { region, .. }
            | Problem::UnusedArgument(_, _, _, region)
            | Problem::UnusedBranchDef(_, region)
            | Problem::PrecedenceProblem(PrecedenceProblem::BothNonAssociative(region, _, _))
            | Problem::UnsupportedPattern(_, region)
            | Problem::MultipleSpreadsInPattern {
                record_region: _,
                first_spread: _,
                shadow_spread: region,
            }
            | Problem::IgnoredFieldInType {
                record_region: _,
                field_region: region,
            }
            | Problem::CyclicAlias(_, region, _, _)
            | Problem::PhantomTypeArgument {
                variable_region: region,
                ..
            }
            | Problem::WildcardNotAllowed {
                one_occurrence: region,
                ..
            }
            | Problem::UnderscoreNotAllowed {
                one_occurrence: region,
                ..
            }
            | Problem::UndeclaredTypeVar {
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
            | Problem::DuplicateTag {
                tag_union_region: region,
                ..
            }
            | Problem::InvalidAliasRigid { region, .. }
            | Problem::InvalidInterpolation(region)
            | Problem::EmptyTupleType(region)
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
            | Problem::ImplementsClauseIsNotAbility { region }
            | Problem::IllegalImplementsClause { region }
            | Problem::DuplicateImplementsAbility { region, .. }
            | Problem::AbilityMemberMissingImplementsClause { region, .. }
            | Problem::AbilityMemberMultipleBoundVars {
                span_implements_clauses: region,
                ..
            }
            | Problem::AbilityNotOnToplevel { region }
            | Problem::AbilityUsedAsType(_, _, region)
            | Problem::NestedSpecialization(_, region)
            | Problem::IllegalDerivedAbility(region)
            | Problem::ImplementationNotFound { region, .. }
            | Problem::NotAnAbilityMember { region, .. }
            | Problem::QualifiedAbilityImpl { region }
            | Problem::AbilityImplNotIdent { region }
            | Problem::IgnoredFieldForAbilityImpl(region)
            | Problem::DefaultValueFieldForAbilityImpl(region)
            | Problem::SpreadUsedInAbilityImpl(region)
            | Problem::DuplicateImpl {
                original: region, ..
            }
            | Problem::NotAnAbility(region)
            | Problem::ImplementsNonRequired { region, .. }
            | Problem::NoIdentifiersIntroduced(region)
            | Problem::DoesNotImplementAbility { region, .. }
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
            | Problem::UnappliedCrash { region }
            | Problem::OverAppliedDbg { region }
            | Problem::UnappliedDbg { region }
            | Problem::UnderAppliedTry { region }
            | Problem::DefsOnlyUsedInRecursion(_, region)
            | Problem::ReturnOutsideOfFunction { region, .. }
            | Problem::StatementsAfterReturn { region }
            | Problem::ReturnAtEndOfFunction { region }
            | Problem::UnboundTypeVarsInAs(region)
            | Problem::UnsuffixedEffectfulRecordField(region)
            | Problem::SuffixedPureRecordField(region)
            | Problem::SpreadInModuleParams {
                params_region: _,
                spread_region: region,
            }
            | Problem::SpreadInTypeNotImplemented(region) => Some(*region),

            Problem::BadRecursion(cycle_entries) => {
                cycle_entries.first().map(|entry| entry.expr_region)
            }

            Problem::RuntimeError(runtime_error) => {
                Some(runtime_error.region()).filter(|region| region.is_empty())
            }

            Problem::FileProblem { .. } | Problem::ExposedButNotDefined(_) => None,
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

impl PrecedenceProblem {
    pub fn region(&self) -> Region {
        match self {
            PrecedenceProblem::BothNonAssociative(region, _, _) => *region,
        }
    }
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
    InvalidIgnoredValue {
        field_name: Lowercase,
        field_region: Region,
        record_region: Region,
    },
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
    // Example: when 1 is 1.X -> 32
    MalformedPattern(MalformedPatternProblem, Region),

    UnresolvedTypeVar,
    ErroneousType,

    LookupNotInScope {
        loc_name: Loc<Ident>,
        /// All of the names in scope (for the error message)
        suggestion_options: MutSet<Box<str>>,
        /// If the unfound variable is `name` and there's an ignored variable called `_name`,
        /// this is the region where `_name` is defined (for the error message)
        underscored_suggestion_region: Option<Region>,
    },
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
    ReadIngestedFileError {
        filename: PathBuf,
        error: io::ErrorKind,
        region: Region,
    },
    InvalidPrecedence(PrecedenceProblem, Region),
    MalformedIdentifier(Box<str>, roc_parse::ident::BadIdent, Region),
    MalformedTypeName(Box<str>, Region),
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

    EmptyRecordBuilder(Region),
    SingleFieldRecordBuilder(Region),
    OptionalFieldInRecordBuilder {
        record_region: Region,
        field_region: Region,
    },
    SpreadInRecordBuilder {
        record_region: Region,
        spread_region: Region,
    },

    SpreadNotImplementedYet(Region),

    NonFunctionHostedAnnotation(Region),
    InvalidTupleIndex(Region),
    IngestedFilePathError(Region),
}

impl RuntimeError {
    pub fn runtime_message(self) -> String {
        use RuntimeError::*;

        match self {
            DegenerateBranch(region) => {
                format!(
                    "Hit a branch pattern that does not bind all symbols its body needs, at {region:?}"
                )
            }
            err => format!("{err:?}"),
        }
    }

    pub fn region(&self) -> Region {
        match self {
            RuntimeError::Shadowing { shadow, .. } => shadow.region,
            RuntimeError::InvalidOptionalValue { field_region, .. } => *field_region,
            RuntimeError::InvalidIgnoredValue { field_region, .. } => *field_region,
            RuntimeError::UnsupportedPattern(region)
            | RuntimeError::MalformedPattern(_, region)
            | RuntimeError::OpaqueOutsideScope {
                referenced_region: region,
                ..
            }
            | RuntimeError::OpaqueAppliedToMultipleArgs(region)
            | RuntimeError::ValueNotExposed { region, .. }
            | RuntimeError::ModuleNotImported { region, .. }
            | RuntimeError::InvalidPrecedence(_, region)
            | RuntimeError::MalformedIdentifier(_, _, region)
            | RuntimeError::MalformedTypeName(_, region)
            | RuntimeError::InvalidRecordUpdate { region }
            | RuntimeError::InvalidFloat(_, region, _)
            | RuntimeError::InvalidInt(_, _, region, _)
            | RuntimeError::EmptySingleQuote(region)
            | RuntimeError::InvalidTupleIndex(region)
            | RuntimeError::IngestedFilePathError(region)
            | RuntimeError::MultipleCharsInSingleQuote(region)
            | RuntimeError::DegenerateBranch(region)
            | RuntimeError::InvalidInterpolation(region)
            | RuntimeError::InvalidHexadecimal(region)
            | RuntimeError::EmptyRecordBuilder(region)
            | RuntimeError::SingleFieldRecordBuilder(region)
            | RuntimeError::OptionalFieldInRecordBuilder {
                record_region: _,
                field_region: region,
            }
            | RuntimeError::SpreadInRecordBuilder {
                record_region: _,
                spread_region: region,
            }
            | RuntimeError::SpreadNotImplementedYet(region)
            | RuntimeError::ReadIngestedFileError { region, .. }
            | RuntimeError::InvalidUnicodeCodePt(region)
            | RuntimeError::NonFunctionHostedAnnotation(region) => *region,

            RuntimeError::UnresolvedTypeVar
            | RuntimeError::ErroneousType
            | RuntimeError::NonExhaustivePattern
            | RuntimeError::NoImplementationNamed { .. }
            | RuntimeError::NoImplementation
            | RuntimeError::VoidValue
            | RuntimeError::ExposedButNotDefined(_) => Region::zero(),

            RuntimeError::LookupNotInScope { loc_name, .. } => loc_name.region,
            RuntimeError::OpaqueNotDefined { usage, .. } => usage.region,
            RuntimeError::OpaqueNotApplied(ident) => ident.region,
            RuntimeError::CircularDef(cycle) => cycle[0].symbol_region,
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
    CantApplyPattern,
}
