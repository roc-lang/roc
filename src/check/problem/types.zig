//! Problem data types for type checking errors
//!
//! This module contains all the problem/error types that can occur during
//! type checking. These are pure data structures - rendering to user-friendly
//! messages is handled by the report module.

const std = @import("std");
const base = @import("base");
const types_mod = @import("types");
const can = @import("can");

const snapshot = @import("../snapshot.zig");
const context_mod = @import("context.zig");

const CIR = can.CIR;
const Ident = base.Ident;
const Var = types_mod.Var;
const SnapshotContentIdx = snapshot.SnapshotContentIdx;

pub const Context = context_mod.Context;
pub const Category = context_mod.Category;

const ByteListRange = struct { start: usize, count: usize };

/// Alias into the Store.extra_strings_backing array
pub const ExtraStringIdx = ByteListRange;

/// A range of patterns
pub const MissingPatternsRange = struct { start: usize, count: usize };

/// The kind of problem we're dealing with
pub const Problem = union(enum) {
    type_mismatch: TypeMismatch,
    type_apply_mismatch_arities: TypeApplyArityMismatch,
    static_dispatch: StaticDispatch,
    cannot_access_opaque_nominal: CannotAccessOpaqueNominal,
    nominal_type_resolution_failed: NominalTypeResolutionFailed,
    recursive_alias: RecursiveAlias,
    unsupported_alias_where_clause: UnsupportedAliasWhereClause,
    infinite_recursion: VarWithSnapshot,
    anonymous_recursion: VarWithSnapshot,
    polymorphic_value: VarWithSnapshot,
    polymorphic_var_annotation: PolymorphicVarAnnotation,
    effectful_top_level: EffectfulTopLevel,
    effectful_expect: EffectfulExpect,
    annotation_only_value: AnnotationOnlyValue,
    hosted_unboxed_function: HostedUnboxedFunction,
    host_boundary_open_row: HostBoundaryOpenRow,
    platform_def_not_found: PlatformDefNotFound,
    platform_hosted_section: PlatformHostedSection,
    platform_alias_not_found: PlatformAliasNotFound,
    comptime_crash: ComptimeCrash,
    comptime_invalid_numeral: ComptimeInvalidNumeral,
    comptime_invalid_quote: ComptimeInvalidQuote,
    comptime_expect_failed: ComptimeExpectFailed,
    comptime_eval_error: ComptimeEvalError,
    invalid_numeric_literal: InvalidNumericLiteral,
    tuple_access_needs_annotation: TupleAccessNeedsAnnotation,
    literal_defaulted: LiteralDefaulted,
    non_exhaustive_match: NonExhaustiveMatch,
    non_exhaustive_destructure: NonExhaustiveDestructure,
    redundant_pattern: RedundantPattern,
    unmatchable_pattern: UnmatchablePattern,
    unreachable_code: UnreachableCode,
    comptime_unused_branch: ComptimeUnusedBranch,
    comptime_condition: ComptimeCondition,

    pub const Idx = enum(u32) { _ };
    pub const Tag = std.meta.Tag(@This());
};

// platform errors //

/// Error for when a platform expects an alias to be defined, but it's not there
pub const PlatformAliasNotFound = struct {
    expected_alias_ident: Ident.Idx,
    app_region: base.Region,
    platform_region: base.Region,
    ctx: enum { not_found, found_but_not_type },
};

/// The platform's hosted section disagrees with the hosted functions its
/// exposed type modules declare, or maps a function to an invalid linker symbol.
pub const PlatformHostedSection = struct {
    /// The qualified function name or linker symbol the problem is about
    name: ExtraStringIdx,
    reason: enum {
        /// A hosted function is missing from the hosted section
        function_not_in_section,
        /// A section entry names a function that is not a hosted function
        unknown_function,
        /// Two section entries name the same hosted function
        duplicate_function,
        /// Two hosted/provides entries use the same linker symbol
        duplicate_symbol,
        /// The symbol is not a valid external C identifier
        invalid_symbol,
        /// The symbol is one of the fixed runtime symbols (the roc_alloc family)
        reserved_symbol,
        /// The symbol starts with the internal roc__ namespace prefix
        reserved_prefix,
    },
};

/// Error for when a platform expects a def to be defined, but it's not there
pub const PlatformDefNotFound = struct {
    expected_def_ident: Ident.Idx,
    app_region: base.Region,
    platform_region: base.Region,
    ctx: enum { not_found, found_but_not_exported },
};

/// Hosted functions cannot accept or return unboxed functions.
pub const HostedUnboxedFunction = struct {
    region: base.Region,
};

/// Host-bound types must not contain open record or tag-union rows.
pub const HostBoundaryOpenRow = struct {
    region: base.Region,
};

/// A standalone type annotation without an implementation cannot be used as a runtime value.
pub const AnnotationOnlyValue = struct {
    region: base.Region,
};

/// A mutable `var` whose annotation introduces an unbound type variable. A `var`
/// is never generalized, so a free type variable in its annotation can never be
/// bound — the variable must have a concrete type.
pub const PolymorphicVarAnnotation = struct {
    region: base.Region,
};

/// A top-level value definition performs effects while initializing.
pub const EffectfulTopLevel = struct {
    region: base.Region,
};

/// An expect expression performs effects while evaluating its condition.
pub const EffectfulExpect = struct {
    region: base.Region,
};

// comptime errors //

/// A crash that occurred during compile-time evaluation
pub const ComptimeCrash = struct {
    message: ExtraStringIdx,
    region: base.Region,
};

/// A numeric literal that a custom `from_numeral` implementation rejected
/// during compile-time evaluation
pub const ComptimeInvalidNumeral = struct {
    message: ExtraStringIdx,
    region: base.Region,
};

/// A string literal that a custom `from_quote` implementation rejected
/// during compile-time evaluation
pub const ComptimeInvalidQuote = struct {
    message: ExtraStringIdx,
    region: base.Region,
};

/// An expect that failed during compile-time evaluation
pub const ComptimeExpectFailed = struct {
    message: ExtraStringIdx,
    region: base.Region,
};

/// An error that occurred during compile-time evaluation
pub const ComptimeEvalError = struct {
    error_name: ExtraStringIdx,
    region: base.Region,
};

// generic errors //

/// A problem involving a single type variable, with a snapshot for error reporting.
/// Used for recursion errors, invalid extension types, etc.
pub const VarWithSnapshot = struct {
    var_: Var,
    snapshot: SnapshotContentIdx,

    /// If this type was found in a top-level def, the name of that def
    def_name: ?Ident.Idx,
};

// number problems //

/// Number literal doesn't fit in the expected type
pub const NumberDoesNotFit = struct {
    literal_var: Var,
    expected_type: SnapshotContentIdx,
};

/// Negative literal assigned to unsigned type
pub const NegativeUnsignedInt = struct {
    literal_var: Var,
    expected_type: SnapshotContentIdx,
};

/// Invalid numeric literal that cannot be converted to target type
pub const InvalidNumericLiteral = struct {
    literal_var: Var,
    expected_type: SnapshotContentIdx,
    is_fractional: bool,
    region: base.Region,
};

/// Tuple access on an unconstrained value cannot infer the tuple's full arity.
pub const TupleAccessNeedsAnnotation = struct {
    region: base.Region,
    elem_index: u32,
};

/// Warning (the Haskell §4.3.4 / `-Wtype-defaults` analogue): an open literal
/// (number or string) unreachable from its definition's type was defaulted at the
/// generalization boundary. Such a literal is shared by every instantiation of the
/// def and can never adapt per call site; the warning lets the user pin a
/// different type with an annotation.
pub const LiteralDefaulted = struct {
    literal_var: Var,
    /// Which kind defaulted (numeral vs. quote), so the report can word the
    /// message and hint per kind.
    kind: types_mod.StaticDispatchConstraint.LiteralKind,
    /// Snapshot of the committed default type (e.g. `Dec`) for rendering.
    default_snapshot: SnapshotContentIdx,
    /// The literal's own source region.
    region: base.Region,
};

/// Error when a stmt expression returns a non-empty record value
pub const UnusedValue = struct {
    var_: Var,
    snapshot: SnapshotContentIdx,
};

// type mismatch //

/// These two variables mismatch. This should usually be cast into a more
/// specific error depending on context.
pub const TypeMismatch = struct {
    types: TypePair,
    /// Where this type mismatch occurred (for contextual error messages)
    context: Context = .none,
};

/// The expected and actual types in a type mismatch
pub const TypePair = struct {
    expected_var: Var,
    expected_snapshot: SnapshotContentIdx,
    actual_var: Var,
    actual_snapshot: SnapshotContentIdx,
    /// The specific region where this constraint originated from (e.g., dot access expression)
    /// If present, this region should be highlighted instead of the variable's region
    constraint_origin_var: ?Var = null,
};

/// Problem data for platform requirement type mismatches
pub const IncompatiblePlatformRequirement = struct {
    /// The identifier that the platform requires
    required_ident: Ident.Idx,
};

/// Problem data for cross-module import type mismatches
pub const CrossModuleImport = struct {
    import_region: CIR.Expr.Idx,
    module_idx: CIR.Import.Idx,
};

/// Problem data for a non-exhaustive match expression
pub const NonExhaustiveMatch = struct {
    match_expr: CIR.Expr.Idx,
    /// Snapshot of the condition type for error messages
    condition_snapshot: SnapshotContentIdx,
    /// Range into the problems store's missing_patterns_backing for pattern indices
    missing_patterns: MissingPatternsRange,
    /// This was discovered by compile-time evaluation taking the generated miss branch.
    empirical: bool = false,
};

/// Problem data for a non-exhaustive destructuring pattern
pub const NonExhaustiveDestructure = struct {
    pattern: CIR.Pattern.Idx,
    /// Snapshot of the destructured value type for error messages
    value_snapshot: SnapshotContentIdx,
    /// Range into the problems store's missing_patterns_backing for pattern indices
    missing_patterns: MissingPatternsRange,
    /// This was discovered by compile-time evaluation taking the generated miss branch.
    empirical: bool = false,
};

/// A compile-time-only branch or match alternative was not taken by compile-time evaluation.
pub const ComptimeUnusedBranch = struct {
    kind: enum {
        match,
        if_,
    },
    site_region: base.Region,
    branch_region: base.Region,
};

/// A conditional expression was known while checking, so it will always make the same choice.
pub const ComptimeCondition = struct {
    kind: enum {
        if_condition,
        if_guard,
        match_scrutinee,
    },
    region: base.Region,
};

/// Problem data for a redundant pattern in a match
pub const RedundantPattern = struct {
    match_expr: CIR.Expr.Idx,
    num_branches: u32,
    problem_branch_index: u32,
};

/// Problem data for an unmatchable pattern (pattern on uninhabited type)
pub const UnmatchablePattern = struct {
    match_expr: CIR.Expr.Idx,
    num_branches: u32,
    problem_branch_index: u32,
};

/// Code that appears after an expression or statement that never returns.
pub const UnreachableCode = struct {
    region: base.Region,
};

// static dispatch //

/// Error related to static dispatch
pub const StaticDispatch = union(enum) {
    dispatcher_not_nominal: DispatcherNotNominal,
    dispatcher_does_not_impl_method: DispatcherDoesNotImplMethod,
    type_does_not_support_equality: TypeDoesNotSupportEquality,
    unresolved_dispatcher: UnresolvedDispatcher,
    recursive_dispatch: RecursiveDispatch,
};

/// Error when a static dispatch method is called on a receiver whose type is an
/// unresolved type variable that no instantiation can ever pin down (an
/// "ambiguous type variable"). Examples: `poly().to_i128()` or `poly() == poly()`
/// where `poly` returns a free variable. Unresolved type variables have no
/// methods, so the dispatch is genuinely ambiguous.
pub const UnresolvedDispatcher = struct {
    /// Region of the offending dispatch call expression (the primary underline
    /// target).
    region: base.Region,
    /// Optional secondary region (the call/argument that left the receiver's type
    /// undetermined) for the per-instantiation, helper-hidden case. When non-null
    /// and distinct from `region`, the renderer shows a connecting note and a
    /// second source region (mirroring `buildNumberUsedAsNonNumber`). When null —
    /// or equal to `region`, i.e. the dispatch IS the call site — only the primary
    /// region is shown, so the direct cases render identically to before.
    secondary_region: ?base.Region,
    /// Snapshot of the dispatcher (receiver) type for rendering.
    dispatcher_snapshot: SnapshotContentIdx,
    /// The dispatch method's name (e.g. `to_i128` for a method call, or `is_eq` /
    /// `plus` / `is_lt` etc. for a desugared operator). When this is a desugared
    /// operator the renderer maps it back to the source operator symbol.
    method_name: Ident.Idx,
    /// True when this constraint originated from a desugared operator (`==`, `+`,
    /// `<`, etc.), in which case the renderer shows the operator symbol rather
    /// than the desugared method name.
    is_binop: bool,
    /// For desugared equality (`==`/`!=`), whether the source operator was `!=`.
    binop_negated: bool,
    /// True when checking inserted an explicit runtime-error node for this
    /// diagnostic, so post-check lowering can safely continue through it.
    runtime_error_inserted: bool,
};

/// Error when you try to static dispatch on something that's not a nominal type
pub const DispatcherNotNominal = struct {
    dispatcher_var: Var,
    dispatcher_snapshot: SnapshotContentIdx,
    fn_var: Var,
    method_name: Ident.Idx,
};

/// Error when you try to static dispatch but the dispatcher does not have that method
pub const DispatcherDoesNotImplMethod = struct {
    dispatcher_var: Var,
    dispatcher_snapshot: SnapshotContentIdx,
    dispatcher_type: DispatcherType,
    fn_var: Var,
    method_name: Ident.Idx,
    origin: types_mod.StaticDispatchConstraint.Origin,
    /// Optional numeric literal info for `from_literal` constraints of kind `numeral`
    num_literal: ?types_mod.NumeralInfo = null,
    /// Source region of the string literal for `from_literal` constraints of kind `quote`
    quote_region: ?base.Region = null,
    /// True when the dispatcher was a numeric literal that was defaulted to Dec
    /// because no type annotation was given. Used to add explanatory text in errors.
    defaulted_from_numeric_literal: bool = false,

    /// Type of the dispatcher
    pub const DispatcherType = enum { nominal, rigid };
};

/// Error when an anonymous type (record, tuple, tag union) doesn't support equality
/// because one or more of its components contain types that don't have is_eq
pub const TypeDoesNotSupportEquality = struct {
    dispatcher_var: Var,
    dispatcher_snapshot: SnapshotContentIdx,
    fn_var: Var,
};

/// Error when satisfying a static-dispatch constraint immediately requires the
/// same static-dispatch constraint again on the same dispatcher type.
pub const RecursiveDispatch = struct {
    dispatcher_snapshot: SnapshotContentIdx,
    fn_var: Var,
    method_name: Ident.Idx,
};

// nominal type errors //

/// Error when you try to use an opaque nominal type constructor
pub const CannotAccessOpaqueNominal = struct {
    var_: Var,
    nominal_type_name: Ident.Idx,
};

/// Compiler bug: a nominal type variable doesn't resolve to a nominal_type structure.
/// This should never happen because:
/// 1. The canonicalizer only creates nominal patterns/expressions for s_nominal_decl statements
/// 2. generateNominalDecl always sets the decl_var to a nominal_type structure
/// 3. instantiateVar and copyVar preserve the nominal_type structure
pub const NominalTypeResolutionFailed = struct {
    var_: Var,
    nominal_type_decl_var: Var,
};

// type declaration errors //

/// Error when you try to apply the wrong number of arguments to a type in
/// an annotation
pub const TypeApplyArityMismatch = struct {
    type_name: base.Ident.Idx,
    region: base.Region,
    num_expected_args: u32,
    num_actual_args: u32,
};

/// Error when a type alias references itself (aliases cannot be recursive)
/// Use nominal types (:=) for recursive types instead
pub const RecursiveAlias = struct {
    type_name: base.Ident.Idx,
    region: base.Region,
};

/// Error when using alias syntax in where clause (e.g., `where [a.SomeAlias]`)
/// This syntax was used for abilities which have been removed from the language
pub const UnsupportedAliasWhereClause = struct {
    alias_name: base.Ident.Idx,
    region: base.Region,
};
