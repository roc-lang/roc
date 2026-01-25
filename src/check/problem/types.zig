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
    platform_def_not_found: PlatformDefNotFound,
    platform_alias_not_found: PlatformAliasNotFound,
    comptime_crash: ComptimeCrash,
    comptime_expect_failed: ComptimeExpectFailed,
    comptime_eval_error: ComptimeEvalError,
    non_exhaustive_match: NonExhaustiveMatch,
    redundant_pattern: RedundantPattern,
    unmatchable_pattern: UnmatchablePattern,

    pub const Idx = enum(u32) { _ };
    pub const Tag = std.meta.Tag(@This());
};

// platform errors //

/// Error for when a platform expects an alias to be defined, but it's not there
pub const PlatformAliasNotFound = struct {
    expected_alias_ident: Ident.Idx,
    ctx: enum { not_found, found_but_not_alias },
};

/// Error for when a platform expects an alias to be defined, but it's not there
pub const PlatformDefNotFound = struct {
    expected_def_ident: Ident.Idx,
    ctx: enum { not_found, found_but_not_exported },
};

// comptime errors //

/// A crash that occurred during compile-time evaluation
pub const ComptimeCrash = struct {
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

// static dispatch //

/// Error related to static dispatch
pub const StaticDispatch = union(enum) {
    dispatcher_not_nominal: DispatcherNotNominal,
    dispatcher_does_not_impl_method: DispatcherDoesNotImplMethod,
    type_does_not_support_equality: TypeDoesNotSupportEquality,
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
    /// Optional numeric literal info for from_numeral constraints
    num_literal: ?types_mod.NumeralInfo = null,

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
