//! Generate Reports for type checking errors

const std = @import("std");
const base = @import("base");
const tracy = @import("tracy");
const types_mod = @import("types");
const can = @import("can");
const reporting = @import("reporting");

const snapshot = @import("snapshot.zig");

const Allocator = std.mem.Allocator;

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

const Report = reporting.Report;
const UnderlineRegion = reporting.UnderlineRegion;
const SourceCodeDisplayRegion = reporting.SourceCodeDisplayRegion;

const Ident = base.Ident;

const SnapshotContentIdx = snapshot.SnapshotContentIdx;

const ByteList = std.array_list.Managed(u8);
const ByteListRange = struct { start: usize, count: usize };

/// Alias into the Store.extra_strings_backing array
pub const ExtraStringIdx = ByteListRange;

/// A range of patterns
pub const MissingPatternsRange = struct { start: usize, count: usize };

const Var = types_mod.Var;

/// Returns singular form if count is 1, plural form otherwise.
/// Usage: pluralize(count, "argument", "arguments")
fn pluralize(count: anytype, singular: []const u8, plural: []const u8) []const u8 {
    return if (count == 1) singular else plural;
}

/// The kind of problem we're dealing with
pub const Problem = union(enum) {
    type_mismatch: TypeMismatch,
    fn_call_arity_mismatch: FnCallArityMismatch,
    type_apply_mismatch_arities: TypeApplyArityMismatch,
    static_dispatch: StaticDispatch,
    cannot_access_opaque_nominal: CannotAccessOpaqueNominal,
    nominal_type_resolution_failed: NominalTypeResolutionFailed,
    unused_value: UnusedValue,
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
    bug: Bug,

    pub const Idx = enum(u32) { _ };
    pub const Tag = std.meta.Tag(@This());
};

/// Error for when a break statement appears outside of a loop
pub const BreakOutsideLoop = struct {
    region: base.Region,
};

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

/// A problem involving a single type variable, with a snapshot for error reporting.
/// Used for recursion errors, invalid extension types, etc.
pub const VarWithSnapshot = struct {
    var_: Var,
    snapshot: SnapshotContentIdx,
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
    detail: ?TypeMismatchDetail,
};

/// The expected and actual types in a type mismatch
pub const TypePair = struct {
    expected_var: Var,
    expected_snapshot: SnapshotContentIdx,
    actual_var: Var,
    actual_snapshot: SnapshotContentIdx,
    /// True if the expected type comes from a type annotation
    from_annotation: bool = false,
    /// The specific region where this constraint originated from (e.g., dot access expression)
    /// If present, this region should be highlighted instead of the variable's region
    constraint_origin_var: ?Var = null,
};

/// More specific details about a particular type mismatch.
pub const TypeMismatchDetail = union(enum) {
    incompatible_list_elements: IncompatibleListElements,
    incompatible_if_cond,
    /// A statement expression must evaluate to {} but has a different type
    statement_not_unit,
    incompatible_if_branches: IncompatibleIfBranches,
    incompatible_match_cond_pattern: IncompatibleMatchCondPattern,
    incompatible_match_patterns: IncompatibleMatchPatterns,
    incompatible_match_branches: IncompatibleMatchBranches,
    non_exhaustive_match: NonExhaustiveMatch,
    invalid_try_operator: InvalidTryOperator,
    invalid_bool_binop: InvalidBoolBinop,
    invalid_nominal_tag,
    invalid_nominal_record,
    invalid_nominal_tuple,
    invalid_nominal_value,
    cross_module_import: CrossModuleImport,
    incompatible_fn_call_arg: IncompatibleFnCallArg,
    incompatible_fn_args_bound_var: IncompatibleFnArgsBoundVar,
    /// App's export type doesn't match the platform's required type
    incompatible_platform_requirement: IncompatiblePlatformRequirement,
};

/// Problem data for platform requirement type mismatches
pub const IncompatiblePlatformRequirement = struct {
    /// The identifier that the platform requires
    required_ident: Ident.Idx,
};

/// Problem data for when list elements have incompatible types
pub const IncompatibleListElements = struct {
    last_elem_idx: CIR.Node.Idx,
    incompatible_elem_index: u32, // 0-based index of the incompatible element
    list_length: u32, // Total number of elements in the list
};

/// Problem data for cross-module import type mismatches
pub const CrossModuleImport = struct {
    import_region: CIR.Expr.Idx,
    module_idx: CIR.Import.Idx,
};

/// Problem data when function is called with wrong number of arguments
pub const FnCallArityMismatch = struct {
    fn_name: ?Ident.Idx,
    fn_var: Var,
    fn_snapshot: SnapshotContentIdx,
    call_region: base.Region,
    expected_args: u32,
    actual_args: u32,
};

/// Problem data when function argument types don't match
pub const IncompatibleFnCallArg = struct {
    fn_name: ?Ident.Idx,
    arg_var: Var,
    incompatible_arg_index: u32, // 0-based index of the incompatible arg
    num_args: u32, // Total number of fn args
};

/// Problem data when function arguments have incompatible types but are bound by the same type variable
pub const IncompatibleFnArgsBoundVar = struct {
    fn_name: ?Ident.Idx,
    first_arg_var: Var,
    second_arg_var: Var,
    first_arg_index: u32, // 0-based index of the first arg
    second_arg_index: u32, // 0-based index of the second arg
    num_args: u32, // number of args to the function call
};

/// Problem data for when if branches have incompatible types
pub const IncompatibleIfBranches = struct {
    parent_if_expr: CIR.Expr.Idx,
    last_if_branch: CIR.Expr.IfBranch.Idx,
    num_branches: u32,
    problem_branch_index: u32,
};

/// Problem data for when match expression type is incompatible from the patterns
pub const IncompatibleMatchCondPattern = struct {
    match_expr: CIR.Expr.Idx,
};

/// Problem data for when match patterns have have incompatible types
pub const IncompatibleMatchPatterns = struct {
    match_expr: CIR.Expr.Idx,
    num_branches: u32,
    problem_branch_index: u32,
    num_patterns: u32,
    problem_pattern_index: u32,
};

/// Problem data for when match branches have have incompatible types
pub const IncompatibleMatchBranches = struct {
    match_expr: CIR.Expr.Idx,
    num_branches: u32,
    problem_branch_index: u32,
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

/// Problem data for when the `?` operator is used on a non-Try type.
/// A Try type is a tag union with Ok and Err tags.
pub const InvalidTryOperator = struct {
    expr: CIR.Expr.Idx,
};

/// Problem data for when a bool binop (`and` or `or`) is invalid
pub const InvalidBoolBinop = struct {
    binop_expr: CIR.Expr.Idx,
    problem_side: enum { lhs, rhs },
    binop: BoolBinop,

    /// Bool binop
    pub const BoolBinop = enum { @"and", @"or" };
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

// bug //

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

// bug //

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

// bug //

/// A bug that occurred during unification
pub const Bug = struct {
    expected_var: Var,
    expected: SnapshotContentIdx,
    actual_var: Var,
    actual: SnapshotContentIdx,
};

// reporting //

/// Build reports for problems
pub const ReportBuilder = struct {
    const Self = @This();

    gpa: Allocator,
    bytes_buf: std.array_list.Managed(u8),
    module_env: *ModuleEnv,
    can_ir: *const ModuleEnv,
    snapshots: *const snapshot.Store,
    problems: *const Store,
    source: []const u8,
    filename: []const u8,
    other_modules: []const *const ModuleEnv,
    import_mapping: *const @import("types").import_mapping.ImportMapping,

    /// Init report builder
    /// Only owned field is `buf`
    pub fn init(
        gpa: Allocator,
        module_env: *ModuleEnv,
        can_ir: *const ModuleEnv,
        snapshots: *const snapshot.Store,
        problems: *const Store,
        filename: []const u8,
        other_modules: []const *const ModuleEnv,
        import_mapping: *const @import("types").import_mapping.ImportMapping,
    ) Self {
        return .{
            .gpa = gpa,
            .bytes_buf = std.array_list.Managed(u8).init(gpa),
            .module_env = module_env,
            .can_ir = can_ir,
            .snapshots = snapshots,
            .problems = problems,
            .import_mapping = import_mapping,
            .source = module_env.common.source,
            .filename = filename,
            .other_modules = other_modules,
        };
    }

    /// Deinit report builder
    /// Only owned field is `buf`
    pub fn deinit(self: *Self) void {
        self.bytes_buf.deinit();
    }

    /// Get the formatted string for a snapshot.
    /// Returns a placeholder if the formatted string is missing, allowing error reporting
    /// to continue gracefully even if snapshots are incomplete.
    fn getFormattedString(self: *const Self, idx: SnapshotContentIdx) []const u8 {
        return self.snapshots.getFormattedString(idx) orelse "<unknown type>";
    }

    /// Returns the operator symbol for a given method ident, or null if not an operator method.
    /// Maps method idents like plus, minus, times, div_by to their corresponding operator symbols.
    fn getOperatorForMethod(self: *const Self, method_ident: Ident.Idx) ?[]const u8 {
        const idents = self.can_ir.idents;
        if (method_ident == idents.plus) return "+";
        if (method_ident == idents.minus) return "-";
        if (method_ident == idents.times) return "*";
        if (method_ident == idents.div_by) return "/";
        if (method_ident == idents.div_trunc_by) return "//";
        if (method_ident == idents.rem_by) return "%";
        if (method_ident == idents.negate) return "-";
        if (method_ident == idents.is_eq) return "==";
        if (method_ident == idents.is_lt) return "<";
        if (method_ident == idents.is_lte) return "<=";
        if (method_ident == idents.is_gt) return ">";
        if (method_ident == idents.is_gte) return ">=";
        if (method_ident == idents.not) return "not";
        return null;
    }

    /// Build a report for a problem
    pub fn build(
        self: *Self,
        problem: Problem,
    ) !Report {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (problem) {
            .type_mismatch => |mismatch| {
                if (mismatch.detail) |detail| {
                    switch (detail) {
                        .incompatible_list_elements => |data| {
                            return self.buildIncompatibleListElementsReport(mismatch.types, data);
                        },
                        .incompatible_if_cond => {
                            return self.buildInvalidIfCondition(mismatch.types);
                        },
                        .statement_not_unit => {
                            return self.buildStatementNotUnit(mismatch.types);
                        },
                        .incompatible_if_branches => |data| {
                            return self.buildIncompatibleIfBranches(mismatch.types, data);
                        },
                        .incompatible_match_cond_pattern => |data| {
                            return self.buildIncompatibleMatchCondPattern(mismatch.types, data);
                        },
                        .incompatible_match_patterns => |data| {
                            return self.buildIncompatibleMatchPatterns(mismatch.types, data);
                        },
                        .incompatible_match_branches => |data| {
                            return self.buildIncompatibleMatchBranches(mismatch.types, data);
                        },
                        .non_exhaustive_match => |data| {
                            return self.buildNonExhaustiveMatch(data);
                        },
                        .invalid_try_operator => |data| {
                            return self.buildInvalidTryOperator(mismatch.types, data);
                        },
                        .invalid_bool_binop => |data| {
                            return self.buildInvalidBoolBinop(mismatch.types, data);
                        },
                        .invalid_nominal_tag => {
                            return self.buildInvalidNominalTag(mismatch.types);
                        },
                        .invalid_nominal_record => {
                            return self.buildInvalidNominalRecord(mismatch.types);
                        },
                        .invalid_nominal_tuple => {
                            return self.buildInvalidNominalTuple(mismatch.types);
                        },
                        .invalid_nominal_value => {
                            return self.buildInvalidNominalValue(mismatch.types);
                        },
                        .cross_module_import => |data| {
                            return self.buildCrossModuleImportError(mismatch.types, data);
                        },
                        .incompatible_fn_call_arg => |data| {
                            return self.buildIncompatibleFnCallArg(mismatch.types, data);
                        },
                        .incompatible_fn_args_bound_var => |data| {
                            return self.buildIncompatibleFnArgsBoundVar(mismatch.types, data);
                        },
                        .incompatible_platform_requirement => {
                            // For now, use generic type mismatch report for platform requirements
                            return self.buildGenericTypeMismatchReport(mismatch.types);
                        },
                    }
                } else {
                    return self.buildGenericTypeMismatchReport(mismatch.types);
                }
            },
            .fn_call_arity_mismatch => |data| {
                return self.buildFnCallArityMismatchReport(data);
            },
            .type_apply_mismatch_arities => |data| {
                return self.buildTypeApplyArityMismatchReport(data);
            },
            .cannot_access_opaque_nominal => |data| {
                return self.buildCannotAccessOpaqueNominal(data);
            },
            .nominal_type_resolution_failed => |data| {
                return self.buildNominalTypeResolutionFailed(data);
            },
            .static_dispatch => |detail| {
                switch (detail) {
                    .dispatcher_not_nominal => |data| return self.buildStaticDispatchDispatcherNotNominal(data),
                    .dispatcher_does_not_impl_method => |data| return self.buildStaticDispatchDispatcherDoesNotImplMethod(data),
                    .type_does_not_support_equality => |data| return self.buildTypeDoesNotSupportEquality(data),
                }
            },
            .unused_value => |data| {
                return self.buildUnusedValueReport(data);
            },
            .recursive_alias => |data| {
                return self.buildRecursiveAliasReport(data);
            },
            .unsupported_alias_where_clause => |data| {
                return self.buildUnsupportedAliasWhereClauseReport(data);
            },
            .infinite_recursion => |data| return self.buildInfiniteRecursionReport(data),
            .anonymous_recursion => |data| return self.buildAnonymousRecursionReport(data),
            .platform_alias_not_found => |data| {
                return self.buildPlatformAliasNotFound(data);
            },
            .platform_def_not_found => |data| {
                return self.buildPlatformDefNotFound(data);
            },
            .comptime_crash => |data| return self.buildComptimeCrashReport(data),
            .comptime_expect_failed => |data| return self.buildComptimeExpectFailedReport(data),
            .comptime_eval_error => |data| return self.buildComptimeEvalErrorReport(data),
            .non_exhaustive_match => |data| return self.buildNonExhaustiveMatchReport(data),
            .redundant_pattern => |data| return self.buildRedundantPatternReport(data),
            .unmatchable_pattern => |data| return self.buildUnmatchablePatternReport(data),
            .bug => |data| return self.buildBugReport(data),
        }
    }

    // type mismatch //

    /// Build a report for type mismatch diagnostic
    fn buildGenericTypeMismatchReport(
        self: *Self,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);
        errdefer report.deinit();

        const owned_actual = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        const owned_expected = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

        // For annotation mismatches, we want to highlight the expression that doesn't match,
        // not the annotation itself. When from_annotation is true and we're showing
        // "The type annotation says...", the expression is in expected_var.
        // If we have a constraint origin (e.g., from dot access), use that for more precise highlighting.

        const region_var = if (types.constraint_origin_var) |origin_var|
            origin_var
        else
            types.actual_var;
        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(region_var)));

        // Check if both types are functions to provide more specific error messages
        const expected_content = self.snapshots.getContent(types.expected_snapshot);
        const actual_content = self.snapshots.getContent(types.actual_snapshot);

        if (types.from_annotation and areBothFunctionSnapshots(expected_content, actual_content)) {
            // When we have constraint_origin_var, it indicates this error originated from
            // a specific constraint like a dot access (e.g., str.to_utf8()).
            // In this case, show a specialized argument type mismatch error.
            if (types.constraint_origin_var) |origin_var| {
                report.deinit();
                return self.buildIncompatibleFnCallArg(types, .{
                    .fn_name = null,
                    .arg_var = origin_var,
                    .incompatible_arg_index = 0, // First argument
                    .num_args = 1, // Single argument lambda
                });
            }
        }

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(region.*);

        try report.document.addReflowingText("This expression is used in an unexpected way:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addText("It has the type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(owned_actual);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        if (types.from_annotation) {
            try report.document.addText("But the type annotation says it should have the type:");
        } else {
            try report.document.addText("But I expected it to be:");
        }
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(owned_expected);

        return report;
    }

    /// Build a report for incompatible list elements
    fn buildIncompatibleListElementsReport(
        self: *Self,
        types: TypePair,
        data: IncompatibleListElements,
    ) !Report {
        var report = Report.init(self.gpa, "INCOMPATIBLE LIST ELEMENTS", .runtime_error);
        errdefer report.deinit();

        // Create owned strings
        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));
        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.incompatible_elem_index);
        const expected_type_ordinal = try report.addOwnedString(self.bytes_buf.items);

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.incompatible_elem_index + 1);
        const actual_type_ordinal = try report.addOwnedString(self.bytes_buf.items);

        // Add description
        if (data.list_length == 2) {
            // Special case for lists with exactly 2 elements
            try report.document.addText("The two elements in this list have incompatible types:");
        } else if (data.incompatible_elem_index == 1) {
            // Special case for first two elements in longer lists
            try report.document.addText("The first two elements in this list have incompatible types:");
        } else {
            try report.document.addText("The ");
            try report.document.addText(expected_type_ordinal);
            try report.document.addText(" and ");
            try report.document.addText(actual_type_ordinal);
            try report.document.addText(" elements in this list have incompatible types:");
        }
        try report.document.addLineBreak();

        // Determine the overall region that encompasses both elements
        const actual_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const expected_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.last_elem_idx)));
        const overall_start_offset = @min(actual_region.start.offset, expected_region.start.offset);
        const overall_end_offset = @max(actual_region.end.offset, expected_region.end.offset);

        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Get region info for both elements
        const actual_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            actual_region.start.offset,
            actual_region.end.offset,
        ) catch return report;

        const expected_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            expected_region.start.offset,
            expected_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, overall_region_info.calculateLineText(self.source, self.module_env.getLineStarts())) catch return report,
            .start_line = overall_region_info.start_line_idx + 1,
            .start_column = overall_region_info.start_col_idx + 1,
            .end_line = overall_region_info.end_line_idx + 1,
            .end_column = overall_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };

        // Create underline regions
        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = expected_region_info.start_line_idx + 1,
                .start_column = expected_region_info.start_col_idx + 1,
                .end_line = expected_region_info.end_line_idx + 1,
                .end_column = expected_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
            .{
                .start_line = actual_region_info.start_line_idx + 1,
                .start_column = actual_region_info.start_col_idx + 1,
                .end_line = actual_region_info.end_line_idx + 1,
                .end_column = actual_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
        try report.document.addLineBreak();

        // Show the type of the first element
        try report.document.addText("The ");
        try report.document.addText(expected_type_ordinal);
        try report.document.addText(" element has this type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the type of the second element
        try report.document.addText("However, the ");
        try report.document.addText(actual_type_ordinal);
        try report.document.addText(" element has this type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // TODO we should categorize this as a tip/hint (maybe relevant to how editors display it)
        try report.document.addText("All elements in a list must have compatible types.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        // TODO link to a speceific explanation of how to mix element types using tag unions
        try report.document.addText("Note: You can wrap each element in a tag to make them compatible.");
        try report.document.addLineBreak();
        try report.document.addText("To learn about tags, see ");
        try report.document.addLink("https://www.roc-lang.org/tutorial#tags");

        return report;
    }

    /// Build a report for incompatible list elements
    fn buildInvalidIfCondition(
        self: *Self,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID IF CONDITION", .runtime_error);
        errdefer report.deinit();

        // Create owned strings
        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));

        // Add description
        try report.document.addText("This ");
        try report.document.addAnnotated("if", .keyword);
        try report.document.addText(" condition needs to be a ");
        try report.document.addAnnotated("Bool", .type_variable);
        try report.document.addText(":");
        try report.document.addLineBreak();

        // Get the region info for the invalid condition
        const actual_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const actual_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            actual_region.start.offset,
            actual_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, actual_region_info.calculateLineText(self.source, self.module_env.getLineStarts())) catch return report,
            .start_line = actual_region_info.start_line_idx + 1,
            .start_column = actual_region_info.start_col_idx + 1,
            .end_line = actual_region_info.end_line_idx + 1,
            .end_column = actual_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };

        // Create underline regions
        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = actual_region_info.start_line_idx + 1,
                .start_column = actual_region_info.start_col_idx + 1,
                .end_line = actual_region_info.end_line_idx + 1,
                .end_column = actual_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
        try report.document.addLineBreak();

        // Add description
        try report.document.addText("Right now, it has the type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();

        // Add explanation
        try report.document.addLineBreak();
        try report.document.addText("Every ");
        try report.document.addAnnotated("if", .keyword);
        try report.document.addText(" condition must evaluate to a ");
        try report.document.addAnnotated("Bool", .type_variable);
        try report.document.addText("–either ");
        try report.document.addAnnotated("True", .tag_name);
        try report.document.addText(" or ");
        try report.document.addAnnotated("False", .tag_name);
        try report.document.addText(".");

        return report;
    }

    /// Build a report for statement expressions that don't return {}
    fn buildStatementNotUnit(
        self: *Self,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "UNUSED VALUE", .runtime_error);
        errdefer report.deinit();

        // Create owned strings
        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));

        // Add description
        try report.document.addReflowingText("This expression produces a value, but it's not being used:");
        try report.document.addLineBreak();

        // Get the region info for the expression
        const actual_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const actual_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            actual_region.start.offset,
            actual_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, actual_region_info.calculateLineText(self.source, self.module_env.getLineStarts())) catch return report,
            .start_line = actual_region_info.start_line_idx + 1,
            .start_column = actual_region_info.start_col_idx + 1,
            .end_line = actual_region_info.end_line_idx + 1,
            .end_column = actual_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };

        // Create underline regions
        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = actual_region_info.start_line_idx + 1,
                .start_column = actual_region_info.start_col_idx + 1,
                .end_line = actual_region_info.end_line_idx + 1,
                .end_column = actual_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
        try report.document.addLineBreak();

        // Show the type
        try report.document.addReflowingText("It has the type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();

        // Add explanation
        try report.document.addLineBreak();
        try report.document.addReflowingText("Since this expression is used as a statement, it must evaluate to ");
        try report.document.addAnnotated("{}", .type_variable);
        try report.document.addText(". ");
        try report.document.addReflowingText("If you don't need the value, you can ignore it with ");
        try report.document.addAnnotated("_ =", .keyword);
        try report.document.addText(".");

        return report;
    }

    /// Build a report for incompatible list elements
    fn buildIncompatibleIfBranches(
        self: *Self,
        types: TypePair,
        data: IncompatibleIfBranches,
    ) !Report {
        // The first branch of an if statement can never be invalid, since that
        // branch determines the type of the entire expression
        std.debug.assert(data.problem_branch_index > 0);

        // Is this error for a if statement with only 2 branches?
        const is_only_if_else = data.num_branches == 2;

        var report = Report.init(self.gpa, "INCOMPATIBLE IF BRANCHES", .runtime_error);
        errdefer report.deinit();

        // Create owned strings
        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.problem_branch_index + 1);
        const branch_ordinal = try report.addOwnedString(self.bytes_buf.items);

        // Add title
        if (is_only_if_else) {
            try report.document.addText("This ");
            try report.document.addAnnotated("if", .keyword);
            try report.document.addText(" has an ");
            try report.document.addAnnotated("else", .keyword);
            try report.document.addText(" branch with a different type from it's ");
            try report.document.addAnnotated("then", .keyword);
            try report.document.addText(" branch:");
        } else {
            try report.document.addText("The type of the ");
            try report.document.addText(branch_ordinal);
            try report.document.addText(" branch of this ");
            try report.document.addAnnotated("if", .keyword);
            try report.document.addText(" does not match the previous branches:");
        }
        try report.document.addLineBreak();

        // Determine the overall region that encompasses both elements
        const last_if_branch_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.last_if_branch)));

        // TODO: getExprSpecific will panic if actual_var is not an Expr
        // It _should_ always be, but we should handle this better so it don't blow up
        const zoomed_in_var = self.can_ir.store.getExprSpecific(@enumFromInt(@intFromEnum(types.actual_var)));
        const actual_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(zoomed_in_var)));

        const overall_start_offset = @min(last_if_branch_region.start.offset, actual_region.start.offset);
        const overall_end_offset = @max(last_if_branch_region.end.offset, actual_region.end.offset);

        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Get region info for invalid branch
        const actual_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            actual_region.start.offset,
            actual_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, overall_region_info.calculateLineText(self.source, self.module_env.getLineStarts())) catch return report,
            .start_line = overall_region_info.start_line_idx + 1,
            .start_column = overall_region_info.start_col_idx + 1,
            .end_line = overall_region_info.end_line_idx + 1,
            .end_column = overall_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };

        // Create underline regions
        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = actual_region_info.start_line_idx + 1,
                .start_column = actual_region_info.start_col_idx + 1,
                .end_line = actual_region_info.end_line_idx + 1,
                .end_column = actual_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
        try report.document.addLineBreak();

        // Show the type of the invalid branch
        if (is_only_if_else) {
            try report.document.addText("The ");
            try report.document.addAnnotated("else", .keyword);
            try report.document.addText(" branch has the type:");
        } else {
            try report.document.addText("The ");
            try report.document.addText(branch_ordinal);
            try report.document.addText(" branch has this type:");
        }
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the type of the other branches
        if (is_only_if_else) {
            try report.document.addText("But the ");
            try report.document.addAnnotated("then", .keyword);
            try report.document.addText(" branch has the type:");
        } else if (data.problem_branch_index == 1) {
            try report.document.addText("But the previous branch has this type:");
        } else {
            try report.document.addText("But all the previous branches have this type:");
        }
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // TODO we should categorize this as a tip/hint (maybe relevant to how editors display it)
        try report.document.addText("All branches in an ");
        try report.document.addAnnotated("if", .keyword);
        try report.document.addText(" must have compatible types.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        // TODO link to a speceific explanation of how to mix element types using tag unions
        try report.document.addText("Note: You can wrap branches in a tag to make them compatible.");
        try report.document.addLineBreak();
        try report.document.addText("To learn about tags, see ");
        try report.document.addLink("https://www.roc-lang.org/tutorial#tags");

        return report;
    }

    /// Build a report for the pattern in a match is a different type than the
    /// match condition expr. e.g. match True { "hello" => ... }
    fn buildIncompatibleMatchCondPattern(
        self: *Self,
        types: TypePair,
        data: IncompatibleMatchCondPattern,
    ) !Report {
        var report = Report.init(self.gpa, "INCOMPATIBLE MATCH PATTERNS", .runtime_error);
        errdefer report.deinit();

        // Create owned strings
        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

        try report.document.addText("The first pattern in this ");
        try report.document.addAnnotated("match", .keyword);
        try report.document.addText(" is incompatible:");
        try report.document.addLineBreak();

        // Determine the overall region that encompasses both elements
        const match_expr_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.match_expr)));
        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            match_expr_region.start.offset,
            match_expr_region.end.offset,
        ) catch return report;

        // Get region info for invalid branch
        const invalid_var_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const invalid_var_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            invalid_var_region.start.offset,
            invalid_var_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, overall_region_info.calculateLineText(self.source, self.module_env.getLineStarts())) catch return report,
            .start_line = overall_region_info.start_line_idx + 1,
            .start_column = overall_region_info.start_col_idx + 1,
            .end_line = overall_region_info.end_line_idx + 1,
            .end_column = overall_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };

        // Create underline regions
        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = invalid_var_region_info.start_line_idx + 1,
                .start_column = invalid_var_region_info.start_col_idx + 1,
                .end_line = invalid_var_region_info.end_line_idx + 1,
                .end_column = invalid_var_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
        try report.document.addLineBreak();

        // Show the type of the invalid branch
        try report.document.addText("The first pattern has the type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the type of the other branches
        try report.document.addText("But the expression between the ");
        try report.document.addAnnotated("match", .keyword);
        try report.document.addText(" parenthesis has the type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("These two types can never match!");

        return report;
    }

    /// Build a report for incompatible match branches
    fn buildIncompatibleMatchPatterns(
        self: *Self,
        types: TypePair,
        data: IncompatibleMatchPatterns,
    ) !Report {
        var report = Report.init(self.gpa, "INCOMPATIBLE MATCH PATTERNS", .runtime_error);
        errdefer report.deinit();

        // Create owned strings
        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.problem_branch_index + 1);
        const branch_ord = try report.addOwnedString(self.bytes_buf.items);

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.problem_pattern_index + 1);
        const pattern_ord = try report.addOwnedString(self.bytes_buf.items);

        // Add description
        if (data.num_patterns > 1) {
            try report.document.addText("The pattern ");
            try report.document.addText(pattern_ord);
            try report.document.addText(" pattern in this ");
            try report.document.addText(branch_ord);
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" differs from previous ones:");
            try report.document.addLineBreak();
        } else {
            try report.document.addText("The pattern in the ");
            try report.document.addText(branch_ord);
            try report.document.addText(" branch of this ");
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" differs from previous ones:");
            try report.document.addLineBreak();
        }

        // Determine the overall region that encompasses both elements
        const match_expr_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.match_expr)));
        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            match_expr_region.start.offset,
            match_expr_region.end.offset,
        ) catch return report;

        // Get region info for invalid branch
        const invalid_var_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const invalid_var_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            invalid_var_region.start.offset,
            invalid_var_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, overall_region_info.calculateLineText(self.source, self.module_env.getLineStarts())) catch return report,
            .start_line = overall_region_info.start_line_idx + 1,
            .start_column = overall_region_info.start_col_idx + 1,
            .end_line = overall_region_info.end_line_idx + 1,
            .end_column = overall_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };

        // Create underline regions
        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = invalid_var_region_info.start_line_idx + 1,
                .start_column = invalid_var_region_info.start_col_idx + 1,
                .end_line = invalid_var_region_info.end_line_idx + 1,
                .end_column = invalid_var_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
        try report.document.addLineBreak();

        // Show the type of the invalid branch
        try report.document.addText("The ");
        try report.document.addText(branch_ord);
        try report.document.addText(" pattern has this type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the type of the other branches
        if (data.num_branches > 2) {
            try report.document.addText("But all the previous patterns have this type: ");
        } else {
            try report.document.addText("But the other pattern has this type:");
        }
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // TODO we should categorize this as a tip/hint (maybe relevant to how editors display it)
        try report.document.addText("All patterns in an ");
        try report.document.addAnnotated("match", .keyword);
        try report.document.addText(" must have compatible types.");

        return report;
    }

    /// Build a report for incompatible match branches
    fn buildIncompatibleMatchBranches(
        self: *Self,
        types: TypePair,
        data: IncompatibleMatchBranches,
    ) !Report {
        // The 1st branch can never be incompatible
        std.debug.assert(data.problem_branch_index > 0);

        var report = Report.init(self.gpa, "INCOMPATIBLE MATCH BRANCHES", .runtime_error);
        errdefer report.deinit();

        // Create owned strings
        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.problem_branch_index + 1);
        const branch_ord = try report.addOwnedString(self.bytes_buf.items);

        // Add description
        if (data.num_branches == 2) {
            try report.document.addText("The second branch's type in this ");
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" is different from the first branch:");
        } else {
            try report.document.addText("The ");
            try report.document.addText(branch_ord);
            try report.document.addText(" branch's type in this ");
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" is different from the previous ones:");
        }
        try report.document.addLineBreak();

        // Determine the overall region that encompasses both elements
        const expr_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.match_expr)));
        const this_branch_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));

        const overall_start_offset = expr_region.start.offset;
        const overall_end_offset = this_branch_region.end.offset;

        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, overall_region_info.calculateLineText(self.source, self.module_env.getLineStarts())) catch return report,
            .start_line = overall_region_info.start_line_idx + 1,
            .start_column = overall_region_info.start_col_idx + 1,
            .end_line = overall_region_info.end_line_idx + 1,
            .end_column = overall_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };

        // Create underline regions
        const this_branch_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            this_branch_region.start.offset,
            this_branch_region.end.offset,
        ) catch return report;
        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = this_branch_region_info.start_line_idx + 1,
                .start_column = this_branch_region_info.start_col_idx + 1,
                .end_line = this_branch_region_info.end_line_idx + 1,
                .end_column = this_branch_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
        try report.document.addLineBreak();

        // Show the type of the invalid branch
        try report.document.addText("The ");
        try report.document.addText(branch_ord);
        try report.document.addText(" branch has this type;");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the type of the other branches
        if (data.num_branches == 2) {
            try report.document.addText("But the first branch has this type: ");
        } else if (data.problem_branch_index == data.num_branches - 1) {
            try report.document.addText("But all the previous branches have this type:");
        } else {
            try report.document.addText("But the previous branch has this type:");
        }
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // TODO we should categorize this as a tip/hint (maybe relevant to how editors display it)
        try report.document.addText("All branches in an ");
        try report.document.addAnnotated("match", .keyword);
        try report.document.addText(" must have compatible types.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        // TODO link to a speceific explanation of how to mix element types using tag unions
        try report.document.addText("Note: You can wrap branches values in a tag to make them compatible.");
        try report.document.addLineBreak();
        try report.document.addText("To learn about tags, see ");
        try report.document.addLink("https://www.roc-lang.org/tutorial#tags");

        return report;
    }

    /// Build a report for non-exhaustive match
    fn buildNonExhaustiveMatch(
        self: *Self,
        data: NonExhaustiveMatch,
    ) !Report {
        var report = Report.init(self.gpa, "NON-EXHAUSTIVE MATCH", .runtime_error);
        errdefer report.deinit();

        // Add description
        try report.document.addText("This ");
        try report.document.addAnnotated("match", .keyword);
        try report.document.addText(" expression may not handle all possible values:");
        try report.document.addLineBreak();

        // Get the match expression region
        const match_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.match_expr)));
        const match_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            match_region.start.offset,
            match_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, match_region_info.calculateLineText(self.source, self.module_env.getLineStarts())) catch return report,
            .start_line = match_region_info.start_line_idx + 1,
            .start_column = match_region_info.start_col_idx + 1,
            .end_line = match_region_info.end_line_idx + 1,
            .end_column = match_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };

        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = match_region_info.start_line_idx + 1,
                .start_column = match_region_info.start_col_idx + 1,
                .end_line = match_region_info.end_line_idx + 1,
                .end_column = match_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
        try report.document.addLineBreak();

        try report.document.addText("The value being matched has an open type (can contain additional tags),");
        try report.document.addLineBreak();
        try report.document.addText("but none of the branches is a catch-all pattern like ");
        try report.document.addAnnotated("_", .type_variable);
        try report.document.addText(".");
        try report.document.addLineBreak();

        return report;
    }

    /// Build a report for when the `?` operator is used on a non-Try type
    fn buildInvalidTryOperator(
        self: *Self,
        types: TypePair,
        data: InvalidTryOperator,
    ) !Report {
        var report = Report.init(self.gpa, "EXPECTED TRY TYPE", .runtime_error);
        errdefer report.deinit();

        // Create owned string for the actual type
        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));

        // Add description
        try report.document.addText("The ");
        try report.document.addAnnotated("?", .keyword);
        try report.document.addText(" operator expects a ");
        try report.document.addAnnotated("Try", .type_variable);
        try report.document.addText(" type (a tag union containing ONLY ");
        try report.document.addAnnotated("Ok", .type_variable);
        try report.document.addText(" and ");
        try report.document.addAnnotated("Err", .type_variable);
        try report.document.addText(" tags),");
        try report.document.addLineBreak();
        try report.document.addText("but I found:");
        try report.document.addLineBreak();

        // Get the expression region
        const expr_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.expr)));
        const expr_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            expr_region.start.offset,
            expr_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, expr_region_info.calculateLineText(self.source, self.module_env.getLineStarts())) catch return report,
            .start_line = expr_region_info.start_line_idx + 1,
            .start_column = expr_region_info.start_col_idx + 1,
            .end_line = expr_region_info.end_line_idx + 1,
            .end_column = expr_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };

        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = expr_region_info.start_line_idx + 1,
                .start_column = expr_region_info.start_col_idx + 1,
                .end_line = expr_region_info.end_line_idx + 1,
                .end_column = expr_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
        try report.document.addLineBreak();

        try report.document.addText("This expression has type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addAnnotated(actual_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("Tip: Maybe wrap a value using ");
        try report.document.addAnnotated("Ok(value)", .type_variable);
        try report.document.addText(" or ");
        try report.document.addAnnotated("Err(value)", .type_variable);
        try report.document.addText(".");

        return report;
    }

    /// Build a report for incompatible match branches
    fn buildInvalidBoolBinop(
        self: *Self,
        types: TypePair,
        data: InvalidBoolBinop,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID BOOL OPERATION", .runtime_error);
        errdefer report.deinit();

        // Create owned strings
        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));

        // Add description
        try report.document.addText("I'm having trouble with this bool operation:");
        try report.document.addLineBreak();

        // Determine the overall region that encompasses both elements
        const expr_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.binop_expr)));
        const problem_side_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));

        const overall_start_offset = expr_region.start.offset;
        const overall_end_offset = problem_side_region.end.offset;

        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, overall_region_info.calculateLineText(self.source, self.module_env.getLineStarts())) catch return report,
            .start_line = overall_region_info.start_line_idx + 1,
            .start_column = overall_region_info.start_col_idx + 1,
            .end_line = overall_region_info.end_line_idx + 1,
            .end_column = overall_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };

        // Create underline regions
        const this_branch_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            problem_side_region.start.offset,
            problem_side_region.end.offset,
        ) catch return report;
        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = this_branch_region_info.start_line_idx + 1,
                .start_column = this_branch_region_info.start_col_idx + 1,
                .end_line = this_branch_region_info.end_line_idx + 1,
                .end_column = this_branch_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
        try report.document.addLineBreak();

        // Show the type of the invalid branch
        try report.document.addText("Both sides of ");
        switch (data.binop) {
            .@"and" => try report.document.addAnnotated("and", .inline_code),
            .@"or" => try report.document.addAnnotated("or", .inline_code),
        }
        try report.document.addText(" must be ");
        try report.document.addAnnotated("Bool", .type_variable);
        try report.document.addText(" values, but the ");
        switch (data.problem_side) {
            .lhs => try report.document.addText("left"),
            .rhs => try report.document.addText("right"),
        }
        try report.document.addText(" side is:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // TODO we should categorize this as a tip/hint (maybe relevant to how editors display it)
        try report.document.addReflowingText("Note: Roc does not have \"truthiness\" where other values ");
        try report.document.addReflowingText("like strings, numbers or lists are automatically converted to bools. ");
        try report.document.addReflowingText("You must do that conversion yourself!");

        return report;
    }

    /// Build a report for incompatible match branches
    fn buildInvalidNominalTag(
        self: *Self,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID NOMINAL TAG", .runtime_error);
        errdefer report.deinit();

        // Create actual tag str
        const actual_content = self.snapshots.getContent(types.actual_snapshot);
        std.debug.assert(actual_content == .structure);
        std.debug.assert(actual_content.structure == .tag_union);
        std.debug.assert(actual_content.structure.tag_union.tags.len() == 1);
        const actual_tag = self.snapshots.tags.get(actual_content.structure.tag_union.tags.start);
        const actual_tag_str = try report.addOwnedString(snapshot.Store.getFormattedTagString(actual_tag));

        // Create expected tag str
        const expected_content = self.snapshots.getContent(types.expected_snapshot);
        std.debug.assert(expected_content == .structure);
        std.debug.assert(expected_content.structure == .tag_union);
        const expected_num_tags_str = expected_content.structure.tag_union.tags.len();

        // Add description
        try report.document.addText("I'm having trouble with this nominal tag:");
        try report.document.addLineBreak();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        // Show the invalid tag
        try report.document.addText("The tag is:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_tag_str);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the expected tags
        if (expected_num_tags_str == 1) {
            const expected_tag = self.snapshots.tags.get(expected_content.structure.tag_union.tags.start);
            const expected_tag_str = try report.addOwnedString(snapshot.Store.getFormattedTagString(expected_tag));

            try report.document.addText("But the nominal type needs it to be:");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addCodeBlock(expected_tag_str);
        } else {
            const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

            try report.document.addText("But the nominal type needs it to one of:");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addCodeBlock(expected_type);

            // Check if there's a tag with the same name in the list of possible tags

            var iter = expected_content.structure.tag_union.tags.iterIndices();
            while (iter.next()) |tag_index| {
                const cur_expected_tag = self.snapshots.tags.get(tag_index);

                if (actual_tag.name == cur_expected_tag.name) {
                    const cur_expected_tag_str = try report.addOwnedString(snapshot.Store.getFormattedTagString(cur_expected_tag));

                    try report.document.addLineBreak();
                    try report.document.addLineBreak();
                    try report.document.addAnnotated("Hint:", .emphasized);
                    try report.document.addReflowingText(" The nominal type has a tag with the same name, but different args:");
                    try report.document.addLineBreak();
                    try report.document.addCodeBlock(cur_expected_tag_str);

                    break;
                }
            }
        }

        return report;
    }

    /// Build a report for invalid nominal record (record fields don't match)
    fn buildInvalidNominalRecord(
        self: *Self,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID NOMINAL RECORD", .runtime_error);
        errdefer report.deinit();

        try report.document.addText("I'm having trouble with this nominal type that wraps a record:");
        try report.document.addLineBreak();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

        try report.document.addText("The record I found is:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("But the nominal type expects:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);

        return report;
    }

    /// Build a report for invalid nominal tuple (tuple elements don't match)
    fn buildInvalidNominalTuple(
        self: *Self,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID NOMINAL TUPLE", .runtime_error);
        errdefer report.deinit();

        try report.document.addText("I'm having trouble with this nominal type that wraps a tuple:");
        try report.document.addLineBreak();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

        try report.document.addText("The tuple I found is:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("But the nominal type expects:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);

        return report;
    }

    /// Build a report for invalid nominal value (value type doesn't match)
    fn buildInvalidNominalValue(
        self: *Self,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID NOMINAL TYPE", .runtime_error);
        errdefer report.deinit();

        try report.document.addText("I'm having trouble with this nominal type:");
        try report.document.addLineBreak();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(types.actual_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));

        try report.document.addText("The value I found has type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("But the nominal type expects:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);

        return report;
    }

    /// Build a report for function argument type mismatch
    fn buildIncompatibleFnCallArg(
        self: *Self,
        types: TypePair,
        data: IncompatibleFnCallArg,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);
        errdefer report.deinit();

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.incompatible_arg_index + 1);
        const arg_index = try report.addOwnedString(self.bytes_buf.items);

        // Extract only the argument types from the function snapshots
        const actual_arg_type = self.extractFirstArgTypeFromFunctionSnapshot(types.actual_snapshot) orelse types.actual_snapshot;
        const expected_arg_type = self.extractFirstArgTypeFromFunctionSnapshot(types.expected_snapshot) orelse types.expected_snapshot;

        const actual_type = try report.addOwnedString(self.getFormattedString(actual_arg_type));
        const expected_type = try report.addOwnedString(self.getFormattedString(expected_arg_type));

        try report.document.addText("The ");
        try report.document.addText(arg_index);
        try report.document.addText(" argument being passed to this function has the wrong type:");
        try report.document.addLineBreak();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.arg_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("This argument has the type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addReflowingText("But ");
        if (data.fn_name) |fn_name_ident| {
            const fn_name = try report.addOwnedString(self.can_ir.getIdent(fn_name_ident));
            try report.document.addAnnotated(fn_name, .inline_code);
        } else {
            try report.document.addReflowingText("the function");
        }
        try report.document.addReflowingText(" needs the ");
        try report.document.addReflowingText(arg_index);
        try report.document.addReflowingText(" argument to be:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);

        return report;
    }

    fn buildIncompatibleFnArgsBoundVar(
        self: *Self,
        types: TypePair,
        data: IncompatibleFnArgsBoundVar,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);
        errdefer report.deinit();

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.first_arg_index + 1);
        const first_arg_index = try report.addOwnedString(self.bytes_buf.items);

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.second_arg_index + 1);
        const second_arg_index = try report.addOwnedString(self.bytes_buf.items);

        // The types from unification already have the correct snapshots
        // expected = first argument, actual = second argument
        const first_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));
        const second_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));

        try report.document.addText("The ");
        try report.document.addText(first_arg_index);
        try report.document.addText(" and ");
        try report.document.addText(second_arg_index);
        try report.document.addText(" arguments to ");
        if (data.fn_name) |fn_name_ident| {
            try report.document.addText("`");
            try report.document.addText(self.can_ir.getIdent(fn_name_ident));
            try report.document.addText("`");
        } else {
            try report.document.addText("this function");
        }
        try report.document.addText(" must have compatible types, but they are incompatible in this call:");
        try report.document.addLineBreak();

        // Highlight both arguments in a single code block with underlines
        const first_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.first_arg_var)));
        const first_region_info = self.module_env.calcRegionInfo(first_region.*);

        const second_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.second_arg_var)));
        const second_region_info = self.module_env.calcRegionInfo(second_region.*);

        // Determine the overall display region (should include both arguments)
        const start_line = @min(first_region_info.start_line_idx, second_region_info.start_line_idx);
        const end_line = @max(first_region_info.end_line_idx, second_region_info.end_line_idx);
        const start_col = if (start_line == first_region_info.start_line_idx) first_region_info.start_col_idx else 0;
        const end_col = if (end_line == second_region_info.end_line_idx) second_region_info.end_col_idx else 0;

        // Get the line text that covers both arguments
        const line_text = base.RegionInfo.getLineText(self.source, self.module_env.getLineStarts(), start_line, end_line);

        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, line_text) catch return report,
            .start_line = start_line + 1, // Convert to 1-based
            .start_column = start_col + 1,
            .end_line = end_line + 1,
            .end_column = end_col + 1,
            .region_annotation = .error_highlight,
            .filename = self.filename,
        };

        // Create underline regions for both arguments (convert to 1-based coordinates)
        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = first_region_info.start_line_idx + 1,
                .start_column = first_region_info.start_col_idx + 1,
                .end_line = first_region_info.end_line_idx + 1,
                .end_column = first_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
            .{
                .start_line = second_region_info.start_line_idx + 1,
                .start_column = second_region_info.start_col_idx + 1,
                .end_line = second_region_info.end_line_idx + 1,
                .end_column = second_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        // Show both arguments with underlines in one code block
        try report.document.addSourceCodeWithUnderlines(
            display_region,
            &underline_regions,
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("The ");
        try report.document.addText(first_arg_index);
        try report.document.addReflowingText(" argument has the type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(first_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addReflowingText("But the ");
        try report.document.addText(second_arg_index);
        try report.document.addReflowingText(" argument has the type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(second_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        if (data.fn_name) |fn_name_ident| {
            const fn_name = try report.addOwnedString(self.can_ir.getIdent(fn_name_ident));
            try report.document.addAnnotated(fn_name, .inline_code);
        } else {
            try report.document.addReflowingText("this function");
        }
        try report.document.addReflowingText(" needs these arguments to have compatible types.");

        return report;
    }

    // annotation problems //

    /// Build a report for "number does not fit in type" diagnostic
    fn buildTypeApplyArityMismatchReport(
        self: *Self,
        data: TypeApplyArityMismatch,
    ) !Report {
        const title = blk: {
            if (data.num_expected_args > data.num_actual_args) {
                break :blk "TOO FEW ARGS";
            } else if (data.num_expected_args < data.num_actual_args) {
                break :blk "TOO MANY ARGS";
            } else {
                break :blk "WRONG NUMBER OF ARGS";
            }
        };
        var report = Report.init(self.gpa, title, .runtime_error);
        errdefer report.deinit();

        // Look up display name in import mapping (handles auto-imported builtin types)
        // If the type_name is in the mapping (e.g., "Builtin.Bool"), use the mapped display name ("Bool")
        // Otherwise, use the identifier as-is
        const type_name_ident = if (self.import_mapping.get(data.type_name)) |display_ident|
            self.can_ir.getIdent(display_ident)
        else
            self.can_ir.getIdent(data.type_name);
        const type_name = try report.addOwnedString(type_name_ident);

        self.bytes_buf.clearRetainingCapacity();
        try self.bytes_buf.writer().print("{d}", .{data.num_expected_args});
        const num_expected_args = try report.addOwnedString(self.bytes_buf.items);

        self.bytes_buf.clearRetainingCapacity();
        try self.bytes_buf.writer().print("{d}", .{data.num_actual_args});
        const num_actual_args = try report.addOwnedString(self.bytes_buf.items);

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);

        try report.document.addReflowingText("The type ");
        try report.document.addAnnotated(type_name, .type_variable);
        try report.document.addReflowingText(" expects ");
        try report.document.addReflowingText(num_expected_args);
        try report.document.addReflowingText(pluralize(data.num_expected_args, " argument, but got ", " arguments, but got "));
        try report.document.addReflowingText(num_actual_args);
        try report.document.addReflowingText(" instead.");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );

        return report;
    }

    /// Build a report for function call arity mismatch
    fn buildFnCallArityMismatchReport(
        self: *Self,
        data: FnCallArityMismatch,
    ) !Report {
        const title = blk: {
            if (data.expected_args > data.actual_args) {
                break :blk "TOO FEW ARGUMENTS";
            } else if (data.expected_args < data.actual_args) {
                break :blk "TOO MANY ARGUMENTS";
            } else {
                break :blk "WRONG NUMBER OF ARGUMENTS";
            }
        };
        var report = Report.init(self.gpa, title, .runtime_error);
        errdefer report.deinit();

        self.bytes_buf.clearRetainingCapacity();
        try self.bytes_buf.writer().print("{d}", .{data.expected_args});
        const num_expected = try report.addOwnedString(self.bytes_buf.items);

        self.bytes_buf.clearRetainingCapacity();
        try self.bytes_buf.writer().print("{d}", .{data.actual_args});
        const num_actual = try report.addOwnedString(self.bytes_buf.items);

        // Build the function type string from the snapshot
        const fn_type = try report.addOwnedString(self.getFormattedString(data.fn_snapshot));

        // Start the error message
        if (data.fn_name) |fn_name_ident| {
            const fn_name = try report.addOwnedString(self.can_ir.getIdent(fn_name_ident));
            try report.document.addReflowingText("The function ");
            try report.document.addAnnotated(fn_name, .inline_code);
        } else {
            try report.document.addReflowingText("This function");
        }
        try report.document.addReflowingText(" expects ");
        try report.document.addReflowingText(num_expected);
        try report.document.addReflowingText(pluralize(data.expected_args, " argument, but ", " arguments, but "));
        try report.document.addReflowingText(num_actual);
        try report.document.addReflowingText(pluralize(data.actual_args, " was provided:", " were provided:"));
        try report.document.addLineBreak();

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.call_region);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        // Show the function signature
        try report.document.addReflowingText("The function has the signature:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(fn_type);

        return report;
    }

    /// Build a report for when a type alias references itself recursively
    fn buildRecursiveAliasReport(
        self: *Self,
        data: RecursiveAlias,
    ) !Report {
        var report = Report.init(self.gpa, "RECURSIVE ALIAS", .runtime_error);
        errdefer report.deinit();

        // Look up display name in import mapping (handles auto-imported builtin types)
        const type_name_ident = if (self.import_mapping.get(data.type_name)) |display_ident|
            self.can_ir.getIdent(display_ident)
        else
            self.can_ir.getIdent(data.type_name);
        const type_name = try report.addOwnedString(type_name_ident);

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);

        try report.document.addReflowingText("The type alias ");
        try report.document.addAnnotated(type_name, .type_variable);
        try report.document.addReflowingText(" references itself, which is not allowed:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("Type aliases cannot be recursive. If you need a recursive type, use a nominal type (:=) instead of an alias (:).");

        return report;
    }

    /// Build a report for when alias syntax is used in a where clause
    /// This syntax was used for abilities which have been removed
    fn buildUnsupportedAliasWhereClauseReport(
        self: *Self,
        data: UnsupportedAliasWhereClause,
    ) !Report {
        var report = Report.init(self.gpa, "UNSUPPORTED WHERE CLAUSE", .runtime_error);
        errdefer report.deinit();

        const alias_name = try report.addOwnedString(self.can_ir.getIdent(data.alias_name));

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);

        try report.document.addReflowingText("The where clause syntax ");
        try report.document.addAnnotated(alias_name, .type_variable);
        try report.document.addReflowingText(" is not supported:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("This syntax was used for abilities, which have been removed from Roc. Use method constraints like `where [a.methodName(args) -> ret]` instead.");

        return report;
    }

    /// Build a report for infinite type recursion (like `a = List(a)`)
    fn buildInfiniteRecursionReport(self: *Self, data: VarWithSnapshot) !Report {
        var report = Report.init(self.gpa, "INFINITE TYPE", .runtime_error);
        errdefer report.deinit();

        const type_str = try report.addOwnedString(self.getFormattedString(data.snapshot));
        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.var_)));
        const region_info = self.module_env.calcRegionInfo(region.*);

        try report.document.addReflowingText("This type definition is infinitely recursive:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("The type expands to:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(type_str);
        try report.document.addLineBreak();

        try report.document.addReflowingText("This creates an infinitely large type. If you need recursion, use a nominal type (defined with ");
        try report.document.addAnnotated(":=", .inline_code);
        try report.document.addReflowingText(") to break the cycle.");

        return report;
    }

    /// Build a report for anonymous type recursion (recursive without going through a nominal type)
    fn buildAnonymousRecursionReport(self: *Self, data: VarWithSnapshot) !Report {
        var report = Report.init(self.gpa, "RECURSIVE TYPE WITHOUT NAME", .runtime_error);
        errdefer report.deinit();

        const type_str = try report.addOwnedString(self.getFormattedString(data.snapshot));
        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.var_)));
        const region_info = self.module_env.calcRegionInfo(region.*);

        try report.document.addReflowingText("This type is recursive, but doesn't go through a named type:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("The type is:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(type_str);
        try report.document.addLineBreak();

        try report.document.addReflowingText("Recursive types must use a nominal type (defined with ");
        try report.document.addAnnotated(":=", .inline_code);
        try report.document.addReflowingText(") to give the recursion a name.");

        return report;
    }

    /// Build a report for internal compiler bugs
    fn buildBugReport(self: *Self, data: Bug) !Report {
        var report = Report.init(self.gpa, "COMPILER BUG", .runtime_error);
        errdefer report.deinit();

        const expected_str = try report.addOwnedString(self.getFormattedString(data.expected));
        const actual_str = try report.addOwnedString(self.getFormattedString(data.actual));
        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.actual_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);

        try report.document.addReflowingText("The Roc compiler encountered an internal error while type checking this code:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("Expected type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_str);
        try report.document.addLineBreak();

        try report.document.addReflowingText("Actual type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_str);
        try report.document.addLineBreak();

        try report.document.addReflowingText("This is a bug in the Roc compiler. Please report it at ");
        try report.document.addAnnotated("https://github.com/roc-lang/roc/issues", .inline_code);
        try report.document.addReflowingText(" with the code that triggered this error.");

        return report;
    }

    // static dispatch //

    /// Build a report for when a type is not nominal, but you're trying to
    /// static dispatch on it
    fn buildStaticDispatchDispatcherNotNominal(
        self: *Self,
        data: DispatcherNotNominal,
    ) !Report {
        var report = Report.init(self.gpa, "MISSING METHOD", .runtime_error);
        errdefer report.deinit();

        const snapshot_str = try report.addOwnedString(self.getFormattedString(data.dispatcher_snapshot));

        const method_name_str = try report.addOwnedString(self.can_ir.getIdentText(data.method_name));

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.fn_var)));

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(region.*);

        try report.document.addReflowingText("This ");
        try report.document.addAnnotated(method_name_str, .emphasized);
        try report.document.addReflowingText(" method is being called on a value whose type doesn't have that method:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("The value's type, which does not have a method named ");
        try report.document.addAnnotated(method_name_str, .emphasized);
        try report.document.addReflowingText(", is:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(snapshot_str);

        return report;
    }

    /// Build a report for when a type doesn't have the expected static dispatch
    /// method
    fn buildStaticDispatchDispatcherDoesNotImplMethod(
        self: *Self,
        data: DispatcherDoesNotImplMethod,
    ) !Report {
        // Special case: number literal being used where a non-number type is expected
        if (data.origin == .from_numeral) {
            return self.buildNumberUsedAsNonNumber(data);
        }

        var report = Report.init(self.gpa, "MISSING METHOD", .runtime_error);
        errdefer report.deinit();

        const snapshot_str = try report.addOwnedString(self.getFormattedString(data.dispatcher_snapshot));

        const method_name_str = try report.addOwnedString(self.can_ir.getIdentText(data.method_name));

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.fn_var)));

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(region.*);

        // Check if this method corresponds to an operator (using ident index comparison, not strings)
        const is_from_binop = data.origin == .desugared_binop;
        const mb_operator = self.getOperatorForMethod(data.method_name);

        if (is_from_binop) {
            if (mb_operator) |operator| {
                try report.document.addReflowingText("The value before this ");
                try report.document.addAnnotated(operator, .emphasized);
                try report.document.addReflowingText(" operator has a type that doesn't have a ");
                try report.document.addAnnotated(method_name_str, .emphasized);
                try report.document.addReflowingText(" method:");
            } else {
                try report.document.addReflowingText("This ");
                try report.document.addAnnotated(method_name_str, .emphasized);
                try report.document.addReflowingText(" method is being called on a value whose type doesn't have that method:");
            }
            try report.document.addLineBreak();
        } else {
            try report.document.addReflowingText("This ");
            try report.document.addAnnotated(method_name_str, .emphasized);
            try report.document.addReflowingText(" method is being called on a value whose type doesn't have that method:");
            try report.document.addLineBreak();
        }

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("The value's type, which does not have a method named ");
        try report.document.addAnnotated(method_name_str, .emphasized);
        try report.document.addReflowingText(", is:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(snapshot_str);

        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addAnnotated("Hint:", .emphasized);
        switch (data.dispatcher_type) {
            .nominal => {
                if (is_from_binop) {
                    if (mb_operator) |operator| {
                        try report.document.addReflowingText("The ");
                        try report.document.addAnnotated(operator, .emphasized);
                        try report.document.addReflowingText(" operator calls a method named ");
                        try report.document.addAnnotated(method_name_str, .emphasized);
                        try report.document.addReflowingText(" on the value preceding it, passing the value after the operator as the one argument.");
                    } else {
                        try report.document.addReflowingText(" For this to work, the type would need to have a method named ");
                        try report.document.addAnnotated(method_name_str, .emphasized);
                        try report.document.addReflowingText(" associated with it in the type's declaration.");
                    }
                } else {
                    try report.document.addReflowingText(" For this to work, the type would need to have a method named ");
                    try report.document.addAnnotated(method_name_str, .emphasized);
                    try report.document.addReflowingText(" associated with it in the type's declaration.");
                }
            },
            .rigid => {
                if (is_from_binop) {
                    if (mb_operator) |operator| {
                        try report.document.addReflowingText(" The ");
                        try report.document.addAnnotated(operator, .emphasized);
                        try report.document.addReflowingText(" operator requires the type to have a ");
                        try report.document.addAnnotated(method_name_str, .emphasized);
                        try report.document.addReflowingText(" method. Did you forget to specify it in the type annotation?");
                    } else {
                        try report.document.addReflowingText(" Did you forget to specify ");
                        try report.document.addAnnotated(method_name_str, .emphasized);
                        try report.document.addReflowingText(" in the type annotation?");
                    }
                } else {
                    try report.document.addReflowingText(" Did you forget to specify ");
                    try report.document.addAnnotated(method_name_str, .emphasized);
                    try report.document.addReflowingText(" in the type annotation?");
                }
            },
        }

        return report;
    }

    /// Build a report for when a number literal is used where a non-number type is expected
    fn buildNumberUsedAsNonNumber(
        self: *Self,
        data: DispatcherDoesNotImplMethod,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);
        errdefer report.deinit();

        const snapshot_str = try report.addOwnedString(self.getFormattedString(data.dispatcher_snapshot));

        // Get the region of the number literal from the num_literal info
        const num_literal = data.num_literal.?;
        const num_region = num_literal.region;
        const num_region_info = self.module_env.calcRegionInfo(num_region);

        // Get the region of the dispatcher (the type that was expected)
        // This might be different if the type came from somewhere else (e.g., a type annotation)
        const dispatcher_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.dispatcher_var))).*;

        try report.document.addReflowingText("This number is being used where a non-number type is needed:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            num_region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        // Check if we have a different origin region we can show
        if (dispatcher_region.start.offset != num_region.start.offset or
            dispatcher_region.end.offset != num_region.end.offset)
        {
            const dispatcher_region_info = self.module_env.calcRegionInfo(dispatcher_region);
            try report.document.addReflowingText("The type was determined to be non-numeric here:");
            try report.document.addLineBreak();

            try report.document.addSourceRegion(
                dispatcher_region_info,
                .error_highlight,
                self.filename,
                self.source,
                self.module_env.getLineStarts(),
            );
            try report.document.addLineBreak();
        }

        try report.document.addReflowingText("Other code expects this to have the type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(snapshot_str);

        return report;
    }

    /// Build a report for when an anonymous type doesn't support equality
    fn buildTypeDoesNotSupportEquality(
        self: *Self,
        data: TypeDoesNotSupportEquality,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE DOES NOT SUPPORT EQUALITY", .runtime_error);
        errdefer report.deinit();

        const snapshot_str = try report.addOwnedString(self.getFormattedString(data.dispatcher_snapshot));

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.fn_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);

        try report.document.addReflowingText("This expression is doing an equality check on a type that doesn't support equality:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("The type is:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(snapshot_str);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Get the content and explain which parts don't support equality
        const content = self.snapshots.getContent(data.dispatcher_snapshot);
        if (content == .structure) {
            switch (content.structure) {
                .record => |record| {
                    try self.explainRecordEqualityFailure(&report, record);
                },
                .tuple => |tuple| {
                    try self.explainTupleEqualityFailure(&report, tuple);
                },
                .tag_union => |tag_union| {
                    try self.explainTagUnionEqualityFailure(&report, tag_union);
                },
                .fn_pure, .fn_effectful, .fn_unbound => {
                    try report.document.addReflowingText("Functions cannot be compared for equality.");
                    try report.document.addLineBreak();
                },
                else => {},
            }
        }

        return report;
    }

    /// Build a report for when an anonymous type doesn't support equality
    fn buildCannotAccessOpaqueNominal(
        self: *Self,
        data: CannotAccessOpaqueNominal,
    ) !Report {
        var report = Report.init(self.gpa, "CANNOT USE OPAQUE NOMINAL TYPE", .runtime_error);
        errdefer report.deinit();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.var_)));
        const region_info = self.module_env.calcRegionInfo(region.*);

        try report.document.addReflowingText("You're attempting to create an instance of ");
        try report.document.addAnnotated(self.can_ir.getIdentText(data.nominal_type_name), .inline_code);
        try report.document.addReflowingText(", but it's an ");
        try report.document.addAnnotated("opaque", .emphasized);
        try report.document.addReflowingText(" type:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );

        try report.document.addLineBreak();
        try report.document.addAnnotated("Hint:", .emphasized);
        try report.document.addReflowingText(" To create an instance of this type outside the module it's defined it, you have to define it with ");
        try report.document.addAnnotated(":=", .emphasized);
        try report.document.addReflowingText(" instead of ");
        try report.document.addAnnotated("::", .emphasized);
        try report.document.addReflowingText(".");

        return report;
    }

    /// Build a report for when a nominal type variable doesn't resolve properly.
    /// This is a compiler bug - it should never happen in correctly compiled code.
    fn buildNominalTypeResolutionFailed(
        self: *Self,
        data: NominalTypeResolutionFailed,
    ) !Report {
        var report = Report.init(self.gpa, "COMPILER BUG", .runtime_error);
        errdefer report.deinit();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.var_)));
        const region_info = self.module_env.calcRegionInfo(region.*);

        try report.document.addReflowingText("An internal compiler error occurred while checking this nominal type usage:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );

        try report.document.addLineBreak();
        try report.document.addReflowingText("The nominal type declaration variable did not resolve to a nominal type structure. This indicates a bug in the Roc compiler. Please report this issue at https://github.com/roc-lang/roc/issues");

        return report;
    }

    /// Explain which record fields don't support equality
    fn explainRecordEqualityFailure(
        self: *Self,
        report: *Report,
        record: snapshot.SnapshotRecord,
    ) !void {
        const fields = self.snapshots.sliceRecordFields(record.fields);
        var has_problem_fields = false;

        // First pass: check if any fields don't support equality
        for (fields.items(.content)) |field_content_idx| {
            if (!self.snapshotSupportsEquality(field_content_idx)) {
                has_problem_fields = true;
                break;
            }
        }

        if (has_problem_fields) {
            try report.document.addReflowingText("This record does not support equality because these fields have types that don't support ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(":");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const field_names = fields.items(.name);
            const field_contents = fields.items(.content);
            for (field_names, field_contents) |name, field_content_idx| {
                if (!self.snapshotSupportsEquality(field_content_idx)) {
                    const field_name = self.can_ir.getIdentText(name);

                    const field_type_str = try report.addOwnedString(self.getFormattedString(field_content_idx));

                    try report.document.addText("    ");
                    try report.document.addAnnotated(field_name, .emphasized);
                    try report.document.addText(": ");
                    try report.document.addAnnotated(field_type_str, .type_variable);
                    try report.document.addLineBreak();

                    // Explain WHY this field doesn't support equality
                    _ = try self.explainWhyNoEquality(report, field_content_idx, "        ");
                }
            }
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" Anonymous records only have an ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" method if all of their fields have ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" methods.");
            try report.document.addLineBreak();
        }
    }

    /// Explain which tuple elements don't support equality
    fn explainTupleEqualityFailure(
        self: *Self,
        report: *Report,
        tuple: snapshot.SnapshotTuple,
    ) !void {
        const elems = self.snapshots.sliceVars(tuple.elems);
        var has_problem_elems = false;

        // First pass: check if any elements don't support equality
        for (elems) |elem_content_idx| {
            if (!self.snapshotSupportsEquality(elem_content_idx)) {
                has_problem_elems = true;
                break;
            }
        }

        if (has_problem_elems) {
            try report.document.addReflowingText("This tuple does not support equality because these elements have types that don't support ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(":");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            for (elems, 0..) |elem_content_idx, i| {
                if (!self.snapshotSupportsEquality(elem_content_idx)) {
                    const elem_type_str = try report.addOwnedString(self.getFormattedString(elem_content_idx));

                    try report.document.addText("    element ");
                    var buf: [20]u8 = undefined;
                    const index_str = std.fmt.bufPrint(&buf, "{}", .{i}) catch "?";
                    try report.document.addAnnotated(index_str, .emphasized);
                    try report.document.addText(": ");
                    try report.document.addAnnotated(elem_type_str, .type_variable);
                    try report.document.addLineBreak();

                    // Explain WHY this element doesn't support equality
                    _ = try self.explainWhyNoEquality(report, elem_content_idx, "        ");
                }
            }
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" Tuples only have an ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" method if all of their elements have ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" methods.");
            try report.document.addLineBreak();
        }
    }

    /// Explain which tag union payloads don't support equality
    fn explainTagUnionEqualityFailure(
        self: *Self,
        report: *Report,
        tag_union: snapshot.SnapshotTagUnion,
    ) !void {
        const tags = self.snapshots.sliceTags(tag_union.tags);
        var has_problem_tags = false;

        // First pass: check if any tag payloads don't support equality
        for (tags.items(.args)) |tag_args| {
            const args = self.snapshots.sliceVars(tag_args);
            for (args) |arg_content_idx| {
                if (!self.snapshotSupportsEquality(arg_content_idx)) {
                    has_problem_tags = true;
                    break;
                }
            }
            if (has_problem_tags) break;
        }

        if (has_problem_tags) {
            try report.document.addReflowingText("This tag union does not support equality because these tags have payload types that don't support ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(":");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const tag_names = tags.items(.name);
            const tag_args_list = tags.items(.args);
            for (tag_names, tag_args_list) |name, tag_args| {
                const args = self.snapshots.sliceVars(tag_args);
                var tag_has_problem = false;
                for (args) |arg_content_idx| {
                    if (!self.snapshotSupportsEquality(arg_content_idx)) {
                        tag_has_problem = true;
                        break;
                    }
                }
                if (tag_has_problem) {
                    const tag_name = self.can_ir.getIdentText(name);
                    try report.document.addText("    ");
                    try report.document.addAnnotated(tag_name, .emphasized);

                    // Show the problematic payload types
                    if (args.len > 0) {
                        try report.document.addText(" (");
                        var first = true;
                        for (args) |arg_content_idx| {
                            if (!first) try report.document.addText(", ");
                            first = false;

                            const arg_type_str = try report.addOwnedString(self.getFormattedString(arg_content_idx));
                            try report.document.addAnnotated(arg_type_str, .type_variable);
                        }
                        try report.document.addText(")");
                    }
                    try report.document.addLineBreak();

                    // Explain WHY each problematic payload doesn't support equality
                    for (args) |arg_content_idx| {
                        if (!self.snapshotSupportsEquality(arg_content_idx)) {
                            _ = try self.explainWhyNoEquality(report, arg_content_idx, "        ");
                        }
                    }
                }
            }
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" Tag unions only have an ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" method if all of their payload types have ");
            try report.document.addAnnotated("is_eq", .emphasized);
            try report.document.addReflowingText(" methods.");
            try report.document.addLineBreak();
        }
    }

    /// Check if a snapshotted type supports equality
    fn snapshotSupportsEquality(self: *Self, content_idx: snapshot.SnapshotContentIdx) bool {
        const content = self.snapshots.getContent(content_idx);
        return switch (content) {
            .structure => |s| switch (s) {
                // Functions never support equality
                .fn_pure, .fn_effectful, .fn_unbound => false,
                // Empty types trivially support equality
                .empty_record, .empty_tag_union => true,
                // Records: all fields must support equality
                .record => |record| {
                    const fields = self.snapshots.sliceRecordFields(record.fields);
                    for (fields.items(.content)) |field_content| {
                        if (!self.snapshotSupportsEquality(field_content)) return false;
                    }
                    return true;
                },
                // Tuples: all elements must support equality
                .tuple => |tuple| {
                    const elems = self.snapshots.sliceVars(tuple.elems);
                    for (elems) |elem_content| {
                        if (!self.snapshotSupportsEquality(elem_content)) return false;
                    }
                    return true;
                },
                // Tag unions: all payloads must support equality
                .tag_union => |tag_union| {
                    const tags_slice = self.snapshots.sliceTags(tag_union.tags);
                    for (tags_slice.items(.args)) |tag_args| {
                        const args = self.snapshots.sliceVars(tag_args);
                        for (args) |arg_content| {
                            if (!self.snapshotSupportsEquality(arg_content)) return false;
                        }
                    }
                    return true;
                },
                // Nominal types: check the backing type (first element in vars)
                .nominal_type => |nominal| {
                    const vars = self.snapshots.sliceVars(nominal.vars);
                    if (vars.len > 0) {
                        // First var is the backing type
                        return self.snapshotSupportsEquality(vars[0]);
                    }
                    return true;
                },
                // Other types (box, etc.) assumed to support equality
                else => true,
            },
            // Aliases: check the underlying type
            .alias => |alias| self.snapshotSupportsEquality(alias.backing),
            // Other types (flex, rigid, recursive, err) assumed to support equality
            else => true,
        };
    }

    /// Explain why a type doesn't support equality, adding the explanation to the report.
    /// Returns true if an explanation was added.
    fn explainWhyNoEquality(self: *Self, report: *Report, content_idx: snapshot.SnapshotContentIdx, indent: []const u8) !bool {
        const content = self.snapshots.getContent(content_idx);
        switch (content) {
            .structure => |s| switch (s) {
                .fn_pure, .fn_effectful, .fn_unbound => {
                    try report.document.addText(indent);
                    try report.document.addReflowingText("Function equality is not supported.");
                    try report.document.addLineBreak();
                    return true;
                },
                .record => |record| {
                    const fields = self.snapshots.sliceRecordFields(record.fields);
                    const field_names = fields.items(.name);
                    const field_contents = fields.items(.content);
                    for (field_names, field_contents) |name, field_content| {
                        if (!self.snapshotSupportsEquality(field_content)) {
                            const field_name = self.can_ir.getIdentText(name);
                            try report.document.addText(indent);
                            try report.document.addReflowingText("The ");
                            try report.document.addAnnotated(field_name, .emphasized);
                            try report.document.addReflowingText(" field doesn't support equality:");
                            try report.document.addLineBreak();
                            // Recurse with more indent
                            var deeper_indent_buf: [64]u8 = undefined;
                            const deeper_indent = std.fmt.bufPrint(&deeper_indent_buf, "{s}    ", .{indent}) catch indent;
                            _ = try self.explainWhyNoEquality(report, field_content, deeper_indent);
                            return true;
                        }
                    }
                    return false;
                },
                .tuple => |tuple| {
                    const elems = self.snapshots.sliceVars(tuple.elems);
                    for (elems, 0..) |elem_content, i| {
                        if (!self.snapshotSupportsEquality(elem_content)) {
                            var buf: [20]u8 = undefined;
                            const index_str = std.fmt.bufPrint(&buf, "{}", .{i}) catch "?";
                            try report.document.addText(indent);
                            try report.document.addReflowingText("Element ");
                            try report.document.addAnnotated(index_str, .emphasized);
                            try report.document.addReflowingText(" doesn't support equality:");
                            try report.document.addLineBreak();
                            // Recurse with more indent
                            var deeper_indent_buf: [64]u8 = undefined;
                            const deeper_indent = std.fmt.bufPrint(&deeper_indent_buf, "{s}    ", .{indent}) catch indent;
                            _ = try self.explainWhyNoEquality(report, elem_content, deeper_indent);
                            return true;
                        }
                    }
                    return false;
                },
                .tag_union => |tag_union| {
                    const tags_slice = self.snapshots.sliceTags(tag_union.tags);
                    const tag_names = tags_slice.items(.name);
                    const tag_args_list = tags_slice.items(.args);
                    for (tag_names, tag_args_list) |name, tag_args| {
                        const args = self.snapshots.sliceVars(tag_args);
                        for (args, 0..) |arg_content, i| {
                            if (!self.snapshotSupportsEquality(arg_content)) {
                                const tag_name = self.can_ir.getIdentText(name);
                                try report.document.addText(indent);
                                try report.document.addReflowingText("The ");
                                try report.document.addAnnotated(tag_name, .emphasized);
                                if (args.len > 1) {
                                    var buf: [32]u8 = undefined;
                                    const payload_str = std.fmt.bufPrint(&buf, " tag's payload {}", .{i}) catch " tag's payload";
                                    try report.document.addReflowingText(payload_str);
                                } else {
                                    try report.document.addReflowingText(" tag's payload");
                                }
                                try report.document.addReflowingText(" doesn't support equality:");
                                try report.document.addLineBreak();
                                // Recurse with more indent
                                var deeper_indent_buf: [64]u8 = undefined;
                                const deeper_indent = std.fmt.bufPrint(&deeper_indent_buf, "{s}    ", .{indent}) catch indent;
                                _ = try self.explainWhyNoEquality(report, arg_content, deeper_indent);
                                return true;
                            }
                        }
                    }
                    return false;
                },
                .nominal_type => |nominal| {
                    const vars = self.snapshots.sliceVars(nominal.vars);
                    if (vars.len > 0) {
                        const backing = vars[0];
                        if (!self.snapshotSupportsEquality(backing)) {
                            const nominal_name = self.can_ir.getIdentText(nominal.ident.ident_idx);
                            try report.document.addText(indent);
                            try report.document.addReflowingText("The ");
                            try report.document.addAnnotated(nominal_name, .emphasized);
                            try report.document.addReflowingText(" type's backing structure doesn't support equality:");
                            try report.document.addLineBreak();
                            // Recurse with more indent
                            var deeper_indent_buf: [64]u8 = undefined;
                            const deeper_indent = std.fmt.bufPrint(&deeper_indent_buf, "{s}    ", .{indent}) catch indent;
                            _ = try self.explainWhyNoEquality(report, backing, deeper_indent);
                            return true;
                        }
                    }
                    return false;
                },
                else => return false,
            },
            .alias => |alias| {
                if (!self.snapshotSupportsEquality(alias.backing)) {
                    return try self.explainWhyNoEquality(report, alias.backing, indent);
                }
                return false;
            },
            else => return false,
        }
    }

    /// Build a report for "unused value" diagnostic
    fn buildUnusedValueReport(self: *Self, data: UnusedValue) !Report {
        var report = Report.init(self.gpa, "UNUSED VALUE", .runtime_error);
        errdefer report.deinit();

        const owned_expected = try report.addOwnedString(self.getFormattedString(data.snapshot));

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.var_)));
        const region_info = self.module_env.calcRegionInfo(region.*);

        try report.document.addReflowingText("This expression produces a value, but it's not being used:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("It has the type:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(owned_expected);

        return report;
    }

    // cross-module import //

    /// Build a report for cross-module import type mismatch
    fn buildCrossModuleImportError(
        self: *Self,
        types: TypePair,
        data: CrossModuleImport,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);
        errdefer report.deinit();

        try report.document.addText("This value is being used in an unexpected way.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Get the import expression
        const import_expr = self.can_ir.store.getExpr(data.import_region);
        const import_region = import_expr.e_lookup_external.region;

        // Get region info for the import
        const import_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.getLineStarts(),
            import_region.start.offset,
            import_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = self.gpa.dupe(u8, import_region_info.calculateLineText(self.source, self.module_env.getLineStarts())) catch return report,
            .start_line = import_region_info.start_line_idx + 1,
            .start_column = import_region_info.start_col_idx + 1,
            .end_line = import_region_info.end_line_idx + 1,
            .end_column = import_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = self.filename,
        };

        // Create underline region
        const underline_regions = [_]UnderlineRegion{
            .{
                .start_line = import_region_info.start_line_idx + 1,
                .start_column = import_region_info.start_col_idx + 1,
                .end_line = import_region_info.end_line_idx + 1,
                .end_column = import_region_info.end_col_idx + 1,
                .annotation = .error_highlight,
            },
        };

        try report.document.addSourceCodeWithUnderlines(display_region, &underline_regions);
        try report.document.addLineBreak();

        // Get module name if available
        const module_idx = @intFromEnum(data.module_idx);
        const module_name = if (module_idx < self.can_ir.imports.imports.len()) blk: {
            const import_string_idx = self.can_ir.imports.imports.items.items[module_idx];
            const import_name = self.can_ir.getString(import_string_idx);
            break :blk import_name;
        } else null;

        // Show what was imported
        try report.document.addText("It has the type:");
        try report.document.addLineBreak();

        const expected_type = try report.addOwnedString(self.getFormattedString(types.expected_snapshot));
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected_type);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the actual type from the import
        if (module_name) |name| {
            try report.document.addText("However, the value imported from ");
            try report.document.addAnnotated(name, .module_name);
            try report.document.addText(" has the type:");
        } else {
            try report.document.addText("However, the imported value has the type:");
        }
        try report.document.addLineBreak();

        const actual_type = try report.addOwnedString(self.getFormattedString(types.actual_snapshot));
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual_type);

        return report;
    }

    /// Build a report for when a platform expects a type alias but it's not found
    fn buildPlatformAliasNotFound(self: *Self, data: PlatformAliasNotFound) !Report {
        var report = Report.init(self.gpa, "MISSING PLATFORM REQUIRED TYPE", .runtime_error);
        errdefer report.deinit();

        const owned_name = try report.addOwnedString(self.can_ir.getIdentText(data.expected_alias_ident));

        try report.document.addReflowingText("The platform expects your ");
        try report.document.addAnnotated("app", .inline_code);
        try report.document.addReflowingText(" module to define a type alias named ");
        try report.document.addAnnotated(owned_name, .type_variable);
        try report.document.addReflowingText(", but I couldn't find one.");

        switch (data.ctx) {
            .not_found => {
                try report.document.addLineBreak();
                try report.document.addLineBreak();
                try report.document.addAnnotated("Hint:", .emphasized);
                try report.document.addReflowingText(" Add a type alias definition for ");
                try report.document.addAnnotated(owned_name, .type_variable);
                try report.document.addReflowingText(" to your app module. Check your platform's documentation for the expected type.");
            },
            .found_but_not_alias => {
                try report.document.addLineBreak();
                try report.document.addLineBreak();
                try report.document.addAnnotated("Hint:", .emphasized);
                try report.document.addReflowingText(" You have a definition named ");
                try report.document.addAnnotated(owned_name, .type_variable);
                try report.document.addReflowingText(", but it's not a type alias. The platform requires a type alias (defined with ");
                try report.document.addAnnotated(":", .inline_code);
                try report.document.addReflowingText("), not a value definition.");
            },
        }

        return report;
    }

    fn buildPlatformDefNotFound(self: *Self, data: PlatformDefNotFound) !Report {
        var report = Report.init(self.gpa, "MISSING PLATFORM REQUIRED DEFINITION", .runtime_error);
        errdefer report.deinit();

        const owned_name = try report.addOwnedString(self.can_ir.getIdentText(data.expected_def_ident));

        try report.document.addReflowingText("The platform expects your ");
        try report.document.addAnnotated("app", .inline_code);
        try report.document.addReflowingText(" module to export a definition named ");
        try report.document.addAnnotated(owned_name, .inline_code);
        try report.document.addReflowingText(", but I couldn't find one.");

        switch (data.ctx) {
            .not_found => {
                try report.document.addLineBreak();
                try report.document.addLineBreak();
                try report.document.addAnnotated("Hint:", .emphasized);
                try report.document.addReflowingText(" Define and export ");
                try report.document.addAnnotated(owned_name, .inline_code);
                try report.document.addReflowingText(" in your app module. Check your platform's documentation for the expected type signature.");
            },
            .found_but_not_exported => {
                try report.document.addLineBreak();
                try report.document.addLineBreak();
                try report.document.addAnnotated("Hint:", .emphasized);
                try report.document.addReflowingText(" You have a definition named ");
                try report.document.addAnnotated(owned_name, .inline_code);
                try report.document.addReflowingText(", but it's not exported. Add it to your module's ");
                try report.document.addAnnotated("exposes", .inline_code);
                try report.document.addReflowingText(" list in the module header.");
            },
        }

        return report;
    }

    /// Build a report for compile-time crash
    fn buildComptimeCrashReport(self: *Self, data: ComptimeCrash) !Report {
        var report = Report.init(self.gpa, "COMPTIME CRASH", .runtime_error);
        errdefer report.deinit();

        const owned_message = try report.addOwnedString(
            self.problems.getExtraString(data.message),
        );

        try report.document.addText("This definition crashed during compile-time evaluation:");
        try report.document.addLineBreak();

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addText("The ");
        try report.document.addAnnotated("crash", .keyword);
        try report.document.addText(" happened with this message:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(owned_message);

        return report;
    }

    /// Build a report for compile-time expect failure
    fn buildComptimeExpectFailedReport(self: *Self, data: ComptimeExpectFailed) !Report {
        // Note: data.message contains raw source bytes which we don't display separately
        // since the source region highlighting already shows the expect expression
        var report = Report.init(self.gpa, "COMPTIME EXPECT FAILED", .runtime_error);
        errdefer report.deinit();

        try report.document.addText("This ");
        try report.document.addAnnotated("expect", .keyword);
        try report.document.addText(" failed during compile-time evaluation:");
        try report.document.addLineBreak();

        // Add source region highlighting - shows the expect expression with syntax highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );

        return report;
    }

    /// Build a report for compile-time evaluation error
    fn buildComptimeEvalErrorReport(self: *Self, data: ComptimeEvalError) !Report {
        var report = Report.init(self.gpa, "COMPTIME EVAL ERROR", .runtime_error);
        errdefer report.deinit();

        const owned_error_name = try report.addOwnedString(
            self.problems.getExtraString(data.error_name),
        );

        try report.document.addText("This definition could not be evaluated at compile time:");
        try report.document.addLineBreak();

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addText("The evaluation failed with error:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(owned_error_name);

        return report;
    }

    fn buildNonExhaustiveMatchReport(self: *Self, data: NonExhaustiveMatch) !Report {
        var report = Report.init(self.gpa, "NON-EXHAUSTIVE MATCH", .runtime_error);
        errdefer report.deinit();

        try report.document.addText("This ");
        try report.document.addAnnotated("match", .keyword);
        try report.document.addText(" expression doesn't cover all possible cases:");
        try report.document.addLineBreak();

        // Add source region highlighting
        const match_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.match_expr)));
        const region_info = self.module_env.calcRegionInfo(match_region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        const condition_type = self.getFormattedString(data.condition_snapshot);
        try report.document.addText("The value being matched on has type:");
        try report.document.addLineBreak();
        try report.document.addText("        ");
        try report.document.addAnnotated(condition_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("Missing patterns:");
        try report.document.addLineBreak();

        const missing_patterns = self.problems.getMissingPatterns(data.missing_patterns);
        for (missing_patterns) |pattern_idx| {
            const pattern = self.problems.getExtraString(pattern_idx);
            const owned_pattern = try report.addOwnedString(pattern);
            try report.document.addText("    ");
            try report.document.addCodeBlock(owned_pattern);
            try report.document.addLineBreak();
        }

        try report.document.addLineBreak();
        try report.document.addText("Hint: Add branches to handle these cases, or use ");
        try report.document.addAnnotated("_", .keyword);
        try report.document.addText(" to match anything.");

        return report;
    }

    fn buildRedundantPatternReport(self: *Self, data: RedundantPattern) !Report {
        var report = Report.init(self.gpa, "REDUNDANT PATTERN", .warning);
        errdefer report.deinit();

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.problem_branch_index + 1);
        const ordinal_str = try report.addOwnedString(self.bytes_buf.items);

        try report.document.addText("The ");
        try report.document.addText(ordinal_str);
        try report.document.addText(" branch of this ");
        try report.document.addAnnotated("match", .keyword);
        try report.document.addText(" is redundant:");
        try report.document.addLineBreak();

        // Add source region highlighting
        const match_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.match_expr)));
        const region_info = self.module_env.calcRegionInfo(match_region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addText("This pattern can never match because earlier patterns already ");
        try report.document.addText("cover all the values it would match.");

        return report;
    }

    fn buildUnmatchablePatternReport(self: *Self, data: UnmatchablePattern) !Report {
        var report = Report.init(self.gpa, "UNMATCHABLE PATTERN", .warning);
        errdefer report.deinit();

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.problem_branch_index + 1);
        const ordinal_str = try report.addOwnedString(self.bytes_buf.items);

        try report.document.addText("The ");
        try report.document.addText(ordinal_str);
        try report.document.addText(" branch of this ");
        try report.document.addAnnotated("match", .keyword);
        try report.document.addText(" can never match:");
        try report.document.addLineBreak();

        // Add source region highlighting
        const match_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.match_expr)));
        const region_info = self.module_env.calcRegionInfo(match_region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addText("This pattern matches a type that has no possible values (an uninhabited type), ");
        try report.document.addText("so no value can ever match it.");

        return report;
    }

    // helpers //

    // Given a buffer and a number, write a the human-readably ordinal number
    // Note that the caller likely needs to clear the buffer before calling this function
    fn appendOrdinal(buf: *std.array_list.Managed(u8), n: u32) !void {
        switch (n) {
            1 => try buf.appendSlice("first"),
            2 => try buf.appendSlice("second"),
            3 => try buf.appendSlice("third"),
            4 => try buf.appendSlice("fourth"),
            5 => try buf.appendSlice("fifth"),
            6 => try buf.appendSlice("sixth"),
            7 => try buf.appendSlice("seventh"),
            8 => try buf.appendSlice("eighth"),
            9 => try buf.appendSlice("ninth"),
            10 => try buf.appendSlice("tenth"),
            else => {
                // Using character arrays to avoid typo checker flagging these strings as typos
                // (e.g. it thinks ['n', 'd'] is a typo of "and") - and that's a useful typo
                // to catch, so we're sacrificing readability of this particular code snippet
                // for the sake of catching actual typos of "and" elsewhere in the code base.
                const suffix = if (n % 100 >= 11 and n % 100 <= 13) &[_]u8{ 't', 'h' } else switch (n % 10) {
                    1 => &[_]u8{ 's', 't' },
                    2 => &[_]u8{ 'n', 'd' },
                    3 => &[_]u8{ 'r', 'd' },
                    else => &[_]u8{ 't', 'h' },
                };
                try buf.writer().print("{d}{s}", .{ n, suffix });
            },
        }
    }

    /// Check if both snapshot contents represent function types
    fn areBothFunctionSnapshots(expected_content: snapshot.SnapshotContent, actual_content: snapshot.SnapshotContent) bool {
        return isSnapshotFunction(expected_content) and isSnapshotFunction(actual_content);
    }

    /// Check if a snapshot content represents a function type
    fn isSnapshotFunction(content: snapshot.SnapshotContent) bool {
        return switch (content) {
            .structure => |structure| switch (structure) {
                .fn_pure, .fn_effectful, .fn_unbound => true,
                else => false,
            },
            else => false,
        };
    }

    /// Extract the first argument type from a function snapshot (if available)
    fn extractFirstArgTypeFromFunctionSnapshot(self: *Self, func_snapshot: SnapshotContentIdx) ?SnapshotContentIdx {
        const content = self.snapshots.getContent(func_snapshot);

        return switch (content) {
            .structure => |structure| switch (structure) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    const args = self.snapshots.sliceVars(func.args);
                    if (args.len > 0) {
                        return args[0];
                    }
                    return null;
                },
                else => null,
            },
            else => null,
        };
    }
};

// store //

/// Self-contained problems store with resolved snapshots of type content
///
/// Whenever a type error occurs, we update the `Var` in the type store to
/// have `.err` content. This is necessary to continue type-checking but
/// looses essential error information. So before doing this, we create a fully
/// resolved snapshot of the type that we can use in reporting
///
/// Entry point is `appendProblem`
pub const Store = struct {
    const Self = @This();
    const ALIGNMENT = std.mem.Alignment.@"16";

    problems: std.ArrayListAligned(Problem, ALIGNMENT) = .{},

    /// Backing storage for formatted pattern strings
    extra_strings_backing: ByteList,
    /// Backing storage for missing patterns index arrays
    missing_patterns_backing: std.array_list.Managed(ExtraStringIdx),

    pub fn init(gpa: Allocator) std.mem.Allocator.Error!Self {
        return .{
            .problems = try std.ArrayListAligned(Problem, ALIGNMENT).initCapacity(gpa, 16),
            .extra_strings_backing = try ByteList.initCapacity(gpa, 512),
            .missing_patterns_backing = try std.array_list.Managed(ExtraStringIdx).initCapacity(gpa, 64),
        };
    }

    pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{
            .problems = try std.ArrayListAligned(Problem, ALIGNMENT).initCapacity(gpa, capacity),
            .extra_strings_backing = try ByteList.initCapacity(gpa, 512),
            .missing_patterns_backing = try std.array_list.Managed(ExtraStringIdx).initCapacity(gpa, 64),
        };
    }

    pub fn deinit(self: *Self, gpa: Allocator) void {
        self.extra_strings_backing.deinit();
        self.missing_patterns_backing.deinit();
        self.problems.deinit(gpa);
    }

    /// Put an extra string in the backing store, returning an "id" (range)
    pub fn putExtraString(self: *Self, str: []const u8) std.mem.Allocator.Error!ExtraStringIdx {
        const start = self.extra_strings_backing.items.len;
        try self.extra_strings_backing.appendSlice(str);
        const end = self.extra_strings_backing.items.len;
        return ByteListRange{ .start = start, .count = end - start };
    }

    /// Put an extra string in the backing store, returning an "id" (range)
    pub fn putFmtExtraString(self: *Self, comptime format: []const u8, args: anytype) std.mem.Allocator.Error!ExtraStringIdx {
        const start = self.extra_strings_backing.items.len;
        var writer = self.extra_strings_backing.writer();
        try writer.print(format, args);
        const end = self.extra_strings_backing.items.len;
        return ByteListRange{ .start = start, .count = end - start };
    }

    /// Get a stored pattern string by its range
    pub fn getExtraString(self: *const Self, idx: ExtraStringIdx) []const u8 {
        return self.extra_strings_backing.items[idx.start..][0..idx.count];
    }

    /// Get the slice of missing pattern indices by range
    pub fn getMissingPatterns(self: *const Self, range: MissingPatternsRange) []const ExtraStringIdx {
        return self.missing_patterns_backing.items[range.start..][0..range.count];
    }

    /// Create a deep snapshot from a Var, storing it in this SnapshotStore
    pub fn appendProblem(self: *Self, gpa: Allocator, problem: Problem) std.mem.Allocator.Error!Problem.Idx {
        const idx: Problem.Idx = @enumFromInt(self.problems.items.len);
        try self.problems.append(gpa, problem);
        return idx;
    }

    pub fn get(self: *Self, idx: Problem.Idx) Problem {
        return self.problems.items[@intFromEnum(idx)];
    }

    pub fn len(self: *Self) usize {
        return self.problems.items.len;
    }
};
