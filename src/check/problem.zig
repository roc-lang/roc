//! Generate Reports for type checking errors

const std = @import("std");
const base = @import("base");
const tracy = @import("tracy");
const collections = @import("collections");
const types_mod = @import("types");
const can = @import("can");
const reporting = @import("reporting");
const Check = @import("Check.zig");

const snapshot = @import("snapshot.zig");

const Allocator = std.mem.Allocator;

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

const Report = reporting.Report;
const Document = reporting.Document;
const UnderlineRegion = reporting.UnderlineRegion;
const SourceCodeDisplayRegion = reporting.SourceCodeDisplayRegion;
const SourceRegion = reporting.SourceRegion;

const TypesStore = types_mod.Store;
const Ident = base.Ident;

const MkSafeMultiList = collections.SafeMultiList;

const SnapshotContentIdx = snapshot.SnapshotContentIdx;

const Var = types_mod.Var;
const Content = types_mod.Content;

/// The kind of problem we're dealing with
pub const Problem = union(enum) {
    type_mismatch: TypeMismatch,
    type_apply_mismatch_arities: TypeApplyArityMismatch,
    static_dispach: StaticDispatch,
    number_does_not_fit: NumberDoesNotFit,
    negative_unsigned_int: NegativeUnsignedInt,
    infinite_recursion: struct { var_: Var },
    anonymous_recursion: struct { var_: Var },
    invalid_number_type: VarProblem1,
    invalid_record_ext: VarProblem1,
    invalid_tag_union_ext: VarProblem1,
    bug: Bug,
    comptime_crash: ComptimeCrash,
    comptime_expect_failed: ComptimeExpectFailed,
    comptime_eval_error: ComptimeEvalError,

    pub const Idx = enum(u32) { _ };
    pub const Tag = std.meta.Tag(@This());
};

/// A crash that occurred during compile-time evaluation
pub const ComptimeCrash = struct {
    message: []const u8,
    region: base.Region,
};

/// An expect that failed during compile-time evaluation
pub const ComptimeExpectFailed = struct {
    message: []const u8,
    region: base.Region,
};

/// An error that occurred during compile-time evaluation
pub const ComptimeEvalError = struct {
    error_name: []const u8,
    region: base.Region,
};

/// A single var problem
pub const VarProblem1 = struct {
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
    incompatible_if_branches: IncompatibleIfBranches,
    incompatible_match_cond_pattern: IncompatibleMatchCondPattern,
    incompatible_match_patterns: IncompatibleMatchPatterns,
    incompatible_match_branches: IncompatibleMatchBranches,
    invalid_bool_binop: InvalidBoolBinop,
    invalid_nominal_tag,
    cross_module_import: CrossModuleImport,
    incompatible_fn_call_arg: IncompatibleFnCallArg,
    incompatible_fn_args_bound_var: IncompatibleFnArgsBoundVar,
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

/// Problem data for when a bool binop (`and` or `or`) is invalid
pub const InvalidBoolBinop = struct {
    binop_expr: CIR.Expr.Idx,
    problem_side: enum { lhs, rhs },
    binop: enum { @"and", @"or" },
};

// static dispatch //

/// Error related to static dispatch
pub const StaticDispatch = union(enum) {
    dispatcher_not_nominal: DispatcherNotNominal,
    dispatcher_does_not_impl_method: DispatcherDoesNotImplMethod,
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
    fn_var: Var,
    method_name: Ident.Idx,
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
    snapshot_writer: snapshot.SnapshotWriter,
    bytes_buf: std.array_list.Managed(u8),
    module_env: *ModuleEnv,
    can_ir: *const ModuleEnv,
    snapshots: *const snapshot.Store,
    source: []const u8,
    filename: []const u8,
    other_modules: []const *const ModuleEnv,

    /// Init report builder
    /// Only owned field is `buf`
    pub fn init(
        gpa: Allocator,
        module_env: *ModuleEnv,
        can_ir: *const ModuleEnv,
        snapshots: *const snapshot.Store,
        filename: []const u8,
        other_modules: []const *const ModuleEnv,
    ) Self {
        return .{
            .gpa = gpa,
            .snapshot_writer = snapshot.SnapshotWriter.init(gpa, snapshots, module_env.getIdentStore()),
            .bytes_buf = std.array_list.Managed(u8).init(gpa),
            .module_env = module_env,
            .can_ir = can_ir,
            .snapshots = snapshots,
            .source = module_env.common.source,
            .filename = filename,
            .other_modules = other_modules,
        };
    }

    /// Deinit report builder
    /// Only owned field is `buf`
    pub fn deinit(self: *Self) void {
        self.snapshot_writer.deinit();
        self.bytes_buf.deinit();
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
                        .invalid_bool_binop => |data| {
                            return self.buildInvalidBoolBinop(mismatch.types, data);
                        },
                        .invalid_nominal_tag => {
                            return self.buildInvalidNominalTag(mismatch.types);
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
                    }
                } else {
                    return self.buildGenericTypeMismatchReport(mismatch.types);
                }
            },
            .type_apply_mismatch_arities => |data| {
                return self.buildTypeApplyArityMismatchReport(data);
            },
            .static_dispach => |detail| {
                switch (detail) {
                    .dispatcher_not_nominal => |data| return self.buildStaticDispatchDispatcherNotNominal(data),
                    .dispatcher_does_not_impl_method => |data| return self.buildStaticDispatchDispatcherDoesNotImplMethod(data),
                }
            },
            .number_does_not_fit => |data| {
                return self.buildNumberDoesNotFitReport(data);
            },
            .negative_unsigned_int => |data| {
                return self.buildNegativeUnsignedIntReport(data);
            },
            .infinite_recursion => |_| return self.buildUnimplementedReport("infinite_recursion"),
            .anonymous_recursion => |_| return self.buildUnimplementedReport("anonymous_recursion"),
            .invalid_number_type => |_| return self.buildUnimplementedReport("invalid_number_type"),
            .invalid_record_ext => |_| return self.buildUnimplementedReport("invalid_record_ext"),
            .invalid_tag_union_ext => |_| return self.buildUnimplementedReport("invalid_tag_union_ext"),
            .bug => |_| return self.buildUnimplementedReport("bug"),
            .comptime_crash => |data| return self.buildComptimeCrashReport(data),
            .comptime_expect_failed => |data| return self.buildComptimeExpectFailedReport(data),
            .comptime_eval_error => |data| return self.buildComptimeEvalErrorReport(data),
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

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.actual_snapshot);
        const owned_actual = try report.addOwnedString(self.snapshot_writer.get());

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.expected_snapshot);
        const owned_expected = try report.addOwnedString(self.snapshot_writer.get());

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
        if (types.from_annotation) {
            // Check the snapshot content to determine if we have function types
            const expected_content = self.snapshots.getContent(types.expected_snapshot);
            const actual_content = self.snapshots.getContent(types.actual_snapshot);

            if (self.areBothFunctionSnapshots(expected_content, actual_content)) {
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
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_actual, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        if (types.from_annotation) {
            try report.document.addText("But the type annotation says it should have the type:");
        } else {
            try report.document.addText("But I expected it to be:");
        }
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_expected, .type_variable);

        // Add a hint if this looks like a numeric literal size issue
        const actual_str = owned_actual;
        const expected_str = owned_expected;

        // Check if we're dealing with numeric types
        const is_numeric_issue = (std.mem.indexOf(u8, actual_str, "Num(") != null and
            std.mem.indexOf(u8, expected_str, "Num(") != null);

        // Check if target is a concrete integer type
        const has_unsigned = std.mem.indexOf(u8, expected_str, "Unsigned") != null;
        const has_signed = std.mem.indexOf(u8, expected_str, "Signed") != null;

        if (is_numeric_issue and (has_unsigned or has_signed)) {
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addAnnotated("Hint:", .emphasized);
            if (has_unsigned) {
                try report.document.addReflowingText(" This might be because the numeric literal is either negative or too large to fit in the unsigned type.");
            } else {
                try report.document.addReflowingText(" This might be because the numeric literal is too large to fit in the target type.");
            }
        }

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
        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.snapshot_writer.get());

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.snapshot_writer.get());

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
        try report.document.addText("    ");
        try report.document.addAnnotated(expected_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the type of the second element
        try report.document.addText("However, the ");
        try report.document.addText(actual_type_ordinal);
        try report.document.addText(" element has this type:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
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
        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.snapshot_writer.get());

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
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
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
        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.snapshot_writer.get());

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.snapshot_writer.get());

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
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
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
        try report.document.addText("    ");
        try report.document.addAnnotated(expected_type, .type_variable);
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
        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.snapshot_writer.get());

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.snapshot_writer.get());

        self.snapshot_writer.resetContext();
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
        self.snapshot_writer.resetContext();
        try report.document.addText("The first pattern has the type:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the type of the other branches
        try report.document.addText("But the expression between the ");
        try report.document.addAnnotated("match", .keyword);
        try report.document.addText(" parenthesis has the type:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(expected_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("These two types can't never match!");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

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
        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.snapshot_writer.get());

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.snapshot_writer.get());

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.problem_branch_index + 1);
        const branch_ord = try report.addOwnedString(self.bytes_buf.items);

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.problem_pattern_index + 1);
        const pattern_ord = try report.addOwnedString(self.bytes_buf.items);

        // Add description
        if (data.num_patterns > 1) {
            self.snapshot_writer.resetContext();
            try report.document.addText("The pattern ");
            try report.document.addText(pattern_ord);
            try report.document.addText(" pattern in this ");
            try report.document.addText(branch_ord);
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" differs from previous ones:");
            try report.document.addLineBreak();
        } else {
            self.snapshot_writer.resetContext();
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
        self.snapshot_writer.resetContext();
        try report.document.addText("The ");
        try report.document.addText(branch_ord);
        try report.document.addText(" pattern has this type:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the type of the other branches
        if (data.num_branches > 2) {
            try report.document.addText("But all the previous patterns have this type: ");
        } else {
            try report.document.addText("But the other pattern has this type:");
        }
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(expected_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // TODO we should categorize this as a tip/hint (maybe relevant to how editors display it)
        try report.document.addText("All patterns in an ");
        try report.document.addAnnotated("match", .keyword);
        try report.document.addText(" must have compatible types.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

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
        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.snapshot_writer.get());

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.snapshot_writer.get());

        self.bytes_buf.clearRetainingCapacity();
        try appendOrdinal(&self.bytes_buf, data.problem_branch_index + 1);
        const branch_ord = try report.addOwnedString(self.bytes_buf.items);

        // Add description
        if (data.num_branches == 2) {
            self.snapshot_writer.resetContext();
            try report.document.addText("The second branch's type in this ");
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" is different from the first branch:");
        } else {
            self.snapshot_writer.resetContext();
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
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
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
        try report.document.addText("    ");
        try report.document.addAnnotated(expected_type, .type_variable);
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

    /// Build a report for incompatible match branches
    fn buildInvalidBoolBinop(
        self: *Self,
        types: TypePair,
        data: InvalidBoolBinop,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID BOOL OPERATION", .runtime_error);
        errdefer report.deinit();

        // Create owned strings
        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.snapshot_writer.get());

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
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
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
        self.snapshot_writer.resetContext();
        try self.snapshot_writer.writeTag(actual_tag, types.actual_snapshot);
        const actual_tag_str = try report.addOwnedString(self.snapshot_writer.get());

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
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_tag_str, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the expected tags
        if (expected_num_tags_str == 1) {
            const expected_tag = self.snapshots.tags.get(expected_content.structure.tag_union.tags.start);
            self.snapshot_writer.resetContext();
            try self.snapshot_writer.writeTag(expected_tag, types.expected_snapshot);
            const expected_tag_str = try report.addOwnedString(self.snapshot_writer.get());

            try report.document.addText("But the nominal type needs it to be:");
            try report.document.addLineBreak();
            try report.document.addText("    ");
            try report.document.addAnnotated(expected_tag_str, .type_variable);
        } else {
            self.snapshot_writer.resetContext();
            try self.snapshot_writer.write(types.expected_snapshot);
            const expected_type = try report.addOwnedString(self.snapshot_writer.get());

            try report.document.addText("But the nominal type needs it to one of:");
            try report.document.addLineBreak();
            try report.document.addText("    ");
            try report.document.addAnnotated(expected_type, .type_variable);

            // Check if there's a tag with the same name in the list of possible tags

            const actual_tag_name_str = self.can_ir.getIdent(actual_tag.name);
            var iter = expected_content.structure.tag_union.tags.iterIndices();
            while (iter.next()) |tag_index| {
                const cur_expected_tag = self.snapshots.tags.get(tag_index);
                const expected_tag_name_str = self.can_ir.getIdent(cur_expected_tag.name);

                if (std.mem.eql(u8, actual_tag_name_str, expected_tag_name_str)) {
                    self.snapshot_writer.resetContext();
                    try self.snapshot_writer.writeTag(cur_expected_tag, types.expected_snapshot);
                    const cur_expected_tag_str = try report.addOwnedString(self.snapshot_writer.get());

                    try report.document.addLineBreak();
                    try report.document.addLineBreak();
                    try report.document.addAnnotated("Hint:", .emphasized);
                    try report.document.addReflowingText(" The nominal type has a tag with the same name, but different args:");
                    try report.document.addLineBreak();
                    try report.document.addLineBreak();
                    try report.document.addText("    ");
                    try report.document.addAnnotated(cur_expected_tag_str, .type_variable);

                    break;
                }
            }
        }

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

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(actual_arg_type);
        const actual_type = try report.addOwnedString(self.snapshot_writer.get());

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(expected_arg_type);
        const expected_type = try report.addOwnedString(self.snapshot_writer.get());

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
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addReflowingText("But ");
        if (data.fn_name) |fn_name_ident| {
            self.snapshot_writer.resetContext();
            const fn_name = try report.addOwnedString(self.can_ir.getIdent(fn_name_ident));
            try report.document.addAnnotated(fn_name, .inline_code);
        } else {
            try report.document.addReflowingText("the function");
        }
        try report.document.addReflowingText(" needs the ");
        try report.document.addReflowingText(arg_index);
        try report.document.addReflowingText(" argument to be:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(expected_type, .type_variable);

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
        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.expected_snapshot);
        const first_type = try report.addOwnedString(self.snapshot_writer.get());

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.actual_snapshot);
        const second_type = try report.addOwnedString(self.snapshot_writer.get());

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
        try report.document.addText("    ");
        try report.document.addAnnotated(first_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addReflowingText("But the ");
        try report.document.addText(second_arg_index);
        try report.document.addReflowingText(" argument has the type:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(second_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        if (data.fn_name) |fn_name_ident| {
            self.snapshot_writer.resetContext();
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

        const type_name = try report.addOwnedString(self.can_ir.getIdent(data.type_name));

        self.bytes_buf.clearRetainingCapacity();
        try self.bytes_buf.writer().print("{d}", .{data.num_expected_args});
        const num_expected_args = try report.addOwnedString(self.snapshot_writer.get());

        self.bytes_buf.clearRetainingCapacity();
        try self.bytes_buf.writer().print("{d}", .{data.num_actual_args});
        const num_actual_args = try report.addOwnedString(self.snapshot_writer.get());

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(data.region);

        try report.document.addReflowingText("The type ");
        try report.document.addAnnotated(type_name, .type_variable);
        try report.document.addReflowingText(" expects ");
        try report.document.addReflowingText(num_expected_args);
        try report.document.addReflowingText(" argument, but got ");
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
        try report.document.addLineBreak();

        return report;
    }

    // static dispatch //

    /// Build a report for when a type is not nominal, but you're tryint to
    /// static  dispatch on it
    fn buildStaticDispatchDispatcherNotNominal(
        self: *Self,
        data: DispatcherNotNominal,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE DOES NOT HAVE METHODS", .runtime_error);
        errdefer report.deinit();

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(data.dispatcher_snapshot);
        const snapshot_str = try report.addOwnedString(self.snapshot_writer.get());

        const method_name_str = try report.addOwnedString(self.can_ir.getIdentText(data.method_name));

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.fn_var)));

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(region.*);

        try report.document.addReflowingText("You're trying to call the ");
        try report.document.addAnnotated(method_name_str, .inline_code);
        try report.document.addReflowingText(" method on a ");
        try report.document.addAnnotated(snapshot_str, .inline_code);
        try report.document.addReflowingText(":");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("But ");
        try report.document.addAnnotated(snapshot_str, .inline_code);
        try report.document.addReflowingText(" doesn't support methods.");

        return report;
    }

    /// Build a report for when a type doesn't have the expected static dispatch
    /// method
    fn buildStaticDispatchDispatcherDoesNotImplMethod(
        self: *Self,
        data: DispatcherDoesNotImplMethod,
    ) !Report {
        var report = Report.init(self.gpa, "MISSING METHOD", .runtime_error);
        errdefer report.deinit();

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(data.dispatcher_snapshot);
        const snapshot_str = try report.addOwnedString(self.snapshot_writer.get());

        const method_name_str = try report.addOwnedString(self.can_ir.getIdentText(data.method_name));

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.fn_var)));

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(region.*);

        try report.document.addReflowingText("The ");
        try report.document.addAnnotated(snapshot_str, .emphasized);
        try report.document.addReflowingText(" type does not have a ");
        try report.document.addAnnotated(method_name_str, .emphasized);
        try report.document.addReflowingText(" method:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );

        // TODO: Find similar method names and show a more helpful error message
        // here

        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addAnnotated("Hint:", .emphasized);
        try report.document.addReflowingText(" Did you forget to define ");
        try report.document.addAnnotated(method_name_str, .emphasized);
        try report.document.addReflowingText(" in the type's method block?");

        return report;
    }

    // number problems //

    /// Build a report for "number does not fit in type" diagnostic
    fn buildNumberDoesNotFitReport(
        self: *Self,
        data: NumberDoesNotFit,
    ) !Report {
        var report = Report.init(self.gpa, "NUMBER DOES NOT FIT IN TYPE", .runtime_error);
        errdefer report.deinit();

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(data.expected_type);
        const owned_expected = try report.addOwnedString(self.snapshot_writer.get());

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.literal_var)));

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(region.*);
        const literal_text = self.source[region.start.offset..region.end.offset];

        try report.document.addReflowingText("The number ");
        try report.document.addAnnotated(literal_text, .emphasized);
        try report.document.addReflowingText(" does not fit in its inferred type:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addText("Its inferred type is:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_expected, .type_variable);

        return report;
    }

    /// Build a report for "negative unsigned integer" diagnostic
    fn buildNegativeUnsignedIntReport(
        self: *Self,
        data: NegativeUnsignedInt,
    ) !Report {
        var report = Report.init(self.gpa, "NEGATIVE UNSIGNED INTEGER", .runtime_error);
        errdefer report.deinit();

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(data.expected_type);
        const owned_expected = try report.addOwnedString(self.snapshot_writer.get());

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.literal_var)));

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(region.*);
        const literal_text = self.source[region.start.offset..region.end.offset];

        try report.document.addReflowingText("The number ");
        try report.document.addAnnotated(literal_text, .emphasized);
        try report.document.addReflowingText(" is ");
        try report.document.addAnnotated("signed", .emphasized);
        try report.document.addReflowingText(" because it is negative:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addText("However, its inferred type is ");
        try report.document.addAnnotated("unsigned", .emphasized);
        try report.document.addReflowingText(":");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_expected, .type_variable);

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

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.snapshot_writer.get());
        try report.document.addText("    ");
        try report.document.addAnnotated(expected_type, .type_variable);
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

        self.snapshot_writer.resetContext();
        try self.snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.snapshot_writer.get());
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
        try report.document.addLineBreak();

        return report;
    }

    /// Build a report for "invalid number literal" diagnostic
    fn buildUnimplementedReport(self: *Self, bytes: []const u8) !Report {
        var report = Report.init(self.gpa, "UNIMPLEMENTED: ", .runtime_error);
        const owned_bytes = try report.addOwnedString(bytes);
        try report.document.addText(owned_bytes);
        return report;
    }

    /// Build a report for compile-time crash
    fn buildComptimeCrashReport(self: *Self, data: ComptimeCrash) !Report {
        var report = Report.init(self.gpa, "COMPTIME CRASH", .runtime_error);
        errdefer report.deinit();

        const owned_message = try report.addOwnedString(data.message);

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
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_message, .emphasized);

        return report;
    }

    /// Build a report for compile-time expect failure
    fn buildComptimeExpectFailedReport(self: *Self, data: ComptimeExpectFailed) !Report {
        var report = Report.init(self.gpa, "COMPTIME EXPECT FAILED", .runtime_error);
        errdefer report.deinit();

        const owned_message = try report.addOwnedString(data.message);

        try report.document.addText("This definition contains an ");
        try report.document.addAnnotated("expect", .keyword);
        try report.document.addText(" that failed during compile-time evaluation:");
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
        try report.document.addAnnotated("expect", .keyword);
        try report.document.addText(" failed with this message:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_message, .emphasized);

        return report;
    }

    /// Build a report for compile-time evaluation error
    fn buildComptimeEvalErrorReport(self: *Self, data: ComptimeEvalError) !Report {
        var report = Report.init(self.gpa, "COMPTIME EVAL ERROR", .runtime_error);
        errdefer report.deinit();

        const owned_error_name = try report.addOwnedString(data.error_name);

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
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_error_name, .emphasized);

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
    fn areBothFunctionSnapshots(self: *Self, expected_content: snapshot.SnapshotContent, actual_content: snapshot.SnapshotContent) bool {
        return self.isSnapshotFunction(expected_content) and self.isSnapshotFunction(actual_content);
    }

    /// Check if a snapshot content represents a function type
    fn isSnapshotFunction(self: *Self, content: snapshot.SnapshotContent) bool {
        _ = self;
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
/// Entry points are `appendProblem` and `deepCopyVar`
pub const Store = struct {
    const Self = @This();
    const ALIGNMENT = std.mem.Alignment.@"16";

    problems: std.ArrayListAligned(Problem, ALIGNMENT) = .{},

    pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{
            .problems = try std.ArrayListAligned(Problem, ALIGNMENT).initCapacity(gpa, capacity),
        };
    }

    pub fn deinit(self: *Self, gpa: Allocator) void {
        self.problems.deinit(gpa);
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
