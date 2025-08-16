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
    number_does_not_fit: NumberDoesNotFit,
    negative_unsigned_int: NegativeUnsignedInt,
    infinite_recursion: struct { var_: Var },
    anonymous_recursion: struct { var_: Var },
    invalid_number_type: VarProblem1,
    invalid_record_ext: VarProblem1,
    invalid_tag_union_ext: VarProblem1,
    bug: Bug,

    pub const Idx = enum(u32) { _ };
    pub const Tag = std.meta.Tag(@This());
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
    incompatible_match_patterns: IncompatibleMatchPatterns,
    incompatible_match_branches: IncompatibleMatchBranches,
    invalid_bool_binop: InvalidBoolBinop,
    invalid_nominal_tag,
    cross_module_import: CrossModuleImport,
    incompatible_fn_call_arg: IncompatibleFnCallArg,
    incompatible_fn_return_type: IncompatibleFnReturnType,
    incompatible_fn_args_bound_var: IncompatibleFnArgsBoundVar,
};

/// Problem data for when list elements have incompatible types
pub const IncompatibleListElements = struct {
    last_elem_expr: CIR.Expr.Idx,
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

/// Problem data when function return type doesn't match
pub const IncompatibleFnReturnType = struct {
    fn_name: ?Ident.Idx,
    return_var: Var,
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

// bug //

/// Error when you try to apply the wrong number of arguments to a type in
/// an annotation
pub const TypeApplyArityMismatch = struct {
    type_name: base.Ident.Idx,
    anno_var: Var,
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
    buf: std.ArrayList(u8),
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
            .buf = std.ArrayList(u8).init(gpa),
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
        self.buf.deinit();
    }

    /// Build a report for a problem
    pub fn build(
        self: *Self,
        problem: Problem,
    ) !Report {
        const trace = tracy.trace(@src());
        defer trace.end();

        var snapshot_writer = snapshot.SnapshotWriter.initWithContext(
            self.buf.writer(),
            self.snapshots,
            self.module_env.getIdentStore(),
            self.can_ir.module_name,
            self.can_ir,
            self.other_modules,
        );

        switch (problem) {
            .type_mismatch => |mismatch| {
                if (mismatch.detail) |detail| {
                    switch (detail) {
                        .incompatible_list_elements => |data| {
                            return self.buildIncompatibleListElementsReport(&snapshot_writer, mismatch.types, data);
                        },
                        .incompatible_if_cond => {
                            return self.buildInvalidIfCondition(&snapshot_writer, mismatch.types);
                        },
                        .incompatible_if_branches => |data| {
                            return self.buildIncompatibleIfBranches(&snapshot_writer, mismatch.types, data);
                        },
                        .incompatible_match_patterns => |data| {
                            return self.buildIncompatibleMatchPatterns(&snapshot_writer, mismatch.types, data);
                        },
                        .incompatible_match_branches => |data| {
                            return self.buildIncompatibleMatchBranches(&snapshot_writer, mismatch.types, data);
                        },
                        .invalid_bool_binop => |data| {
                            return self.buildInvalidBoolBinop(&snapshot_writer, mismatch.types, data);
                        },
                        .invalid_nominal_tag => {
                            return self.buildInvalidNominalTag(&snapshot_writer, mismatch.types);
                        },
                        .cross_module_import => |data| {
                            return self.buildCrossModuleImportError(&snapshot_writer, mismatch.types, data);
                        },
                        .incompatible_fn_call_arg => |data| {
                            return self.buildIncompatibleFnCallArg(&snapshot_writer, mismatch.types, data);
                        },
                        .incompatible_fn_return_type => |data| {
                            return self.buildIncompatibleFnReturnType(&snapshot_writer, mismatch.types, data);
                        },
                        .incompatible_fn_args_bound_var => |data| {
                            return self.buildIncompatibleFnArgsBoundVar(&snapshot_writer, mismatch.types, data);
                        },
                    }
                } else {
                    return self.buildGenericTypeMismatchReport(&snapshot_writer, mismatch.types);
                }
            },
            .type_apply_mismatch_arities => |data| {
                return self.buildTypeApplyArityMismatchReport(&snapshot_writer, data);
            },
            .number_does_not_fit => |data| {
                return self.buildNumberDoesNotFitReport(&snapshot_writer, data);
            },
            .negative_unsigned_int => |data| {
                return self.buildNegativeUnsignedIntReport(&snapshot_writer, data);
            },
            .infinite_recursion => |_| return self.buildUnimplementedReport(),
            .anonymous_recursion => |_| return self.buildUnimplementedReport(),
            .invalid_number_type => |_| return self.buildUnimplementedReport(),
            .invalid_record_ext => |_| return self.buildUnimplementedReport(),
            .invalid_tag_union_ext => |_| return self.buildUnimplementedReport(),
            .bug => |_| return self.buildUnimplementedReport(),
        }
    }

    // type mismatch //

    /// Build a report for type mismatch diagnostic
    fn buildGenericTypeMismatchReport(
        self: *Self,
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.actual_snapshot);
        const owned_actual = try report.addOwnedString(self.buf.items[0..]);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.expected_snapshot);
        const owned_expected = try report.addOwnedString(self.buf.items[0..]);

        // For annotation mismatches, we want to highlight the expression that doesn't match,
        // not the annotation itself. When from_annotation is true and we're showing
        // "The type annotation says...", the expression is in expected_var.
        // If we have a constraint origin (e.g., from dot access), use that for more precise highlighting.

        const region_var = if (types.constraint_origin_var) |origin_var|
            origin_var
        else if (types.from_annotation)
            types.expected_var // Use expected_var to highlight the expression causing the mismatch
        else
            types.actual_var;
        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(region_var)));

        // Check if both types are functions to provide more specific error messages
        if (types.from_annotation) {
            // Check the snapshot content to determine if we have function types
            const expected_content = self.snapshots.getContent(types.expected_snapshot);
            const actual_content = self.snapshots.getContent(types.actual_snapshot);

            if (self.areBothFunctionSnapshots(expected_content, actual_content)) {
                // Both are functions - determine if it's an argument or return type mismatch
                // Extract argument and return types
                const expected_arg = self.extractFirstArgTypeFromFunctionSnapshot(types.expected_snapshot);
                const actual_arg = self.extractFirstArgTypeFromFunctionSnapshot(types.actual_snapshot);

                // Check if arguments match by comparing their string representations
                var args_match = false;
                if (expected_arg != null and actual_arg != null) {
                    self.buf.clearRetainingCapacity();
                    snapshot_writer.write(expected_arg.?) catch {};
                    const expected_arg_str = self.gpa.dupe(u8, self.buf.items) catch "";

                    self.buf.clearRetainingCapacity();
                    snapshot_writer.write(actual_arg.?) catch {};
                    const actual_arg_str = self.buf.items;

                    args_match = std.mem.eql(u8, expected_arg_str, actual_arg_str);
                    self.gpa.free(expected_arg_str);
                }

                // Only use specialized function error messages when we have constraint_origin_var
                // which indicates this is actually about a function call argument mismatch
                if (types.constraint_origin_var != null and !args_match) {
                    // Argument type mismatch (e.g., str.to_utf8())
                    return self.buildIncompatibleFnCallArg(snapshot_writer, types, .{
                        .fn_name = null,
                        .arg_var = types.constraint_origin_var.?,
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

        if (types.from_annotation) {
            try report.document.addText("The type annotation says it should have the type:");
        } else {
            try report.document.addText("It is of type:");
        }
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_actual, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("But here it's being used as:");
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
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
        data: IncompatibleListElements,
    ) !Report {
        var report = Report.init(self.gpa, "INCOMPATIBLE LIST ELEMENTS", .runtime_error);

        // Create owned strings
        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try appendOrdinal(&self.buf, data.incompatible_elem_index);
        const expected_type_ordinal = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try appendOrdinal(&self.buf, data.incompatible_elem_index + 1);
        const actual_type_ordinal = try report.addOwnedString(self.buf.items);

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
        const expected_region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.last_elem_expr)));
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
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID IF CONDITION", .runtime_error);

        // Create owned strings
        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.buf.items);

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
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
        data: IncompatibleIfBranches,
    ) !Report {
        // The first branch of an if statement can never be invalid, since that
        // branch determines the type of the entire expression
        std.debug.assert(data.problem_branch_index > 0);

        // Is this error for a if statement with only 2 branches?
        const is_only_if_else = data.num_branches == 2;

        var report = Report.init(self.gpa, "INCOMPATIBLE IF BRANCHES", .runtime_error);

        // Create owned strings
        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try appendOrdinal(&self.buf, data.problem_branch_index + 1);
        const branch_ordinal = try report.addOwnedString(self.buf.items);

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

    /// Build a report for incompatible match branches
    fn buildIncompatibleMatchPatterns(
        self: *Self,
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
        data: IncompatibleMatchPatterns,
    ) !Report {
        var report = Report.init(self.gpa, "INCOMPATIBLE MATCH PATTERNS", .runtime_error);

        // Create owned strings
        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try appendOrdinal(&self.buf, data.problem_branch_index + 1);
        const branch_ord = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try appendOrdinal(&self.buf, data.problem_pattern_index + 1);
        const pattern_ord = try report.addOwnedString(self.buf.items);

        // Add description
        if (data.num_patterns > 1) {
            self.buf.clearRetainingCapacity();
            try report.document.addText("The pattern ");
            try report.document.addText(pattern_ord);
            try report.document.addText(" pattern in this ");
            try report.document.addText(branch_ord);
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" differs from previous ones:");
            try report.document.addLineBreak();
        } else {
            self.buf.clearRetainingCapacity();
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
        self.buf.clearRetainingCapacity();
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
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
        data: IncompatibleMatchBranches,
    ) !Report {
        // The 1st branch can never be incompatible
        std.debug.assert(data.problem_branch_index > 0);

        var report = Report.init(self.gpa, "INCOMPATIBLE MATCH BRANCHES", .runtime_error);

        // Create owned strings
        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try appendOrdinal(&self.buf, data.problem_branch_index + 1);
        const branch_ord = try report.addOwnedString(self.buf.items);

        // Add description
        if (data.num_branches == 2) {
            self.buf.clearRetainingCapacity();
            try report.document.addText("The second branch's type in this ");
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" is different from the first branch:");
        } else {
            self.buf.clearRetainingCapacity();
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
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
        data: InvalidBoolBinop,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID BOOL OPERATION", .runtime_error);

        // Create owned strings
        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.buf.items);

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
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
    ) !Report {
        var report = Report.init(self.gpa, "INVALID NOMINAL TAG", .runtime_error);

        // Create actual tag str
        const actual_content = self.snapshots.getContent(types.actual_snapshot);
        std.debug.assert(actual_content == .structure);
        std.debug.assert(actual_content.structure == .tag_union);
        std.debug.assert(actual_content.structure.tag_union.tags.len() == 1);
        const actual_tag = self.snapshots.tags.get(actual_content.structure.tag_union.tags.start);
        self.buf.clearRetainingCapacity();
        try snapshot_writer.writeTag(actual_tag, types.actual_snapshot);
        const actual_tag_str = try report.addOwnedString(self.buf.items);

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
            self.buf.clearRetainingCapacity();
            try snapshot_writer.writeTag(expected_tag, types.expected_snapshot);
            const expected_tag_str = try report.addOwnedString(self.buf.items);

            try report.document.addText("But it should be:");
            try report.document.addLineBreak();
            try report.document.addText("    ");
            try report.document.addAnnotated(expected_tag_str, .type_variable);
        } else {
            self.buf.clearRetainingCapacity();
            try snapshot_writer.write(types.expected_snapshot);
            const expected_type = try report.addOwnedString(self.buf.items);

            try report.document.addText("But it should be one of:");
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
                    snapshot_writer.resetContext();

                    self.buf.clearRetainingCapacity();
                    try snapshot_writer.writeTag(cur_expected_tag, types.expected_snapshot);
                    const cur_expected_tag_str = try report.addOwnedString(self.buf.items);

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
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
        data: IncompatibleFnCallArg,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);

        self.buf.clearRetainingCapacity();
        try appendOrdinal(&self.buf, data.incompatible_arg_index + 1);
        const arg_index = try report.addOwnedString(self.buf.items);

        // Extract only the argument types from the function snapshots
        const actual_arg_type = self.extractFirstArgTypeFromFunctionSnapshot(types.actual_snapshot) orelse types.actual_snapshot;
        const expected_arg_type = self.extractFirstArgTypeFromFunctionSnapshot(types.expected_snapshot) orelse types.expected_snapshot;

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(actual_arg_type);
        const actual_type = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(expected_arg_type);
        const expected_type = try report.addOwnedString(self.buf.items);

        try report.document.addText("The ");
        try report.document.addText(arg_index);
        try report.document.addText(" argument being passed here doesn't fit the type the function expects:");
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

        try report.document.addReflowingText("This argument is of type:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addReflowingText("But ");
        if (data.fn_name) |fn_name_ident| {
            self.buf.clearRetainingCapacity();
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

    /// Build a report for function return type mismatch
    fn buildIncompatibleFnReturnType(
        self: *Self,
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
        data: IncompatibleFnReturnType,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);

        // Extract only the return types from the function snapshots
        const actual_return_type = self.extractReturnTypeFromFunctionSnapshot(types.actual_snapshot) orelse types.actual_snapshot;
        const expected_return_type = self.extractReturnTypeFromFunctionSnapshot(types.expected_snapshot) orelse types.expected_snapshot;

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(actual_return_type);
        const actual_type = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(expected_return_type);
        const expected_type = try report.addOwnedString(self.buf.items);

        try report.document.addText("This function's return type is not what I expect:");
        try report.document.addLineBreak();

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.return_var)));
        const region_info = self.module_env.calcRegionInfo(region.*);
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
            self.source,
            self.module_env.getLineStarts(),
        );
        try report.document.addLineBreak();

        try report.document.addReflowingText("This returns:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addReflowingText("But ");
        if (data.fn_name) |fn_name_ident| {
            self.buf.clearRetainingCapacity();
            const fn_name = try report.addOwnedString(self.can_ir.getIdent(fn_name_ident));
            try report.document.addReflowingText("the annotation on ");
            try report.document.addAnnotated(fn_name, .inline_code);
            try report.document.addReflowingText(" says it should return:");
        } else {
            try report.document.addReflowingText("the annotation says it should return:");
        }
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(expected_type, .type_variable);

        return report;
    }

    fn buildIncompatibleFnArgsBoundVar(
        self: *Self,
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
        data: IncompatibleFnArgsBoundVar,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);

        self.buf.clearRetainingCapacity();
        try appendOrdinal(&self.buf, data.first_arg_index + 1);
        const first_arg_index = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try appendOrdinal(&self.buf, data.second_arg_index + 1);
        const second_arg_index = try report.addOwnedString(self.buf.items);

        // The types from unification already have the correct snapshots
        // expected = first argument, actual = second argument
        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.expected_snapshot);
        const first_type = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.actual_snapshot);
        const second_type = try report.addOwnedString(self.buf.items);

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
        try report.document.addReflowingText(" argument is of type:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(first_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addReflowingText("But the ");
        try report.document.addText(second_arg_index);
        try report.document.addReflowingText(" argument is of type:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(second_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        if (data.fn_name) |fn_name_ident| {
            self.buf.clearRetainingCapacity();
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
        _: *snapshot.SnapshotWriter,
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

        const type_name = try report.addOwnedString(self.can_ir.getIdent(data.type_name));

        self.buf.clearRetainingCapacity();
        try self.buf.writer().print("{d}", .{data.num_expected_args});
        const num_expected_args = try report.addOwnedString(self.buf.items);

        self.buf.clearRetainingCapacity();
        try self.buf.writer().print("{d}", .{data.num_actual_args});
        const num_actual_args = try report.addOwnedString(self.buf.items);

        const region = self.can_ir.store.regions.get(@enumFromInt(@intFromEnum(data.anno_var)));

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(region.*);

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

    // number problems //

    /// Build a report for "number does not fit in type" diagnostic
    fn buildNumberDoesNotFitReport(
        self: *Self,
        snapshot_writer: *snapshot.SnapshotWriter,
        data: NumberDoesNotFit,
    ) !Report {
        var report = Report.init(self.gpa, "NUMBER DOES NOT FIT IN TYPE", .runtime_error);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(data.expected_type);
        const owned_expected = try report.addOwnedString(self.buf.items[0..]);

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
        snapshot_writer: *snapshot.SnapshotWriter,
        data: NegativeUnsignedInt,
    ) !Report {
        var report = Report.init(self.gpa, "NEGATIVE UNSIGNED INTEGER", .runtime_error);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(data.expected_type);
        const owned_expected = try report.addOwnedString(self.buf.items[0..]);

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
        snapshot_writer: *snapshot.SnapshotWriter,
        types: TypePair,
        data: CrossModuleImport,
    ) !Report {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);

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

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.buf.items);
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

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.actual_snapshot);
        const actual_type = try report.addOwnedString(self.buf.items);
        try report.document.addText("    ");
        try report.document.addAnnotated(actual_type, .type_variable);
        try report.document.addLineBreak();

        return report;
    }

    /// Build a report for "invalid number literal" diagnostic
    fn buildUnimplementedReport(self: *Self) !Report {
        const report = Report.init(self.gpa, "UNIMPLEMENTED", .runtime_error);
        return report;
    }

    // helpers //

    // Given a buffer and a number, write a the human-readably ordinal number
    // Note that the caller likely needs to clear the buffer before calling this function
    fn appendOrdinal(buf: *std.ArrayList(u8), n: u32) !void {
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

    /// Check if two function snapshots have unifiable return types (simplified check)
    fn functionSnapshotsHaveUnifiableReturnTypes(self: *Self, expected_content: snapshot.SnapshotContent, actual_content: snapshot.SnapshotContent) bool {
        _ = self;
        _ = expected_content;
        _ = actual_content;
        // For now, we'll assume that if both are function types and have reached this point,
        // their return types are likely compatible (the user's insight was that
        // Result(Error, [InvalidHex(Str)]) and Result(Error, [InvalidHex(Str)]_others) unify)
        // A more sophisticated implementation would extract the return types and compare them

        // Simple heuristic: if we get here, the return types probably unify
        return true;
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

    /// Extract the return type from a function snapshot
    fn extractReturnTypeFromFunctionSnapshot(self: *Self, func_snapshot: SnapshotContentIdx) ?SnapshotContentIdx {
        const content = self.snapshots.getContent(func_snapshot);

        return switch (content) {
            .structure => |structure| switch (structure) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| func.ret,
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
    const ALIGNMENT = 16;

    problems: std.ArrayListAlignedUnmanaged(Problem, ALIGNMENT) = .{},

    pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{
            .problems = try std.ArrayListAlignedUnmanaged(Problem, ALIGNMENT).initCapacity(gpa, capacity),
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
