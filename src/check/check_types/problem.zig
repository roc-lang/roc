//! Generate Reports for type checking errors

const std = @import("std");
const base = @import("../../base.zig");
const tracy = @import("../../tracy.zig");
const collections = @import("../../collections.zig");
const can = @import("../canonicalize.zig");
const types_mod = @import("../../types/types.zig");
const reporting = @import("../../reporting.zig");
const store_mod = @import("../../types/store.zig");
const snapshot = @import("./snapshot.zig");

const Report = reporting.Report;
const Document = reporting.Document;
const UnderlineRegion = @import("../../reporting/document.zig").UnderlineRegion;
const SourceCodeDisplayRegion = @import("../../reporting/document.zig").SourceCodeDisplayRegion;

const CIR = can.CIR;
const TypesStore = store_mod.Store;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;

const MkSafeMultiList = collections.SafeMultiList;

const SnapshotContentIdx = snapshot.SnapshotContentIdx;

const Var = types_mod.Var;
const Content = types_mod.Content;

/// The kind of problem we're dealing with
pub const Problem = union(enum) {
    type_mismatch: TypeMismatch,
    number_does_not_fit: NumberDoesNotFit,
    negative_unsigned_int: NegativeUnsignedInt,
    infinite_recursion: struct { var_: Var },
    anonymous_recursion: struct { var_: Var },
    invalid_number_type: VarProblem1,
    invalid_record_ext: VarProblem1,
    invalid_tag_union_ext: VarProblem1,
    bug: Bug,

    pub const SafeMultiList = MkSafeMultiList(@This());
    pub const Idx = SafeMultiList.Idx;
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
};

/// More specific details about a particular type mismatch. k
pub const TypeMismatchDetail = union(enum) {
    incompatible_list_elements: IncompatibleListElements,
    incompatible_if_cond,
    incompatible_if_branches: IncompatibleIfBranches,
    incompatible_match_patterns: IncompatibleMatchPatterns,
    incompatible_match_branches: IncompatibleMatchBranches,
    invalid_bool_binop: InvalidBoolBinop,
    invalid_nominal_tag,
    cross_module_import: CrossModuleImport,
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
    module_env: *const base.ModuleEnv,
    can_ir: *const can.CIR,
    snapshots: *const snapshot.Store,
    source: []const u8,
    filename: []const u8,

    /// Init report builder
    /// Only owned field is `buf`
    pub fn init(
        gpa: Allocator,
        module_env: *const base.ModuleEnv,
        can_ir: *const can.CIR,
        snapshots: *const snapshot.Store,
        source: []const u8,
        filename: []const u8,
    ) Self {
        return .{
            .gpa = gpa,
            .buf = std.ArrayList(u8).init(gpa),
            .module_env = module_env,
            .can_ir = can_ir,
            .snapshots = snapshots,
            .source = source,
            .filename = filename,
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

        var snapshot_writer = snapshot.SnapshotWriter.init(
            self.buf.writer(),
            self.snapshots,
            &self.module_env.idents,
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
                    }
                } else {
                    return self.buildGenericTypeMismatchReport(&snapshot_writer, mismatch.types);
                }
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

        try snapshot_writer.write(types.expected_snapshot);
        const owned_expected = try report.addOwnedString(self.buf.items[0..]);

        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.actual_snapshot);
        const owned_actual = try report.addOwnedString(self.buf.items[0..]);

        const region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(types.actual_var)));

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(self.source, region.start.offset, region.end.offset) catch |err| switch (err) {
            else => base.RegionInfo{
                .start_line_idx = 0,
                .start_col_idx = 0,
                .end_line_idx = 0,
                .end_col_idx = 0,
                .line_text = "",
            },
        };

        try report.document.addReflowingText("This expression is used in an unexpected way:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
        );
        try report.document.addLineBreak();

        try report.document.addText("It is of type:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_actual, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("But you are trying to use it as:");
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
        const actual_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(types.actual_var)));
        const expected_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.last_elem_expr)));
        const overall_start_offset = @min(actual_region.start.offset, expected_region.start.offset);
        const overall_end_offset = @max(actual_region.end.offset, expected_region.end.offset);

        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.line_starts.items.items,
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Get region info for both elements
        const actual_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.line_starts.items.items,
            actual_region.start.offset,
            actual_region.end.offset,
        ) catch return report;

        const expected_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.line_starts.items.items,
            expected_region.start.offset,
            expected_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = overall_region_info.line_text,
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
        const actual_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(types.actual_var)));
        const actual_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.line_starts.items.items,
            actual_region.start.offset,
            actual_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = actual_region_info.line_text,
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
        const last_if_branch_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.last_if_branch)));

        // TODO: getExprSpecific will panic if actual_var is not an Expr
        // It _should_ always be, but we should handle this better so it don't blow up
        const zoomed_in_var = self.can_ir.store.getExprSpecific(@enumFromInt(@intFromEnum(types.actual_var)));
        const actual_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(zoomed_in_var)));

        const overall_start_offset = @min(last_if_branch_region.start.offset, actual_region.start.offset);
        const overall_end_offset = @max(last_if_branch_region.end.offset, actual_region.end.offset);

        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.line_starts.items.items,
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Get region info for invalid branch
        const actual_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.line_starts.items.items,
            actual_region.start.offset,
            actual_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = overall_region_info.line_text,
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
        const match_expr_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.match_expr)));
        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.line_starts.items.items,
            match_expr_region.start.offset,
            match_expr_region.end.offset,
        ) catch return report;

        // Get region info for invalid branch
        const invalid_var_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(types.actual_var)));
        const invalid_var_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.line_starts.items.items,
            invalid_var_region.start.offset,
            invalid_var_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = overall_region_info.line_text,
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
        const expr_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.match_expr)));
        const this_branch_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(types.actual_var)));

        const overall_start_offset = expr_region.start.offset;
        const overall_end_offset = this_branch_region.end.offset;

        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.line_starts.items.items,
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = overall_region_info.line_text,
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
            self.module_env.line_starts.items.items,
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
        const expr_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.binop_expr)));
        const problem_side_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(types.actual_var)));

        const overall_start_offset = expr_region.start.offset;
        const overall_end_offset = problem_side_region.end.offset;

        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.line_starts.items.items,
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = overall_region_info.line_text,
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
            self.module_env.line_starts.items.items,
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

        // Create owned strings
        self.buf.clearRetainingCapacity();
        try snapshot_writer.write(types.expected_snapshot);
        const expected_type = try report.addOwnedString(self.buf.items);

        const actual_content = self.snapshots.getContent(types.actual_snapshot);
        std.debug.assert(actual_content == .structure);
        std.debug.assert(actual_content.structure == .tag_union);
        std.debug.assert(actual_content.structure.tag_union.tags.len() == 1);
        const actual_tag = self.snapshots.tags.get(actual_content.structure.tag_union.tags.start);
        const tag_name_bytes = self.can_ir.env.idents.getText(actual_tag.name);
        const tag_name = try report.addOwnedString(tag_name_bytes);

        // Add description
        try report.document.addText("I'm having trouble with this nominal tag:");
        try report.document.addLineBreak();

        // Determine the overall region that encompasses both elements
        const expr_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(types.expected_var)));
        const problem_side_region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(types.actual_var)));

        const overall_start_offset = expr_region.start.offset;
        const overall_end_offset = problem_side_region.end.offset;

        const overall_region_info = base.RegionInfo.position(
            self.source,
            self.module_env.line_starts.items.items,
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = overall_region_info.line_text,
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
            self.module_env.line_starts.items.items,
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

        // Show the invalid tag
        try report.document.addText("The tag is:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(tag_name, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the expected tags
        try report.document.addText("But it should be one of:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(expected_type, .type_variable);

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

        const region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.literal_var)));

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(self.source, region.start.offset, region.end.offset) catch |err| switch (err) {
            else => base.RegionInfo{
                .start_line_idx = 0,
                .start_col_idx = 0,
                .end_line_idx = 0,
                .end_col_idx = 0,
                .line_text = "",
            },
        };
        const literal_text = self.source[region.start.offset..region.end.offset];

        try report.document.addReflowingText("The number ");
        try report.document.addAnnotated(literal_text, .emphasized);
        try report.document.addReflowingText(" does not fit in its inferred type:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            self.filename,
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

        const region = self.can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.literal_var)));

        // Add source region highlighting
        const region_info = self.module_env.calcRegionInfo(self.source, region.start.offset, region.end.offset) catch |err| switch (err) {
            else => base.RegionInfo{
                .start_line_idx = 0,
                .start_col_idx = 0,
                .end_line_idx = 0,
                .end_col_idx = 0,
                .line_text = "",
            },
        };
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
            self.module_env.line_starts.items,
            import_region.start.offset,
            import_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .line_text = import_region_info.line_text,
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
        const module_name = if (module_idx < self.can_ir.imports.imports.items.len) blk: {
            const import_name = self.can_ir.imports.imports.items[module_idx];
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

    // unimplemented //

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

    problems: Problem.SafeMultiList,

    pub fn initCapacity(gpa: Allocator, capacity: usize) Self {
        return .{
            .problems = Problem.SafeMultiList.initCapacity(gpa, capacity),
        };
    }

    pub fn deinit(self: *Self, gpa: Allocator) void {
        self.problems.deinit(gpa);
    }

    /// Create a deep snapshot from a Var, storing it in this SnapshotStore
    pub fn appendProblem(self: *Self, gpa: Allocator, problem: Problem) Problem.SafeMultiList.Idx {
        return self.problems.append(gpa, problem);
    }
};
