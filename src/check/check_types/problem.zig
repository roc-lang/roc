//! Generate Reports for type checking errors

const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");
const can = @import("../canonicalize.zig");
const types = @import("../../types/types.zig");
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

const Var = types.Var;
const Content = types.Content;

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

    /// Build a report for a problem
    pub fn buildReport(
        problem: Problem,
        gpa: Allocator,
        buf: *std.ArrayList(u8),
        module_env: *const base.ModuleEnv,
        can_ir: *const can.CIR,
        snapshots: *const snapshot.Store,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        buf.clearRetainingCapacity();
        var snapshot_writer = snapshot.SnapshotWriter.init(
            buf.writer(),
            snapshots,
            &module_env.idents,
        );

        switch (problem) {
            .type_mismatch => |mismatch| {
                if (mismatch.detail) |detail| {
                    switch (detail) {
                        .incompatible_list_elements => |data| {
                            return buildIncompatibleListElementsReport(
                                gpa,
                                buf,
                                module_env,
                                can_ir,
                                &snapshot_writer,
                                mismatch,
                                data,
                                source,
                                filename,
                            );
                        },
                        .incompatible_if_cond => {
                            return buildInvalidIfCondition(
                                gpa,
                                buf,
                                module_env,
                                can_ir,
                                &snapshot_writer,
                                mismatch,
                                source,
                                filename,
                            );
                        },
                        .incompatible_if_branches => |data| {
                            return buildIncompatibleIfBranches(
                                gpa,
                                buf,
                                module_env,
                                can_ir,
                                &snapshot_writer,
                                mismatch,
                                data,
                                source,
                                filename,
                            );
                        },
                        .incompatible_match_patterns => |data| {
                            return buildIncompatibleMatchPatterns(
                                gpa,
                                buf,
                                module_env,
                                can_ir,
                                &snapshot_writer,
                                mismatch,
                                data,
                                source,
                                filename,
                            );
                        },
                        .incompatible_match_branches => |data| {
                            return buildIncompatibleMatchBranches(
                                gpa,
                                buf,
                                module_env,
                                can_ir,
                                &snapshot_writer,
                                mismatch,
                                data,
                                source,
                                filename,
                            );
                        },
                    }
                } else {
                    return buildTypeMismatchReport(
                        gpa,
                        buf,
                        module_env,
                        can_ir,
                        &snapshot_writer,
                        mismatch,
                        source,
                        filename,
                    );
                }
            },
            .number_does_not_fit => |data| {
                return buildNumberDoesNotFitReport(
                    gpa,
                    buf,
                    module_env,
                    can_ir,
                    &snapshot_writer,
                    data,
                    source,
                    filename,
                );
            },
            .negative_unsigned_int => |data| {
                return buildNegativeUnsignedIntReport(
                    gpa,
                    buf,
                    module_env,
                    can_ir,
                    &snapshot_writer,
                    data,
                    source,
                    filename,
                );
            },
            .infinite_recursion => |_| return buildUnimplementedReport(gpa),
            .anonymous_recursion => |_| return buildUnimplementedReport(gpa),
            .invalid_number_type => |_| return buildUnimplementedReport(gpa),
            .invalid_record_ext => |_| return buildUnimplementedReport(gpa),
            .invalid_tag_union_ext => |_| return buildUnimplementedReport(gpa),
            .bug => |_| return buildUnimplementedReport(gpa),
        }
    }

    /// Build a report for type mismatch diagnostic
    pub fn buildTypeMismatchReport(
        gpa: Allocator,
        buf: *std.ArrayList(u8),
        module_env: *const base.ModuleEnv,
        can_ir: *const can.CIR,
        writer: *snapshot.SnapshotWriter,
        vars: TypeMismatch,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(gpa, "TYPE MISMATCH", .runtime_error);

        try writer.write(vars.expected);
        const owned_expected = try report.addOwnedString(buf.items[0..]);

        buf.clearRetainingCapacity();
        try writer.write(vars.actual);
        const owned_actual = try report.addOwnedString(buf.items[0..]);

        const region = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(vars.actual_var)));

        // Add source region highlighting
        const region_info = module_env.calcRegionInfo(source, region.start.offset, region.end.offset) catch |err| switch (err) {
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
            source,
            region_info.start_line_idx,
            region_info.start_col_idx,
            region_info.end_line_idx,
            region_info.end_col_idx,
            .error_highlight,
            filename,
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
    pub fn buildIncompatibleListElementsReport(
        gpa: Allocator,
        buf: *std.ArrayList(u8),
        module_env: *const base.ModuleEnv,
        can_ir: *const can.CIR,
        snapshot_writer: *snapshot.SnapshotWriter,
        mismatch: TypeMismatch,
        data: IncompatibleListElements,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(gpa, "INCOMPATIBLE LIST ELEMENTS", .runtime_error);

        // Format the type strings
        buf.clearRetainingCapacity();
        try snapshot_writer.write(mismatch.expected);
        const expected_type = try report.addOwnedString(buf.items);

        buf.clearRetainingCapacity();
        try snapshot_writer.write(mismatch.actual);
        const actual_type = try report.addOwnedString(buf.items);

        buf.clearRetainingCapacity();
        try appendOrdinal(buf, data.incompatible_elem_index);
        const expected_type_ordinal = try report.addOwnedString(buf.items);

        buf.clearRetainingCapacity();
        try appendOrdinal(buf, data.incompatible_elem_index + 1);
        const actual_type_ordinal = try report.addOwnedString(buf.items);

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
        const actual_region = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(mismatch.actual_var)));
        const expected_reigon = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.last_elem_expr)));
        const overall_start_offset = @min(actual_region.start.offset, expected_reigon.start.offset);
        const overall_end_offset = @max(actual_region.end.offset, expected_reigon.end.offset);

        const overall_region_info = base.RegionInfo.position(
            source,
            module_env.line_starts.items,
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Get region info for both elements
        const actual_region_info = base.RegionInfo.position(
            source,
            module_env.line_starts.items,
            actual_region.start.offset,
            actual_region.end.offset,
        ) catch return report;

        const expected_region_info = base.RegionInfo.position(
            source,
            module_env.line_starts.items,
            expected_reigon.start.offset,
            expected_reigon.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .start_line = overall_region_info.start_line_idx + 1,
            .start_column = overall_region_info.start_col_idx + 1,
            .end_line = overall_region_info.end_line_idx + 1,
            .end_column = overall_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = filename,
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

        try report.document.addSourceCodeWithUnderlines(source, display_region, &underline_regions);
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
    pub fn buildInvalidIfCondition(
        gpa: Allocator,
        buf: *std.ArrayList(u8),
        module_env: *const base.ModuleEnv,
        can_ir: *const can.CIR,
        snapshot_writer: *snapshot.SnapshotWriter,
        mismatch: TypeMismatch,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(gpa, "INVALID IF CONDITION", .runtime_error);

        // Format the type strings
        buf.clearRetainingCapacity();
        try snapshot_writer.write(mismatch.actual);
        const actual_type = try report.addOwnedString(buf.items);

        // Add title
        try report.document.addText("This ");
        try report.document.addAnnotated("if", .keyword);
        try report.document.addText(" condition needs to be a ");
        try report.document.addAnnotated("Bool", .type_variable);
        try report.document.addText(":");
        try report.document.addLineBreak();

        // Get the region info for the invalid condition
        const actual_region = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(mismatch.actual_var)));
        const actual_region_info = base.RegionInfo.position(
            source,
            module_env.line_starts.items,
            actual_region.start.offset,
            actual_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .start_line = actual_region_info.start_line_idx + 1,
            .start_column = actual_region_info.start_col_idx + 1,
            .end_line = actual_region_info.end_line_idx + 1,
            .end_column = actual_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = filename,
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

        try report.document.addSourceCodeWithUnderlines(source, display_region, &underline_regions);
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
    pub fn buildIncompatibleIfBranches(
        gpa: Allocator,
        buf: *std.ArrayList(u8),
        module_env: *const base.ModuleEnv,
        can_ir: *const can.CIR,
        snapshot_writer: *snapshot.SnapshotWriter,
        mismatch: TypeMismatch,
        data: IncompatibleIfBranches,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        // The first branch of an if statement can never be invalid, since that
        // branch determines the type of the entire expression
        std.debug.assert(data.problem_branch_index > 0);

        // Is this error for a if statement with only 2 branches?
        const is_only_if_else = data.num_branches == 2;

        var report = Report.init(gpa, "INCOMPATIBLE IF BRANCHES", .runtime_error);

        // Format the type strings
        buf.clearRetainingCapacity();
        try snapshot_writer.write(mismatch.actual);
        const actual_type = try report.addOwnedString(buf.items);

        buf.clearRetainingCapacity();
        try snapshot_writer.write(mismatch.expected);
        const expected_type = try report.addOwnedString(buf.items);

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
            buf.clearRetainingCapacity();
            try buf.appendSlice("The type of the ");
            try appendOrdinal(buf, data.problem_branch_index + 1);
            try buf.appendSlice(" branch of this ");
            const title_prefix = try report.addOwnedString(buf.items);
            try report.document.addText(title_prefix);

            try report.document.addAnnotated("if", .keyword);
            try report.document.addText(" does not match the previous branches:");
        }
        try report.document.addLineBreak();

        // Determine the overall region that encompasses both elements
        const if_expr_region = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.last_if_branch)));
        const actual_region = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(mismatch.actual_var)));

        const overall_start_offset = @min(if_expr_region.start.offset, actual_region.start.offset);
        const overall_end_offset = @max(if_expr_region.end.offset, actual_region.end.offset);

        const overall_region_info = base.RegionInfo.position(
            source,
            module_env.line_starts.items,
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Get region info for invalid branch
        const actual_region_info = base.RegionInfo.position(
            source,
            module_env.line_starts.items,
            actual_region.start.offset,
            actual_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .start_line = overall_region_info.start_line_idx + 1,
            .start_column = overall_region_info.start_col_idx + 1,
            .end_line = overall_region_info.end_line_idx + 1,
            .end_column = overall_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = filename,
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

        try report.document.addSourceCodeWithUnderlines(source, display_region, &underline_regions);
        try report.document.addLineBreak();

        // Show the type of the invalid branch
        if (is_only_if_else) {
            try report.document.addText("The ");
            try report.document.addAnnotated("else", .keyword);
            try report.document.addText(" branch has the type:");
        } else {
            buf.clearRetainingCapacity();
            try buf.appendSlice("The ");
            try appendOrdinal(buf, data.problem_branch_index + 1);
            try buf.appendSlice(" branch has this type:");
            const branch_ord = try report.addOwnedString(buf.items);
            try report.document.addText(branch_ord);
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
    pub fn buildIncompatibleMatchPatterns(
        gpa: Allocator,
        buf: *std.ArrayList(u8),
        module_env: *const base.ModuleEnv,
        can_ir: *const can.CIR,
        snapshot_writer: *snapshot.SnapshotWriter,
        mismatch: TypeMismatch,
        data: IncompatibleMatchPatterns,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(gpa, "INCOMPATIBLE MATCH PATTERNS", .runtime_error);

        // Format the type strings
        buf.clearRetainingCapacity();
        try snapshot_writer.write(mismatch.actual);
        const actual_type = try report.addOwnedString(buf.items);

        buf.clearRetainingCapacity();
        try snapshot_writer.write(mismatch.expected);
        const expected_type = try report.addOwnedString(buf.items);

        buf.clearRetainingCapacity();
        try appendOrdinal(buf, data.problem_branch_index + 1);
        const branch_ord = try report.addOwnedString(buf.items);

        buf.clearRetainingCapacity();
        try appendOrdinal(buf, data.problem_pattern_index + 1);
        const pattern_ord = try report.addOwnedString(buf.items);

        // title
        if (data.num_patterns > 1) {
            buf.clearRetainingCapacity();
            try report.document.addText("The pattern ");
            try report.document.addText(pattern_ord);
            try report.document.addText(" pattern in this ");
            try report.document.addText(branch_ord);
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" differs from previous ones:");
            try report.document.addLineBreak();
        } else {
            buf.clearRetainingCapacity();
            try report.document.addText("The pattern in the ");
            try report.document.addText(branch_ord);
            try report.document.addText(" branch of this ");
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" differs from previous ones:");
            try report.document.addLineBreak();
        }

        // Determine the overall region that encompasses both elements
        const match_expr_region = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.match_expr)));
        const overall_region_info = base.RegionInfo.position(
            source,
            module_env.line_starts.items,
            match_expr_region.start.offset,
            match_expr_region.end.offset,
        ) catch return report;

        // Get region info for invalid branch
        const invalid_var_region = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(mismatch.actual_var)));
        const invalid_var_region_info = base.RegionInfo.position(
            source,
            module_env.line_starts.items,
            invalid_var_region.start.offset,
            invalid_var_region.end.offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .start_line = overall_region_info.start_line_idx + 1,
            .start_column = overall_region_info.start_col_idx + 1,
            .end_line = overall_region_info.end_line_idx + 1,
            .end_column = overall_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = filename,
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

        try report.document.addSourceCodeWithUnderlines(source, display_region, &underline_regions);
        try report.document.addLineBreak();

        // Show the type of the invalid branch
        buf.clearRetainingCapacity();
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
    pub fn buildIncompatibleMatchBranches(
        gpa: Allocator,
        buf: *std.ArrayList(u8),
        module_env: *const base.ModuleEnv,
        can_ir: *const can.CIR,
        snapshot_writer: *snapshot.SnapshotWriter,
        mismatch: TypeMismatch,
        data: IncompatibleMatchBranches,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        // The 1st branch can never be incompatible
        std.debug.assert(data.problem_branch_index > 0);

        var report = Report.init(gpa, "INCOMPATIBLE MATCH BRANCHES", .runtime_error);

        // Format the type strings
        buf.clearRetainingCapacity();
        try snapshot_writer.write(mismatch.actual);
        const actual_type = try report.addOwnedString(buf.items);

        buf.clearRetainingCapacity();
        try snapshot_writer.write(mismatch.expected);
        const expected_type = try report.addOwnedString(buf.items);

        buf.clearRetainingCapacity();
        try appendOrdinal(buf, data.problem_branch_index + 1);
        const branch_ord = try report.addOwnedString(buf.items);

        // Title
        if (data.num_branches == 2) {
            buf.clearRetainingCapacity();
            try report.document.addText("The second branch's type in this ");
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" is different from the first branch:");
        } else {
            buf.clearRetainingCapacity();
            try report.document.addText("The ");
            try report.document.addText(branch_ord);
            try report.document.addText(" branch's type in this ");
            try report.document.addAnnotated("match", .keyword);
            try report.document.addText(" is different from the previous ones:");
        }
        try report.document.addLineBreak();

        // Determine the overall region that encompasses both elements
        const expr_region = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.match_expr)));
        const this_branch_region = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(mismatch.actual_var)));

        const overall_start_offset = expr_region.start.offset;
        const overall_end_offset = this_branch_region.end.offset;

        const overall_region_info = base.RegionInfo.position(
            source,
            module_env.line_starts.items,
            overall_start_offset,
            overall_end_offset,
        ) catch return report;

        // Create the display region
        const display_region = SourceCodeDisplayRegion{
            .start_line = overall_region_info.start_line_idx + 1,
            .start_column = overall_region_info.start_col_idx + 1,
            .end_line = overall_region_info.end_line_idx + 1,
            .end_column = overall_region_info.end_col_idx + 1,
            .region_annotation = .dimmed,
            .filename = filename,
        };

        // Create underline regions
        const this_branch_region_info = base.RegionInfo.position(
            source,
            module_env.line_starts.items,
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

        try report.document.addSourceCodeWithUnderlines(source, display_region, &underline_regions);
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

    /// Build a report for "number does not fit in type" diagnostic
    pub fn buildNumberDoesNotFitReport(
        gpa: Allocator,
        buf: *std.ArrayList(u8),
        module_env: *const base.ModuleEnv,
        can_ir: *const can.CIR,
        writer: *snapshot.SnapshotWriter,
        data: NumberDoesNotFit,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(gpa, "NUMBER DOES NOT FIT IN TYPE", .runtime_error);

        buf.clearRetainingCapacity();
        try writer.write(data.expected_type);
        const owned_expected = try report.addOwnedString(buf.items[0..]);

        const region = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.literal_var)));

        // Add source region highlighting
        const region_info = module_env.calcRegionInfo(source, region.start.offset, region.end.offset) catch |err| switch (err) {
            else => base.RegionInfo{
                .start_line_idx = 0,
                .start_col_idx = 0,
                .end_line_idx = 0,
                .end_col_idx = 0,
                .line_text = "",
            },
        };
        const literal_text = source[region.start.offset..region.end.offset];

        try report.document.addReflowingText("The number ");
        try report.document.addAnnotated(literal_text, .emphasized);
        try report.document.addReflowingText(" does not fit in its inferred type:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            source,
            region_info.start_line_idx,
            region_info.start_col_idx,
            region_info.end_line_idx,
            region_info.end_col_idx,
            .error_highlight,
            filename,
        );
        try report.document.addLineBreak();

        try report.document.addText("Its inferred type is:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_expected, .type_variable);

        return report;
    }

    /// Build a report for "negative unsigned integer" diagnostic
    pub fn buildNegativeUnsignedIntReport(
        gpa: Allocator,
        buf: *std.ArrayList(u8),
        module_env: *const base.ModuleEnv,
        can_ir: *const can.CIR,
        writer: *snapshot.SnapshotWriter,
        data: NegativeUnsignedInt,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(gpa, "NEGATIVE UNSIGNED INTEGER", .runtime_error);

        buf.clearRetainingCapacity();
        try writer.write(data.expected_type);
        const owned_expected = try report.addOwnedString(buf.items[0..]);

        const region = can_ir.store.getNodeRegion(@enumFromInt(@intFromEnum(data.literal_var)));

        // Add source region highlighting
        const region_info = module_env.calcRegionInfo(source, region.start.offset, region.end.offset) catch |err| switch (err) {
            else => base.RegionInfo{
                .start_line_idx = 0,
                .start_col_idx = 0,
                .end_line_idx = 0,
                .end_col_idx = 0,
                .line_text = "",
            },
        };
        const literal_text = source[region.start.offset..region.end.offset];

        try report.document.addReflowingText("The number ");
        try report.document.addAnnotated(literal_text, .emphasized);
        try report.document.addReflowingText(" is ");
        try report.document.addAnnotated("signed", .emphasized);
        try report.document.addReflowingText(" because it is negative:");
        try report.document.addLineBreak();

        try report.document.addSourceRegion(
            source,
            region_info.start_line_idx,
            region_info.start_col_idx,
            region_info.end_line_idx,
            region_info.end_col_idx,
            .error_highlight,
            filename,
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

    /// Build a report for "invalid number literal" diagnostic
    pub fn buildUnimplementedReport(allocator: Allocator) !Report {
        const report = Report.init(allocator, "UNIMPLEMENTED", .runtime_error);
        return report;
    }

    fn appendOrdinal(buf: *std.ArrayList(u8), n: usize) !void {
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
    expected_var: Var,
    expected: SnapshotContentIdx,
    actual_var: Var,
    actual: SnapshotContentIdx,
    detail: ?TypeMismatchDetail,
};

/// More specific details about a particular type mismatch. k
pub const TypeMismatchDetail = union(enum) {
    incompatible_list_elements: IncompatibleListElements,
    incompatible_if_cond,
    incompatible_if_branches: IncompatibleIfBranches,
    incompatible_match_patterns: IncompatibleMatchPatterns,
    incompatible_match_branches: IncompatibleMatchBranches,
};

/// Problem data for when list elements have incompatible types
pub const IncompatibleListElements = struct {
    last_elem_expr: CIR.Expr.Idx,
    incompatible_elem_index: usize, // 0-based index of the incompatible element
    list_length: usize, // Total number of elements in the list
};

/// Problem data for when if branches have have incompatible types
pub const IncompatibleIfBranches = struct {
    parent_if_expr: CIR.Expr.Idx,
    last_if_branch: CIR.IfBranch.Idx,
    num_branches: usize,
    problem_branch_index: usize,
};

/// Problem data for when match patterns have have incompatible types
pub const IncompatibleMatchPatterns = struct {
    match_expr: CIR.Expr.Idx,
    num_branches: usize,
    problem_branch_index: usize,
    num_patterns: usize,
    problem_pattern_index: usize,
};

/// Problem data for when match branches have have incompatible types
pub const IncompatibleMatchBranches = struct {
    match_expr: CIR.Expr.Idx,
    num_branches: usize,
    problem_branch_index: usize,
};

// bug //

/// A bug that occurred during unification
pub const Bug = struct {
    expected_var: Var,
    expected: SnapshotContentIdx,
    actual_var: Var,
    actual: SnapshotContentIdx,
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
