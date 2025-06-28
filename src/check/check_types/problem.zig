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

const TypesStore = store_mod.Store;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;

const MkSafeMultiList = collections.SafeMultiList;

const SnapshotContentIdx = snapshot.SnapshotContentIdx;

const Var = types.Var;
const Content = types.Content;

/// The kind of problem we're dealing with
pub const Problem = union(enum) {
    type_mismatch: VarProblem2,
    incompatible_list_elements: IncompatibleListElements,
    number_does_not_fit: NumberDoesNotFit,
    negative_unsigned_int: NegativeUnsignedInt,
    infinite_recursion: struct { var_: Var },
    anonymous_recursion: struct { var_: Var },
    invalid_number_type: VarProblem1,
    invalid_record_ext: VarProblem1,
    invalid_tag_union_ext: VarProblem1,
    bug: VarProblem2,

    pub const SafeMultiList = MkSafeMultiList(@This());
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
        var snapshot_writer = snapshot.SnapshotWriter.init(
            buf.writer(),
            snapshots,
            &module_env.idents,
        );

        switch (problem) {
            .type_mismatch => |vars| {
                return buildTypeMismatchReport(
                    gpa,
                    buf,
                    module_env,
                    can_ir,
                    &snapshot_writer,
                    vars,
                    source,
                    filename,
                );
            },
            .incompatible_list_elements => |data| {
                return buildIncompatibleListElementsReport(
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
        vars: VarProblem2,
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
        _: *const can.CIR,
        snapshot_writer: *snapshot.SnapshotWriter,
        data: IncompatibleListElements,
        source: []const u8,
        filename: []const u8,
    ) !Report {
        var report = Report.init(gpa, "INCOMPATIBLE LIST ELEMENTS", .runtime_error);

        // Format the type strings
        buf.clearRetainingCapacity();
        try snapshot_writer.write(data.first_elem_snapshot);
        const owned_first_type = try report.addOwnedString(buf.items);

        buf.clearRetainingCapacity();
        try snapshot_writer.write(data.incompatible_elem_snapshot);
        const owned_incompatible_type = try report.addOwnedString(buf.items);

        // Add description (title already set in Report.init)
        try report.document.addText("This list contains elements with incompatible types:");
        try report.document.addLineBreak();

        // Show the list
        const list_region_info = base.RegionInfo.position(
            source,
            module_env.line_starts.items,
            data.list_region.start.offset,
            data.list_region.end.offset,
        ) catch return report;
        try report.document.addSourceRegion(
            source,
            list_region_info.start_line_idx,
            list_region_info.start_col_idx,
            list_region_info.end_line_idx,
            list_region_info.end_col_idx,
            .error_highlight,
            filename,
        );
        try report.document.addLineBreak();

        // Show the first element type
        try report.document.addText("The element");
        try report.document.addLineBreak();
        // Extract just the element text from the source
        const first_elem_text = source[data.first_elem_region.start.offset..data.first_elem_region.end.offset];
        try report.document.addText("    ");
        try report.document.addAnnotated(first_elem_text, .inline_code);
        try report.document.addLineBreak();
        try report.document.addText("has the type");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_first_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        // Show the incompatible element
        try report.document.addText("However, the element");
        try report.document.addLineBreak();
        // Extract just the element text from the source
        const incompatible_elem_text = source[data.incompatible_elem_region.start.offset..data.incompatible_elem_region.end.offset];
        try report.document.addText("    ");
        try report.document.addAnnotated(incompatible_elem_text, .inline_code);
        try report.document.addLineBreak();
        try report.document.addText("has the incompatible type:");
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(owned_incompatible_type, .type_variable);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        try report.document.addText("All elements in a list must have compatible types.");

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
};

/// A single var problem
pub const VarProblem1 = struct {
    var_: Var,
    snapshot: SnapshotContentIdx,
};

pub const IncompatibleListElements = struct {
    list_region: base.Region,
    first_elem_region: base.Region,
    first_elem_var: Var,
    first_elem_snapshot: SnapshotContentIdx,
    incompatible_elem_region: base.Region,
    incompatible_elem_var: Var,
    incompatible_elem_snapshot: SnapshotContentIdx,
};

/// A two var problem
pub const VarProblem2 = struct {
    expected_var: Var,
    expected: SnapshotContentIdx,
    actual_var: Var,
    actual: SnapshotContentIdx,
};

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

/// Self-contained problems store with resolved snapshots of type content
///
/// Whenever a type error occurs, we update the `Var` in the type store to
/// have `.err` content. This is necessary to continue type-checking but
/// looses essential error information. So before doing this, we create a fully
/// resolved snapshot of the type that we can use in reporting
///
/// Entry points are `appendProblem` and `createSnapshot`
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
