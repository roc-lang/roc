//! The diagnostics a checked module's own checking and compile-time evaluation
//! reported, stored beside its checked cache entry. A cache hit renders them
//! again from these problems, against the current filename, source and
//! imports, so a warm build reports exactly what a cold one does.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const problem = @import("problem.zig");
const snapshot = @import("snapshot.zig");
const serde = @import("artifact_serialize.zig");
const CheckingOutcome = @import("checking_outcome.zig");
const ImportMapping = @import("types").import_mapping.ImportMapping;

const Allocator = std.mem.Allocator;
const Self = @This();

/// Problems with the strings and snapshots they render with.
pub const Outcome = CheckingOutcome;

/// One row of the checker's import display-name mapping.
pub const ImportName = struct { qualified: base.Ident.Idx, display: base.Ident.Idx };

/// The problems before this index are checking's; evaluation reported the rest.
checking_problem_count: u64 = 0,
/// Every problem with the problem store's strings, and the snapshots
/// checking's problems render with.
outcome: CheckingOutcome = .{},
/// The display names the module's reports give its imports.
import_names: []const ImportName = &.{},

/// Portable storage for the module's reports.
pub const Serialized = extern struct {
    checking_problem_count: u64 = 0,
    outcome: CheckingOutcome.Serialized = .{},
    import_names: serde.SerializedSlice(ImportName) = .{},

    pub const root_contract: collections.serde_validation.RootContract = .{ .Owner = Self };

    pub fn serialize(self: *@This(), value: *const Self, allocator: Allocator, writer: *collections.CompactWriter) Allocator.Error!void {
        self.checking_problem_count = value.checking_problem_count;
        try self.outcome.serialize(&value.outcome, allocator, writer);
        try self.import_names.serialize(value.import_names, allocator, writer);
    }

    pub fn deserialize(self: *const @This(), address: usize) Self {
        return .{
            .checking_problem_count = self.checking_problem_count,
            .outcome = self.outcome.deserialize(address),
            .import_names = self.import_names.deserialize(address),
        };
    }
};

/// Whether the module reported anything its entry must reproduce.
pub fn isEmpty(self: *const Self) bool {
    return self.outcome.problems.len == 0;
}

/// The evaluation problems, which render without checking's snapshots.
pub fn evaluationProblemCount(self: *const Self) usize {
    return self.outcome.problems.len - @as(usize, @intCast(self.checking_problem_count));
}

/// Copy the snapshots checking's problems render with out of the
/// checker, which is released once the module is checked. The problem store
/// columns are left empty: the entry reads them from the store once
/// evaluation has added its own problems.
pub fn captureChecking(allocator: Allocator, checker: anytype) Allocator.Error!CheckingOutcome {
    const captured = try CheckingOutcome.capture(allocator, checker, null);
    // `capture` builds the text, tag, formatted-string and record-field
    // columns in `allocator` and borrows the rest from the checker.
    return .{
        .status = captured.status,
        .contents = try allocator.dupe(snapshot.SnapshotContent, captured.contents),
        .node_formats = try allocator.dupe(snapshot.NodeFormat, captured.node_formats),
        .captures = try allocator.dupe(snapshot.Capture, captured.captures),
        .content_indexes = try allocator.dupe(snapshot.SnapshotContentIdx, captured.content_indexes),
        .record_fields = captured.record_fields,
        .tags = captured.tags,
        .constraints = try allocator.dupe(snapshot.SnapshotStaticDispatchConstraint, captured.constraints),
        .formatted = captured.formatted,
        .text = captured.text,
    };
}

/// The reports of a module whose checking captured `checking` and whose
/// problem store, after evaluation, is `store`, copied into `allocator`.
pub fn fromStore(
    allocator: Allocator,
    checking: CheckingOutcome,
    checking_problem_count: usize,
    store: *const problem.Store,
    mapping: *const ImportMapping,
) Allocator.Error!Self {
    return .{
        .checking_problem_count = checking_problem_count,
        .outcome = .{
            .status = checking.status,
            .problems = try allocator.dupe(problem.Problem, store.problems.items),
            .extra_strings = try allocator.dupe(u8, store.extra_strings_backing.items),
            .missing_patterns = try allocator.dupe(problem.ExtraStringIdx, store.missing_patterns_backing.items),
            .contents = try allocator.dupe(snapshot.SnapshotContent, checking.contents),
            .node_formats = try allocator.dupe(snapshot.NodeFormat, checking.node_formats),
            .captures = try allocator.dupe(snapshot.Capture, checking.captures),
            .content_indexes = try allocator.dupe(snapshot.SnapshotContentIdx, checking.content_indexes),
            .record_fields = try allocator.dupe(snapshot.SnapshotRecordField, checking.record_fields),
            .tags = try allocator.dupe(CheckingOutcome.Tag, checking.tags),
            .constraints = try allocator.dupe(snapshot.SnapshotStaticDispatchConstraint, checking.constraints),
            .formatted = try allocator.dupe(CheckingOutcome.Formatted, checking.formatted),
            .text = try allocator.dupe(u8, checking.text),
        },
        .import_names = try importNames(allocator, mapping),
    };
}

fn importNames(allocator: Allocator, mapping: *const ImportMapping) Allocator.Error![]const ImportName {
    if (mapping.count() == 0) return &.{};
    const rows = try allocator.alloc(ImportName, mapping.count());
    var entries = mapping.iterator();
    var i: usize = 0;
    while (entries.next()) |entry| : (i += 1) rows[i] = .{ .qualified = entry.key_ptr.*, .display = entry.value_ptr.* };
    std.mem.sort(ImportName, rows, {}, struct {
        fn lessThan(_: void, a: ImportName, b: ImportName) bool {
            return @as(u32, @bitCast(a.qualified)) < @as(u32, @bitCast(b.qualified));
        }
    }.lessThan);
    return rows;
}

/// The checker's import display-name mapping the reports were rendered with.
pub fn instantiateImportMapping(self: *const Self, allocator: Allocator) Allocator.Error!ImportMapping {
    var mapping = ImportMapping.init(allocator);
    errdefer mapping.deinit();
    try mapping.ensureTotalCapacity(@intCast(self.import_names.len));
    for (self.import_names) |row| mapping.putAssumeCapacity(row.qualified, row.display);
    return mapping;
}

/// The problem store the module's checking and evaluation left, from which
/// a reopened evaluation continues.
pub fn instantiateProblems(self: *const Self, allocator: Allocator) Allocator.Error!problem.Store {
    var store = problem.Store.initEmpty(allocator);
    errdefer store.deinit();
    try self.outcome.installProblems(&store);
    return store;
}

/// The snapshots checking's problems render with, rendering on
/// demand with `format_context`.
pub fn instantiateRendering(
    self: *const Self,
    allocator: Allocator,
    snapshots: *snapshot.Store,
    format_context: snapshot.FormatContext,
) Allocator.Error!void {
    try self.outcome.installRendering(allocator, snapshots, format_context);
}

test "checked reports keep every problem, its strings and the import names across serialization" {
    const allocator = std.testing.allocator;
    var store = try problem.Store.init(allocator);
    defer store.deinit();
    const message = try store.putExtraString("the helper message is unavailable");
    _ = try store.appendProblem(.{ .comptime_crash = .{ .message = message, .region = base.Region.zero() } });
    var names = ImportMapping.init(allocator);
    defer names.deinit();
    const qualified: base.Ident.Idx = @bitCast(@as(u32, 9));
    const display: base.Ident.Idx = @bitCast(@as(u32, 4));
    try names.put(qualified, display);

    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    const reports = try fromStore(arena.allocator(), .{}, 0, &store, &names);
    try std.testing.expectEqual(@as(usize, 1), reports.evaluationProblemCount());

    var round_trip = try serde.roundTripForTest(allocator, Self, &reports);
    defer allocator.free(round_trip.buffer);
    try std.testing.expectEqual(@as(u64, 0), round_trip.loaded.checking_problem_count);
    var restored = try round_trip.loaded.instantiateProblems(allocator);
    defer restored.deinit();
    try std.testing.expectEqual(@as(usize, 1), restored.problems.items.len);
    try std.testing.expectEqualStrings("the helper message is unavailable", restored.getExtraString(restored.problems.items[0].comptime_crash.message));
    var restored_names = try round_trip.loaded.instantiateImportMapping(allocator);
    defer restored_names.deinit();
    try std.testing.expectEqual(display, restored_names.get(qualified).?);
}
