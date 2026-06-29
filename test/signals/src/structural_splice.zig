const std = @import("std");
const scope_runtime = @import("scope_runtime.zig");

pub const EachSite = scope_runtime.EachSite;

pub const ReplacementTarget = union(enum) {
    scope: u64,
    each_site: EachSite,
};

pub const PatchTargets = struct {
    removed: ReplacementTarget,
    replacement: ReplacementTarget,
};

pub const Splice = struct {
    removed_elem_ids: []u64,
    touched_parent_ids: []u64,
    replacement_elem_ids: []u64,
    replacement_on_change_indices: []usize,
    replacement_mount_indices: []usize,

    pub fn deinit(self: Splice, allocator: std.mem.Allocator) void {
        allocator.free(self.removed_elem_ids);
        allocator.free(self.touched_parent_ids);
        allocator.free(self.replacement_elem_ids);
        allocator.free(self.replacement_on_change_indices);
        allocator.free(self.replacement_mount_indices);
    }
};

pub const SpliceAndTargets = struct {
    splice: Splice,
    targets: PatchTargets,
};

pub fn scopeIsInTargetSet(target_scopes: []const bool, scope_id: u64) bool {
    if (scope_id >= target_scopes.len) @panic("descriptor referenced scope outside replacement target set");
    return target_scopes[@intCast(scope_id)];
}

fn removalIndexDesc(_: void, lhs: usize, rhs: usize) bool {
    return lhs > rhs;
}

pub fn sortRemovalIndexesDescending(indexes: []usize) void {
    std.mem.sort(usize, indexes, {}, removalIndexDesc);
}

pub fn appendRemovalIndex(allocator: std.mem.Allocator, indexes: *std.ArrayListUnmanaged(usize), index: ?usize) void {
    indexes.append(allocator, index orelse return) catch @panic("out of memory");
}

pub fn appendTextFieldRemovalIndexes(allocator: std.mem.Allocator, indexes: *std.ArrayListUnmanaged(usize), fields: anytype) void {
    appendRemovalIndex(allocator, indexes, fields.text);
    appendRemovalIndex(allocator, indexes, fields.role);
    appendRemovalIndex(allocator, indexes, fields.label);
    appendRemovalIndex(allocator, indexes, fields.test_id);
    appendRemovalIndex(allocator, indexes, fields.value);
    appendRemovalIndex(allocator, indexes, fields.class);
}

pub fn appendBoolFieldRemovalIndexes(allocator: std.mem.Allocator, indexes: *std.ArrayListUnmanaged(usize), fields: anytype) void {
    appendRemovalIndex(allocator, indexes, fields.checked);
    appendRemovalIndex(allocator, indexes, fields.disabled);
}

pub fn appendEventRemovalIndexes(allocator: std.mem.Allocator, indexes: *std.ArrayListUnmanaged(usize), events: anytype) void {
    appendRemovalIndex(allocator, indexes, events.click);
    appendRemovalIndex(allocator, indexes, events.input);
    appendRemovalIndex(allocator, indexes, events.check);
    appendRemovalIndex(allocator, indexes, events.pointer_down);
    appendRemovalIndex(allocator, indexes, events.pointer_up);
    appendRemovalIndex(allocator, indexes, events.pointer_enter);
    appendRemovalIndex(allocator, indexes, events.pointer_leave);
}

test "structural splice owns replacement slices" {
    const allocator = std.testing.allocator;
    const splice = Splice{
        .removed_elem_ids = try allocator.dupe(u64, &.{ 1, 2 }),
        .touched_parent_ids = try allocator.dupe(u64, &.{3}),
        .replacement_elem_ids = try allocator.dupe(u64, &.{ 4, 5 }),
        .replacement_on_change_indices = try allocator.dupe(usize, &.{6}),
        .replacement_mount_indices = try allocator.dupe(usize, &.{7}),
    };
    splice.deinit(allocator);
}

test "structural splice collects removal indexes" {
    const TextFields = struct {
        text: ?usize = 1,
        role: ?usize = null,
        label: ?usize = 7,
        test_id: ?usize = null,
        value: ?usize = 3,
        class: ?usize = null,
    };

    var indexes: std.ArrayListUnmanaged(usize) = .empty;
    defer indexes.deinit(std.testing.allocator);

    appendTextFieldRemovalIndexes(std.testing.allocator, &indexes, TextFields{});
    sortRemovalIndexesDescending(indexes.items);

    try std.testing.expectEqualSlices(usize, &.{ 7, 3, 1 }, indexes.items);
    try std.testing.expect(scopeIsInTargetSet(&.{ false, true, false }, 1));
}
