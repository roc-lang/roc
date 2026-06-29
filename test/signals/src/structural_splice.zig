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
