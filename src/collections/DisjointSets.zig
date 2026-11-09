//! Union-find over dense `u32` indices. Each set is represented by its
//! lowest-numbered member, and `find` halves paths as it walks them.

const std = @import("std");
const Allocator = std.mem.Allocator;

const DisjointSets = @This();

parents: []u32,

/// Every index in its own set.
pub fn init(allocator: Allocator, count: usize) Allocator.Error!DisjointSets {
    const parents = try allocator.alloc(u32, count);
    for (parents, 0..) |*parent, index| parent.* = @intCast(index);
    return .{ .parents = parents };
}

pub fn deinit(self: *DisjointSets, allocator: Allocator) void {
    allocator.free(self.parents);
}

/// The lowest-numbered member of `start`'s set.
pub fn find(self: *DisjointSets, start: u32) u32 {
    var current = start;
    while (self.parents[current] != current) {
        self.parents[current] = self.parents[self.parents[current]];
        current = self.parents[current];
    }
    return current;
}

/// Merge the sets holding `a` and `b`; the smaller root becomes the representative.
pub fn unite(self: *DisjointSets, a: u32, b: u32) void {
    const a_root = self.find(a);
    const b_root = self.find(b);
    self.parents[@max(a_root, b_root)] = @min(a_root, b_root);
}

test "disjoint sets name each set by its lowest member" {
    var sets = try DisjointSets.init(std.testing.allocator, 6);
    defer sets.deinit(std.testing.allocator);
    sets.unite(4, 2);
    sets.unite(5, 4);
    sets.unite(1, 3);
    try std.testing.expectEqual(@as(u32, 2), sets.find(5));
    try std.testing.expectEqual(@as(u32, 2), sets.find(4));
    try std.testing.expectEqual(@as(u32, 1), sets.find(3));
    try std.testing.expectEqual(@as(u32, 0), sets.find(0));
    sets.unite(3, 5);
    try std.testing.expectEqual(@as(u32, 1), sets.find(2));
}
