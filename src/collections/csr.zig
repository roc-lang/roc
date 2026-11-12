//! Compressed adjacency: `starts[n]..starts[n + 1]` indexes node n's
//! targets in `targets`.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// A compressed sparse row adjacency: node `n`'s targets are `targets[starts[n]..starts[n + 1]]`.
pub const Csr = struct {
    starts: []u32,
    targets: []u32,
};

/// The reversed graph: every edge `a -> b` becomes `b -> a`, with each
/// node's sources in the order their edges appear.
pub fn transpose(allocator: Allocator, starts: []const u32, targets: []const u32) Allocator.Error!Csr {
    const groups = try @import("groups.zig").CsrGroups(u32).transpose(allocator, starts, targets);
    return .{ .starts = groups.starts, .targets = groups.items };
}

test "transpose reverses every edge in order" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    // 0 -> 1, 0 -> 2, 2 -> 1, 1 -> 1
    const starts = [_]u32{ 0, 2, 3, 4 };
    const targets = [_]u32{ 1, 2, 1, 1 };
    const reversed = try transpose(arena.allocator(), &starts, &targets);
    try std.testing.expectEqualSlices(u32, &.{ 0, 0, 3, 4 }, reversed.starts);
    try std.testing.expectEqualSlices(u32, &.{ 0, 1, 2, 0 }, reversed.targets);
}
