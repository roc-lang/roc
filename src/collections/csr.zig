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
    const node_count = starts.len - 1;
    const reversed_starts = try allocator.alloc(u32, node_count + 1);
    @memset(reversed_starts, 0);
    for (targets) |target| reversed_starts[target + 1] += 1;
    for (1..node_count + 1) |index| reversed_starts[index] += reversed_starts[index - 1];
    const reversed = try allocator.alloc(u32, targets.len);
    const fill = try allocator.dupe(u32, reversed_starts[0..node_count]);
    defer allocator.free(fill);
    for (0..node_count) |source| {
        for (targets[starts[source]..starts[source + 1]]) |target| {
            reversed[fill[target]] = @intCast(source);
            fill[target] += 1;
        }
    }
    return .{ .starts = reversed_starts, .targets = reversed };
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
