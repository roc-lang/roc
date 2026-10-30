//! Finite monotone equations, solved iteratively from their least element.
//! Completion uses 0/1/2; emptiness uses 0/1. No provisional value escapes.
const std = @import("std");
const Allocator = std.mem.Allocator;
const Self = @This();

/// How a child outcome contributes to its parent equation.
pub const Transform = enum { identity, unchanged, backing };
const none = std.math.maxInt(usize);
const Node = struct {
    value: u2 = 0,
    all: bool = false,
    remaining: usize = 0,
    first: usize = none,
};
const Edge = struct { parent: usize, next: usize, transform: Transform };
nodes: std.ArrayList(Node) = .empty,
edges: std.ArrayList(Edge) = .empty,
work: std.ArrayList(usize) = .empty,
/// Release all equation and worklist storage.
pub fn deinit(self: *Self, gpa: Allocator) void {
    self.nodes.deinit(gpa);
    self.edges.deinit(gpa);
    self.work.deinit(gpa);
}
/// Discard equations while retaining reusable scratch capacity.
pub fn clear(self: *Self) void {
    self.nodes.clearRetainingCapacity();
    self.edges.clearRetainingCapacity();
    self.work.clearRetainingCapacity();
}
/// Reserve one equation, initially at the least element.
pub fn add(self: *Self, gpa: Allocator) Allocator.Error!usize {
    const index = self.nodes.items.len;
    try self.nodes.append(gpa, .{});
    return index;
}
/// Add an exact dependency from a parent equation to a child.
pub fn edge(self: *Self, gpa: Allocator, parent: usize, child: usize, transform: Transform) Allocator.Error!void {
    const index = self.edges.items.len;
    try self.edges.append(gpa, .{ .parent = parent, .next = self.nodes.items[child].first, .transform = transform });
    self.nodes.items[child].first = index;
    self.nodes.items[parent].remaining += 1;
    if (transform == .backing) self.nodes.items[parent].value = @max(1, self.nodes.items[parent].value);
}
/// Read an outcome after solving has converged.
pub fn value(self: *const Self, index: usize) u2 {
    return self.nodes.items[index].value;
}
/// Set the local constant before propagation begins.
pub fn set(self: *Self, index: usize, result: u2) void {
    self.nodes.items[index].value = result;
}
/// Make a Boolean equation a conjunction instead of a maximum.
pub fn all(self: *Self, index: usize) void {
    self.nodes.items[index].all = true;
}
/// Converge all equations by propagating monotone promotions.
pub fn solve(self: *Self, gpa: Allocator) Allocator.Error!void {
    // At most two promotions per node. Reserve before propagation so an OOM
    // cannot leave a partially converged answer looking completed.
    try self.work.ensureTotalCapacity(gpa, self.nodes.items.len * 2);
    for (self.nodes.items, 0..) |*node, index| {
        if (node.all and node.remaining == 0) node.value = 1;
        if (node.value != 0) self.work.appendAssumeCapacity(index);
    }
    var cursor: usize = 0;
    while (cursor < self.work.items.len) : (cursor += 1) {
        const child = self.nodes.items[self.work.items[cursor]];
        var link = child.first;
        while (link != none) {
            const item = self.edges.items[link];
            link = item.next;
            const parent = &self.nodes.items[item.parent];
            const result: u2 = switch (item.transform) {
                .identity => child.value,
                .unchanged => if (child.value == 0) 0 else 2,
                .backing => if (child.value == 2) 2 else 1,
            };
            if (parent.all) {
                std.debug.assert(child.value == 1 and item.transform == .identity);
                parent.remaining -= 1;
                if (parent.remaining != 0) continue;
            }
            if (result > parent.value) {
                parent.value = result;
                self.work.appendAssumeCapacity(item.parent);
            }
        }
    }
}

test "issue 12616 fixed points agree with independent exhaustive iteration" {
    const gpa = std.testing.allocator;
    // Exhaust every three-node Boolean graph and AND/OR choice. The oracle
    // scans whole equations synchronously; the implementation uses counters
    // and reverse edges. Neither oracle state nor provisional values are shared.
    for (0..512) |links| {
        for (0..8) |kinds| {
            var equations: Self = .{};
            defer equations.deinit(gpa);
            for (0..3) |_| _ = try equations.add(gpa);
            for (0..3) |parent| {
                if (kinds & (@as(usize, 1) << @intCast(parent)) != 0) equations.all(parent);
                for (0..3) |child| {
                    if (links & (@as(usize, 1) << @intCast(parent * 3 + child)) != 0) try equations.edge(gpa, parent, child, .identity);
                }
            }
            var oracle = [_]bool{ false, false, false };
            while (true) {
                var next = oracle;
                for (0..3) |parent| {
                    const conjunction = kinds & (@as(usize, 1) << @intCast(parent)) != 0;
                    next[parent] = conjunction;
                    for (0..3) |child| {
                        if (links & (@as(usize, 1) << @intCast(parent * 3 + child)) == 0) continue;
                        next[parent] = if (conjunction) next[parent] and oracle[child] else next[parent] or oracle[child];
                    }
                }
                if (std.mem.eql(bool, &next, &oracle)) break;
                oracle = next;
            }
            try equations.solve(gpa);
            for (oracle, 0..) |expected, index| try std.testing.expectEqual(@intFromBool(expected), equations.value(index));
        }
    }
    // A back edge is followed by a completing or mismatching leaf. Completion
    // and row-tail transformations must propagate through the whole cycle.
    for (0..3) |leaf| {
        inline for (std.meta.tags(Transform)) |transform| {
            var equations: Self = .{};
            defer equations.deinit(gpa);
            for (0..3) |_| _ = try equations.add(gpa);
            try equations.edge(gpa, 0, 1, transform);
            try equations.edge(gpa, 1, 0, .identity);
            try equations.edge(gpa, 1, 2, .identity);
            equations.set(2, @intCast(leaf));
            var oracle = [_]u2{ 0, 0, @intCast(leaf) };
            while (true) {
                const next = [_]u2{
                    switch (transform) {
                        .identity => oracle[1],
                        .unchanged => if (oracle[1] == 0) 0 else 2,
                        .backing => if (oracle[1] == 2) 2 else 1,
                    },
                    @max(oracle[0], oracle[2]),
                    oracle[2],
                };
                if (std.mem.eql(u2, &next, &oracle)) break;
                oracle = next;
            }
            try equations.solve(gpa);
            for (oracle, 0..) |expected, index| try std.testing.expectEqual(expected, equations.value(index));
        }
    }
}
