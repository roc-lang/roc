//! Producer-owned stored-constant eligibility of global callables.
//! A rejection propagates to every caller, including every member of a cycle.

const std = @import("std");
const base = @import("base");

const Allocator = std.mem.Allocator;

/// A dense callable ordinal in one module's publication.
pub const Id = enum(u32) { _ };

/// Exact callee edges, stored backwards so rejection visits each edge once.
pub const Graph = struct {
    const none = std.math.maxInt(u32);
    const Edge = struct { caller: Id, next: u32 };

    allocator: Allocator,
    heads: []u32,
    allowed: []bool,
    edges: std.ArrayList(Edge) = .empty,
    rejected: std.ArrayList(Id) = .empty,

    pub fn init(allocator: Allocator, count: usize) Allocator.Error!Graph {
        if (count > std.math.maxInt(u32)) return error.OutOfMemory;
        const heads = try allocator.alloc(u32, count);
        errdefer allocator.free(heads);
        const allowed = try allocator.alloc(bool, count);
        errdefer allocator.free(allowed);
        var rejected = try std.ArrayList(Id).initCapacity(allocator, count);
        errdefer rejected.deinit(allocator);
        @memset(heads, none);
        @memset(allowed, true);
        return .{ .allocator = allocator, .heads = heads, .allowed = allowed, .rejected = rejected };
    }

    pub fn deinit(self: *Graph) void {
        self.allocator.free(self.heads);
        self.allocator.free(self.allowed);
        self.edges.deinit(self.allocator);
        self.rejected.deinit(self.allocator);
    }

    pub fn addDependency(self: *Graph, caller: Id, callee: Id) Allocator.Error!void {
        const index = std.math.cast(u32, self.edges.items.len) orelse return error.OutOfMemory;
        if (index == none) return error.OutOfMemory;
        try self.edges.append(self.allocator, .{ .caller = caller, .next = self.heads[@backingInt(callee)] });
        self.heads[@backingInt(callee)] = index;
    }

    pub fn reject(self: *Graph, id: Id) void {
        if (!self.allowed[@backingInt(id)]) return;
        self.allowed[@backingInt(id)] = false;
        // One queue entry per vertex; init reserves the entire domain.
        self.rejected.appendAssumeCapacity(id);
    }

    /// Compute the greatest fixed point: recursion alone does not reject a
    /// callable, but any rejected reachable body rejects all its callers.
    pub fn finish(self: *Graph) void {
        var index: usize = 0;
        while (index < self.rejected.items.len) : (index += 1) {
            var edge_index = self.heads[@backingInt(self.rejected.items[index])];
            while (edge_index != none) {
                const edge = self.edges.items[edge_index];
                self.reject(edge.caller);
                edge_index = edge.next;
            }
        }
    }

    pub fn allowsStoredConst(self: *const Graph, id: Id) bool {
        if (self.rejected.items.len > self.allowed.len) base.invariant("a callable eligibility vertex is rejected at most once", .{});
        return self.allowed[@backingInt(id)];
    }
};

test "callable checking propagates observations through cycles and preserves independent recursion" {
    const gpa = std.testing.allocator;
    // 0 -> 1 -> 2 -> 0, 2 -> 3 (observation); 4 -> 4 is independent.
    // The edge order must not affect the solution, even when rejection was
    // recorded before callers were discovered.
    for ([_]bool{ false, true }) |reverse| {
        var graph = try Graph.init(gpa, 5);
        defer graph.deinit();
        graph.reject(@fromBackingInt(3));
        const edges = [_][2]u32{ .{ 0, 1 }, .{ 1, 2 }, .{ 2, 0 }, .{ 2, 3 }, .{ 4, 4 } };
        for (0..edges.len) |i| {
            const edge = edges[if (reverse) edges.len - 1 - i else i];
            try graph.addDependency(@fromBackingInt(edge[0]), @fromBackingInt(edge[1]));
        }
        graph.finish();
        for (0..4) |i| try std.testing.expect(!graph.allowsStoredConst(@fromBackingInt(@as(u32, @intCast(i)))));
        try std.testing.expect(graph.allowsStoredConst(@fromBackingInt(4)));
    }
}

test "callable checking preserves an unobserved recursive component" {
    var graph = try Graph.init(std.testing.allocator, 2);
    defer graph.deinit();
    try graph.addDependency(@fromBackingInt(0), @fromBackingInt(1));
    try graph.addDependency(@fromBackingInt(1), @fromBackingInt(0));
    graph.finish();
    try std.testing.expect(graph.allowsStoredConst(@fromBackingInt(0)));
    try std.testing.expect(graph.allowsStoredConst(@fromBackingInt(1)));
}
