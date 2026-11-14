//! Producer-owned stored-constant eligibility of callable definitions.
//! A rejection propagates to every caller, including every member of a cycle.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const Allocator = std.mem.Allocator;

/// A dense callable ordinal in one module's publication.
pub const Id = enum(u32) { _ };

/// Exact callee edges, stored backwards so rejection visits each edge once.
pub const Graph = struct {
    const EdgeIndex = enum(u32) { _ };
    const OptionalEdgeIndex = collections.OptionalId(EdgeIndex);
    const Edge = struct { caller: Id, next: OptionalEdgeIndex };

    allocator: Allocator,
    heads: collections.OptionalIndexColumn(EdgeIndex),
    allowed: []bool,
    edges: std.ArrayList(Edge) = .empty,
    rejected: std.ArrayList(Id) = .empty,

    /// A graph over `count` callables, all eligible until rejected.
    pub fn init(allocator: Allocator, count: usize) Allocator.Error!Graph {
        if (count > std.math.maxInt(u32)) return error.OutOfMemory;
        var heads = try collections.OptionalIndexColumn(EdgeIndex).initFilled(allocator, count);
        errdefer heads.deinit(allocator);
        const allowed = try allocator.alloc(bool, count);
        errdefer allocator.free(allowed);
        var rejected = try std.ArrayList(Id).initCapacity(allocator, count);
        errdefer rejected.deinit(allocator);
        @memset(allowed, true);
        return .{ .allocator = allocator, .heads = heads, .allowed = allowed, .rejected = rejected };
    }

    /// Free the graph's storage.
    pub fn deinit(self: *Graph) void {
        self.heads.deinit(self.allocator);
        self.allocator.free(self.allowed);
        self.edges.deinit(self.allocator);
        self.rejected.deinit(self.allocator);
    }

    /// Record that `caller`'s body reaches `callee`.
    pub fn addDependency(self: *Graph, caller: Id, callee: Id) Allocator.Error!void {
        const index = std.math.cast(u32, self.edges.items.len) orelse return error.OutOfMemory;
        if (index == std.math.maxInt(u32)) return error.OutOfMemory;
        const previous: OptionalEdgeIndex = if (self.heads.get(@backingInt(callee))) |head| .some(head) else .none;
        try self.edges.append(self.allocator, .{ .caller = caller, .next = previous });
        self.heads.set(@backingInt(callee), @fromBackingInt(index));
    }

    /// Mark a callable whose own body is ineligible.
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
            var edge_index = self.heads.get(@backingInt(self.rejected.items[index]));
            while (edge_index) |current| {
                const edge = self.edges.items[@backingInt(current)];
                self.reject(edge.caller);
                edge_index = edge.next.get();
            }
        }
    }

    /// The settled eligibility of `id`; valid after `finish`.
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
    // The graph's two definitions, which depend on each other.
    const first_def: u32 = 0;
    const second_def: u32 = 1;
    try graph.addDependency(@fromBackingInt(first_def), @fromBackingInt(second_def));
    try graph.addDependency(@fromBackingInt(second_def), @fromBackingInt(first_def));
    graph.finish();
    try std.testing.expect(graph.allowsStoredConst(@fromBackingInt(first_def)));
    try std.testing.expect(graph.allowsStoredConst(@fromBackingInt(second_def)));
}
