//! Exact, demand-driven reachability over an immutable checked graph.
//!
//! A query asks whether any reachable node satisfies the context's predicate.
//! Completed strongly connected components share one answer. An active back
//! edge is not a completed negative proof: a later edge of its component may
//! still reach a positive node. All graph and continuation storage is explicit.
const std = @import("std");
const Allocator = std.mem.Allocator;

/// `Context.visit(children, key)` returns a definite result, or null after
/// listing the successors which decide the result. The context and its graph
/// remain immutable for this analysis's lifetime. Key includes every mode
/// which can change a node's meaning.
pub fn AnyReachable(comptime Key: type, comptime Context: type) type {
    return struct {
        const Self = @This();
        const Node = struct {
            key: Key,
            low: usize,
            positive: bool = false,
            complete: bool = false,
        };
        const Frame = struct { node: usize, start: usize, next: usize };
        pub const Children = struct {
            allocator: Allocator,
            list: *std.ArrayList(Key),
            pub fn add(self: @This(), key: Key) Allocator.Error!void {
                try self.list.append(self.allocator, key);
            }
            pub fn addSlice(self: @This(), keys: []const Key) Allocator.Error!void {
                try self.list.appendSlice(self.allocator, keys);
            }
        };
        allocator: Allocator,
        context: *Context,
        index: std.AutoHashMapUnmanaged(Key, usize) = .empty,
        nodes: std.ArrayList(Node) = .empty,
        frames: std.ArrayList(Frame) = .empty,
        children: std.ArrayList(Key) = .empty,
        component: std.ArrayList(usize) = .empty,

        pub fn init(allocator: Allocator, context: *Context) Self {
            return .{ .allocator = allocator, .context = context };
        }
        pub fn deinit(self: *Self) void {
            self.index.deinit(self.allocator);
            self.nodes.deinit(self.allocator);
            self.frames.deinit(self.allocator);
            self.children.deinit(self.allocator);
            self.component.deinit(self.allocator);
        }

        pub fn visit(self: *Self, root: Key) Allocator.Error!bool {
            std.debug.assert(self.frames.items.len == 0 and self.component.items.len == 0);
            const first_new = self.nodes.items.len;
            errdefer {
                // A failed expansion publishes no partial proof. Earlier
                // queries' completed components remain usable after OOM.
                for (self.nodes.items[first_new..]) |node| _ = self.index.remove(node.key);
                self.nodes.shrinkRetainingCapacity(first_new);
                self.frames.clearRetainingCapacity();
                self.children.clearRetainingCapacity();
                self.component.clearRetainingCapacity();
            }
            const root_id = if (self.index.get(root)) |id| id else try self.begin(root);
            if (self.nodes.items[root_id].complete) return self.nodes.items[root_id].positive;
            while (self.frames.items.len != 0) {
                const frame_index = self.frames.items.len - 1;
                const frame = self.frames.items[frame_index];
                const parent = frame.node;
                if (!self.nodes.items[parent].positive and frame.next < self.children.items.len) {
                    const key = self.children.items[frame.next];
                    self.frames.items[frame_index].next += 1;
                    if (self.index.get(key)) |child| {
                        const found = self.nodes.items[child];
                        self.nodes.items[parent].positive = self.nodes.items[parent].positive or found.positive;
                        if (!found.complete) self.nodes.items[parent].low = @min(self.nodes.items[parent].low, child);
                    } else {
                        const child = try self.begin(key);
                        // A non-leaf resumes this parent when its frame
                        // finishes. A leaf has no continuation to suspend on.
                        if (self.nodes.items[child].complete) self.nodes.items[parent].positive = self.nodes.items[child].positive;
                    }
                    continue;
                }
                // Positive reachability needs only one witness; unexplored
                // successors cannot change it. Negative answers require every
                // successor and are committed only for a complete component.
                const finished = self.nodes.items[parent];
                if (finished.low == parent) {
                    while (true) {
                        const member = self.component.pop().?;
                        self.nodes.items[member].positive = finished.positive;
                        self.nodes.items[member].complete = true;
                        if (member == parent) break;
                    }
                }
                _ = self.frames.pop();
                self.children.shrinkRetainingCapacity(frame.start);
                if (self.frames.items.len != 0) {
                    const caller = self.frames.items[self.frames.items.len - 1].node;
                    self.nodes.items[caller].positive = self.nodes.items[caller].positive or finished.positive;
                    self.nodes.items[caller].low = @min(self.nodes.items[caller].low, finished.low);
                }
            }
            std.debug.assert(self.nodes.items[root_id].complete);
            return self.nodes.items[root_id].positive;
        }

        /// Expand one new node exactly once. No pointers into growable
        /// storage are retained while the context appends its successors.
        fn begin(self: *Self, key: Key) Allocator.Error!usize {
            const id = self.nodes.items.len;
            try self.nodes.append(self.allocator, .{ .key = key, .low = id });
            try self.index.put(self.allocator, key, id);
            const start = self.children.items.len;
            const result = try self.context.visit(Children{ .allocator = self.allocator, .list = &self.children }, key);
            if (result) |positive| {
                self.children.shrinkRetainingCapacity(start);
                self.nodes.items[id].positive = positive;
                self.nodes.items[id].complete = true;
            } else {
                try self.component.append(self.allocator, id);
                try self.frames.append(self.allocator, .{ .node = id, .start = start, .next = start });
            }
            return id;
        }
    };
}

const TestGraph = struct {
    edges: []const []const u8,
    positive: []const bool,
    visits: usize = 0,
    edges_listed: usize = 0,
    fn visit(self: *TestGraph, children: anytype, key: u8) Allocator.Error!?bool {
        self.visits += 1;
        if (self.positive[key]) return true;
        self.edges_listed += self.edges[key].len;
        try children.addSlice(self.edges[key]);
        return null;
    }
};

test "checked reachability commits recursive components together" {
    // 0 -> 1 -> 0, then 0 -> 2 (positive). Publishing 1 as negative when
    // it first returns would make a subsequent query of 1 incorrect.
    var graph = TestGraph{ .edges = &.{ &.{ 1, 2 }, &.{0}, &.{} }, .positive = &.{ false, false, true } };
    var walk = AnyReachable(u8, TestGraph).init(std.testing.allocator, &graph);
    defer walk.deinit();
    try std.testing.expect(try walk.visit(0));
    try std.testing.expect(try walk.visit(1));
    try std.testing.expect(try walk.visit(2));
    try std.testing.expectEqual(@as(usize, 3), graph.visits);
    try std.testing.expectEqual(@as(usize, 3), graph.edges_listed);
}

test "checked reachability negative cycles and shared diamonds expand once" {
    var graph = TestGraph{ .edges = &.{ &.{ 1, 2 }, &.{3}, &.{3}, &.{ 4, 4 }, &.{3} }, .positive = &.{ false, false, false, false, false } };
    var walk = AnyReachable(u8, TestGraph).init(std.testing.allocator, &graph);
    defer walk.deinit();
    for (0..graph.edges.len) |id| try std.testing.expect(!try walk.visit(@intCast(id)));
    try std.testing.expectEqual(graph.edges.len, graph.visits);
    try std.testing.expectEqual(@as(usize, 7), graph.edges_listed);
}

test "checked reachability positive witnesses need no unrelated expansion" {
    var graph = TestGraph{ .edges = &.{ &.{ 1, 2 }, &.{}, &.{} }, .positive = &.{ false, true, false } };
    var walk = AnyReachable(u8, TestGraph).init(std.testing.allocator, &graph);
    defer walk.deinit();
    try std.testing.expect(try walk.visit(0));
    try std.testing.expectEqual(@as(usize, 2), graph.visits);
    try std.testing.expect(!try walk.visit(2));
    try std.testing.expectEqual(@as(usize, 3), graph.visits);
}

fn allocationCase(allocator: Allocator) (Allocator.Error || error{TestUnexpectedResult})!void {
    var graph = TestGraph{ .edges = &.{ &.{ 1, 2 }, &.{0}, &.{} }, .positive = &.{ false, false, true } };
    var walk = AnyReachable(u8, TestGraph).init(allocator, &graph);
    defer walk.deinit();
    try std.testing.expect(try walk.visit(0));
    try std.testing.expect(try walk.visit(1));
}

test "checked reachability releases every allocation on failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, allocationCase, .{});
}

test "checked reachability agrees with exhaustive three-node reachability in every query order" {
    const orders = [_][3]u8{ .{ 0, 1, 2 }, .{ 0, 2, 1 }, .{ 1, 0, 2 }, .{ 1, 2, 0 }, .{ 2, 0, 1 }, .{ 2, 1, 0 } };
    for (0..512) |edge_bits| {
        var storage: [3][3]u8 = undefined;
        var edges: [3][]const u8 = undefined;
        for (0..3) |node| {
            var count: usize = 0;
            for (0..3) |child| {
                if (edge_bits & (@as(usize, 1) << @intCast(node * 3 + child)) == 0) continue;
                storage[node][count] = @intCast(child);
                count += 1;
            }
            edges[node] = storage[node][0..count];
        }
        for (0..8) |positive_bits| {
            var positive: [3]bool = undefined;
            for (&positive, 0..) |*value, id| value.* = positive_bits & (@as(usize, 1) << @intCast(id)) != 0;
            // Independent finite-set closure, without DFS or component proofs.
            var expected = positive;
            for (0..3) |_| for (0..3) |node| {
                for (edges[node]) |child| expected[node] = expected[node] or expected[child];
            };
            for (orders) |order| {
                var graph = TestGraph{ .edges = &edges, .positive = &positive };
                var walk = AnyReachable(u8, TestGraph).init(std.testing.allocator, &graph);
                defer walk.deinit();
                for (order) |root| try std.testing.expectEqual(expected[root], try walk.visit(root));
                try std.testing.expect(graph.visits <= 3);
            }
        }
    }
}

test "checked reachability shared DAG work is bounded by unique nodes" {
    var storage: [64][2]u8 = undefined;
    var edges: [64][]const u8 = undefined;
    for (0..63) |index| {
        storage[index] = @splat(@intCast(index + 1));
        edges[index] = &storage[index];
    }
    edges[63] = &.{};
    const positive: [64]bool = @splat(false);
    var graph = TestGraph{ .edges = &edges, .positive = &positive };
    var walk = AnyReachable(u8, TestGraph).init(std.testing.allocator, &graph);
    defer walk.deinit();
    for (0..64) |id| try std.testing.expect(!try walk.visit(@intCast(id)));
    try std.testing.expectEqual(@as(usize, 64), graph.visits);
    try std.testing.expectEqual(@as(usize, 126), graph.edges_listed);
}
