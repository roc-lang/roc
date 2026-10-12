//! The loop-nesting forest of a directed graph.
//!
//! A loop is a header together with every depth-first descendant of the
//! header that reaches one of the header's back edges without passing through
//! the header; loops nest, and each one is strongly connected. A loop of a
//! reducible graph is entered only at its header; a loop of an irreducible
//! region can also be entered at other nodes, and is marked so. The forest is
//! built in near-linear time (Tarjan, with Havlak's treatment of irreducible
//! entries): headers are visited in reverse depth-first preorder, and each
//! loop collects its body through a union-find in which every loop already
//! built stands for its whole body. Graphs are given in compressed adjacency
//! form: `succ_starts[n]..succ_starts[n + 1]` indexes node n's successors in
//! `succs`, and likewise for predecessors.

const std = @import("std");
const Allocator = std.mem.Allocator;

const LoopForest = @This();

/// No loop, or no node.
pub const none = std.math.maxInt(u32);

/// One loop of the forest.
pub const Loop = struct {
    header: u32,
    parent: u32 = none,
    depth: u32 = 0,
    /// Preorder interval of the loop in the loop tree: a loop contains
    /// another exactly when its interval contains the other's `enter`.
    enter: u32 = 0,
    exit: u32 = 0,
    /// Whether some edge enters the loop at a node other than its header.
    irreducible: bool = false,
};

allocator: Allocator,
loops: []Loop,
/// Each node's innermost loop, or `none`.
innermost: []u32,
/// Loop indices in loop-tree preorder (parents before children).
preorder: []u32,
/// `lift[level * loops.len + loop]` is the loop's ancestor 2^level levels up,
/// or `none`.
lift: []u32,
lift_levels: usize,

/// Building a forest only allocates.
pub const Error = Allocator.Error;

/// The forest of the graph with the given compressed adjacency.
pub fn build(
    allocator: Allocator,
    succ_starts: []const u32,
    succs: []const u32,
    pred_starts: []const u32,
    preds: []const u32,
) Error!LoopForest {
    const node_count = succ_starts.len - 1;
    var scratch_arena = std.heap.ArenaAllocator.init(allocator);
    defer scratch_arena.deinit();
    const scratch = scratch_arena.allocator();

    // Depth-first preorder; `last[n]` is the largest preorder number in n's
    // subtree, so v is an ancestor-or-self of u exactly when
    // pre[v] <= pre[u] <= last[v].
    const pre = try scratch.alloc(u32, node_count);
    @memset(pre, none);
    const last = try scratch.alloc(u32, node_count);
    const by_pre = try scratch.alloc(u32, node_count);
    {
        const Frame = struct { node: u32, next: u32 };
        var frames = std.ArrayList(Frame).empty;
        var counter: u32 = 0;
        for (0..node_count) |root| {
            if (pre[root] != none) continue;
            pre[root] = counter;
            by_pre[counter] = @intCast(root);
            counter += 1;
            try frames.append(scratch, .{ .node = @intCast(root), .next = succ_starts[root] });
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                if (frame.next < succ_starts[frame.node + 1]) {
                    const successor = succs[frame.next];
                    frame.next += 1;
                    if (pre[successor] == none) {
                        pre[successor] = counter;
                        by_pre[counter] = successor;
                        counter += 1;
                        try frames.append(scratch, .{ .node = successor, .next = succ_starts[successor] });
                    }
                    continue;
                }
                last[frame.node] = counter - 1;
                _ = frames.pop();
            }
        }
    }

    var loops = std.ArrayList(Loop).empty;
    errdefer loops.deinit(allocator);
    const innermost = try allocator.alloc(u32, node_count);
    errdefer allocator.free(innermost);
    @memset(innermost, none);
    const loop_of_header = try scratch.alloc(u32, node_count);
    @memset(loop_of_header, none);
    const union_parent = try scratch.alloc(u32, node_count);
    for (union_parent, 0..) |*parent, index| parent.* = @intCast(index);
    const in_body = try scratch.alloc(u32, node_count);
    @memset(in_body, none);
    var body = std.ArrayList(u32).empty;
    var work = std.ArrayList(u32).empty;

    var order_index = node_count;
    while (order_index > 0) {
        order_index -= 1;
        const header = by_pre[order_index];
        const loop_index: u32 = @intCast(loops.items.len);
        body.clearRetainingCapacity();
        work.clearRetainingCapacity();
        var has_back_edge = false;
        for (preds[pred_starts[header]..pred_starts[header + 1]]) |predecessor| {
            if (!isAncestorOrSelf(pre, last, header, predecessor)) continue;
            has_back_edge = true;
            const representative = find(union_parent, predecessor);
            if (representative == header or in_body[representative] == loop_index) continue;
            in_body[representative] = loop_index;
            try body.append(scratch, representative);
            try work.append(scratch, representative);
        }
        if (!has_back_edge) continue;
        try loops.append(allocator, .{ .header = header });
        loop_of_header[header] = loop_index;
        innermost[header] = loop_index;
        while (work.pop()) |member| {
            for (preds[pred_starts[member]..pred_starts[member + 1]]) |predecessor| {
                const representative = find(union_parent, predecessor);
                if (representative == header or representative == member or in_body[representative] == loop_index) continue;
                if (!isAncestorOrSelf(pre, last, header, representative)) {
                    // Not reachable from the header: an entry of the loop
                    // that bypasses its header.
                    loops.items[loop_index].irreducible = true;
                    continue;
                }
                in_body[representative] = loop_index;
                try body.append(scratch, representative);
                try work.append(scratch, representative);
            }
        }
        for (body.items) |member| {
            union_parent[member] = header;
            if (loop_of_header[member] != none) {
                loops.items[loop_of_header[member]].parent = loop_index;
            } else {
                innermost[member] = loop_index;
            }
        }
    }

    const loop_count = loops.items.len;
    const preorder = try allocator.alloc(u32, loop_count);
    errdefer allocator.free(preorder);
    {
        const child_starts = try scratch.alloc(u32, loop_count + 1);
        @memset(child_starts, 0);
        for (loops.items) |loop| if (loop.parent != none) {
            child_starts[loop.parent + 1] += 1;
        };
        for (1..loop_count + 1) |index| child_starts[index] += child_starts[index - 1];
        const children = try scratch.alloc(u32, child_starts[loop_count]);
        const fill = try scratch.dupe(u32, child_starts[0..loop_count]);
        for (loops.items, 0..) |loop, index| if (loop.parent != none) {
            children[fill[loop.parent]] = @intCast(index);
            fill[loop.parent] += 1;
        };
        const Frame = struct { loop: u32, next: u32 };
        var frames = std.ArrayList(Frame).empty;
        var counter: u32 = 0;
        for (0..loop_count) |root| {
            if (loops.items[root].parent != none) continue;
            loops.items[root].enter = counter;
            preorder[counter] = @intCast(root);
            counter += 1;
            try frames.append(scratch, .{ .loop = @intCast(root), .next = child_starts[root] });
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                if (frame.next < child_starts[frame.loop + 1]) {
                    const child = children[frame.next];
                    frame.next += 1;
                    loops.items[child].enter = counter;
                    loops.items[child].depth = loops.items[frame.loop].depth + 1;
                    preorder[counter] = child;
                    counter += 1;
                    try frames.append(scratch, .{ .loop = child, .next = child_starts[child] });
                    continue;
                }
                loops.items[frame.loop].exit = counter - 1;
                _ = frames.pop();
            }
        }
    }

    var lift_levels: usize = 1;
    while ((@as(usize, 1) << @intCast(lift_levels)) <= loop_count) lift_levels += 1;
    const lift = try allocator.alloc(u32, lift_levels * loop_count);
    errdefer allocator.free(lift);
    for (loops.items, 0..) |loop, index| lift[index] = loop.parent;
    for (1..lift_levels) |level| {
        for (0..loop_count) |index| {
            const mid = lift[(level - 1) * loop_count + index];
            lift[level * loop_count + index] = if (mid == none) none else lift[(level - 1) * loop_count + mid];
        }
    }

    return .{
        .allocator = allocator,
        .loops = try loops.toOwnedSlice(allocator),
        .innermost = innermost,
        .preorder = preorder,
        .lift = lift,
        .lift_levels = lift_levels,
    };
}

/// Free the forest.
pub fn deinit(self: *LoopForest) void {
    self.allocator.free(self.lift);
    self.allocator.free(self.preorder);
    self.allocator.free(self.innermost);
    self.allocator.free(self.loops);
}

/// Whether loop `outer` contains loop `inner` (or is it).
pub fn contains(self: *const LoopForest, outer: u32, inner: u32) bool {
    return self.loops[outer].enter <= self.loops[inner].enter and self.loops[inner].enter <= self.loops[outer].exit;
}

/// Whether loop `loop` contains node `node`.
pub fn containsNode(self: *const LoopForest, loop: u32, node: u32) bool {
    const inner = self.innermost[node];
    return inner != none and self.contains(loop, inner);
}

/// The outermost ancestor-or-self of `loop` satisfying `keep`, given that
/// `keep` holds of `loop` and, once false for a loop, is false for every
/// loop around it.
pub fn outermostWhile(self: *const LoopForest, loop: u32, context: anytype, comptime keep: fn (@TypeOf(context), u32) bool) u32 {
    var current = loop;
    var level = self.lift_levels;
    while (level > 0) {
        level -= 1;
        const ancestor = self.lift[level * self.loops.len + current];
        if (ancestor == none or !keep(context, ancestor)) continue;
        current = ancestor;
    }
    return current;
}

fn isAncestorOrSelf(pre: []const u32, last: []const u32, ancestor: u32, node: u32) bool {
    return pre[ancestor] <= pre[node] and pre[node] <= last[ancestor];
}

fn find(parents: []u32, start: u32) u32 {
    var root = start;
    while (parents[root] != root) root = parents[root];
    var cursor = start;
    while (parents[cursor] != root) {
        const next = parents[cursor];
        parents[cursor] = root;
        cursor = next;
    }
    return root;
}

fn testForest(edges: []const [2]u32, node_count: usize) Error!LoopForest {
    const allocator = std.testing.allocator;
    const succ_starts = try allocator.alloc(u32, node_count + 1);
    defer allocator.free(succ_starts);
    const pred_starts = try allocator.alloc(u32, node_count + 1);
    defer allocator.free(pred_starts);
    @memset(succ_starts, 0);
    @memset(pred_starts, 0);
    for (edges) |edge| {
        succ_starts[edge[0] + 1] += 1;
        pred_starts[edge[1] + 1] += 1;
    }
    for (1..node_count + 1) |index| {
        succ_starts[index] += succ_starts[index - 1];
        pred_starts[index] += pred_starts[index - 1];
    }
    const succs = try allocator.alloc(u32, edges.len);
    defer allocator.free(succs);
    const preds = try allocator.alloc(u32, edges.len);
    defer allocator.free(preds);
    const succ_fill = try allocator.dupe(u32, succ_starts[0..node_count]);
    defer allocator.free(succ_fill);
    const pred_fill = try allocator.dupe(u32, pred_starts[0..node_count]);
    defer allocator.free(pred_fill);
    for (edges) |edge| {
        succs[succ_fill[edge[0]]] = edge[1];
        succ_fill[edge[0]] += 1;
        preds[pred_fill[edge[1]]] = edge[0];
        pred_fill[edge[1]] += 1;
    }
    return try build(allocator, succ_starts, succs, pred_starts, preds);
}

test "nested loops nest in the forest" {
    // 0 -> 1 (outer header) -> 2 (inner header) -> 3 -> 2, 3 -> 4 -> 1, 1 -> 5
    var forest = try testForest(&.{ .{ 0, 1 }, .{ 1, 2 }, .{ 2, 3 }, .{ 3, 2 }, .{ 3, 4 }, .{ 4, 1 }, .{ 1, 5 } }, 6);
    defer forest.deinit();
    try std.testing.expectEqual(@as(usize, 2), forest.loops.len);
    const outer = forest.innermost[1];
    const inner = forest.innermost[2];
    try std.testing.expect(outer != none and inner != none and outer != inner);
    try std.testing.expectEqual(outer, forest.loops[inner].parent);
    try std.testing.expectEqual(inner, forest.innermost[3]);
    try std.testing.expectEqual(outer, forest.innermost[4]);
    try std.testing.expectEqual(none, forest.innermost[0]);
    try std.testing.expectEqual(none, forest.innermost[5]);
    try std.testing.expect(forest.contains(outer, inner));
    try std.testing.expect(!forest.contains(inner, outer));
    try std.testing.expect(forest.containsNode(outer, 3));
}

test "a cycle entered at two nodes is one irreducible loop" {
    // 0 -> 1, 0 -> 2, 1 -> 2, 2 -> 1
    var forest = try testForest(&.{ .{ 0, 1 }, .{ 0, 2 }, .{ 1, 2 }, .{ 2, 1 } }, 3);
    defer forest.deinit();
    try std.testing.expectEqual(@as(usize, 1), forest.loops.len);
    try std.testing.expect(forest.loops[0].irreducible);
    try std.testing.expectEqual(forest.innermost[1], forest.innermost[2]);
    try std.testing.expectEqual(none, forest.innermost[0]);
}
