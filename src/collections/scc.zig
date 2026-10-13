//! Strongly connected components of a directed graph in compressed adjacency
//! form (`succ_starts[n]..succ_starts[n + 1]` indexes node n's successors in
//! `succs`, and likewise for predecessors), numbered in topological order:
//! no edge leads into a lower-numbered component, so a node in a
//! lower-numbered component than another is never reachable from it.
//! Kosaraju's two passes run on explicit work stacks.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Strongly connected components of a graph, numbered in topological order.
pub const Components = struct {
    component_of: []u32,
    /// `nodes[starts[c]..starts[c + 1]]` lists component `c`'s nodes.
    nodes: []u32,
    starts: []u32,

    pub fn count(self: *const Components) usize {
        return self.starts.len - 1;
    }

    pub fn membersOf(self: *const Components, component: usize) []u32 {
        return self.nodes[self.starts[component]..self.starts[component + 1]];
    }
};

/// Compute the strongly connected components of the graph whose successors are given in compressed sparse row form.
pub fn compute(
    allocator: Allocator,
    succ_starts: []const u32,
    succs: []const u32,
    pred_starts: []const u32,
    preds: []const u32,
) Allocator.Error!Components {
    const node_count = succ_starts.len - 1;
    const no_component = std.math.maxInt(u32);
    const component_of = try allocator.alloc(u32, node_count);
    @memset(component_of, no_component);
    var component_nodes = std.ArrayList(u32).empty;
    var component_starts = std.ArrayList(u32).empty;
    const Frame = struct { node: u32, next_successor: u32 };
    var seen = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, node_count);
    var frames = std.ArrayList(Frame).empty;
    var finish_order = std.ArrayList(u32).empty;
    for (0..node_count) |root| {
        if (seen.isSet(root)) continue;
        seen.set(root);
        try frames.append(allocator, .{ .node = @intCast(root), .next_successor = succ_starts[root] });
        while (frames.items.len != 0) {
            const frame = &frames.items[frames.items.len - 1];
            if (frame.next_successor < succ_starts[frame.node + 1]) {
                const successor = succs[frame.next_successor];
                frame.next_successor += 1;
                if (!seen.isSet(successor)) {
                    seen.set(successor);
                    try frames.append(allocator, .{ .node = successor, .next_successor = succ_starts[successor] });
                }
                continue;
            }
            try finish_order.append(allocator, frame.node);
            _ = frames.pop();
        }
    }
    var reverse_work = std.ArrayList(u32).empty;
    var order_index = finish_order.items.len;
    while (order_index > 0) {
        order_index -= 1;
        const root = finish_order.items[order_index];
        if (component_of[root] != no_component) continue;
        const component: u32 = @intCast(component_starts.items.len);
        try component_starts.append(allocator, @intCast(component_nodes.items.len));
        component_of[root] = component;
        try reverse_work.append(allocator, root);
        while (reverse_work.pop()) |member| {
            try component_nodes.append(allocator, member);
            for (preds[pred_starts[member]..pred_starts[member + 1]]) |predecessor| {
                if (component_of[predecessor] != no_component) continue;
                component_of[predecessor] = component;
                try reverse_work.append(allocator, predecessor);
            }
        }
    }
    try component_starts.append(allocator, @intCast(component_nodes.items.len));
    return .{ .component_of = component_of, .nodes = component_nodes.items, .starts = component_starts.items };
}

test "components never number a successor's component lower" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    // 0 -> 1 -> 2 -> 1 (a loop of 1 and 2), 2 -> 3, 0 -> 4 -> 3.
    const succ_starts = [_]u32{ 0, 2, 3, 5, 5, 6 };
    const succs = [_]u32{ 1, 4, 2, 1, 3, 3 };
    const pred_starts = [_]u32{ 0, 0, 2, 3, 5, 6 };
    const preds = [_]u32{ 0, 2, 1, 2, 4, 0 };
    const components = try compute(arena.allocator(), &succ_starts, &succs, &pred_starts, &preds);
    try std.testing.expectEqual(@as(usize, 4), components.count());
    try std.testing.expectEqual(components.component_of[1], components.component_of[2]);
    for (0..succ_starts.len - 1) |node| {
        for (succs[succ_starts[node]..succ_starts[node + 1]]) |succ| {
            try std.testing.expect(components.component_of[succ] >= components.component_of[node]);
        }
        const members = components.membersOf(components.component_of[node]);
        try std.testing.expect(std.mem.findScalar(u32, members, @intCast(node)) != null);
    }
    try std.testing.expect(components.component_of[0] < components.component_of[1]);
    try std.testing.expect(components.component_of[1] < components.component_of[3]);
    try std.testing.expect(components.component_of[4] < components.component_of[3]);
}
