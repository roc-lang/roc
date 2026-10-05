//! Least-fixpoint read-before-rebind liveness over a compressed control graph,
//! solved by loop structure into persistent rows.
//!
//! A node's row holds every bit read on some path from the node before a node
//! on that path defines it: the least solution of
//! `row(n) = reads(n) | (union of row(s) over successors s, minus defs(n))`.
//! A loop nest keeps an outer value live through every inner node, so rows
//! hold quadratic content in the nest depth. They are therefore never built
//! per node: inside a cyclic component, a bit that a whole loop carries (the
//! loop defines it nowhere, and it is exposed somewhere in the loop) is set
//! once on that loop's carried row, every row of the loop shares that row's
//! structure, and only the bits exposed at a node itself are added to the
//! node's row. Acyclic nodes are one equation step from their successors'
//! finished rows and share their structure.

const std = @import("std");
const collections = @import("collections");
const ArcSnapshot = @import("arc_state.zig").Snapshot;

const Allocator = std.mem.Allocator;
const LoopForest = collections.LoopForest;

/// A persistent bit row. Copies share structure; changing a bit allocates
/// only that bit's path.
pub const Row = struct {
    words: ArcSnapshot(u64, 0),

    pub fn initEmpty(allocator: Allocator, bit_len: usize) Row {
        return .{ .words = ArcSnapshot(u64, 0).init(allocator, (bit_len + 63) / 64) };
    }

    pub fn set(self: *Row, bit: usize) Allocator.Error!void {
        const word: u32 = @intCast(bit / 64);
        const mask = @as(u64, 1) << @intCast(bit % 64);
        try self.words.put(word, self.words.get(word) | mask);
    }

    pub fn unset(self: *Row, bit: usize) Allocator.Error!void {
        const word: u32 = @intCast(bit / 64);
        const mask = @as(u64, 1) << @intCast(bit % 64);
        try self.words.put(word, self.words.get(word) & ~mask);
    }

    pub fn isSet(self: *const Row, bit: usize) bool {
        const word: u32 = @intCast(bit / 64);
        return self.words.get(word) & (@as(u64, 1) << @intCast(bit % 64)) != 0;
    }

    pub fn setUnion(self: *Row, other: *const Row) Allocator.Error!void {
        _ = try self.words.joinWith(&other.words, {}, unionWord);
    }

    pub fn eql(self: *const Row, other: *const Row) bool {
        return self.words.eql(&other.words);
    }

    /// Set bits in ascending order, skipping absent subtrees.
    pub fn iterator(self: *const Row) Iterator {
        return .{ .words = self.words.iterator() };
    }

    pub const Iterator = struct {
        words: ArcSnapshot(u64, 0).Iterator,
        base: usize = 0,
        pending: u64 = 0,

        pub fn next(self: *Iterator) ?usize {
            while (self.pending == 0) {
                const entry = self.words.next() orelse return null;
                self.base = @as(usize, entry.index) * 64;
                self.pending = entry.value;
            }
            const bit = self.base + @ctz(self.pending);
            self.pending &= self.pending - 1;
            return bit;
        }
    };

    fn unionWord(_: void, lhs: u64, rhs: u64) u64 {
        return lhs | rhs;
    }
};

/// A control graph in compressed adjacency form, with the bits each node
/// reads and defines. `read_starts[n]..read_starts[n + 1]` indexes node n's
/// read bits in `reads`, and likewise for successors, predecessors, and
/// defined bits.
pub const Graph = struct {
    succ_starts: []const u32,
    succs: []const u32,
    pred_starts: []const u32,
    preds: []const u32,
    read_starts: []const u32,
    reads: []const u32,
    def_starts: []const u32,
    defs: []const u32,
    bit_len: usize,

    fn nodeCount(self: *const Graph) usize {
        return self.succ_starts.len - 1;
    }

    fn successorsOf(self: *const Graph, node: u32) []const u32 {
        return self.succs[self.succ_starts[node]..self.succ_starts[node + 1]];
    }

    fn predecessorsOf(self: *const Graph, node: u32) []const u32 {
        return self.preds[self.pred_starts[node]..self.pred_starts[node + 1]];
    }

    fn readsOf(self: *const Graph, node: u32) []const u32 {
        return self.reads[self.read_starts[node]..self.read_starts[node + 1]];
    }

    fn defsOf(self: *const Graph, node: u32) []const u32 {
        return self.defs[self.def_starts[node]..self.def_starts[node + 1]];
    }

    fn defines(self: *const Graph, node: u32, bit: u32) bool {
        for (self.defsOf(node)) |defined| {
            if (defined == bit) return true;
        }
        return false;
    }
};

/// Solves every node's row. Rows and their shared subtrees live in
/// `allocator`; all other work uses `scratch_allocator` and is freed.
pub fn solve(allocator: Allocator, scratch_allocator: Allocator, graph: *const Graph) Allocator.Error![]Row {
    var scratch_arena = std.heap.ArenaAllocator.init(scratch_allocator);
    defer scratch_arena.deinit();
    const scratch = scratch_arena.allocator();
    const node_count = graph.nodeCount();
    const no_loop = LoopForest.none;

    const rows = try allocator.alloc(Row, node_count);
    for (rows) |*row| row.* = Row.initEmpty(allocator, graph.bit_len);

    var forest = try LoopForest.build(scratch, graph.succ_starts, graph.succs, graph.pred_starts, graph.preds);
    const loop_count = forest.loops.len;

    // Edges entering a loop other than at its header, by loop.
    const LoopEntry = struct { loop: u32, predecessor: u32, target: u32 };
    var side_entries = std.ArrayList(LoopEntry).empty;
    for (0..node_count) |target_index| {
        const target: u32 = @intCast(target_index);
        for (graph.predecessorsOf(target)) |predecessor| {
            var loop = forest.innermost[target];
            while (loop != no_loop and !forest.containsNode(loop, predecessor)) : (loop = forest.loops[loop].parent) {
                if (forest.loops[loop].header != target) {
                    try side_entries.append(scratch, .{ .loop = loop, .predecessor = predecessor, .target = target });
                }
            }
        }
    }
    std.mem.sort(LoopEntry, side_entries.items, {}, struct {
        fn lessThan(_: void, lhs: LoopEntry, rhs: LoopEntry) bool {
            return lhs.loop < rhs.loop;
        }
    }.lessThan);
    const side_entry_starts = try scratch.alloc(u32, loop_count + 1);
    {
        var cursor: usize = 0;
        for (0..loop_count) |loop| {
            side_entry_starts[loop] = @intCast(cursor);
            while (cursor < side_entries.items.len and side_entries.items[cursor].loop == loop) cursor += 1;
        }
        side_entry_starts[loop_count] = @intCast(cursor);
    }

    // Bits each loop defines: a node's definitions belong to its innermost
    // loop and to every loop around it.
    const written = try scratch.alloc(Row, loop_count);
    for (written) |*bits| bits.* = Row.initEmpty(scratch, graph.bit_len);
    for (0..node_count) |node_index| {
        const loop = forest.innermost[node_index];
        if (loop == no_loop) continue;
        for (graph.defsOf(@intCast(node_index))) |bit| try written[loop].set(bit);
    }
    {
        var preorder_index = loop_count;
        while (preorder_index > 0) {
            preorder_index -= 1;
            const loop = forest.preorder[preorder_index];
            const parent = forest.loops[loop].parent;
            if (parent != no_loop) try written[parent].setUnion(&written[loop]);
        }
    }

    // Components in an order that solves every successor component first.
    const no_component = std.math.maxInt(u32);
    const component_of = try scratch.alloc(u32, node_count);
    @memset(component_of, no_component);
    var component_nodes = std.ArrayList(u32).empty;
    var component_starts = std.ArrayList(u32).empty;
    {
        const Frame = struct { node: u32, next_successor: u32 };
        var seen = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(scratch, node_count);
        var frames = std.ArrayList(Frame).empty;
        var finish_order = std.ArrayList(u32).empty;
        for (0..node_count) |root| {
            if (seen.isSet(root)) continue;
            seen.set(root);
            try frames.append(scratch, .{ .node = @intCast(root), .next_successor = 0 });
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const successors = graph.successorsOf(frame.node);
                if (frame.next_successor < successors.len) {
                    const successor = successors[frame.next_successor];
                    frame.next_successor += 1;
                    if (!seen.isSet(successor)) {
                        seen.set(successor);
                        try frames.append(scratch, .{ .node = successor, .next_successor = 0 });
                    }
                    continue;
                }
                try finish_order.append(scratch, frame.node);
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
            try component_starts.append(scratch, @intCast(component_nodes.items.len));
            component_of[root] = component;
            try reverse_work.append(scratch, root);
            while (reverse_work.pop()) |member| {
                try component_nodes.append(scratch, member);
                for (graph.predecessorsOf(member)) |predecessor| {
                    if (component_of[predecessor] != no_component) continue;
                    component_of[predecessor] = component;
                    try reverse_work.append(scratch, predecessor);
                }
            }
        }
        try component_starts.append(scratch, @intCast(component_nodes.items.len));
    }

    // Inside a cyclic component, one backward search per bit. A unit is a
    // node, or the largest loop around a node that defines nothing of the
    // bit, whose whole body shares one answer.
    const Unit = struct { loop: bool, index: u32 };
    const BitAt = struct { index: u32, bit: u32 };
    const Search = struct {
        graph_: *const Graph,
        forest_: *const LoopForest,
        written_: []const Row,
        component_of_: []const u32,
        component: u32 = 0,
        node_stamp: []u32,
        loop_stamp: []u32,
        stamp: u32 = 0,
        bit: u32 = 0,
        work: std.ArrayList(Unit) = .empty,
        node_bits: std.ArrayList(BitAt) = .empty,
        loop_bits: std.ArrayList(BitAt) = .empty,
        allocator_: Allocator,

        fn transparent(search: *const @This(), loop: u32) bool {
            return !search.written_[loop].isSet(search.bit);
        }

        fn reach(search: *@This(), node: u32) Allocator.Error!void {
            if (search.component_of_[node] != search.component) return;
            const innermost = search.forest_.innermost[node];
            if (innermost != LoopForest.none and search.transparent(innermost)) {
                const loop = search.forest_.outermostWhile(innermost, search, transparent);
                if (search.loop_stamp[loop] == search.stamp) return;
                search.loop_stamp[loop] = search.stamp;
                try search.work.append(search.allocator_, .{ .loop = true, .index = loop });
            } else {
                if (search.node_stamp[node] == search.stamp) return;
                search.node_stamp[node] = search.stamp;
                try search.work.append(search.allocator_, .{ .loop = false, .index = node });
            }
        }

        fn reachPredecessorsOf(search: *@This(), target: u32, outside: ?u32) Allocator.Error!void {
            for (search.graph_.predecessorsOf(target)) |predecessor| {
                if (outside) |loop| if (search.forest_.containsNode(loop, predecessor)) continue;
                if (!search.graph_.defines(predecessor, search.bit)) try search.reach(predecessor);
            }
        }
    };
    var search = Search{
        .graph_ = graph,
        .forest_ = &forest,
        .written_ = written,
        .component_of_ = component_of,
        .node_stamp = try scratch.alloc(u32, node_count),
        .loop_stamp = try scratch.alloc(u32, loop_count),
        .allocator_ = scratch,
    };
    @memset(search.node_stamp, std.math.maxInt(u32));
    @memset(search.loop_stamp, std.math.maxInt(u32));
    const carried = try scratch.alloc(Row, loop_count);
    const byIndex = struct {
        fn lessThan(_: void, lhs: BitAt, rhs: BitAt) bool {
            return if (lhs.index == rhs.index) lhs.bit < rhs.bit else lhs.index < rhs.index;
        }
    }.lessThan;
    const byBit = struct {
        fn lessThan(_: void, lhs: BitAt, rhs: BitAt) bool {
            return if (lhs.bit == rhs.bit) lhs.index < rhs.index else lhs.bit < rhs.bit;
        }
    }.lessThan;
    var seeds = std.ArrayList(BitAt).empty;

    var component_cursor = component_starts.items.len - 1;
    while (component_cursor > 0) {
        component_cursor -= 1;
        const members = component_nodes.items[component_starts.items[component_cursor]..component_starts.items[component_cursor + 1]];
        const first = members[0];
        if (members.len == 1 and forest.innermost[first] == no_loop) {
            try solveAcyclicNode(graph, rows, first);
            continue;
        }
        const component: u32 = @intCast(component_cursor);
        search.component = component;
        search.node_bits.clearRetainingCapacity();
        search.loop_bits.clearRetainingCapacity();

        // Seeds: reads inside the component, and bits exposed just past an
        // edge leaving it.
        seeds.clearRetainingCapacity();
        for (members) |member| {
            for (graph.readsOf(member)) |bit| try seeds.append(scratch, .{ .index = member, .bit = bit });
            for (graph.successorsOf(member)) |successor| {
                if (component_of[successor] == component) continue;
                var exposed = rows[successor].iterator();
                while (exposed.next()) |bit| try seeds.append(scratch, .{ .index = member, .bit = @intCast(bit) });
            }
        }
        std.mem.sort(BitAt, seeds.items, {}, byBit);
        var seed_index: usize = 0;
        while (seed_index < seeds.items.len) : (search.stamp += 1) {
            search.bit = seeds.items[seed_index].bit;
            while (seed_index < seeds.items.len and seeds.items[seed_index].bit == search.bit) : (seed_index += 1) {
                const seed = seeds.items[seed_index].index;
                // An exit seed is exposed at its node only through the node's
                // definition; a read is exposed regardless.
                if (nodeReads(graph, seed, search.bit) or !graph.defines(seed, search.bit)) try search.reach(seed);
            }
            while (search.work.pop()) |unit| {
                if (unit.loop) {
                    try search.loop_bits.append(scratch, .{ .index = unit.index, .bit = search.bit });
                    try search.reachPredecessorsOf(forest.loops[unit.index].header, unit.index);
                    for (side_entries.items[side_entry_starts[unit.index]..side_entry_starts[unit.index + 1]]) |entry| {
                        if (component_of[entry.predecessor] != component) continue;
                        if (!graph.defines(entry.predecessor, search.bit)) try search.reach(entry.predecessor);
                    }
                } else {
                    try search.node_bits.append(scratch, .{ .index = unit.index, .bit = search.bit });
                    try search.reachPredecessorsOf(unit.index, null);
                }
            }
        }

        // Rows: the bits whole loops carry, shared down the component's loop
        // tree, plus the bits exposed at the node itself.
        std.mem.sort(BitAt, search.loop_bits.items, {}, byIndex);
        std.mem.sort(BitAt, search.node_bits.items, {}, byIndex);
        var top = forest.innermost[first];
        while (forest.loops[top].parent != no_loop) top = forest.loops[top].parent;
        for (forest.preorder[forest.loops[top].enter .. forest.loops[top].exit + 1]) |loop| {
            const parent = forest.loops[loop].parent;
            var row = if (parent != no_loop) carried[parent] else Row.initEmpty(allocator, graph.bit_len);
            var low: usize = 0;
            var high: usize = search.loop_bits.items.len;
            while (low < high) {
                const mid = low + (high - low) / 2;
                if (search.loop_bits.items[mid].index < loop) low = mid + 1 else high = mid;
            }
            while (low < search.loop_bits.items.len and search.loop_bits.items[low].index == loop) : (low += 1) {
                try row.set(search.loop_bits.items[low].bit);
            }
            carried[loop] = row;
        }
        std.mem.sort(u32, members, {}, std.sort.asc(u32));
        var bit_index: usize = 0;
        for (members) |member| {
            var row = carried[forest.innermost[member]];
            while (bit_index < search.node_bits.items.len and search.node_bits.items[bit_index].index < member) bit_index += 1;
            while (bit_index < search.node_bits.items.len and search.node_bits.items[bit_index].index == member) : (bit_index += 1) {
                try row.set(search.node_bits.items[bit_index].bit);
            }
            rows[member] = row;
        }
    }
    return rows;
}

fn nodeReads(graph: *const Graph, node: u32, bit: u32) bool {
    for (graph.readsOf(node)) |read| {
        if (read == bit) return true;
    }
    return false;
}

/// One exact equation step for a node outside every cycle, whose successors
/// are all solved.
fn solveAcyclicNode(graph: *const Graph, rows: []Row, node: u32) Allocator.Error!void {
    var row = rows[node];
    for (graph.successorsOf(node)) |successor| try row.setUnion(&rows[successor]);
    for (graph.defsOf(node)) |bit| try row.unset(bit);
    for (graph.readsOf(node)) |bit| try row.set(bit);
    rows[node] = row;
}

const TestGraph = struct {
    succ_starts: std.ArrayList(u32) = .empty,
    succs: std.ArrayList(u32) = .empty,
    pred_starts: std.ArrayList(u32) = .empty,
    preds: std.ArrayList(u32) = .empty,
    read_starts: std.ArrayList(u32) = .empty,
    reads: std.ArrayList(u32) = .empty,
    def_starts: std.ArrayList(u32) = .empty,
    defs: std.ArrayList(u32) = .empty,

    const Node = struct { succs: []const u32 = &.{}, reads: []const u32 = &.{}, defs: []const u32 = &.{} };

    fn build(allocator: Allocator, nodes: []const Node) Allocator.Error!TestGraph {
        var graph = TestGraph{};
        try graph.succ_starts.append(allocator, 0);
        try graph.read_starts.append(allocator, 0);
        try graph.def_starts.append(allocator, 0);
        for (nodes) |node| {
            try graph.succs.appendSlice(allocator, node.succs);
            try graph.succ_starts.append(allocator, @intCast(graph.succs.items.len));
            try graph.reads.appendSlice(allocator, node.reads);
            try graph.read_starts.append(allocator, @intCast(graph.reads.items.len));
            try graph.defs.appendSlice(allocator, node.defs);
            try graph.def_starts.append(allocator, @intCast(graph.defs.items.len));
        }
        try graph.pred_starts.append(allocator, 0);
        for (0..nodes.len) |target| {
            for (nodes, 0..) |node, source| {
                for (node.succs) |successor| {
                    if (successor == target) try graph.preds.append(allocator, @intCast(source));
                }
            }
            try graph.pred_starts.append(allocator, @intCast(graph.preds.items.len));
        }
        return graph;
    }

    fn view(self: *const TestGraph, bit_len: usize) Graph {
        return .{
            .succ_starts = self.succ_starts.items,
            .succs = self.succs.items,
            .pred_starts = self.pred_starts.items,
            .preds = self.preds.items,
            .read_starts = self.read_starts.items,
            .reads = self.reads.items,
            .def_starts = self.def_starts.items,
            .defs = self.defs.items,
            .bit_len = bit_len,
        };
    }
};

fn expectRowBits(row: *const Row, expected: []const usize) error{TestExpectedEqual}!void {
    var iterator = row.iterator();
    for (expected) |bit| try std.testing.expectEqual(@as(?usize, bit), iterator.next());
    try std.testing.expectEqual(@as(?usize, null), iterator.next());
}

test "liveness carries a value through a nested loop that never mentions it" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    // 0: def 0 -> 1
    // 1: outer header, reads 0 -> 2, 4
    // 2: inner header -> 3, 1
    // 3: inner body, def 1 -> 2
    // 4: exit
    const graph = try TestGraph.build(allocator, &.{
        .{ .succs = &.{1}, .defs = &.{0} },
        .{ .succs = &.{ 2, 4 }, .reads = &.{0} },
        .{ .succs = &.{ 3, 1 } },
        .{ .succs = &.{2}, .defs = &.{1} },
        .{},
    });
    const view = graph.view(2);
    const rows = try solve(allocator, std.testing.allocator, &view);
    try expectRowBits(&rows[0], &.{});
    try expectRowBits(&rows[1], &.{0});
    try expectRowBits(&rows[2], &.{0});
    try expectRowBits(&rows[3], &.{0});
    try expectRowBits(&rows[4], &.{});
}

test "liveness stops at definitions inside a loop" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    // 0 -> 1; 1: loop header -> 2, 3; 2: def 0 -> 1; 3: reads 0
    const graph = try TestGraph.build(allocator, &.{
        .{ .succs = &.{1} },
        .{ .succs = &.{ 2, 3 } },
        .{ .succs = &.{1}, .defs = &.{0} },
        .{ .reads = &.{0} },
    });
    const view = graph.view(1);
    const rows = try solve(allocator, std.testing.allocator, &view);
    try expectRowBits(&rows[0], &.{0});
    try expectRowBits(&rows[1], &.{0});
    try expectRowBits(&rows[2], &.{});
    try expectRowBits(&rows[3], &.{0});
}
