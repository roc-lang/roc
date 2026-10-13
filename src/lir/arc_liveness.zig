//! Least-fixpoint read-before-rebind liveness over a compressed control graph,
//! solved by loop structure into persistent rows.
//!
//! A node's row holds every bit read on some path from the node before a node
//! on that path defines it or an edge on that path kills it: the least
//! solution of `row(n) = reads(n) | (union over successors s of row(s) minus
//! kills(n -> s), minus defs(n))`.
//! A loop nest keeps an outer value live through every inner node, so rows
//! hold quadratic content in the nest depth. They are therefore never built
//! per node: inside a cyclic component, a bit that a whole loop carries (the
//! loop defines it nowhere, and it is exposed somewhere in the loop) is set
//! once on that loop's carried row, every row of the loop shares that row's
//! structure, and only the bits exposed at a node itself are added to the
//! node's row. Acyclic nodes are one equation step from their successors'
//! finished rows and share their structure.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");
const ArcSnapshot = @import("arc_state.zig").Snapshot;

const invariant = base.invariant;

const Allocator = std.mem.Allocator;
const LoopForest = collections.LoopForest;

/// A persistent bit row over an exact bit domain. Copies share structure;
/// changing a bit allocates only that bit's path.
pub const Row = struct {
    bit_len: usize,
    words: ArcSnapshot(u64, 0),

    pub fn initEmpty(allocator: Allocator, bit_len: usize) Row {
        const word_len = (bit_len + 63) / 64;
        std.debug.assert(word_len == 0 or word_len - 1 <= std.math.maxInt(u32));
        return .{ .bit_len = bit_len, .words = ArcSnapshot(u64, 0).init(allocator, word_len) };
    }

    /// The same bits, with further changes allocated in `allocator`.
    pub fn clone(self: *const Row, allocator: Allocator) Row {
        var cloned = self.*;
        cloned.words.allocator = allocator;
        return cloned;
    }

    pub fn set(self: *Row, bit: usize) Allocator.Error!void {
        std.debug.assert(bit < self.bit_len);
        const word: u32 = @intCast(bit / 64);
        const mask = @as(u64, 1) << @intCast(bit % 64);
        try self.words.put(word, self.words.get(word) | mask);
    }

    pub fn unset(self: *Row, bit: usize) Allocator.Error!void {
        std.debug.assert(bit < self.bit_len);
        const word: u32 = @intCast(bit / 64);
        const mask = @as(u64, 1) << @intCast(bit % 64);
        try self.words.put(word, self.words.get(word) & ~mask);
    }

    pub fn isSet(self: *const Row, bit: usize) bool {
        std.debug.assert(bit < self.bit_len);
        const word: u32 = @intCast(bit / 64);
        return self.words.get(word) & (@as(u64, 1) << @intCast(bit % 64)) != 0;
    }

    /// Exact range existence, without enumerating either members or live bits.
    pub fn anySetInRange(self: *const Row, start: usize, end: usize) bool {
        std.debug.assert(start <= end and end <= self.bit_len);
        if (start == end) return false;
        const first_word: u32 = @intCast(start / 64);
        const last_word: u32 = @intCast((end - 1) / 64);
        const first_mask = @as(u64, std.math.maxInt(u64)) << @as(u6, @intCast(start % 64));
        const last_mask = @as(u64, std.math.maxInt(u64)) >> @as(u6, @intCast(63 - (end - 1) % 64));
        if (first_word == last_word) return self.words.get(first_word) & first_mask & last_mask != 0;
        if (self.words.get(first_word) & first_mask != 0) return true;
        if (self.words.get(last_word) & last_mask != 0) return true;
        return self.words.hasNonEmptyInRange(first_word + 1, last_word);
    }

    pub fn unsetAll(self: *Row) void {
        self.words.clear();
    }

    pub fn setUnion(self: *Row, other: *const Row) Allocator.Error!void {
        std.debug.assert(self.bit_len == other.bit_len);
        _ = try self.words.joinWith(&other.words, {}, unionWord);
    }

    pub fn setIntersection(self: *Row, other: *const Row) Allocator.Error!void {
        std.debug.assert(self.bit_len == other.bit_len);
        _ = try self.words.meetWith(&other.words, {}, intersectWord);
    }

    /// Whether any bit is set. The snapshot answers from its structure, so
    /// this costs the tree depth rather than the domain's width.
    pub fn isEmpty(self: *const Row) bool {
        return !self.words.hasNonEmptyInRange(0, (self.bit_len + 63) / 64);
    }

    pub fn count(self: *const Row) usize {
        var total: usize = 0;
        for (0..(self.bit_len + 63) / 64) |word| total += @popCount(self.words.get(@intCast(word)));
        return total;
    }

    pub fn eql(self: *const Row, other: *const Row) bool {
        return self.bit_len == other.bit_len and self.words.eql(&other.words);
    }

    /// Set bits in ascending order, skipping absent subtrees.
    pub fn iterator(self: *const Row) Iterator {
        return self.iteratorRange(0, self.bit_len);
    }

    /// Set bits in a half-open range, skipping absent and excluded word
    /// subtrees rather than looking up every word in the domain.
    pub fn iteratorRange(self: *const Row, start: usize, end: usize) Iterator {
        std.debug.assert(start <= end and end <= self.bit_len);
        return .{
            .words = self.words.iteratorRange(@intCast(start / 64), if (start == end) start / 64 else @as(u64, @intCast(end / 64)) + @intFromBool(end % 64 != 0)),
            .start = start,
            .end = end,
        };
    }

    pub const Iterator = struct {
        words: ArcSnapshot(u64, 0).RangeIterator,
        start: usize,
        end: usize,
        base: usize = 0,
        pending: u64 = 0,

        pub fn next(self: *Iterator) ?usize {
            while (self.pending == 0) {
                const entry = self.words.next() orelse return null;
                self.base = @as(usize, entry.index) * 64;
                self.pending = entry.value;
                if (self.start > self.base) self.pending &= @as(u64, std.math.maxInt(u64)) << @as(u6, @intCast(self.start - self.base));
                if (self.end - self.base < 64) self.pending &= @as(u64, std.math.maxInt(u64)) >> @as(u6, @intCast(64 - (self.end - self.base)));
            }
            const bit = self.base + @ctz(self.pending);
            self.pending &= self.pending - 1;
            return bit;
        }
    };

    fn unionWord(_: void, lhs: u64, rhs: u64) u64 {
        return lhs | rhs;
    }

    fn intersectWord(_: void, lhs: u64, rhs: u64) u64 {
        return lhs & rhs;
    }
};

/// A control graph in compressed adjacency form, with the bits each node
/// reads and defines and the bits each edge kills. `read_starts[n]..
/// read_starts[n + 1]` indexes node n's read bits in `reads`, and likewise
/// for successors, predecessors, and defined bits; `kill_starts[e]..
/// kill_starts[e + 1]` indexes the bits killed on edge `e`, a position in
/// `succs`, and is empty when no edge kills anything.
pub const Graph = struct {
    succ_starts: []const u32,
    succs: []const u32,
    pred_starts: []const u32,
    preds: []const u32,
    read_starts: []const u32,
    reads: []const u32,
    def_starts: []const u32,
    defs: []const u32,
    kill_starts: []const u32 = &.{},
    kills: []const u32 = &.{},
    bit_len: usize,

    pub fn nodeCount(self: *const Graph) usize {
        return self.succ_starts.len - 1;
    }

    pub fn successorsOf(self: *const Graph, node: usize) []const u32 {
        return self.succs[self.succ_starts[node]..self.succ_starts[node + 1]];
    }

    pub fn predecessorsOf(self: *const Graph, node: usize) []const u32 {
        return self.preds[self.pred_starts[node]..self.pred_starts[node + 1]];
    }

    fn readsOf(self: *const Graph, node: usize) []const u32 {
        return self.reads[self.read_starts[node]..self.read_starts[node + 1]];
    }

    fn defsOf(self: *const Graph, node: usize) []const u32 {
        return self.defs[self.def_starts[node]..self.def_starts[node + 1]];
    }

    fn killsOf(self: *const Graph, edge: usize) []const u32 {
        if (self.kill_starts.len == 0) return &.{};
        return self.kills[self.kill_starts[edge]..self.kill_starts[edge + 1]];
    }

    fn defines(self: *const Graph, node: u32, bit: u32) bool {
        return std.mem.findScalar(u32, self.defsOf(node), bit) != null;
    }

    fn reads_(self: *const Graph, node: u32, bit: u32) bool {
        return std.mem.findScalar(u32, self.readsOf(node), bit) != null;
    }

    /// Whether the bit exposed at `successor` is exposed at `predecessor`:
    /// the predecessor does not define it, and some edge between them does
    /// not kill it.
    fn passes(self: *const Graph, predecessor: u32, successor: u32, bit: u32) bool {
        if (self.defines(predecessor, bit)) return false;
        if (self.kill_starts.len == 0) return true;
        for (self.successorsOf(predecessor), self.succ_starts[predecessor]..) |candidate, edge| {
            if (candidate == successor and std.mem.findScalar(u32, self.killsOf(edge), bit) == null) return true;
        }
        return false;
    }
};

/// The right-hand side of `node`'s equation over the rows `rowOf` answers:
/// each successor's row less the bits its edge kills, less the node's
/// defined bits, plus its reads. The result lives in `allocator`.
pub fn equationRow(
    graph: *const Graph,
    node: usize,
    allocator: Allocator,
    rows: anytype,
    comptime rowOf: fn (@TypeOf(rows), u32) *const Row,
) Allocator.Error!Row {
    var row = Row.initEmpty(allocator, graph.bit_len);
    for (graph.successorsOf(node), graph.succ_starts[node]..) |successor, edge| {
        const kills = graph.killsOf(edge);
        if (kills.len == 0) {
            try row.setUnion(rowOf(rows, successor));
            continue;
        }
        var exposed = rowOf(rows, successor).clone(allocator);
        for (kills) |bit| try exposed.unset(bit);
        try row.setUnion(&exposed);
    }
    for (graph.defsOf(node)) |bit| try row.unset(bit);
    for (graph.readsOf(node)) |bit| try row.set(bit);
    return row;
}

fn rowAt(rows: []const Row, node: u32) *const Row {
    return &rows[node];
}

/// Debug-only check that every row satisfies its equation, independently of
/// how the rows were solved.
pub fn certify(scratch_allocator: Allocator, graph: *const Graph, rows: []const Row) Allocator.Error!void {
    if (builtin.mode != .debug) return;
    var arena = std.heap.ArenaAllocator.init(scratch_allocator);
    defer arena.deinit();
    for (rows, 0..) |*row, node| {
        const candidate = try equationRow(graph, node, arena.allocator(), rows, rowAt);
        if (!candidate.eql(row)) invariant("ARC liveness row did not satisfy its equation", .{});
    }
}

/// Solves every node's row. Rows and their shared subtrees live in
/// `allocator`; all other work uses `scratch_allocator` and is freed.
pub fn solve(allocator: Allocator, scratch_allocator: Allocator, graph: *const Graph) Allocator.Error![]Row {
    var scratch_arena = collections.SingleThreadArena.init(scratch_allocator);
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

    // Bits each loop defines or kills: a node's definitions belong to its
    // innermost loop and to every loop around it, and an edge's kills to the
    // innermost loop holding both its ends.
    const written = try scratch.alloc(Row, loop_count);
    for (written) |*bits| bits.* = Row.initEmpty(scratch, graph.bit_len);
    for (0..node_count) |node_index| {
        const loop = forest.innermost[node_index];
        if (loop == no_loop) continue;
        for (graph.defsOf(node_index)) |bit| try written[loop].set(bit);
        for (graph.successorsOf(node_index), graph.succ_starts[node_index]..) |successor, edge| {
            const kills = graph.killsOf(edge);
            if (kills.len == 0) continue;
            const target_loop = forest.innermost[successor];
            if (target_loop == no_loop) continue;
            var common = loop;
            while (common != no_loop and !forest.contains(common, target_loop)) common = forest.loops[common].parent;
            if (common == no_loop) continue;
            for (kills) |bit| try written[common].set(bit);
        }
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
    const components = try collections.scc.compute(scratch, graph.succ_starts, graph.succs, graph.pred_starts, graph.preds);
    const component_of = components.component_of;

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
                if (search.graph_.passes(predecessor, target, search.bit)) try search.reach(predecessor);
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

    var component_cursor = components.count();
    while (component_cursor > 0) {
        component_cursor -= 1;
        const members = components.membersOf(component_cursor);
        const first = members[0];
        if (members.len == 1 and forest.innermost[first] == no_loop) {
            // One exact equation step from the solved successors' rows.
            rows[first] = try equationRow(graph, first, allocator, @as([]const Row, rows), rowAt);
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
            for (graph.successorsOf(member), graph.succ_starts[member]..) |successor, edge| {
                if (component_of[successor] == component) continue;
                const kills = graph.killsOf(edge);
                var exposed = rows[successor].iterator();
                while (exposed.next()) |bit| {
                    if (std.mem.findScalar(u32, kills, @intCast(bit)) != null) continue;
                    try seeds.append(scratch, .{ .index = member, .bit = @intCast(bit) });
                }
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
                if (graph.reads_(seed, search.bit) or !graph.defines(seed, search.bit)) try search.reach(seed);
            }
            while (search.work.pop()) |unit| {
                if (unit.loop) {
                    try search.loop_bits.append(scratch, .{ .index = unit.index, .bit = search.bit });
                    try search.reachPredecessorsOf(forest.loops[unit.index].header, unit.index);
                    for (side_entries.items[side_entry_starts[unit.index]..side_entry_starts[unit.index + 1]]) |entry| {
                        if (component_of[entry.predecessor] != component) continue;
                        if (graph.passes(entry.predecessor, entry.target, search.bit)) try search.reach(entry.predecessor);
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
    try certify(scratch_allocator, graph, rows);
    return rows;
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
    kill_starts: std.ArrayList(u32) = .empty,
    kills: std.ArrayList(u32) = .empty,

    /// `kills[i]` lists the bits killed on the edge to `succs[i]`.
    const Node = struct { succs: []const u32 = &.{}, reads: []const u32 = &.{}, defs: []const u32 = &.{}, kills: []const []const u32 = &.{} };

    fn build(allocator: Allocator, nodes: []const Node) Allocator.Error!TestGraph {
        var graph = TestGraph{};
        try graph.succ_starts.append(allocator, 0);
        try graph.read_starts.append(allocator, 0);
        try graph.def_starts.append(allocator, 0);
        try graph.kill_starts.append(allocator, 0);
        for (nodes) |node| {
            try graph.succs.appendSlice(allocator, node.succs);
            try graph.succ_starts.append(allocator, @intCast(graph.succs.items.len));
            for (node.succs, 0..) |_, edge| {
                if (edge < node.kills.len) try graph.kills.appendSlice(allocator, node.kills[edge]);
                try graph.kill_starts.append(allocator, @intCast(graph.kills.items.len));
            }
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
            .kill_starts = if (self.kills.items.len == 0) &.{} else self.kill_starts.items,
            .kills = self.kills.items,
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
    var arena = collections.SingleThreadArena.init(std.testing.allocator);
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
    var arena = collections.SingleThreadArena.init(std.testing.allocator);
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

test "liveness stops at a kill on one edge but not the other" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    // 0 -> 1 (kills 0), 0 -> 2; 1: reads 0; 2: reads 0, 1
    const graph = try TestGraph.build(arena.allocator(), &.{
        .{ .succs = &.{ 1, 2 }, .kills = &.{&.{0}} },
        .{ .reads = &.{0} },
        .{ .reads = &.{ 0, 1 } },
    });
    const view = graph.view(2);
    const rows = try solve(arena.allocator(), std.testing.allocator, &view);
    try expectRowBits(&rows[0], &.{ 0, 1 });
    try expectRowBits(&rows[1], &.{0});
    try expectRowBits(&rows[2], &.{ 0, 1 });
}

test "liveness carries a bit through a loop whose exit edge kills it" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    // 0 -> 1; 1: header -> 2, 3 (kills 0); 2: reads 0 -> 1; 3: reads 0
    const graph = try TestGraph.build(arena.allocator(), &.{
        .{ .succs = &.{1} },
        .{ .succs = &.{ 2, 3 }, .kills = &.{ &.{}, &.{0} } },
        .{ .succs = &.{1}, .reads = &.{0} },
        .{ .reads = &.{0} },
    });
    const view = graph.view(1);
    const rows = try solve(arena.allocator(), std.testing.allocator, &view);
    try expectRowBits(&rows[0], &.{0});
    try expectRowBits(&rows[1], &.{0});
    try expectRowBits(&rows[2], &.{0});
    try expectRowBits(&rows[3], &.{0});
}

test "liveness does not carry a bit a loop kills on its back edge" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    // 0 -> 1; 1: header -> 2, 3; 2 -> 1 (kills 0); 3: reads 0
    const graph = try TestGraph.build(arena.allocator(), &.{
        .{ .succs = &.{1} },
        .{ .succs = &.{ 2, 3 } },
        .{ .succs = &.{1}, .kills = &.{&.{0}} },
        .{ .reads = &.{0} },
    });
    const view = graph.view(1);
    const rows = try solve(arena.allocator(), std.testing.allocator, &view);
    try expectRowBits(&rows[0], &.{0});
    try expectRowBits(&rows[1], &.{0});
    try expectRowBits(&rows[2], &.{});
    try expectRowBits(&rows[3], &.{0});
}

test "rows preserve operations across persistent forks" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    for ([_]usize{ 64, 65 }) |bit_len| {
        var left = Row.initEmpty(arena.allocator(), bit_len);
        var right = Row.initEmpty(arena.allocator(), bit_len);
        try left.set(0);
        try left.set(bit_len - 1);
        try right.set(bit_len - 1);
        try left.setIntersection(&right);
        try std.testing.expectEqual(@as(usize, 1), left.count());
        try std.testing.expect(left.isSet(bit_len - 1));
        const cloned = left.clone(arena.allocator());
        try std.testing.expect(cloned.eql(&left));
        // A domain whose width is not a whole number of words must not
        // enumerate the padding bits above it.
        try expectRowBits(&cloned, &.{bit_len - 1});
    }
}
