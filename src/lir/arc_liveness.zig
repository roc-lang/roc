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
//! once on that loop's carried row, and a loop header's row is that row plus
//! the bits exposed at the header itself. Every other node, in a loop or not,
//! is one equation step from its successors' finished rows and shares their
//! structure.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");
const ArcSnapshot = @import("arc_state.zig").Snapshot;

const invariant = base.invariant;

const Allocator = std.mem.Allocator;
const LoopForest = collections.LoopForest;

/// A persistent bit row over an exact bit domain. A domain of at most
/// `inline_bits` bits is held inline, by value, in the space a tree's root
/// takes: it copies as plain words and changes without allocating. A wider
/// domain is a persistent tree whose copies share structure; changing a bit
/// allocates only that bit's path, in the allocator the change names. Both
/// forms give every operation the same answer.
pub const Row = struct {
    storage: Storage,
    bit_len: u32,

    const Words = ArcSnapshot(u64, 0);
    /// As many words as fit in a tree root, whose size follows the target's
    /// pointer width.
    const inline_word_count = @sizeOf(Words) / @sizeOf(u64);
    pub const inline_bits = 64 * inline_word_count;

    /// `inline_words` exactly when the domain fits `inline_bits`; words past
    /// the domain stay zero.
    const Storage = union {
        inline_words: [inline_word_count]u64,
        tree: Words,
    };

    comptime {
        std.debug.assert(inline_word_count >= 1);
        std.debug.assert(@sizeOf([inline_word_count]u64) <= @sizeOf(Words));
    }

    /// A domain wider than `u32` bits is a compiler invariant violation: ARC
    /// bit domains are bounded by a procedure's frame.
    pub fn initEmpty(bit_len: usize) Row {
        if (bit_len <= inline_bits) return .{ .bit_len = @intCast(bit_len), .storage = .{ .inline_words = @splat(0) } };
        const word_len = (bit_len + 63) / 64;
        return .{ .bit_len = @intCast(bit_len), .storage = .{ .tree = Words.init(word_len) } };
    }

    fn isInline(self: *const Row) bool {
        return self.bit_len <= inline_bits;
    }

    fn wordLen(self: *const Row) usize {
        return (@as(usize, self.bit_len) + 63) / 64;
    }

    pub fn set(self: *Row, allocator: Allocator, bit: usize) Allocator.Error!void {
        std.debug.assert(bit < self.bit_len);
        const word: u32 = @intCast(bit / 64);
        const mask = @as(u64, 1) << @intCast(bit % 64);
        if (self.isInline()) {
            self.storage.inline_words[word] |= mask;
            return;
        }
        try self.storage.tree.put(allocator, word, self.storage.tree.get(word) | mask);
    }

    pub fn unset(self: *Row, allocator: Allocator, bit: usize) Allocator.Error!void {
        std.debug.assert(bit < self.bit_len);
        const word: u32 = @intCast(bit / 64);
        const mask = @as(u64, 1) << @intCast(bit % 64);
        if (self.isInline()) {
            self.storage.inline_words[word] &= ~mask;
            return;
        }
        try self.storage.tree.put(allocator, word, self.storage.tree.get(word) & ~mask);
    }

    /// The word holding bits `64 * index` up to `64 * index + 64`.
    fn wordAt(self: *const Row, index: u32) u64 {
        if (self.isInline()) return self.storage.inline_words[index];
        return self.storage.tree.get(index);
    }

    pub fn isSet(self: *const Row, bit: usize) bool {
        std.debug.assert(bit < self.bit_len);
        return self.wordAt(@intCast(bit / 64)) & (@as(u64, 1) << @intCast(bit % 64)) != 0;
    }

    /// Exact range existence, without enumerating either members or live bits.
    pub fn anySetInRange(self: *const Row, start: usize, end: usize) bool {
        std.debug.assert(start <= end and end <= self.bit_len);
        if (start == end) return false;
        const first_word: u32 = @intCast(start / 64);
        const last_word: u32 = @intCast((end - 1) / 64);
        const first_mask = @as(u64, std.math.maxInt(u64)) << @as(u6, @intCast(start % 64));
        const last_mask = @as(u64, std.math.maxInt(u64)) >> @as(u6, @intCast(63 - (end - 1) % 64));
        if (first_word == last_word) return self.wordAt(first_word) & first_mask & last_mask != 0;
        if (self.wordAt(first_word) & first_mask != 0) return true;
        if (self.wordAt(last_word) & last_mask != 0) return true;
        if (self.isInline()) {
            for (self.storage.inline_words[first_word + 1 .. last_word]) |word| if (word != 0) return true;
            return false;
        }
        return self.storage.tree.hasNonEmptyInRange(first_word + 1, last_word);
    }

    pub fn unsetAll(self: *Row) void {
        if (self.isInline()) {
            self.storage.inline_words = @splat(0);
            return;
        }
        self.storage.tree.clear();
    }

    pub fn setUnion(self: *Row, allocator: Allocator, other: *const Row) Allocator.Error!void {
        std.debug.assert(self.bit_len == other.bit_len);
        if (self.isInline()) {
            for (&self.storage.inline_words, other.storage.inline_words) |*word, other_word| word.* |= other_word;
            return;
        }
        _ = try self.storage.tree.joinWith(allocator, &other.storage.tree, {}, unionWord);
    }

    pub fn setIntersection(self: *Row, allocator: Allocator, other: *const Row) Allocator.Error!void {
        std.debug.assert(self.bit_len == other.bit_len);
        if (self.isInline()) {
            for (&self.storage.inline_words, other.storage.inline_words) |*word, other_word| word.* &= other_word;
            return;
        }
        _ = try self.storage.tree.meetWith(allocator, &other.storage.tree, {}, intersectWord);
    }

    /// Whether any bit is set. The snapshot answers from its structure, so
    /// this costs the tree depth rather than the domain's width.
    pub fn isEmpty(self: *const Row) bool {
        if (self.isInline()) {
            for (self.storage.inline_words) |word| if (word != 0) return false;
            return true;
        }
        return !self.storage.tree.hasNonEmptyInRange(0, self.wordLen());
    }

    pub fn count(self: *const Row) usize {
        var total: usize = 0;
        for (0..self.wordLen()) |word| total += @popCount(self.wordAt(@intCast(word)));
        return total;
    }

    pub fn eql(self: *const Row, other: *const Row) bool {
        if (self.bit_len != other.bit_len) return false;
        if (self.isInline()) return std.mem.eql(u64, &self.storage.inline_words, &other.storage.inline_words);
        return self.storage.tree.eql(&other.storage.tree);
    }

    /// Calls `emitFn` with the index and both words of every word that may
    /// differ between this row and `other`, among this row's nonempty words,
    /// in ascending order. `emitFn` must treat equal words as no difference.
    pub fn differenceWith(
        self: *const Row,
        other: *const Row,
        context: anytype,
        comptime emitFn: fn (@TypeOf(context), u32, u64, u64) Allocator.Error!void,
    ) Allocator.Error!void {
        std.debug.assert(self.bit_len == other.bit_len);
        if (self.isInline()) {
            for (self.storage.inline_words, other.storage.inline_words, 0..) |word, other_word, index| {
                if (word != 0) try emitFn(context, @intCast(index), word, other_word);
            }
            return;
        }
        try self.storage.tree.differenceWith(&other.storage.tree, context, emitFn);
    }

    /// Set bits in ascending order, skipping absent subtrees.
    pub fn iterator(self: *const Row) Iterator {
        return self.iteratorRange(0, self.bit_len);
    }

    /// Set bits in a half-open range, skipping absent and excluded word
    /// subtrees rather than looking up every word in the domain.
    pub fn iteratorRange(self: *const Row, start: usize, end: usize) Iterator {
        std.debug.assert(start <= end and end <= self.bit_len);
        if (self.isInline()) {
            // The inline words, masked to the range, are the whole row.
            var words = self.storage.inline_words;
            for (&words, 0..) |*word, index| {
                const word_start = index * 64;
                if (end <= word_start or start >= word_start + 64) {
                    word.* = 0;
                    continue;
                }
                if (start > word_start) word.* &= @as(u64, std.math.maxInt(u64)) << @as(u6, @intCast(start - word_start));
                if (end < word_start + 64) word.* &= ~(@as(u64, std.math.maxInt(u64)) << @as(u6, @intCast(end - word_start)));
            }
            return .{ .source = .{ .inline_words = .{ .words = words } }, .start = start, .end = end };
        }
        return .{
            .source = .{ .tree = self.storage.tree.iteratorRange(@intCast(start / 64), if (start == end) start / 64 else @as(u64, @intCast(end / 64)) + @intFromBool(end % 64 != 0)) },
            .start = start,
            .end = end,
        };
    }

    pub const Iterator = struct {
        /// Where the words after `pending` come from.
        source: union(enum) {
            inline_words: struct { words: [inline_word_count]u64, next: u8 = 0 },
            tree: Words.RangeIterator,
        },
        start: usize,
        end: usize,
        base: usize = 0,
        pending: u64 = 0,

        pub fn next(self: *Iterator) ?usize {
            while (self.pending == 0) switch (self.source) {
                .inline_words => |*inline_source| {
                    if (inline_source.next == inline_word_count) return null;
                    self.base = @as(usize, inline_source.next) * 64;
                    self.pending = inline_source.words[inline_source.next];
                    inline_source.next += 1;
                },
                .tree => |*words| {
                    const entry = words.next() orelse return null;
                    self.base = @as(usize, entry.index) * 64;
                    self.pending = entry.value;
                    if (self.start > self.base) self.pending &= @as(u64, std.math.maxInt(u64)) << @as(u6, @intCast(self.start - self.base));
                    if (self.end - self.base < 64) self.pending &= @as(u64, std.math.maxInt(u64)) >> @as(u6, @intCast(64 - (self.end - self.base)));
                },
            };
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
    var row = Row.initEmpty(graph.bit_len);
    for (graph.successorsOf(node), graph.succ_starts[node]..) |successor, edge| {
        const kills = graph.killsOf(edge);
        if (kills.len == 0) {
            try row.setUnion(allocator, rowOf(rows, successor));
            continue;
        }
        var exposed = rowOf(rows, successor).*;
        for (kills) |bit| try exposed.unset(allocator, bit);
        try row.setUnion(allocator, &exposed);
    }
    for (graph.defsOf(node)) |bit| try row.unset(allocator, bit);
    for (graph.readsOf(node)) |bit| try row.set(allocator, bit);
    return row;
}

/// Whether `node` is the header of its innermost loop.
fn isHeader(forest: *const LoopForest, node: u32) bool {
    const loop = forest.innermostOf(node) orelse return false;
    return forest.loops[loop].header == node;
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

/// Chains `node` onto `bit`'s seeds, listing `bit` the first time it is seeded.
/// Solves every node's row. Rows and their shared subtrees live in
/// `allocator`; all other work uses `scratch_allocator` and is freed.
pub fn solve(allocator: Allocator, scratch_allocator: Allocator, graph: *const Graph) Allocator.Error![]Row {
    var scratch_arena = collections.SingleThreadArena.init(scratch_allocator);
    defer scratch_arena.deinit();
    const scratch = scratch_arena.allocator();
    const node_count = graph.nodeCount();

    const rows = try allocator.alloc(Row, node_count);
    for (rows) |*row| row.* = Row.initEmpty(graph.bit_len);

    var forest = try LoopForest.build(scratch, graph.succ_starts, graph.succs, graph.pred_starts, graph.preds);
    const loop_count = forest.loops.len;

    // Edges entering a loop other than at its header, by loop, each loop's
    // in the order they are found.
    const LoopEntry = struct { loop: u32, predecessor: u32, target: u32 };
    var found_entries = std.ArrayList(LoopEntry).empty;
    for (0..node_count) |target_index| {
        const target: u32 = @intCast(target_index);
        for (graph.predecessorsOf(target)) |predecessor| {
            var cursor = forest.innermostOf(target);
            while (cursor) |loop| : (cursor = forest.loops[loop].parent.get()) {
                if (forest.containsNode(loop, predecessor)) break;
                if (forest.loops[loop].header != target) {
                    try found_entries.append(scratch, .{ .loop = loop, .predecessor = predecessor, .target = target });
                }
            }
        }
    }
    const side_entry_starts = try scratch.alloc(u32, loop_count + 1);
    const side_entries = try scratch.alloc(LoopEntry, found_entries.items.len);
    {
        @memset(side_entry_starts, 0);
        for (found_entries.items) |entry| side_entry_starts[entry.loop + 1] += 1;
        for (1..side_entry_starts.len) |loop| side_entry_starts[loop] += side_entry_starts[loop - 1];
        const fill = try scratch.dupe(u32, side_entry_starts[0..loop_count]);
        for (found_entries.items) |entry| {
            side_entries[fill[entry.loop]] = entry;
            fill[entry.loop] += 1;
        }
    }

    // Bits each loop defines or kills: a node's definitions belong to its
    // innermost loop and to every loop around it, and an edge's kills to the
    // innermost loop holding both its ends.
    const written = try scratch.alloc(Row, loop_count);
    for (written) |*bits| bits.* = Row.initEmpty(graph.bit_len);
    for (0..node_count) |node_index| {
        const loop = forest.innermostOf(node_index) orelse continue;
        for (graph.defsOf(node_index)) |bit| try written[loop].set(scratch, bit);
        for (graph.successorsOf(node_index), graph.succ_starts[node_index]..) |successor, edge| {
            const kills = graph.killsOf(edge);
            if (kills.len == 0) continue;
            const target_loop = forest.innermostOf(successor) orelse continue;
            var cursor: ?u32 = loop;
            while (cursor) |candidate| {
                if (forest.contains(candidate, target_loop)) break;
                cursor = forest.loops[candidate].parent.get();
            }
            const common = cursor orelse continue;
            for (kills) |bit| try written[common].set(scratch, bit);
        }
    }
    {
        var preorder_index = loop_count;
        while (preorder_index > 0) {
            preorder_index -= 1;
            const loop = forest.preorder[preorder_index];
            if (forest.loops[loop].parent.get()) |parent| try written[parent].setUnion(scratch, &written[loop]);
        }
    }

    // Components in an order that solves every successor component first.
    const components = try collections.scc.compute(scratch, graph.succ_starts, graph.succs, graph.pred_starts, graph.preds);
    const component_of = components.component_of;

    // Inside a cyclic component, one backward search per bit. A unit is a
    // node, or the largest loop around a node that defines nothing of the
    // bit, whose whole body shares one answer.
    const Unit = struct { loop: bool, index: u32 };
    const Search = struct {
        graph_: *const Graph,
        forest_: *const LoopForest,
        written_: []const Row,
        component_of_: []const u32,
        component: u32 = 0,
        node_seen: collections.EpochSet(u32, .stamps, .internal),
        loop_seen: collections.EpochSet(u32, .stamps, .internal),
        bit: u32 = 0,
        work: std.ArrayList(Unit) = .empty,
        node_bits: collections.SortedGroups(u32) = .{},
        loop_bits: collections.SortedGroups(u32) = .{},
        allocator_: Allocator,

        fn transparent(search: *const @This(), loop: u32) bool {
            return !search.written_[loop].isSet(search.bit);
        }

        fn reach(search: *@This(), node: u32) Allocator.Error!void {
            if (search.component_of_[node] != search.component) return;
            const transparent_loop: ?u32 = if (search.forest_.innermostOf(node)) |innermost|
                (if (search.transparent(innermost)) innermost else null)
            else
                null;
            if (transparent_loop) |innermost| {
                const loop = search.forest_.outermostWhile(innermost, search, transparent);
                if (search.loop_seen.testAndInsert(loop)) return;
                try search.work.append(search.allocator_, .{ .loop = true, .index = loop });
            } else {
                if (search.node_seen.testAndInsert(node)) return;
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
        .node_seen = try .init(scratch, node_count),
        .loop_seen = try .init(scratch, loop_count),
        .allocator_ = scratch,
    };
    const carried = try scratch.alloc(Row, loop_count);
    // Group seed nodes by bit without a directory over the whole bit domain.
    var seeds: collections.SortedGroups(u32) = .{};
    // A component's non-header members, by how many of their successors in
    // the component that are not headers still lack a row.
    var pending = collections.DenseMap(u32, u32).init(scratch);
    var ready = std.ArrayList(u32).empty;

    var component_cursor = components.count();
    while (component_cursor > 0) {
        component_cursor -= 1;
        const members = components.membersOf(component_cursor);
        const first = members[0];
        if (members.len == 1 and forest.innermostOf(first) == null) {
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
            for (graph.readsOf(member)) |bit| try seeds.add(scratch, bit, member);
            for (graph.successorsOf(member), graph.succ_starts[member]..) |successor, edge| {
                if (component_of[successor] == component) continue;
                const kills = graph.killsOf(edge);
                var exposed = rows[successor].iterator();
                while (exposed.next()) |bit| {
                    if (std.mem.findScalar(u32, kills, @intCast(bit)) != null) continue;
                    try seeds.add(scratch, @intCast(bit), member);
                }
            }
        }
        seeds.finish();
        var seed_runs = seeds.runs();
        while (seed_runs.next()) |run| {
            search.node_seen.reset();
            search.loop_seen.reset();
            search.bit = run.key;
            // Head-pushed chains visit the newest seed first.
            var cursor = run.pairs.len;
            while (cursor > 0) {
                cursor -= 1;
                const seed = run.pairs[cursor].item;
                // An exit seed is exposed at its node only through the node's
                // definition; a read is exposed regardless.
                if (graph.reads_(seed, search.bit) or !graph.defines(seed, search.bit)) try search.reach(seed);
            }
            while (search.work.pop()) |unit| {
                if (unit.loop) {
                    try search.loop_bits.add(scratch, unit.index, search.bit);
                    try search.reachPredecessorsOf(forest.loops[unit.index].header, unit.index);
                    for (side_entries[side_entry_starts[unit.index]..side_entry_starts[unit.index + 1]]) |entry| {
                        if (component_of[entry.predecessor] != component) continue;
                        if (graph.passes(entry.predecessor, entry.target, search.bit)) try search.reach(entry.predecessor);
                    }
                } else {
                    // Only a loop header's row is built from its exposed bits;
                    // every other member follows from its successors' rows.
                    if (isHeader(&forest, unit.index)) try search.node_bits.add(scratch, unit.index, search.bit);
                    try search.reachPredecessorsOf(unit.index, null);
                }
            }
        }

        search.node_bits.finish();
        search.loop_bits.finish();

        // Rows: the bits whole loops carry, shared down the component's loop
        // tree, plus the bits exposed at the node itself.
        var top = forest.innermostOf(first).?;
        while (forest.loops[top].parent.get()) |parent| top = parent;
        for (forest.preorder[forest.loops[top].enter .. forest.loops[top].exit + 1]) |loop| {
            var row = if (forest.loops[loop].parent.get()) |parent| carried[parent] else Row.initEmpty(graph.bit_len);
            const bits = search.loop_bits.group(loop);
            var cursor = bits.len;
            while (cursor > 0) {
                cursor -= 1;
                try row.set(allocator, bits[cursor].item);
            }
            carried[loop] = row;
        }
        // A header's row is the bits its loops carry plus the bits exposed
        // at the header. Every other member's row is one equation step from
        // its successors' rows, which the least solution satisfies, so it is
        // built after them and shares their structure: removing the edges
        // into headers leaves the component acyclic, since every cycle
        // passes through a depth-first back edge and every back edge enters
        // a header. Building each member's row from its own exposed bits
        // instead would share nothing between neighboring rows, and a loop
        // holding many values live across many nodes would allocate the
        // product of the two.
        pending.clearRetainingCapacity();
        ready.clearRetainingCapacity();
        for (members) |member| {
            if (isHeader(&forest, member)) {
                var row = carried[forest.innermostOf(member).?];
                const bits = search.node_bits.group(member);
                var cursor = bits.len;
                while (cursor > 0) {
                    cursor -= 1;
                    try row.set(allocator, bits[cursor].item);
                }
                rows[member] = row;
                continue;
            }
            var waiting: u32 = 0;
            for (graph.successorsOf(member)) |successor| {
                if (component_of[successor] == component and !isHeader(&forest, successor)) waiting += 1;
            }
            try pending.put(member, waiting);
            if (waiting == 0) try ready.append(scratch, member);
        }
        var built: usize = 0;
        while (ready.pop()) |member| {
            rows[member] = try equationRow(graph, member, allocator, @as([]const Row, rows), rowAt);
            built += 1;
            for (graph.predecessorsOf(member)) |predecessor| {
                const waiting = pending.getPtr(predecessor) orelse continue;
                waiting.* -= 1;
                if (waiting.* == 0) try ready.append(scratch, predecessor);
            }
        }
        if (built != pending.count()) invariant("ARC liveness component held a cycle that enters no loop header", .{});
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
        var left = Row.initEmpty(bit_len);
        var right = Row.initEmpty(bit_len);
        try left.set(arena.allocator(), 0);
        try left.set(arena.allocator(), bit_len - 1);
        try right.set(arena.allocator(), bit_len - 1);
        try left.setIntersection(arena.allocator(), &right);
        try std.testing.expectEqual(@as(usize, 1), left.count());
        try std.testing.expect(left.isSet(bit_len - 1));
        const cloned = left;
        try std.testing.expect(cloned.eql(&left));
        // A domain whose width is not a whole number of words must not
        // enumerate the padding bits above it.
        try expectRowBits(&cloned, &.{bit_len - 1});
    }
}

test "an inline row answers every operation as a tree row does" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var rng = std.Random.DefaultPrng.init(0x7264);
    const random = rng.random();
    // Each bit of an inline row is mirrored by the same bit of a tree row
    // over a wider domain, whose other bits stay clear.
    const width = Row.inline_bits;
    const Pair = struct { small: Row, wide: Row };
    var pairs: [4]Pair = undefined;
    for (&pairs) |*pair| pair.* = .{ .small = Row.initEmpty(width), .wide = Row.initEmpty(width + 66) };
    const WordDiff = struct {
        words: std.ArrayList(u64) = .empty,
        allocator: Allocator,
        fn note(self: *@This(), index: u32, lhs: u64, rhs: u64) Allocator.Error!void {
            if (lhs != rhs) try self.words.append(self.allocator, @as(u64, index) << 32 | @as(u64, @truncate(lhs ^ rhs)));
        }
    };
    for (0..2000) |_| {
        const target = &pairs[random.uintLessThan(usize, pairs.len)];
        const other = &pairs[random.uintLessThan(usize, pairs.len)];
        switch (random.uintLessThan(u3, 6)) {
            0, 1 => {
                const bit = random.uintLessThan(usize, width);
                try target.small.set(allocator, bit);
                try target.wide.set(allocator, bit);
            },
            2 => {
                const bit = random.uintLessThan(usize, width);
                try target.small.unset(allocator, bit);
                try target.wide.unset(allocator, bit);
            },
            3 => {
                try target.small.setUnion(allocator, &other.small);
                try target.wide.setUnion(allocator, &other.wide);
            },
            4 => {
                try target.small.setIntersection(allocator, &other.small);
                try target.wide.setIntersection(allocator, &other.wide);
            },
            else => if (random.uintLessThan(u4, 8) == 0) {
                target.small.unsetAll();
                target.wide.unsetAll();
            } else {
                target.* = other.*;
            },
        }
        for (pairs) |pair| {
            try std.testing.expectEqual(pair.wide.count(), pair.small.count());
            try std.testing.expectEqual(pair.wide.isEmpty(), pair.small.isEmpty());
            for (0..width) |bit| try std.testing.expectEqual(pair.wide.isSet(bit), pair.small.isSet(bit));
            const start = random.uintLessThan(usize, width + 1);
            const end = start + random.uintLessThan(usize, width + 1 - start);
            try std.testing.expectEqual(pair.wide.anySetInRange(start, end), pair.small.anySetInRange(start, end));
            var small_bits = pair.small.iteratorRange(start, end);
            var wide_bits = pair.wide.iteratorRange(start, end);
            while (true) {
                const expected = wide_bits.next();
                try std.testing.expectEqual(expected, small_bits.next());
                if (expected == null) break;
            }
        }
        try std.testing.expectEqual(other.wide.eql(&target.wide), other.small.eql(&target.small));
        var small_diff = WordDiff{ .allocator = allocator };
        var wide_diff = WordDiff{ .allocator = allocator };
        try target.small.differenceWith(&other.small, &small_diff, WordDiff.note);
        try target.wide.differenceWith(&other.wide, &wide_diff, WordDiff.note);
        try std.testing.expectEqualSlices(u64, wide_diff.words.items, small_diff.words.items);
    }
}
