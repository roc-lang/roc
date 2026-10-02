//! Exact procedure-local stack lifetimes. Straight-line lifetimes are intervals;
//! only block boundaries participate in backward propagation. No per-statement
//! live sets or pairwise interference graph are materialized.
const std = @import("std");
const collections = @import("collections");
const LoopForest = collections.LoopForest;
const lir = @import("lir");
const Allocator = std.mem.Allocator;
const none = std.math.maxInt(u32);
const Self = @This();

allocator: Allocator,
locals: collections.DenseMap(lir.LocalId, u32),
values: std.ArrayList(Value) = .empty,
nodes: std.ArrayList(Node) = .empty,
edges: std.ArrayList(Edge) = .empty,
accesses: std.ArrayList(Access) = .empty,
slots: std.ArrayList(Slot) = .empty,

/// One local, or the representative of a proven immutable alias group.
pub const Value = struct {
    local: lir.LocalId,
    representative: u32,
    definitions: u32 = 0,
    mutable: bool = false,
    size: u32 = 0,
    slot: ?u32 = null,
    ranges: Ranges = .{},
};
const Node = struct {
    succ: u32 = none,
    pred: u32 = none,
    succ_count: u32 = 0,
    pred_count: u32 = 0,
    block: u32 = none,
    position: u32 = none,
};
const Edge = struct { from: u32, to: u32, next_succ: u32, next_pred: u32 };
const Access = struct { node: u32, value: u32, read: bool, write: bool };
const Block = struct { first: u32, start: u32, end: u32 };
const Range = struct { start: u32, end: u32 };
// Most locals have one interval. Keep it inline so a long straight-line
// procedure never performs one heap allocation per temporary or stack slot.
const Ranges = struct {
    single: [1]Range = undefined,
    single_len: usize = 0,
    multiple: std.ArrayList(Range) = .empty,

    fn items(self: *const Ranges) []const Range {
        return if (self.multiple.capacity == 0) self.single[0..self.single_len] else self.multiple.items;
    }
    fn mutableItems(self: *Ranges) []Range {
        return if (self.multiple.capacity == 0) self.single[0..self.single_len] else self.multiple.items;
    }
    fn truncate(self: *Ranges, len: usize) void {
        if (self.multiple.capacity == 0) self.single_len = len else self.multiple.items.len = len;
    }
    fn append(self: *Ranges, allocator: Allocator, range: Range) Allocator.Error!void {
        if (self.multiple.capacity == 0) {
            if (self.single_len == 0) {
                self.single[0] = range;
                self.single_len = 1;
                return;
            }
            try self.multiple.ensureTotalCapacity(allocator, 2);
            self.multiple.appendAssumeCapacity(self.single[0]);
        }
        try self.multiple.append(allocator, range);
    }
    fn appendSlice(self: *Ranges, allocator: Allocator, ranges: []const Range) Allocator.Error!void {
        for (ranges) |range| try self.append(allocator, range);
    }
    fn deinit(self: *Ranges, allocator: Allocator) void {
        self.multiple.deinit(allocator);
    }
};
/// A physical slot's size and disjoint occupied intervals.
pub const Slot = struct {
    size: u32,
    ranges: Ranges = .{},
    cursor: usize = 0,
};

/// Start an empty procedure plan.
pub fn init(allocator: Allocator) Self {
    return .{ .allocator = allocator, .locals = collections.DenseMap(lir.LocalId, u32).init(allocator) };
}
/// Release all procedure-owned analysis storage.
pub fn deinit(self: *Self) void {
    for (self.values.items) |*v| v.ranges.deinit(self.allocator);
    for (self.slots.items) |*s| s.ranges.deinit(self.allocator);
    self.locals.deinit();
    self.values.deinit(self.allocator);
    self.nodes.deinit(self.allocator);
    self.edges.deinit(self.allocator);
    self.accesses.deinit(self.allocator);
    self.slots.deinit(self.allocator);
}
/// Intern a local in the compact procedure domain.
pub fn local(self: *Self, id: lir.LocalId) Allocator.Error!u32 {
    if (self.locals.get(id)) |index| return index;
    const index: u32 = @intCast(self.values.items.len);
    try self.values.append(self.allocator, .{ .local = id, .representative = index });
    try self.locals.put(id, index);
    return index;
}
/// Add an instruction-selection region. Reads and writes in one region overlap.
pub fn node(self: *Self) Allocator.Error!u32 {
    const index: u32 = @intCast(self.nodes.items.len);
    try self.nodes.append(self.allocator, .{});
    return index;
}
/// Record a runtime successor, including back edges.
pub fn edge(self: *Self, from: u32, to: u32) Allocator.Error!void {
    const index: u32 = @intCast(self.edges.items.len);
    try self.edges.append(self.allocator, .{ .from = from, .to = to, .next_succ = self.nodes.items[from].succ, .next_pred = self.nodes.items[to].pred });
    self.nodes.items[from].succ = index;
    self.nodes.items[from].succ_count += 1;
    self.nodes.items[to].pred = index;
    self.nodes.items[to].pred_count += 1;
}
/// Record an explicit local access. A write kills only that local's old value.
pub fn access(self: *Self, at: u32, id: lir.LocalId, read: bool, write: bool) Allocator.Error!void {
    const index = try self.local(id);
    try self.accesses.append(self.allocator, .{ .node = at, .value = index, .read = read, .write = write });
    if (write) self.values.items[index].definitions += 1;
}
/// Resolve and compress a proven alias chain.
pub fn representative(self: *Self, index: u32) u32 {
    var root = index;
    while (self.values.items[root].representative != root) root = self.values.items[root].representative;
    var current = index;
    while (current != root) {
        const next = self.values.items[current].representative;
        self.values.items[current].representative = root;
        current = next;
    }
    return root;
}

fn appendBlock(self: *Self, blocks: *std.ArrayList(Block), first: u32, position: *u32) Allocator.Error!void {
    const index: u32 = @intCast(blocks.items.len);
    const start = position.*;
    var current = first;
    while (true) {
        const n = &self.nodes.items[current];
        n.block = index;
        n.position = position.*;
        position.* += 1;
        if (n.succ_count != 1) break;
        const next = self.edges.items[n.succ].to;
        if (self.nodes.items[next].pred_count != 1 or self.nodes.items[next].block != none) break;
        current = next;
    }
    try blocks.append(self.allocator, .{ .first = first, .start = start, .end = position.* });
}
fn accessLess(self: *Self, a: Access, b: Access) bool {
    if (a.value != b.value) return a.value < b.value;
    return self.nodes.items[a.node].position < self.nodes.items[b.node].position;
}
fn rangeLess(_: void, a: Range, b: Range) bool {
    return a.start < b.start;
}
fn compactRanges(ranges: *Ranges) void {
    std.mem.sort(Range, ranges.mutableItems(), {}, rangeLess);
    var count: usize = 0;
    for (ranges.items()) |r| {
        if (count > 0 and r.start <= ranges.items()[count - 1].end) {
            ranges.mutableItems()[count - 1].end = @max(r.end, ranges.items()[count - 1].end);
        } else {
            ranges.mutableItems()[count] = r;
            count += 1;
        }
    }
    ranges.truncate(count);
}
/// The block graph of a procedure, its loop-nesting forest, and each loop's
/// position coverage.
const BlockStructure = struct {
    succ_starts: []u32,
    succs: []u32,
    pred_starts: []u32,
    preds: []u32,
    forest: LoopForest,
    /// Every loop's entering blocks: its header's predecessors outside it,
    /// then its side entries.
    entry_starts: []u32,
    entries: []u32,
    /// Blocks ordered by their innermost loop's loop-tree position (blocks in
    /// no loop last), so each loop's blocks are one contiguous run.
    blocks_by_loop: []u32,
    /// The span of positions a loop's blocks cover, and whether they cover it
    /// without gaps.
    first_position: []u32,
    end_position: []u32,
    contiguous: []bool,

    fn init(plan: *Self, blocks: []Block) Allocator.Error!BlockStructure {
        const allocator = plan.allocator;
        const count = blocks.len;
        const succ_starts = try allocator.alloc(u32, count + 1);
        errdefer allocator.free(succ_starts);
        const pred_starts = try allocator.alloc(u32, count + 1);
        errdefer allocator.free(pred_starts);
        @memset(succ_starts, 0);
        @memset(pred_starts, 0);
        for (blocks, 0..) |block, index| {
            const last_node = plan.lastNodeOf(block);
            var edge_index = plan.nodes.items[last_node].succ;
            while (edge_index != none) : (edge_index = plan.edges.items[edge_index].next_succ) {
                const target = plan.nodes.items[plan.edges.items[edge_index].to].block;
                succ_starts[index + 1] += 1;
                pred_starts[target + 1] += 1;
            }
        }
        for (1..count + 1) |index| {
            succ_starts[index] += succ_starts[index - 1];
            pred_starts[index] += pred_starts[index - 1];
        }
        const succs = try allocator.alloc(u32, succ_starts[count]);
        errdefer allocator.free(succs);
        const preds = try allocator.alloc(u32, pred_starts[count]);
        errdefer allocator.free(preds);
        {
            const succ_fill = try allocator.dupe(u32, succ_starts[0..count]);
            defer allocator.free(succ_fill);
            const pred_fill = try allocator.dupe(u32, pred_starts[0..count]);
            defer allocator.free(pred_fill);
            for (blocks, 0..) |block, index| {
                const last_node = plan.lastNodeOf(block);
                var edge_index = plan.nodes.items[last_node].succ;
                while (edge_index != none) : (edge_index = plan.edges.items[edge_index].next_succ) {
                    const target = plan.nodes.items[plan.edges.items[edge_index].to].block;
                    succs[succ_fill[index]] = target;
                    succ_fill[index] += 1;
                    preds[pred_fill[target]] = @intCast(index);
                    pred_fill[target] += 1;
                }
            }
        }
        var forest = try LoopForest.build(allocator, succ_starts, succs, pred_starts, preds);
        errdefer forest.deinit();
        const loop_count = forest.loops.len;

        // Side entries, by loop.
        const SideEntry = struct { loop: u32, pred: u32 };
        var side: std.ArrayList(SideEntry) = .empty;
        defer side.deinit(allocator);
        for (0..count) |target_index| {
            for (preds[pred_starts[target_index]..pred_starts[target_index + 1]]) |pred| {
                var loop = forest.innermost[target_index];
                while (loop != LoopForest.none and !forest.containsNode(loop, pred)) : (loop = forest.loops[loop].parent) {
                    if (forest.loops[loop].header != target_index) try side.append(allocator, .{ .loop = loop, .pred = pred });
                }
            }
        }
        std.mem.sort(SideEntry, side.items, {}, struct {
            fn lessThan(_: void, lhs: SideEntry, rhs: SideEntry) bool {
                return lhs.loop < rhs.loop;
            }
        }.lessThan);
        var entry_list: std.ArrayList(u32) = .empty;
        defer entry_list.deinit(allocator);
        const entry_starts = try allocator.alloc(u32, loop_count + 1);
        errdefer allocator.free(entry_starts);
        {
            var cursor: usize = 0;
            for (0..loop_count) |loop| {
                entry_starts[loop] = @intCast(entry_list.items.len);
                const header = forest.loops[loop].header;
                for (preds[pred_starts[header]..pred_starts[header + 1]]) |pred| {
                    if (!forest.containsNode(@intCast(loop), pred)) try entry_list.append(allocator, pred);
                }
                while (cursor < side.items.len and side.items[cursor].loop == loop) : (cursor += 1) {
                    try entry_list.append(allocator, side.items[cursor].pred);
                }
            }
            entry_starts[loop_count] = @intCast(entry_list.items.len);
        }
        const entries = try entry_list.toOwnedSlice(allocator);
        errdefer allocator.free(entries);

        // Blocks grouped by loop-tree position, and each loop's coverage.
        const blocks_by_loop = try allocator.alloc(u32, count);
        errdefer allocator.free(blocks_by_loop);
        for (blocks_by_loop, 0..) |*slot, index| slot.* = @intCast(index);
        const LoopOrder = struct {
            forest: *const LoopForest,
            fn key(ctx: @This(), block: u32) u64 {
                const loop = ctx.forest.innermost[block];
                const position: u64 = if (loop == LoopForest.none) std.math.maxInt(u32) else ctx.forest.loops[loop].enter;
                return (position << 32) | block;
            }
            fn lessThan(ctx: @This(), lhs: u32, rhs: u32) bool {
                return key(ctx, lhs) < key(ctx, rhs);
            }
        };
        std.mem.sort(u32, blocks_by_loop, LoopOrder{ .forest = &forest }, LoopOrder.lessThan);

        // Lay the blocks out in that order, so every loop's positions are one
        // interval. Lifetimes overlap exactly when they share a node, so the
        // order of blocks does not change which values interfere.
        {
            var position: u32 = 0;
            for (blocks_by_loop) |block_index| {
                const block = &blocks[block_index];
                const start = position;
                var current = block.first;
                while (true) {
                    const n = &plan.nodes.items[current];
                    const next_in_block: ?u32 = if (n.succ_count == 1) blk: {
                        const next = plan.edges.items[n.succ].to;
                        const next_node = plan.nodes.items[next];
                        break :blk if (next_node.block == n.block and next_node.position == n.position + 1) next else null;
                    } else null;
                    n.position = position;
                    position += 1;
                    current = next_in_block orelse break;
                }
                block.start = start;
                block.end = position;
            }
        }
        const first_position = try allocator.alloc(u32, loop_count);
        errdefer allocator.free(first_position);
        const end_position = try allocator.alloc(u32, loop_count);
        errdefer allocator.free(end_position);
        const contiguous = try allocator.alloc(bool, loop_count);
        errdefer allocator.free(contiguous);
        {
            const covered = try allocator.alloc(u64, loop_count);
            defer allocator.free(covered);
            @memset(first_position, std.math.maxInt(u32));
            @memset(end_position, 0);
            @memset(covered, 0);
            for (blocks, 0..) |block, index| {
                const loop = forest.innermost[index];
                if (loop == LoopForest.none) continue;
                first_position[loop] = @min(first_position[loop], block.start);
                end_position[loop] = @max(end_position[loop], block.end);
                covered[loop] += block.end - block.start;
            }
            var order = loop_count;
            while (order > 0) {
                order -= 1;
                const loop = forest.preorder[order];
                const parent = forest.loops[loop].parent;
                if (parent == LoopForest.none) continue;
                first_position[parent] = @min(first_position[parent], first_position[loop]);
                end_position[parent] = @max(end_position[parent], end_position[loop]);
                covered[parent] += covered[loop];
            }
            for (0..loop_count) |loop| contiguous[loop] = covered[loop] == end_position[loop] - first_position[loop];
        }

        return .{
            .succ_starts = succ_starts,
            .succs = succs,
            .pred_starts = pred_starts,
            .preds = preds,
            .forest = forest,
            .entry_starts = entry_starts,
            .entries = entries,
            .blocks_by_loop = blocks_by_loop,
            .first_position = first_position,
            .end_position = end_position,
            .contiguous = contiguous,
        };
    }

    fn deinit(self: *BlockStructure, allocator: Allocator) void {
        allocator.free(self.contiguous);
        allocator.free(self.end_position);
        allocator.free(self.first_position);
        allocator.free(self.blocks_by_loop);
        allocator.free(self.entries);
        allocator.free(self.entry_starts);
        self.forest.deinit();
        allocator.free(self.preds);
        allocator.free(self.pred_starts);
        allocator.free(self.succs);
        allocator.free(self.succ_starts);
    }

    fn blockPreds(self: *const BlockStructure, block: u32) []const u32 {
        return self.preds[self.pred_starts[block]..self.pred_starts[block + 1]];
    }

    fn loopEntries(self: *const BlockStructure, loop: u32) []const u32 {
        return self.entries[self.entry_starts[loop]..self.entry_starts[loop + 1]];
    }

    /// The first index of `blocks_by_loop` whose block's innermost loop is at
    /// or after loop-tree position `order`.
    fn firstBlockFrom(self: *const BlockStructure, order: u32) usize {
        var low: usize = 0;
        var high: usize = self.blocks_by_loop.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            const loop = self.forest.innermost[self.blocks_by_loop[mid]];
            const position = if (loop == LoopForest.none) std.math.maxInt(u32) else self.forest.loops[loop].enter;
            if (position < order) low = mid + 1 else high = mid;
        }
        return low;
    }

    /// Append every position a loop's blocks cover.
    fn appendCoverage(self: *const BlockStructure, allocator: Allocator, loop: u32, blocks: []const Block, ranges: *Ranges) Allocator.Error!void {
        if (self.contiguous[loop]) {
            try ranges.append(allocator, .{ .start = self.first_position[loop], .end = self.end_position[loop] });
            return;
        }
        const run_start = self.firstBlockFrom(self.forest.loops[loop].enter);
        const run_end = self.firstBlockFrom(self.forest.loops[loop].exit + 1);
        for (self.blocks_by_loop[run_start..run_end]) |block| {
            try ranges.append(allocator, .{ .start = blocks[block].start, .end = blocks[block].end });
        }
    }
};

/// Which loops write a value: the sorted loop-tree positions of the loops
/// holding its writes.
const Transparency = struct {
    forest: *const LoopForest,
    write_enters: []const u32,

    fn holdsWrite(self: Transparency, loop: u32) bool {
        const enter = self.forest.loops[loop].enter;
        const exit = self.forest.loops[loop].exit;
        // The first write position at or after `enter`.
        var low: usize = 0;
        var high: usize = self.write_enters.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (self.write_enters[mid] < enter) low = mid + 1 else high = mid;
        }
        return low < self.write_enters.len and self.write_enters[low] <= exit;
    }

    fn transparent(self: Transparency, loop: u32) bool {
        return !self.holdsWrite(loop);
    }

    /// The largest loop around `innermost` (a block's innermost loop) that
    /// holds none of the value's writes, or none.
    fn outermost(self: Transparency, innermost: u32) ?u32 {
        if (innermost == LoopForest.none or self.holdsWrite(innermost)) return null;
        return self.forest.outermostWhile(innermost, self, transparent);
    }
};

fn lastNodeOf(self: *const Self, block: Block) u32 {
    var current = block.first;
    while (true) {
        const n = self.nodes.items[current];
        if (n.succ_count != 1) return current;
        const next = self.edges.items[n.succ].to;
        if (self.nodes.items[next].block != n.block or self.nodes.items[next].position != n.position + 1) return current;
        current = next;
    }
}

/// Solve exact read-before-write lifetimes and assign reusable physical slots.
/// `size == 0` values have externally assigned storage or no represented bytes.
pub fn solve(self: *Self) Allocator.Error!void {
    var blocks: std.ArrayList(Block) = .empty;
    defer blocks.deinit(self.allocator);
    var position: u32 = 0;
    for (self.nodes.items, 0..) |n, i| {
        if (n.block != none) continue;
        if (n.pred_count == 1 and self.nodes.items[self.edges.items[n.pred].from].succ_count == 1) continue;
        try self.appendBlock(&blocks, @intCast(i), &position);
    }
    // A component consisting entirely of a cycle has no distinguished entry.
    for (self.nodes.items, 0..) |n, i| if (n.block == none) {
        try self.appendBlock(&blocks, @intCast(i), &position);
    };
    for (self.accesses.items) |*a| {
        const root = self.representative(a.value);
        if (root != a.value and a.write) {
            // Defining an alias does not overwrite the representative's bytes.
            a.write = false;
            a.read = true;
        }
        a.value = root;
    }
    var structure = try BlockStructure.init(self, blocks.items);
    defer structure.deinit(self.allocator);
    std.mem.sort(Access, self.accesses.items, self, accessLess);
    const BlockState = struct { epoch: u32 = none, last_write: ?u32 = null, propagated: bool = false };
    const states = try self.allocator.alloc(BlockState, blocks.items.len);
    defer self.allocator.free(states);
    @memset(states, .{});
    const loop_epochs = try self.allocator.alloc(u32, structure.forest.loops.len);
    defer self.allocator.free(loop_epochs);
    @memset(loop_epochs, none);
    // A unit of propagation: a block, or a loop that writes the value nowhere
    // and so holds it at every position or at none.
    const Unit = struct { loop: bool, index: u32 };
    var work: std.ArrayList(Unit) = .empty;
    defer work.deinit(self.allocator);
    var write_loops: std.ArrayList(u32) = .empty;
    defer write_loops.deinit(self.allocator);
    var begin: usize = 0;
    while (begin < self.accesses.items.len) {
        const value = self.accesses.items[begin].value;
        var end = begin;
        while (end < self.accesses.items.len and self.accesses.items[end].value == value) : (end += 1) {}
        defer begin = end;
        if (self.values.items[value].size == 0) continue;
        const ranges = &self.values.items[value].ranges;

        // The loop-tree positions of the loops holding the value's writes:
        // a loop is transparent to the value when none lies within it.
        write_loops.clearRetainingCapacity();
        for (self.accesses.items[begin..end]) |a| {
            if (!a.write) continue;
            const loop = structure.forest.innermost[self.nodes.items[a.node].block];
            if (loop != LoopForest.none) try write_loops.append(self.allocator, structure.forest.loops[loop].enter);
        }
        std.mem.sort(u32, write_loops.items, {}, std.sort.asc(u32));
        const transparency = Transparency{ .forest = &structure.forest, .write_enters = write_loops.items };

        var b = begin;
        while (b < end) {
            const block = self.nodes.items[self.accesses.items[b].node].block;
            var e = b;
            while (e < end and self.nodes.items[self.accesses.items[e].node].block == block) : (e += 1) {}
            const state = &states[block];
            state.* = .{ .epoch = value };
            var needed_until: ?u32 = null;
            var cursor = e;
            while (cursor > b) {
                cursor -= 1;
                const pos = self.nodes.items[self.accesses.items[cursor].node].position;
                var reads = self.accesses.items[cursor].read;
                var writes = self.accesses.items[cursor].write;
                while (cursor > b and self.nodes.items[self.accesses.items[cursor - 1].node].position == pos) {
                    cursor -= 1;
                    reads = reads or self.accesses.items[cursor].read;
                    writes = writes or self.accesses.items[cursor].write;
                }
                if (writes) {
                    if (state.last_write == null) state.last_write = pos;
                    try ranges.append(self.allocator, .{ .start = pos, .end = needed_until orelse pos + 1 });
                    needed_until = null;
                }
                if (reads and needed_until == null) needed_until = pos + 1;
            }
            if (needed_until) |limit| {
                try ranges.append(self.allocator, .{ .start = blocks.items[block].start, .end = limit });
                if (transparency.outermost(structure.forest.innermost[block])) |loop| {
                    if (loop_epochs[loop] != value) {
                        loop_epochs[loop] = value;
                        try work.append(self.allocator, .{ .loop = true, .index = loop });
                    }
                } else {
                    try work.append(self.allocator, .{ .loop = false, .index = block });
                }
            }
            b = e;
        }
        while (work.pop()) |unit| {
            const entries = if (unit.loop)
                structure.loopEntries(unit.index)
            else
                structure.blockPreds(unit.index);
            if (unit.loop) try structure.appendCoverage(self.allocator, unit.index, blocks.items, ranges);
            for (entries) |pred| {
                if (transparency.outermost(structure.forest.innermost[pred])) |loop| {
                    if (loop_epochs[loop] == value) continue;
                    loop_epochs[loop] = value;
                    try work.append(self.allocator, .{ .loop = true, .index = loop });
                    continue;
                }
                const state = &states[pred];
                if (state.epoch != value) state.* = .{ .epoch = value };
                if (state.propagated) continue;
                state.propagated = true;
                try ranges.append(self.allocator, .{ .start = state.last_write orelse blocks.items[pred].start, .end = blocks.items[pred].end });
                if (state.last_write == null) try work.append(self.allocator, .{ .loop = false, .index = pred });
            }
        }
        compactRanges(ranges);
    }
    try self.assignSlots();
}
const Busy = struct { end: u32, slot: u32 };
fn busyOrder(_: void, a: Busy, b: Busy) std.math.Order {
    return std.math.order(a.end, b.end);
}
const SizeClass = struct {
    busy: std.PriorityQueue(Busy, void, busyOrder),
    available: std.ArrayList(u32) = .empty,
};
fn valueLess(self: *Self, a: u32, b: u32) bool {
    const x = self.values.items[a].ranges.items()[0].start;
    const y = self.values.items[b].ranges.items()[0].start;
    return if (x == y) a < b else x < y;
}
fn overlaps(a: []const Range, b: []const Range) bool {
    var i: usize = 0;
    var j: usize = 0;
    while (i < a.len and j < b.len) {
        if (a[i].end <= b[j].start) {
            i += 1;
        } else if (b[j].end <= a[i].start) {
            j += 1;
        } else return true;
    }
    return false;
}
fn assignSlots(self: *Self) Allocator.Error!void {
    var order: std.ArrayList(u32) = .empty;
    defer order.deinit(self.allocator);
    for (self.values.items, 0..) |v, i| if (v.ranges.items().len != 0) {
        try order.append(self.allocator, @intCast(i));
    };
    std.mem.sort(u32, order.items, self, valueLess);
    // Sizes are structural keys, not compiler identities.
    var classes = std.AutoHashMap(u32, SizeClass).init(self.allocator);
    defer {
        var it = classes.valueIterator();
        while (it.next()) |c| {
            c.busy.deinit(self.allocator);
            c.available.deinit(self.allocator);
        }
        classes.deinit();
    }
    for (order.items) |index| {
        const value = &self.values.items[index];
        const ranges = value.ranges.items();
        const start = ranges[0].start;
        const entry = try classes.getOrPut(value.size);
        if (!entry.found_existing) entry.value_ptr.* = .{ .busy = std.PriorityQueue(Busy, void, busyOrder).initContext({}) };
        const class = entry.value_ptr;
        while (class.busy.peek()) |busy| {
            if (busy.end > start) break;
            _ = class.busy.pop();
            try class.available.append(self.allocator, busy.slot);
        }
        var chosen: ?u32 = null;
        var i = class.available.items.len;
        while (i > 0) {
            i -= 1;
            const slot_index = class.available.items[i];
            const slot = &self.slots.items[slot_index];
            while (slot.cursor < slot.ranges.items().len and slot.ranges.items()[slot.cursor].end <= start) slot.cursor += 1;
            const remaining = slot.ranges.items()[slot.cursor..];
            if (remaining.len > 0 and remaining[0].start <= start) {
                _ = class.available.swapRemove(i);
                try class.busy.push(self.allocator, .{ .end = remaining[0].end, .slot = slot_index });
                continue;
            }
            if (overlaps(remaining, ranges)) continue;
            chosen = slot_index;
            _ = class.available.swapRemove(i);
            // Retire past intervals before merging future reservations.
            std.mem.copyForwards(Range, slot.ranges.mutableItems()[0..remaining.len], remaining);
            slot.ranges.truncate(remaining.len);
            slot.cursor = 0;
            break;
        }
        const slot_index = chosen orelse blk: {
            const next: u32 = @intCast(self.slots.items.len);
            try self.slots.append(self.allocator, .{ .size = value.size });
            break :blk next;
        };
        const slot = &self.slots.items[slot_index];
        try slot.ranges.appendSlice(self.allocator, ranges);
        compactRanges(&slot.ranges);
        value.slot = slot_index;
        try class.busy.push(self.allocator, .{ .end = slot.ranges.items()[0].end, .slot = slot_index });
    }
}

fn testLocal(index: u32) lir.LocalId {
    return @enumFromInt(index);
}

// The oracle deliberately uses dense statement-level fixed-point equations,
// independent of the planner's block intervals and sparse backward traversal.
test "stack plan matches exact liveness on generated typed scalar graphs" {
    var rng = std.Random.DefaultPrng.init(0x11448);
    const random = rng.random();
    for (0..100) |_| {
        var plan = init(std.testing.allocator);
        defer plan.deinit();
        const count = 20;
        const locals_count = 7;
        var reads = [_]u8{0} ** count;
        var writes = [_]u8{0} ** count;
        var successors = [_]u32{0} ** count;
        for (0..count) |_| _ = try plan.node();
        for (0..locals_count) |i| {
            try plan.access(0, testLocal(@intCast(i)), false, true);
            writes[0] |= @as(u8, 1) << @intCast(i);
            plan.values.items[i].size = 8;
        }
        for (1..count) |i| {
            const a = random.uintLessThan(u3, locals_count);
            const b = random.uintLessThan(u3, locals_count);
            const result = random.uintLessThan(u3, locals_count);
            reads[i] = (@as(u8, 1) << a) | (@as(u8, 1) << b);
            writes[i] = @as(u8, 1) << result;
            try plan.access(@intCast(i), testLocal(a), true, false);
            try plan.access(@intCast(i), testLocal(b), true, false);
            try plan.access(@intCast(i), testLocal(result), false, true);
        }
        for (0..count - 1) |i| {
            try plan.edge(@intCast(i), @intCast(i + 1));
            successors[i] |= @as(u32, 1) << @intCast(i + 1);
            if (i > 0 and random.boolean()) {
                const next = 1 + random.uintLessThan(u32, count - 1);
                try plan.edge(@intCast(i), next);
                successors[i] |= @as(u32, 1) << @intCast(next);
            }
        }
        var live_in = [_]u8{0} ** count;
        var live_out = [_]u8{0} ** count;
        var changed = true;
        while (changed) {
            changed = false;
            for (0..count) |i| {
                var out: u8 = 0;
                for (0..count) |j| if (successors[i] & (@as(u32, 1) << @intCast(j)) != 0) {
                    out |= live_in[j];
                };
                const in = reads[i] | (out & ~writes[i]);
                changed = changed or in != live_in[i] or out != live_out[i];
                live_in[i] = in;
                live_out[i] = out;
            }
        }
        try plan.solve();
        for (0..count) |i| {
            const occupied = live_in[i] | live_out[i] | writes[i];
            for (0..locals_count) |v| {
                var planned_live = false;
                for (plan.values.items[v].ranges.items()) |range| {
                    const pos = plan.nodes.items[i].position;
                    planned_live = planned_live or (range.start <= pos and pos < range.end);
                }
                const expected_live = occupied & (@as(u8, 1) << @intCast(v)) != 0;
                try std.testing.expectEqual(expected_live, planned_live);
                if (!expected_live) continue;
                for (v + 1..locals_count) |w| {
                    if (occupied & (@as(u8, 1) << @intCast(w)) != 0) {
                        try std.testing.expect(plan.values.items[v].slot != plan.values.items[w].slot);
                    }
                }
            }
        }
    }
}

test "stack plan reuses mutually exclusive branch storage" {
    var plan = init(std.testing.allocator);
    defer plan.deinit();
    for (0..5) |_| _ = try plan.node();
    try plan.edge(0, 1);
    try plan.edge(0, 3);
    try plan.edge(1, 2);
    try plan.edge(3, 4);
    try plan.access(1, testLocal(0), false, true);
    try plan.access(2, testLocal(0), true, false);
    try plan.access(3, testLocal(1), false, true);
    try plan.access(4, testLocal(1), true, false);
    for (plan.values.items) |*v| v.size = 32;
    try plan.solve();
    try std.testing.expectEqual(@as(usize, 1), plan.slots.items.len);
}

test "stack plan keeps alias storage live through the final alias read" {
    var plan = init(std.testing.allocator);
    defer plan.deinit();
    for (0..5) |_| _ = try plan.node();
    for (0..4) |i| try plan.edge(@intCast(i), @intCast(i + 1));
    try plan.access(0, testLocal(0), false, true);
    try plan.access(1, testLocal(0), true, false);
    try plan.access(1, testLocal(1), false, true);
    try plan.access(2, testLocal(2), false, true);
    try plan.access(3, testLocal(1), true, false);
    try plan.access(4, testLocal(2), true, false);
    for (plan.values.items) |*v| v.size = 8;
    plan.values.items[1].representative = 0;
    try plan.solve();
    try std.testing.expect(plan.values.items[0].slot != plan.values.items[2].slot);
}

test "stack plan uses linear interval storage for a wide straight-line block" {
    var plan = init(std.testing.allocator);
    defer plan.deinit();
    const count = 10000;
    for (0..count + 1) |_| _ = try plan.node();
    for (0..count) |i| {
        try plan.edge(@intCast(i), @intCast(i + 1));
        try plan.access(@intCast(i), testLocal(@intCast(i)), false, true);
        try plan.access(count, testLocal(@intCast(i)), true, false);
        plan.values.items[i].size = 8;
    }
    try plan.solve();
    try std.testing.expectEqual(@as(usize, count), plan.slots.items.len);
    for (plan.values.items) |v| try std.testing.expectEqual(@as(usize, 1), v.ranges.items().len);
}

fn checkLifetimeHoles(allocator: Allocator) (Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    var plan = init(allocator);
    defer plan.deinit();
    for (0..9) |_| _ = try plan.node();
    for (0..8) |i| try plan.edge(@intCast(i), @intCast(i + 1));
    // A occupies [0,2) and [6,8). B fits inside its hole. C starts in
    // the same hole but remains live during A's second occupied interval.
    try plan.access(0, testLocal(0), false, true);
    try plan.access(1, testLocal(0), true, false);
    try plan.access(6, testLocal(0), false, true);
    try plan.access(7, testLocal(0), true, false);
    try plan.access(2, testLocal(1), false, true);
    try plan.access(3, testLocal(1), true, false);
    try plan.access(4, testLocal(2), false, true);
    try plan.access(8, testLocal(2), true, false);
    for (plan.values.items) |*v| v.size = 8;
    try plan.solve();
    try std.testing.expectEqual(@as(usize, 2), plan.slots.items.len);
    try std.testing.expectEqual(plan.values.items[0].slot, plan.values.items[1].slot);
    try std.testing.expect(plan.values.items[0].slot != plan.values.items[2].slot);
}

test "stack plan reuses lifetime holes without overwriting future reservations" {
    try checkLifetimeHoles(std.testing.allocator);
}

test "stack plan releases all storage on allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, checkLifetimeHoles, .{});
}
