//! The compile-time producers each compile-time producer's evaluation reads
//! (design.md "Dependency-Ordered Compile-Time Evaluation"), computed from
//! the lifted program before any of its code is lowered.
//!
//! A producer's evaluation runs the functions its own function reaches
//! through direct calls. The compile-time values those functions read name
//! their producers, so a producer depends on the producers of every value the
//! functions it reaches read. A function referenced as a value runs only when
//! something calls a function value, so it joins a producer's evaluation
//! exactly when that evaluation makes such a call: then every function value
//! the evaluation creates, and every one a value it reads holds, may run.
//!
//! The graph over-approximates what any one lowering of the program reads: a
//! value call may reach only some of the function values in scope, and a
//! specialization may drop code the lifted program still has.

const std = @import("std");
const collections = @import("collections");
/// A position in the closure's effect groups.
const EffectGroup = enum(u32) { _ };
const EffectGroupColumn = collections.OptionalIndexColumn(EffectGroup);
const base = @import("base");
const check = @import("check");
const Lifted = @import("monotype_lifted/ast.zig");
const Common = @import("common.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;

/// One producer: the function its evaluation runs and the value it produces.
pub const Producer = struct {
    fn_id: Lifted.FnId,
    module: checked.ModuleId,
    root: Common.ComptimeProducer,
};

/// For each producer, the producers its evaluation reads, in ascending order.
pub const Graph = struct {
    deps: []const []const u32,

    pub fn deinit(self: *Graph, allocator: Allocator) void {
        for (self.deps) |deps| allocator.free(deps);
        allocator.free(self.deps);
        self.* = undefined;
    }
};

/// What running a function, and everything it calls, does that matters to
/// scheduling: the producers it reads, the functions it creates as values,
/// and whether it calls a function value.
const Effects = struct {
    reads: std.ArrayList(u32) = .empty,
    values: std.ArrayList(u32) = .empty,
    value_call: bool = false,

    fn deinit(self: *Effects, allocator: Allocator) void {
        self.reads.deinit(allocator);
        self.values.deinit(allocator);
    }

    /// Add `other`'s effects; true when this grew.
    fn add(self: *Effects, allocator: Allocator, other: *const Effects) Allocator.Error!bool {
        var grew = try unionInto(allocator, &self.reads, other.reads.items);
        if (try unionInto(allocator, &self.values, other.values.items)) grew = true;
        if (other.value_call and !self.value_call) {
            self.value_call = true;
            grew = true;
        }
        return grew;
    }
};

/// A producer's effects while the fixpoint grows them: each list grows by
/// appending, and its set answers membership.
const Total = struct {
    reads: std.ArrayList(u32) = .empty,
    read_set: std.AutoHashMapUnmanaged(u32, void) = .empty,
    /// Per read, how many of that producer's values this has taken.
    taken: std.ArrayList(usize) = .empty,
    values: std.ArrayList(u32) = .empty,
    value_set: std.AutoHashMapUnmanaged(u32, void) = .empty,
    /// The prefix of `values` whose effects this has added.
    expanded: usize = 0,
    value_call: bool = false,

    fn deinit(self: *Total, allocator: Allocator) void {
        self.reads.deinit(allocator);
        self.read_set.deinit(allocator);
        self.taken.deinit(allocator);
        self.values.deinit(allocator);
        self.value_set.deinit(allocator);
    }

    /// Add `effects`; true when this grew.
    fn add(self: *Total, allocator: Allocator, effects: *const Effects) Allocator.Error!bool {
        var grew = false;
        for (effects.reads.items) |producer| {
            if (self.read_set.contains(producer)) continue;
            try self.reads.ensureUnusedCapacity(allocator, 1);
            try self.taken.ensureUnusedCapacity(allocator, 1);
            try self.read_set.put(allocator, producer, {});
            self.reads.appendAssumeCapacity(producer);
            self.taken.appendAssumeCapacity(0);
            grew = true;
        }
        for (effects.values.items) |value| {
            if (try self.addValue(allocator, value)) grew = true;
        }
        if (effects.value_call and !self.value_call) {
            self.value_call = true;
            grew = true;
        }
        return grew;
    }

    fn addValue(self: *Total, allocator: Allocator, value: u32) Allocator.Error!bool {
        try self.values.ensureUnusedCapacity(allocator, 1);
        const entry = try self.value_set.getOrPut(allocator, value);
        if (entry.found_existing) return false;
        self.values.appendAssumeCapacity(value);
        return true;
    }
};

/// Merge sorted `items` into sorted `list`; true when it grew.
fn unionInto(allocator: Allocator, list: *std.ArrayList(u32), items: []const u32) Allocator.Error!bool {
    if (items.len == 0) return false;
    var merged = try std.ArrayList(u32).initCapacity(allocator, list.items.len + items.len);
    errdefer merged.deinit(allocator);
    var left: usize = 0;
    var right: usize = 0;
    while (left < list.items.len or right < items.len) {
        const next = if (right == items.len or (left < list.items.len and list.items[left] <= items[right])) blk: {
            const value = list.items[left];
            left += 1;
            if (right < items.len and items[right] == value) right += 1;
            break :blk value;
        } else blk: {
            const value = items[right];
            right += 1;
            break :blk value;
        };
        merged.appendAssumeCapacity(next);
    }
    const grew = merged.items.len != list.items.len;
    list.deinit(allocator);
    list.* = merged;
    return grew;
}

fn insertSorted(allocator: Allocator, list: *std.ArrayList(u32), value: u32) Allocator.Error!void {
    var index: usize = list.items.len;
    while (index > 0 and list.items[index - 1] > value) index -= 1;
    if (index > 0 and list.items[index - 1] == value) return;
    try list.insert(allocator, index, value);
}

const ProducerKey = struct {
    module: [32]u8,
    tag: u8,
    index: u32,

    fn of(module: checked.ModuleId, root: Common.ComptimeProducer) ProducerKey {
        return switch (root) {
            .checked => |id| .{ .module = module.bytes, .tag = 0, .index = @backingInt(id) },
            .literal => |id| .{ .module = module.bytes, .tag = 1, .index = @backingInt(id) },
        };
    }
};

/// Build the graph for `producers` over `program`. A value a producer's
/// evaluation can read is always one of `producers`; several producers may
/// name one value, and a read names the first of them.
pub fn build(allocator: Allocator, program: *const Lifted.Program, producers: []const Producer) Allocator.Error!Graph {
    var producer_of = std.AutoHashMap(ProducerKey, u32).init(allocator);
    defer producer_of.deinit();
    for (producers, 0..) |producer, index| {
        const entry = try producer_of.getOrPut(ProducerKey.of(producer.module, producer.root));
        if (!entry.found_existing) entry.value_ptr.* = @intCast(index);
    }

    const fn_count = program.fnCount();
    const own = try allocator.alloc(Effects, fn_count);
    defer {
        for (own) |*effects| effects.deinit(allocator);
        allocator.free(own);
    }
    for (own) |*effects| effects.* = .{};
    const callees = try allocator.alloc(std.ArrayList(u32), fn_count);
    defer {
        for (callees) |*list| list.deinit(allocator);
        allocator.free(callees);
    }
    for (callees) |*list| list.* = .empty;
    const scanned = try allocator.alloc(bool, fn_count);
    defer allocator.free(scanned);
    @memset(scanned, false);
    var pending = std.ArrayList(u32).empty;
    defer pending.deinit(allocator);
    for (producers) |producer| try pending.append(allocator, @backingInt(producer.fn_id));

    const Collector = struct {
        allocator: Allocator,
        program: *const Lifted.Program,
        producer_of: *const std.AutoHashMap(ProducerKey, u32),
        effects: *Effects,
        callees: *std.ArrayList(u32),
        pending: *std.ArrayList(u32),

        pub fn enterExpr(self: @This(), expr_id: Lifted.ExprId) Allocator.Error!Lifted.ExprWalk {
            switch (self.program.getExpr(expr_id).data) {
                .call_proc => |call| {
                    const callee: u32 = @backingInt(Lifted.localDirectCallee(call).?);
                    try self.callees.append(self.allocator, callee);
                    try self.pending.append(self.allocator, callee);
                },
                .fn_ref => |value| {
                    const fn_index: u32 = @backingInt(value.fn_id);
                    try insertSorted(self.allocator, &self.effects.values, fn_index);
                    try self.pending.append(self.allocator, fn_index);
                },
                .call_value => self.effects.value_call = true,
                .comptime_value => |value| {
                    // The initializer only gives the read its type; a read
                    // runs none of it.
                    const root = self.program.getComptimeValueRoot(value.root);
                    const producer = self.producer_of.get(ProducerKey.of(root.module, root.root)) orelse
                        base.invariant("compile-time code read a value no producer of the program produces", .{});
                    try insertSorted(self.allocator, &self.effects.reads, producer);
                    return .skip;
                },
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
            }
            return .descend;
        }

        pub fn exitExpr(_: @This(), _: Lifted.ExprId) Allocator.Error!void {}
    };
    while (pending.pop()) |fn_index| {
        if (scanned[fn_index]) continue;
        scanned[fn_index] = true;
        const fn_id: Lifted.FnId = @fromBackingInt(fn_index);
        switch (program.getFn(fn_id).body) {
            .roc => |body| try Lifted.walkExprs(allocator, program, .{ .expr = body }, Collector{
                .allocator = allocator,
                .program = program,
                .producer_of = &producer_of,
                .effects = &own[fn_index],
                .callees = &callees[fn_index],
                .pending = &pending,
            }),
            .hosted => {},
        }
    }

    // Close each function's effects over its calls: a strongly connected
    // group of functions has what its members have and what everything it
    // calls has. Tarjan's algorithm finishes a group after everything it calls.
    var closed = try EffectGroupColumn.initFilled(allocator, fn_count);
    defer closed.deinit(allocator);
    var groups = std.ArrayList(Effects).empty;
    defer {
        for (groups.items) |*effects| effects.deinit(allocator);
        groups.deinit(allocator);
    }
    try closeOverCalls(allocator, callees, own, scanned, &closed, &groups);

    // A producer's evaluation has its function's effects. When it calls a
    // function value, every function value it creates or a value it reads
    // holds may run too, which can add more; iterate to the fixpoint. A
    // total adds each value's effects, and takes each value of a producer it
    // reads, once.
    const totals = try allocator.alloc(Total, producers.len);
    var initialized: usize = 0;
    defer {
        for (totals[0..initialized]) |*total| total.deinit(allocator);
        allocator.free(totals);
    }
    for (totals, producers) |*total, producer| {
        total.* = .{};
        initialized += 1;
        _ = try total.add(allocator, &groups.items[@backingInt(closed.get(@backingInt(producer.fn_id)).?)]);
    }
    var changed = true;
    while (changed) {
        changed = false;
        for (totals) |*total| {
            if (!total.value_call) continue;
            for (0..total.reads.items.len) |read_index| {
                const producer = total.reads.items[read_index];
                // A total can read its own producer, whose values grow as
                // this takes them, so each value is read by position.
                while (total.taken.items[read_index] < totals[producer].values.items.len) {
                    const value = totals[producer].values.items[total.taken.items[read_index]];
                    total.taken.items[read_index] += 1;
                    if (try total.addValue(allocator, value)) changed = true;
                }
            }
            while (total.expanded < total.values.items.len) {
                const value = total.values.items[total.expanded];
                total.expanded += 1;
                if (try total.add(allocator, &groups.items[@backingInt(closed.get(value).?)])) changed = true;
            }
        }
    }

    const deps = try allocator.alloc([]const u32, producers.len);
    var built: usize = 0;
    errdefer {
        for (deps[0..built]) |list| allocator.free(list);
        allocator.free(deps);
    }
    for (totals, deps) |*total, *out| {
        const reads = try allocator.dupe(u32, total.reads.items);
        std.sort.pdq(u32, reads, {}, std.sort.asc(u32));
        out.* = reads;
        built += 1;
    }
    return .{ .deps = deps };
}

fn closeOverCalls(
    allocator: Allocator,
    callees: []const std.ArrayList(u32),
    own: []const Effects,
    scanned: []const bool,
    closed: *EffectGroupColumn,
    groups: *std.ArrayList(Effects),
) Allocator.Error!void {
    // Visit indices are issued from one, so zero marks an unvisited node.
    const unvisited: u32 = 0;
    const fn_count = callees.len;
    const index = try allocator.alloc(u32, fn_count);
    defer allocator.free(index);
    @memset(index, unvisited);
    const low = try allocator.alloc(u32, fn_count);
    defer allocator.free(low);
    const on_stack = try allocator.alloc(bool, fn_count);
    defer allocator.free(on_stack);
    @memset(on_stack, false);
    var stack = std.ArrayList(u32).empty;
    defer stack.deinit(allocator);
    const Frame = struct { node: u32, next: usize };
    var frames = std.ArrayList(Frame).empty;
    defer frames.deinit(allocator);
    var next_index: u32 = 1;
    for (0..fn_count) |start_index| {
        if (!scanned[start_index] or index[start_index] != unvisited) continue;
        const start: u32 = @intCast(start_index);
        index[start] = next_index;
        low[start] = next_index;
        next_index += 1;
        try stack.append(allocator, start);
        on_stack[start] = true;
        try frames.append(allocator, .{ .node = start, .next = 0 });
        while (frames.items.len != 0) {
            const frame = &frames.items[frames.items.len - 1];
            const node = frame.node;
            if (frame.next < callees[node].items.len) {
                const callee = callees[node].items[frame.next];
                frame.next += 1;
                if (index[callee] == unvisited) {
                    index[callee] = next_index;
                    low[callee] = next_index;
                    next_index += 1;
                    try stack.append(allocator, callee);
                    on_stack[callee] = true;
                    try frames.append(allocator, .{ .node = callee, .next = 0 });
                } else if (on_stack[callee]) {
                    low[node] = @min(low[node], index[callee]);
                }
                continue;
            }
            _ = frames.pop();
            if (frames.items.len != 0) {
                const parent = frames.items[frames.items.len - 1].node;
                low[parent] = @min(low[parent], low[node]);
            }
            if (low[node] != index[node]) continue;
            const group: u32 = @intCast(groups.items.len);
            try groups.append(allocator, .{});
            const effects = &groups.items[group];
            var member_start = stack.items.len;
            while (true) {
                member_start -= 1;
                const member = stack.items[member_start];
                on_stack[member] = false;
                closed.set(member, @fromBackingInt(group));
                if (member == node) break;
            }
            for (stack.items[member_start..]) |member| {
                _ = try effects.add(allocator, &own[member]);
                for (callees[member].items) |callee| {
                    const callee_group: u32 = @backingInt(closed.get(callee).?);
                    if (callee_group != group) _ = try effects.add(allocator, &groups.items[callee_group]);
                }
            }
            stack.shrinkRetainingCapacity(member_start);
        }
    }
}
