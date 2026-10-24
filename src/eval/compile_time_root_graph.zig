//! The compile-time producers each compile-time root's evaluation reads
//! (design.md "Dependency-Ordered Compile-Time Evaluation").
//!
//! A root's evaluation runs the procedures its root procedure reaches through
//! calls, static data it builds, and the constructors a read of a list of
//! copies names. The compile-time value slots those procedures read name
//! their producers, so a root depends on the producers of every slot the
//! procedures it reaches read. A procedure packed into an erased callable runs
//! only when something calls that callable, so it joins a root's evaluation
//! exactly when that evaluation makes an erased call: then every callable the
//! evaluation packs, and every callable a value it reads holds, may run. A
//! Boxy dictionary call, comparison, hash, inspection, or adaptation runs a
//! worker procedure its descriptor or dictionary names, so it is an erased
//! call that may run any of the program's Boxy workers.

const std = @import("std");
const lir = @import("lir");
const collections = @import("collections");

const Allocator = std.mem.Allocator;
const LIR = lir.LIR;
const LirStore = lir.LirStore;

/// For each producer, the producers its evaluation reads, in ascending order.
pub const Graph = struct {
    deps: []const []const u32,

    pub fn deinit(self: *Graph, allocator: Allocator) void {
        for (self.deps) |deps| allocator.free(deps);
        allocator.free(self.deps);
        self.* = undefined;
    }
};

/// What running a procedure, and everything it calls, does that matters to
/// scheduling: the producers it reads, the procedures it packs into erased
/// callables, and whether it calls an erased callable.
const Effects = struct {
    reads: std.ArrayList(u32) = .empty,
    packs: std.ArrayList(u32) = .empty,
    erased_call: bool = false,

    fn deinit(self: *Effects, allocator: Allocator) void {
        self.reads.deinit(allocator);
        self.packs.deinit(allocator);
    }

    /// Add `other`'s effects; true when this grew.
    fn add(self: *Effects, allocator: Allocator, other: *const Effects) Allocator.Error!bool {
        var grew = try unionInto(allocator, &self.reads, other.reads.items);
        if (try unionInto(allocator, &self.packs, other.packs.items)) grew = true;
        if (other.erased_call and !self.erased_call) {
            self.erased_call = true;
            grew = true;
        }
        return grew;
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

/// Build the graph for producers whose evaluation starts at `producer_procs`.
/// `slot_producers` names the producer of every compile-time value slot.
pub fn build(
    allocator: Allocator,
    store: *const LirStore,
    producer_procs: []const LIR.LirProcSpecId,
    slot_producers: []const ?usize,
    boxy_worker_procs: []const LIR.LirProcSpecId,
) Allocator.Error!Graph {
    var workers = std.ArrayList(u32).empty;
    defer workers.deinit(allocator);
    for (boxy_worker_procs) |worker| try insertSorted(allocator, &workers, @backingInt(worker));
    const proc_count = store.procSpecCount();

    // Each reachable procedure's own effects and the procedures it calls.
    const own = try allocator.alloc(Effects, proc_count);
    defer {
        for (own) |*effects| effects.deinit(allocator);
        allocator.free(own);
    }
    for (own) |*effects| effects.* = .{};
    const callees = try allocator.alloc(std.ArrayList(u32), proc_count);
    defer {
        for (callees) |*list| list.deinit(allocator);
        allocator.free(callees);
    }
    for (callees) |*list| list.* = .empty;
    const scanned = try allocator.alloc(bool, proc_count);
    defer allocator.free(scanned);
    @memset(scanned, false);
    var pending = std.ArrayList(u32).empty;
    defer pending.deinit(allocator);
    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(allocator);
    var visited = collections.DenseMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();
    for (producer_procs) |proc| try pending.append(allocator, @backingInt(proc));
    try pending.appendSlice(allocator, workers.items);
    while (pending.pop()) |proc_index| {
        if (scanned[proc_index]) continue;
        scanned[proc_index] = true;
        const body = store.getProcSpec(@fromBackingInt(proc_index)).body orelse continue;
        const effects = &own[proc_index];
        work.clearRetainingCapacity();
        visited.clearRetainingCapacity();
        try work.append(allocator, body);
        while (work.pop()) |stmt_id| {
            if ((try visited.getOrPut(stmt_id)).found_existing) continue;
            try lir.BodyClone.appendSuccessors(store, &work, stmt_id, allocator);
            switch (store.getCFStmtImmediate(stmt_id).*) {
                .assign_call => |call| try addCallee(allocator, &callees[proc_index], &pending, call.proc),
                .assign_call_erased => effects.erased_call = true,
                .assign_call_dict, .assign_boxy_eq, .assign_boxy_hash, .assign_boxy_inspect, .assign_boxy_adapt => {
                    effects.erased_call = true;
                    _ = try unionInto(allocator, &effects.packs, workers.items);
                },
                .assign_packed_erased_fn => |pack| {
                    const packed_proc = store.stmtRow(pack.data).proc;
                    try insertSorted(allocator, &effects.packs, @backingInt(packed_proc));
                    try pending.append(allocator, @backingInt(packed_proc));
                },
                .assign_literal => |assign| {
                    if (store.literalExtra(assign.value).fresh_alternative) |constructor| try addCallee(allocator, &callees[proc_index], &pending, constructor);
                    switch (assign.value) {
                        .proc_ref => |referenced| {
                            try insertSorted(allocator, &effects.packs, @backingInt(referenced));
                            try pending.append(allocator, @backingInt(referenced));
                        },
                        .static_data => |read| if (slot_producers[@backingInt(read.id)]) |producer| {
                            try insertSorted(allocator, &effects.reads, @intCast(producer));
                        },
                        .i64_literal, .i128_literal, .f64_literal, .f32_literal, .dec_literal, .str_literal, .boxy_dynamic_num_literal, .boxy_dynamic_frac_literal, .bytes_literal => {},
                    }
                },
                .init_uninitialized, .assign_ref, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_record_update, .assign_boxy_unbox, .assign_boxy_tag, .assign_boxy_tag_payload, .boxy_tag_match, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .incref, .decref, .decref_if_initialized, .free, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .join, .jump, .ret, .crash => {},
            }
        }
    }

    // Close each procedure's effects over its calls: a strongly connected
    // group of procedures has what its members have and what everything it
    // calls has. Tarjan's algorithm finishes a group after everything it calls.
    const closed = try allocator.alloc(?u32, proc_count);
    defer allocator.free(closed);
    @memset(closed, null);
    var groups = std.ArrayList(Effects).empty;
    defer {
        for (groups.items) |*effects| effects.deinit(allocator);
        groups.deinit(allocator);
    }
    try closeOverCalls(allocator, callees, own, scanned, closed, &groups);

    // A producer's evaluation has its root procedure's effects. When it makes
    // an erased call, every callable it packs or a value it reads holds may
    // run too, which can add more; iterate to the fixpoint.
    const totals = try allocator.alloc(Effects, producer_procs.len);
    defer {
        for (totals) |*effects| effects.deinit(allocator);
        allocator.free(totals);
    }
    for (totals, producer_procs) |*total, proc| {
        total.* = .{};
        _ = try total.add(allocator, &groups.items[closed[@backingInt(proc)].?]);
    }
    var changed = true;
    while (changed) {
        changed = false;
        for (totals) |*total| {
            if (!total.erased_call) continue;
            for (total.reads.items) |producer| {
                if (try unionInto(allocator, &total.packs, totals[producer].packs.items)) changed = true;
            }
            var index: usize = 0;
            while (index < total.packs.items.len) : (index += 1) {
                const pack = total.packs.items[index];
                if (try total.add(allocator, &groups.items[closed[pack].?])) changed = true;
            }
        }
    }

    const deps = try allocator.alloc([]const u32, producer_procs.len);
    var built: usize = 0;
    errdefer {
        for (deps[0..built]) |list| allocator.free(list);
        allocator.free(deps);
    }
    for (totals, deps) |*total, *out| {
        out.* = try allocator.dupe(u32, total.reads.items);
        built += 1;
    }
    return .{ .deps = deps };
}

fn addCallee(allocator: Allocator, list: *std.ArrayList(u32), pending: *std.ArrayList(u32), proc: LIR.LirProcSpecId) Allocator.Error!void {
    try list.append(allocator, @backingInt(proc));
    try pending.append(allocator, @backingInt(proc));
}

fn closeOverCalls(
    allocator: Allocator,
    callees: []const std.ArrayList(u32),
    own: []const Effects,
    scanned: []const bool,
    closed: []?u32,
    groups: *std.ArrayList(Effects),
) Allocator.Error!void {
    const unvisited = std.math.maxInt(u32);
    const proc_count = callees.len;
    const index = try allocator.alloc(u32, proc_count);
    defer allocator.free(index);
    @memset(index, unvisited);
    const low = try allocator.alloc(u32, proc_count);
    defer allocator.free(low);
    const on_stack = try allocator.alloc(bool, proc_count);
    defer allocator.free(on_stack);
    @memset(on_stack, false);
    var stack = std.ArrayList(u32).empty;
    defer stack.deinit(allocator);
    const Frame = struct { proc: u32, next: usize };
    var frames = std.ArrayList(Frame).empty;
    defer frames.deinit(allocator);
    var next_index: u32 = 0;
    for (0..proc_count) |start_index| {
        if (!scanned[start_index] or index[start_index] != unvisited) continue;
        const start: u32 = @intCast(start_index);
        index[start] = next_index;
        low[start] = next_index;
        next_index += 1;
        try stack.append(allocator, start);
        on_stack[start] = true;
        try frames.append(allocator, .{ .proc = start, .next = 0 });
        while (frames.items.len != 0) {
            const frame = &frames.items[frames.items.len - 1];
            const proc = frame.proc;
            if (frame.next < callees[proc].items.len) {
                const callee = callees[proc].items[frame.next];
                frame.next += 1;
                if (index[callee] == unvisited) {
                    index[callee] = next_index;
                    low[callee] = next_index;
                    next_index += 1;
                    try stack.append(allocator, callee);
                    on_stack[callee] = true;
                    try frames.append(allocator, .{ .proc = callee, .next = 0 });
                } else if (on_stack[callee]) {
                    low[proc] = @min(low[proc], index[callee]);
                }
                continue;
            }
            _ = frames.pop();
            if (frames.items.len != 0) {
                const parent = frames.items[frames.items.len - 1].proc;
                low[parent] = @min(low[parent], low[proc]);
            }
            if (low[proc] != index[proc]) continue;
            const group: u32 = @intCast(groups.items.len);
            try groups.append(allocator, .{});
            const effects = &groups.items[group];
            var member_start = stack.items.len;
            while (true) {
                member_start -= 1;
                const member = stack.items[member_start];
                on_stack[member] = false;
                closed[member] = group;
                if (member == proc) break;
            }
            for (stack.items[member_start..]) |member| {
                _ = try effects.add(allocator, &own[member]);
                for (callees[member].items) |callee| {
                    const callee_group = closed[callee].?;
                    if (callee_group != group) _ = try effects.add(allocator, &groups.items[callee_group]);
                }
            }
            stack.shrinkRetainingCapacity(member_start);
        }
    }
}

const TestProcs = struct {
    store: LirStore,
    unit: LIR.LocalId,
    next_identity: u32 = 1,

    fn init(allocator: Allocator) Allocator.Error!TestProcs {
        var store = LirStore.init(allocator);
        errdefer store.deinit();
        const unit = try store.addLocal(.{ .layout_idx = .zst });
        return .{ .store = store, .unit = unit };
    }

    fn proc(self: *TestProcs, body: LIR.CFStmtId) Allocator.Error!LIR.LirProcSpecId {
        self.next_identity += 1;
        return try self.store.addProcSpec(.{ .name = self.store.freshSyntheticSymbol(), .identity = LIR.ProcIdentity.forTest(self.next_identity), .args = LIR.LocalSpan.empty(), .body = body, .ret_layout = .zst }, .none);
    }

    fn ret(self: *TestProcs) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .ret = .{ .value = self.unit } }, .test_fixture);
    }
};

/// The static-data slot at `index` of a test's `slot_producers`.
fn testSlot(index: u32) LIR.StaticDataId {
    return @fromBackingInt(index);
}

test "a root depends on the producers of every slot the procedures it calls read" {
    const allocator = std.testing.allocator;
    var procs = try TestProcs.init(allocator);
    defer procs.store.deinit();
    const read = try procs.store.addCFStmt(.{ .assign_literal = .{ .target = procs.unit, .value = .{ .static_data = .{ .id = testSlot(0) } }, .next = try procs.ret() } }, .test_fixture);
    const reader = try procs.proc(read);
    const call = try procs.store.addCFStmt(.{ .assign_call = .{ .target = procs.unit, .proc = reader, .args = LIR.LocalSpan.empty(), .next = try procs.ret() } }, .test_fixture);
    const caller = try procs.proc(call);
    const plain = try procs.proc(try procs.ret());
    var graph = try build(allocator, &procs.store, &.{ caller, plain }, &.{1}, &.{});
    defer graph.deinit(allocator);
    try std.testing.expectEqualSlices(u32, &.{1}, graph.deps[0]);
    try std.testing.expectEqualSlices(u32, &.{}, graph.deps[1]);
}

test "a packed callable's reads join only an evaluation that makes an erased call" {
    const allocator = std.testing.allocator;
    var procs = try TestProcs.init(allocator);
    defer procs.store.deinit();
    const read = try procs.store.addCFStmt(.{ .assign_literal = .{ .target = procs.unit, .value = .{ .static_data = .{ .id = testSlot(0) } }, .next = try procs.ret() } }, .test_fixture);
    const callable = try procs.proc(read);
    const pack = try procs.store.addCFStmt(.{ .assign_packed_erased_fn = .{ .target = procs.unit, .data = try procs.store.addStmtRow(LIR.PackedErasedFnData, .{ .proc = callable, .capture = null, .capture_layout = null, .on_drop = .none }), .next = try procs.ret() } }, .test_fixture);
    const packer = try procs.proc(pack);
    var graph = try build(allocator, &procs.store, &.{packer}, &.{0}, &.{});
    defer graph.deinit(allocator);
    // The packing root only stores the callable; reading its own slot
    // through it would need an erased call.
    try std.testing.expectEqualSlices(u32, &.{}, graph.deps[0]);
}
