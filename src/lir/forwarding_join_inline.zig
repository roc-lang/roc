//! Collapse one-use join continuations that only forward a value unchanged.
//!
//! Direct lowering introduces nested joins while recursively lowering a value
//! producer and its consumer. When an inner join's body does nothing except
//! copy its sole parameter into an outer join's sole parameter, that boundary
//! has no semantic work: the outer body can consume the inner parameter
//! directly. Moving the body inward exposes constructor/destructor pairs to
//! the structural passes without reconstructing source-level intent.

const std = @import("std");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");
const body_clone = @import("body_clone.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = LirStore.GuardedList;
const Allocator = std.mem.Allocator;

pub const ResourceError = Allocator.Error;

const Candidate = struct {
    outer_stmt: LIR.CFStmtId,
    outer_param: LIR.LocalId,
    inner_stmt: LIR.CFStmtId,
    inner_param: LIR.LocalId,
};

const RetRewriter = struct {
    pub fn cloneRet(_: *RetRewriter, cloner: anytype, value: LIR.LocalId) ResourceError!LIR.CFStmtId {
        return try cloner.store.addCFStmt(.{ .ret = .{ .value = try cloner.mapLocal(value) } });
    }
};

pub fn run(store: *LirStore, layouts: *const layout_mod.Store) ResourceError!void {
    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        if (!store.getProcSpec(proc_id).iterator_fusion_scope) continue;
        while (try findCandidate(store, layouts, proc_id)) |candidate| {
            try applyCandidate(store, proc_id, candidate);
        }
    }
}

fn findCandidate(store: *LirStore, layouts: *const layout_mod.Store, proc_id: LIR.LirProcSpecId) ResourceError!?Candidate {
    const proc = store.getProcSpec(proc_id);
    const proc_body = proc.body orelse return null;
    const incoming_edges = try reachableIncomingEdgeCounts(store, proc_body);
    defer store.allocator.free(incoming_edges);

    var walk = try body_clone.ReachableStmts.init(store, proc_body);
    defer walk.deinit();
    while (try walk.next()) |outer_stmt| {
        const outer_node = store.getCFStmt(outer_stmt);
        if (outer_node != .join) continue;
        const outer = outer_node.join;
        const outer_params = store.getLocalSpan(outer.params);
        if (outer_params.len != 1) continue;
        const outer_param = GuardedList.at(outer_params, 0);

        // Before ARC, moving a continuation that consumes an owning value can
        // change the dominance facts ARC uses to certify the join parameter's
        // initialization. The iterator-state forwarding needed here is purely
        // scalar; admit exactly that ownership-neutral subset. A future
        // ownership-aware continuation move can widen this contract by
        // carrying explicit path ownership with the clone.
        const outer_layout = layouts.getLayout(store.getLocal(outer_param).layout_idx);
        if (layouts.layoutContainsRefcounted(outer_layout)) continue;

        // Sinking a recursive join body would move its back edge but remove
        // the declaration that edge targets. Recursive joins are loop
        // headers, not one-shot continuations, even when their initial entry
        // happens to pass through a single forwarding join.
        if (try subtreeJumpsTo(store, outer.body, outer.id)) continue;

        var incoming_count: usize = 0;
        var selected: ?Candidate = null;
        var remainder_walk = try body_clone.ReachableStmts.init(store, outer.remainder);
        defer remainder_walk.deinit();
        while (try remainder_walk.next()) |inner_stmt| {
            const inner_node = store.getCFStmt(inner_stmt);
            if (inner_node == .jump and inner_node.jump.target == outer.id) {
                incoming_count += 1;
                continue;
            }
            if (inner_node != .join) continue;
            const inner = inner_node.join;
            const inner_params = store.getLocalSpan(inner.params);
            if (inner_params.len != 1) continue;
            const inner_param = GuardedList.at(inner_params, 0);
            if (!forwardsToJoin(store, inner.body, inner_param, outer_param, outer.id)) continue;
            const forwarding = store.getCFStmt(inner.body);
            const terminal = switch (forwarding) {
                .assign_ref => |assign| assign.next,
                .set_local => |set| set.next,
                else => unreachable,
            };
            // Replacing only the inner join's body must make the forwarding
            // chain unreachable. LIR permits shared statement tails, so a
            // statement-occurrence count is insufficient: require both nodes
            // to have exactly the one structural predecessor visible here.
            const first_incoming = incoming_edges[@intFromEnum(inner.body)];
            const terminal_incoming = incoming_edges[@intFromEnum(terminal)];
            if (first_incoming != 1 or terminal_incoming != 1) {
                continue;
            }
            selected = .{
                .outer_stmt = outer_stmt,
                .outer_param = outer_param,
                .inner_stmt = inner_stmt,
                .inner_param = inner_param,
            };
        }

        // `ReachableStmts` saw the forwarding body's terminal jump as the one
        // incoming edge. A second jump means the outer join has another value
        // source, so sinking its consumer into one predecessor would duplicate
        // or bypass that source.
        if (incoming_count == 1 and selected != null) return selected;
    }
    return null;
}

fn reachableIncomingEdgeCounts(store: *LirStore, body: LIR.CFStmtId) ResourceError![]u32 {
    const counts = try store.allocator.alloc(u32, store.cfStmtCount());
    errdefer store.allocator.free(counts);
    @memset(counts, 0);
    var successors = std.ArrayList(LIR.CFStmtId).empty;
    defer successors.deinit(store.allocator);
    var walk = try body_clone.ReachableStmts.init(store, body);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        successors.clearRetainingCapacity();
        try body_clone.appendSuccessors(store, &successors, stmt_id);
        for (successors.items) |successor| {
            const count = &counts[@intFromEnum(successor)];
            if (count.* == std.math.maxInt(u32)) @panic("LIR statement incoming-edge count overflowed");
            count.* += 1;
        }
    }
    return counts;
}

fn subtreeJumpsTo(store: *LirStore, body: LIR.CFStmtId, target: LIR.JoinPointId) ResourceError!bool {
    var walk = try body_clone.ReachableStmts.init(store, body);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        if (stmt == .jump and stmt.jump.target == target) return true;
    }
    return false;
}

fn forwardsToJoin(
    store: *const LirStore,
    body: LIR.CFStmtId,
    source: LIR.LocalId,
    target: LIR.LocalId,
    join_id: LIR.JoinPointId,
) bool {
    const first = store.getCFStmt(body);
    if (first == .assign_ref) {
        const assign = first.assign_ref;
        if (assign.target != target or assign.op != .local or assign.op.local != source) return false;
        const terminal = store.getCFStmt(assign.next);
        return terminal == .jump and terminal.jump.target == join_id;
    }
    if (first == .set_local) {
        const set = first.set_local;
        if (set.target != target or set.value != source or set.mode != .initialize_join_param) return false;
        const terminal = store.getCFStmt(set.next);
        return terminal == .jump and terminal.jump.target == join_id;
    }
    return false;
}

fn applyCandidate(store: *LirStore, proc_id: LIR.LirProcSpecId, candidate: Candidate) ResourceError!void {
    const outer = store.getCFStmt(candidate.outer_stmt).join;

    var cloner = try body_clone.BodyCloner(RetRewriter).initWithFreshDeclaredJoins(store, .{}, outer.body);
    defer cloner.deinit();

    // This is a continuation move within one procedure, not a procedure-body
    // clone. Existing locals keep their identities; only the eliminated outer
    // parameter is substituted with the inner parameter that carried the same
    // value.
    const frame = store.getLocalSpan(store.getProcSpec(proc_id).frame_locals);
    for (0..frame.len) |index| {
        const local = GuardedList.at(frame, index);
        cloner.local_map[@intFromEnum(local)] = local;
    }
    cloner.local_map[@intFromEnum(candidate.outer_param)] = candidate.inner_param;

    const moved_body = try cloner.cloneStmt(outer.body);
    store.getCFStmtPtr(candidate.inner_stmt).join.body = moved_body;

    // All references to the outer join entered through the forwarding body we
    // just replaced, so its remainder is now the complete procedure path.
    store.getCFStmtPtr(candidate.outer_stmt).* = store.getCFStmt(outer.remainder);
}

test "forwarding join inline declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "forwarding join inline sinks the sole consumer without duplicating it" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();

    const outer_param = try store.addLocal(.{ .layout_idx = .u64 });
    const inner_param = try store.addLocal(.{ .layout_idx = .u64 });
    const one = try store.addLocal(.{ .layout_idx = .u64 });
    const outer_id: LIR.JoinPointId = @enumFromInt(0);
    const inner_id: LIR.JoinPointId = @enumFromInt(1);

    const outer_body = try store.addCFStmt(.{ .ret = .{ .value = outer_param } });
    const jump_outer = try store.addCFStmt(.{ .jump = .{ .target = outer_id } });
    const forward = try store.addCFStmt(.{ .set_local = .{
        .target = outer_param,
        .value = inner_param,
        .mode = .initialize_join_param,
        .next = jump_outer,
    } });
    const jump_inner = try store.addCFStmt(.{ .jump = .{ .target = inner_id } });
    const set_inner = try store.addCFStmt(.{ .set_local = .{
        .target = inner_param,
        .value = one,
        .mode = .initialize_join_param,
        .next = jump_inner,
    } });
    const inner_remainder = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = set_inner,
    } });
    const inner_stmt = try store.addCFStmt(.{ .join = .{
        .id = inner_id,
        .params = try store.addLocalSpan(&.{inner_param}),
        .body = forward,
        .remainder = inner_remainder,
    } });
    const outer_stmt = try store.addCFStmt(.{ .join = .{
        .id = outer_id,
        .params = try store.addLocalSpan(&.{outer_param}),
        .body = outer_body,
        .remainder = inner_stmt,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = LIR.LocalSpan.empty(),
        .iterator_fusion_scope = true,
        .body = outer_stmt,
        .frame_locals = try store.addLocalSpan(&.{ outer_param, inner_param, one }),
        .ret_layout = .u64,
    });

    try run(&store, &layouts);

    const rewritten_outer = store.getCFStmt(store.getProcSpec(proc).body.?);
    try testing.expect(rewritten_outer == .join);
    try testing.expectEqual(inner_id, rewritten_outer.join.id);
    const moved = store.getCFStmt(rewritten_outer.join.body);
    try testing.expect(moved == .ret);
    try testing.expectEqual(inner_param, moved.ret.value);
}

test "forwarding join inline preserves recursive outer join declarations" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();

    const outer_param = try store.addLocal(.{ .layout_idx = .u64 });
    const inner_param = try store.addLocal(.{ .layout_idx = .u64 });
    const one = try store.addLocal(.{ .layout_idx = .u64 });
    const outer_id: LIR.JoinPointId = @enumFromInt(0);
    const inner_id: LIR.JoinPointId = @enumFromInt(1);

    const outer_back_edge = try store.addCFStmt(.{ .jump = .{ .target = outer_id } });
    const jump_outer = try store.addCFStmt(.{ .jump = .{ .target = outer_id } });
    const forward = try store.addCFStmt(.{ .set_local = .{
        .target = outer_param,
        .value = inner_param,
        .mode = .initialize_join_param,
        .next = jump_outer,
    } });
    const jump_inner = try store.addCFStmt(.{ .jump = .{ .target = inner_id } });
    const set_inner = try store.addCFStmt(.{ .set_local = .{
        .target = inner_param,
        .value = one,
        .mode = .initialize_join_param,
        .next = jump_inner,
    } });
    const inner_remainder = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = set_inner,
    } });
    const inner_stmt = try store.addCFStmt(.{ .join = .{
        .id = inner_id,
        .params = try store.addLocalSpan(&.{inner_param}),
        .body = forward,
        .remainder = inner_remainder,
    } });
    const outer_stmt = try store.addCFStmt(.{ .join = .{
        .id = outer_id,
        .params = try store.addLocalSpan(&.{outer_param}),
        .body = outer_back_edge,
        .remainder = inner_stmt,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = LIR.LocalSpan.empty(),
        .iterator_fusion_scope = true,
        .body = outer_stmt,
        .frame_locals = try store.addLocalSpan(&.{ outer_param, inner_param, one }),
        .ret_layout = .u64,
    });

    try run(&store, &layouts);

    const preserved = store.getCFStmt(store.getProcSpec(proc).body.?);
    try testing.expect(preserved == .join);
    try testing.expectEqual(outer_id, preserved.join.id);
    try testing.expectEqual(outer_back_edge, preserved.join.body);
}

test "forwarding join inline preserves owning continuation parameters" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();

    const outer_param = try store.addLocal(.{ .layout_idx = .str });
    const inner_param = try store.addLocal(.{ .layout_idx = .str });
    const source = try store.addLocal(.{ .layout_idx = .str });
    const outer_id: LIR.JoinPointId = @enumFromInt(0);
    const inner_id: LIR.JoinPointId = @enumFromInt(1);

    const outer_body = try store.addCFStmt(.{ .ret = .{ .value = outer_param } });
    const jump_outer = try store.addCFStmt(.{ .jump = .{ .target = outer_id } });
    const forward = try store.addCFStmt(.{ .set_local = .{
        .target = outer_param,
        .value = inner_param,
        .mode = .initialize_join_param,
        .next = jump_outer,
    } });
    const jump_inner = try store.addCFStmt(.{ .jump = .{ .target = inner_id } });
    const set_inner = try store.addCFStmt(.{ .set_local = .{
        .target = inner_param,
        .value = source,
        .mode = .initialize_join_param,
        .next = jump_inner,
    } });
    const inner_stmt = try store.addCFStmt(.{ .join = .{
        .id = inner_id,
        .params = try store.addLocalSpan(&.{inner_param}),
        .body = forward,
        .remainder = set_inner,
    } });
    const outer_stmt = try store.addCFStmt(.{ .join = .{
        .id = outer_id,
        .params = try store.addLocalSpan(&.{outer_param}),
        .body = outer_body,
        .remainder = inner_stmt,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = try store.addLocalSpan(&.{source}),
        .iterator_fusion_scope = true,
        .body = outer_stmt,
        .frame_locals = try store.addLocalSpan(&.{ outer_param, inner_param, source }),
        .ret_layout = .str,
    });

    try run(&store, &layouts);

    const preserved = store.getCFStmt(store.getProcSpec(proc).body.?);
    try testing.expect(preserved == .join);
    try testing.expectEqual(outer_id, preserved.join.id);
    try testing.expectEqual(outer_body, preserved.join.body);
}
