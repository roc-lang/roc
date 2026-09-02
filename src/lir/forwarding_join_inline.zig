//! Collapse one-use join continuations that only forward a value unchanged.
//!
//! Direct lowering introduces nested joins while recursively lowering a value
//! producer and its consumer. When an inner join's body does nothing except
//! copy its sole parameter into an outer join's sole parameter, that boundary
//! has no semantic work: the outer body can consume the inner parameter
//! directly. Moving the body inward exposes constructor/destructor pairs to
//! the structural passes without reconstructing source-level intent.

const std = @import("std");
const core = @import("lir_core");
const layout_mod = @import("layout");
const body_clone = @import("body_clone.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = LirStore.GuardedList;
const Allocator = std.mem.Allocator;

/// Allocation failures produced while rewriting forwarding joins.
pub const ResourceError = Allocator.Error;

const Candidate = struct {
    outer_stmt: LIR.CFStmtId,
    outer_param: LIR.LocalId,
    inner_stmt: LIR.CFStmtId,
    inner_param: LIR.LocalId,
    fresh_definitions: []bool,
};

const RetRewriter = struct {
    pub fn cloneRet(_: *RetRewriter, cloner: anytype, value: LIR.LocalId) ResourceError!LIR.CFStmtId {
        return try cloner.store.addCFStmt(.{ .ret = .{ .value = try cloner.mapLocal(value) } });
    }
};

/// Sink eligible one-use forwarding continuations in iterator-fusion scopes.
pub fn run(store: *LirStore, layouts: *const layout_mod.Store) ResourceError!void {
    var join_params = body_clone.JoinParamIndex.init(store.allocator);
    defer join_params.deinit();
    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        if (!store.getProcSpec(proc_id).iterator_fusion_scope) continue;
        var indexed = false;
        while (try findCandidate(store, layouts, proc_id)) |candidate| {
            if (!indexed) {
                try join_params.indexReachable(store, store.getProcSpec(proc_id).body.?);
                indexed = true;
            }
            defer store.allocator.free(candidate.fresh_definitions);
            try applyCandidate(store, layouts, &join_params, proc_id, candidate);
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
            const terminal = if (forwarding == .assign_ref)
                forwarding.assign_ref.next
            else if (forwarding == .set_local)
                forwarding.set_local.next
            else
                unreachable;
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
                .fresh_definitions = undefined,
            };
        }

        // `ReachableStmts` saw the forwarding body's terminal jump as the one
        // incoming edge. A second jump means the outer join has another value
        // source, so sinking its consumer into one predecessor would duplicate
        // or bypass that source.
        if (incoming_count == 1) {
            if (selected) |*candidate| {
                candidate.fresh_definitions = try collectFreshDefinitions(store, outer.body, incoming_edges);
                return candidate.*;
            }
        }
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

fn collectFreshDefinitions(
    store: *LirStore,
    root: LIR.CFStmtId,
    proc_incoming_edges: []const u32,
) ResourceError![]bool {
    const internal_incoming = try store.allocator.alloc(u32, store.cfStmtCount());
    defer store.allocator.free(internal_incoming);
    @memset(internal_incoming, 0);

    var nodes = std.ArrayList(LIR.CFStmtId).empty;
    defer nodes.deinit(store.allocator);
    var successors = std.ArrayList(LIR.CFStmtId).empty;
    defer successors.deinit(store.allocator);
    var walk = try body_clone.ReachableStmts.init(store, root);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        try nodes.append(store.allocator, stmt_id);
        successors.clearRetainingCapacity();
        try body_clone.appendSuccessors(store, &successors, stmt_id);
        for (successors.items) |successor| {
            const count = &internal_incoming[@intFromEnum(successor)];
            if (count.* == std.math.maxInt(u32)) @panic("LIR subtree incoming-edge count overflowed");
            count.* += 1;
        }
    }

    const shared_stmts = try store.allocator.alloc(bool, store.cfStmtCount());
    defer store.allocator.free(shared_stmts);
    @memset(shared_stmts, false);
    var shared_work = std.ArrayList(LIR.CFStmtId).empty;
    defer shared_work.deinit(store.allocator);
    for (nodes.items) |stmt_id| {
        const index = @intFromEnum(stmt_id);
        const expected = internal_incoming[index] + @intFromBool(stmt_id == root);
        if (proc_incoming_edges[index] < expected) @panic("LIR subtree has more internal edges than its procedure graph");
        if (proc_incoming_edges[index] > expected) try shared_work.append(store.allocator, stmt_id);
    }
    while (shared_work.pop()) |stmt_id| {
        const index = @intFromEnum(stmt_id);
        if (shared_stmts[index]) continue;
        shared_stmts[index] = true;
        successors.clearRetainingCapacity();
        try body_clone.appendSuccessors(store, &successors, stmt_id);
        try shared_work.appendSlice(store.allocator, successors.items);
    }

    const shared_definitions = try store.allocator.alloc(bool, store.localCount());
    errdefer store.allocator.free(shared_definitions);
    @memset(shared_definitions, false);
    for (nodes.items) |stmt_id| {
        // A cloned join declaration receives a fresh join-point identity, so
        // its parameter binders must be alpha-renamed with it. Unlike ordinary
        // statement targets, jump initialization is not represented by a
        // structural successor edge to the declaration; retaining a parameter
        // identity can therefore merge two distinct control-flow binders.
        if (store.getCFStmt(stmt_id) == .join or shared_stmts[@intFromEnum(stmt_id)]) {
            body_clone.markStmtDefinitions(store, shared_definitions, stmt_id);
        }
    }
    return shared_definitions;
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

fn applyCandidate(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    join_params: *body_clone.JoinParamIndex,
    proc_id: LIR.LirProcSpecId,
    candidate: Candidate,
) ResourceError!void {
    const outer = store.getCFStmt(candidate.outer_stmt).join;

    var cloner = try body_clone.BodyCloner(RetRewriter).initWithFreshDeclaredJoins(store, .{}, outer.body, join_params);
    defer cloner.deinit();

    // Ordinary definitions in the exclusive prefix are moved, so they keep
    // their identities. Definitions in a structurally shared suffix and
    // parameter binders of alpha-renamed joins receive fresh identities. The
    // eliminated outer parameter is substituted with the inner parameter that
    // carried the same value.
    const frame = store.getLocalSpan(store.getProcSpec(proc_id).frame_locals);
    for (0..frame.len) |index| {
        const local = GuardedList.at(frame, index);
        if (!candidate.fresh_definitions[@intFromEnum(local)]) {
            cloner.local_map[@intFromEnum(local)] = local;
        }
    }
    cloner.local_map[@intFromEnum(candidate.outer_param)] = candidate.inner_param;

    const moved_body = try cloner.cloneStmt(outer.body);
    store.getCFStmtPtr(candidate.inner_stmt).join.body = moved_body;

    const proc = store.getProcSpecPtr(proc_id);
    const old_frame = store.getLocalSpan(proc.frame_locals);
    var merged = try std.ArrayList(LIR.LocalId).initCapacity(store.allocator, old_frame.len + cloner.new_locals.items.len);
    defer merged.deinit(store.allocator);
    for (0..old_frame.len) |index| merged.appendAssumeCapacity(GuardedList.at(old_frame, index));
    merged.appendSliceAssumeCapacity(cloner.new_locals.items);
    std.mem.sort(LIR.LocalId, merged.items, {}, body_clone.localIdLessThan);
    const unique_len = body_clone.uniqueSortedLocals(merged.items);
    proc.frame_locals = try store.addLocalSpan(merged.items[0..unique_len]);
    if (store.procNeedsStackProbe(layouts, proc.*)) proc.stack_probe = .required;

    // All references to the outer join entered through the forwarding body we
    // just replaced, so its remainder is now the complete procedure path.
    store.getCFStmtPtr(candidate.outer_stmt).* = store.getCFStmt(outer.remainder);
}

test "forwarding join inline declarations are referenced" {
    std.testing.refAllDecls(@This());
}

fn testFreshJoinPointId(next_join_point: *u32) LIR.JoinPointId {
    const id: LIR.JoinPointId = @enumFromInt(next_join_point.*);
    next_join_point.* += 1;
    return id;
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
    const copied = try store.addLocal(.{ .layout_idx = .u64 });
    var next_join_point: u32 = 0;
    const outer_id = testFreshJoinPointId(&next_join_point);
    const inner_id = testFreshJoinPointId(&next_join_point);

    const outer_ret = try store.addCFStmt(.{ .ret = .{ .value = copied } });
    const outer_body = try store.addCFStmt(.{ .assign_ref = .{
        .target = copied,
        .op = .{ .local = outer_param },
        .next = outer_ret,
    } });
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
        .frame_locals = try store.addLocalSpan(&.{ outer_param, inner_param, one, copied }),
        .ret_layout = .u64,
    });

    try run(&store, &layouts);

    const rewritten_outer = store.getCFStmt(store.getProcSpec(proc).body.?);
    try testing.expect(rewritten_outer == .join);
    try testing.expectEqual(inner_id, rewritten_outer.join.id);
    const moved = store.getCFStmt(rewritten_outer.join.body);
    try testing.expect(moved == .assign_ref);
    try testing.expectEqual(copied, moved.assign_ref.target);
    try testing.expectEqual(inner_param, moved.assign_ref.op.local);
    const moved_ret = store.getCFStmt(moved.assign_ref.next);
    try testing.expect(moved_ret == .ret);
    try testing.expectEqual(moved.assign_ref.target, moved_ret.ret.value);
    const rewritten_frame = store.getLocalSpan(store.getProcSpec(proc).frame_locals);
    try testing.expectEqual(@as(usize, 4), rewritten_frame.len);
}

test "forwarding join inline freshens definitions only in a shared tail" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();

    const outer_param = try store.addLocal(.{ .layout_idx = .u64 });
    const inner_param = try store.addLocal(.{ .layout_idx = .u64 });
    const one = try store.addLocal(.{ .layout_idx = .u64 });
    const copied = try store.addLocal(.{ .layout_idx = .u64 });
    const shared_result = try store.addLocal(.{ .layout_idx = .u64 });
    var next_join_point: u32 = 0;
    const outer_id = testFreshJoinPointId(&next_join_point);
    const inner_id = testFreshJoinPointId(&next_join_point);

    const shared_ret = try store.addCFStmt(.{ .ret = .{ .value = shared_result } });
    const shared_tail = try store.addCFStmt(.{ .assign_ref = .{
        .target = shared_result,
        .op = .{ .local = one },
        .next = shared_ret,
    } });
    const outer_body = try store.addCFStmt(.{ .assign_ref = .{
        .target = copied,
        .op = .{ .local = outer_param },
        .next = shared_tail,
    } });
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
    const choose = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = one,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = shared_tail }}),
        .default_branch = set_inner,
        .default_is_cold = false,
        .continuation = null,
    } });
    const inner_remainder = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = choose,
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
        .frame_locals = try store.addLocalSpan(&.{ outer_param, inner_param, one, copied, shared_result }),
        .ret_layout = .u64,
    });

    try run(&store, &layouts);

    const rewritten = store.getCFStmt(store.getProcSpec(proc).body.?);
    try testing.expect(rewritten == .join);
    try testing.expectEqual(inner_id, rewritten.join.id);
    const moved_prefix = store.getCFStmt(rewritten.join.body);
    try testing.expect(moved_prefix == .assign_ref);
    try testing.expectEqual(copied, moved_prefix.assign_ref.target);
    try testing.expectEqual(inner_param, moved_prefix.assign_ref.op.local);
    const moved_tail = store.getCFStmt(moved_prefix.assign_ref.next);
    try testing.expect(moved_tail == .assign_ref);
    try testing.expect(moved_tail.assign_ref.target != shared_result);
    const moved_ret = store.getCFStmt(moved_tail.assign_ref.next);
    try testing.expect(moved_ret == .ret);
    try testing.expectEqual(moved_tail.assign_ref.target, moved_ret.ret.value);

    const original_tail = store.getCFStmt(shared_tail);
    try testing.expectEqual(shared_result, original_tail.assign_ref.target);
    const rewritten_frame = store.getLocalSpan(store.getProcSpec(proc).frame_locals);
    try testing.expectEqual(@as(usize, 6), rewritten_frame.len);
    try testing.expectEqual(moved_tail.assign_ref.target, GuardedList.at(rewritten_frame, 5));
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
    var next_join_point: u32 = 0;
    const outer_id = testFreshJoinPointId(&next_join_point);
    const inner_id = testFreshJoinPointId(&next_join_point);

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
    var next_join_point: u32 = 0;
    const outer_id = testFreshJoinPointId(&next_join_point);
    const inner_id = testFreshJoinPointId(&next_join_point);

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
