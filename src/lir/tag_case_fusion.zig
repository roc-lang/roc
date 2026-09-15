//! Fuse a tag-valued join directly into its discriminant consumer.
//!
//! A value producer lowered in continuation-passing style commonly ends in
//! several `assign_tag; jump join` edges, while the join body immediately
//! reads the discriminant and switches. Materializing the union is unnecessary:
//! each producer edge already proves its exact variant. This pass replaces the
//! one union-valued join with one continuation per produced variant and passes
//! only that variant's payload. No branch is predicted or deleted; the same
//! producer control-flow edge selects the same consumer arm explicitly.

const std = @import("std");
const builtin = @import("builtin");
const core = @import("lir_core");
const collections = @import("collections");
const layout_mod = @import("layout");
const body_clone = @import("body_clone.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = LirStore.GuardedList;
const Allocator = std.mem.Allocator;

/// Allocation failures produced while cloning fused tag branches.
pub const ResourceError = Allocator.Error;

const BuildSite = struct {
    stmt: LIR.CFStmtId,
    /// The `jump` that ends this producer edge. Between the constructor and
    /// the jump the edge may release values the arm no longer needs; those
    /// statements are carried over to the redirected edge unchanged.
    edge_jump: LIR.CFStmtId,
    variant_index: u16,
    discriminant: u16,
    payload: ?LIR.LocalId,
};

const Candidate = struct {
    proc: LIR.LirProcSpecId,
    join_stmt: LIR.CFStmtId,
    param: LIR.LocalId,
    matched_value: LIR.LocalId,
    switch_stmt: LIR.CFStmtId,
    builds: std.ArrayList(BuildSite),
    /// The parameter, its linear aliases, and the matched value: every local
    /// through which an arm can release the union.
    union_locals: std.ArrayList(LIR.LocalId),
    /// Joins declared at the head of the union join's body, outermost first,
    /// before the match itself. Lowering a match whose result initializes a
    /// binding declares that binding's continuation here, and every arm
    /// jumps to it. Fusion keeps these declarations enclosing the fused arms
    /// and the producers alike, so those jumps stay in scope.
    wrappers: std.ArrayList(LIR.CFStmtId),
    /// The first statement of the match: the innermost wrapper's remainder,
    /// or the union join's body when nothing wraps it.
    match_start: LIR.CFStmtId,
    complete: bool,

    fn deinit(self: *Candidate, allocator: Allocator) void {
        self.builds.deinit(allocator);
        self.union_locals.deinit(allocator);
        self.wrappers.deinit(allocator);
    }
};

const VariantDest = struct {
    variant_index: u16,
    discriminant: u16,
    payload_param: ?LIR.LocalId,
    join_id: LIR.JoinPointId,
    join_stmt: LIR.CFStmtId,
};

const BranchRewriter = struct {
    param: LIR.LocalId,
    variant_index: u16,
    payload: LIR.LocalId,
    payload_layout: layout_mod.Idx,
    layouts: *const layout_mod.Store,
    /// Locals naming the union inside the arm; releasing one of them releases
    /// this variant's payload, which the fused arm holds directly.
    union_locals: []const LIR.LocalId,
    /// Whether this variant even has a payload local to release. A variant
    /// without one (or whose payload holds nothing refcounted) makes the
    /// release a no-op, and the statement disappears.
    has_payload: bool,

    pub fn cloneRet(_: *BranchRewriter, cloner: anytype, value: LIR.LocalId) ResourceError!LIR.CFStmtId {
        return try cloner.store.addCFStmt(.{ .ret = .{ .value = try cloner.mapLocal(value) } });
    }

    fn namesUnion(self: *const BranchRewriter, local: LIR.LocalId) bool {
        for (self.union_locals) |candidate| {
            if (candidate == local) return true;
        }
        return false;
    }

    /// The helper that releases this variant's payload, or null when the
    /// payload owns nothing.
    fn payloadRelease(self: *const BranchRewriter) ?LIR.RcHelper {
        if (!self.has_payload) return null;
        var layout_idx = self.payload_layout;
        while (self.layouts.getLayout(layout_idx).tag == .closure) {
            layout_idx = self.layouts.getLayout(layout_idx).getClosure().captures_layout_idx;
        }
        const helper = layout_mod.RcHelper{ .op = .decref, .layout_idx = layout_idx };
        if (self.layouts.rcHelperPlan(helper) == .noop) return null;
        return LIR.RcHelper.fromConcrete(helper);
    }

    pub fn interceptStmt(self: *BranchRewriter, cloner: anytype, _: LIR.CFStmtId, stmt: LIR.CFStmt) ResourceError!?LIR.CFStmtId {
        switch (stmt) {
            .decref => |release| {
                if (!self.namesUnion(release.value)) return null;
                const next = try cloner.cloneStmt(release.next);
                const rc = self.payloadRelease() orelse return next;
                return try cloner.store.addCFStmt(.{ .decref = .{
                    .value = self.payload,
                    .rc = rc,
                    .atomicity = release.atomicity,
                    .next = next,
                } });
            },
            .decref_if_initialized => |release| {
                if (!self.namesUnion(release.value)) return null;
                const next = try cloner.cloneStmt(release.next);
                const rc = self.payloadRelease() orelse return next;
                return try cloner.store.addCFStmt(.{ .decref_if_initialized = .{
                    .cond = try cloner.mapLocal(release.cond),
                    .cond_mask = release.cond_mask,
                    .value = self.payload,
                    .rc = rc,
                    .atomicity = release.atomicity,
                    .next = next,
                } });
            },
            .assign_ref => {},
            .init_uninitialized,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .incref,
            .free,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .loop_continue,
            .loop_break,
            .join,
            .jump,
            .ret,
            .crash,
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_reuse_box,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .boxy_tag_match,
            .assign_call_dict,
            => return null,
        }
        const assign = stmt.assign_ref;
        if (assign.op == .tag_payload_struct) {
            const payload = assign.op.tag_payload_struct;
            if (payload.source != self.param) return null;
            std.debug.assert(payload.variant_index == self.variant_index);
            try cloner.local_map.put(assign.target, self.payload);
            return try cloner.cloneStmt(assign.next);
        }
        if (assign.op != .tag_payload) return null;
        const payload = assign.op.tag_payload;
        if (payload.source != self.param) return null;
        std.debug.assert(payload.variant_index == self.variant_index);
        const op: LIR.RefOp = if (self.layouts.getLayout(self.payload_layout).tag == .struct_)
            .{ .field = .{
                .source = self.payload,
                .field_idx = payload.payload_idx,
            } }
        else blk: {
            std.debug.assert(payload.payload_idx == 0);
            break :blk .{ .local = self.payload };
        };
        return try cloner.store.addCFStmt(.{ .assign_ref = .{
            .target = try cloner.mapLocal(assign.target),
            .op = op,
            .next = try cloner.cloneStmt(assign.next),
        } });
    }
};

/// Fuse eligible tag-producing joins with their immediate case analysis.
pub fn run(store: *LirStore, layouts: *const layout_mod.Store) ResourceError!void {
    var join_params = body_clone.JoinParamIndex.init(store.allocator);
    defer join_params.deinit();
    for (0..store.procSpecCount()) |proc_index| {
        const proc: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        var indexed = false;
        while (try findCandidate(store, layouts, proc)) |found| {
            if (!indexed) {
                try join_params.indexReachable(store, store.getProcSpec(proc).body.?);
                indexed = true;
            }
            var candidate = found;
            defer candidate.deinit(store.allocator);
            const fused_id = store.getCFStmt(candidate.join_stmt).join.id;
            try applyCandidate(store, layouts, &join_params, &candidate);
            try debugCheckJumpScopes(store, proc, fused_id);
        }
    }
}

/// Debug-only invariant: after a fusion, every jump in the procedure still
/// targets a join whose declaration encloses it. A fusion moves arms into
/// fresh joins and hoists the match's continuation joins around them, and a
/// jump left outside its declaration's scope would only surface later as an
/// ARC lift failure with no pointer back here.
fn debugCheckJumpScopes(store: *LirStore, proc: LIR.LirProcSpecId, fused_id: LIR.JoinPointId) ResourceError!void {
    if (builtin.mode != .Debug) return;
    const allocator = store.allocator;
    const Item = struct { stmt: LIR.CFStmtId, depth: usize };
    var work = std.ArrayList(Item).empty;
    defer work.deinit(allocator);
    var scope = std.ArrayList(LIR.JoinPointId).empty;
    defer scope.deinit(allocator);
    var successors = std.ArrayList(LIR.CFStmtId).empty;
    defer successors.deinit(allocator);
    var visited = collections.DenseMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();
    try work.append(allocator, .{ .stmt = store.getProcSpec(proc).body orelse return, .depth = 0 });
    while (work.pop()) |item| {
        if ((try visited.getOrPut(item.stmt)).found_existing) continue;
        scope.shrinkRetainingCapacity(item.depth);
        switch (store.getCFStmt(item.stmt)) {
            .join => |join| {
                try scope.append(allocator, join.id);
                try work.append(allocator, .{ .stmt = join.body, .depth = scope.items.len });
                try work.append(allocator, .{ .stmt = join.remainder, .depth = scope.items.len });
            },
            .jump => |jump| {
                var in_scope = false;
                for (scope.items) |id| {
                    if (id == jump.target) in_scope = true;
                }
                if (!in_scope) {
                    std.debug.panic("tag case fusion of j{d} in {s} left a jump to j{d} outside its declaration", .{
                        @intFromEnum(fused_id),
                        store.procDebugName(proc) orelse "an unnamed procedure",
                        @intFromEnum(jump.target),
                    });
                }
            },
            .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .loop_continue,
            .loop_break,
            .ret,
            .crash,
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_reuse_box,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .boxy_tag_match,
            .assign_call_dict,
            => {
                successors.clearRetainingCapacity();
                try body_clone.appendSuccessors(store, &successors, item.stmt);
                for (successors.items) |next| try work.append(allocator, .{ .stmt = next, .depth = item.depth });
            },
        }
    }
}

fn findCandidate(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    proc: LIR.LirProcSpecId,
) ResourceError!?Candidate {
    const body = store.getProcSpec(proc).body orelse return null;
    var walk = try body_clone.ReachableStmts.init(store, body);
    defer walk.deinit();
    while (try walk.next()) |join_stmt| {
        const node = store.getCFStmt(join_stmt);
        if (node != .join) continue;
        const join = node.join;
        const params = store.getLocalSpan(join.params);
        if (params.len != 1 or !join.maybe_uninitialized_params.isEmpty()) continue;
        if (store.getLocalSpan(join.retained).len != 0) continue;
        const param = GuardedList.at(params, 0);
        const param_layout = layouts.getLayout(store.getLocal(param).layout_idx);
        if (param_layout.tag != .tag_union or store.getLocal(param).boxy_desc != null) continue;

        var wrappers = std.ArrayList(LIR.CFStmtId).empty;
        errdefer wrappers.deinit(store.allocator);
        var keep_wrappers = false;
        defer if (!keep_wrappers) wrappers.deinit(store.allocator);
        var match_start = join.body;
        while (store.getCFStmt(match_start) == .join) {
            try wrappers.append(store.allocator, match_start);
            match_start = store.getCFStmt(match_start).join.remainder;
        }

        var matched_value = param;
        var match_stmt = match_start;
        var alias_sources = std.ArrayList(LIR.LocalId).empty;
        defer alias_sources.deinit(store.allocator);
        while (true) {
            const alias_node = store.getCFStmt(match_stmt);
            if (alias_node != .assign_ref or alias_node.assign_ref.op != .local) break;
            if (alias_node.assign_ref.op.local != matched_value) break;
            try alias_sources.append(store.allocator, matched_value);
            matched_value = alias_node.assign_ref.target;
            match_stmt = alias_node.assign_ref.next;
        }

        const match_node = store.getCFStmt(match_stmt);
        const switch_stmt = if (match_node == .assign_ref and match_node.assign_ref.op == .discriminant) blk: {
            const discriminant = match_node.assign_ref;
            if (discriminant.op.discriminant.source != matched_value) continue;
            const switch_node = store.getCFStmt(discriminant.next);
            if (switch_node != .switch_stmt or switch_node.switch_stmt.cond != discriminant.target) continue;
            break :blk discriminant.next;
        } else if (match_node == .switch_stmt and match_node.switch_stmt.cond == matched_value)
            match_stmt
        else
            continue;
        const switch_node = store.getCFStmt(switch_stmt);
        if (switch_node.switch_stmt.continuation != null) continue;

        var union_locals = std.ArrayList(LIR.LocalId).empty;
        errdefer union_locals.deinit(store.allocator);
        try union_locals.append(store.allocator, param);
        for (alias_sources.items) |source| {
            if (source != param) try union_locals.append(store.allocator, source);
        }
        if (matched_value != param) try union_locals.append(store.allocator, matched_value);

        // The consumer may re-enter the match with a new value, jumping back
        // to the union join from an arm or from later code. Such an edge is a
        // producer the union join must still receive, and it lies inside the
        // region the hoisted continuations would enclose, so fusion cannot
        // keep every jump in scope; the join stays as lowered.
        var body_reenters = false;
        {
            var body_walk = try body_clone.ReachableStmts.init(store, join.body);
            defer body_walk.deinit();
            while (try body_walk.next()) |stmt_id| {
                const stmt = store.getCFStmt(stmt_id);
                if (stmt == .jump and stmt.jump.target == join.id) body_reenters = true;
            }
        }
        if (body_reenters) {
            union_locals.deinit(store.allocator);
            continue;
        }

        var join_reads = try body_clone.countReachableReads(store, join.body);
        defer join_reads.deinit();
        var release_reads = try countReleaseReads(store, join.body, union_locals.items);
        defer release_reads.deinit();
        var aliases_are_linear = true;
        for (alias_sources.items) |source| {
            if (join_reads.get(source) != 1 + release_reads.get(source)) {
                aliases_are_linear = false;
                break;
            }
        }
        if (!aliases_are_linear) {
            union_locals.deinit(store.allocator);
            continue;
        }
        if (match_node == .assign_ref and join_reads.get(match_node.assign_ref.target) != 1) {
            union_locals.deinit(store.allocator);
            continue;
        }

        var builds = std.ArrayList(BuildSite).empty;
        errdefer builds.deinit(store.allocator);
        var jump_count: usize = 0;
        var remainder_walk = try body_clone.ReachableStmts.init(store, join.remainder);
        defer remainder_walk.deinit();
        while (try remainder_walk.next()) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .jump and stmt.jump.target == join.id) jump_count += 1;
            if (stmt != .assign_tag) continue;
            const assign = stmt.assign_tag;
            if (assign.target != param or assign.target_desc != null) continue;
            const edge_jump = producerEdgeJump(store, assign.next, param, assign.payload, join.id) orelse continue;
            try builds.append(store.allocator, .{
                .stmt = stmt_id,
                .edge_jump = edge_jump,
                .variant_index = assign.variant_index,
                .discriminant = assign.discriminant,
                .payload = assign.payload,
            });
        }
        if (builds.items.len == 0 or builds.items.len != jump_count) {
            builds.deinit(store.allocator);
            union_locals.deinit(store.allocator);
            continue;
        }
        // A producer edge's statements may also be reached from elsewhere:
        // lowering shares a jump between a constructor's continuation and the
        // body of a join it declared for the same result. Redirecting the
        // constructor's edge leaves that other path arriving at the union
        // join with whatever it holds, so the join must stay for it.
        var shared_edge = false;
        {
            var predecessors = try countStructuralPredecessors(store, join.remainder);
            defer predecessors.deinit();
            for (builds.items) |build| {
                var current = store.getCFStmt(build.stmt).assign_tag.next;
                while (true) {
                    if ((predecessors.get(current) orelse 0) != 1) shared_edge = true;
                    if (current == build.edge_jump) break;
                    current = switch (store.getCFStmt(current)) {
                        .decref => |release| release.next,
                        .incref => |retain| retain.next,
                        .decref_if_initialized => |release| release.next,
                        .init_uninitialized,
                        .assign_ref,
                        .assign_literal,
                        .assign_call,
                        .assign_call_erased,
                        .assign_packed_erased_fn,
                        .assign_low_level,
                        .assign_list,
                        .assign_struct,
                        .assign_tag,
                        .store_struct,
                        .store_tag,
                        .set_local,
                        .debug,
                        .expect,
                        .expect_err,
                        .runtime_error,
                        .comptime_exhaustiveness_failed,
                        .comptime_branch_taken,
                        .free,
                        .switch_stmt,
                        .switch_initialized_payload,
                        .str_match,
                        .str_match_set,
                        .loop_continue,
                        .loop_break,
                        .join,
                        .jump,
                        .ret,
                        .crash,
                        .assign_boxy_desc_ref,
                        .assign_boxy_dict_ref,
                        .assign_boxy_box,
                        .assign_boxy_reuse_box,
                        .assign_boxy_unbox,
                        .assign_boxy_adapt,
                        .assign_boxy_inspect,
                        .assign_boxy_eq,
                        .assign_boxy_tag,
                        .assign_boxy_tag_payload,
                        .boxy_tag_match,
                        .assign_call_dict,
                        => unreachable,
                    };
                }
            }
        }
        if (!payloadPresenceConsistent(builds.items)) {
            builds.deinit(store.allocator);
            union_locals.deinit(store.allocator);
            continue;
        }
        if (!try branchesUseOnlyPayloads(store, layouts, matched_value, param, switch_node.switch_stmt, builds.items, union_locals.items)) {
            builds.deinit(store.allocator);
            union_locals.deinit(store.allocator);
            continue;
        }
        const known_edges_cover = (try classifyJoinTagUses(store, layouts, join.body, matched_value, match_stmt, builds.items, &join_reads, &release_reads)) orelse {
            builds.deinit(store.allocator);
            union_locals.deinit(store.allocator);
            continue;
        };
        const complete = known_edges_cover and !shared_edge;
        keep_wrappers = true;
        return .{
            .proc = proc,
            .join_stmt = join_stmt,
            .param = param,
            .matched_value = matched_value,
            .switch_stmt = switch_stmt,
            .builds = builds,
            .union_locals = union_locals,
            .wrappers = wrappers,
            .match_start = match_start,
            .complete = complete,
        };
    }
    return null;
}

/// Follow a producer edge from the statement after its constructor to the
/// jump into the join. Lowering releases values the arm has finished with
/// between the two, and such releases never touch the union or its payload,
/// so the edge is still an exact producer of one variant. Anything else on
/// the edge makes it opaque.
fn producerEdgeJump(
    store: *const LirStore,
    start: LIR.CFStmtId,
    param: LIR.LocalId,
    payload: ?LIR.LocalId,
    join_id: LIR.JoinPointId,
) ?LIR.CFStmtId {
    var current = start;
    while (true) {
        switch (store.getCFStmt(current)) {
            .jump => |jump| return if (jump.target == join_id) current else null,
            .decref => |release| {
                if (release.value == param or release.value == payload) return null;
                current = release.next;
            },
            .incref => |retain| {
                if (retain.value == param or retain.value == payload) return null;
                current = retain.next;
            },
            .decref_if_initialized => |release| {
                if (release.value == param or release.value == payload) return null;
                current = release.next;
            },
            .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .free,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .loop_continue,
            .loop_break,
            .join,
            .ret,
            .crash,
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_reuse_box,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .boxy_tag_match,
            .assign_call_dict,
            => return null,
        }
    }
}

/// Count, per statement, how many statements in the subtree under `root`
/// continue into it structurally. Jumps do not count: they name joins, and a
/// join reached only by jumps has one structural predecessor, its declarer.
fn countStructuralPredecessors(store: *LirStore, root: LIR.CFStmtId) ResourceError!collections.DenseMap(LIR.CFStmtId, u32) {
    var counts = collections.DenseMap(LIR.CFStmtId, u32).init(store.allocator);
    errdefer counts.deinit();
    var successors = std.ArrayList(LIR.CFStmtId).empty;
    defer successors.deinit(store.allocator);
    var walk = try body_clone.ReachableStmts.init(store, root);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        successors.clearRetainingCapacity();
        try body_clone.appendSuccessors(store, &successors, stmt_id);
        for (successors.items) |next| try counts.put(next, (counts.get(next) orelse 0) + 1);
    }
    return counts;
}

/// Count, per local, the reachable releases of the locals naming the joined
/// union. A release of the union inside an arm becomes a release of that
/// arm's payload once the union is gone, so these reads are compatible with
/// fusion where any other use is not.
fn countReleaseReads(
    store: *LirStore,
    body: LIR.CFStmtId,
    union_locals: []const LIR.LocalId,
) ResourceError!body_clone.ReadCounts {
    var counts = collections.DenseMap(LIR.LocalId, u32).init(store.allocator);
    errdefer counts.deinit();
    var walk = try body_clone.ReachableStmts.init(store, body);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const released: ?LIR.LocalId = switch (store.getCFStmt(stmt_id)) {
            .decref => |release| release.value,
            .decref_if_initialized => |release| release.value,
            .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .incref,
            .free,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .loop_continue,
            .loop_break,
            .join,
            .jump,
            .ret,
            .crash,
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_reuse_box,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .boxy_tag_match,
            .assign_call_dict,
            => null,
        };
        const local = released orelse continue;
        for (union_locals) |candidate| {
            if (candidate == local) try counts.put(local, (counts.get(local) orelse 0) + 1);
        }
    }
    return .{ .counts = counts };
}

/// Prove that every use of the joined tag is the match itself or a valid
/// payload projection in one of its arms. Returns whether the known literal
/// producer edges cover every projected variant. When they do not, fusion is
/// partial: known edges bypass the tag while the original join remains for
/// opaque producers and retains the complete match.
fn classifyJoinTagUses(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    body: LIR.CFStmtId,
    matched_value: LIR.LocalId,
    match_stmt: LIR.CFStmtId,
    builds: []const BuildSite,
    reads: *const body_clone.ReadCounts,
    release_reads: *const body_clone.ReadCounts,
) ResourceError!?bool {
    const tag_layout = layouts.getLayout(store.getLocal(matched_value).layout_idx);
    const info = layouts.getTagUnionInfo(tag_layout);
    var allowed: u32 = 1; // the discriminant read or direct switch condition
    var known: u32 = 1;
    var walk = try body_clone.ReachableStmts.init(store, body);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        if (stmt_id == match_stmt) continue;
        const stmt = store.getCFStmt(stmt_id);
        if (stmt != .assign_ref) continue;
        const op = stmt.assign_ref.op;
        const projection = if (op == .tag_payload_struct)
            if (op.tag_payload_struct.source == matched_value)
                .{ op.tag_payload_struct.variant_index, op.tag_payload_struct.tag_discriminant }
            else
                null
        else if (op == .tag_payload)
            if (op.tag_payload.source == matched_value)
                .{ op.tag_payload.variant_index, op.tag_payload.tag_discriminant }
            else
                null
        else
            null;
        if (projection) |indices| {
            if (indices[0] >= info.variants.len) return null;
            allowed += 1;
            if (buildExists(builds, indices[0], indices[1])) known += 1;
        }
    }
    if (reads.get(matched_value) != allowed + release_reads.get(matched_value)) return null;
    return known == allowed;
}

fn buildExists(builds: []const BuildSite, variant_index: u16, discriminant: u16) bool {
    for (builds) |build| {
        if (build.variant_index == variant_index and build.discriminant == discriminant) return true;
    }
    return false;
}

fn payloadPresenceConsistent(builds: []const BuildSite) bool {
    for (builds, 0..) |build, index| {
        for (builds[0..index]) |previous| {
            if (build.variant_index != previous.variant_index or build.discriminant != previous.discriminant) continue;
            if ((build.payload == null) != (previous.payload == null)) return false;
        }
    }
    return true;
}

fn branchesUseOnlyPayloads(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    matched_value: LIR.LocalId,
    layout_param: LIR.LocalId,
    switch_stmt: @FieldType(LIR.CFStmt, "switch_stmt"),
    builds: []const BuildSite,
    union_locals: []const LIR.LocalId,
) ResourceError!bool {
    for (builds) |build| {
        const payload_layout = variantPayloadLayout(store, layouts, layout_param, build.variant_index) orelse return false;
        if (build.payload) |payload| {
            if (store.getLocal(payload).layout_idx != payload_layout) return false;
        }
        const branch = switchTarget(store, switch_stmt, build.discriminant);
        var reads = try body_clone.countReachableReads(store, branch);
        defer reads.deinit();
        var release_reads = try countReleaseReads(store, branch, union_locals);
        defer release_reads.deinit();

        var allowed: u32 = 0;
        var walk = try body_clone.ReachableStmts.init(store, branch);
        defer walk.deinit();
        while (try walk.next()) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (stmt != .assign_ref) continue;
            const op = stmt.assign_ref.op;
            if (op == .tag_payload_struct) {
                const payload = op.tag_payload_struct;
                if (payload.source == matched_value and
                    payload.variant_index == build.variant_index and
                    payload.tag_discriminant == build.discriminant)
                {
                    allowed += 1;
                }
            } else if (op == .tag_payload) {
                const payload = op.tag_payload;
                if (payload.source == matched_value and
                    payload.variant_index == build.variant_index and
                    payload.tag_discriminant == build.discriminant)
                {
                    if (layouts.getLayout(payload_layout).tag != .struct_ and payload.payload_idx != 0) return false;
                    allowed += 1;
                }
            }
        }
        if (build.payload == null and allowed != 0) return false;
        if (reads.get(matched_value) != allowed + release_reads.get(matched_value)) return false;
    }
    return true;
}

fn variantPayloadLayout(
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    param: LIR.LocalId,
    variant_index: u16,
) ?layout_mod.Idx {
    const tag_layout = layouts.getLayout(store.getLocal(param).layout_idx);
    const info = layouts.getTagUnionInfo(tag_layout);
    if (variant_index >= info.variants.len) return null;
    return info.variants.get(variant_index).payload_layout;
}

fn switchTarget(
    store: *const LirStore,
    switch_stmt: @FieldType(LIR.CFStmt, "switch_stmt"),
    discriminant: u16,
) LIR.CFStmtId {
    const branches = store.getCFSwitchBranches(switch_stmt.branches);
    for (0..branches.len) |index| {
        const branch = GuardedList.at(branches, index);
        if (branch.value == discriminant) return branch.body;
    }
    return switch_stmt.default_branch;
}

fn applyCandidate(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    join_params: *body_clone.JoinParamIndex,
    candidate: *const Candidate,
) ResourceError!void {
    const join = store.getCFStmt(candidate.join_stmt).join;
    const switch_stmt = store.getCFStmt(candidate.switch_stmt).switch_stmt;
    var dests = std.ArrayList(VariantDest).empty;
    defer dests.deinit(store.allocator);
    var cloned_locals = std.ArrayList(LIR.LocalId).empty;
    defer cloned_locals.deinit(store.allocator);
    for (candidate.builds.items) |build| {
        if (findDest(dests.items, build.variant_index, build.discriminant) != null) continue;
        const payload_layout = variantPayloadLayout(store, layouts, candidate.param, build.variant_index) orelse unreachable;
        const payload_param = if (build.payload != null) try store.addLocal(.{ .layout_idx = payload_layout }) else null;
        const join_id: LIR.JoinPointId = @enumFromInt(nextJoinPointRaw(store));
        const params = if (payload_param) |payload| try store.addLocalSpan(&.{payload}) else LIR.LocalSpan.empty();

        // Reserve the destination identity before cloning its branch. The
        // branch cloner obtains fresh identities from the same store, so a
        // merely local reservation would let both allocators choose this id.
        // Body and remainder are replaced below before the rewritten graph is
        // made reachable.
        const join_stmt = try store.addCFStmt(.{ .join = .{
            .id = join_id,
            .params = params,
            .body = join.remainder,
            .remainder = join.remainder,
        } });
        try join_params.record(store.getCFStmt(join_stmt).join);

        const branch = switchTarget(store, switch_stmt, build.discriminant);
        var branch_defs = try body_clone.collectReachableDefinitions(store, branch);
        defer branch_defs.deinit();
        var cloner = try body_clone.BodyCloner(BranchRewriter).initWithFreshDeclaredJoins(store, .{
            .param = candidate.matched_value,
            .variant_index = build.variant_index,
            .payload = payload_param orelse candidate.param,
            .payload_layout = payload_layout,
            .layouts = layouts,
            .union_locals = candidate.union_locals.items,
            .has_payload = payload_param != null,
        }, branch, join_params);
        defer cloner.deinit();
        const frame = store.getLocalSpan(store.getProcSpec(candidate.proc).frame_locals);
        for (0..frame.len) |index| {
            const local = GuardedList.at(frame, index);
            if (branch_defs.get(local) == 0) try cloner.local_map.put(local, local);
        }
        const body = try cloner.cloneStmt(branch);
        try cloned_locals.appendSlice(store.allocator, cloner.new_locals.items);
        store.getCFStmtPtr(join_stmt).join.body = body;
        try dests.append(store.allocator, .{
            .variant_index = build.variant_index,
            .discriminant = build.discriminant,
            .payload_param = payload_param,
            .join_id = join_id,
            .join_stmt = join_stmt,
        });
    }

    for (candidate.builds.items) |build| {
        const dest = findDest(dests.items, build.variant_index, build.discriminant).?;
        const edge = try redirectProducerEdge(store, store.getCFStmt(build.stmt).assign_tag.next, build.edge_jump, dest.join_id);
        if (dest.payload_param) |payload_param| {
            const payload = build.payload orelse unreachable;
            store.getCFStmtPtr(build.stmt).* = .{ .set_local = .{
                .target = payload_param,
                .value = payload,
                .mode = .initialize_join_param,
                .next = edge,
            } };
        } else {
            store.getCFStmtPtr(build.stmt).* = store.getCFStmt(edge);
        }
    }

    // A complete fusion removes the original tag join. A partial fusion keeps
    // an exact copy for producer edges that were not literal constructors in
    // this procedure; known edges have already been redirected around it.
    var replacement = if (candidate.complete)
        join.remainder
    else blk: {
        var kept = join;
        kept.body = candidate.match_start;
        break :blk try store.addCFStmt(.{ .join = kept });
    };
    var index = dests.items.len;
    while (index > 0) {
        index -= 1;
        const dest = dests.items[index];
        store.getCFStmtPtr(dest.join_stmt).join.remainder = replacement;
        replacement = dest.join_stmt;
    }
    if (candidate.wrappers.items.len == 0) {
        store.getCFStmtPtr(candidate.join_stmt).* = store.getCFStmt(replacement);
    } else {
        const innermost = candidate.wrappers.items[candidate.wrappers.items.len - 1];
        store.getCFStmtPtr(innermost).join.remainder = replacement;
        store.getCFStmtPtr(candidate.join_stmt).* = store.getCFStmt(candidate.wrappers.items[0]);
    }

    const proc = store.getProcSpecPtr(candidate.proc);
    var frame = std.ArrayList(LIR.LocalId).empty;
    defer frame.deinit(store.allocator);
    const old_frame = store.getLocalSpan(proc.frame_locals);
    for (0..old_frame.len) |old_index| try frame.append(store.allocator, GuardedList.at(old_frame, old_index));
    for (dests.items) |dest| if (dest.payload_param) |payload| try frame.append(store.allocator, payload);
    try frame.appendSlice(store.allocator, cloned_locals.items);
    std.mem.sort(LIR.LocalId, frame.items, {}, body_clone.localIdLessThan);
    const unique_len = body_clone.uniqueSortedLocals(frame.items);
    proc.frame_locals = try store.addLocalSpan(frame.items[0..unique_len]);
    if (store.procNeedsStackProbe(layouts, proc.*)) proc.stack_probe = .required;
}

/// Copy the releases carried on a producer edge so that the edge ends in
/// a jump to the variant's own join. The original statements are left in
/// place: an edge is only ever redirected through a fresh copy, so a release
/// reachable from elsewhere keeps its old continuation.
fn redirectProducerEdge(
    store: *LirStore,
    start: LIR.CFStmtId,
    edge_jump: LIR.CFStmtId,
    target: LIR.JoinPointId,
) ResourceError!LIR.CFStmtId {
    if (start == edge_jump) return try store.addCFStmt(.{ .jump = .{ .target = target } });
    const stmt = store.getCFStmt(start);
    switch (stmt) {
        .decref => |release| {
            const next = try redirectProducerEdge(store, release.next, edge_jump, target);
            var copy = release;
            copy.next = next;
            return try store.addCFStmt(.{ .decref = copy });
        },
        .incref => |retain| {
            const next = try redirectProducerEdge(store, retain.next, edge_jump, target);
            var copy = retain;
            copy.next = next;
            return try store.addCFStmt(.{ .incref = copy });
        },
        .decref_if_initialized => |release| {
            const next = try redirectProducerEdge(store, release.next, edge_jump, target);
            var copy = release;
            copy.next = next;
            return try store.addCFStmt(.{ .decref_if_initialized = copy });
        },
        .init_uninitialized,
        .assign_ref,
        .assign_literal,
        .assign_call,
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .store_struct,
        .store_tag,
        .set_local,
        .debug,
        .expect,
        .expect_err,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .comptime_branch_taken,
        .free,
        .switch_stmt,
        .switch_initialized_payload,
        .str_match,
        .str_match_set,
        .loop_continue,
        .loop_break,
        .join,
        .jump,
        .ret,
        .crash,
        .assign_boxy_desc_ref,
        .assign_boxy_dict_ref,
        .assign_boxy_box,
        .assign_boxy_reuse_box,
        .assign_boxy_unbox,
        .assign_boxy_adapt,
        .assign_boxy_inspect,
        .assign_boxy_eq,
        .assign_boxy_tag,
        .assign_boxy_tag_payload,
        .boxy_tag_match,
        .assign_call_dict,
        => unreachable,
    }
}

fn findDest(dests: []const VariantDest, variant_index: u16, discriminant: u16) ?VariantDest {
    for (dests) |dest| {
        if (dest.variant_index == variant_index and dest.discriminant == discriminant) return dest;
    }
    return null;
}

fn nextJoinPointRaw(store: *LirStore) u32 {
    var next: u32 = 0;
    for (store.getCFStmts()) |stmt| {
        if (stmt != .join) continue;
        const raw = @intFromEnum(stmt.join.id);
        if (raw == std.math.maxInt(u32)) @panic("join-point id space exhausted");
        next = @max(next, raw + 1);
    }
    return next;
}

test "tag case fusion declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "tag case fusion routes exact constructor edges without materializing tags" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();

    const param = try store.addLocal(.{ .layout_idx = .bool });
    const disc = try store.addLocal(.{ .layout_idx = .u16 });
    const selector = try store.addLocal(.{ .layout_idx = .bool });
    const zero = try store.addLocal(.{ .layout_idx = .u64 });
    const one = try store.addLocal(.{ .layout_idx = .u64 });
    const join_id: LIR.JoinPointId = @enumFromInt(nextJoinPointRaw(&store));

    const ret_zero = try store.addCFStmt(.{ .ret = .{ .value = zero } });
    const branch_zero = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = ret_zero,
    } });
    const ret_one = try store.addCFStmt(.{ .ret = .{ .value = one } });
    const branch_one = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = ret_one,
    } });
    const consume = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = branch_zero }}),
        .default_branch = branch_one,
    } });
    const read_disc = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = param } },
        .next = consume,
    } });

    const jump_zero = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_zero = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = jump_zero,
    } });
    const jump_one = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_one = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = jump_one,
    } });
    const choose = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = selector,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = build_zero }}),
        .default_branch = build_one,
    } });
    const body = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{param}),
        .body = read_disc,
        .remainder = choose,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = try store.addLocalSpan(&.{selector}),
        .iterator_fusion_scope = true,
        .body = body,
        .frame_locals = try store.addLocalSpan(&.{ param, disc, selector, zero, one }),
        .ret_layout = .u64,
    });

    try run(&store, &layouts);

    var walk = try body_clone.ReachableStmts.init(&store, store.getProcSpec(proc).body.?);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        if (stmt == .assign_tag) try testing.expect(stmt.assign_tag.target != param);
        if (stmt == .assign_ref and stmt.assign_ref.target == disc) return error.TestUnexpectedResult;
    }
}

test "tag case fusion renames complete arms with a shared suffix" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();

    const tag_layout = try layouts.putTagUnion(&.{ .zst, .zst, .zst });
    const param = try store.addLocal(.{ .layout_idx = tag_layout });
    const disc = try store.addLocal(.{ .layout_idx = .u16 });
    const selector = try store.addLocal(.{ .layout_idx = .u16 });
    const zero = try store.addLocal(.{ .layout_idx = .u64 });
    const one_prefix = try store.addLocal(.{ .layout_idx = .u64 });
    const two_prefix = try store.addLocal(.{ .layout_idx = .u64 });
    const shared_default = try store.addLocal(.{ .layout_idx = .u64 });
    const join_id: LIR.JoinPointId = @enumFromInt(nextJoinPointRaw(&store));

    const ret_zero = try store.addCFStmt(.{ .ret = .{ .value = zero } });
    const branch_zero = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = ret_zero,
    } });
    const ret_default = try store.addCFStmt(.{ .ret = .{ .value = shared_default } });
    const branch_default = try store.addCFStmt(.{ .assign_literal = .{
        .target = shared_default,
        .value = .{ .i64_literal = .{ .value = 7, .layout_idx = .u64 } },
        .next = ret_default,
    } });
    const branch_one = try store.addCFStmt(.{ .assign_literal = .{
        .target = one_prefix,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = branch_default,
    } });
    const branch_two = try store.addCFStmt(.{ .assign_literal = .{
        .target = two_prefix,
        .value = .{ .i64_literal = .{ .value = 2, .layout_idx = .u64 } },
        .next = branch_default,
    } });
    const consume = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&.{
            .{ .value = 0, .body = branch_zero },
            .{ .value = 1, .body = branch_one },
        }),
        .default_branch = branch_two,
    } });
    const read_disc = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = param } },
        .next = consume,
    } });

    const jump_zero = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_zero = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = jump_zero,
    } });
    const jump_one = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_one = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = jump_one,
    } });
    const jump_two = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_two = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 2,
        .discriminant = 2,
        .payload = null,
        .next = jump_two,
    } });
    const choose = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = selector,
        .branches = try store.addCFSwitchBranches(&.{
            .{ .value = 0, .body = build_zero },
            .{ .value = 1, .body = build_one },
        }),
        .default_branch = build_two,
    } });
    const body = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{param}),
        .body = read_disc,
        .remainder = choose,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = try store.addLocalSpan(&.{selector}),
        .iterator_fusion_scope = true,
        .body = body,
        .frame_locals = try store.addLocalSpan(&.{ param, disc, selector, zero, one_prefix, two_prefix, shared_default }),
        .ret_layout = .u64,
    });

    try run(&store, &layouts);

    var default_targets: [2]LIR.LocalId = undefined;
    var default_count: usize = 0;
    var walk = try body_clone.ReachableStmts.init(&store, store.getProcSpec(proc).body.?);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        if (stmt != .assign_literal or stmt.assign_literal.value != .i64_literal) continue;
        if (stmt.assign_literal.value.i64_literal.value != 7) continue;
        try testing.expect(default_count < default_targets.len);
        default_targets[default_count] = stmt.assign_literal.target;
        default_count += 1;
    }
    try testing.expectEqual(default_targets.len, default_count);
    try testing.expect(default_targets[0] != default_targets[1]);
    try testing.expect(default_targets[0] != shared_default);
    try testing.expect(default_targets[1] != shared_default);
}

test "tag case fusion carries releases on a producer edge" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();

    const tag_layout = try layouts.putTagUnion(&.{ .zst, .zst });
    const param = try store.addLocal(.{ .layout_idx = tag_layout });
    const disc = try store.addLocal(.{ .layout_idx = .u16 });
    const selector = try store.addLocal(.{ .layout_idx = .bool });
    const finished = try store.addLocal(.{ .layout_idx = .str });
    const zero = try store.addLocal(.{ .layout_idx = .u64 });
    const one = try store.addLocal(.{ .layout_idx = .u64 });
    const join_id: LIR.JoinPointId = @enumFromInt(nextJoinPointRaw(&store));

    const ret_zero = try store.addCFStmt(.{ .ret = .{ .value = zero } });
    const branch_zero = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = ret_zero,
    } });
    const ret_one = try store.addCFStmt(.{ .ret = .{ .value = one } });
    const branch_one = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = ret_one,
    } });
    const consume = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = branch_zero }}),
        .default_branch = branch_one,
    } });
    const read_disc = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = param } },
        .next = consume,
    } });

    // The first edge releases a value it has finished with before jumping.
    const jump_zero = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const release = try store.addCFStmt(.{ .decref = .{
        .value = finished,
        .rc = LIR.RcHelper.fromConcrete(.{ .op = .decref, .layout_idx = .str }),
        .next = jump_zero,
    } });
    const build_zero = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = release,
    } });
    const jump_one = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_one = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = jump_one,
    } });
    const choose = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = selector,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = build_zero }}),
        .default_branch = build_one,
    } });
    const body = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{param}),
        .body = read_disc,
        .remainder = choose,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = try store.addLocalSpan(&.{ selector, finished }),
        .body = body,
        .frame_locals = try store.addLocalSpan(&.{ param, disc, selector, finished, zero, one }),
        .ret_layout = .u64,
    });

    try run(&store, &layouts);

    var releases: u32 = 0;
    var walk = try body_clone.ReachableStmts.init(&store, store.getProcSpec(proc).body.?);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        if (stmt == .assign_tag) try testing.expect(stmt.assign_tag.target != param);
        if (stmt == .assign_ref and stmt.assign_ref.target == disc) return error.TestUnexpectedResult;
        if (stmt == .decref and stmt.decref.value == finished) releases += 1;
    }
    try testing.expectEqual(@as(u32, 1), releases);
}

test "tag case fusion releases the payload where an arm released the union" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();

    const tag_layout = try layouts.putTagUnion(&.{ .zst, .str });
    const param = try store.addLocal(.{ .layout_idx = tag_layout });
    const disc = try store.addLocal(.{ .layout_idx = .u16 });
    const selector = try store.addLocal(.{ .layout_idx = .bool });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const taken = try store.addLocal(.{ .layout_idx = .str });
    const zero = try store.addLocal(.{ .layout_idx = .u64 });
    const one = try store.addLocal(.{ .layout_idx = .u64 });
    const join_id: LIR.JoinPointId = @enumFromInt(nextJoinPointRaw(&store));

    // The payload arm reads the payload, then releases the whole union.
    const ret_one = try store.addCFStmt(.{ .ret = .{ .value = one } });
    const release_union = try store.addCFStmt(.{ .decref = .{
        .value = param,
        .rc = LIR.RcHelper.fromConcrete(.{ .op = .decref, .layout_idx = tag_layout }),
        .next = ret_one,
    } });
    const lit_one = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = release_union,
    } });
    const branch_one = try store.addCFStmt(.{ .assign_ref = .{
        .target = taken,
        .op = .{ .tag_payload_struct = .{ .source = param, .variant_index = 1, .tag_discriminant = 1 } },
        .next = lit_one,
    } });
    const ret_zero = try store.addCFStmt(.{ .ret = .{ .value = zero } });
    const branch_zero = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = ret_zero,
    } });
    const consume = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = branch_one }}),
        .default_branch = branch_zero,
    } });
    const read_disc = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = param } },
        .next = consume,
    } });

    const jump_zero = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_zero = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = jump_zero,
    } });
    const jump_one = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_one = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 1,
        .discriminant = 1,
        .payload = text,
        .next = jump_one,
    } });
    const choose = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = selector,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = build_zero }}),
        .default_branch = build_one,
    } });
    const body = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{param}),
        .body = read_disc,
        .remainder = choose,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = try store.addLocalSpan(&.{ selector, text }),
        .body = body,
        .frame_locals = try store.addLocalSpan(&.{ param, disc, selector, text, taken, zero, one }),
        .ret_layout = .u64,
    });

    try run(&store, &layouts);

    var payload_releases: u32 = 0;
    var walk = try body_clone.ReachableStmts.init(&store, store.getProcSpec(proc).body.?);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        if (stmt == .assign_tag) try testing.expect(stmt.assign_tag.target != param);
        if (stmt == .assign_ref and stmt.assign_ref.target == disc) return error.TestUnexpectedResult;
        if (stmt == .decref) {
            try testing.expect(stmt.decref.value != param);
            if (store.getLocal(stmt.decref.value).layout_idx == .str) payload_releases += 1;
        }
    }
    try testing.expectEqual(@as(u32, 1), payload_releases);
}

test "tag case fusion keeps the match's continuation join enclosing the fused arms" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();

    const tag_layout = try layouts.putTagUnion(&.{ .zst, .zst });
    const param = try store.addLocal(.{ .layout_idx = tag_layout });
    const disc = try store.addLocal(.{ .layout_idx = .u16 });
    const selector = try store.addLocal(.{ .layout_idx = .bool });
    const out = try store.addLocal(.{ .layout_idx = .u64 });
    const zero = try store.addLocal(.{ .layout_idx = .u64 });
    const one = try store.addLocal(.{ .layout_idx = .u64 });
    const join_id: LIR.JoinPointId = @enumFromInt(nextJoinPointRaw(&store));
    const cont_id: LIR.JoinPointId = @enumFromInt(@intFromEnum(join_id) + 1);

    // Each arm initializes the match result and jumps to its continuation,
    // which lowering declared at the head of the union join's body.
    const jump_cont_zero = try store.addCFStmt(.{ .jump = .{ .target = cont_id } });
    const set_zero = try store.addCFStmt(.{ .set_local = .{ .target = out, .value = zero, .mode = .initialize_join_param, .next = jump_cont_zero } });
    const branch_zero = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = set_zero,
    } });
    const jump_cont_one = try store.addCFStmt(.{ .jump = .{ .target = cont_id } });
    const set_one = try store.addCFStmt(.{ .set_local = .{ .target = out, .value = one, .mode = .initialize_join_param, .next = jump_cont_one } });
    const branch_one = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = set_one,
    } });
    const consume = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = branch_zero }}),
        .default_branch = branch_one,
    } });
    const read_disc = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = param } },
        .next = consume,
    } });
    const ret_out = try store.addCFStmt(.{ .ret = .{ .value = out } });
    const cont = try store.addCFStmt(.{ .join = .{
        .id = cont_id,
        .params = try store.addLocalSpan(&.{out}),
        .body = ret_out,
        .remainder = read_disc,
    } });

    const jump_zero = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_zero = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = jump_zero,
    } });
    const jump_one = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_one = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = jump_one,
    } });
    const choose = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = selector,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = build_zero }}),
        .default_branch = build_one,
    } });
    const body = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{param}),
        .body = cont,
        .remainder = choose,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = try store.addLocalSpan(&.{selector}),
        .body = body,
        .frame_locals = try store.addLocalSpan(&.{ param, disc, selector, out, zero, one }),
        .ret_layout = .u64,
    });

    try run(&store, &layouts);

    // The continuation is now the outermost declaration, and every jump to it
    // sits inside its remainder.
    const root = store.getCFStmt(store.getProcSpec(proc).body.?);
    try testing.expect(root == .join);
    try testing.expectEqual(cont_id, root.join.id);
    var jumps_to_cont: u32 = 0;
    var walk = try body_clone.ReachableStmts.init(&store, root.join.remainder);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        if (stmt == .assign_tag) try testing.expect(stmt.assign_tag.target != param);
        if (stmt == .assign_ref and stmt.assign_ref.target == disc) return error.TestUnexpectedResult;
        if (stmt == .jump and stmt.jump.target == cont_id) jumps_to_cont += 1;
    }
    try testing.expectEqual(@as(u32, 2), jumps_to_cont);
}

test "tag case fusion keeps the join when a producer edge is shared with another path" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();

    const tag_layout = try layouts.putTagUnion(&.{ .zst, .zst });
    const param = try store.addLocal(.{ .layout_idx = tag_layout });
    const disc = try store.addLocal(.{ .layout_idx = .u16 });
    const selector = try store.addLocal(.{ .layout_idx = .bool });
    const zero = try store.addLocal(.{ .layout_idx = .u64 });
    const one = try store.addLocal(.{ .layout_idx = .u64 });
    const join_id: LIR.JoinPointId = @enumFromInt(nextJoinPointRaw(&store));
    const dead_id: LIR.JoinPointId = @enumFromInt(@intFromEnum(join_id) + 1);

    const ret_zero = try store.addCFStmt(.{ .ret = .{ .value = zero } });
    const branch_zero = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = ret_zero,
    } });
    const ret_one = try store.addCFStmt(.{ .ret = .{ .value = one } });
    const branch_one = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = ret_one,
    } });
    const consume = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = branch_zero }}),
        .default_branch = branch_one,
    } });
    const read_disc = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = param } },
        .next = consume,
    } });

    // One jump statement serves both the second constructor's edge and the
    // body of a join lowering declared for the same result.
    const shared_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_one = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = shared_jump,
    } });
    const dead = try store.addCFStmt(.{ .join = .{
        .id = dead_id,
        .params = try store.addLocalSpan(&.{param}),
        .body = shared_jump,
        .remainder = build_one,
    } });
    const jump_zero = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build_zero = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = jump_zero,
    } });
    const choose = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = selector,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = build_zero }}),
        .default_branch = dead,
    } });
    const body = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{param}),
        .body = read_disc,
        .remainder = choose,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = try store.addLocalSpan(&.{selector}),
        .body = body,
        .frame_locals = try store.addLocalSpan(&.{ param, disc, selector, zero, one }),
        .ret_layout = .u64,
    });

    try run(&store, &layouts);

    // The literal edges bypass the join, but the join itself survives for the
    // shared path, still matching the tag.
    var union_joins: u32 = 0;
    var tag_builds: u32 = 0;
    var walk = try body_clone.ReachableStmts.init(&store, store.getProcSpec(proc).body.?);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        if (stmt == .join and stmt.join.id == join_id) union_joins += 1;
        if (stmt == .assign_tag and stmt.assign_tag.target == param) tag_builds += 1;
    }
    try testing.expectEqual(@as(u32, 1), union_joins);
    try testing.expectEqual(@as(u32, 0), tag_builds);
}
