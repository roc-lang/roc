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

fn variantKey(variant_index: u16, discriminant: u16) u32 {
    return (@as(u32, variant_index) << 16) | discriminant;
}

/// First-producer order is retained even though identity lookup is indexed.
const Variants = struct {
    indices: std.AutoHashMap(u32, usize),
    builds: std.ArrayList(BuildSite) = .empty,
    targets: std.ArrayList(LIR.CFStmtId) = .empty,

    fn init(allocator: Allocator) Variants {
        return .{ .indices = std.AutoHashMap(u32, usize).init(allocator) };
    }

    fn deinit(self: *Variants) void {
        self.builds.deinit(self.indices.allocator);
        self.targets.deinit(self.indices.allocator);
        self.indices.deinit();
    }

    fn add(self: *Variants, build: BuildSite, stats: *WorkStats) ResourceError!bool {
        stats.variant_lookups += 1;
        const entry = try self.indices.getOrPut(variantKey(build.variant_index, build.discriminant));
        if (entry.found_existing) {
            return (self.builds.items[entry.value_ptr.*].payload == null) == (build.payload == null);
        }
        entry.value_ptr.* = self.builds.items.len;
        try self.builds.append(self.indices.allocator, build);
        return true;
    }

    fn resolveTargets(self: *Variants, store: *LirStore, switch_stmt: @FieldType(LIR.CFStmt, "switch_stmt"), stats: *WorkStats) ResourceError!void {
        var targets = std.AutoHashMap(u64, LIR.CFStmtId).init(self.indices.allocator);
        defer targets.deinit();
        const branches = store.getCFSwitchBranches(switch_stmt.branches);
        for (0..branches.len) |index| {
            const branch = GuardedList.at(branches, index);
            stats.variant_lookups += 1;
            const entry = try targets.getOrPut(branch.value);
            if (!entry.found_existing) entry.value_ptr.* = branch.body;
        }
        for (self.builds.items) |build| {
            stats.variant_lookups += 1;
            try self.targets.append(self.indices.allocator, targets.get(build.discriminant) orelse switch_stmt.default_branch);
        }
    }
};

const Candidate = struct {
    proc: LIR.LirProcSpecId,
    join_stmt: LIR.CFStmtId,
    param: LIR.LocalId,
    matched_value: LIR.LocalId,
    builds: std.ArrayList(BuildSite),
    variants: Variants,
    branch_facts: RegionCache,
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
        self.variants.deinit();
        self.branch_facts.deinit();
        self.union_locals.deinit(allocator);
        self.wrappers.deinit(allocator);
    }
};

const VariantDest = struct {
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

/// Deterministic work counts; separate discovery from immutable-region analysis.
pub const WorkStats = struct {
    global_statement_visits: usize = 0,
    discovery_walks: usize = 0,
    discovery_statement_visits: usize = 0,
    inventory_walks: usize = 0,
    inventory_statement_visits: usize = 0,
    definition_walks: usize = 0,
    definition_statement_visits: usize = 0,
    variant_lookups: usize = 0,
    fusions: usize = 0,
};

/// Fuse eligible tag-producing joins with their immediate case analysis.
pub fn run(store: *LirStore, layouts: *const layout_mod.Store) ResourceError!void {
    _ = try runWithStats(store, layouts);
}

/// Run the same fixed point while reporting deterministic work for scaling tests.
pub fn runWithStats(store: *LirStore, layouts: *const layout_mod.Store) ResourceError!WorkStats {
    var stats: WorkStats = .{};
    var analysis = body_clone.AnalysisScratch.init(store.allocator);
    defer analysis.deinit();
    var join_params = body_clone.JoinParamIndex.init(store.allocator);
    defer join_params.deinit();
    // The identity domain includes other procedures and unreachable old clones.
    // Reserve it once; destinations and branch clones share the same allocator.
    join_params.next_join_point = body_clone.firstFreshJoinPoint(store);
    stats.global_statement_visits = store.cfStmtCount();
    for (0..store.procSpecCount()) |proc_index| {
        const proc: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        try runProcWithStats(store, layouts, proc, store.allocator, &join_params, &stats, &analysis);
    }
    return stats;
}

/// Fusion applies to every available body, including erased-ABI procedures.
pub fn rewritableProcBody(store: *const LirStore, proc: LIR.LirProcSpecId) ?LIR.CFStmtId {
    return store.getProcSpec(proc).body;
}

/// Reach the procedure's fixed point using task-owned scratch. The caller must
/// seed the scratch-owned join index above every source join identity before
/// dispatch; only emitted LIR is retained by the store.
pub fn runProc(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    proc: LIR.LirProcSpecId,
    scratch_allocator: Allocator,
    join_params: *body_clone.JoinParamIndex,
) ResourceError!void {
    var analysis = body_clone.AnalysisScratch.init(scratch_allocator);
    defer analysis.deinit();
    try runProcWithScratch(store, layouts, proc, scratch_allocator, join_params, &analysis);
}

/// Keep region inventories independent while reusing empty lane-owned storage.
pub fn runProcWithScratch(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    proc: LIR.LirProcSpecId,
    scratch_allocator: Allocator,
    join_params: *body_clone.JoinParamIndex,
    analysis: *body_clone.AnalysisScratch,
) ResourceError!void {
    var stats: WorkStats = .{};
    try runProcWithStats(store, layouts, proc, scratch_allocator, join_params, &stats, analysis);
}

fn runProcWithStats(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    proc: LIR.LirProcSpecId,
    allocator: Allocator,
    join_params: *body_clone.JoinParamIndex,
    stats: *WorkStats,
    analysis: *body_clone.AnalysisScratch,
) ResourceError!void {
    const body = rewritableProcBody(store, proc) orelse return;
    var indexed = false;
    while (try findCandidate(store, layouts, proc, stats, allocator, analysis)) |found| {
        var candidate = found;
        defer candidate.deinit(allocator);
        if (!indexed) {
            try join_params.indexReachable(store, body);
            indexed = true;
        }
        const fused_id = store.getCFStmt(candidate.join_stmt).join.id;
        try applyCandidate(store, layouts, join_params, &candidate, stats, allocator);
        stats.fusions += 1;
        try debugCheckJumpScopes(store, proc, fused_id, allocator);
    }
}

/// Debug-only invariant: after a fusion, every jump in the procedure still
/// targets a join whose declaration encloses it. A fusion moves arms into
/// fresh joins and hoists the match's continuation joins around them, and a
/// jump left outside its declaration's scope would only surface later as an
/// ARC lift failure with no pointer back here.
fn debugCheckJumpScopes(store: *LirStore, proc: LIR.LirProcSpecId, fused_id: LIR.JoinPointId, allocator: Allocator) ResourceError!void {
    if (builtin.mode != .Debug) return;
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
                try body_clone.appendSuccessorsWithAllocator(store, &successors, item.stmt, allocator);
                for (successors.items) |next| try work.append(allocator, .{ .stmt = next, .depth = item.depth });
            },
        }
    }
}

fn findCandidate(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    proc: LIR.LirProcSpecId,
    stats: *WorkStats,
    allocator: Allocator,
    analysis: *body_clone.AnalysisScratch,
) ResourceError!?Candidate {
    const body = rewritableProcBody(store, proc) orelse return null;
    stats.discovery_walks += 1;
    var walk = try body_clone.ReachableStmts.initWithScratch(store, body, analysis);
    defer walk.deinit();
    while (try walk.next()) |join_stmt| {
        stats.discovery_statement_visits += 1;
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
        var keep_wrappers = false;
        defer if (!keep_wrappers) wrappers.deinit(allocator);
        var match_start = join.body;
        while (store.getCFStmt(match_start) == .join) {
            try wrappers.append(allocator, match_start);
            match_start = store.getCFStmt(match_start).join.remainder;
        }

        var matched_value = param;
        var match_stmt = match_start;
        var alias_sources = std.ArrayList(LIR.LocalId).empty;
        defer alias_sources.deinit(allocator);
        while (true) {
            const alias_node = store.getCFStmt(match_stmt);
            if (alias_node != .assign_ref or alias_node.assign_ref.op != .local) break;
            if (alias_node.assign_ref.op.local != matched_value) break;
            try alias_sources.append(allocator, matched_value);
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
        errdefer union_locals.deinit(allocator);
        try union_locals.append(allocator, param);
        for (alias_sources.items) |source| {
            if (source != param) try union_locals.append(allocator, source);
        }
        if (matched_value != param) try union_locals.append(allocator, matched_value);

        // The consumer may re-enter the match with a new value, jumping back
        // to the union join from an arm or from later code. Such an edge is a
        // producer the union join must still receive, and it lies inside the
        // region the hoisted continuations would enclose, so fusion cannot
        // keep every jump in scope; the join stays as lowered.
        var body_facts = try RegionFacts.init(store, join.body, false, stats, allocator, analysis);
        defer body_facts.deinit();
        if (body_facts.jump_targets.contains(join.id)) {
            union_locals.deinit(allocator);
            continue;
        }

        const join_reads = &body_facts.reads;
        const release_reads = &body_facts.releases;
        var aliases_are_linear = true;
        for (alias_sources.items) |source| {
            if (join_reads.get(source) != 1 + release_reads.get(source)) {
                aliases_are_linear = false;
                break;
            }
        }
        if (!aliases_are_linear) {
            union_locals.deinit(allocator);
            continue;
        }
        if (match_node == .assign_ref and join_reads.get(match_node.assign_ref.target) != 1) {
            union_locals.deinit(allocator);
            continue;
        }

        var builds = std.ArrayList(BuildSite).empty;
        errdefer builds.deinit(allocator);
        var variants = Variants.init(allocator);
        var keep_variants = false;
        defer if (!keep_variants) variants.deinit();
        var consistent_payloads = true;
        var jump_count: usize = 0;
        var predecessors = collections.DenseMap(LIR.CFStmtId, u32).init(allocator);
        defer predecessors.deinit();
        var successors = std.ArrayList(LIR.CFStmtId).empty;
        defer successors.deinit(allocator);
        var remainder_walk = try body_clone.ReachableStmts.initWithScratch(store, join.remainder, analysis);
        defer remainder_walk.deinit();
        stats.inventory_walks += 1;
        while (try remainder_walk.next()) |stmt_id| {
            stats.inventory_statement_visits += 1;
            successors.clearRetainingCapacity();
            try body_clone.appendSuccessorsWithAllocator(store, &successors, stmt_id, allocator);
            for (successors.items) |next| try predecessors.put(next, (predecessors.get(next) orelse 0) + 1);
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .jump and stmt.jump.target == join.id) jump_count += 1;
            if (stmt != .assign_tag) continue;
            const assign = stmt.assign_tag;
            if (assign.target != param or assign.target_desc != null) continue;
            const edge_jump = producerEdgeJump(store, assign.next, param, assign.payload, join.id) orelse continue;
            const build: BuildSite = .{
                .stmt = stmt_id,
                .edge_jump = edge_jump,
                .variant_index = assign.variant_index,
                .discriminant = assign.discriminant,
                .payload = assign.payload,
            };
            try builds.append(allocator, build);
            if (!try variants.add(build, stats)) consistent_payloads = false;
            // Every producer's payload must be checked, including duplicates
            // of a variant whose consumer only needs to be analyzed once.
            const payload_layout = variantPayloadLayout(store, layouts, param, build.variant_index);
            if (payload_layout) |expected| {
                if (build.payload) |payload| {
                    if (store.getLocal(payload).layout_idx != expected) consistent_payloads = false;
                }
            } else consistent_payloads = false;
        }
        if (builds.items.len == 0 or builds.items.len != jump_count) {
            builds.deinit(allocator);
            union_locals.deinit(allocator);
            continue;
        }
        // A producer edge's statements may also be reached from elsewhere:
        // lowering shares a jump between a constructor's continuation and the
        // body of a join it declared for the same result. Redirecting the
        // constructor's edge leaves that other path arriving at the union
        // join with whatever it holds, so the join must stay for it.
        var shared_edge = false;
        {
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
        if (!consistent_payloads) {
            builds.deinit(allocator);
            union_locals.deinit(allocator);
            continue;
        }
        var branch_facts = RegionCache.init(allocator, analysis);
        var keep_branch_facts = false;
        defer if (!keep_branch_facts) branch_facts.deinit();
        try variants.resolveTargets(store, switch_node.switch_stmt, stats);
        if (!try branchesUseOnlyPayloads(store, layouts, matched_value, param, &variants, &branch_facts, stats)) {
            builds.deinit(allocator);
            union_locals.deinit(allocator);
            continue;
        }
        const known_edges_cover = classifyJoinTagUses(store, layouts, &body_facts, matched_value, match_stmt, &variants, stats) orelse {
            builds.deinit(allocator);
            union_locals.deinit(allocator);
            continue;
        };
        const complete = known_edges_cover and !shared_edge;
        keep_wrappers = true;
        keep_variants = true;
        keep_branch_facts = true;
        return .{
            .proc = proc,
            .join_stmt = join_stmt,
            .param = param,
            .matched_value = matched_value,
            .builds = builds,
            .variants = variants,
            .branch_facts = branch_facts,
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
/// An immutable region inventory belongs to one candidate analysis.
/// Reads, releases, projections, and reentry therefore describe the same
/// reachable DAG, with shared suffixes counted once. Branch binders remain
/// valid during append-only cloning; no fact is reused after rewiring.
const RegionFacts = struct {
    reads: body_clone.ReadCounts,
    defs: body_clone.ReadCounts,
    releases: body_clone.ReadCounts,
    projections: std.ArrayList(LIR.CFStmtId) = .empty,
    jump_targets: collections.DenseMap(LIR.JoinPointId, void),

    fn init(store: *LirStore, body: LIR.CFStmtId, comptime include_defs: bool, stats: *WorkStats, allocator: Allocator, analysis: *body_clone.AnalysisScratch) ResourceError!RegionFacts {
        var self: RegionFacts = blk: {
            var reads = try analysis.acquireCounts();
            errdefer reads.deinit();
            const releases = try analysis.acquireCounts();
            break :blk .{
                .reads = reads,
                .defs = .{ .counts = collections.DenseMap(LIR.LocalId, u32).init(allocator) },
                .releases = releases,
                .jump_targets = collections.DenseMap(LIR.JoinPointId, void).init(allocator),
            };
        };
        errdefer self.deinit();
        var walk = try body_clone.ReachableStmts.initWithScratch(store, body, analysis);
        defer walk.deinit();
        stats.inventory_walks += 1;
        var statement_count: usize = 0;
        while (try walk.next()) |stmt_id| {
            stats.inventory_statement_visits += 1;
            statement_count += 1;
            const stmt = store.getCFStmt(stmt_id);
            body_clone.forEachStmtRead(store, stmt, &self.reads, noteRead);
            if (self.reads.failure) |failure| return failure;
            if (stmt == .assign_ref) {
                try self.projections.append(allocator, stmt_id);
            } else if (stmt == .jump) {
                try self.jump_targets.put(stmt.jump.target, {});
            } else if (stmt == .decref) {
                try self.noteRelease(stmt.decref.value);
            } else if (stmt == .decref_if_initialized) {
                try self.noteRelease(stmt.decref_if_initialized.value);
            }
        }
        if (include_defs) {
            // Cloning needs lexical binders, not operand writes (`set_local`
            // writes an outer binder). Keep using the cloner's exact inventory.
            self.defs = try body_clone.collectReachableDefinitionsWithScratch(store, body, analysis);
            stats.definition_walks += 1;
            stats.definition_statement_visits += statement_count;
        }
        return self;
    }

    fn noteRead(reads: *body_clone.ReadCounts, local: LIR.LocalId) void {
        reads.counts.put(local, reads.get(local) + 1) catch |err| {
            reads.failure = err;
        };
    }

    fn noteRelease(self: *RegionFacts, local: LIR.LocalId) ResourceError!void {
        try self.releases.counts.put(local, self.releases.get(local) + 1);
    }

    fn deinit(self: *RegionFacts) void {
        self.projections.deinit(self.jump_targets.allocator);
        self.jump_targets.deinit();
        self.reads.deinit();
        self.defs.deinit();
        self.releases.deinit();
    }
};

/// Different discriminants can select the same default arm. Its reachable
/// reads and definitions are invariant, even when the projection proof differs.
const RegionCache = struct {
    regions: collections.DenseMap(LIR.CFStmtId, RegionFacts),
    analysis: *body_clone.AnalysisScratch,

    fn init(allocator: Allocator, analysis: *body_clone.AnalysisScratch) RegionCache {
        return .{ .regions = collections.DenseMap(LIR.CFStmtId, RegionFacts).init(allocator), .analysis = analysis };
    }

    fn deinit(self: *RegionCache) void {
        var values = self.regions.valueIterator();
        while (values.next()) |facts| facts.deinit();
        self.regions.deinit();
    }

    fn get(self: *RegionCache, store: *LirStore, body: LIR.CFStmtId, stats: *WorkStats) ResourceError!*const RegionFacts {
        if (self.regions.getPtr(body)) |facts| return facts;
        var facts = try RegionFacts.init(store, body, true, stats, self.regions.allocator, self.analysis);
        errdefer facts.deinit();
        try self.regions.put(body, facts);
        return self.regions.getPtr(body).?;
    }
};

/// Prove that every use of the joined tag is the match itself or a valid
/// payload projection in one of its arms. Returns whether the known literal
/// producer edges cover every projected variant. When they do not, fusion is
/// partial: known edges bypass the tag while the original join remains for
/// opaque producers and retains the complete match.
fn classifyJoinTagUses(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    facts: *const RegionFacts,
    matched_value: LIR.LocalId,
    match_stmt: LIR.CFStmtId,
    variants: *const Variants,
    stats: *WorkStats,
) ?bool {
    const tag_layout = layouts.getLayout(store.getLocal(matched_value).layout_idx);
    const info = layouts.getTagUnionInfo(tag_layout);
    var allowed: u32 = 1; // the discriminant read or direct switch condition
    var known: u32 = 1;
    for (facts.projections.items) |stmt_id| {
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
            stats.variant_lookups += 1;
            if (variants.indices.contains(variantKey(indices[0], indices[1]))) known += 1;
        }
    }
    if (facts.reads.get(matched_value) != allowed + facts.releases.get(matched_value)) return null;
    return known == allowed;
}

fn branchesUseOnlyPayloads(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    matched_value: LIR.LocalId,
    layout_param: LIR.LocalId,
    variants: *const Variants,
    cache: *RegionCache,
    stats: *WorkStats,
) ResourceError!bool {
    for (variants.builds.items, variants.targets.items) |build, branch| {
        const payload_layout = variantPayloadLayout(store, layouts, layout_param, build.variant_index) orelse return false;
        if (build.payload) |payload| {
            if (store.getLocal(payload).layout_idx != payload_layout) return false;
        }
        const facts = try cache.get(store, branch, stats);

        var allowed: u32 = 0;
        for (facts.projections.items) |stmt_id| {
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
        if (facts.reads.get(matched_value) != allowed + facts.releases.get(matched_value)) return false;
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

fn applyCandidate(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    join_params: *body_clone.JoinParamIndex,
    candidate: *const Candidate,
    stats: *WorkStats,
    allocator: Allocator,
) ResourceError!void {
    const join = store.getCFStmt(candidate.join_stmt).join;
    var dests = std.ArrayList(VariantDest).empty;
    defer dests.deinit(allocator);
    var cloned_locals = std.ArrayList(LIR.LocalId).empty;
    defer cloned_locals.deinit(allocator);
    for (candidate.variants.builds.items, candidate.variants.targets.items) |build, branch| {
        const payload_layout = variantPayloadLayout(store, layouts, candidate.param, build.variant_index) orelse unreachable;
        const payload_param = if (build.payload != null) try store.addLocal(.{ .layout_idx = payload_layout }) else null;
        const join_id = join_params.freshJoinPoint();
        const params = if (payload_param) |payload| try store.addLocalSpan(&.{payload}) else LIR.LocalSpan.empty();

        // Body and remainder are replaced before the declaration is reachable.
        const join_stmt = try store.addCFStmt(.{ .join = .{
            .id = join_id,
            .params = params,
            .body = join.remainder,
            .remainder = join.remainder,
        } });
        try join_params.record(store.getCFStmt(join_stmt).join);

        const branch_defs = candidate.branch_facts.regions.get(branch).?.defs;
        var cloner = try body_clone.BodyCloner(BranchRewriter).initWithFreshDeclaredJoinsAndAllocator(store, .{
            .param = candidate.matched_value,
            .variant_index = build.variant_index,
            .payload = payload_param orelse candidate.param,
            .payload_layout = payload_layout,
            .layouts = layouts,
            .union_locals = candidate.union_locals.items,
            .has_payload = payload_param != null,
        }, branch, join_params, allocator);
        defer cloner.deinit();
        const frame = store.getLocalSpan(store.getProcSpec(candidate.proc).frame_locals);
        for (0..frame.len) |index| {
            const local = GuardedList.at(frame, index);
            if (branch_defs.get(local) == 0) try cloner.local_map.put(local, local);
        }
        const body = try cloner.cloneStmt(branch);
        try cloned_locals.appendSlice(allocator, cloner.new_locals.items);
        store.getCFStmtPtr(join_stmt).join.body = body;
        try dests.append(allocator, .{
            .payload_param = payload_param,
            .join_id = join_id,
            .join_stmt = join_stmt,
        });
    }

    for (candidate.builds.items) |build| {
        stats.variant_lookups += 1;
        const dest_index = candidate.variants.indices.get(variantKey(build.variant_index, build.discriminant)).?;
        const dest = dests.items[dest_index];
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
    defer frame.deinit(allocator);
    const old_frame = store.getLocalSpan(proc.frame_locals);
    for (0..old_frame.len) |old_index| try frame.append(allocator, GuardedList.at(old_frame, old_index));
    for (dests.items) |dest| if (dest.payload_param) |payload| try frame.append(allocator, payload);
    try frame.appendSlice(allocator, cloned_locals.items);
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

test "tag case fusion declarations are referenced" {
    std.testing.refAllDecls(@This());
}

const TestGraph = struct {
    store: *LirStore,
    locals: std.ArrayList(LIR.LocalId) = .empty,
    next_join: u32 = 0,

    fn deinit(self: *TestGraph) void {
        self.locals.deinit(self.store.allocator);
    }

    fn local(self: *TestGraph, layout_idx: layout_mod.Idx) ResourceError!LIR.LocalId {
        const id = try self.store.addLocal(.{ .layout_idx = layout_idx });
        try self.locals.append(self.store.allocator, id);
        return id;
    }

    fn freshJoin(self: *TestGraph) LIR.JoinPointId {
        defer self.next_join += 1;
        return @enumFromInt(self.next_join);
    }

    fn tag(self: *TestGraph, param: LIR.LocalId, variant: u16, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        return self.store.addCFStmt(.{ .assign_tag = .{
            .target = param,
            .variant_index = variant,
            .discriminant = variant,
            .payload = null,
            .next = next,
        } });
    }

    fn consumer(self: *TestGraph, param: LIR.LocalId, arms: [2]LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        const disc = try self.local(.u16);
        const choose = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = disc,
            .branches = try self.store.addCFSwitchBranches(&.{.{ .value = 0, .body = arms[0] }}),
            .default_branch = arms[1],
        } });
        return self.store.addCFStmt(.{ .assign_ref = .{
            .target = disc,
            .op = .{ .discriminant = .{ .source = param } },
            .next = choose,
        } });
    }

    fn candidate(self: *TestGraph, selector: LIR.LocalId, arms: [2]LIR.CFStmtId, producer_count: usize) ResourceError!LIR.CFStmtId {
        const param = try self.local(.bool);
        const id = self.freshJoin();
        var producers = std.ArrayList(LIR.CFSwitchBranch).empty;
        defer producers.deinit(self.store.allocator);
        for (0..producer_count) |index| {
            const jump = try self.store.addCFStmt(.{ .jump = .{ .target = id } });
            const build = try self.tag(param, @intCast(index % 2), jump);
            try producers.append(self.store.allocator, .{ .value = index, .body = build });
        }
        const choose = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = selector,
            .branches = try self.store.addCFSwitchBranches(producers.items),
            .default_branch = producers.items[0].body,
        } });
        return self.store.addCFStmt(.{ .join = .{
            .id = id,
            .params = try self.store.addLocalSpan(&.{param}),
            .body = try self.consumer(param, arms),
            .remainder = choose,
        } });
    }

    fn proc(self: *TestGraph, body: LIR.CFStmtId) ResourceError!LIR.LirProcSpecId {
        return self.store.addProcSpec(.{
            .name = LIR.Symbol.fromRaw(1),
            .identity = LIR.ProcIdentity.forTest(2),
            .args = try self.store.addLocalSpan(self.locals.items[0..2]),
            .body = body,
            .frame_locals = try self.store.addLocalSpan(self.locals.items),
            .ret_layout = .u64,
        });
    }
};

const TestError = ResourceError || error{ TestExpectedEqual, TestUnexpectedResult };

test "tag case fusion procedure scratch is bounded and output survives scratch destruction" {
    const testing = std.testing;
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    const layout_count = layouts.layoutCount();
    var standalone = LirStore.init(testing.allocator);
    defer standalone.deinit();
    var procedures = LirStore.init(testing.allocator);
    defer procedures.deinit();
    for ([_]*LirStore{ &standalone, &procedures }) |store| {
        var graph: TestGraph = .{ .store = store };
        defer graph.deinit();
        // Put the small procedure beyond a large unrelated identity prefix.
        for (0..16384) |_| {
            const local = try store.addLocal(.{ .layout_idx = .u64 });
            _ = try store.addCFStmt(.{ .ret = .{ .value = local } });
        }
        const selector = try graph.local(.u64);
        const result = try graph.local(.u64);
        const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
        _ = try store.addCFStmt(.{ .join = .{
            .id = @enumFromInt(50000),
            .params = .empty(),
            .body = ret,
            .remainder = ret,
        } });
        for (0..2) |index| {
            const body = try graph.candidate(selector, .{ ret, ret }, 2);
            const proc = try graph.proc(body);
            // Unlike call-specializing passes, fusion does not change the ABI.
            if (index == 1) store.getProcSpecPtr(proc).abi = .erased_callable;
        }
    }
    try run(&standalone, &layouts);
    var next_join = body_clone.firstFreshJoinPoint(&procedures);
    try testing.expectEqual(@as(u32, 50001), next_join);
    for (0..procedures.procSpecCount()) |index| {
        var scratch: [65536]u8 = undefined;
        {
            var fixed = std.heap.FixedBufferAllocator.init(&scratch);
            var joins = body_clone.JoinParamIndex.init(fixed.allocator());
            defer joins.deinit();
            joins.next_join_point = next_join;
            try runProc(&procedures, &layouts, @enumFromInt(index), fixed.allocator(), &joins);
            next_join = joins.next_join_point;
        }
        // Poison the storage to catch accidentally retained temporary spans.
        @memset(&scratch, 0xa5);
    }
    try testing.expectEqual(@as(u32, 50005), next_join);
    try testing.expectEqual(layout_count, layouts.layoutCount());
    try testing.expectEqual(standalone.cfStmtCount(), procedures.cfStmtCount());
    for (0..standalone.cfStmtCount()) |index| {
        try testing.expectEqualDeep(standalone.getCFStmt(@enumFromInt(index)), procedures.getCFStmt(@enumFromInt(index)));
    }
    for (0..procedures.procSpecCount()) |index| {
        const proc: LIR.LirProcSpecId = @enumFromInt(index);
        try testing.expectEqualDeep(standalone.getProcSpec(proc), procedures.getProcSpec(proc));
        var walk = try body_clone.ReachableStmts.init(&procedures, rewritableProcBody(&procedures, proc).?);
        defer walk.deinit();
        while (try walk.next()) |stmt| {
            const node = procedures.getCFStmt(stmt);
            try testing.expect(node != .assign_tag);
            if (node == .join) try testing.expect(@intFromEnum(node.join.id) > 50000);
        }
    }
}

fn testIndependentFusions(allocator: Allocator, candidate_count: usize, unrelated_count: usize, producer_count: usize) TestError!WorkStats {
    const testing = std.testing;
    var store = LirStore.init(allocator);
    defer store.deinit();
    // Layouts are immutable pass inputs, outside the LIR allocation sweep.
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    var graph: TestGraph = .{ .store = &store };
    defer graph.deinit();
    const selector = try graph.local(.u64);
    const result = try graph.local(.u64);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    var branches = std.ArrayList(LIR.CFSwitchBranch).empty;
    defer branches.deinit(allocator);
    for (0..candidate_count) |index| {
        const candidate = try graph.candidate(selector, .{ ret, ret }, producer_count);
        try branches.append(allocator, .{ .value = index, .body = candidate });
    }
    const root = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = selector,
        .branches = try store.addCFSwitchBranches(branches.items),
        .default_branch = ret,
    } });
    _ = try graph.proc(root);
    // Unreachable declarations still occupy the global identity domain.
    _ = try store.addCFStmt(.{ .join = .{
        .id = @enumFromInt(50000),
        .params = LIR.LocalSpan.empty(),
        .body = ret,
        .remainder = ret,
    } });
    for (0..unrelated_count) |_| _ = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const initial_statement_count = store.getCFStmts().len;
    const stats = try runWithStats(&store, &layouts);
    try testing.expectEqual(candidate_count, stats.fusions);
    try testing.expectEqual(initial_statement_count, stats.global_statement_visits);
    var walk = try body_clone.ReachableStmts.init(&store, root);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        try testing.expect(stmt != .assign_tag);
        if (stmt == .join) try testing.expect(@intFromEnum(stmt.join.id) > 50000);
    }
    return stats;
}

fn testFusionAllocationFailures(allocator: Allocator) TestError!void {
    _ = try testIndependentFusions(allocator, 1, 0, 2);
}

test "tag case fusion releases candidate on every allocation failure including initial join indexing" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testFusionAllocationFailures, .{});
}

test "tag case fusion inventories scale with producers and not unrelated store size" {
    const testing = std.testing;
    const small = try testIndependentFusions(testing.allocator, 1, 0, 2);
    try testing.expectEqual(@as(usize, 3), small.inventory_walks);
    try testing.expectEqual(@as(usize, 9), small.inventory_statement_visits);
    try testing.expectEqual(@as(usize, 1), small.definition_walks);
    try testing.expectEqual(@as(usize, 7), small.variant_lookups);
    const unrelated = try testIndependentFusions(testing.allocator, 1, 4096, 2);
    try testing.expectEqual(small.global_statement_visits + 4096, unrelated.global_statement_visits);
    try testing.expectEqual(small.discovery_statement_visits, unrelated.discovery_statement_visits);
    try testing.expectEqual(small.inventory_statement_visits, unrelated.inventory_statement_visits);
    try testing.expectEqual(small.variant_lookups, unrelated.variant_lookups);
    const many = try testIndependentFusions(testing.allocator, 8, 4096, 2);
    try testing.expectEqual(8 * small.inventory_walks, many.inventory_walks);
    try testing.expectEqual(8 * small.definition_walks, many.definition_walks);
    try testing.expectEqual(8 * small.variant_lookups, many.variant_lookups);
    const repeated = try testIndependentFusions(testing.allocator, 1, 0, 128);
    try testing.expectEqual(small.inventory_walks, repeated.inventory_walks);
    try testing.expectEqual(small.definition_walks, repeated.definition_walks);
    try testing.expectEqual(small.inventory_statement_visits + 2 * (128 - 2), repeated.inventory_statement_visits);
    try testing.expectEqual(small.variant_lookups + 2 * (128 - 2), repeated.variant_lookups);
}

test "tag case fusion does not recover opaque producers after descendant cloning" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    var graph: TestGraph = .{ .store = &store };
    defer graph.deinit();
    const selector = try graph.local(.u64);
    const result = try graph.local(.u64);
    const outer_param = try graph.local(.bool);
    const outer_id = graph.freshJoin();
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const shared_jump = try store.addCFStmt(.{ .jump = .{ .target = outer_id } });
    const zero = try graph.tag(outer_param, 0, shared_jump);
    const one = try graph.tag(outer_param, 1, shared_jump);
    const inner = try graph.candidate(selector, .{ zero, one }, 2);
    const outer = try store.addCFStmt(.{ .join = .{
        .id = outer_id,
        .params = try store.addLocalSpan(&.{outer_param}),
        .body = try graph.consumer(outer_param, .{ ret, ret }),
        .remainder = inner,
    } });
    const proc = try graph.proc(outer);
    var analysis = body_clone.AnalysisScratch.init(testing.allocator);
    defer analysis.deinit();
    var before: WorkStats = .{};
    var first = (try findCandidate(&store, &layouts, proc, &before, testing.allocator, &analysis)).?;
    defer first.deinit(testing.allocator);
    try testing.expectEqual(inner, first.join_stmt);
    const stats = try runWithStats(&store, &layouts);
    try testing.expectEqual(@as(usize, 1), stats.fusions);
    try testing.expectEqual(outer_id, store.getCFStmt(outer).join.id);
    // Renamed constructors now initialize the outer parameter through explicit
    // transfers. Those are deliberately not recovered as literal producers.
    var walk = try body_clone.ReachableStmts.init(&store, outer);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        if (stmt == .assign_tag) try testing.expect(stmt.assign_tag.target != outer_param);
    }
}

test "tag case fusion retries an ancestor after a descendant removes an unproduced reentry arm" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    var graph: TestGraph = .{ .store = &store };
    defer graph.deinit();
    const selector = try graph.local(.u64);
    const result = try graph.local(.u64);
    const outer_param = try graph.local(.bool);
    const outer_id = graph.freshJoin();
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const reenter = try store.addCFStmt(.{ .jump = .{ .target = outer_id } });
    // Only variant zero is produced. Fusing this join removes the consumer
    // arm that reenters the outer join, making the previously rejected outer
    // candidate eligible without changing any actually selected effect path.
    const inner = try graph.candidate(selector, .{ ret, reenter }, 1);
    const zero_jump = try store.addCFStmt(.{ .jump = .{ .target = outer_id } });
    const one_jump = try store.addCFStmt(.{ .jump = .{ .target = outer_id } });
    const zero = try graph.tag(outer_param, 0, zero_jump);
    const one = try graph.tag(outer_param, 1, one_jump);
    const choose = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = selector,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = zero }}),
        .default_branch = one,
    } });
    const outer = try store.addCFStmt(.{ .join = .{
        .id = outer_id,
        .params = try store.addLocalSpan(&.{outer_param}),
        .body = try graph.consumer(outer_param, .{ inner, ret }),
        .remainder = choose,
    } });
    const proc = try graph.proc(outer);
    var analysis = body_clone.AnalysisScratch.init(testing.allocator);
    defer analysis.deinit();
    var before: WorkStats = .{};
    {
        var first = (try findCandidate(&store, &layouts, proc, &before, testing.allocator, &analysis)).?;
        defer first.deinit(testing.allocator);
        try testing.expectEqual(inner, first.join_stmt);
    }
    const stats = try runWithStats(&store, &layouts);
    try testing.expectEqual(@as(usize, 2), stats.fusions);
    var walk = try body_clone.ReachableStmts.init(&store, outer);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        try testing.expect(stmt != .assign_tag);
        if (stmt == .jump) try testing.expect(stmt.jump.target != outer_id);
    }
}

test "tag case fusion discovers nested candidates only in reachable clones" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    var graph: TestGraph = .{ .store = &store };
    defer graph.deinit();
    const selector = try graph.local(.u64);
    const result = try graph.local(.u64);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const inner = try graph.candidate(selector, .{ ret, ret }, 2);
    const inner_id = store.getCFStmt(inner).join.id;
    const outer = try graph.candidate(selector, .{ inner, ret }, 2);
    _ = try graph.proc(outer);
    const stats = try runWithStats(&store, &layouts);
    try testing.expectEqual(@as(usize, 2), stats.fusions);
    // The original nested declaration is orphaned, not transformed.
    try testing.expectEqual(inner_id, store.getCFStmt(inner).join.id);
    var walk = try body_clone.ReachableStmts.init(&store, outer);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        try testing.expect(stmt_id != inner);
        try testing.expect(store.getCFStmt(stmt_id) != .assign_tag);
    }
}

test "tag case fusion rejects a consumer that loops back to the union join" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    var graph: TestGraph = .{ .store = &store };
    defer graph.deinit();
    const selector = try graph.local(.u64);
    const result = try graph.local(.u64);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const reenter = try store.addCFStmt(.{ .jump = .{ .target = @enumFromInt(graph.next_join) } });
    const root = try graph.candidate(selector, .{ ret, reenter }, 2);
    _ = try graph.proc(root);
    const stats = try runWithStats(&store, &layouts);
    try testing.expectEqual(@as(usize, 0), stats.fusions);
    try testing.expectEqual(@as(usize, 1), stats.inventory_walks);
}

test "tag case fusion shares conditional alias release suffix and rejects extra alias reads" {
    try testConditionalAliasRelease(false);
    try testConditionalAliasRelease(true);
}

fn testConditionalAliasRelease(extra_read: bool) TestError!void {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    var graph: TestGraph = .{ .store = &store };
    defer graph.deinit();
    const text = try graph.local(.str);
    const result = try graph.local(.u64);
    const tag_layout = try layouts.putTagUnion(&.{ .str, .zst });
    const param = try graph.local(tag_layout);
    const alias = try graph.local(tag_layout);
    const matched = try graph.local(tag_layout);
    const join_id = graph.freshJoin();
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const release = try store.addCFStmt(.{ .decref_if_initialized = .{
        .cond = result,
        .cond_mask = 8,
        .value = alias,
        .rc = LIR.RcHelper.fromConcrete(.{ .op = .decref, .layout_idx = tag_layout }),
        .atomicity = .single_thread,
        .next = if (extra_read) try store.addCFStmt(.{ .ret = .{ .value = alias } }) else ret,
    } });
    const left = try graph.local(.u64);
    const right = try graph.local(.u64);
    const prefix_left = try store.addCFStmt(.{ .assign_literal = .{
        .target = left,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = release,
    } });
    const prefix_right = try store.addCFStmt(.{ .assign_literal = .{
        .target = right,
        .value = .{ .i64_literal = .{ .value = 2, .layout_idx = .u64 } },
        .next = release,
    } });
    const arm = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = result,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = prefix_left }}),
        .default_branch = prefix_right,
    } });
    const consume = try graph.consumer(matched, .{ arm, arm });
    const second_alias = try store.addCFStmt(.{ .assign_ref = .{
        .target = matched,
        .op = .{ .local = alias },
        .next = consume,
    } });
    const first_alias = try store.addCFStmt(.{ .assign_ref = .{
        .target = alias,
        .op = .{ .local = param },
        .next = second_alias,
    } });
    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const build = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 0,
        .discriminant = 0,
        .payload = text,
        .next = jump,
    } });
    const root = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{param}),
        .body = first_alias,
        .remainder = build,
    } });
    _ = try graph.proc(root);
    const stats = try runWithStats(&store, &layouts);
    if (extra_read) {
        try testing.expectEqual(@as(usize, 0), stats.fusions);
        return;
    }
    try testing.expectEqual(@as(usize, 1), stats.fusions);
    var releases: usize = 0;
    var walk = try body_clone.ReachableStmts.init(&store, root);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        try testing.expect(stmt != .assign_tag);
        if (stmt == .decref_if_initialized) {
            releases += 1;
            const cloned = stmt.decref_if_initialized;
            try testing.expectEqual(result, cloned.cond);
            try testing.expectEqual(@as(u64, 8), cloned.cond_mask);
            try testing.expectEqual(LIR.RcAtomicity.single_thread, cloned.atomicity);
            try testing.expectEqual(layout_mod.Idx.str, store.getLocal(cloned.value).layout_idx);
            try testing.expect(cloned.value != param and cloned.value != alias and cloned.value != matched);
        }
    }
    try testing.expectEqual(@as(usize, 1), releases);
}

test "tag case fusion variant index preserves pair identity and checks repeated payload presence" {
    var store = LirStore.init(std.testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(std.testing.allocator, .u64);
    defer layouts.deinit();
    var graph: TestGraph = .{ .store = &store };
    defer graph.deinit();
    const payload = try graph.local(.u64);
    const param = try graph.local(try layouts.putTagUnion(&.{ .u64, .zst, .zst }));
    const jump = try store.addCFStmt(.{ .jump = .{ .target = graph.freshJoin() } });
    const build = try store.addCFStmt(.{ .assign_tag = .{
        .target = param,
        .variant_index = 1,
        .discriminant = 2,
        .payload = null,
        .next = jump,
    } });
    var variants = Variants.init(std.testing.allocator);
    defer variants.deinit();
    var stats: WorkStats = .{};
    const first: BuildSite = .{
        .stmt = build,
        .edge_jump = jump,
        .variant_index = 1,
        .discriminant = 2,
        .payload = null,
    };
    try std.testing.expect(try variants.add(first, &stats));
    try std.testing.expect(try variants.add(first, &stats));
    var different = first;
    different.variant_index = 2;
    try std.testing.expect(try variants.add(different, &stats));
    different = first;
    different.discriminant = 1;
    try std.testing.expect(try variants.add(different, &stats));
    different = first;
    different.payload = payload;
    try std.testing.expect(!try variants.add(different, &stats));
    try std.testing.expectEqual(@as(usize, 3), variants.builds.items.len);
    try std.testing.expectEqual(@as(usize, 5), stats.variant_lookups);
}

test "tag case fusion routes distinct discriminants and shares default facts but requires projected payload" {
    try testDiscriminantRouting(false);
    try testDiscriminantRouting(true);
}

fn testDiscriminantRouting(missing_payload: bool) TestError!void {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    var graph: TestGraph = .{ .store = &store };
    defer graph.deinit();
    const selector = try graph.local(.u64);
    const payload = try graph.local(.u64);
    const param = try graph.local(try layouts.putTagUnion(&.{ .u64, .zst, .zst }));
    const projected = try graph.local(.u64);
    const disc = try graph.local(.u16);
    const join_id = graph.freshJoin();
    const explicit_ret = try store.addCFStmt(.{ .ret = .{ .value = projected } });
    const explicit = try store.addCFStmt(.{ .assign_ref = .{
        .target = projected,
        .op = .{ .tag_payload = .{
            .source = param,
            .variant_index = 0,
            .tag_discriminant = 7,
            .payload_idx = 0,
        } },
        .next = explicit_ret,
    } });
    const default = try store.addCFStmt(.{ .ret = .{ .value = selector } });
    const choose = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 7, .body = explicit }}),
        .default_branch = default,
    } });
    const consume = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = param } },
        .next = choose,
    } });
    var producers: [3]LIR.CFStmtId = undefined;
    for (&producers, 0..) |*producer, index| {
        producer.* = try store.addCFStmt(.{ .assign_tag = .{
            .target = param,
            .variant_index = @intCast(index),
            .discriminant = @intCast(7 + index),
            .payload = if (index == 0 and !missing_payload) payload else null,
            .next = try store.addCFStmt(.{ .jump = .{ .target = join_id } }),
        } });
    }
    const produce = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = selector,
        .branches = try store.addCFSwitchBranches(&.{
            .{ .value = 0, .body = producers[0] },
            .{ .value = 1, .body = producers[1] },
        }),
        .default_branch = producers[2],
    } });
    const root = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{param}),
        .body = consume,
        .remainder = produce,
    } });
    _ = try graph.proc(root);
    const stats = try runWithStats(&store, &layouts);
    try testing.expectEqual(@as(usize, if (missing_payload) 0 else 1), stats.fusions);
    if (missing_payload) return;
    // One inventory per distinct arm, not per variant selecting the default.
    try testing.expectEqual(@as(usize, 2), stats.definition_walks);
    for (producers, 0..) |producer, index| {
        const rewritten = store.getCFStmt(producer);
        const target = if (rewritten == .set_local)
            store.getCFStmt(rewritten.set_local.next).jump.target
        else
            rewritten.jump.target;
        var destination = root;
        while (store.getCFStmt(destination).join.id != target) {
            destination = store.getCFStmt(destination).join.remainder;
        }
        const join = store.getCFStmt(destination).join;
        if (index == 0) {
            const write = rewritten.set_local;
            try testing.expectEqual(payload, write.value);
            try testing.expectEqual(GuardedList.at(store.getLocalSpan(join.params), 0), write.target);
            try testing.expectEqual(join.id, store.getCFStmt(write.next).jump.target);
            const projection = store.getCFStmt(join.body).assign_ref;
            try testing.expectEqual(write.target, projection.op.local);
            try testing.expectEqual(projection.target, store.getCFStmt(projection.next).ret.value);
        } else {
            try testing.expectEqual(join.id, rewritten.jump.target);
            try testing.expectEqual(selector, store.getCFStmt(join.body).ret.value);
        }
    }
    var defaults: usize = 0;
    var explicit_returns: usize = 0;
    var walk = try body_clone.ReachableStmts.init(&store, root);
    defer walk.deinit();
    while (try walk.next()) |id| {
        const stmt = store.getCFStmt(id);
        try testing.expect(stmt != .assign_tag);
        if (stmt == .ret) {
            if (stmt.ret.value == selector) {
                defaults += 1;
            } else {
                explicit_returns += 1;
                try testing.expect(stmt.ret.value != projected);
                try testing.expectEqual(layout_mod.Idx.u64, store.getLocal(stmt.ret.value).layout_idx);
            }
        }
    }
    try testing.expectEqual(@as(usize, 2), defaults);
    try testing.expectEqual(@as(usize, 1), explicit_returns);
}

test "tag case fusion indexes reused join ids per procedure when cloning nested and external jumps" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    var roots: [2]LIR.CFStmtId = undefined;
    var results: [2]LIR.LocalId = undefined;
    const external_id: LIR.JoinPointId = @enumFromInt(1);
    const internal_id: LIR.JoinPointId = @enumFromInt(2);
    for (&roots, &results) |*root, *result| {
        // Both procedures reuse all source join IDs but own distinct parameters.
        var graph: TestGraph = .{ .store = &store };
        defer graph.deinit();
        const selector = try graph.local(.u64);
        result.* = try graph.local(.u64);
        const ret = try store.addCFStmt(.{ .ret = .{ .value = result.* } });
        const external_jump = try store.addCFStmt(.{ .jump = .{ .target = external_id } });
        const internal_jump = try store.addCFStmt(.{ .jump = .{ .target = internal_id } });
        const initialize = try store.addCFStmt(.{ .assign_literal = .{
            .target = result.*,
            .value = .{ .i64_literal = .{ .value = 42, .layout_idx = .u64 } },
            .next = internal_jump,
        } });
        const arm = try store.addCFStmt(.{ .join = .{
            .id = internal_id,
            .params = try store.addLocalSpan(&.{result.*}),
            .body = external_jump,
            .remainder = initialize,
        } });
        const candidate = try graph.candidate(selector, .{ arm, arm }, 2);
        root.* = try store.addCFStmt(.{ .join = .{
            .id = external_id,
            .params = try store.addLocalSpan(&.{result.*}),
            .body = ret,
            .remainder = candidate,
        } });
        _ = try graph.proc(root.*);
    }
    const stats = try runWithStats(&store, &layouts);
    try testing.expectEqual(@as(usize, 2), stats.fusions);
    var cloned_ids = collections.DenseMap(LIR.JoinPointId, void).init(testing.allocator);
    defer cloned_ids.deinit();
    for (roots, results) |root, result| {
        var bridges: usize = 0;
        var nested: usize = 0;
        var walk = try body_clone.ReachableStmts.init(&store, root);
        defer walk.deinit();
        while (try walk.next()) |id| {
            const stmt = store.getCFStmt(id);
            try testing.expect(stmt != .assign_tag);
            if (stmt == .join and stmt.join.id != external_id) {
                try testing.expect(@intFromEnum(stmt.join.id) > @intFromEnum(internal_id));
                try testing.expect(!cloned_ids.contains(stmt.join.id));
                try cloned_ids.put(stmt.join.id, {});
                const params = store.getLocalSpan(stmt.join.params);
                if (params.len == 0) continue; // The payload-free variant join.
                nested += 1;
                const cloned_param = GuardedList.at(params, 0);
                try testing.expect(cloned_param != result);
                const bridge = store.getCFStmt(stmt.join.body).set_local;
                try testing.expectEqual(result, bridge.target);
                try testing.expectEqual(cloned_param, bridge.value);
                try testing.expectEqual(LIR.SetLocalWriteMode.initialize_join_param, bridge.mode);
                try testing.expectEqual(external_id, store.getCFStmt(bridge.next).jump.target);
                const initialized = store.getCFStmt(stmt.join.remainder).assign_literal;
                try testing.expectEqual(cloned_param, initialized.target);
                try testing.expectEqual(stmt.join.id, store.getCFStmt(initialized.next).jump.target);
                bridges += 1;
            }
        }
        try testing.expectEqual(@as(usize, 2), nested);
        try testing.expectEqual(@as(usize, 2), bridges);
    }
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
    const join_id: LIR.JoinPointId = @enumFromInt(body_clone.firstFreshJoinPoint(&store));

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
        .identity = LIR.ProcIdentity.forTest(2),
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
    const join_id: LIR.JoinPointId = @enumFromInt(body_clone.firstFreshJoinPoint(&store));

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
        .identity = LIR.ProcIdentity.forTest(1),
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
    const join_id: LIR.JoinPointId = @enumFromInt(body_clone.firstFreshJoinPoint(&store));

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
        .identity = LIR.ProcIdentity.forTest(1),
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
    const join_id: LIR.JoinPointId = @enumFromInt(body_clone.firstFreshJoinPoint(&store));

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
        .identity = LIR.ProcIdentity.forTest(1),
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
    const join_id: LIR.JoinPointId = @enumFromInt(body_clone.firstFreshJoinPoint(&store));
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
        .identity = LIR.ProcIdentity.forTest(1),
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
    const join_id: LIR.JoinPointId = @enumFromInt(body_clone.firstFreshJoinPoint(&store));
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
        .identity = LIR.ProcIdentity.forTest(1),
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
