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
const core = @import("lir_core");
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
    complete: bool,

    fn deinit(self: *Candidate, allocator: Allocator) void {
        self.builds.deinit(allocator);
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

    pub fn cloneRet(_: *BranchRewriter, cloner: anytype, value: LIR.LocalId) ResourceError!LIR.CFStmtId {
        return try cloner.store.addCFStmt(.{ .ret = .{ .value = try cloner.mapLocal(value) } });
    }

    pub fn interceptStmt(self: *BranchRewriter, cloner: anytype, stmt: LIR.CFStmt) ResourceError!?LIR.CFStmtId {
        if (stmt != .assign_ref) return null;
        const assign = stmt.assign_ref;
        if (assign.op == .tag_payload_struct) {
            const payload = assign.op.tag_payload_struct;
            if (payload.source != self.param) return null;
            std.debug.assert(payload.variant_index == self.variant_index);
            cloner.local_map[@intFromEnum(assign.target)] = self.payload;
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
        if (!store.getProcSpec(proc).iterator_fusion_scope) continue;
        var indexed = false;
        while (try findCandidate(store, layouts, proc)) |found| {
            if (!indexed) {
                try join_params.indexReachable(store, store.getProcSpec(proc).body.?);
                indexed = true;
            }
            var candidate = found;
            defer candidate.deinit(store.allocator);
            try applyCandidate(store, layouts, &join_params, &candidate);
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
        const param = GuardedList.at(params, 0);
        const param_layout = layouts.getLayout(store.getLocal(param).layout_idx);
        if (param_layout.tag != .tag_union or store.getLocal(param).boxy_desc != null) continue;

        var matched_value = param;
        var match_stmt = join.body;
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

        var join_reads = try body_clone.countReachableReads(store, join.body);
        defer join_reads.deinit();
        var aliases_are_linear = true;
        for (alias_sources.items) |source| {
            if (join_reads.get(source) != 1) {
                aliases_are_linear = false;
                break;
            }
        }
        if (!aliases_are_linear) continue;
        if (match_node == .assign_ref and join_reads.get(match_node.assign_ref.target) != 1) continue;

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
            const next = store.getCFStmt(assign.next);
            if (next != .jump or next.jump.target != join.id) continue;
            try builds.append(store.allocator, .{
                .stmt = stmt_id,
                .variant_index = assign.variant_index,
                .discriminant = assign.discriminant,
                .payload = assign.payload,
            });
        }
        if (builds.items.len == 0 or builds.items.len != jump_count) {
            builds.deinit(store.allocator);
            continue;
        }
        if (!payloadPresenceConsistent(builds.items)) {
            builds.deinit(store.allocator);
            continue;
        }
        if (!try branchesUseOnlyPayloads(store, layouts, matched_value, param, switch_node.switch_stmt, builds.items)) {
            builds.deinit(store.allocator);
            continue;
        }
        const complete = (try classifyJoinTagUses(store, layouts, join.body, matched_value, match_stmt, builds.items, &join_reads)) orelse {
            builds.deinit(store.allocator);
            continue;
        };
        if (!try branchesAreOwnershipNeutral(store, layouts, switch_node.switch_stmt, builds.items)) {
            builds.deinit(store.allocator);
            continue;
        }
        return .{
            .proc = proc,
            .join_stmt = join_stmt,
            .param = param,
            .matched_value = matched_value,
            .switch_stmt = switch_stmt,
            .builds = builds,
            .complete = complete,
        };
    }
    return null;
}

/// Before ARC, cloning scalar locals is ownership-neutral. Refcounted branch
/// definitions require a full ownership-aware clone, including path-specific
/// move facts across the newly introduced joins, even when complete fusion
/// removes the original arms.
fn branchesAreOwnershipNeutral(
    store: *LirStore,
    layouts: *const layout_mod.Store,
    switch_stmt: @FieldType(LIR.CFStmt, "switch_stmt"),
    builds: []const BuildSite,
) ResourceError!bool {
    for (builds) |build| {
        const branch = switchTarget(store, switch_stmt, build.discriminant);
        const definitions = try body_clone.collectReachableDefinitions(store, branch);
        defer store.allocator.free(definitions);
        for (definitions, 0..) |is_defined, local_index| {
            if (!is_defined) continue;
            const local: LIR.LocalId = @enumFromInt(@as(u32, @intCast(local_index)));
            if (layouts.layoutContainsRefcounted(layouts.getLayout(store.getLocal(local).layout_idx))) return false;
        }
    }
    return true;
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
    if (reads.get(matched_value) != allowed) return null;
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
) ResourceError!bool {
    for (builds) |build| {
        const payload_layout = variantPayloadLayout(store, layouts, layout_param, build.variant_index) orelse return false;
        if (build.payload) |payload| {
            if (store.getLocal(payload).layout_idx != payload_layout) return false;
        }
        const branch = switchTarget(store, switch_stmt, build.discriminant);
        var reads = try body_clone.countReachableReads(store, branch);
        defer reads.deinit();

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
        if (reads.get(matched_value) != allowed) return false;
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
        const branch_defs = try body_clone.collectReachableDefinitions(store, branch);
        defer store.allocator.free(branch_defs);
        var cloner = try body_clone.BodyCloner(BranchRewriter).initWithFreshDeclaredJoins(store, .{
            .param = candidate.matched_value,
            .variant_index = build.variant_index,
            .payload = payload_param orelse candidate.param,
            .payload_layout = payload_layout,
            .layouts = layouts,
        }, branch, join_params);
        defer cloner.deinit();
        const frame = store.getLocalSpan(store.getProcSpec(candidate.proc).frame_locals);
        for (0..frame.len) |index| {
            const local = GuardedList.at(frame, index);
            if (!branch_defs[@intFromEnum(local)]) cloner.local_map[@intFromEnum(local)] = local;
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
        const jump = try store.addCFStmt(.{ .jump = .{ .target = dest.join_id } });
        if (dest.payload_param) |payload_param| {
            const payload = build.payload orelse unreachable;
            store.getCFStmtPtr(build.stmt).* = .{ .set_local = .{
                .target = payload_param,
                .value = payload,
                .mode = .initialize_join_param,
                .next = jump,
            } };
        } else {
            store.getCFStmtPtr(build.stmt).* = store.getCFStmt(jump);
        }
    }

    // A complete fusion removes the original tag join. A partial fusion keeps
    // an exact copy for producer edges that were not literal constructors in
    // this procedure; known edges have already been redirected around it.
    var replacement = if (candidate.complete)
        join.remainder
    else
        try store.addCFStmt(.{ .join = join });
    var index = dests.items.len;
    while (index > 0) {
        index -= 1;
        const dest = dests.items[index];
        store.getCFStmtPtr(dest.join_stmt).join.remainder = replacement;
        replacement = dest.join_stmt;
    }
    store.getCFStmtPtr(candidate.join_stmt).* = store.getCFStmt(replacement);

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

test "tag case fusion requires ownership-neutral branch definitions" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(testing.allocator, .u64);
    defer layouts.deinit();

    const cond = try store.addLocal(.{ .layout_idx = .bool });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = text } });
    const branch = try store.addCFStmt(.{ .assign_literal = .{
        .target = text,
        .value = .{ .str_literal = try store.insertStringView("owned", 0, 5) },
        .next = ret,
    } });
    const switch_id = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = branch }}),
        .default_branch = branch,
    } });
    const builds = [_]BuildSite{.{
        .stmt = branch,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
    }};

    try testing.expect(!try branchesAreOwnershipNeutral(
        &store,
        &layouts,
        store.getCFStmt(switch_id).switch_stmt,
        &builds,
    ));
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
