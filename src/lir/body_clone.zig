//! Shared machinery for the proc-specializing rewrite passes that clone a
//! source proc body into an internal variant with rewritten return handling.
//!
//! `StrAppend` and `ReturnSlot` both clone an existing proc body statement by
//! statement, remapping every local, while intercepting the body's returns to
//! emit their own destination-aware tail. This module owns the parts that do
//! not vary between them: the generic `BodyCloner(Rewriter)`, the reachable
//! successor walk, the alias-forwarding walk, the operand read counter used to
//! prove rewrite soundness, and the frame-local deduplication helpers.

const std = @import("std");
const collections = @import("collections");
const Allocator = std.mem.Allocator;
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = LirStore.GuardedList;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;

/// A local reached by forwarding through `assign_ref .local` aliases, paired
/// with the first statement past the alias chain.
pub const ForwardedAlias = struct {
    value: LocalId,
    next: CFStmtId,
};

/// Runtime parameters associated with every join identity in the current LIR.
/// Index each procedure once before rewriting it and update the index as the
/// pass adds joins, so subtree clones can resolve external jump targets
/// without rescanning the procedure for every cloned branch.
pub const JoinParamIndex = struct {
    params: collections.DenseMap(LIR.JoinPointId, LIR.LocalSpan),

    pub fn init(allocator: Allocator) JoinParamIndex {
        return .{ .params = collections.DenseMap(LIR.JoinPointId, LIR.LocalSpan).init(allocator) };
    }

    pub fn deinit(self: *JoinParamIndex) void {
        self.params.deinit();
    }

    pub fn record(self: *JoinParamIndex, join: @FieldType(LIR.CFStmt, "join")) Allocator.Error!void {
        try self.params.put(join.id, join.params);
    }

    pub fn indexReachable(self: *JoinParamIndex, store: *LirStore, body: CFStmtId) Allocator.Error!void {
        var walk = try ReachableStmts.init(store, body);
        defer walk.deinit();
        while (try walk.next()) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .join) try self.record(stmt.join);
        }
    }

    fn get(self: *const JoinParamIndex, id: LIR.JoinPointId) ?LIR.LocalSpan {
        return self.params.get(id);
    }
};

/// Follow a straight-line chain of `assign_ref .local` aliases of `source`,
/// returning the final aliased local and the first statement that is not such
/// an alias. Only aliases that copy the tracked value into a same-layout local
/// are crossed.
pub fn forwardLocalAliasChain(store: *const LirStore, source: LocalId, first_stmt: CFStmtId) ForwardedAlias {
    return forwardLocalAliasChainImpl(store, undefined, source, first_stmt, null) catch unreachable;
}

/// Like `forwardLocalAliasChain`, but also appends `source` and every crossed
/// alias local to `chain`, so a caller can require each consumed local is only
/// used by the chain before fusing the producer into the store.
pub fn forwardLocalAliasChainInto(
    store: *const LirStore,
    allocator: Allocator,
    source: LocalId,
    first_stmt: CFStmtId,
    chain: *std.ArrayList(LocalId),
) Allocator.Error!ForwardedAlias {
    return forwardLocalAliasChainImpl(store, allocator, source, first_stmt, chain);
}

fn forwardLocalAliasChainImpl(
    store: *const LirStore,
    allocator: Allocator,
    source: LocalId,
    first_stmt: CFStmtId,
    chain: ?*std.ArrayList(LocalId),
) Allocator.Error!ForwardedAlias {
    if (chain) |list| try list.append(allocator, source);
    var value = source;
    var current = first_stmt;
    while (true) {
        const stmt_node = store.getCFStmt(current);
        if (stmt_node != .assign_ref) return .{ .value = value, .next = current };
        const stmt = stmt_node.assign_ref;
        if (stmt.op == .local) {
            const local = stmt.op.local;
            if (local == value and store.getLocal(stmt.target).layout_idx == store.getLocal(value).layout_idx) {
                if (chain) |list| try list.append(allocator, stmt.target);
                value = stmt.target;
                current = stmt.next;
                continue;
            }
        }
        return .{ .value = value, .next = current };
    }
}

/// Push every control-flow successor of `stmt_id` onto `work`, covering
/// straight-line `next` edges, switch branches and continuations, initialized
/// payload arms, string-match arms, and join bodies. This is the reachability
/// step shared by the proc walkers.
pub fn appendSuccessors(
    store: *LirStore,
    work: *std.ArrayList(CFStmtId),
    stmt_id: CFStmtId,
) Allocator.Error!void {
    switch (store.getCFStmt(stmt_id)) {
        inline .assign_ref,
        .assign_literal,
        .init_uninitialized,
        .assign_call,
        .assign_call_erased,
        .assign_packed_erased_fn,
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
        .assign_call_dict,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .store_struct,
        .store_tag,
        .set_local,
        .debug,
        .expect,
        .comptime_branch_taken,
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        => |s| try work.append(store.allocator, s.next),

        .switch_stmt => |s| {
            if (s.continuation) |continuation| try work.append(store.allocator, continuation);
            try work.append(store.allocator, s.default_branch);
            const branches = store.getCFSwitchBranches(s.branches);
            for (0..branches.len) |index| {
                try work.append(store.allocator, GuardedList.at(branches, index).body);
            }
        },
        .switch_initialized_payload => |s| {
            try work.append(store.allocator, s.initialized_branch);
            try work.append(store.allocator, s.uninitialized_branch);
        },
        .str_match => |s| {
            try work.append(store.allocator, s.on_match);
            try work.append(store.allocator, s.on_miss);
        },
        .boxy_tag_match => |s| {
            try work.append(store.allocator, s.on_match);
            try work.append(store.allocator, s.on_miss);
        },
        .str_match_set => |s| {
            const arms = store.getStrMatchArms(s.arms);
            for (0..arms.len) |index| {
                try work.append(store.allocator, GuardedList.at(arms, index).on_match);
            }
            try work.append(store.allocator, s.on_miss);
        },
        .join => |s| {
            try work.append(store.allocator, s.body);
            try work.append(store.allocator, s.remainder);
        },
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .expect_err,
        .loop_continue,
        .loop_break,
        .jump,
        .ret,
        .crash,
        => {},
    }
}

/// Per-local operand read counts over the statements reachable from one proc
/// body. A rewrite that fuses a producer into its consumer orphans the
/// producer's result local; these counts let a pass require that no other
/// statement still reads that local before it commits the fusion.
pub const ReadCounts = struct {
    allocator: Allocator,
    counts: []u32,

    /// Release the backing count storage.
    pub fn deinit(self: *ReadCounts) void {
        self.allocator.free(self.counts);
    }

    /// How many reachable statements read `local` as an operand.
    pub fn get(self: *const ReadCounts, local: LocalId) u32 {
        return self.counts[@intFromEnum(local)];
    }
};

/// Count operand reads of every local reachable from `body`, walking all
/// successor edges. Definitions (statement targets and join parameters) are not
/// reads; only operand positions count.
pub fn countReachableReads(store: *LirStore, body: CFStmtId) Allocator.Error!ReadCounts {
    const counts = try store.allocator.alloc(u32, store.localCount());
    errdefer store.allocator.free(counts);
    @memset(counts, 0);

    var work = std.ArrayList(CFStmtId).empty;
    defer work.deinit(store.allocator);
    var visited = collections.DenseMap(CFStmtId, void).init(store.allocator);
    defer visited.deinit();

    try work.append(store.allocator, body);
    while (work.pop()) |stmt_id| {
        const entry = try visited.getOrPut(stmt_id);
        if (entry.found_existing) continue;

        countStmtReads(store, counts, store.getCFStmt(stmt_id));
        try appendSuccessors(store, &work, stmt_id);
    }

    return .{ .allocator = store.allocator, .counts = counts };
}

/// Add this statement's operand reads to an existing per-local count row.
/// Definitions are deliberately excluded, matching `countReachableReads`.
pub fn countStmtReads(store: *const LirStore, counts: []u32, stmt: LIR.CFStmt) void {
    switch (stmt) {
        .assign_ref => |s| switch (s.op) {
            .local => |source| noteRead(counts, source),
            .discriminant => |ref| noteRead(counts, ref.source),
            .field => |ref| noteRead(counts, ref.source),
            .tag_payload => |ref| noteRead(counts, ref.source),
            .tag_payload_struct => |ref| noteRead(counts, ref.source),
            .list_reinterpret => |ref| noteRead(counts, ref.backing_ref),
            .nominal => |ref| noteRead(counts, ref.backing_ref),
        },
        .assign_call => |s| {
            if (s.result_desc) |desc| noteDescRead(counts, desc);
            const args = store.getLocalSpan(s.args);
            for (0..args.len) |index| noteRead(counts, GuardedList.at(args, index));
        },
        .assign_call_erased => |s| {
            noteRead(counts, s.closure);
            noteSpanReads(store, counts, s.args);
            noteSpanReads(store, counts, s.arg_descs);
            if (s.result_desc) |desc| noteDescRead(counts, desc);
            if (s.reuse_source) |reuse_source| noteRead(counts, reuse_source);
        },
        .assign_packed_erased_fn => |s| {
            if (s.capture) |capture| noteRead(counts, capture);
            if (s.result_desc) |desc| noteDescRead(counts, desc);
            if (s.reuse) |reuse| noteRead(counts, reuse);
        },
        .assign_boxy_desc_ref => |s| {
            noteDescRead(counts, s.desc);
            if (s.tag_residual_for) |desc| noteDescRead(counts, desc);
            noteSpanReads(store, counts, s.captures);
        },
        .assign_boxy_dict_ref => |s| noteDictRead(counts, s.dict),
        .assign_boxy_box => |s| {
            noteRead(counts, s.payload);
            if (s.source_desc) |desc| noteDescRead(counts, desc);
            if (s.payload_desc) |desc| noteDescRead(counts, desc);
        },
        .assign_boxy_reuse_box => |s| {
            noteRead(counts, s.source);
            noteDescRead(counts, s.desc);
        },
        .assign_boxy_unbox => |s| {
            noteRead(counts, s.source);
            noteDescRead(counts, s.source_desc);
            if (s.target_desc) |desc| noteDescRead(counts, desc);
        },
        .assign_boxy_adapt => |s| {
            noteRead(counts, s.source);
            if (s.source_desc) |desc| noteDescRead(counts, desc);
            if (s.target_desc) |desc| noteDescRead(counts, desc);
        },
        .assign_boxy_inspect => |s| {
            noteRead(counts, s.source);
            noteDescRead(counts, s.source_desc);
        },
        .assign_boxy_eq => |s| {
            noteRead(counts, s.lhs);
            noteRead(counts, s.rhs);
            noteDescRead(counts, s.source_desc);
        },
        .assign_boxy_tag => |s| {
            noteDescRead(counts, s.target_desc);
            if (s.payload) |payload| noteRead(counts, payload);
            if (s.payload_desc) |desc| noteDescRead(counts, desc);
        },
        .assign_boxy_tag_payload => |s| {
            noteRead(counts, s.source);
            noteDescRead(counts, s.source_desc);
        },
        .boxy_tag_match => |s| {
            noteRead(counts, s.source);
            noteDescRead(counts, s.source_desc);
        },
        .assign_call_dict => |s| {
            noteDictRead(counts, s.dict);
            noteSpanReads(store, counts, s.args);
            noteSpanReads(store, counts, s.arg_descs);
            noteSpanReads(store, counts, s.hidden_args);
            if (s.result_desc) |desc| noteDescRead(counts, desc);
        },
        .assign_low_level => |s| {
            const args = store.getLocalSpan(s.args);
            for (0..args.len) |index| noteRead(counts, GuardedList.at(args, index));
        },
        .assign_list => |s| {
            const elems = store.getLocalSpan(s.elems);
            for (0..elems.len) |index| noteRead(counts, GuardedList.at(elems, index));
        },
        .assign_struct => |s| {
            const fields = store.getLocalSpan(s.fields);
            for (0..fields.len) |index| noteRead(counts, GuardedList.at(fields, index));
        },
        .assign_tag => |s| if (s.payload) |payload| noteRead(counts, payload),
        .store_struct => |s| {
            noteRead(counts, s.dest);
            const fields = store.getLocalSpan(s.fields);
            for (0..fields.len) |index| noteRead(counts, GuardedList.at(fields, index));
        },
        .store_tag => |s| {
            noteRead(counts, s.dest);
            if (s.payload) |payload| noteRead(counts, payload);
        },
        .set_local => |s| noteRead(counts, s.value),
        .debug => |s| noteRead(counts, s.message),
        .expect => |s| noteRead(counts, s.condition),
        .expect_err => |s| noteRead(counts, s.message),
        .switch_stmt => |s| noteRead(counts, s.cond),
        .switch_initialized_payload => |s| {
            noteRead(counts, s.cond);
            noteRead(counts, s.payload);
        },
        .str_match => |s| noteRead(counts, s.source),
        .str_match_set => |s| noteRead(counts, s.source),
        .ret => |s| noteRead(counts, s.value),
        .crash => |s| if (s.msg.localId()) |message| noteRead(counts, message),
        .incref => |s| noteRead(counts, s.value),
        .decref => |s| noteRead(counts, s.value),
        .decref_if_initialized => |s| {
            noteRead(counts, s.cond);
            noteRead(counts, s.value);
        },
        .free => |s| noteRead(counts, s.value),
        .init_uninitialized,
        .assign_literal,
        .comptime_branch_taken,
        .join,
        .jump,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .loop_continue,
        .loop_break,
        => {},
    }
}

fn noteRead(counts: []u32, local: LocalId) void {
    counts[@intFromEnum(local)] += 1;
}

fn noteSpanReads(store: *const LirStore, counts: []u32, span: LIR.LocalSpan) void {
    const locals = store.getLocalSpan(span);
    for (0..locals.len) |index| noteRead(counts, GuardedList.at(locals, index));
}

fn noteDescRead(counts: []u32, desc: LIR.BoxyDescRef) void {
    if (desc.localOrNull()) |local| noteRead(counts, local);
}

fn noteDictRead(counts: []u32, dict: LIR.BoxyDictRef) void {
    if (dict.localOrNull()) |local| noteRead(counts, local);
}

/// Compact a sorted slice of local ids in place, returning the length of the
/// deduplicated prefix. Callers sort with `localIdLessThan` first.
pub fn uniqueSortedLocals(items: []LocalId) usize {
    var unique_len: usize = 0;
    for (items, 0..) |local, idx| {
        if (idx > 0 and items[unique_len - 1] == local) continue;
        items[unique_len] = local;
        unique_len += 1;
    }
    return unique_len;
}

/// Order two local ids by their integer index, for `std.mem.sort`.
pub fn localIdLessThan(_: void, a: LocalId, b: LocalId) bool {
    return @intFromEnum(a) < @intFromEnum(b);
}

/// The body of a proc these passes may rewrite, or null when the proc has no
/// body of its own to clone: a hosted proc's implementation comes from the
/// host, and a non-Roc ABI proc's calling convention is fixed by its boundary.
pub fn rewritableProcBody(store: *const LirStore, proc_id: LIR.LirProcSpecId) ?CFStmtId {
    const proc = store.getProcSpec(proc_id);
    if (proc.hosted != null or proc.abi != .roc) return null;
    return proc.body;
}

/// Every statement reachable from a proc body, visited exactly once. Shared
/// join targets are reached through whichever predecessor arrives first.
pub const ReachableStmts = struct {
    store: *LirStore,
    work: std.ArrayList(CFStmtId),
    visited: collections.DenseMap(CFStmtId, void),

    /// Start a walk rooted at `body`.
    pub fn init(store: *LirStore, body: CFStmtId) Allocator.Error!ReachableStmts {
        var work = std.ArrayList(CFStmtId).empty;
        errdefer work.deinit(store.allocator);
        try work.append(store.allocator, body);
        return .{
            .store = store,
            .work = work,
            .visited = collections.DenseMap(CFStmtId, void).init(store.allocator),
        };
    }

    /// Release the walk's scratch storage.
    pub fn deinit(self: *ReachableStmts) void {
        self.work.deinit(self.store.allocator);
        self.visited.deinit();
    }

    /// The next unvisited statement, or null when the walk is done.
    pub fn next(self: *ReachableStmts) Allocator.Error!?CFStmtId {
        while (self.work.pop()) |stmt_id| {
            const entry = try self.visited.getOrPut(stmt_id);
            if (entry.found_existing) continue;
            try appendSuccessors(self.store, &self.work, stmt_id);
            return stmt_id;
        }
        return null;
    }
};

/// Return one flag per store local identifying definitions reachable from
/// `body`. Clone passes use this to give definitions fresh identities while
/// retaining read-only external inputs.
pub fn collectReachableDefinitions(store: *LirStore, body: CFStmtId) Allocator.Error![]bool {
    const defined = try store.allocator.alloc(bool, store.localCount());
    errdefer store.allocator.free(defined);
    @memset(defined, false);

    var walk = try ReachableStmts.init(store, body);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| markStmtDefinitions(store, defined, stmt_id);
    return defined;
}

/// Add every local defined by `stmt_id` to an existing definition set.
pub fn markStmtDefinitions(store: *const LirStore, defined: []bool, stmt_id: CFStmtId) void {
    switch (store.getCFStmt(stmt_id)) {
        inline .init_uninitialized,
        .assign_ref,
        .assign_literal,
        .assign_packed_erased_fn,
        .assign_boxy_desc_ref,
        .assign_boxy_dict_ref,
        .assign_boxy_box,
        .assign_boxy_reuse_box,
        .assign_boxy_unbox,
        .assign_boxy_adapt,
        .assign_boxy_inspect,
        .assign_boxy_eq,
        .assign_boxy_tag,
        .assign_call_dict,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        => |stmt| defined[@intFromEnum(stmt.target)] = true,
        .assign_call => |stmt| {
            defined[@intFromEnum(stmt.target)] = true;
            if (stmt.out_desc) |out_desc| defined[@intFromEnum(out_desc)] = true;
        },
        .assign_call_erased => |stmt| {
            defined[@intFromEnum(stmt.target)] = true;
            if (stmt.out_desc) |out_desc| defined[@intFromEnum(out_desc)] = true;
        },
        .assign_boxy_tag_payload => |stmt| {
            defined[@intFromEnum(stmt.target)] = true;
            if (stmt.target_desc) |target_desc| defined[@intFromEnum(target_desc)] = true;
        },
        .join => |join| {
            const params = store.getLocalSpan(join.params);
            for (0..params.len) |index| defined[@intFromEnum(GuardedList.at(params, index))] = true;
            const maybe_uninitialized = store.getLocalSpan(join.maybe_uninitialized_params);
            for (0..maybe_uninitialized.len) |index| defined[@intFromEnum(GuardedList.at(maybe_uninitialized, index))] = true;
        },
        .str_match => |str_match| markStrMatchDefinitions(store, defined, str_match.steps),
        .str_match_set => |str_match_set| {
            const arms = store.getStrMatchArms(str_match_set.arms);
            for (0..arms.len) |index| {
                markStrMatchDefinitions(store, defined, GuardedList.at(arms, index).steps);
            }
        },
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
        .boxy_tag_match,
        .loop_continue,
        .loop_break,
        .jump,
        .ret,
        .crash,
        => {},
    }
}

fn markStrMatchDefinitions(store: *const LirStore, defined: []bool, span: LIR.StrMatchStepSpan) void {
    const steps = store.getStrMatchSteps(span);
    for (0..steps.len) |index| switch (GuardedList.at(steps, index).capture) {
        .discard => {},
        .view => |local| defined[@intFromEnum(local)] = true,
    };
}

/// Whether every local in `chain` has exactly one read across the proc.
///
/// This is the soundness guard for fusing a call with the statement that
/// consumes its result: the result is aliased or consumed exactly once, each
/// alias feeds the next link exactly once, and the final value is the matched
/// consumer's only reader. Any extra read means the fusion would orphan a
/// still-live local, so both fusion passes must ask exactly this question.
pub fn chainIsSingleUse(store: *LirStore, proc_body: CFStmtId, chain: []const LocalId) Allocator.Error!bool {
    var reads = try countReachableReads(store, proc_body);
    defer reads.deinit();
    for (chain) |local| {
        if (reads.get(local) != 1) return false;
    }
    return true;
}

/// What a cloned call variant adds to the proc it was cloned from.
pub const CallVariantSpec = struct {
    /// Locals prepended to the variant's argument list, ahead of the source
    /// proc's own arguments. The fused call passes these at the call site.
    leading_args: []const LocalId,
    /// Locals the variant's frame needs that are neither its arguments nor
    /// created by cloning the body.
    extra_frame_locals: []const LocalId = &.{},
    /// The variant's return layout, which the rewritten returns produce.
    ret_layout: layout_mod.Idx,
};

/// Clone `source` into an internal variant whose returns are rewritten by
/// `rewriter` and whose arguments are `spec.leading_args` followed by the
/// source's own. The source proc is left untouched; callers cache the result
/// so one variant serves every fused call site.
pub fn cloneCallVariant(
    comptime Rewriter: type,
    store: *LirStore,
    source: LIR.LirProcSpecId,
    rewriter: Rewriter,
    spec: CallVariantSpec,
) Allocator.Error!LIR.LirProcSpecId {
    const source_spec = store.getProcSpec(source);
    const source_body = source_spec.body orelse
        @panic("call-variant clone reached a proc with no body");
    const source_args = store.getLocalSpan(source_spec.args);

    var variant_args = try std.ArrayList(LocalId).initCapacity(
        store.allocator,
        source_args.len + spec.leading_args.len,
    );
    defer variant_args.deinit(store.allocator);
    variant_args.appendSliceAssumeCapacity(spec.leading_args);

    for (0..source_args.len) |index| {
        const source_arg = GuardedList.at(source_args, index);
        const arg = try store.addLocal(.{ .layout_idx = store.getLocal(source_arg).layout_idx });
        variant_args.appendAssumeCapacity(arg);
    }

    var cloner = try BodyCloner(Rewriter).init(store, rewriter);
    defer cloner.deinit();

    for (0..source_args.len) |index| {
        const source_arg = GuardedList.at(source_args, index);
        cloner.local_map[@intFromEnum(source_arg)] = variant_args.items[index + spec.leading_args.len];
    }

    try cloner.new_locals.appendSlice(store.allocator, variant_args.items);
    try cloner.new_locals.appendSlice(store.allocator, spec.extra_frame_locals);

    const source_frame = store.getLocalSpan(source_spec.frame_locals);
    for (0..source_frame.len) |index| {
        _ = try cloner.mapLocal(GuardedList.at(source_frame, index));
    }

    const body = try cloner.cloneStmt(source_body);
    const erased_reuse_arg = if (source_spec.erased_reuse_arg) |source_arg|
        try cloner.mapLocal(source_arg)
    else
        null;

    var frame_locals = try std.ArrayList(LocalId).initCapacity(store.allocator, cloner.new_locals.items.len);
    defer frame_locals.deinit(store.allocator);
    frame_locals.appendSliceAssumeCapacity(cloner.new_locals.items);
    std.mem.sort(LocalId, frame_locals.items, {}, localIdLessThan);
    const unique_len = uniqueSortedLocals(frame_locals.items);

    const variant = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(variant_args.items),
        .erased_reuse_arg = erased_reuse_arg,
        .frame_locals = try store.addLocalSpan(frame_locals.items[0..unique_len]),
        .body = body,
        .ret_layout = spec.ret_layout,
        .abi = .roc,
    });
    try store.copyProcDebugInfo(variant, source);

    return variant;
}

/// Clone a source proc body into fresh statements and locals, delegating return
/// handling to `Rewriter`.
///
/// `Rewriter` carries the pass-specific destination state and supplies the
/// hooks that diverge between passes:
///
///   * `cloneRet(self: *Rewriter, cloner: anytype, value: LocalId)`—required.
///     Produces the cloned tail for a source `ret value`.
///   * `interceptStmt(self: *Rewriter, cloner: anytype, stmt: LIR.CFStmt)`—
///     optional. Returns a cloned statement id to short-circuit the default
///     clone, letting the pass fuse a direct constructor/concat return into
///     the tail, or `null` to fall through to the ordinary clone.
///
/// Both hooks receive the cloner and use its `mapLocal`, `mapLocalSpan`,
/// `addTemp`, `directReturnOf`, and `store` surface to build their statements.
pub fn BodyCloner(comptime Rewriter: type) type {
    return struct {
        const Self = @This();

        /// The store the clone is written into.
        store: *LirStore,
        /// Pass-specific return rewriter and its destination state.
        rewriter: Rewriter,
        /// Old-local index to cloned-local, `null` until first mapped.
        local_map: []?LocalId,
        stmt_map: collections.DenseMap(CFStmtId, CFStmtId),
        /// Every local created by this clone, in creation order.
        new_locals: std.ArrayList(LocalId),
        /// Optional virtual frame under which cloned source scopes are rebased.
        inline_scope_outer: LIR.InlineScopeId,
        inline_scope_map: []?LIR.InlineScopeId,
        /// Which join identities must be alpha-renamed. Whole-procedure
        /// inlining owns every jump target and remaps all of them. A cloned
        /// subtree remaps only declarations structurally contained in that
        /// subtree, preserving jumps to lexically enclosing joins.
        join_remap: enum { none, all, declared },
        declared_joins: collections.DenseMap(LIR.JoinPointId, void),
        join_params: ?*JoinParamIndex,
        join_map: collections.DenseMap(LIR.JoinPointId, LIR.JoinPointId),
        next_join_point: u32,

        /// Allocate the clone state sized for the store's current locals.
        pub fn init(store: *LirStore, rewriter: Rewriter) Allocator.Error!Self {
            return initInternal(store, rewriter, LIR.InlineScopeId.none, .none, null);
        }

        /// Allocate clone state and rebase every cloned source location below
        /// `outer`. Procedure variants pass `.none`; true call-site inlining
        /// supplies the virtual frame representing the removed call.
        pub fn initWithInlineScopeOuter(
            store: *LirStore,
            rewriter: Rewriter,
            outer: LIR.InlineScopeId,
        ) Allocator.Error!Self {
            return initInternal(store, rewriter, outer, .all, null);
        }

        /// Clone a control-flow subtree within its current procedure. Join
        /// declarations owned by the subtree receive fresh identities, while
        /// jumps to lexically enclosing declarations retain their targets.
        pub fn initWithFreshDeclaredJoins(
            store: *LirStore,
            rewriter: Rewriter,
            body: CFStmtId,
            join_params: *JoinParamIndex,
        ) Allocator.Error!Self {
            var self = try initInternal(store, rewriter, LIR.InlineScopeId.none, .declared, join_params);
            errdefer self.deinit();
            var walk = try ReachableStmts.init(store, body);
            defer walk.deinit();
            while (try walk.next()) |stmt_id| {
                const stmt = store.getCFStmt(stmt_id);
                if (stmt == .join) try self.declared_joins.put(stmt.join.id, {});
            }
            return self;
        }

        fn initInternal(
            store: *LirStore,
            rewriter: Rewriter,
            outer: LIR.InlineScopeId,
            join_remap: @FieldType(Self, "join_remap"),
            join_params: ?*JoinParamIndex,
        ) Allocator.Error!Self {
            const local_map = try store.allocator.alloc(?LocalId, store.localCount());
            errdefer store.allocator.free(local_map);
            @memset(local_map, null);
            const inline_scope_map = try store.allocator.alloc(?LIR.InlineScopeId, store.inlineScopeCount());
            @memset(inline_scope_map, null);
            return .{
                .store = store,
                .rewriter = rewriter,
                .local_map = local_map,
                .stmt_map = collections.DenseMap(CFStmtId, CFStmtId).init(store.allocator),
                .new_locals = .empty,
                .inline_scope_outer = outer,
                .inline_scope_map = inline_scope_map,
                .join_remap = join_remap,
                .declared_joins = collections.DenseMap(LIR.JoinPointId, void).init(store.allocator),
                .join_params = join_params,
                .join_map = collections.DenseMap(LIR.JoinPointId, LIR.JoinPointId).init(store.allocator),
                .next_join_point = if (join_remap != .none) nextJoinPointRaw(store) else 0,
            };
        }

        /// Release the clone's scratch storage.
        pub fn deinit(self: *Self) void {
            self.new_locals.deinit(self.store.allocator);
            self.stmt_map.deinit();
            self.join_map.deinit();
            self.declared_joins.deinit();
            self.store.allocator.free(self.inline_scope_map);
            self.store.allocator.free(self.local_map);
        }

        /// Clone `old_id` and everything reachable from it, memoizing by
        /// original statement id so shared join targets are cloned once.
        pub fn cloneStmt(self: *Self, old_id: CFStmtId) Allocator.Error!CFStmtId {
            if (self.stmt_map.get(old_id)) |existing| return existing;

            const saved_loc = self.store.current_loc;
            defer self.store.current_loc = saved_loc;
            const saved_region = self.store.current_region;
            defer self.store.current_region = saved_region;
            const saved_inline_scope = self.store.current_inline_scope;
            defer self.store.current_inline_scope = saved_inline_scope;
            self.store.current_loc = self.store.stmtLoc(old_id);
            self.store.current_region = self.store.stmtRegion(old_id);
            self.store.current_inline_scope = try self.mapInlineScope(self.store.stmtInlineScope(old_id));

            const stmt = self.store.getCFStmt(old_id);
            if (@hasDecl(Rewriter, "interceptStmt")) {
                if (try self.rewriter.interceptStmt(self, stmt)) |intercepted| {
                    try self.stmt_map.put(old_id, intercepted);
                    return intercepted;
                }
            }

            const cloned = switch (stmt) {
                .init_uninitialized => |s| try self.store.addCFStmt(.{ .init_uninitialized = .{
                    .target = try self.mapLocal(s.target),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_ref => |s| try self.store.addCFStmt(.{ .assign_ref = .{
                    .target = try self.mapLocal(s.target),
                    .op = try self.mapRefOp(s.op),
                    .take_kind = s.take_kind,
                    .residual_shell_absent_fields = s.residual_shell_absent_fields,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_literal => |s| try self.store.addCFStmt(.{ .assign_literal = .{
                    .target = try self.mapLocal(s.target),
                    .value = s.value,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_call => |s| try self.store.addCFStmt(.{ .assign_call = .{
                    .target = try self.mapLocal(s.target),
                    .proc = s.proc,
                    .args = try self.mapLocalSpan(s.args),
                    .result_desc = try self.mapMaybeBoxyDescRef(s.result_desc),
                    .out_desc = try self.mapMaybeLocal(s.out_desc),
                    .is_cold = s.is_cold,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_call_erased => |s| try self.store.addCFStmt(.{ .assign_call_erased = .{
                    .target = try self.mapLocal(s.target),
                    .closure = try self.mapLocal(s.closure),
                    .args = try self.mapLocalSpan(s.args),
                    .arg_layouts = s.arg_layouts,
                    .arg_descs = try self.mapLocalSpan(s.arg_descs),
                    .arg_desc_keys = s.arg_desc_keys,
                    .result_desc = try self.mapMaybeBoxyDescRef(s.result_desc),
                    .out_desc = try self.mapMaybeLocal(s.out_desc),
                    .arg_plan = s.arg_plan,
                    .reuse_closure = s.reuse_closure,
                    .reuse_source = try self.mapMaybeLocal(s.reuse_source),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_packed_erased_fn => |s| try self.store.addCFStmt(.{ .assign_packed_erased_fn = .{
                    .target = try self.mapLocal(s.target),
                    .proc = s.proc,
                    .capture = try self.mapMaybeLocal(s.capture),
                    .capture_layout = s.capture_layout,
                    .on_drop = s.on_drop,
                    .result_desc = try self.mapMaybeBoxyDescRef(s.result_desc),
                    .reuse = try self.mapMaybeLocal(s.reuse),
                    .reuse_unique = s.reuse_unique,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_desc_ref => |s| try self.store.addCFStmt(.{ .assign_boxy_desc_ref = .{
                    .target = try self.mapLocal(s.target),
                    .desc = try self.mapBoxyDescRef(s.desc),
                    .nested_index = s.nested_index,
                    .box_payload_layout = s.box_payload_layout,
                    .tag_payload = s.tag_payload,
                    .tag_ext = s.tag_ext,
                    .tag_residual_for = try self.mapMaybeBoxyDescRef(s.tag_residual_for),
                    .captures = try self.mapLocalSpan(s.captures),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_dict_ref => |s| try self.store.addCFStmt(.{ .assign_boxy_dict_ref = .{
                    .target = try self.mapLocal(s.target),
                    .dict = try self.mapBoxyDictRef(s.dict),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_box => |s| try self.store.addCFStmt(.{ .assign_boxy_box = .{
                    .target = try self.mapLocal(s.target),
                    .payload = try self.mapLocal(s.payload),
                    .payload_layout = s.payload_layout,
                    .source_desc = try self.mapMaybeBoxyDescRef(s.source_desc),
                    .payload_desc = try self.mapMaybeBoxyDescRef(s.payload_desc),
                    .payload_mode = s.payload_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_reuse_box => |s| try self.store.addCFStmt(.{ .assign_boxy_reuse_box = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .desc = try self.mapBoxyDescRef(s.desc),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_unbox => |s| try self.store.addCFStmt(.{ .assign_boxy_unbox = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .target_desc = try self.mapMaybeBoxyDescRef(s.target_desc),
                    .target_layout = s.target_layout,
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_adapt => |s| try self.store.addCFStmt(.{ .assign_boxy_adapt = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .adapter = s.adapter,
                    .source_desc = try self.mapMaybeBoxyDescRef(s.source_desc),
                    .target_desc = try self.mapMaybeBoxyDescRef(s.target_desc),
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_inspect => |s| try self.store.addCFStmt(.{ .assign_boxy_inspect = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_eq => |s| try self.store.addCFStmt(.{ .assign_boxy_eq = .{
                    .target = try self.mapLocal(s.target),
                    .lhs = try self.mapLocal(s.lhs),
                    .rhs = try self.mapLocal(s.rhs),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_tag => |s| try self.store.addCFStmt(.{ .assign_boxy_tag = .{
                    .target = try self.mapLocal(s.target),
                    .target_desc = try self.mapBoxyDescRef(s.target_desc),
                    .tag_name = s.tag_name,
                    .payload = try self.mapMaybeLocal(s.payload),
                    .payload_layout = s.payload_layout,
                    .payload_desc = try self.mapMaybeBoxyDescRef(s.payload_desc),
                    .payload_mode = s.payload_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_tag_payload => |s| try self.store.addCFStmt(.{ .assign_boxy_tag_payload = .{
                    .target = try self.mapLocal(s.target),
                    .target_desc = try self.mapMaybeLocal(s.target_desc),
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .tag_name = s.tag_name,
                    .payload_index = s.payload_index,
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .boxy_tag_match => |s| try self.store.addCFStmt(.{ .boxy_tag_match = .{
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .tag_name = s.tag_name,
                    .on_match = try self.cloneStmt(s.on_match),
                    .on_miss = try self.cloneStmt(s.on_miss),
                } }),
                .assign_call_dict => |s| try self.store.addCFStmt(.{ .assign_call_dict = .{
                    .target = try self.mapLocal(s.target),
                    .dict = try self.mapBoxyDictRef(s.dict),
                    .method = s.method,
                    .method_slot = s.method_slot,
                    .args = try self.mapLocalSpan(s.args),
                    .arg_descs = try self.mapLocalSpan(s.arg_descs),
                    .hidden_args = try self.mapLocalSpan(s.hidden_args),
                    .result_desc = try self.mapMaybeBoxyDescRef(s.result_desc),
                    .is_cold = s.is_cold,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_low_level => |s| try self.store.addCFStmt(.{ .assign_low_level = .{
                    .target = try self.mapLocal(s.target),
                    .op = s.op,
                    .rc_effect = s.rc_effect,
                    .unique_args = s.unique_args,
                    .interchangeable = s.interchangeable,
                    .simd_concat_count = s.simd_concat_count,
                    .args = try self.mapLocalSpan(s.args),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_list => |s| try self.store.addCFStmt(.{ .assign_list = .{
                    .target = try self.mapLocal(s.target),
                    .elems = try self.mapLocalSpan(s.elems),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_struct => |s| try self.store.addCFStmt(.{ .assign_struct = .{
                    .target = try self.mapLocal(s.target),
                    .fields = try self.mapLocalSpan(s.fields),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_tag => |s| try self.store.addCFStmt(.{ .assign_tag = .{
                    .target = try self.mapLocal(s.target),
                    .variant_index = s.variant_index,
                    .discriminant = s.discriminant,
                    .payload = try self.mapMaybeLocal(s.payload),
                    .next = try self.cloneStmt(s.next),
                } }),
                .store_struct => |s| try self.store.addCFStmt(.{ .store_struct = .{
                    .dest = try self.mapLocal(s.dest),
                    .struct_layout = s.struct_layout,
                    .fields = try self.mapLocalSpan(s.fields),
                    .next = try self.cloneStmt(s.next),
                } }),
                .store_tag => |s| try self.store.addCFStmt(.{ .store_tag = .{
                    .dest = try self.mapLocal(s.dest),
                    .tag_layout = s.tag_layout,
                    .variant_index = s.variant_index,
                    .discriminant = s.discriminant,
                    .payload = try self.mapMaybeLocal(s.payload),
                    .next = try self.cloneStmt(s.next),
                } }),
                .set_local => |s| try self.store.addCFStmt(.{ .set_local = .{
                    .target = try self.mapLocal(s.target),
                    .value = try self.mapLocal(s.value),
                    .mode = s.mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .debug => |s| try self.store.addCFStmt(.{ .debug = .{
                    .message = try self.mapLocal(s.message),
                    .next = try self.cloneStmt(s.next),
                } }),
                .expect => |s| try self.store.addCFStmt(.{ .expect = .{
                    .condition = try self.mapLocal(s.condition),
                    .site = s.site,
                    .next = try self.cloneStmt(s.next),
                } }),
                .expect_err => |s| try self.store.addCFStmt(.{ .expect_err = .{
                    .message = try self.mapLocal(s.message),
                    .region = s.region,
                } }),
                .runtime_error => try self.store.addCFStmt(.runtime_error),
                .comptime_exhaustiveness_failed => |s| try self.store.addCFStmt(.{ .comptime_exhaustiveness_failed = .{
                    .site = s.site,
                } }),
                .comptime_branch_taken => |s| try self.store.addCFStmt(.{ .comptime_branch_taken = .{
                    .site = s.site,
                    .branch_index = s.branch_index,
                    .next = try self.cloneStmt(s.next),
                } }),
                .incref => |s| try self.store.addCFStmt(.{ .incref = .{
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .count = s.count,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }),
                .decref => |s| try self.store.addCFStmt(.{ .decref = .{
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }),
                .decref_if_initialized => |s| try self.store.addCFStmt(.{ .decref_if_initialized = .{
                    .cond = try self.mapLocal(s.cond),
                    .cond_mask = s.cond_mask,
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }),
                .free => |s| try self.store.addCFStmt(.{ .free = .{
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }),
                .switch_stmt => |s| try self.cloneSwitch(s),
                .switch_initialized_payload => |s| try self.store.addCFStmt(.{ .switch_initialized_payload = .{
                    .cond = try self.mapLocal(s.cond),
                    .cond_mask = s.cond_mask,
                    .payload = try self.mapLocal(s.payload),
                    .uninitialized_is_cold = s.uninitialized_is_cold,
                    .initialized_branch = try self.cloneStmt(s.initialized_branch),
                    .uninitialized_branch = try self.cloneStmt(s.uninitialized_branch),
                } }),
                .str_match => |s| try self.store.addCFStmt(.{ .str_match = .{
                    .source = try self.mapLocal(s.source),
                    .prefix = s.prefix,
                    .steps = try self.mapStrMatchSteps(s.steps),
                    .end = s.end,
                    .on_match = try self.cloneStmt(s.on_match),
                    .on_miss = try self.cloneStmt(s.on_miss),
                } }),
                .str_match_set => |s| try self.cloneStrMatchSet(s),
                .loop_continue => try self.store.addCFStmt(.loop_continue),
                .loop_break => try self.store.addCFStmt(.loop_break),
                .join => |s| try self.cloneJoin(s),
                .jump => |s| try self.cloneJump(s),
                .ret => |s| try self.rewriter.cloneRet(self, s.value),
                .crash => |s| try self.store.addCFStmt(.{ .crash = .{ .msg = switch (s.msg) {
                    .literal => |literal| .{ .literal = literal },
                    .local => |local| .{ .local = try self.mapLocal(local) },
                } } }),
            };

            try self.stmt_map.put(old_id, cloned);
            return cloned;
        }

        fn cloneJoin(self: *Self, join: @FieldType(LIR.CFStmt, "join")) Allocator.Error!CFStmtId {
            const cloned = try self.store.addCFStmt(.{ .join = .{
                .id = try self.mapJoinPoint(join.id),
                .params = try self.mapLocalSpan(join.params),
                .retained = try self.mapLocalSpan(join.retained),
                .maybe_uninitialized_params = try self.mapLocalSpan(join.maybe_uninitialized_params),
                .maybe_uninitialized_conditions = try self.mapLocalSpan(join.maybe_uninitialized_conditions),
                .maybe_uninitialized_condition_masks = join.maybe_uninitialized_condition_masks,
                .body = try self.cloneStmt(join.body),
                .remainder = try self.cloneStmt(join.remainder),
            } });
            if (self.join_params) |params| try params.record(self.store.getCFStmt(cloned).join);
            return cloned;
        }

        /// A declared-subtree clone can alpha-rename a local that is also a
        /// parameter of an enclosing join. Jumps encode their arguments as
        /// preceding writes, so bridge each remapped value back into the
        /// enclosing join's original parameter before leaving the clone.
        fn cloneJump(self: *Self, jump: @FieldType(LIR.CFStmt, "jump")) Allocator.Error!CFStmtId {
            var next = try self.store.addCFStmt(.{ .jump = .{ .target = try self.mapJoinPoint(jump.target) } });
            if (self.join_remap != .declared or self.declared_joins.contains(jump.target)) return next;

            const params = self.join_params.?.get(jump.target) orelse
                @panic("subtree clone jumped to an unknown external join");
            next = try self.bridgeExternalJoinParams(params, next);
            return next;
        }

        fn bridgeExternalJoinParams(self: *Self, span: LIR.LocalSpan, tail: CFStmtId) Allocator.Error!CFStmtId {
            var next = tail;
            const params = self.store.getLocalSpan(span);
            var index = params.len;
            while (index > 0) {
                index -= 1;
                const param = GuardedList.at(params, index);
                const mapped = self.local_map[@intFromEnum(param)] orelse {
                    self.local_map[@intFromEnum(param)] = param;
                    continue;
                };
                if (mapped == param) continue;
                next = try self.store.addCFStmt(.{ .set_local = .{
                    .target = param,
                    .value = mapped,
                    .mode = .initialize_join_param,
                    .next = next,
                } });
            }
            return next;
        }

        /// True when `next` is a `ret` of exactly `value`, marking a direct
        /// constructor/concat return the rewriter may fuse into its tail.
        pub fn directReturnOf(self: *const Self, next: CFStmtId, value: LocalId) bool {
            const stmt = self.store.getCFStmt(next);
            return stmt == .ret and stmt.ret.value == value;
        }

        fn cloneSwitch(self: *Self, s: anytype) Allocator.Error!CFStmtId {
            // Recursive body cloning appends switch branches and can therefore
            // invalidate a guarded borrow of this same backing list. Own the
            // source descriptors before cloning any branch body.
            const old_branches = try GuardedList.dupe(
                self.store.allocator,
                LIR.CFSwitchBranch,
                self.store.getCFSwitchBranches(s.branches),
            );
            defer self.store.allocator.free(old_branches);
            const branches = try self.store.allocator.alloc(LIR.CFSwitchBranch, old_branches.len);
            defer self.store.allocator.free(branches);
            for (0..old_branches.len) |index| {
                const old = old_branches[index];
                const new = &branches[index];
                new.* = .{
                    .value = old.value,
                    .body = try self.cloneStmt(old.body),
                };
            }
            return try self.store.addCFStmt(.{ .switch_stmt = .{
                .cond = try self.mapLocal(s.cond),
                .branches = try self.store.addCFSwitchBranches(branches),
                .default_branch = try self.cloneStmt(s.default_branch),
                .default_is_cold = s.default_is_cold,
                .continuation = if (s.continuation) |continuation| try self.cloneStmt(continuation) else null,
            } });
        }

        fn cloneStrMatchSet(self: *Self, s: anytype) Allocator.Error!CFStmtId {
            // Cloning an arm can append nested string-match arms. Snapshot the
            // source descriptors so those appends cannot invalidate the input.
            const old_arms = try GuardedList.dupe(
                self.store.allocator,
                LIR.StrMatchArm,
                self.store.getStrMatchArms(s.arms),
            );
            defer self.store.allocator.free(old_arms);
            const arms = try self.store.allocator.alloc(LIR.StrMatchArm, old_arms.len);
            defer self.store.allocator.free(arms);
            for (0..old_arms.len) |index| {
                const old = old_arms[index];
                const new = &arms[index];
                new.* = .{
                    .prefix = old.prefix,
                    .steps = try self.mapStrMatchSteps(old.steps),
                    .end = old.end,
                    .on_match = try self.cloneStmt(old.on_match),
                };
            }
            return try self.store.addCFStmt(.{ .str_match_set = .{
                .source = try self.mapLocal(s.source),
                .arms = try self.store.addStrMatchArms(arms),
                .on_miss = try self.cloneStmt(s.on_miss),
            } });
        }

        fn mapStrMatchSteps(self: *Self, span: LIR.StrMatchStepSpan) Allocator.Error!LIR.StrMatchStepSpan {
            const old_steps = self.store.getStrMatchSteps(span);
            const steps = try self.store.allocator.alloc(LIR.StrMatchStep, old_steps.len);
            defer self.store.allocator.free(steps);
            for (0..old_steps.len) |index| {
                const old = GuardedList.at(old_steps, index);
                const new = &steps[index];
                new.* = old;
                new.capture = switch (old.capture) {
                    .discard => .discard,
                    .view => |local| .{ .view = try self.mapLocal(local) },
                };
            }
            return try self.store.addStrMatchSteps(steps);
        }

        fn mapRefOp(self: *Self, op: LIR.RefOp) Allocator.Error!LIR.RefOp {
            return switch (op) {
                .local => |local| .{ .local = try self.mapLocal(local) },
                .discriminant => |d| .{ .discriminant = .{ .source = try self.mapLocal(d.source) } },
                .field => |f| .{ .field = .{
                    .source = try self.mapLocal(f.source),
                    .field_idx = f.field_idx,
                } },
                .tag_payload => |t| .{ .tag_payload = .{
                    .source = try self.mapLocal(t.source),
                    .payload_idx = t.payload_idx,
                    .variant_index = t.variant_index,
                    .tag_discriminant = t.tag_discriminant,
                } },
                .tag_payload_struct => |t| .{ .tag_payload_struct = .{
                    .source = try self.mapLocal(t.source),
                    .variant_index = t.variant_index,
                    .tag_discriminant = t.tag_discriminant,
                } },
                .list_reinterpret => |l| .{ .list_reinterpret = .{ .backing_ref = try self.mapLocal(l.backing_ref) } },
                .nominal => |n| .{ .nominal = .{ .backing_ref = try self.mapLocal(n.backing_ref) } },
            };
        }

        /// Clone a local-id span, remapping each element.
        pub fn mapLocalSpan(self: *Self, span: LIR.LocalSpan) Allocator.Error!LIR.LocalSpan {
            const old_locals = self.store.getLocalSpan(span);
            const locals = try self.store.allocator.alloc(LocalId, old_locals.len);
            defer self.store.allocator.free(locals);
            for (0..old_locals.len) |index| {
                locals[index] = try self.mapLocal(GuardedList.at(old_locals, index));
            }
            return try self.store.addLocalSpan(locals);
        }

        /// Remap an optional local, preserving `null`.
        pub fn mapMaybeLocal(self: *Self, maybe: ?LocalId) Allocator.Error!?LocalId {
            return if (maybe) |local| try self.mapLocal(local) else null;
        }

        fn mapBoxyDescRef(self: *Self, desc: LIR.BoxyDescRef) Allocator.Error!LIR.BoxyDescRef {
            return switch (desc) {
                .static => |id| .{ .static = id },
                .local => |local| .{ .local = try self.mapLocal(local) },
                .runtime => |index| .{ .runtime = index },
                .dict_method_arg => |arg| .{ .dict_method_arg = arg },
                .dict_method_hidden => |hidden| .{ .dict_method_hidden = hidden },
            };
        }

        fn mapMaybeBoxyDescRef(self: *Self, maybe: ?LIR.BoxyDescRef) Allocator.Error!?LIR.BoxyDescRef {
            return if (maybe) |desc| try self.mapBoxyDescRef(desc) else null;
        }

        fn mapBoxyDictRef(self: *Self, dict: LIR.BoxyDictRef) Allocator.Error!LIR.BoxyDictRef {
            return switch (dict) {
                .static => |id| .{ .static = id },
                .local => |local| .{ .local = try self.mapLocal(local) },
            };
        }

        /// Map an old local to its clone, allocating a fresh same-layout local
        /// on first encounter.
        pub fn mapLocal(self: *Self, old: LocalId) Allocator.Error!LocalId {
            const index = @intFromEnum(old);
            if (index >= self.local_map.len) unreachable;
            if (self.local_map[index]) |existing| return existing;

            const old_local = self.store.getLocal(old);
            const fresh = try self.store.addLocal(.{ .layout_idx = old_local.layout_idx });
            self.local_map[index] = fresh;
            const boxy_desc = try self.mapMaybeBoxyDescRef(old_local.boxy_desc);
            if (boxy_desc) |desc| self.store.setLocalBoxyDesc(fresh, desc);
            try self.new_locals.append(self.store.allocator, fresh);
            return fresh;
        }

        /// Allocate a fresh local of `layout_idx` owned by the clone.
        pub fn addTemp(self: *Self, layout_idx: layout_mod.Idx) Allocator.Error!LocalId {
            const local = try self.store.addLocal(.{ .layout_idx = layout_idx });
            try self.new_locals.append(self.store.allocator, local);
            return local;
        }

        fn mapInlineScope(self: *Self, old: LIR.InlineScopeId) Allocator.Error!LIR.InlineScopeId {
            if (self.inline_scope_outer == LIR.InlineScopeId.none) return old;
            if (old == LIR.InlineScopeId.none) return self.inline_scope_outer;

            const index = @intFromEnum(old);
            if (index >= self.inline_scope_map.len) unreachable;
            if (self.inline_scope_map[index]) |existing| return existing;

            const source = self.store.inlineScope(old);
            const mapped = try self.store.addInlineScope(.{
                .source_symbol = source.source_symbol,
                .source_name = source.source_name,
                .source_loc = source.source_loc,
                .call_site = source.call_site,
                .parent = try self.mapInlineScope(source.parent),
            });
            self.inline_scope_map[index] = mapped;
            return mapped;
        }

        fn mapJoinPoint(self: *Self, old: LIR.JoinPointId) Allocator.Error!LIR.JoinPointId {
            switch (self.join_remap) {
                .none => return old,
                .all => {},
                .declared => if (!self.declared_joins.contains(old)) return old,
            }
            const entry = try self.join_map.getOrPut(old);
            if (!entry.found_existing) {
                if (self.next_join_point == std.math.maxInt(u32)) @panic("join-point id space exhausted");
                entry.value_ptr.* = @enumFromInt(self.next_join_point);
                self.next_join_point += 1;
            }
            return entry.value_ptr.*;
        }
    };
}

fn nextJoinPointRaw(store: *const LirStore) u32 {
    var next: u32 = 0;
    for (store.getCFStmts()) |stmt| {
        if (stmt != .join) continue;
        const raw = @intFromEnum(stmt.join.id);
        if (raw == std.math.maxInt(u32)) @panic("join-point id space exhausted");
        next = @max(next, raw + 1);
    }
    return next;
}

const TestRetRewriter = struct {
    pub fn cloneRet(_: *TestRetRewriter, cloner: anytype, value: LocalId) Allocator.Error!CFStmtId {
        return try cloner.store.addCFStmt(.{ .ret = .{ .value = try cloner.mapLocal(value) } });
    }
};

test "subtree clone bridges renamed parameters before an external join" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();

    const shared_param = try store.addLocal(.{ .layout_idx = .u64 });
    const external_id: LIR.JoinPointId = @enumFromInt(1);
    const internal_id: LIR.JoinPointId = @enumFromInt(2);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = shared_param } });
    const jump_external = try store.addCFStmt(.{ .jump = .{ .target = external_id } });
    const jump_internal = try store.addCFStmt(.{ .jump = .{ .target = internal_id } });
    const internal_stmt = try store.addCFStmt(.{ .join = .{
        .id = internal_id,
        .params = try store.addLocalSpan(&.{shared_param}),
        .body = jump_external,
        .remainder = jump_internal,
    } });
    const external_stmt = try store.addCFStmt(.{ .join = .{
        .id = external_id,
        .params = try store.addLocalSpan(&.{shared_param}),
        .body = ret,
        .remainder = internal_stmt,
    } });

    var join_params = JoinParamIndex.init(testing.allocator);
    defer join_params.deinit();
    try join_params.indexReachable(&store, external_stmt);
    var cloner = try BodyCloner(TestRetRewriter).initWithFreshDeclaredJoins(&store, .{}, internal_stmt, &join_params);
    defer cloner.deinit();
    const cloned_stmt = try cloner.cloneStmt(internal_stmt);
    const cloned = store.getCFStmt(cloned_stmt).join;
    const cloned_param = GuardedList.at(store.getLocalSpan(cloned.params), 0);

    try testing.expect(cloned.id != internal_id);
    try testing.expect(cloned_param != shared_param);
    try testing.expectEqual(cloned.id, store.getCFStmt(cloned.remainder).jump.target);

    const bridge = store.getCFStmt(cloned.body).set_local;
    try testing.expectEqual(shared_param, bridge.target);
    try testing.expectEqual(cloned_param, bridge.value);
    try testing.expectEqual(LIR.SetLocalWriteMode.initialize_join_param, bridge.mode);
    try testing.expectEqual(external_id, store.getCFStmt(bridge.next).jump.target);
}

test "chainIsSingleUse rejects an extra read of any link" {
    // The soundness guard both fusion passes depend on: a chain link read more
    // than once means the fusion would orphan a still-live local. Each case
    // below is one shape that must be refused, and one that must be allowed.
    const gpa = std.testing.allocator;
    var store = LirStore.init(gpa);
    defer store.deinit();

    const a = try store.addLocal(.{ .layout_idx = .str });
    const b = try store.addLocal(.{ .layout_idx = .str });
    const unit = try store.addLocal(.{ .layout_idx = .zst });

    const ret = try store.addCFStmt(.{ .ret = .{ .value = b } });
    const alias = try store.addCFStmt(.{ .assign_ref = .{
        .target = b,
        .op = .{ .local = a },
        .next = ret,
    } });

    // `a` is read once (by the alias) and `b` once (by the return).
    try std.testing.expect(try chainIsSingleUse(&store, alias, &.{ a, b }));

    // A second read of the head link refuses the chain.
    const extra_head = try store.addCFStmt(.{ .assign_ref = .{
        .target = unit,
        .op = .{ .local = a },
        .next = alias,
    } });
    try std.testing.expect(!try chainIsSingleUse(&store, extra_head, &.{ a, b }));

    // A second read of the tail link refuses it just the same.
    const extra_tail = try store.addCFStmt(.{ .assign_ref = .{
        .target = unit,
        .op = .{ .local = b },
        .next = alias,
    } });
    try std.testing.expect(!try chainIsSingleUse(&store, extra_tail, &.{ a, b }));

    // A local with no reads at all is not "exactly one" either.
    const unread = try store.addLocal(.{ .layout_idx = .str });
    try std.testing.expect(!try chainIsSingleUse(&store, alias, &.{unread}));
}

test "rewritableProcBody refuses procs whose body is not the compiler's to clone" {
    const gpa = std.testing.allocator;
    var store = LirStore.init(gpa);
    defer store.deinit();

    const unit = try store.addLocal(.{ .layout_idx = .zst });
    const body = try store.addCFStmt(.{ .ret = .{ .value = unit } });

    const roc_proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{}),
        .frame_locals = try store.addLocalSpan(&.{unit}),
        .body = body,
        .ret_layout = .zst,
        .abi = .roc,
    });
    try std.testing.expectEqual(body, rewritableProcBody(&store, roc_proc).?);

    const bodyless = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{}),
        .frame_locals = try store.addLocalSpan(&.{}),
        .body = null,
        .ret_layout = .zst,
        .abi = .roc,
    });
    try std.testing.expect(rewritableProcBody(&store, bodyless) == null);

    const erased = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{}),
        .frame_locals = try store.addLocalSpan(&.{unit}),
        .body = body,
        .ret_layout = .zst,
        .abi = .erased_callable,
    });
    try std.testing.expect(rewritableProcBody(&store, erased) == null);
}

test "call-result fusion machinery has one definition" {
    // `return_slot` and `str_append` are the same pass with different consumer
    // matches. The walk, the liveness guard, and the variant clone live here so
    // a fix to any of them cannot land on one pass and miss the other.
    const sources = [_][]const u8{
        @embedFile("return_slot.zig"),
        @embedFile("str_append.zig"),
        @embedFile("box_reuse.zig"),
    };
    const shared = [_][]const u8{
        "fn chainIsSingleUse(",
        "fn cloneCallVariant(",
        "fn rewritableProcBody(",
    };
    for (sources) |source| {
        for (shared) |decl| {
            try std.testing.expect(std.mem.find(u8, source, decl) == null);
        }
    }
    const own = @embedFile("body_clone.zig");
    for (shared) |decl| {
        try std.testing.expect(std.mem.find(u8, own, decl) != null);
    }
}
