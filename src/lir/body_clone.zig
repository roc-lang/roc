//! Shared machinery for the proc-specializing rewrite passes that clone a
//! source proc body into an internal variant with rewritten return handling.
//!
//! `StrAppend` and `ReturnSlot` both clone an existing proc body statement by
//! statement, remapping every local, while intercepting the body's returns to
//! emit their own destination-aware tail. This module owns the parts that do
//! not vary between them: the generic `BodyCloner(Rewriter)`, the reachable
//! successor walk, the alias-forwarding walk, the operand read counter used to
//! prove rewrite soundness, and the frame-local deduplication helpers.
//! Scratch is indexed only by encountered identities and can be owned by a
//! procedure task independently of the store receiving its cloned output.

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

/// Retains empty analysis storage up to the peak number of simultaneous leases.
/// Nodes stay at stable addresses even when nested analyses acquire more storage.
/// Returning a lease never allocates, including while unwinding an allocation failure.
fn ScratchPool(comptime T: type) type {
    return struct {
        const Self = @This();
        const Lease = struct {
            value: T,
            pool: *Self,
            next: ?*Lease = null,

            fn release(self: *Lease, value: T) void {
                self.value = value;
                self.value.clearRetainingCapacity();
                self.next = self.pool.free;
                self.pool.free = self;
                self.pool.active -= 1;
            }
        };

        allocator: Allocator,
        free: ?*Lease = null,
        active: usize = 0,

        fn acquire(self: *Self) Allocator.Error!*Lease {
            const lease = if (self.free) |entry| blk: {
                self.free = entry.next;
                break :blk entry;
            } else blk: {
                const entry = try self.allocator.create(Lease);
                entry.* = .{ .value = T.init(self.allocator), .pool = self };
                break :blk entry;
            };
            self.active += 1;
            return lease;
        }

        fn deinit(self: *Self) void {
            std.debug.assert(self.active == 0);
            while (self.free) |entry| {
                self.free = entry.next;
                entry.value.deinit();
                self.allocator.destroy(entry);
            }
            self.* = undefined;
        }
    };
}

const CountPool = ScratchPool(collections.DenseMap(LocalId, u32));
const WalkPool = ScratchPool(WalkStorage);

const WalkStorage = struct {
    work: std.ArrayList(CFStmtId) = .empty,
    visited: collections.DenseMap(CFStmtId, void),

    fn init(allocator: Allocator) WalkStorage {
        return .{ .visited = collections.DenseMap(CFStmtId, void).init(allocator) };
    }

    fn clearRetainingCapacity(self: *WalkStorage) void {
        self.work.clearRetainingCapacity();
        self.visited.clearRetainingCapacity();
    }

    fn deinit(self: *WalkStorage) void {
        self.work.deinit(self.visited.allocator);
        self.visited.deinit();
    }
};

/// Exclusive worker-owned storage. Leases contain current analysis results;
/// only empty capacity survives a procedure, a rewrite, or a compilation.
/// This owner must stay at a stable address and outlive every acquired result.
pub const AnalysisScratch = struct {
    counts: CountPool,
    walks: WalkPool,

    pub fn init(allocator: Allocator) AnalysisScratch {
        return .{ .counts = .{ .allocator = allocator }, .walks = .{ .allocator = allocator } };
    }

    pub fn deinit(self: *AnalysisScratch) void {
        self.counts.deinit();
        self.walks.deinit();
    }

    pub fn acquireCounts(self: *AnalysisScratch) Allocator.Error!ReadCounts {
        const lease = try self.counts.acquire();
        const counts = lease.value;
        lease.value = undefined;
        return .{ .counts = counts, .lease = lease };
    }
};

/// Reserve the source identity domain once before dispatching procedure work.
/// Include unreachable statements and borrowed prefixes, not just owned output.
pub fn firstFreshJoinPoint(store: *const LirStore) u32 {
    var next: u32 = 0;
    for (0..store.cfStmtCount()) |index| {
        const stmt = store.getCFStmt(@enumFromInt(index));
        if (stmt != .join) continue;
        const raw = @intFromEnum(stmt.join.id);
        if (raw == std.math.maxInt(u32)) @panic("join-point id space exhausted");
        next = @max(next, raw + 1);
    }
    return next;
}

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
    next_join_point: u32 = 0,

    pub fn init(allocator: Allocator) JoinParamIndex {
        return .{ .params = collections.DenseMap(LIR.JoinPointId, LIR.LocalSpan).init(allocator) };
    }

    pub fn deinit(self: *JoinParamIndex) void {
        self.params.deinit();
    }

    pub fn record(self: *JoinParamIndex, join: @FieldType(LIR.CFStmt, "join")) Allocator.Error!void {
        try self.params.put(join.id, join.params);
        const raw = @intFromEnum(join.id);
        if (raw == std.math.maxInt(u32)) @panic("join-point id space exhausted");
        self.next_join_point = @max(self.next_join_point, raw + 1);
    }

    /// Reserve an identity in the same domain used by subtree clones.
    pub fn freshJoinPoint(self: *JoinParamIndex) LIR.JoinPointId {
        if (self.next_join_point == std.math.maxInt(u32)) @panic("join-point id space exhausted");
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
    }

    pub fn indexReachable(self: *JoinParamIndex, store: *LirStore, body: CFStmtId) Allocator.Error!void {
        var walk = try ReachableStmts.initWithAllocator(store, body, self.params.allocator);
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
    store: *const LirStore,
    work: *std.ArrayList(CFStmtId),
    stmt_id: CFStmtId,
) Allocator.Error!void {
    return appendSuccessorsWithAllocator(store, work, stmt_id, store.allocator);
}

/// Like `appendSuccessors`, using the walk owner's scratch allocator.
pub fn appendSuccessorsWithAllocator(
    store: *const LirStore,
    work: *std.ArrayList(CFStmtId),
    stmt_id: CFStmtId,
    allocator: Allocator,
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
        => |s| try work.append(allocator, s.next),

        .switch_stmt => |s| {
            if (s.continuation) |continuation| try work.append(allocator, continuation);
            try work.append(allocator, s.default_branch);
            const branches = store.getCFSwitchBranches(s.branches);
            for (0..branches.len) |index| {
                try work.append(allocator, GuardedList.at(branches, index).body);
            }
        },
        .switch_initialized_payload => |s| {
            try work.append(allocator, s.initialized_branch);
            try work.append(allocator, s.uninitialized_branch);
        },
        .str_match => |s| {
            try work.append(allocator, s.on_match);
            try work.append(allocator, s.on_miss);
        },
        .boxy_tag_match => |s| {
            try work.append(allocator, s.on_match);
            try work.append(allocator, s.on_miss);
        },
        .str_match_set => |s| {
            const arms = store.getStrMatchArms(s.arms);
            for (0..arms.len) |index| {
                try work.append(allocator, GuardedList.at(arms, index).on_match);
            }
            try work.append(allocator, s.on_miss);
        },
        .join => |s| {
            try work.append(allocator, s.body);
            try work.append(allocator, s.remainder);
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
    counts: collections.DenseMap(LocalId, u32),
    lease: ?*CountPool.Lease = null,
    // Operand enumeration is also used by allocation-free dense callers.
    // Defer a sparse insertion failure until the end of that statement.
    failure: ?Allocator.Error = null,

    /// Release the backing count storage.
    pub fn deinit(self: *ReadCounts) void {
        if (self.lease) |lease| lease.release(self.counts) else self.counts.deinit();
        self.* = undefined;
    }

    /// Number of counted operand occurrences (or definitions) of `local`.
    pub fn get(self: *const ReadCounts, local: LocalId) u32 {
        return self.counts.get(local) orelse 0;
    }
};

/// Count operand reads of every local reachable from `body`, walking all
/// successor edges. Definitions (statement targets and join parameters) are not
/// reads; only operand positions count.
pub fn countReachableReads(store: *LirStore, body: CFStmtId) Allocator.Error!ReadCounts {
    return countReachableReadsWithAllocator(store, body, store.allocator);
}

/// Count only reachable operands, allocating scratch independently of output.
pub fn countReachableReadsWithAllocator(store: *LirStore, body: CFStmtId, allocator: Allocator) Allocator.Error!ReadCounts {
    return countReachable(store, body, allocator, null, .reads);
}

/// Count operands using retained worker storage, with an independent result lease.
pub fn countReachableReadsWithScratch(store: *LirStore, body: CFStmtId, scratch: *AnalysisScratch) Allocator.Error!ReadCounts {
    return countReachable(store, body, scratch.counts.allocator, scratch, .reads);
}

/// Add this statement's operand reads to an existing per-local count row.
/// Definitions are deliberately excluded, matching `countReachableReads`.
pub fn countStmtReads(store: *const LirStore, counts: []u32, stmt: LIR.CFStmt) void {
    forEachStmtRead(store, stmt, counts, noteRead);
}

/// Call `note(ctx, local)` for every operand this statement reads, in the
/// one inventory every reader of operands shares; definitions are excluded.
pub fn forEachStmtRead(
    store: *const LirStore,
    stmt: LIR.CFStmt,
    ctx: anytype,
    comptime note: fn (@TypeOf(ctx), LocalId) void,
) void {
    switch (stmt) {
        .assign_ref => |s| switch (s.op) {
            .local => |source| note(ctx, source),
            .discriminant => |ref| note(ctx, ref.source),
            .field => |ref| note(ctx, ref.source),
            .tag_payload => |ref| note(ctx, ref.source),
            .tag_payload_struct => |ref| note(ctx, ref.source),
            .list_reinterpret => |ref| note(ctx, ref.backing_ref),
            .nominal => |ref| note(ctx, ref.backing_ref),
        },
        .assign_call => |s| {
            if (s.result_desc) |desc| emitDesc(ctx, note, desc);
            const args = store.getLocalSpan(s.args);
            for (0..args.len) |index| note(ctx, GuardedList.at(args, index));
        },
        .assign_call_erased => |s| {
            note(ctx, s.closure);
            emitSpan(store, ctx, note, s.args);
            emitSpan(store, ctx, note, s.arg_descs);
            if (s.result_desc) |desc| emitDesc(ctx, note, desc);
            if (s.reuse_source) |reuse_source| note(ctx, reuse_source);
        },
        .assign_packed_erased_fn => |s| {
            if (s.capture) |capture| note(ctx, capture);
            if (s.result_desc) |desc| emitDesc(ctx, note, desc);
            if (s.reuse) |reuse| note(ctx, reuse);
        },
        .assign_boxy_desc_ref => |s| {
            emitDesc(ctx, note, s.desc);
            if (s.tag_residual_for) |desc| emitDesc(ctx, note, desc);
            emitSpan(store, ctx, note, s.captures);
        },
        .assign_boxy_dict_ref => |s| {
            emitDict(ctx, note, s.dict);
            emitSpan(store, ctx, note, s.captures);
        },
        .assign_boxy_box => |s| {
            note(ctx, s.payload);
            if (s.source_desc) |desc| emitDesc(ctx, note, desc);
            if (s.payload_desc) |desc| emitDesc(ctx, note, desc);
        },
        .assign_boxy_reuse_box => |s| {
            note(ctx, s.source);
            emitDesc(ctx, note, s.desc);
        },
        .assign_boxy_unbox => |s| {
            note(ctx, s.source);
            emitDesc(ctx, note, s.source_desc);
            if (s.target_desc) |desc| emitDesc(ctx, note, desc);
        },
        .assign_boxy_adapt => |s| {
            note(ctx, s.source);
            if (s.source_desc) |desc| emitDesc(ctx, note, desc);
            if (s.target_desc) |desc| emitDesc(ctx, note, desc);
        },
        .assign_boxy_inspect => |s| {
            note(ctx, s.source);
            emitDesc(ctx, note, s.source_desc);
        },
        .assign_boxy_eq => |s| {
            note(ctx, s.lhs);
            note(ctx, s.rhs);
            emitDesc(ctx, note, s.source_desc);
        },
        .assign_boxy_tag => |s| {
            emitDesc(ctx, note, s.target_desc);
            if (s.payload) |payload| note(ctx, payload);
            if (s.payload_desc) |desc| emitDesc(ctx, note, desc);
        },
        .assign_boxy_tag_payload => |s| {
            note(ctx, s.source);
            emitDesc(ctx, note, s.source_desc);
        },
        .boxy_tag_match => |s| {
            note(ctx, s.source);
            emitDesc(ctx, note, s.source_desc);
        },
        .assign_call_dict => |s| {
            emitDict(ctx, note, s.dict);
            emitSpan(store, ctx, note, s.args);
            emitSpan(store, ctx, note, s.arg_descs);
            emitSpan(store, ctx, note, s.hidden_args);
            if (s.result_desc) |desc| emitDesc(ctx, note, desc);
        },
        .assign_low_level => |s| {
            const args = store.getLocalSpan(s.args);
            for (0..args.len) |index| note(ctx, GuardedList.at(args, index));
        },
        .assign_list => |s| {
            const elems = store.getLocalSpan(s.elems);
            for (0..elems.len) |index| note(ctx, GuardedList.at(elems, index));
        },
        .assign_struct => |s| {
            const fields = store.getLocalSpan(s.fields);
            for (0..fields.len) |index| note(ctx, GuardedList.at(fields, index));
        },
        .assign_tag => |s| if (s.payload) |payload| note(ctx, payload),
        .store_struct => |s| {
            note(ctx, s.dest);
            const fields = store.getLocalSpan(s.fields);
            for (0..fields.len) |index| note(ctx, GuardedList.at(fields, index));
        },
        .store_tag => |s| {
            note(ctx, s.dest);
            if (s.payload) |payload| note(ctx, payload);
        },
        .set_local => |s| note(ctx, s.value),
        .debug => |s| note(ctx, s.message),
        .expect => |s| note(ctx, s.condition),
        .expect_err => |s| note(ctx, s.message),
        .switch_stmt => |s| note(ctx, s.cond),
        .switch_initialized_payload => |s| {
            note(ctx, s.cond);
            note(ctx, s.payload);
        },
        .str_match => |s| note(ctx, s.source),
        .str_match_set => |s| note(ctx, s.source),
        .ret => |s| note(ctx, s.value),
        .crash => |s| if (s.msg.localId()) |message| note(ctx, message),
        .incref => |s| note(ctx, s.value),
        .decref => |s| note(ctx, s.value),
        .decref_if_initialized => |s| {
            note(ctx, s.cond);
            note(ctx, s.value);
        },
        .free => |s| note(ctx, s.value),
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

/// Count definitions of every local reachable from `body`, walking all
/// successor edges: statement targets, join parameters, descriptor outputs,
/// and pattern-match captures. Operand reads are not definitions.
pub fn countReachableDefs(store: *LirStore, body: CFStmtId) Allocator.Error!ReadCounts {
    return countReachableDefsWithAllocator(store, body, store.allocator);
}

/// Like `countReachableDefs`, using the procedure task's scratch allocator.
pub fn countReachableDefsWithAllocator(store: *LirStore, body: CFStmtId, allocator: Allocator) Allocator.Error!ReadCounts {
    return countReachable(store, body, allocator, null, .defs);
}

/// Count writes using retained worker storage, independently of live read counts.
pub fn countReachableDefsWithScratch(store: *LirStore, body: CFStmtId, scratch: *AnalysisScratch) Allocator.Error!ReadCounts {
    return countReachable(store, body, scratch.counts.allocator, scratch, .defs);
}

fn countReachable(store: *LirStore, body: CFStmtId, allocator: Allocator, scratch: ?*AnalysisScratch, comptime kind: enum { reads, defs, binders }) Allocator.Error!ReadCounts {
    var counts: ReadCounts = if (scratch) |owner| try owner.acquireCounts() else .{ .counts = collections.DenseMap(LocalId, u32).init(allocator) };
    errdefer counts.deinit();
    var walk = if (scratch) |owner| try ReachableStmts.initWithScratch(store, body, owner) else try ReachableStmts.initWithAllocator(store, body, allocator);
    defer walk.deinit();

    while (try walk.next()) |stmt_id| {
        switch (kind) {
            .reads => forEachStmtRead(store, store.getCFStmt(stmt_id), &counts, noteReachableRead),
            .defs => forEachStmtDef(store, store.getCFStmt(stmt_id), &counts, noteReachableRead),
            .binders => visitStmtDefinitions(store, &counts, stmt_id),
        }
        if (counts.failure) |err| return err;
    }
    return counts;
}

/// Add this statement's definitions to an existing per-local count row.
/// Operand reads are deliberately excluded, matching `countReachableDefs`.
pub fn countStmtDefs(store: *const LirStore, counts: []u32, stmt: LIR.CFStmt) void {
    forEachStmtDef(store, stmt, counts, noteRead);
}

/// Call `note(ctx, local)` for every local this statement defines, in the
/// one inventory every reader of definitions shares; operand reads are
/// excluded.
pub fn forEachStmtDef(
    store: *const LirStore,
    stmt: LIR.CFStmt,
    ctx: anytype,
    comptime note: fn (@TypeOf(ctx), LocalId) void,
) void {
    switch (stmt) {
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
        .set_local,
        => |s| note(ctx, s.target),
        .assign_call => |s| {
            note(ctx, s.target);
            if (s.out_desc) |out_desc| note(ctx, out_desc);
        },
        .assign_call_erased => |s| {
            note(ctx, s.target);
            if (s.out_desc) |out_desc| note(ctx, out_desc);
        },
        .assign_boxy_tag_payload => |s| {
            note(ctx, s.target);
            if (s.target_desc) |target_desc| note(ctx, target_desc);
        },
        .join => |s| emitSpan(store, ctx, note, s.params),
        .str_match => |s| emitStepCaptures(store, ctx, note, s.steps),
        .str_match_set => |s| {
            const arms = store.getStrMatchArms(s.arms);
            for (0..arms.len) |index| emitStepCaptures(store, ctx, note, GuardedList.at(arms, index).steps);
        },
        .store_struct,
        .store_tag,
        .debug,
        .expect,
        .expect_err,
        .switch_stmt,
        .switch_initialized_payload,
        .boxy_tag_match,
        .ret,
        .crash,
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        .comptime_branch_taken,
        .jump,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .loop_continue,
        .loop_break,
        => {},
    }
}

fn emitStepCaptures(store: *const LirStore, ctx: anytype, comptime note: fn (@TypeOf(ctx), LocalId) void, span: LIR.StrMatchStepSpan) void {
    const steps = store.getStrMatchSteps(span);
    for (0..steps.len) |index| switch (GuardedList.at(steps, index).capture) {
        .discard => {},
        .view => |local| note(ctx, local),
    };
}

fn noteRead(counts: []u32, local: LocalId) void {
    counts[@intFromEnum(local)] += 1;
}

fn noteReachableRead(counts: *ReadCounts, local: LocalId) void {
    if (counts.failure != null) return;
    const entry = counts.counts.getOrPut(local) catch |err| {
        counts.failure = err;
        return;
    };
    if (!entry.found_existing) entry.value_ptr.* = 0;
    entry.value_ptr.* += 1;
}

fn emitSpan(store: *const LirStore, ctx: anytype, comptime note: fn (@TypeOf(ctx), LocalId) void, span: LIR.LocalSpan) void {
    const locals = store.getLocalSpan(span);
    for (0..locals.len) |index| note(ctx, GuardedList.at(locals, index));
}

fn emitDesc(ctx: anytype, comptime note: fn (@TypeOf(ctx), LocalId) void, desc: LIR.BoxyDescRef) void {
    if (desc.localOrNull()) |local| note(ctx, local);
}

fn emitDict(ctx: anytype, comptime note: fn (@TypeOf(ctx), LocalId) void, dict: LIR.BoxyDictRef) void {
    if (dict.localOrNull()) |local| note(ctx, local);
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
    allocator: Allocator,
    work: std.ArrayList(CFStmtId),
    visited: collections.DenseMap(CFStmtId, void),
    lease: ?*WalkPool.Lease = null,

    /// Start a walk rooted at `body`.
    pub fn init(store: *LirStore, body: CFStmtId) Allocator.Error!ReachableStmts {
        return initWithAllocator(store, body, store.allocator);
    }

    pub fn initWithAllocator(store: *LirStore, body: CFStmtId, allocator: Allocator) Allocator.Error!ReachableStmts {
        var work = std.ArrayList(CFStmtId).empty;
        errdefer work.deinit(allocator);
        try work.append(allocator, body);
        return .{
            .store = store,
            .allocator = allocator,
            .work = work,
            .visited = collections.DenseMap(CFStmtId, void).init(allocator),
        };
    }

    /// Lease an empty traversal, including its retained work stack and visited map.
    pub fn initWithScratch(store: *LirStore, body: CFStmtId, scratch: *AnalysisScratch) Allocator.Error!ReachableStmts {
        const lease = try scratch.walks.acquire();
        var walk: ReachableStmts = .{
            .store = store,
            .allocator = scratch.walks.allocator,
            .work = lease.value.work,
            .visited = lease.value.visited,
            .lease = lease,
        };
        lease.value = undefined;
        errdefer walk.deinit();
        try walk.work.append(walk.allocator, body);
        return walk;
    }

    /// Release the walk's scratch storage.
    pub fn deinit(self: *ReachableStmts) void {
        if (self.lease) |lease| {
            lease.release(.{ .work = self.work, .visited = self.visited });
        } else {
            self.work.deinit(self.allocator);
            self.visited.deinit();
        }
        self.* = undefined;
    }

    /// The next unvisited statement, or null when the walk is done.
    pub fn next(self: *ReachableStmts) Allocator.Error!?CFStmtId {
        while (self.work.pop()) |stmt_id| {
            const entry = try self.visited.getOrPut(stmt_id);
            if (entry.found_existing) continue;
            try appendSuccessorsWithAllocator(self.store, &self.work, stmt_id, self.allocator);
            return stmt_id;
        }
        return null;
    }
};

/// Return only binders reachable from `body`. Clone passes give these fresh
/// identities while retaining read-only external inputs. Unlike write counts,
/// this excludes `set_local` and includes maybe-uninitialized join binders.
pub fn collectReachableDefinitions(store: *LirStore, body: CFStmtId) Allocator.Error!ReadCounts {
    return collectReachableDefinitionsWithAllocator(store, body, store.allocator);
}

/// Like `collectReachableDefinitions`, with independently owned scratch.
pub fn collectReachableDefinitionsWithAllocator(store: *LirStore, body: CFStmtId, allocator: Allocator) Allocator.Error!ReadCounts {
    return countReachable(store, body, allocator, null, .binders);
}

/// Collect lexical binders using an independent lease from the worker's storage.
pub fn collectReachableDefinitionsWithScratch(store: *LirStore, body: CFStmtId, scratch: *AnalysisScratch) Allocator.Error!ReadCounts {
    return countReachable(store, body, scratch.counts.allocator, scratch, .binders);
}

/// Add every local defined by `stmt_id` to an existing definition set.
pub fn markStmtDefinitions(store: *const LirStore, defined: []bool, stmt_id: CFStmtId) void {
    visitStmtDefinitions(store, defined, stmt_id);
}

/// Add exact lexical binders without allocating for unrelated local identities.
pub fn markStmtDefinitionsSparse(store: *const LirStore, defined: *ReadCounts, stmt_id: CFStmtId) Allocator.Error!void {
    visitStmtDefinitions(store, defined, stmt_id);
    if (defined.failure) |err| return err;
}

fn noteDefinition(defined: anytype, local: LocalId) void {
    if (@TypeOf(defined) == *ReadCounts) {
        noteReachableRead(defined, local);
    } else {
        defined[@intFromEnum(local)] = true;
    }
}

fn visitStmtDefinitions(store: *const LirStore, defined: anytype, stmt_id: CFStmtId) void {
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
        => |stmt| noteDefinition(defined, stmt.target),
        .assign_call => |stmt| {
            noteDefinition(defined, stmt.target);
            if (stmt.out_desc) |out_desc| noteDefinition(defined, out_desc);
        },
        .assign_call_erased => |stmt| {
            noteDefinition(defined, stmt.target);
            if (stmt.out_desc) |out_desc| noteDefinition(defined, out_desc);
        },
        .assign_boxy_tag_payload => |stmt| {
            noteDefinition(defined, stmt.target);
            if (stmt.target_desc) |target_desc| noteDefinition(defined, target_desc);
        },
        .join => |join| {
            const params = store.getLocalSpan(join.params);
            for (0..params.len) |index| noteDefinition(defined, GuardedList.at(params, index));
            const maybe_uninitialized = store.getLocalSpan(join.maybe_uninitialized_params);
            for (0..maybe_uninitialized.len) |index| noteDefinition(defined, GuardedList.at(maybe_uninitialized, index));
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

fn markStrMatchDefinitions(store: *const LirStore, defined: anytype, span: LIR.StrMatchStepSpan) void {
    const steps = store.getStrMatchSteps(span);
    for (0..steps.len) |index| switch (GuardedList.at(steps, index).capture) {
        .discard => {},
        .view => |local| noteDefinition(defined, local),
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

/// Identity of a call variant of `source`. The variant is determined by the
/// source procedure and the shape of the variant specification; the return
/// layout index is program-local, so a fused variant's identity is stable
/// only within one program until fusion output carries content identities.
fn callVariantIdentity(source: LIR.ProcIdentity, spec: CallVariantSpec) LIR.ProcIdentity {
    var key: [12]u8 = undefined;
    std.mem.writeInt(u32, key[0..4], @intCast(spec.leading_args.len), .little);
    std.mem.writeInt(u32, key[4..8], @intCast(spec.extra_frame_locals.len), .little);
    std.mem.writeInt(u32, key[8..12], @intFromEnum(spec.ret_layout), .little);
    return source.derived("call-variant", &key);
}

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
        try cloner.local_map.put(source_arg, variant_args.items[index + spec.leading_args.len]);
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
        .identity = callVariantIdentity(source_spec.identity, spec),
        .args = try store.addLocalSpan(variant_args.items),
        .erased_reuse_arg = erased_reuse_arg,
        .frame_locals = try store.addLocalSpan(frame_locals.items[0..unique_len]),
        .body = body,
        .ret_layout = spec.ret_layout,
        .abi = .roc,
    }, store.procLoc(source));
    try store.copyProcDebugInfo(variant, source);

    return variant;
}

/// Clone a source proc body into fresh statements and locals, delegating return
/// handling to `Rewriter`.
///
/// `Rewriter` carries the pass-specific destination state and supplies the
/// hooks that diverge between passes:
///
///   * `cloneRet(self: *Rewriter, cloner: anytype, value: LocalId, origin: LIR.StmtOrigin)`—required.
///     Produces the cloned tail for a source `ret value`.
///   * `interceptStmt(self: *Rewriter, cloner: anytype, old_id: CFStmtId, stmt: LIR.CFStmt, origin: LIR.StmtOrigin)`—
///     optional. Returns a cloned statement id to short-circuit the default
///     clone, letting the pass fuse a direct constructor/concat return into
///     the tail or rewrite statements it identified up front, or `null` to
///     fall through to the ordinary clone.
///   * `preserveLocal(self: *Rewriter, old: LocalId) bool`—optional.
///     Retains an uncached local's original identity instead of allocating a
///     clone. This lets subtree rewrites preserve external inputs by default
///     without seeding a map over the entire store.
///
/// `origin` is the provenance of the source statement being cloned, with its
/// inline scope already mapped into the destination; a hook's statements
/// state it (with the pass's own kind where the hook rewrites) explicitly.
///
/// Both hooks receive the cloner and use its `mapLocal`, `mapLocalSpan`,
/// `addTemp`, `directReturnOf`, and `store` surface to build their statements.
pub fn BodyCloner(comptime Rewriter: type) type {
    return struct {
        const Self = @This();

        /// The store the clone is written into.
        store: *LirStore,
        /// Task-local storage, separate from the store-owned clone output.
        allocator: Allocator,
        /// Pass-specific return rewriter and its destination state.
        rewriter: Rewriter,
        /// Only locals encountered by this clone have entries.
        local_map: collections.DenseMap(LocalId, LocalId),
        stmt_map: collections.DenseMap(CFStmtId, CFStmtId),
        /// Every local created by this clone, in creation order.
        new_locals: std.ArrayList(LocalId),
        /// Optional virtual frame under which cloned source scopes are rebased.
        inline_scope_outer: LIR.InlineScopeId,
        inline_scope_map: collections.DenseMap(LIR.InlineScopeId, LIR.InlineScopeId),
        /// Which join identities must be alpha-renamed. Whole-procedure
        /// inlining owns every jump target and remaps all of them. A cloned
        /// subtree remaps only declarations structurally contained in that
        /// subtree, preserving jumps to lexically enclosing joins.
        join_remap: enum { none, all, declared },
        declared_joins: collections.DenseMap(LIR.JoinPointId, void),
        join_params: ?*JoinParamIndex,
        join_map: collections.DenseMap(LIR.JoinPointId, LIR.JoinPointId),
        next_join_point: u32,

        /// Initialize clone state without allocating for unrelated procedures.
        /// Join identities are retained; use a destination-aware constructor
        /// when cloning into an existing join domain rather than a new proc.
        pub fn init(store: *LirStore, rewriter: Rewriter) Allocator.Error!Self {
            return initWithAllocator(store, rewriter, store.allocator);
        }

        /// Use task-owned scratch while emitting clone output into `store`.
        pub fn initWithAllocator(store: *LirStore, rewriter: Rewriter, allocator: Allocator) Allocator.Error!Self {
            return initInternal(store, rewriter, LIR.InlineScopeId.none, .none, null, allocator);
        }

        /// Allocate clone state and rebase every cloned source location below
        /// `outer`. Procedure variants pass `.none`; true call-site inlining
        /// supplies the virtual frame representing the removed call. The
        /// destination body defines the existing join identity domain.
        pub fn initWithInlineScopeOuter(
            store: *LirStore,
            rewriter: Rewriter,
            outer: LIR.InlineScopeId,
            destination_body: CFStmtId,
        ) Allocator.Error!Self {
            return initWithInlineScopeOuterAndAllocator(store, rewriter, outer, destination_body, store.allocator);
        }

        /// Inline into an existing destination using task-owned scratch.
        pub fn initWithInlineScopeOuterAndAllocator(
            store: *LirStore,
            rewriter: Rewriter,
            outer: LIR.InlineScopeId,
            destination_body: CFStmtId,
            allocator: Allocator,
        ) Allocator.Error!Self {
            var self = try initInternal(store, rewriter, outer, .all, null, allocator);
            errdefer self.deinit();
            var index = JoinParamIndex.init(self.allocator);
            defer index.deinit();
            try index.indexReachable(store, destination_body);
            self.next_join_point = index.next_join_point;
            return self;
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
            return initWithFreshDeclaredJoinsAndAllocator(store, rewriter, body, join_params, store.allocator);
        }

        /// Clone a subtree with task-owned scratch and a destination join index.
        pub fn initWithFreshDeclaredJoinsAndAllocator(
            store: *LirStore,
            rewriter: Rewriter,
            body: CFStmtId,
            join_params: *JoinParamIndex,
            allocator: Allocator,
        ) Allocator.Error!Self {
            var self = try initInternal(store, rewriter, LIR.InlineScopeId.none, .declared, join_params, allocator);
            errdefer self.deinit();
            var walk = try ReachableStmts.initWithAllocator(store, body, allocator);
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
            allocator: Allocator,
        ) Allocator.Error!Self {
            return .{
                .store = store,
                .allocator = allocator,
                .rewriter = rewriter,
                .local_map = collections.DenseMap(LocalId, LocalId).init(allocator),
                .stmt_map = collections.DenseMap(CFStmtId, CFStmtId).init(allocator),
                .new_locals = .empty,
                .inline_scope_outer = outer,
                .inline_scope_map = collections.DenseMap(LIR.InlineScopeId, LIR.InlineScopeId).init(allocator),
                .join_remap = join_remap,
                .declared_joins = collections.DenseMap(LIR.JoinPointId, void).init(allocator),
                .join_params = join_params,
                .join_map = collections.DenseMap(LIR.JoinPointId, LIR.JoinPointId).init(allocator),
                .next_join_point = 0,
            };
        }

        /// Release the clone's scratch storage.
        pub fn deinit(self: *Self) void {
            self.new_locals.deinit(self.allocator);
            self.stmt_map.deinit();
            self.join_map.deinit();
            self.declared_joins.deinit();
            self.inline_scope_map.deinit();
            self.local_map.deinit();
        }

        /// Clone `old_id` and everything reachable from it, memoizing by
        /// original statement id so shared join targets are cloned once.
        pub fn cloneStmt(self: *Self, old_id: CFStmtId) Allocator.Error!CFStmtId {
            if (self.stmt_map.get(old_id)) |existing| return existing;

            var origin = self.store.stmtOrigin(old_id);
            origin.inline_scope = try self.mapInlineScope(origin.inline_scope);

            const stmt = self.store.getCFStmt(old_id);
            if (@hasDecl(Rewriter, "interceptStmt")) {
                if (try self.rewriter.interceptStmt(self, old_id, stmt, origin)) |intercepted| {
                    try self.stmt_map.put(old_id, intercepted);
                    return intercepted;
                }
            }

            const cloned = switch (stmt) {
                .init_uninitialized => |s| try self.store.addCFStmt(.{ .init_uninitialized = .{
                    .target = try self.mapLocal(s.target),
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_ref => |s| try self.store.addCFStmt(.{ .assign_ref = .{
                    .target = try self.mapLocal(s.target),
                    .op = try self.mapRefOp(s.op),
                    .take_kind = s.take_kind,
                    .residual_shell_absent_fields = s.residual_shell_absent_fields,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_literal => |s| try self.store.addCFStmt(.{ .assign_literal = .{
                    .target = try self.mapLocal(s.target),
                    .value = s.value,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_call => |s| try self.store.addCFStmt(.{ .assign_call = .{
                    .target = try self.mapLocal(s.target),
                    .proc = s.proc,
                    .args = try self.mapLocalSpan(s.args),
                    .result_desc = try self.mapMaybeBoxyDescRef(s.result_desc),
                    .out_desc = try self.mapMaybeLocal(s.out_desc),
                    .is_cold = s.is_cold,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
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
                } }, origin),
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
                } }, origin),
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
                } }, origin),
                .assign_boxy_dict_ref => |s| try self.store.addCFStmt(.{ .assign_boxy_dict_ref = .{
                    .target = try self.mapLocal(s.target),
                    .dict = try self.mapBoxyDictRef(s.dict),
                    .captures = try self.mapLocalSpan(s.captures),
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_boxy_box => |s| try self.store.addCFStmt(.{ .assign_boxy_box = .{
                    .target = try self.mapLocal(s.target),
                    .payload = try self.mapLocal(s.payload),
                    .payload_layout = s.payload_layout,
                    .source_desc = try self.mapMaybeBoxyDescRef(s.source_desc),
                    .payload_desc = try self.mapMaybeBoxyDescRef(s.payload_desc),
                    .payload_mode = s.payload_mode,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_boxy_reuse_box => |s| try self.store.addCFStmt(.{ .assign_boxy_reuse_box = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .desc = try self.mapBoxyDescRef(s.desc),
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_boxy_unbox => |s| try self.store.addCFStmt(.{ .assign_boxy_unbox = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .target_desc = try self.mapMaybeBoxyDescRef(s.target_desc),
                    .target_layout = s.target_layout,
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_boxy_adapt => |s| try self.store.addCFStmt(.{ .assign_boxy_adapt = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .adapter = s.adapter,
                    .source_desc = try self.mapMaybeBoxyDescRef(s.source_desc),
                    .target_desc = try self.mapMaybeBoxyDescRef(s.target_desc),
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_boxy_inspect => |s| try self.store.addCFStmt(.{ .assign_boxy_inspect = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_boxy_eq => |s| try self.store.addCFStmt(.{ .assign_boxy_eq = .{
                    .target = try self.mapLocal(s.target),
                    .lhs = try self.mapLocal(s.lhs),
                    .rhs = try self.mapLocal(s.rhs),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_boxy_tag => |s| try self.store.addCFStmt(.{ .assign_boxy_tag = .{
                    .target = try self.mapLocal(s.target),
                    .target_desc = try self.mapBoxyDescRef(s.target_desc),
                    .tag_name = s.tag_name,
                    .payload = try self.mapMaybeLocal(s.payload),
                    .payload_layout = s.payload_layout,
                    .payload_desc = try self.mapMaybeBoxyDescRef(s.payload_desc),
                    .payload_mode = s.payload_mode,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_boxy_tag_payload => |s| try self.store.addCFStmt(.{ .assign_boxy_tag_payload = .{
                    .target = try self.mapLocal(s.target),
                    .target_desc = try self.mapMaybeLocal(s.target_desc),
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .tag_name = s.tag_name,
                    .payload_index = s.payload_index,
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .boxy_tag_match => |s| try self.store.addCFStmt(.{ .boxy_tag_match = .{
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .tag_name = s.tag_name,
                    .on_match = try self.cloneStmt(s.on_match),
                    .on_miss = try self.cloneStmt(s.on_miss),
                } }, origin),
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
                } }, origin),
                .assign_low_level => |s| try self.store.addCFStmt(.{ .assign_low_level = .{
                    .target = try self.mapLocal(s.target),
                    .op = s.op,
                    .rc_effect = s.rc_effect,
                    .unique_args = s.unique_args,
                    .interchangeable = s.interchangeable,
                    .simd_concat_count = s.simd_concat_count,
                    .args = try self.mapLocalSpan(s.args),
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_list => |s| try self.store.addCFStmt(.{ .assign_list = .{
                    .target = try self.mapLocal(s.target),
                    .elems = try self.mapLocalSpan(s.elems),
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_struct => |s| try self.store.addCFStmt(.{ .assign_struct = .{
                    .target = try self.mapLocal(s.target),
                    .fields = try self.mapLocalSpan(s.fields),
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .assign_tag => |s| try self.store.addCFStmt(.{ .assign_tag = .{
                    .target = try self.mapLocal(s.target),
                    .variant_index = s.variant_index,
                    .discriminant = s.discriminant,
                    .payload = try self.mapMaybeLocal(s.payload),
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .store_struct => |s| try self.store.addCFStmt(.{ .store_struct = .{
                    .dest = try self.mapLocal(s.dest),
                    .struct_layout = s.struct_layout,
                    .fields = try self.mapLocalSpan(s.fields),
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .store_tag => |s| try self.store.addCFStmt(.{ .store_tag = .{
                    .dest = try self.mapLocal(s.dest),
                    .tag_layout = s.tag_layout,
                    .variant_index = s.variant_index,
                    .discriminant = s.discriminant,
                    .payload = try self.mapMaybeLocal(s.payload),
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .set_local => |s| try self.store.addCFStmt(.{ .set_local = .{
                    .target = try self.mapLocal(s.target),
                    .value = try self.mapLocal(s.value),
                    .mode = s.mode,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .debug => |s| try self.store.addCFStmt(.{ .debug = .{
                    .message = try self.mapLocal(s.message),
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .expect => |s| try self.store.addCFStmt(.{ .expect = .{
                    .condition = try self.mapLocal(s.condition),
                    .site = s.site,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .expect_err => |s| try self.store.addCFStmt(.{ .expect_err = .{
                    .message = try self.mapLocal(s.message),
                    .region = s.region,
                } }, origin),
                .runtime_error => try self.store.addCFStmt(.runtime_error, origin),
                .comptime_exhaustiveness_failed => |s| try self.store.addCFStmt(.{ .comptime_exhaustiveness_failed = .{
                    .site = s.site,
                } }, origin),
                .comptime_branch_taken => |s| try self.store.addCFStmt(.{ .comptime_branch_taken = .{
                    .site = s.site,
                    .branch_index = s.branch_index,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .incref => |s| try self.store.addCFStmt(.{ .incref = .{
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .count = s.count,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .decref => |s| try self.store.addCFStmt(.{ .decref = .{
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .decref_if_initialized => |s| try self.store.addCFStmt(.{ .decref_if_initialized = .{
                    .cond = try self.mapLocal(s.cond),
                    .cond_mask = s.cond_mask,
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .free => |s| try self.store.addCFStmt(.{ .free = .{
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }, origin),
                .switch_stmt => |s| try self.cloneSwitch(s, origin),
                .switch_initialized_payload => |s| try self.store.addCFStmt(.{ .switch_initialized_payload = .{
                    .cond = try self.mapLocal(s.cond),
                    .cond_mask = s.cond_mask,
                    .payload = try self.mapLocal(s.payload),
                    .uninitialized_is_cold = s.uninitialized_is_cold,
                    .initialized_branch = try self.cloneStmt(s.initialized_branch),
                    .uninitialized_branch = try self.cloneStmt(s.uninitialized_branch),
                } }, origin),
                .str_match => |s| try self.store.addCFStmt(.{ .str_match = .{
                    .source = try self.mapLocal(s.source),
                    .prefix = s.prefix,
                    .steps = try self.mapStrMatchSteps(s.steps),
                    .end = s.end,
                    .on_match = try self.cloneStmt(s.on_match),
                    .on_miss = try self.cloneStmt(s.on_miss),
                } }, origin),
                .str_match_set => |s| try self.cloneStrMatchSet(s, origin),
                .loop_continue => try self.store.addCFStmt(.loop_continue, origin),
                .loop_break => try self.store.addCFStmt(.loop_break, origin),
                .join => |s| try self.cloneJoin(s, origin),
                .jump => |s| try self.cloneJump(s, origin),
                .ret => |s| try self.rewriter.cloneRet(self, s.value, origin),
                .crash => |s| try self.store.addCFStmt(.{ .crash = .{ .msg = switch (s.msg) {
                    .literal => |literal| .{ .literal = literal },
                    .local => |local| .{ .local = try self.mapLocal(local) },
                } } }, origin),
            };

            try self.stmt_map.put(old_id, cloned);
            return cloned;
        }

        fn cloneJoin(self: *Self, join: @FieldType(LIR.CFStmt, "join"), origin: LIR.StmtOrigin) Allocator.Error!CFStmtId {
            const cloned = try self.store.addCFStmt(.{ .join = .{
                .id = try self.mapJoinPoint(join.id),
                .params = try self.mapLocalSpan(join.params),
                .retained = try self.mapLocalSpan(join.retained),
                .maybe_uninitialized_params = try self.mapLocalSpan(join.maybe_uninitialized_params),
                .maybe_uninitialized_conditions = try self.mapLocalSpan(join.maybe_uninitialized_conditions),
                .maybe_uninitialized_condition_masks = join.maybe_uninitialized_condition_masks,
                .body = try self.cloneStmt(join.body),
                .remainder = try self.cloneStmt(join.remainder),
            } }, origin);
            if (self.join_params) |params| try params.record(self.store.getCFStmt(cloned).join);
            return cloned;
        }

        /// A declared-subtree clone can alpha-rename a local that is also a
        /// parameter of an enclosing join. Jumps encode their arguments as
        /// preceding writes, so bridge each remapped value back into the
        /// enclosing join's original parameter before leaving the clone.
        fn cloneJump(self: *Self, jump: @FieldType(LIR.CFStmt, "jump"), origin: LIR.StmtOrigin) Allocator.Error!CFStmtId {
            var next = try self.store.addCFStmt(.{ .jump = .{ .target = try self.mapJoinPoint(jump.target) } }, origin);
            if (self.join_remap != .declared or self.declared_joins.contains(jump.target)) return next;

            const params = self.join_params.?.get(jump.target) orelse
                @panic("subtree clone jumped to an unknown external join");
            next = try self.bridgeExternalJoinParams(params, origin, next);
            return next;
        }

        fn bridgeExternalJoinParams(self: *Self, span: LIR.LocalSpan, origin: LIR.StmtOrigin, tail: CFStmtId) Allocator.Error!CFStmtId {
            var next = tail;
            const params = self.store.getLocalSpan(span);
            var index = params.len;
            while (index > 0) {
                index -= 1;
                const param = GuardedList.at(params, index);
                const mapped = self.local_map.get(param) orelse {
                    try self.local_map.put(param, param);
                    continue;
                };
                if (mapped == param) continue;
                next = try self.store.addCFStmt(.{ .set_local = .{
                    .target = param,
                    .value = mapped,
                    .mode = .initialize_join_param,
                    .next = next,
                } }, origin);
            }
            return next;
        }

        /// True when `next` is a `ret` of exactly `value`, marking a direct
        /// constructor/concat return the rewriter may fuse into its tail.
        pub fn directReturnOf(self: *const Self, next: CFStmtId, value: LocalId) bool {
            const stmt = self.store.getCFStmt(next);
            return stmt == .ret and stmt.ret.value == value;
        }

        fn cloneSwitch(self: *Self, s: anytype, origin: LIR.StmtOrigin) Allocator.Error!CFStmtId {
            // Recursive body cloning appends switch branches and can therefore
            // invalidate a guarded borrow of this same backing list. Own the
            // source descriptors before cloning any branch body.
            const old_branches = try GuardedList.dupe(
                self.allocator,
                LIR.CFSwitchBranch,
                self.store.getCFSwitchBranches(s.branches),
            );
            defer self.allocator.free(old_branches);
            const branches = try self.allocator.alloc(LIR.CFSwitchBranch, old_branches.len);
            defer self.allocator.free(branches);
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
            } }, origin);
        }

        fn cloneStrMatchSet(self: *Self, s: anytype, origin: LIR.StmtOrigin) Allocator.Error!CFStmtId {
            // Cloning an arm can append nested string-match arms. Snapshot the
            // source descriptors so those appends cannot invalidate the input.
            const old_arms = try GuardedList.dupe(
                self.allocator,
                LIR.StrMatchArm,
                self.store.getStrMatchArms(s.arms),
            );
            defer self.allocator.free(old_arms);
            const arms = try self.allocator.alloc(LIR.StrMatchArm, old_arms.len);
            defer self.allocator.free(arms);
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
            } }, origin);
        }

        fn mapStrMatchSteps(self: *Self, span: LIR.StrMatchStepSpan) Allocator.Error!LIR.StrMatchStepSpan {
            const old_steps = self.store.getStrMatchSteps(span);
            const steps = try self.allocator.alloc(LIR.StrMatchStep, old_steps.len);
            defer self.allocator.free(steps);
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
            const locals = try self.allocator.alloc(LocalId, old_locals.len);
            defer self.allocator.free(locals);
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
                .runtime => std.debug.panic("LIR invariant violated: a runtime dictionary reference reached LIR cloning", .{}),
            };
        }

        /// Map an old local to its clone, allocating a fresh same-layout local
        /// on first encounter.
        pub fn mapLocal(self: *Self, old: LocalId) Allocator.Error!LocalId {
            if (self.local_map.get(old)) |existing| return existing;
            if (@hasDecl(Rewriter, "preserveLocal")) {
                if (self.rewriter.preserveLocal(old)) {
                    try self.local_map.put(old, old);
                    return old;
                }
            }

            const old_local = self.store.getLocal(old);
            const fresh = try self.store.addLocal(.{ .layout_idx = old_local.layout_idx });
            try self.local_map.put(old, fresh);
            const boxy_desc = try self.mapMaybeBoxyDescRef(old_local.boxy_desc);
            if (boxy_desc) |desc| self.store.setLocalBoxyDesc(fresh, desc);
            try self.new_locals.append(self.allocator, fresh);
            return fresh;
        }

        /// Allocate a fresh local of `layout_idx` owned by the clone.
        pub fn addTemp(self: *Self, layout_idx: layout_mod.Idx) Allocator.Error!LocalId {
            const local = try self.store.addLocal(.{ .layout_idx = layout_idx });
            try self.new_locals.append(self.allocator, local);
            return local;
        }

        fn mapInlineScope(self: *Self, old: LIR.InlineScopeId) Allocator.Error!LIR.InlineScopeId {
            if (self.inline_scope_outer == LIR.InlineScopeId.none) return old;
            if (old == LIR.InlineScopeId.none) return self.inline_scope_outer;

            if (self.inline_scope_map.get(old)) |existing| return existing;

            const source = self.store.inlineScope(old);
            const mapped = try self.store.addInlineScope(.{
                .source_symbol = source.source_symbol,
                .source_name = source.source_name,
                .source_loc = source.source_loc,
                .call_site = source.call_site,
                .parent = try self.mapInlineScope(source.parent),
            });
            try self.inline_scope_map.put(old, mapped);
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
                if (self.join_params) |params| {
                    entry.value_ptr.* = params.freshJoinPoint();
                } else {
                    if (self.next_join_point == std.math.maxInt(u32)) @panic("join-point id space exhausted");
                    entry.value_ptr.* = @enumFromInt(self.next_join_point);
                    self.next_join_point += 1;
                }
            }
            return entry.value_ptr.*;
        }
    };
}

const TestRetRewriter = struct {
    pub fn cloneRet(_: *TestRetRewriter, cloner: anytype, value: LocalId, origin: LIR.StmtOrigin) Allocator.Error!CFStmtId {
        return try cloner.store.addCFStmt(.{ .ret = .{ .value = try cloner.mapLocal(value) } }, origin);
    }
};

test "subtree clone bridges renamed parameters before an external join" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();

    const shared_param = try store.addLocal(.{ .layout_idx = .u64 });
    const external_id: LIR.JoinPointId = @enumFromInt(1);
    const internal_id: LIR.JoinPointId = @enumFromInt(2);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = shared_param } }, .test_fixture);
    const jump_external = try store.addCFStmt(.{ .jump = .{ .target = external_id } }, .test_fixture);
    const jump_internal = try store.addCFStmt(.{ .jump = .{ .target = internal_id } }, .test_fixture);
    const internal_stmt = try store.addCFStmt(.{ .join = .{
        .id = internal_id,
        .params = try store.addLocalSpan(&.{shared_param}),
        .body = jump_external,
        .remainder = jump_internal,
    } }, .test_fixture);
    const external_stmt = try store.addCFStmt(.{ .join = .{
        .id = external_id,
        .params = try store.addLocalSpan(&.{shared_param}),
        .body = ret,
        .remainder = internal_stmt,
    } }, .test_fixture);

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

    const ret = try store.addCFStmt(.{ .ret = .{ .value = b } }, .test_fixture);
    const alias = try store.addCFStmt(.{ .assign_ref = .{
        .target = b,
        .op = .{ .local = a },
        .next = ret,
    } }, .test_fixture);

    // `a` is read once (by the alias) and `b` once (by the return).
    try std.testing.expect(try chainIsSingleUse(&store, alias, &.{ a, b }));

    // A second read of the head link refuses the chain.
    const extra_head = try store.addCFStmt(.{ .assign_ref = .{
        .target = unit,
        .op = .{ .local = a },
        .next = alias,
    } }, .test_fixture);
    try std.testing.expect(!try chainIsSingleUse(&store, extra_head, &.{ a, b }));

    // A second read of the tail link refuses it just the same.
    const extra_tail = try store.addCFStmt(.{ .assign_ref = .{
        .target = unit,
        .op = .{ .local = b },
        .next = alias,
    } }, .test_fixture);
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
    const body = try store.addCFStmt(.{ .ret = .{ .value = unit } }, .test_fixture);

    const roc_proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(1),
        .args = try store.addLocalSpan(&.{}),
        .frame_locals = try store.addLocalSpan(&.{unit}),
        .body = body,
        .ret_layout = .zst,
        .abi = .roc,
    }, .none);
    try std.testing.expectEqual(body, rewritableProcBody(&store, roc_proc).?);

    const bodyless = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(2),
        .args = try store.addLocalSpan(&.{}),
        .frame_locals = try store.addLocalSpan(&.{}),
        .body = null,
        .ret_layout = .zst,
        .abi = .roc,
    }, .none);
    try std.testing.expect(rewritableProcBody(&store, bodyless) == null);

    const erased = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(3),
        .args = try store.addLocalSpan(&.{}),
        .frame_locals = try store.addLocalSpan(&.{unit}),
        .body = body,
        .ret_layout = .zst,
        .abi = .erased_callable,
    }, .none);
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

test "body_clone scratch is bounded by reachable locals and scopes" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    const scope: LIR.InlineScope = .{
        .source_symbol = store.freshSyntheticSymbol(),
        .source_name = .none,
        .source_loc = .none,
        .call_site = .none,
        .parent = .none,
    };
    for (0..65536) |_| {
        _ = try store.addLocal(.{ .layout_idx = .u64 });
        _ = try store.addCFStmt(.runtime_error, .test_fixture);
        _ = try store.addInlineScope(scope);
    }
    const desc = try store.addLocal(.{ .layout_idx = .u64 });
    const value = try store.addLocal(.{ .layout_idx = .u64, .boxy_desc = .{ .local = desc } });
    const alias = try store.addLocal(.{ .layout_idx = .u64 });
    const parent_scope = try store.addInlineScope(scope);
    var child_scope = scope;
    child_scope.parent = parent_scope;
    const source_scope = try store.addInlineScope(child_scope);
    var source_origin = LIR.StmtOrigin.test_fixture;
    source_origin.inline_scope = source_scope;
    const ret = try store.addCFStmt(.{ .ret = .{ .value = alias } }, source_origin);
    const copy = try store.addCFStmt(.{ .assign_ref = .{
        .target = alias,
        .op = .{ .local = value },
        .next = ret,
    } }, source_origin);
    const body = try store.addCFStmt(.{ .assign_boxy_desc_ref = .{
        .target = desc,
        .desc = .{ .local = value },
        .captures = try store.addLocalSpan(&.{ value, value }),
        .next = copy,
    } }, source_origin);
    var scratch: [32768]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&scratch);
    const allocator = fba.allocator();
    var reads = try countReachableReadsWithAllocator(&store, body, allocator);
    defer reads.deinit();
    var defs = try countReachableDefsWithAllocator(&store, body, allocator);
    defer defs.deinit();
    var flags = try collectReachableDefinitionsWithAllocator(&store, body, allocator);
    defer flags.deinit();
    try testing.expectEqual(@as(usize, 2), reads.counts.count());
    try testing.expectEqual(@as(u32, 4), reads.get(value));
    try testing.expectEqual(@as(u32, 1), reads.get(alias));
    try testing.expectEqual(@as(u32, 0), reads.get(desc));
    try testing.expectEqual(@as(u32, 1), defs.get(desc));
    try testing.expectEqual(@as(u32, 1), flags.get(alias));

    const dense = try testing.allocator.alloc(u32, store.localCount());
    defer testing.allocator.free(dense);
    @memset(dense, 0);
    for ([_]CFStmtId{ body, copy, ret }) |stmt| countStmtReads(&store, dense, store.getCFStmt(stmt));
    for (dense, 0..) |count, index| try testing.expectEqual(count, reads.get(@enumFromInt(index)));
    @memset(dense, 0);
    for ([_]CFStmtId{ body, copy, ret }) |stmt| countStmtDefs(&store, dense, store.getCFStmt(stmt));
    for (dense, 0..) |count, index| try testing.expectEqual(count, defs.get(@enumFromInt(index)));

    var cloner = try BodyCloner(TestRetRewriter).initWithAllocator(&store, .{}, allocator);
    defer cloner.deinit();
    cloner.inline_scope_outer = try store.addInlineScope(scope);
    const cloned = try cloner.cloneStmt(body);
    try testing.expectEqual(cloned, try cloner.cloneStmt(body));
    try testing.expectEqual(@as(usize, 3), cloner.local_map.count());
    try testing.expectEqual(@as(usize, 2), cloner.inline_scope_map.count());
    try testing.expectEqual(cloner.local_map.get(desc).?, store.getLocal(cloner.local_map.get(value).?).boxy_desc.?.local);
    const cloned_scope = store.stmtInlineScope(cloned);
    const cloned_parent = store.inlineScope(cloned_scope).parent;
    try testing.expectEqual(cloner.inline_scope_outer, store.inlineScope(cloned_parent).parent);
    try testing.expect(cloned_scope != source_scope);
}

test "body_clone sparse maps preserve loop back jumps and local reuse" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    const param = try store.addLocal(.{ .layout_idx = .u64 });
    const id: LIR.JoinPointId = @enumFromInt(9);
    const jump = try store.addCFStmt(.{ .jump = .{ .target = id } }, .test_fixture);
    const write = try store.addCFStmt(.{ .set_local = .{ .target = param, .value = param, .mode = .initialize_join_param, .next = jump } }, .test_fixture);
    const body = try store.addCFStmt(.{ .join = .{
        .id = id,
        .params = try store.addLocalSpan(&.{param}),
        .body = write,
        .remainder = write,
    } }, .test_fixture);
    var cloner = try BodyCloner(TestRetRewriter).init(&store, .{});
    defer cloner.deinit();
    const cloned = store.getCFStmt(try cloner.cloneStmt(body)).join;
    try testing.expectEqual(cloned.body, cloned.remainder);
    const cloned_write = store.getCFStmt(cloned.body).set_local;
    try testing.expectEqual(cloned_write.target, cloned_write.value);
    try testing.expectEqual(cloned_write.target, GuardedList.at(store.getLocalSpan(cloned.params), 0));
    try testing.expectEqual(cloned.id, store.getCFStmt(cloned_write.next).jump.target);
    try testing.expectEqual(@as(usize, 1), cloner.local_map.count());
}

test "body_clone join allocation belongs to the destination procedure" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    const end = try store.addCFStmt(.runtime_error, .test_fixture);
    _ = try store.addCFStmt(.{ .join = .{
        .id = @enumFromInt(9000),
        .params = LIR.LocalSpan.empty(),
        .body = end,
        .remainder = end,
    } }, .test_fixture);
    const destination = try store.addCFStmt(.{ .join = .{
        .id = @enumFromInt(40),
        .params = LIR.LocalSpan.empty(),
        .body = end,
        .remainder = end,
    } }, .test_fixture);
    const jump = try store.addCFStmt(.{ .jump = .{ .target = @enumFromInt(2) } }, .test_fixture);
    const source = try store.addCFStmt(.{ .join = .{
        .id = @enumFromInt(2),
        .params = LIR.LocalSpan.empty(),
        .body = jump,
        .remainder = jump,
    } }, .test_fixture);
    var inliner = try BodyCloner(TestRetRewriter).initWithInlineScopeOuter(&store, .{}, .none, destination);
    defer inliner.deinit();
    const cloned = store.getCFStmt(try inliner.cloneStmt(source)).join;
    try testing.expectEqual(@as(u32, 41), @intFromEnum(cloned.id));
    try testing.expectEqual(cloned.id, store.getCFStmt(cloned.body).jump.target);

    var index = JoinParamIndex.init(testing.allocator);
    defer index.deinit();
    try index.indexReachable(&store, destination);
    for (41..43) |expected| {
        var subtree = try BodyCloner(TestRetRewriter).initWithFreshDeclaredJoins(&store, .{}, source, &index);
        defer subtree.deinit();
        const clone = store.getCFStmt(try subtree.cloneStmt(source)).join;
        try testing.expectEqual(@as(u32, @intCast(expected)), @intFromEnum(clone.id));
    }
}

fn testSparseCountAllocations(allocator: Allocator, store: *LirStore, body: CFStmtId) Allocator.Error!void {
    var reads = try countReachableReadsWithAllocator(store, body, allocator);
    defer reads.deinit();
    var defs = try countReachableDefsWithAllocator(store, body, allocator);
    defer defs.deinit();
    var binders = try collectReachableDefinitionsWithAllocator(store, body, allocator);
    defer binders.deinit();
}

test "body_clone sparse counting propagates allocation failures" {
    var store = LirStore.init(std.testing.allocator);
    defer store.deinit();
    const local = try store.addLocal(.{ .layout_idx = .u64 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
    const body = try store.addCFStmt(.{ .assign_ref = .{
        .target = local,
        .op = .{ .local = local },
        .next = ret,
    } }, .test_fixture);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testSparseCountAllocations, .{ &store, body });
}

test "body_clone preserves external locals without preseeded map entries" {
    const Rewriter = struct {
        renamed: LocalId,

        pub fn preserveLocal(self: *@This(), local: LocalId) bool {
            return local != self.renamed;
        }
    };
    var store = LirStore.init(std.testing.allocator);
    defer store.deinit();
    const desc = try store.addLocal(.{ .layout_idx = .u64 });
    const value = try store.addLocal(.{ .layout_idx = .u64, .boxy_desc = .{ .local = desc } });
    var cloner = try BodyCloner(Rewriter).init(&store, .{ .renamed = value });
    defer cloner.deinit();
    const cloned = try cloner.mapLocal(value);
    try std.testing.expect(cloned != value);
    try std.testing.expectEqual(desc, store.getLocal(cloned).boxy_desc.?.local);
    try std.testing.expectEqual(cloned, try cloner.mapLocal(value));
    try std.testing.expectEqual(@as(usize, 1), cloner.new_locals.items.len);
    try std.testing.expectEqual(@as(usize, 2), cloner.local_map.count());
}

test "body_clone retained counts isolate nested inventories and clear only live entries" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    const low = try store.addLocal(.{ .layout_idx = .u64 });
    for (0..100000) |_| _ = try store.addLocal(.{ .layout_idx = .u64 });
    const high = try store.addLocal(.{ .layout_idx = .u64 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = high } }, .test_fixture);
    const copy = try store.addCFStmt(.{ .assign_ref = .{ .target = high, .op = .{ .local = low }, .next = ret } }, .test_fixture);
    // Both successors share a suffix: count its statements once, but count
    // both operand occurrences of low (the condition and the copy).
    const body = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = low,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = copy }}),
        .default_branch = copy,
    } }, .test_fixture);
    var meter = testing.FailingAllocator.init(testing.allocator, .{});
    var scratch = AnalysisScratch.init(meter.allocator());
    defer scratch.deinit();
    var inventories: [20]?ReadCounts = @splat(null);
    defer for (&inventories) |*inventory| {
        if (inventory.*) |*counts| counts.deinit();
    };
    var warmed_bytes: usize = 0;
    for (0..3) |round| {
        for (&inventories, 0..) |*inventory, index| {
            inventory.* = try countReachableReadsWithScratch(&store, if (index % 2 == round % 2) body else copy, &scratch);
        }
        for (&inventories, 0..) |*inventory, index| {
            const counts = &inventory.*.?;
            try testing.expectEqual(@as(u32, if (index % 2 == round % 2) 2 else 1), counts.get(low));
            try testing.expectEqual(@as(u32, 1), counts.get(high));
            try testing.expectEqual(@as(u32, 0), counts.get(@enumFromInt(1)));
        }
        if (round == 0) {
            warmed_bytes = meter.allocated_bytes;
        } else {
            try testing.expectEqual(warmed_bytes, meter.allocated_bytes);
        }
        // Release in non-stack order. More than eight inventories must keep
        // their capacity, and no result may borrow another result's counts.
        for (0..2) |parity| {
            for (&inventories, 0..) |*inventory, index| {
                if (index % 2 != parity) continue;
                inventory.*.?.deinit();
                inventory.* = null;
            }
        }
    }
    // Returning a partially consumed traversal must discard its work stack.
    {
        var walk = try ReachableStmts.initWithScratch(&store, body, &scratch);
        defer walk.deinit();
        try testing.expectEqual(body, (try walk.next()).?);
    }
    var walk = try ReachableStmts.initWithScratch(&store, ret, &scratch);
    defer walk.deinit();
    try testing.expectEqual(ret, (try walk.next()).?);
    try testing.expectEqual(@as(?CFStmtId, null), try walk.next());

    // Same IDs in a different store are different values, not cached facts.
    var other = LirStore.init(testing.allocator);
    defer other.deinit();
    const other_low = try other.addLocal(.{ .layout_idx = .u64 });
    const other_ret = try other.addCFStmt(.{ .ret = .{ .value = other_low } }, .test_fixture);
    var reads = try countReachableReadsWithScratch(&other, other_ret, &scratch);
    defer reads.deinit();
    try testing.expectEqual(@as(u32, 1), reads.get(low));
    try testing.expectEqual(@as(u32, 0), reads.get(high));
}

fn testRetainedCountAllocations(allocator: Allocator, store: *LirStore, body: CFStmtId) Allocator.Error!void {
    var scratch = AnalysisScratch.init(allocator);
    defer scratch.deinit();
    var reads = try countReachableReadsWithScratch(store, body, &scratch);
    defer reads.deinit();
    var defs = try countReachableDefsWithScratch(store, body, &scratch);
    defer defs.deinit();
    var binders = try collectReachableDefinitionsWithScratch(store, body, &scratch);
    defer binders.deinit();
}

test "body_clone retained counting returns every lease on allocation failure" {
    var store = LirStore.init(std.testing.allocator);
    defer store.deinit();
    const local = try store.addLocal(.{ .layout_idx = .u64 });
    for (0..256) |_| _ = try store.addLocal(.{ .layout_idx = .u64 });
    const distant = try store.addLocal(.{ .layout_idx = .u64 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
    const body = try store.addCFStmt(.{ .assign_low_level = .{
        .target = local,
        .op = .num_int_add_wrap,
        .rc_effect = .none(),
        .args = try store.addLocalSpan(&.{ local, distant }),
        .next = ret,
    } }, .test_fixture);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testRetainedCountAllocations, .{ &store, body });

    // Retry with the same owner after each possible allocation failure. An
    // aborted count may have inserted rows before its deferred failure surfaced.
    var fail_index: usize = 0;
    while (true) : (fail_index += 1) {
        var meter = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = fail_index });
        var scratch = AnalysisScratch.init(meter.allocator());
        defer scratch.deinit();
        if (countReachableReadsWithScratch(&store, body, &scratch)) |result| {
            var counts = result;
            counts.deinit();
            break;
        } else |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            meter.fail_index = std.math.maxInt(usize);
            var retry = try countReachableReadsWithScratch(&store, body, &scratch);
            defer retry.deinit();
            try std.testing.expectEqual(@as(u32, 2), retry.get(local));
            try std.testing.expectEqual(@as(u32, 1), retry.get(distant));
        }
    }
}
