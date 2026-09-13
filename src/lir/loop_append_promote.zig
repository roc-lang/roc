//! Promotes loop-carried append-only lists to slack-counted form.
//!
//! This runs after ScalarizeJoins and before ARC. A checked `List.append`
//! re-establishes every list invariant per element: it decodes the
//! seamless-slice tag, proves unique ownership through the refcount word, and
//! compares the grown length against capacity, all to almost always conclude
//! that one element may be stored. Inside a loop that only appends, those
//! facts are stable: ownership cannot change while the loop holds the only
//! reference, and capacity changes only when an append actually grows.
//!
//! The pass threads a fill limit (the length at which unchecked appends must
//! stop) alongside such a list. Entry computes it once as the list's length
//! plus `list_slack_unique`, which answers zero spare for shared or
//! slice-backed lists so their first append takes the checked path and
//! uniquifies. Each matched checked-append call is rewritten to
//!
//! ```text
//! if List.len(list) == limit { list = list_reserve(list, 1); limit = List.len(list) + list_slack_unique(list) }
//! list = list_append_unsafe(list, elem)
//! ```
//!
//! whose hot path is a compare against a loop-invariant register and the
//! store-plus-length-bump of the unchecked append; the length the append
//! already maintains doubles as the fill cursor, so nothing else is
//! decremented or tracked. Other
//! recognized list operations along the carried chain (range and sublist
//! appends, explicit reserves) are kept as they are, with the limit
//! recomputed after them because they may have grown or cloned the
//! allocation.
//!
//! Soundness rests on one invariant: a limit local is only ever consulted for
//! a value it was computed for, and the span from that value's length to the
//! limit under-approximates its true uniquely-owned spare capacity. The analysis works on a proc-wide value
//! flow graph: the carried chain is the forward closure of the loop parameter
//! through plain aliases, recognized operations, and join-parameter writes. A
//! merged carrier receives matching metadata on every definition: tracked
//! inputs forward it, and outside inputs transfer ownership before measuring
//! their own allocation. Chain membership alone does not initialize a merge.
//! A chain value with any unrecognized use is tainted (something may retain or
//! observe it); a tainted value may end the chain (escape to the loop's
//! result) but must not feed further chain edges, since a later unchecked
//! append through it could write into shared memory. Lowering emits one
//! `ref.local` alias per use, so a taint lands on the single-purpose alias
//! and leaves the chain spine clean.
//!
//! Element overwrites on a carried list thread an owned flag beside the
//! limit: one once the list uniquely owns a non-slice allocation, measured on
//! entry and re-established by every recognized operation. A set guarded by
//! that flag still costs a branch per element and keeps a checked call in the
//! loop body, which is enough to stop the backend from vectorizing the loop.
//! Once the flag reaches one it never drops back, so a loop whose flags are
//! all one is versioned: its head dispatches on the flags to a nested copy of
//! the body in which every set whose flag is known one runs the unchecked
//! store outright and every back edge that keeps the flags at one returns to
//! the copy. The original body stays as the cold arm for shared entries and
//! for edges that bring in a list of unknown ownership; those re-enter the
//! head, which measures again. Both bodies share the loop's parameter locals,
//! so the dispatch needs no argument copies, and the copy is lexically nested
//! inside the head's body so every jump still targets an enclosing join.

const std = @import("std");
const Allocator = std.mem.Allocator;
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");
const body_clone = @import("body_clone.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = LirStore.GuardedList;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const LowLevelOp = LIR.LowLevel;

/// Allocation failure raised while rewriting append statements.
pub const ResourceError = Allocator.Error;

/// Rewrite qualifying loops in every proc.
pub fn run(store: *LirStore, layouts: *const layout_mod.Store) ResourceError!void {
    var pass = Pass{
        .store = store,
        .layouts = layouts,
        .append_kind = collections.DenseMap(LIR.LirProcSpecId, ?ProcKind).init(store.allocator),
        .owned_defs = collections.DenseMap(LocalId, std.ArrayList(OwnedSource)).init(store.allocator),
        .set_dispatches = collections.DenseMap(CFStmtId, void).init(store.allocator),
        .loop_versions = collections.DenseMap(CFStmtId, LoopVersion).init(store.allocator),
    };
    defer {
        pass.resetProcState();
        pass.append_kind.deinit();
        pass.owned_defs.deinit();
        pass.set_dispatches.deinit();
        pass.loop_versions.deinit();
    }

    const proc_count = store.procSpecCount();
    var proc_index: usize = 0;
    while (proc_index < proc_count) : (proc_index += 1) {
        try pass.transformProc(@enumFromInt(proc_index));
    }
}

/// One definition of an owned-flag local.
const OwnedSource = union(enum) {
    /// The literal one a rewritten site establishes.
    one,
    /// A `list_owned_unique` measurement of a list the chain did not carry.
    measured,
    /// A copy of, or a parameter write from, another owned-flag local.
    local: LocalId,
};

/// Versioning bookkeeping for one promoted loop.
const LoopVersion = struct {
    /// Owned-flag parameters the loop carries, one per promoted list parameter.
    owned_params: std.ArrayList(LocalId) = .empty,
    /// Every jump into the loop, with the owned values written on that edge.
    edge_values: collections.DenseMap(CFStmtId, std.ArrayList(LocalId)),
    /// An edge's jump could not be located, so the loop keeps its flag form.
    unresolved: bool = false,

    fn deinit(self: *LoopVersion, allocator: Allocator) void {
        self.owned_params.deinit(allocator);
        var values = self.edge_values.valueIterator();
        while (values.next()) |list| list.deinit(allocator);
        self.edge_values.deinit();
    }
};

/// What a helper proc's linear body reduces to.
const ProcKind = enum {
    /// `list_reserve(arg0, arg1)`.
    reserve,
    /// `list_append_unsafe(arg0, arg1)`.
    append_unsafe,
    /// `list_append_unsafe(list_reserve(arg0, <literal >= 1>), arg1)`.
    checked_append,
};

/// How one statement uses one chain local.
const EdgeKind = enum {
    /// `target = ref.local source`.
    alias,
    /// A matched checked-append call: `target = append(source, elem)`.
    append_call,
    /// A kept list operation returning the (possibly reallocated) list.
    refresh_op,
    /// A matched range-within append: `target = list_append_range_within(source, start, count)`.
    range_append,
    /// A matched element overwrite: `target = list_set(source, index, elem)`.
    set_op,
    /// `set param := source` with initialize-join-param mode.
    param_write,
};

/// One forward value edge of the chain graph.
const Edge = struct {
    kind: EdgeKind,
    stmt: CFStmtId,
    source: LocalId,
    target: LocalId,
    /// Classified once for the current candidate, before validating or emitting
    /// metadata. Entry definitions supply a new list rather than chain facts.
    flow: enum { outside, carried, entry } = .outside,
};

const Pass = struct {
    store: *LirStore,
    layouts: *const layout_mod.Store,
    append_kind: collections.DenseMap(LIR.LirProcSpecId, ?ProcKind),
    /// Definitions of every owned-flag local threaded in the current proc.
    owned_defs: collections.DenseMap(LocalId, std.ArrayList(OwnedSource)),
    /// Set-site dispatch switches of the current proc.
    set_dispatches: collections.DenseMap(CFStmtId, void),
    /// Promoted loops of the current proc, keyed by their join statement.
    loop_versions: collections.DenseMap(CFStmtId, LoopVersion),

    fn resetProcState(self: *Pass) void {
        const allocator = self.store.allocator;
        var defs = self.owned_defs.valueIterator();
        while (defs.next()) |list| list.deinit(allocator);
        self.owned_defs.clearRetainingCapacity();
        self.set_dispatches.clearRetainingCapacity();
        var versions = self.loop_versions.valueIterator();
        while (versions.next()) |version| version.deinit(allocator);
        self.loop_versions.clearRetainingCapacity();
    }

    fn noteOwnedDef(self: *Pass, local: LocalId, source: OwnedSource) ResourceError!void {
        const entry = try self.owned_defs.getOrPut(local);
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        try entry.value_ptr.append(self.store.allocator, source);
    }

    fn loopVersion(self: *Pass, loop_stmt: CFStmtId) ResourceError!*LoopVersion {
        const entry = try self.loop_versions.getOrPut(loop_stmt);
        if (!entry.found_existing) {
            entry.value_ptr.* = .{ .edge_values = collections.DenseMap(CFStmtId, std.ArrayList(LocalId)).init(self.store.allocator) };
        }
        return entry.value_ptr;
    }

    // Helper-proc classification

    /// Symbolic value of a local inside a linear helper body.
    const Abstract = union(enum) {
        arg: u16,
        literal: u64,
        /// `list_reserve(arg0, arg1)`: the spare is forwarded.
        reserve_forward,
        /// `list_reserve(arg0, <literal >= 1>)`.
        reserve_lit,
        /// `list_append_unsafe(arg0, arg1)`.
        unsafe_of_args,
        /// `list_append_unsafe(<reserve of arg0 with spare >= 1>, arg1)`.
        append_of_reserve,
        other,
    };

    fn classifyProc(self: *Pass, proc_id: LIR.LirProcSpecId, depth: u32) ResourceError!?ProcKind {
        if (depth > 6) return null;
        if (self.append_kind.get(proc_id)) |cached| return cached;
        // Seed the cache so a recursive proc settles to "not a helper".
        try self.append_kind.put(proc_id, null);
        const kind = try self.classifyProcUncached(proc_id, depth);
        try self.append_kind.put(proc_id, kind);
        return kind;
    }

    fn classifyProcUncached(self: *Pass, proc_id: LIR.LirProcSpecId, depth: u32) ResourceError!?ProcKind {
        const proc = self.store.getProcSpec(proc_id);
        if (proc.body == null or proc.hosted != null or proc.abi != .roc) return null;
        const params = self.store.getLocalSpan(proc.args);
        if (GuardedList.borrowLen(params) != 2) return null;

        var env = collections.DenseMap(LocalId, Abstract).init(self.store.allocator);
        defer env.deinit();
        try env.put(GuardedList.at(params, 0), .{ .arg = 0 });
        try env.put(GuardedList.at(params, 1), .{ .arg = 1 });

        var current = proc.body.?;
        var steps: u32 = 0;
        while (steps < 64) : (steps += 1) {
            switch (self.store.getCFStmt(current)) {
                .assign_ref => |assign| {
                    const value: Abstract = switch (assign.op) {
                        .local => |src| env.get(src) orelse .other,
                        .discriminant, .field, .tag_payload, .tag_payload_struct, .list_reinterpret, .nominal => .other,
                    };
                    try env.put(assign.target, value);
                    current = assign.next;
                },
                .assign_literal => |assign| {
                    const value: Abstract = switch (assign.value) {
                        .i64_literal => |lit| if (lit.value >= 0) .{ .literal = @intCast(lit.value) } else Abstract.other,
                        .i128_literal => |lit| if (lit.value >= 0 and lit.value <= std.math.maxInt(u64)) .{ .literal = @intCast(lit.value) } else Abstract.other,
                        .f64_literal, .f32_literal, .dec_literal, .str_literal, .static_data, .bytes_literal, .null_ptr, .proc_ref, .boxy_dynamic_num_literal, .boxy_dynamic_frac_literal => .other,
                    };
                    try env.put(assign.target, value);
                    current = assign.next;
                },
                .assign_low_level => |assign| {
                    try env.put(assign.target, try self.classifyStep(&env, assign.op, assign.args, null, depth));
                    current = assign.next;
                },
                .assign_call => |assign| {
                    try env.put(assign.target, try self.classifyStep(&env, null, assign.args, assign.proc, depth));
                    current = assign.next;
                },
                .ret => |ret_stmt| {
                    const value = env.get(ret_stmt.value) orelse return null;
                    return switch (value) {
                        .reserve_forward, .reserve_lit => .reserve,
                        .unsafe_of_args => .append_unsafe,
                        .append_of_reserve => .checked_append,
                        .arg, .literal, .other => null,
                    };
                },
                .init_uninitialized,
                .assign_call_erased,
                .assign_packed_erased_fn,
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
                .join,
                .jump,
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
        return null;
    }

    /// Abstract outcome of one call or low-level step of a helper body.
    fn classifyStep(
        self: *Pass,
        env: *collections.DenseMap(LocalId, Abstract),
        op: ?LowLevelOp,
        args_span: LIR.LocalSpan,
        callee: ?LIR.LirProcSpecId,
        depth: u32,
    ) ResourceError!Abstract {
        const args = self.store.getLocalSpan(args_span);
        if (GuardedList.borrowLen(args) != 2) return .other;
        const a0 = env.get(GuardedList.at(args, 0)) orelse return .other;
        const a1 = env.get(GuardedList.at(args, 1)) orelse return .other;

        const step: enum { reserve, append_unsafe, checked_append, other } = blk: {
            if (op) |low_level| {
                if (low_level == .list_reserve) break :blk .reserve;
                if (low_level == .list_append_unsafe) break :blk .append_unsafe;
                break :blk .other;
            }
            const kind = (try self.classifyProc(callee.?, depth + 1)) orelse break :blk .other;
            break :blk switch (kind) {
                .reserve => .reserve,
                .append_unsafe => .append_unsafe,
                .checked_append => .checked_append,
            };
        };

        return switch (step) {
            .reserve => switch (a0) {
                .arg => |n| blk: {
                    if (n != 0) break :blk .other;
                    break :blk switch (a1) {
                        .literal => |k| if (k >= 1) Abstract.reserve_lit else .other,
                        .arg => |m| if (m == 1) Abstract.reserve_forward else .other,
                        .reserve_forward, .reserve_lit, .unsafe_of_args, .append_of_reserve, .other => .other,
                    };
                },
                .literal, .reserve_forward, .reserve_lit, .unsafe_of_args, .append_of_reserve, .other => .other,
            },
            .append_unsafe => switch (a0) {
                // The spare must be a known positive literal by the time the
                // unsafe append consumes the reserved list; a still-forwarded
                // spare could be zero at runtime.
                .reserve_lit => switch (a1) {
                    .arg => |n| if (n == 1) Abstract.append_of_reserve else .other,
                    .literal, .reserve_forward, .reserve_lit, .unsafe_of_args, .append_of_reserve, .other => .other,
                },
                .arg => |n| blk: {
                    if (n != 0) break :blk .other;
                    break :blk switch (a1) {
                        .arg => |m| if (m == 1) Abstract.unsafe_of_args else .other,
                        .literal, .reserve_forward, .reserve_lit, .unsafe_of_args, .append_of_reserve, .other => .other,
                    };
                },
                .literal, .reserve_forward, .unsafe_of_args, .append_of_reserve, .other => .other,
            },
            .checked_append => switch (a0) {
                .arg => |n| if (n == 0 and a1 == .arg and a1.arg == 1) Abstract.append_of_reserve else .other,
                .literal, .reserve_forward, .reserve_lit, .unsafe_of_args, .append_of_reserve, .other => .other,
            },
            .other => .other,
        };
    }

    // Proc scan: flow edges, use accounting, joins

    const JoinInfo = struct {
        stmt: CFStmtId,
        has_back_edge: bool,
    };

    const Scan = struct {
        /// Forward value edges of every potentially chain-relevant statement.
        edges: std.ArrayList(Edge) = .empty,
        /// Per-local counts of total operand uses and of uses this pass
        /// understands. A carrier with untracked uses is tainted.
        total_uses: collections.DenseMap(LocalId, u32),
        tracked_uses: collections.DenseMap(LocalId, u32),
        /// Owning join statement of every join parameter.
        param_join: collections.DenseMap(LocalId, CFStmtId),
        joins: std.ArrayList(JoinInfo) = .empty,
        max_join_id: u32 = 0,
        /// Locals written by `set_local` in any mode other than
        /// initialize-join-param; a chain parameter in this set has writes the
        /// analysis does not model.
        dirty_targets: collections.DenseMap(LocalId, void),
        /// Direct-assignment definition counts per local. A join parameter
        /// initialized this way on some edge has no place to receive a slack
        /// write, and a chain value defined by more than one statement has no
        /// single slack: branches can assign the same result local and
        /// converge without a join parameter, leaving the other path's slack
        /// never computed.
        assigned_targets: collections.DenseMap(LocalId, u32),

        fn deinit(self: *Scan, allocator: Allocator) void {
            self.edges.deinit(allocator);
            self.total_uses.deinit();
            self.tracked_uses.deinit();
            self.param_join.deinit();
            self.joins.deinit(allocator);
            self.dirty_targets.deinit();
            self.assigned_targets.deinit();
        }
    };

    fn bumpUse(map: *collections.DenseMap(LocalId, u32), local: LocalId) ResourceError!void {
        const entry = try map.getOrPut(local);
        if (!entry.found_existing) entry.value_ptr.* = 0;
        entry.value_ptr.* += 1;
    }

    fn noteUse(scan: *Scan, local: LocalId, tracked: bool) ResourceError!void {
        try bumpUse(&scan.total_uses, local);
        if (tracked) try bumpUse(&scan.tracked_uses, local);
    }

    /// Whether a local's layout is a list; only list locals join the chain.
    fn isListLocal(self: *Pass, local: LocalId) bool {
        return self.layouts.getLayout(self.store.getLocal(local).layout_idx).tag == .list;
    }

    fn scanProc(self: *Pass, body: CFStmtId, scan: *Scan) ResourceError!void {
        const allocator = self.store.allocator;
        var stack = std.ArrayList(CFStmtId).empty;
        defer stack.deinit(allocator);
        var visited = collections.DenseMap(CFStmtId, void).init(allocator);
        defer visited.deinit();
        try stack.append(allocator, body);
        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});
            switch (self.store.getCFStmt(current)) {
                .assign_ref => |assign| {
                    try bumpUse(&scan.assigned_targets, assign.target);
                    switch (assign.op) {
                        .local => |src| {
                            if (self.isListLocal(src) and self.isListLocal(assign.target)) {
                                try scan.edges.append(allocator, .{ .kind = .alias, .stmt = current, .source = src, .target = assign.target });
                                try noteUse(scan, src, true);
                            } else {
                                try noteUse(scan, src, false);
                            }
                        },
                        .discriminant => |op| try noteUse(scan, op.source, false),
                        .field => |op| try noteUse(scan, op.source, false),
                        .tag_payload => |op| try noteUse(scan, op.source, false),
                        .tag_payload_struct => |op| try noteUse(scan, op.source, false),
                        .list_reinterpret => |op| try noteUse(scan, op.backing_ref, false),
                        .nominal => |op| try noteUse(scan, op.backing_ref, false),
                    }
                    try stack.append(allocator, assign.next);
                },
                .assign_call => |assign| {
                    try bumpUse(&scan.assigned_targets, assign.target);
                    const args = self.store.getLocalSpan(assign.args);
                    const arg_count = GuardedList.borrowLen(args);
                    var matched = false;
                    if (arg_count == 2 and self.isListLocal(GuardedList.at(args, 0)) and self.isListLocal(assign.target) and !self.isListLocal(GuardedList.at(args, 1))) {
                        if ((try self.classifyProc(assign.proc, 0)) == ProcKind.checked_append) {
                            matched = true;
                            try scan.edges.append(allocator, .{
                                .kind = .append_call,
                                .stmt = current,
                                .source = GuardedList.at(args, 0),
                                .target = assign.target,
                            });
                        }
                    }
                    for (0..arg_count) |i| {
                        const arg = GuardedList.at(args, i);
                        // The list argument of a matched append is tracked;
                        // its element argument is an ordinary consumed value
                        // that the rewrite passes through unchanged, so it is
                        // tracked for the element local too.
                        try noteUse(scan, arg, matched);
                    }
                    try stack.append(allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    try bumpUse(&scan.assigned_targets, assign.target);
                    const args = self.store.getLocalSpan(assign.args);
                    const arg_count = GuardedList.borrowLen(args);
                    const list_arg0 = arg_count > 0 and self.isListLocal(GuardedList.at(args, 0));
                    const rebinds = assign.op == .list_reserve or assign.op == .list_append_unsafe or assign.op == .list_append_range_within or assign.op == .list_copy_range_within or assign.op == .list_append_sublist or assign.op == .list_append_le_bytes or assign.op == .list_set;
                    const read_ok = assign.op == .list_len or assign.op == .list_get_unsafe or assign.op == .list_slack_unique;
                    if (rebinds and list_arg0 and self.isListLocal(assign.target)) {
                        // Range-within appends promote to a slack-guarded
                        // diamond of their own; zero-sized elements have no
                        // bytes to copy, so they keep the checked call.
                        const elem_size = self.layouts.builtinListAbi(self.store.getLocal(assign.target).layout_idx).elem_size;
                        const kind: EdgeKind = if (elem_size == 0)
                            .refresh_op
                        else if (assign.op == .list_append_range_within)
                            .range_append
                        else if (assign.op == .list_set)
                            .set_op
                        else
                            .refresh_op;
                        try scan.edges.append(allocator, .{
                            .kind = kind,
                            .stmt = current,
                            .source = GuardedList.at(args, 0),
                            .target = assign.target,
                        });
                    }
                    for (0..arg_count) |i| {
                        const arg = GuardedList.at(args, i);
                        // The first argument of the recognized operations is a
                        // modeled use; any list appearing in another operand
                        // position (a source list to copy from, say) is a
                        // borrow the chain analysis treats as foreign.
                        const tracked = (i == 0 and list_arg0 and (rebinds or read_ok)) or
                            (i > 0 and !self.isListLocal(arg));
                        try noteUse(scan, arg, tracked);
                    }
                    try stack.append(allocator, assign.next);
                },
                .set_local => |assign| {
                    if (assign.mode == .initialize_join_param and self.isListLocal(assign.value) and self.isListLocal(assign.target)) {
                        try scan.edges.append(allocator, .{ .kind = .param_write, .stmt = current, .source = assign.value, .target = assign.target });
                        try noteUse(scan, assign.value, true);
                    } else {
                        try noteUse(scan, assign.value, false);
                        try scan.dirty_targets.put(assign.target, {});
                    }
                    try stack.append(allocator, assign.next);
                },
                .ret => |ret_stmt| {
                    // Returning ends the chain; the slack local dies with the
                    // frame.
                    try noteUse(scan, ret_stmt.value, true);
                },
                .join => |join| {
                    try scan.joins.append(allocator, .{ .stmt = current, .has_back_edge = false });
                    scan.max_join_id = @max(scan.max_join_id, @intFromEnum(join.id) + 1);
                    const params = self.store.getLocalSpan(join.params);
                    for (0..GuardedList.borrowLen(params)) |i| {
                        try scan.param_join.put(GuardedList.at(params, i), current);
                    }
                    try stack.append(allocator, join.body);
                    try stack.append(allocator, join.remainder);
                },
                .switch_stmt => |s| {
                    try noteUse(scan, s.cond, false);
                    const branches = self.store.getCFSwitchBranches(s.branches);
                    for (0..GuardedList.borrowLen(branches)) |i| try stack.append(allocator, GuardedList.at(branches, i).body);
                    try stack.append(allocator, s.default_branch);
                    if (s.continuation) |continuation| try stack.append(allocator, continuation);
                },
                .switch_initialized_payload => |s| {
                    try noteUse(scan, s.cond, false);
                    try stack.append(allocator, s.initialized_branch);
                    try stack.append(allocator, s.uninitialized_branch);
                },
                .str_match => |s| {
                    try noteUse(scan, s.source, false);
                    try stack.append(allocator, s.on_match);
                    try stack.append(allocator, s.on_miss);
                },
                .str_match_set => |s| {
                    try noteUse(scan, s.source, false);
                    const arms = self.store.getStrMatchArms(s.arms);
                    for (0..GuardedList.borrowLen(arms)) |i| try stack.append(allocator, GuardedList.at(arms, i).on_match);
                    try stack.append(allocator, s.on_miss);
                },
                .assign_literal => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try stack.append(allocator, s.next);
                },
                .init_uninitialized => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try stack.append(allocator, s.next);
                },
                .comptime_branch_taken => |s| try stack.append(allocator, s.next),
                .assign_call_erased => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    const args = self.store.getLocalSpan(s.args);
                    for (0..GuardedList.borrowLen(args)) |i| try noteUse(scan, GuardedList.at(args, i), false);
                    try noteUse(scan, s.closure, false);
                    try stack.append(allocator, s.next);
                },
                .assign_packed_erased_fn => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try stack.append(allocator, s.next);
                },
                .assign_list => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    const elems = self.store.getLocalSpan(s.elems);
                    for (0..GuardedList.borrowLen(elems)) |i| try noteUse(scan, GuardedList.at(elems, i), false);
                    try stack.append(allocator, s.next);
                },
                .assign_struct => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    const fields = self.store.getLocalSpan(s.fields);
                    for (0..GuardedList.borrowLen(fields)) |i| try noteUse(scan, GuardedList.at(fields, i), false);
                    try stack.append(allocator, s.next);
                },
                .assign_tag => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    if (s.payload) |payload| try noteUse(scan, payload, false);
                    try stack.append(allocator, s.next);
                },
                .store_struct => |s| {
                    try noteUse(scan, s.dest, false);
                    const fields = self.store.getLocalSpan(s.fields);
                    for (0..GuardedList.borrowLen(fields)) |i| try noteUse(scan, GuardedList.at(fields, i), false);
                    try stack.append(allocator, s.next);
                },
                .store_tag => |s| {
                    try noteUse(scan, s.dest, false);
                    if (s.payload) |payload| try noteUse(scan, payload, false);
                    try stack.append(allocator, s.next);
                },
                .debug => |s| {
                    try noteUse(scan, s.message, false);
                    try stack.append(allocator, s.next);
                },
                .expect => |s| {
                    try noteUse(scan, s.condition, false);
                    try stack.append(allocator, s.next);
                },
                .incref => |s| {
                    try noteUse(scan, s.value, false);
                    try stack.append(allocator, s.next);
                },
                .decref => |s| {
                    try noteUse(scan, s.value, false);
                    try stack.append(allocator, s.next);
                },
                .decref_if_initialized => |s| {
                    try noteUse(scan, s.value, false);
                    try noteUse(scan, s.cond, false);
                    try stack.append(allocator, s.next);
                },
                .free => |s| {
                    try noteUse(scan, s.value, false);
                    try stack.append(allocator, s.next);
                },
                .expect_err => |s| try noteUse(scan, s.message, false),
                .assign_boxy_desc_ref => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try stack.append(allocator, s.next);
                },
                .assign_boxy_dict_ref => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try stack.append(allocator, s.next);
                },
                .assign_boxy_box => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try noteUse(scan, s.payload, false);
                    try stack.append(allocator, s.next);
                },
                .assign_boxy_reuse_box => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try noteUse(scan, s.source, false);
                    try stack.append(allocator, s.next);
                },
                .assign_boxy_unbox => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try noteUse(scan, s.source, false);
                    try stack.append(allocator, s.next);
                },
                .assign_boxy_adapt => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try noteUse(scan, s.source, false);
                    try stack.append(allocator, s.next);
                },
                .assign_boxy_inspect => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try noteUse(scan, s.source, false);
                    try stack.append(allocator, s.next);
                },
                .assign_boxy_eq => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try noteUse(scan, s.lhs, false);
                    try noteUse(scan, s.rhs, false);
                    try stack.append(allocator, s.next);
                },
                .assign_boxy_tag => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    if (s.payload) |payload| try noteUse(scan, payload, false);
                    try stack.append(allocator, s.next);
                },
                .assign_boxy_tag_payload => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    try noteUse(scan, s.source, false);
                    try stack.append(allocator, s.next);
                },
                .assign_call_dict => |s| {
                    try bumpUse(&scan.assigned_targets, s.target);
                    const args = self.store.getLocalSpan(s.args);
                    for (0..GuardedList.borrowLen(args)) |i| try noteUse(scan, GuardedList.at(args, i), false);
                    try stack.append(allocator, s.next);
                },
                .boxy_tag_match => |s| {
                    try noteUse(scan, s.source, false);
                    try stack.append(allocator, s.on_match);
                    try stack.append(allocator, s.on_miss);
                },
                .jump, .crash, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }
    }

    /// Mark joins that have a back edge (a jump to their own id inside their
    /// body subtree): those are loops.
    fn markLoops(self: *Pass, scan: *Scan) ResourceError!void {
        for (scan.joins.items) |*info| {
            const join = self.store.getCFStmt(info.stmt).join;
            info.has_back_edge = try self.subtreeJumpsTo(join.body, join.id);
        }
    }

    fn subtreeJumpsTo(self: *Pass, body: CFStmtId, id: LIR.JoinPointId) ResourceError!bool {
        const allocator = self.store.allocator;
        var stack = std.ArrayList(CFStmtId).empty;
        defer stack.deinit(allocator);
        var visited = collections.DenseMap(CFStmtId, void).init(allocator);
        defer visited.deinit();
        try stack.append(allocator, body);
        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});
            switch (self.store.getCFStmt(current)) {
                .jump => |jump| if (jump.target == id) return true,
                .join => |join| {
                    try stack.append(allocator, join.body);
                    try stack.append(allocator, join.remainder);
                },
                .switch_stmt => |s| {
                    const branches = self.store.getCFSwitchBranches(s.branches);
                    for (0..GuardedList.borrowLen(branches)) |i| try stack.append(allocator, GuardedList.at(branches, i).body);
                    try stack.append(allocator, s.default_branch);
                    if (s.continuation) |continuation| try stack.append(allocator, continuation);
                },
                .switch_initialized_payload => |s| {
                    try stack.append(allocator, s.initialized_branch);
                    try stack.append(allocator, s.uninitialized_branch);
                },
                .str_match => |s| {
                    try stack.append(allocator, s.on_match);
                    try stack.append(allocator, s.on_miss);
                },
                .str_match_set => |s| {
                    const arms = self.store.getStrMatchArms(s.arms);
                    for (0..GuardedList.borrowLen(arms)) |i| try stack.append(allocator, GuardedList.at(arms, i).on_match);
                    try stack.append(allocator, s.on_miss);
                },
                .boxy_tag_match => |s| {
                    try stack.append(allocator, s.on_match);
                    try stack.append(allocator, s.on_miss);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict => |s| {
                    try stack.append(allocator, s.next);
                },
                .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }
        return false;
    }

    // Per-parameter qualification and rewrite

    fn transformProc(self: *Pass, proc_id: LIR.LirProcSpecId) ResourceError!void {
        const proc = self.store.getProcSpec(proc_id);
        if (proc.body == null or proc.hosted != null) return;
        const allocator = self.store.allocator;
        self.resetProcState();

        var proc_args = collections.DenseMap(LocalId, void).init(allocator);
        defer proc_args.deinit();
        {
            const args = self.store.getLocalSpan(proc.args);
            for (0..GuardedList.borrowLen(args)) |i| try proc_args.put(GuardedList.at(args, i), {});
        }

        var new_locals = std.ArrayList(LocalId).empty;
        defer new_locals.deinit(allocator);
        // A promotion rewrites statements the scan indexed, so each round
        // works from a fresh scan and promotes at most one parameter;
        // already-promoted parameters are skipped by identity on later
        // rounds.
        var attempted = collections.DenseMap(LocalId, void).init(allocator);
        defer attempted.deinit();

        var max_join_id: u32 = 0;
        var promoting = true;
        while (promoting) {
            promoting = false;
            var scan = Scan{
                .total_uses = collections.DenseMap(LocalId, u32).init(allocator),
                .tracked_uses = collections.DenseMap(LocalId, u32).init(allocator),
                .param_join = collections.DenseMap(LocalId, CFStmtId).init(allocator),
                .dirty_targets = collections.DenseMap(LocalId, void).init(allocator),
                .assigned_targets = collections.DenseMap(LocalId, u32).init(allocator),
            };
            defer scan.deinit(allocator);
            try self.scanProc(self.store.getProcSpec(proc_id).body.?, &scan);
            max_join_id = @max(max_join_id, scan.max_join_id);
            if (scan.edges.items.len == 0) break;
            try self.markLoops(&scan);
            outer: for (scan.joins.items) |info| {
                if (!info.has_back_edge) continue;
                const join = self.store.getCFStmt(info.stmt).join;
                const params = self.store.getLocalSpan(join.params);
                const param_count = GuardedList.borrowLen(params);
                var position: usize = 0;
                while (position < param_count) : (position += 1) {
                    const param = GuardedList.at(params, position);
                    if (!self.isListLocal(param)) continue;
                    if (attempted.contains(param)) continue;
                    try attempted.put(param, {});
                    if (try self.promoteParam(&scan, &proc_args, info.stmt, param, &max_join_id, &new_locals)) {
                        promoting = true;
                        break :outer;
                    }
                }
            }
        }

        try self.versionLoops(self.store.getProcSpec(proc_id).body.?, &max_join_id, &new_locals);

        if (new_locals.items.len > 0) {
            const spec = self.store.getProcSpec(proc_id);
            const frame = self.store.getLocalSpan(spec.frame_locals);
            var combined = std.ArrayList(LocalId).empty;
            defer combined.deinit(allocator);
            for (0..GuardedList.borrowLen(frame)) |i| try combined.append(allocator, GuardedList.at(frame, i));
            try combined.appendSlice(allocator, new_locals.items);
            const frame_span = try self.store.addLocalSpan(combined.items);
            self.store.getProcSpecPtr(proc_id).frame_locals = frame_span;
        }
    }

    fn promoteParam(
        self: *Pass,
        scan: *Scan,
        proc_args: *collections.DenseMap(LocalId, void),
        loop_stmt: CFStmtId,
        list_param: LocalId,
        max_join_id: *u32,
        new_locals: *std.ArrayList(LocalId),
    ) ResourceError!bool {
        const allocator = self.store.allocator;

        // Forward closure of the loop parameter over the chain edges.
        var carriers = collections.DenseMap(LocalId, void).init(allocator);
        defer carriers.deinit();
        try carriers.put(list_param, {});
        var changed = true;
        while (changed) {
            changed = false;
            for (scan.edges.items) |edge| {
                if (!carriers.contains(edge.source)) continue;
                if (!carriers.contains(edge.target)) {
                    try carriers.put(edge.target, {});
                    changed = true;
                }
            }
        }

        var rewrite_site_count: u32 = 0;
        var has_sets = false;
        var chain_params = collections.DenseMap(LocalId, CFStmtId).init(allocator);
        defer chain_params.deinit();
        try chain_params.put(list_param, loop_stmt);

        for (scan.edges.items) |*edge| {
            edge.flow = if (!carriers.contains(edge.target)) .outside else if (carriers.contains(edge.source)) .carried else .entry;
            if (edge.flow != .carried) continue;
            switch (edge.kind) {
                .append_call, .range_append => rewrite_site_count += 1,
                .set_op => {
                    rewrite_site_count += 1;
                    has_sets = true;
                },
                .param_write => {
                    const owner = scan.param_join.get(edge.target) orelse return false;
                    try chain_params.put(edge.target, owner);
                },
                .alias, .refresh_op => {},
            }
        }
        if (rewrite_site_count == 0) return false;

        // A write-less entry (a parameter that doubles as a proc argument)
        // would leave the slack parameter uninitialized on that path.
        {
            var it = chain_params.keyIterator();
            while (it.next()) |param| {
                if (proc_args.contains(param.*)) return false;
            }
        }

        // Chain parameters must not be conditionally initialized: that
        // machinery names whole locals and is not modeled here.
        {
            var it = chain_params.valueIterator();
            while (it.next()) |owner| {
                const join = self.store.getCFStmt(owner.*).join;
                const maybe = self.store.getLocalSpan(join.maybe_uninitialized_params);
                for (0..GuardedList.borrowLen(maybe)) |i| {
                    if (carriers.contains(GuardedList.at(maybe, i))) return false;
                }
            }
        }

        // A chain parameter's non-carrier writes are entry edges: the wiring
        // in `apply` measures the incoming list's slack fresh at each one, so
        // a foreign value never runs under a slack computed for another list.
        // This covers both the promoted loop's own parameter and any nested
        // header a shape split introduced.
        // Writes to a chain parameter by any other set-local mode, or by a
        // direct assignment on some edge, leave paths with no place to hand
        // over a slack value.
        {
            var it = chain_params.keyIterator();
            while (it.next()) |param| {
                if (scan.dirty_targets.contains(param.*)) return false;
                if (scan.assigned_targets.contains(param.*)) return false;
            }
        }

        // Every definition of a non-parameter carrier must have an edge plan.
        // Locals are not single-assignment: branch results converge by
        // assigning one local in each arm, so a carrier may have several
        // definitions. Carried definitions forward metadata, and entry
        // definitions measure the incoming list. Unmodeled definitions cannot
        // supply metadata on their paths.
        var chain_defs = collections.DenseMap(LocalId, u32).init(allocator);
        defer chain_defs.deinit();
        {
            for (scan.edges.items) |edge| {
                if (edge.kind == .param_write) continue;
                if (edge.flow == .outside) continue;
                try bumpUse(&chain_defs, edge.target);
            }
            var it = chain_defs.iterator();
            while (it.next()) |entry| {
                if (chain_params.contains(entry.key_ptr.*)) continue;
                const assigned = scan.assigned_targets.get(entry.key_ptr.*) orelse 0;
                if (assigned != entry.value_ptr.*) return false;
            }
        }

        // A tainted carrier (one with uses this analysis does not understand)
        // must be terminal: if it feeds any chain edge, an unchecked append
        // could later run on a value whose uniqueness the untracked use may
        // have broken.
        for (scan.edges.items) |edge| {
            if (!carriers.contains(edge.source)) continue;
            const total = scan.total_uses.get(edge.source) orelse 0;
            const tracked = scan.tracked_uses.get(edge.source) orelse 0;
            if (total != tracked) return false;
        }

        // Qualified: thread the slack.
        try self.apply(scan, &chain_defs, &chain_params, has_sets, loop_stmt, list_param, max_join_id, new_locals);
        return true;
    }

    // Rewrite

    /// Emit `limit_target = List.len(list) + list_slack_unique(list)` ending
    /// at `next`, returning the head statement. The sum cannot wrap: length
    /// plus spare is the capacity, which is bounded by the allocator.
    fn seedLimit(
        self: *Pass,
        list: LocalId,
        limit_target: LocalId,
        next: CFStmtId,
        new_locals: *std.ArrayList(LocalId),
    ) ResourceError!CFStmtId {
        const spare = try self.freshLocal(.u64, new_locals);
        const len = try self.freshLocal(.u64, new_locals);
        const add = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = limit_target,
            .op = .num_int_add_wrap,
            .rc_effect = LowLevelOp.num_int_add_wrap.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ len, spare }),
            .next = next,
        } });
        const measure_len = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = len,
            .op = .list_len,
            .rc_effect = LowLevelOp.list_len.rcEffect(),
            .args = try self.store.addLocalSpan(&.{list}),
            .next = add,
        } });
        return try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = spare,
            .op = .list_slack_unique,
            .rc_effect = LowLevelOp.list_slack_unique.rcEffect(),
            .args = try self.store.addLocalSpan(&.{list}),
            .next = measure_len,
        } });
    }

    fn freshLocal(self: *Pass, layout_idx: layout_mod.Idx, new_locals: *std.ArrayList(LocalId)) ResourceError!LocalId {
        const local = try self.store.addLocal(.{ .layout_idx = layout_idx });
        try new_locals.append(self.store.allocator, local);
        return local;
    }

    /// Observe an incoming ownership unit after its consuming definition. Both
    /// measurements describe this list, including its current slice encoding.
    fn seedMetadata(self: *Pass, list: LocalId, limit: LocalId, owned: ?LocalId, next: CFStmtId, new_locals: *std.ArrayList(LocalId)) ResourceError!CFStmtId {
        var continuation = next;
        if (owned) |flag| {
            try self.noteOwnedDef(flag, .measured);
            continuation = try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = flag,
                .op = .list_owned_unique,
                .rc_effect = LowLevelOp.list_owned_unique.rcEffect(),
                .args = try self.store.addLocalSpan(&.{list}),
                .next = continuation,
            } });
        }
        return self.seedLimit(list, limit, continuation, new_locals);
    }

    fn apply(
        self: *Pass,
        scan: *Scan,
        chain_defs: *collections.DenseMap(LocalId, u32),
        chain_params: *collections.DenseMap(LocalId, CFStmtId),
        has_sets: bool,
        loop_stmt: CFStmtId,
        list_param: LocalId,
        max_join_id: *u32,
        new_locals: *std.ArrayList(LocalId),
    ) ResourceError!void {
        const allocator = self.store.allocator;

        // One slack parameter per chain join; chains containing element
        // overwrites also carry an owned flag (one when the list uniquely
        // owns a non-slice allocation, so a set may run in place).
        var slack_params = collections.DenseMap(LocalId, LocalId).init(allocator);
        defer slack_params.deinit();
        var owned_params = collections.DenseMap(LocalId, LocalId).init(allocator);
        defer owned_params.deinit();
        {
            var it = chain_params.iterator();
            while (it.next()) |entry| {
                const slack_param = try self.freshLocal(.u64, new_locals);
                try slack_params.put(entry.key_ptr.*, slack_param);
                // Extend the owning join's parameter span.
                const join_ptr = &self.store.getCFStmtPtr(entry.value_ptr.*).join;
                const old_params = self.store.getLocalSpan(join_ptr.params);
                var params = std.ArrayList(LocalId).empty;
                defer params.deinit(allocator);
                for (0..GuardedList.borrowLen(old_params)) |i| try params.append(allocator, GuardedList.at(old_params, i));
                try params.append(allocator, slack_param);
                if (has_sets) {
                    const owned_param = try self.freshLocal(.u64, new_locals);
                    try owned_params.put(entry.key_ptr.*, owned_param);
                    try params.append(allocator, owned_param);
                    try self.owned_defs.put(owned_param, .empty);
                    if (entry.key_ptr.* == list_param) {
                        const version = try self.loopVersion(loop_stmt);
                        try version.owned_params.append(allocator, owned_param);
                    }
                }
                join_ptr.params = try self.store.addLocalSpan(params.items);
            }
        }

        // Slack local per carrier. Chain parameters have theirs up front;
        // tracked sites derive theirs from their input and entry sites measure
        // their result. The scan visits statements in stack order,
        // so resolution runs to a fixpoint over the edges instead of assuming
        // definition order. Every carrier is reachable from a chain parameter
        // through these edges, so the fixpoint resolves them all.
        var slack_of = collections.DenseMap(LocalId, LocalId).init(allocator);
        defer slack_of.deinit();
        var owned_of = collections.DenseMap(LocalId, LocalId).init(allocator);
        defer owned_of.deinit();
        {
            var it = chain_params.keyIterator();
            while (it.next()) |param| {
                try slack_of.put(param.*, slack_params.get(param.*).?);
                if (has_sets) try owned_of.put(param.*, owned_params.get(param.*).?);
            }
        }

        // A carrier defined by several edges gets one shared slack
        // local, defined next to each of its definitions: the materialized
        // form of the slack's control-flow merge.
        var shared_slack = collections.DenseMap(LocalId, LocalId).init(allocator);
        defer shared_slack.deinit();
        var shared_owned = collections.DenseMap(LocalId, LocalId).init(allocator);
        defer shared_owned.deinit();
        {
            var it = chain_defs.iterator();
            while (it.next()) |entry| {
                if (chain_params.contains(entry.key_ptr.*)) continue;
                if (entry.value_ptr.* > 1) {
                    const sx = try self.freshLocal(.u64, new_locals);
                    try shared_slack.put(entry.key_ptr.*, sx);
                    try slack_of.put(entry.key_ptr.*, sx);
                    if (has_sets) {
                        const ox = try self.freshLocal(.u64, new_locals);
                        try shared_owned.put(entry.key_ptr.*, ox);
                        try owned_of.put(entry.key_ptr.*, ox);
                        try self.owned_defs.put(ox, .empty);
                    }
                }
            }
        }

        var rewritten = collections.DenseMap(CFStmtId, void).init(allocator);
        defer rewritten.deinit();
        var resolving = true;
        while (resolving) {
            resolving = false;
            for (scan.edges.items) |edge| {
                if (edge.flow == .outside or edge.kind == .param_write) continue;
                if (slack_of.contains(edge.target) and !shared_slack.contains(edge.target)) continue;
                if (rewritten.contains(edge.stmt)) continue;
                if (edge.flow == .entry) {
                    const limit = shared_slack.get(edge.target) orelse try self.freshLocal(.u64, new_locals);
                    const owned = try self.ownedOutFor(edge.target, has_sets, &shared_owned, new_locals);
                    const stmt = self.store.getCFStmt(edge.stmt);
                    const next = switch (edge.kind) {
                        .alias => stmt.assign_ref.next,
                        .append_call => stmt.assign_call.next,
                        .refresh_op, .range_append, .set_op => stmt.assign_low_level.next,
                        .param_write => unreachable,
                    };
                    const seed = try self.seedMetadata(edge.target, limit, owned, next, new_locals);
                    switch (edge.kind) {
                        // A plain alias can still borrow from an outside holder.
                        // Transfer ownership before observing its refcount, so
                        // ARC preserves any other live uses before the query.
                        .alias => self.store.getCFStmtPtr(edge.stmt).* = .{ .assign_low_level = .{
                            .target = edge.target,
                            .op = .list_map_prepare_reuse,
                            .rc_effect = LowLevelOp.list_map_prepare_reuse.rcEffect(),
                            .args = try self.store.addLocalSpan(&.{edge.source}),
                            .next = seed,
                        } },
                        .append_call => self.store.getCFStmtPtr(edge.stmt).assign_call.next = seed,
                        .refresh_op, .range_append, .set_op => self.store.getCFStmtPtr(edge.stmt).assign_low_level.next = seed,
                        .param_write => unreachable,
                    }
                    try slack_of.put(edge.target, limit);
                    if (owned) |flag| try owned_of.put(edge.target, flag);
                    try rewritten.put(edge.stmt, {});
                    resolving = true;
                    continue;
                }
                switch (edge.kind) {
                    .alias => {
                        const source_slack = slack_of.get(edge.source) orelse continue;
                        const source_owned = if (has_sets) owned_of.get(edge.source) orelse continue else undefined;
                        if (shared_slack.get(edge.target)) |sx| {
                            // Materialize this definition's contribution to
                            // the shared slack right after the alias.
                            try rewritten.put(edge.stmt, {});
                            const alias = self.store.getCFStmt(edge.stmt).assign_ref;
                            var next = alias.next;
                            if (has_sets) {
                                const flag = shared_owned.get(edge.target).?;
                                try self.noteOwnedDef(flag, .{ .local = source_owned });
                                next = try self.store.addCFStmt(.{ .assign_ref = .{
                                    .target = flag,
                                    .op = .{ .local = source_owned },
                                    .next = next,
                                } });
                            }
                            const copy = try self.store.addCFStmt(.{ .assign_ref = .{
                                .target = sx,
                                .op = .{ .local = source_slack },
                                .next = next,
                            } });
                            self.store.getCFStmtPtr(edge.stmt).assign_ref.next = copy;
                        } else {
                            try slack_of.put(edge.target, source_slack);
                            if (has_sets) try owned_of.put(edge.target, source_owned);
                        }
                        resolving = true;
                    },
                    .append_call => {
                        const slack_in = slack_of.get(edge.source) orelse continue;
                        try rewritten.put(edge.stmt, {});
                        const slack_out = shared_slack.get(edge.target) orelse try self.freshLocal(.u64, new_locals);
                        const owned_out = try self.ownedOutFor(edge.target, has_sets, &shared_owned, new_locals);
                        try self.rewriteAppendSite(edge, slack_in, slack_out, owned_out, max_join_id, new_locals);
                        try slack_of.put(edge.target, slack_out);
                        if (owned_out) |flag| try owned_of.put(edge.target, flag);
                        resolving = true;
                    },
                    .range_append => {
                        const slack_in = slack_of.get(edge.source) orelse continue;
                        try rewritten.put(edge.stmt, {});
                        const slack_out = shared_slack.get(edge.target) orelse try self.freshLocal(.u64, new_locals);
                        const owned_out = try self.ownedOutFor(edge.target, has_sets, &shared_owned, new_locals);
                        try self.rewriteRangeAppendSite(edge, slack_in, slack_out, owned_out, max_join_id, new_locals);
                        try slack_of.put(edge.target, slack_out);
                        if (owned_out) |flag| try owned_of.put(edge.target, flag);
                        resolving = true;
                    },
                    .set_op => {
                        const slack_in = slack_of.get(edge.source) orelse continue;
                        const owned_in = owned_of.get(edge.source) orelse continue;
                        try rewritten.put(edge.stmt, {});
                        const slack_out = shared_slack.get(edge.target) orelse try self.freshLocal(.u64, new_locals);
                        const owned_out = shared_owned.get(edge.target) orelse try self.freshLocal(.u64, new_locals);
                        try self.rewriteSetSite(edge, owned_in, owned_out, slack_in, slack_out, max_join_id);
                        try slack_of.put(edge.target, slack_out);
                        try owned_of.put(edge.target, owned_out);
                        resolving = true;
                    },
                    .refresh_op => {
                        try rewritten.put(edge.stmt, {});
                        const assign = self.store.getCFStmt(edge.stmt).assign_low_level;
                        const slack_out = shared_slack.get(edge.target) orelse try self.freshLocal(.u64, new_locals);
                        var next = assign.next;
                        if (try self.ownedOutFor(edge.target, has_sets, &shared_owned, new_locals)) |flag| {
                            // Every kept list operation returns a uniquely
                            // owned non-slice result.
                            try self.noteOwnedDef(flag, .one);
                            next = try self.store.addCFStmt(.{ .assign_literal = .{
                                .target = flag,
                                .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
                                .next = next,
                            } });
                            try owned_of.put(edge.target, flag);
                        }
                        const recompute = try self.seedLimit(edge.target, slack_out, next, new_locals);
                        self.store.getCFStmtPtr(edge.stmt).assign_low_level.next = recompute;
                        try slack_of.put(edge.target, slack_out);
                        resolving = true;
                    },
                    .param_write => {},
                }
            }
        }

        // Metadata availability in the map is not proof that every incoming
        // definition initialized it. Check the emitted definitions themselves.
        if (std.debug.runtime_safety) {
            for (scan.edges.items) |edge| {
                if (edge.flow == .outside or edge.kind == .param_write) continue;
                std.debug.assert(slack_of.contains(edge.target));
                if (shared_slack.contains(edge.target) or edge.kind != .alias) {
                    std.debug.assert(rewritten.contains(edge.stmt));
                }
                if (has_sets) std.debug.assert(owned_of.contains(edge.target));
            }
        }

        // Wire every write of a chain parameter: carrier values hand over
        // their slack local; the loop entry computes a fresh one from the
        // incoming list.
        for (scan.edges.items) |edge| {
            if (edge.kind != .param_write) continue;
            if (!chain_params.contains(edge.target)) continue;
            const slack_param = slack_params.get(edge.target).?;
            var original = self.store.getCFStmt(edge.stmt).set_local;
            if (edge.flow == .carried) {
                // Resolved by the fixpoint: every carrier's slack derives from
                // a chain parameter.
                const slack = slack_of.get(edge.source).?;
                var forward = try self.store.addCFStmt(.{ .set_local = original });
                if (has_sets) {
                    const owned_param = owned_params.get(edge.target).?;
                    const owned = owned_of.get(edge.source).?;
                    try self.noteOwnedDef(owned_param, .{ .local = owned });
                    if (edge.target == list_param) try self.noteLoopEdge(loop_stmt, original.next, owned);
                    forward = try self.store.addCFStmt(.{ .set_local = .{
                        .target = owned_param,
                        .value = owned,
                        .mode = .initialize_join_param,
                        .next = forward,
                    } });
                }
                self.store.getCFStmtPtr(edge.stmt).* = .{ .set_local = .{
                    .target = slack_param,
                    .value = slack,
                    .mode = .initialize_join_param,
                    .next = forward,
                } };
            } else {
                std.debug.assert(edge.flow == .entry);
                // Entry edge: acquire the incoming ownership unit before
                // measuring it, just as for an outside alias definition.
                const incoming = try self.freshLocal(self.store.getLocal(edge.source).layout_idx, new_locals);
                original.value = incoming;
                const measured = try self.freshLocal(.u64, new_locals);
                var forward = try self.store.addCFStmt(.{ .set_local = original });
                var owned: ?LocalId = null;
                if (has_sets) {
                    const measured_owned = try self.freshLocal(.u64, new_locals);
                    owned = measured_owned;
                    const owned_param = owned_params.get(edge.target).?;
                    try self.noteOwnedDef(owned_param, .{ .local = measured_owned });
                    if (edge.target == list_param) try self.noteLoopEdge(loop_stmt, original.next, measured_owned);
                    forward = try self.store.addCFStmt(.{ .set_local = .{
                        .target = owned_param,
                        .value = measured_owned,
                        .mode = .initialize_join_param,
                        .next = forward,
                    } });
                }
                const write_slack = try self.store.addCFStmt(.{ .set_local = .{
                    .target = slack_param,
                    .value = measured,
                    .mode = .initialize_join_param,
                    .next = forward,
                } });
                const seed = try self.seedMetadata(incoming, measured, owned, write_slack, new_locals);
                self.store.getCFStmtPtr(edge.stmt).* = .{ .assign_low_level = .{
                    .target = incoming,
                    .op = .list_map_prepare_reuse,
                    .rc_effect = LowLevelOp.list_map_prepare_reuse.rcEffect(),
                    .args = try self.store.addLocalSpan(&.{edge.source}),
                    .next = seed,
                } };
            }
        }
    }

    /// The owned-flag local a chain-op definition of `target` must write, or
    /// null when the chain threads no owned flag.
    fn ownedOutFor(
        self: *Pass,
        target: LocalId,
        has_sets: bool,
        shared_owned: *const collections.DenseMap(LocalId, LocalId),
        new_locals: *std.ArrayList(LocalId),
    ) ResourceError!?LocalId {
        if (!has_sets) return null;
        if (shared_owned.get(target)) |flag| return flag;
        return try self.freshLocal(.u64, new_locals);
    }

    /// Rewrite `target = list_set(list, index, elem); next` into an
    /// owned-guarded pair: the hot side stores in place with no ownership
    /// check, the cold side keeps the checked call (which uniquifies).
    /// Either way the result is uniquely owned and the capacity unchanged,
    /// so the flag comes out one and the slack is carried through.
    fn rewriteSetSite(
        self: *Pass,
        edge: Edge,
        owned_in: LocalId,
        owned_out: LocalId,
        slack_in: LocalId,
        slack_out: LocalId,
        max_join_id: *u32,
    ) ResourceError!void {
        const call = self.store.getCFStmt(edge.stmt).assign_low_level;

        const join_id: LIR.JoinPointId = @enumFromInt(max_join_id.*);
        max_join_id.* += 1;

        // Merged continuation: both sides leave a uniquely owned list with
        // the same spare capacity.
        const slack_copy = try self.store.addCFStmt(.{ .assign_ref = .{
            .target = slack_out,
            .op = .{ .local = slack_in },
            .next = call.next,
        } });
        try self.noteOwnedDef(owned_out, .one);
        const owned_lit = try self.store.addCFStmt(.{ .assign_literal = .{
            .target = owned_out,
            .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
            .next = slack_copy,
        } });

        const hot_jump = try self.store.addCFStmt(.{ .jump = .{ .target = join_id } });
        const hot_set = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = call.target,
            .op = .list_set_in_place_unsafe,
            .rc_effect = LowLevelOp.list_set_in_place_unsafe.rcEffect(),
            .args = call.args,
            .next = hot_jump,
        } });

        const cold_jump = try self.store.addCFStmt(.{ .jump = .{ .target = join_id } });
        const cold_set = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = call.target,
            .op = call.op,
            .rc_effect = call.rc_effect,
            .args = call.args,
            .next = cold_jump,
        } });

        const branches = try self.store.addCFSwitchBranches(&.{.{ .value = 1, .body = hot_set }});
        const dispatch = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = owned_in,
            .branches = branches,
            .default_branch = cold_set,
            .default_is_cold = true,
            .continuation = null,
        } });
        try self.set_dispatches.put(dispatch, {});

        self.store.getCFStmtPtr(edge.stmt).* = .{ .join = .{
            .id = join_id,
            .params = try self.store.addLocalSpan(&.{}),
            .body = owned_lit,
            .remainder = dispatch,
        } };
    }

    /// Rewrite `target = append(list, elem); next` into the slack-guarded
    /// grow/fast diamond, returning the slack local valid for `target`.
    fn rewriteAppendSite(
        self: *Pass,
        edge: Edge,
        slack_in: LocalId,
        slack_out: LocalId,
        owned_out: ?LocalId,
        max_join_id: *u32,
        new_locals: *std.ArrayList(LocalId),
    ) ResourceError!void {
        const call = self.store.getCFStmt(edge.stmt).assign_call;
        const args = self.store.getLocalSpan(call.args);
        const list_arg = GuardedList.at(args, 0);
        const elem_arg = GuardedList.at(args, 1);
        const list_layout = self.store.getLocal(list_arg).layout_idx;

        const merged_list = try self.freshLocal(list_layout, new_locals);
        const merged_slack = try self.freshLocal(.u64, new_locals);
        const cur_len = try self.freshLocal(.u64, new_locals);
        const is_full = try self.freshLocal(.bool, new_locals);
        const grown = try self.freshLocal(list_layout, new_locals);
        const grown_slack = try self.freshLocal(.u64, new_locals);
        const grow_spare = try self.freshLocal(.u64, new_locals);

        const join_id: LIR.JoinPointId = @enumFromInt(max_join_id.*);
        max_join_id.* += 1;

        // Join body: the unchecked append bumps the length, which is the
        // fill cursor, so the limit passes through untouched.
        const forward_limit = try self.store.addCFStmt(.{ .assign_ref = .{
            .target = slack_out,
            .op = .{ .local = merged_slack },
            .next = call.next,
        } });
        const unsafe_append = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = call.target,
            .op = .list_append_unsafe,
            .rc_effect = LowLevelOp.list_append_unsafe.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ merged_list, elem_arg }),
            .next = forward_limit,
        } });

        // Fast path: hand the list and its remaining slack to the join.
        const fast_jump = try self.store.addCFStmt(.{ .jump = .{ .target = join_id } });
        const fast_set_slack = try self.store.addCFStmt(.{ .set_local = .{
            .target = merged_slack,
            .value = slack_in,
            .mode = .initialize_join_param,
            .next = fast_jump,
        } });
        const fast_set_list = try self.store.addCFStmt(.{ .set_local = .{
            .target = merged_list,
            .value = list_arg,
            .mode = .initialize_join_param,
            .next = fast_set_slack,
        } });

        // Grow path: the checked reserve uniquifies and grows, then the slack
        // is measured fresh.
        const grow_jump = try self.store.addCFStmt(.{ .jump = .{ .target = join_id } });
        const grow_set_slack = try self.store.addCFStmt(.{ .set_local = .{
            .target = merged_slack,
            .value = grown_slack,
            .mode = .initialize_join_param,
            .next = grow_jump,
        } });
        const grow_set_list = try self.store.addCFStmt(.{ .set_local = .{
            .target = merged_list,
            .value = grown,
            .mode = .initialize_join_param,
            .next = grow_set_slack,
        } });
        const grow_measure = try self.seedLimit(grown, grown_slack, grow_set_list, new_locals);
        const grow_reserve = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = grown,
            .op = .list_reserve,
            .rc_effect = LowLevelOp.list_reserve.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ list_arg, grow_spare }),
            .next = grow_measure,
        } });
        const grow_spare_lit = try self.store.addCFStmt(.{ .assign_literal = .{
            .target = grow_spare,
            .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
            .next = grow_reserve,
        } });

        // Dispatch: a list filled to its limit takes the cold grow path.
        const branches = try self.store.addCFSwitchBranches(&.{.{ .value = 0, .body = fast_set_list }});
        const dispatch = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = is_full,
            .branches = branches,
            .default_branch = grow_spare_lit,
            .default_is_cold = true,
            .continuation = null,
        } });
        const compare = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = is_full,
            .op = .num_is_eq,
            .rc_effect = LowLevelOp.num_is_eq.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ cur_len, slack_in }),
            .next = dispatch,
        } });
        const measure_len = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = cur_len,
            .op = .list_len,
            .rc_effect = LowLevelOp.list_len.rcEffect(),
            .args = try self.store.addLocalSpan(&.{list_arg}),
            .next = compare,
        } });

        // The call statement becomes the whole construct in place.
        var body = unsafe_append;
        if (owned_out) |flag| {
            // Both sides guarantee a uniquely owned non-slice list.
            try self.noteOwnedDef(flag, .one);
            body = try self.store.addCFStmt(.{ .assign_literal = .{
                .target = flag,
                .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
                .next = body,
            } });
        }
        self.store.getCFStmtPtr(edge.stmt).* = .{ .join = .{
            .id = join_id,
            .params = try self.store.addLocalSpan(&.{ merged_list, merged_slack }),
            .body = body,
            .remainder = measure_len,
        } };
    }

    /// Rewrite `target = list_append_range_within(list, start, count); next`
    /// into a slack-guarded diamond. The hot path runs the unchecked variant
    /// and pays a subtraction; the cold path keeps the checked call (which
    /// uniquifies and grows, reserving the copy scratch itself) and measures
    /// the slack fresh. The guard demands `slack >= count + slop` where slop
    /// covers the unchecked copy's overshooting word stores, phrased as two
    /// compares joined by an AND so a huge `count` cannot wrap the sum: the
    /// subtraction in the second compare may wrap, but only when the first
    /// compare already vetoes the fast path.
    fn rewriteRangeAppendSite(
        self: *Pass,
        edge: Edge,
        slack_in: LocalId,
        slack_out: LocalId,
        owned_out: ?LocalId,
        max_join_id: *u32,
        new_locals: *std.ArrayList(LocalId),
    ) ResourceError!void {
        const call = self.store.getCFStmt(edge.stmt).assign_low_level;
        const args = self.store.getLocalSpan(call.args);
        const list_arg = GuardedList.at(args, 0);
        const count_arg = GuardedList.at(args, 2);

        const elem_size = self.layouts.builtinListAbi(self.store.getLocal(list_arg).layout_idx).elem_size;
        std.debug.assert(elem_size != 0);
        const slop_elements: u64 = (40 + elem_size - 1) / elem_size;

        const slop = try self.freshLocal(.u64, new_locals);
        const cur_len = try self.freshLocal(.u64, new_locals);
        const spare = try self.freshLocal(.u64, new_locals);
        const enough_for_slop = try self.freshLocal(.u8, new_locals);
        const adjusted = try self.freshLocal(.u64, new_locals);
        const enough_for_count = try self.freshLocal(.u8, new_locals);
        const fits = try self.freshLocal(.u8, new_locals);

        const join_id: LIR.JoinPointId = @enumFromInt(max_join_id.*);
        max_join_id.* += 1;

        // Hot path: the unchecked append bumps the length by the count, so
        // the limit passes through untouched.
        const hot_jump = try self.store.addCFStmt(.{ .jump = .{ .target = join_id } });
        const hot_forward = try self.store.addCFStmt(.{ .assign_ref = .{
            .target = slack_out,
            .op = .{ .local = slack_in },
            .next = hot_jump,
        } });
        const hot_append = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = call.target,
            .op = .list_append_range_within_unsafe,
            .rc_effect = LowLevelOp.list_append_range_within_unsafe.rcEffect(),
            .args = call.args,
            .next = hot_forward,
        } });

        // Cold path: the original checked call, then a fresh measurement.
        const cold_jump = try self.store.addCFStmt(.{ .jump = .{ .target = join_id } });
        const cold_measure = try self.seedLimit(call.target, slack_out, cold_jump, new_locals);
        const cold_append = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = call.target,
            .op = call.op,
            .rc_effect = call.rc_effect,
            .args = call.args,
            .next = cold_measure,
        } });

        // Dispatch: only `fits == 1` takes the unchecked path.
        const branches = try self.store.addCFSwitchBranches(&.{.{ .value = 1, .body = hot_append }});
        const dispatch = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = fits,
            .branches = branches,
            .default_branch = cold_append,
            .default_is_cold = true,
            .continuation = null,
        } });
        const combine = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = fits,
            .op = .num_bitwise_and,
            .rc_effect = LowLevelOp.num_bitwise_and.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ enough_for_slop, enough_for_count }),
            .next = dispatch,
        } });
        const compare_count = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = enough_for_count,
            .op = .num_is_gte,
            .rc_effect = LowLevelOp.num_is_gte.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ adjusted, count_arg }),
            .next = combine,
        } });
        const subtract_slop = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = adjusted,
            .op = .num_int_sub_wrap,
            .rc_effect = LowLevelOp.num_int_sub_wrap.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ spare, slop }),
            .next = compare_count,
        } });
        const compare_slop = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = enough_for_slop,
            .op = .num_is_gte,
            .rc_effect = LowLevelOp.num_is_gte.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ spare, slop }),
            .next = subtract_slop,
        } });
        // The chain invariant keeps the length at most the limit, so this
        // difference cannot wrap.
        const measure_spare = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = spare,
            .op = .num_int_sub_wrap,
            .rc_effect = LowLevelOp.num_int_sub_wrap.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ slack_in, cur_len }),
            .next = compare_slop,
        } });
        const measure_len = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = cur_len,
            .op = .list_len,
            .rc_effect = LowLevelOp.list_len.rcEffect(),
            .args = try self.store.addLocalSpan(&.{list_arg}),
            .next = measure_spare,
        } });
        const slop_lit = try self.store.addCFStmt(.{ .assign_literal = .{
            .target = slop,
            .value = .{ .i64_literal = .{ .value = @intCast(slop_elements), .layout_idx = .u64 } },
            .next = measure_len,
        } });

        // The call statement becomes the whole construct in place.
        var body = call.next;
        if (owned_out) |flag| {
            // Both sides guarantee a uniquely owned non-slice list.
            try self.noteOwnedDef(flag, .one);
            body = try self.store.addCFStmt(.{ .assign_literal = .{
                .target = flag,
                .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
                .next = body,
            } });
        }
        self.store.getCFStmtPtr(edge.stmt).* = .{ .join = .{
            .id = join_id,
            .params = try self.store.addLocalSpan(&.{}),
            .body = body,
            .remainder = slop_lit,
        } };
    }

    // Loop versioning

    /// The jump that a run of parameter writes starting at `start` ends in,
    /// or null when control branches before reaching one.
    fn jumpAfter(self: *Pass, start: CFStmtId) ?CFStmtId {
        var current = start;
        while (true) {
            switch (self.store.getCFStmt(current)) {
                .jump => return current,
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict => |s| current = s.next,
                .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .boxy_tag_match, .join, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => return null,
            }
        }
    }

    /// Record the owned value a loop's own parameter receives on the edge
    /// whose writes begin at `start`, keyed by the jump that ends the edge.
    fn noteLoopEdge(self: *Pass, loop_stmt: CFStmtId, start: CFStmtId, owned: LocalId) ResourceError!void {
        const loop_id = self.store.getCFStmt(loop_stmt).join.id;
        const version = try self.loopVersion(loop_stmt);
        const jump = self.jumpAfter(start) orelse {
            version.unresolved = true;
            return;
        };
        if (self.store.getCFStmt(jump).jump.target != loop_id) {
            version.unresolved = true;
            return;
        }
        const entry = try version.edge_values.getOrPut(jump);
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        try entry.value_ptr.append(self.store.allocator, owned);
    }

    /// Version every promoted loop whose edges were all traced, innermost
    /// first so an outer loop's copy carries the already-versioned inner one.
    fn versionLoops(self: *Pass, proc_body: CFStmtId, max_join_id: *u32, new_locals: *std.ArrayList(LocalId)) ResourceError!void {
        const allocator = self.store.allocator;
        const Candidate = struct { stmt: CFStmtId, size: u32 };
        var candidates = std.ArrayList(Candidate).empty;
        defer candidates.deinit(allocator);
        var it = self.loop_versions.iterator();
        while (it.next()) |entry| {
            const version = entry.value_ptr;
            if (version.unresolved or version.owned_params.items.len == 0) continue;
            const join = self.store.getCFStmt(entry.key_ptr.*).join;
            // Retained and conditionally initialized environments are
            // released per join under conditions ARC tracks; the copy would
            // need that environment duplicated, which is not modeled here.
            if (self.store.getLocalSpan(join.retained).len != 0) continue;
            if (self.store.getLocalSpan(join.maybe_uninitialized_params).len != 0) continue;
            var stmts = try body_clone.ReachableStmts.init(self.store, join.body);
            defer stmts.deinit();
            var size: u32 = 0;
            while (try stmts.next()) |_| size += 1;
            try candidates.append(allocator, .{ .stmt = entry.key_ptr.*, .size = size });
        }
        // A loop nested in another's body has the smaller body.
        std.mem.sort(Candidate, candidates.items, {}, struct {
            fn lessThan(_: void, a: Candidate, b: Candidate) bool {
                return a.size < b.size;
            }
        }.lessThan);
        for (candidates.items) |candidate| {
            try self.versionLoop(proc_body, candidate.stmt, max_join_id, new_locals);
        }
    }

    fn containsLocal(locals: []const LocalId, local: LocalId) bool {
        for (locals) |candidate| {
            if (candidate == local) return true;
        }
        return false;
    }

    /// Split one promoted loop into a head that dispatches on its owned
    /// flags and a nested unique-only copy of its body.
    fn versionLoop(self: *Pass, proc_body: CFStmtId, loop_stmt: CFStmtId, max_join_id: *u32, new_locals: *std.ArrayList(LocalId)) ResourceError!void {
        const allocator = self.store.allocator;
        const join = self.store.getCFStmt(loop_stmt).join;
        const version = self.loop_versions.getPtr(loop_stmt).?;
        const flags = version.owned_params.items;

        // Flags that may be zero inside the copy: measurements of lists the
        // chain did not carry, and every flag fed by one, to a fixpoint. The
        // loop's own flags are one throughout the copy, which is entered only
        // when they are and re-entered only by edges that keep them so.
        var non_static = collections.DenseMap(LocalId, void).init(allocator);
        defer non_static.deinit();
        {
            var changed = true;
            while (changed) {
                changed = false;
                var defs = self.owned_defs.iterator();
                while (defs.next()) |entry| {
                    const flag = entry.key_ptr.*;
                    if (non_static.contains(flag) or containsLocal(flags, flag)) continue;
                    for (entry.value_ptr.items) |def| {
                        const unknown = switch (def) {
                            .one => false,
                            .measured => true,
                            .local => |source| non_static.contains(source),
                        };
                        if (unknown) {
                            try non_static.put(flag, {});
                            changed = true;
                            break;
                        }
                    }
                }
            }
        }

        var retarget = collections.DenseMap(CFStmtId, void).init(allocator);
        defer retarget.deinit();
        {
            var edges = version.edge_values.iterator();
            while (edges.next()) |entry| {
                var keeps_flags = entry.value_ptr.items.len == flags.len;
                for (entry.value_ptr.items) |value| {
                    if (non_static.contains(value)) keeps_flags = false;
                }
                if (keeps_flags) try retarget.put(entry.key_ptr.*, {});
            }
        }
        var fold = collections.DenseMap(CFStmtId, void).init(allocator);
        defer fold.deinit();
        {
            var sites = self.set_dispatches.keyIterator();
            while (sites.next()) |site| {
                const cond = self.store.getCFStmt(site.*).switch_stmt.cond;
                if (!non_static.contains(cond)) try fold.put(site.*, {});
            }
        }

        // The copy gets fresh locals for values that live entirely inside the
        // body, so its stores and loads never share a slot with the cold
        // arm's calls; everything else, the parameters included, is shared.
        var body_reads = try body_clone.countReachableReads(self.store, join.body);
        defer body_reads.deinit();
        var body_defs = try body_clone.countReachableDefs(self.store, join.body);
        defer body_defs.deinit();
        var proc_reads = try body_clone.countReachableReads(self.store, proc_body);
        defer proc_reads.deinit();
        var proc_defs = try body_clone.countReachableDefs(self.store, proc_body);
        defer proc_defs.deinit();

        const unique_id: LIR.JoinPointId = @enumFromInt(max_join_id.*);
        max_join_id.* += 1;
        const rewriter = VersionRewriter{
            .loop_id = join.id,
            .unique_id = unique_id,
            .retarget = &retarget,
            .fold = &fold,
            .join_map = collections.DenseMap(LIR.JoinPointId, LIR.JoinPointId).init(allocator),
            .max_join_id = max_join_id,
        };
        var cloner = try body_clone.BodyCloner(VersionRewriter).init(self.store, rewriter);
        defer cloner.deinit();
        defer cloner.rewriter.join_map.deinit();
        for (cloner.local_map, 0..) |*slot, index| {
            const defs_inside = body_defs.counts[index];
            const renamable = defs_inside > 0 and
                defs_inside == proc_defs.counts[index] and
                body_reads.counts[index] == proc_reads.counts[index];
            if (!renamable) slot.* = @enumFromInt(index);
        }
        const body_copy = try cloner.cloneStmt(join.body);
        try new_locals.appendSlice(allocator, cloner.new_locals.items);

        const back = try self.store.addCFStmt(.{ .jump = .{ .target = unique_id } });
        const unique_loop = try self.store.addCFStmt(.{ .join = .{
            .id = unique_id,
            .params = join.params,
            .body = body_copy,
            .remainder = back,
        } });

        // Dispatch on the conjunction of the flags; a shared entry takes the
        // cold arm, which is the body as promoted.
        var combined = std.ArrayList(LocalId).empty;
        defer combined.deinit(allocator);
        for (1..flags.len) |_| try combined.append(allocator, try self.freshLocal(.u64, new_locals));
        const cond = if (combined.items.len == 0) flags[0] else combined.items[combined.items.len - 1];
        const branches = try self.store.addCFSwitchBranches(&.{.{ .value = 1, .body = unique_loop }});
        var head = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = branches,
            .default_branch = join.body,
            .default_is_cold = true,
            .continuation = null,
        } });
        var index = combined.items.len;
        while (index > 0) {
            index -= 1;
            const left = if (index == 0) flags[0] else combined.items[index - 1];
            head = try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = combined.items[index],
                .op = .num_bitwise_and,
                .rc_effect = LowLevelOp.num_bitwise_and.rcEffect(),
                .args = try self.store.addLocalSpan(&.{ left, flags[index + 1] }),
                .next = head,
            } });
        }
        self.store.getCFStmtPtr(loop_stmt).join.body = head;
    }
};

/// Clones a promoted loop body into its unique-only copy: nested joins get
/// fresh ids, back edges that keep the flags at one return to the copy, and
/// set-site dispatches whose flag is one collapse to the unchecked store.
const VersionRewriter = struct {
    loop_id: LIR.JoinPointId,
    unique_id: LIR.JoinPointId,
    retarget: *const collections.DenseMap(CFStmtId, void),
    fold: *const collections.DenseMap(CFStmtId, void),
    join_map: collections.DenseMap(LIR.JoinPointId, LIR.JoinPointId),
    max_join_id: *u32,

    pub fn cloneRet(_: *VersionRewriter, cloner: anytype, value: LocalId) ResourceError!CFStmtId {
        return try cloner.store.addCFStmt(.{ .ret = .{ .value = try cloner.mapLocal(value) } });
    }

    pub fn interceptStmt(self: *VersionRewriter, cloner: anytype, old_id: CFStmtId, stmt: LIR.CFStmt) ResourceError!?CFStmtId {
        switch (stmt) {
            .join => |s| {
                const fresh: LIR.JoinPointId = @enumFromInt(self.max_join_id.*);
                self.max_join_id.* += 1;
                try self.join_map.put(s.id, fresh);
                const body = try cloner.cloneStmt(s.body);
                const remainder = try cloner.cloneStmt(s.remainder);
                return try cloner.store.addCFStmt(.{ .join = .{
                    .id = fresh,
                    .params = try cloner.mapLocalSpan(s.params),
                    .retained = try cloner.mapLocalSpan(s.retained),
                    .maybe_uninitialized_params = try cloner.mapLocalSpan(s.maybe_uninitialized_params),
                    .maybe_uninitialized_conditions = try cloner.mapLocalSpan(s.maybe_uninitialized_conditions),
                    .maybe_uninitialized_condition_masks = s.maybe_uninitialized_condition_masks,
                    .body = body,
                    .remainder = remainder,
                } });
            },
            .jump => |s| {
                const target = if (s.target == self.loop_id)
                    (if (self.retarget.contains(old_id)) self.unique_id else self.loop_id)
                else
                    (self.join_map.get(s.target) orelse s.target);
                return try cloner.store.addCFStmt(.{ .jump = .{ .target = target } });
            },
            .switch_stmt => |s| {
                if (!self.fold.contains(old_id)) return null;
                const branches = cloner.store.getCFSwitchBranches(s.branches);
                return try cloner.cloneStmt(GuardedList.at(branches, 0).body);
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
            => return null,
        }
    }
};

// Tests

const testing = std.testing;

const PromoteTest = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    list: layout_mod.Idx,
    next_join_point: u32 = 0,

    fn init(allocator: Allocator) Allocator.Error!PromoteTest {
        var layouts = try layout_mod.Store.init(allocator, .u64);
        errdefer layouts.deinit();
        const list = try layouts.insertList(.u8);
        return .{
            .store = LirStore.init(allocator),
            .layouts = layouts,
            .list = list,
        };
    }

    fn deinit(self: *PromoteTest) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    fn freshJoinPointId(self: *PromoteTest) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
    }

    /// A checked-append helper in the fully lowered direct form:
    /// `append(list, elem) = list_append_unsafe(list_reserve(list, 1), elem)`.
    fn addAppendHelper(self: *PromoteTest) Allocator.Error!LIR.LirProcSpecId {
        const store = &self.store;
        const list_arg = try store.addLocal(.{ .layout_idx = self.list });
        const elem_arg = try store.addLocal(.{ .layout_idx = .u8 });
        const spare = try store.addLocal(.{ .layout_idx = .u64 });
        const reserved = try store.addLocal(.{ .layout_idx = self.list });
        const appended = try store.addLocal(.{ .layout_idx = self.list });

        const ret = try store.addCFStmt(.{ .ret = .{ .value = appended } });
        const unsafe_append = try store.addCFStmt(.{ .assign_low_level = .{
            .target = appended,
            .op = .list_append_unsafe,
            .rc_effect = LowLevelOp.list_append_unsafe.rcEffect(),
            .args = try store.addLocalSpan(&.{ reserved, elem_arg }),
            .next = ret,
        } });
        const reserve = try store.addCFStmt(.{ .assign_low_level = .{
            .target = reserved,
            .op = .list_reserve,
            .rc_effect = LowLevelOp.list_reserve.rcEffect(),
            .args = try store.addLocalSpan(&.{ list_arg, spare }),
            .next = unsafe_append,
        } });
        const spare_lit = try store.addCFStmt(.{ .assign_literal = .{
            .target = spare,
            .value = .{ .i128_literal = .{ .value = 1, .layout_idx = .u64 } },
            .next = reserve,
        } });
        return try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{ list_arg, elem_arg }),
            .body = spare_lit,
            .ret_layout = self.list,
        });
    }
};

test "promote threads slack through an append-only loop" {
    var f = try PromoteTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;
    const append_proc = try f.addAppendHelper();

    // loop J(out):
    //   body: a = ref out; appended = call append(a, elem)
    //         b = ref appended; set out := b; jump J
    //   remainder: set out := init; jump J
    const out = try store.addLocal(.{ .layout_idx = f.list });
    const init_list = try store.addLocal(.{ .layout_idx = f.list });
    const elem = try store.addLocal(.{ .layout_idx = .u8 });
    const a = try store.addLocal(.{ .layout_idx = f.list });
    const appended = try store.addLocal(.{ .layout_idx = f.list });
    const b = try store.addLocal(.{ .layout_idx = f.list });
    const join_id = f.freshJoinPointId();

    const back_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const back_set = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = b,
        .mode = .initialize_join_param,
        .next = back_jump,
    } });
    const alias_b = try store.addCFStmt(.{ .assign_ref = .{
        .target = b,
        .op = .{ .local = appended },
        .next = back_set,
    } });
    const append_call = try store.addCFStmt(.{ .assign_call = .{
        .target = appended,
        .proc = append_proc,
        .args = try store.addLocalSpan(&.{ a, elem }),
        .next = alias_b,
    } });
    const alias_a = try store.addCFStmt(.{ .assign_ref = .{
        .target = a,
        .op = .{ .local = out },
        .next = append_call,
    } });

    const entry_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const entry_set = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = init_list,
        .mode = .initialize_join_param,
        .next = entry_jump,
    } });
    const loop = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{out}),
        .body = alias_a,
        .remainder = entry_set,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = loop,
        .ret_layout = f.list,
    });

    try run(store, &f.layouts);

    // The loop gained a slack parameter.
    const new_loop = store.getCFStmt(loop).join;
    const params = store.getLocalSpan(new_loop.params);
    try testing.expectEqual(@as(usize, 2), params.len);
    try testing.expectEqual(out, GuardedList.at(params, 0));
    const slack_param = GuardedList.at(params, 1);

    // The entry edge measures the incoming list and seeds the fill limit:
    // its length plus its uniquely owned spare capacity.
    const entry_prepare = store.getCFStmt(entry_set).assign_low_level;
    try testing.expectEqual(LowLevelOp.list_map_prepare_reuse, entry_prepare.op);
    try testing.expectEqual(init_list, GuardedList.at(store.getLocalSpan(entry_prepare.args), 0));
    const entry_measure = store.getCFStmt(entry_prepare.next).assign_low_level;
    try testing.expectEqual(LowLevelOp.list_slack_unique, entry_measure.op);
    const entry_args = store.getLocalSpan(entry_measure.args);
    try testing.expectEqual(entry_prepare.target, GuardedList.at(entry_args, 0));
    const entry_len = store.getCFStmt(entry_measure.next).assign_low_level;
    try testing.expectEqual(LowLevelOp.list_len, entry_len.op);
    const entry_sum = store.getCFStmt(entry_len.next).assign_low_level;
    try testing.expectEqual(LowLevelOp.num_int_add_wrap, entry_sum.op);
    const entry_limit_write = store.getCFStmt(entry_sum.next).set_local;
    try testing.expectEqual(slack_param, entry_limit_write.target);
    try testing.expectEqual(entry_sum.target, entry_limit_write.value);
    const entry_list_write = store.getCFStmt(entry_limit_write.next).set_local;
    try testing.expectEqual(out, entry_list_write.target);
    try testing.expectEqual(entry_prepare.target, entry_list_write.value);
    try testing.expectEqual(entry_jump, entry_list_write.next);

    // The append call became the limit diamond: a join whose body is the
    // unchecked append with the limit passed through, and whose remainder
    // dispatches on the length reaching the limit.
    const site = store.getCFStmt(append_call).join;
    const site_params = store.getLocalSpan(site.params);
    try testing.expectEqual(@as(usize, 2), site_params.len);
    const unsafe_append = store.getCFStmt(site.body).assign_low_level;
    try testing.expectEqual(LowLevelOp.list_append_unsafe, unsafe_append.op);
    try testing.expectEqual(appended, unsafe_append.target);
    const unsafe_args = store.getLocalSpan(unsafe_append.args);
    try testing.expectEqual(GuardedList.at(site_params, 0), GuardedList.at(unsafe_args, 0));
    try testing.expectEqual(elem, GuardedList.at(unsafe_args, 1));
    const forward = store.getCFStmt(unsafe_append.next).assign_ref;
    try testing.expectEqual(GuardedList.at(site_params, 1), forward.op.local);
    try testing.expectEqual(alias_b, forward.next);

    // The back edge hands the unchanged limit to the loop parameter.
    const back_limit_write = store.getCFStmt(back_set).set_local;
    try testing.expectEqual(slack_param, back_limit_write.target);
    try testing.expectEqual(forward.target, back_limit_write.value);
    const back_list_write = store.getCFStmt(back_limit_write.next).set_local;
    try testing.expectEqual(out, back_list_write.target);
    try testing.expectEqual(b, back_list_write.value);
    try testing.expectEqual(back_jump, back_list_write.next);

    // The dispatch compares the list's length against the incoming limit,
    // with the grow path as the cold default.
    var dispatch_stmt = site.remainder;
    var found_switch = false;
    var steps: u32 = 0;
    while (steps < 8) : (steps += 1) {
        switch (store.getCFStmt(dispatch_stmt)) {
            .assign_literal => |lit| dispatch_stmt = lit.next,
            .assign_low_level => |cmp| {
                try testing.expect(cmp.op == .num_is_eq or cmp.op == .list_len);
                dispatch_stmt = cmp.next;
            },
            .switch_stmt => |sw| {
                try testing.expect(sw.default_is_cold);
                found_switch = true;
                // The cold default grows through the checked reserve and
                // re-measures.
                var grow = sw.default_branch;
                var grow_steps: u32 = 0;
                var saw_reserve = false;
                var saw_measure = false;
                while (grow_steps < 12) : (grow_steps += 1) {
                    switch (store.getCFStmt(grow)) {
                        .assign_literal => |lit| grow = lit.next,
                        .assign_low_level => |op| {
                            if (op.op == .list_reserve) saw_reserve = true;
                            if (op.op == .list_slack_unique) saw_measure = true;
                            grow = op.next;
                        },
                        .set_local => |set| grow = set.next,
                        .jump => break,
                        .init_uninitialized,
                        .assign_ref,
                        .assign_call,
                        .assign_call_erased,
                        .assign_packed_erased_fn,
                        .assign_list,
                        .assign_struct,
                        .assign_tag,
                        .store_struct,
                        .store_tag,
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
                        => return error.TestUnexpectedResult,
                    }
                }
                try testing.expect(saw_reserve);
                try testing.expect(saw_measure);
                break;
            },
            .init_uninitialized,
            .assign_ref,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
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
            => return error.TestUnexpectedResult,
        }
    }
    try testing.expect(found_switch);
}

test "promote leaves a tainted chain alone" {
    var f = try PromoteTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;
    const append_proc = try f.addAppendHelper();

    // Same loop, but the appended value also escapes into a struct build
    // before feeding the back edge, so the chain is tainted mid-spine.
    const pair = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = f.list },
    });
    const out = try store.addLocal(.{ .layout_idx = f.list });
    const init_list = try store.addLocal(.{ .layout_idx = f.list });
    const elem = try store.addLocal(.{ .layout_idx = .u8 });
    const a = try store.addLocal(.{ .layout_idx = f.list });
    const appended = try store.addLocal(.{ .layout_idx = f.list });
    const boxed = try store.addLocal(.{ .layout_idx = pair });
    const join_id = f.freshJoinPointId();

    const back_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const back_set = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = appended,
        .mode = .initialize_join_param,
        .next = back_jump,
    } });
    const escape = try store.addCFStmt(.{ .assign_struct = .{
        .target = boxed,
        .fields = try store.addLocalSpan(&.{appended}),
        .next = back_set,
    } });
    const append_call = try store.addCFStmt(.{ .assign_call = .{
        .target = appended,
        .proc = append_proc,
        .args = try store.addLocalSpan(&.{ a, elem }),
        .next = escape,
    } });
    const alias_a = try store.addCFStmt(.{ .assign_ref = .{
        .target = a,
        .op = .{ .local = out },
        .next = append_call,
    } });

    const entry_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const entry_set = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = init_list,
        .mode = .initialize_join_param,
        .next = entry_jump,
    } });
    const loop = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{out}),
        .body = alias_a,
        .remainder = entry_set,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = loop,
        .ret_layout = f.list,
    });

    try run(store, &f.layouts);

    // Nothing changed: the call is still a call and the loop still has one
    // parameter.
    try testing.expect(store.getCFStmt(append_call) == .assign_call);
    const unchanged = store.getCFStmt(loop).join;
    try testing.expectEqual(@as(usize, 1), store.getLocalSpan(unchanged.params).len);
}

test "promote ignores a join without a back edge" {
    var f = try PromoteTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;
    const append_proc = try f.addAppendHelper();

    // join J(out): body appends once and returns; remainder enters. No jump
    // back to J from its body, so J is not a loop.
    const out = try store.addLocal(.{ .layout_idx = f.list });
    const init_list = try store.addLocal(.{ .layout_idx = f.list });
    const elem = try store.addLocal(.{ .layout_idx = .u8 });
    const a = try store.addLocal(.{ .layout_idx = f.list });
    const appended = try store.addLocal(.{ .layout_idx = f.list });
    const join_id = f.freshJoinPointId();

    const ret = try store.addCFStmt(.{ .ret = .{ .value = appended } });
    const append_call = try store.addCFStmt(.{ .assign_call = .{
        .target = appended,
        .proc = append_proc,
        .args = try store.addLocalSpan(&.{ a, elem }),
        .next = ret,
    } });
    const alias_a = try store.addCFStmt(.{ .assign_ref = .{
        .target = a,
        .op = .{ .local = out },
        .next = append_call,
    } });
    const entry_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const entry_set = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = init_list,
        .mode = .initialize_join_param,
        .next = entry_jump,
    } });
    const loop = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{out}),
        .body = alias_a,
        .remainder = entry_set,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = loop,
        .ret_layout = f.list,
    });

    try run(store, &f.layouts);

    try testing.expect(store.getCFStmt(append_call) == .assign_call);
    const unchanged = store.getCFStmt(loop).join;
    try testing.expectEqual(@as(usize, 1), store.getLocalSpan(unchanged.params).len);
}

/// What one loop body contains, for checking a versioned loop's two arms.
const BodyShape = struct {
    switches: u32 = 0,
    checked_sets: u32 = 0,
    unchecked_sets: u32 = 0,
    jumps_to_head: u32 = 0,
    jumps_to_copy: u32 = 0,
};

fn shapeOf(store: *LirStore, root: CFStmtId, head: LIR.JoinPointId, copy: LIR.JoinPointId) Allocator.Error!BodyShape {
    var stmts = try body_clone.ReachableStmts.init(store, root);
    defer stmts.deinit();
    var shape = BodyShape{};
    while (try stmts.next()) |id| {
        switch (store.getCFStmt(id)) {
            .switch_stmt => shape.switches += 1,
            .assign_low_level => |s| {
                if (s.op == .list_set) shape.checked_sets += 1;
                if (s.op == .list_set_in_place_unsafe) shape.unchecked_sets += 1;
            },
            .jump => |s| {
                if (s.target == head) shape.jumps_to_head += 1;
                if (s.target == copy) shape.jumps_to_copy += 1;
            },
            .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
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
            => {},
        }
    }
    return shape;
}

/// A set site `target = list_set(list, 0, 9)` followed by `next`, returning
/// the head statement.
fn addSetSite(f: *PromoteTest, target: LocalId, list: LocalId, next: CFStmtId) Allocator.Error!CFStmtId {
    const store = &f.store;
    const idx = try store.addLocal(.{ .layout_idx = .u64 });
    const elem = try store.addLocal(.{ .layout_idx = .u8 });
    const set_stmt = try store.addCFStmt(.{ .assign_low_level = .{
        .target = target,
        .op = .list_set,
        .rc_effect = LowLevelOp.list_set.rcEffect(),
        .args = try store.addLocalSpan(&.{ list, idx, elem }),
        .next = next,
    } });
    const elem_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = elem,
        .value = .{ .i64_literal = .{ .value = 9, .layout_idx = .u8 } },
        .next = set_stmt,
    } });
    return try store.addCFStmt(.{ .assign_literal = .{
        .target = idx,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = elem_lit,
    } });
}

test "promote versions a set loop into a dispatching head and a unique-only copy" {
    var f = try PromoteTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    // loop J(out):
    //   body: a = ref out; s = list_set(a, 0, 9); b = ref s
    //         set out := b; jump J
    //   remainder: set out := init; jump J
    const out = try store.addLocal(.{ .layout_idx = f.list });
    const init_list = try store.addLocal(.{ .layout_idx = f.list });
    const a = try store.addLocal(.{ .layout_idx = f.list });
    const s = try store.addLocal(.{ .layout_idx = f.list });
    const b = try store.addLocal(.{ .layout_idx = f.list });
    const join_id = f.freshJoinPointId();

    const back_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const back_set = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = b,
        .mode = .initialize_join_param,
        .next = back_jump,
    } });
    const alias_b = try store.addCFStmt(.{ .assign_ref = .{
        .target = b,
        .op = .{ .local = s },
        .next = back_set,
    } });
    const site = try addSetSite(&f, s, a, alias_b);
    const alias_a = try store.addCFStmt(.{ .assign_ref = .{
        .target = a,
        .op = .{ .local = out },
        .next = site,
    } });

    const entry_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const entry_set = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = init_list,
        .mode = .initialize_join_param,
        .next = entry_jump,
    } });
    const loop = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{out}),
        .body = alias_a,
        .remainder = entry_set,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = loop,
        .ret_layout = f.list,
    });

    try run(store, &f.layouts);

    // The loop carries the list, its limit, and its owned flag.
    const head = store.getCFStmt(loop).join;
    const params = store.getLocalSpan(head.params);
    try testing.expectEqual(@as(usize, 3), params.len);
    const owned_param = GuardedList.at(params, 2);

    // The head dispatches on the flag: the hot arm is a nested join sharing
    // the parameters, entered directly from its remainder; the cold arm is
    // the body as promoted.
    const dispatch = store.getCFStmt(head.body).switch_stmt;
    try testing.expectEqual(owned_param, dispatch.cond);
    try testing.expect(dispatch.default_is_cold);
    try testing.expectEqual(alias_a, dispatch.default_branch);
    const branches = store.getCFSwitchBranches(dispatch.branches);
    try testing.expectEqual(@as(usize, 1), branches.len);
    try testing.expectEqual(@as(u64, 1), GuardedList.at(branches, 0).value);
    const copy = store.getCFStmt(GuardedList.at(branches, 0).body).join;
    try testing.expect(copy.id != head.id);
    try testing.expectEqual(head.params, copy.params);
    try testing.expectEqual(copy.id, store.getCFStmt(copy.remainder).jump.target);

    // The copy stores in place with no dispatch and returns to itself; the
    // cold arm keeps the guarded pair and returns to the head.
    const hot = try shapeOf(store, copy.body, head.id, copy.id);
    try testing.expectEqual(BodyShape{ .unchecked_sets = 1, .jumps_to_copy = 1 }, hot);
    const cold = try shapeOf(store, alias_a, head.id, copy.id);
    try testing.expectEqual(BodyShape{ .switches = 1, .checked_sets = 1, .unchecked_sets = 1, .jumps_to_head = 1 }, cold);
}

test "promote keeps a foreign back edge on the head inside the copy" {
    var f = try PromoteTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    // loop J(out):
    //   body: a = ref out; c = 1
    //         switch c { 1: s = list_set(a, 0, 9); b = ref s; set out := b; jump J
    //                    default: o = ref other; set out := o; jump J }
    //   remainder: set out := init; jump J
    // `other` is a proc argument the chain never carried, so its edge must
    // measure ownership again and re-enter the head.
    const out = try store.addLocal(.{ .layout_idx = f.list });
    const init_list = try store.addLocal(.{ .layout_idx = f.list });
    const other = try store.addLocal(.{ .layout_idx = f.list });
    const a = try store.addLocal(.{ .layout_idx = f.list });
    const c = try store.addLocal(.{ .layout_idx = .u8 });
    const s = try store.addLocal(.{ .layout_idx = f.list });
    const b = try store.addLocal(.{ .layout_idx = f.list });
    const o = try store.addLocal(.{ .layout_idx = f.list });
    const join_id = f.freshJoinPointId();

    const set_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_write = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = b,
        .mode = .initialize_join_param,
        .next = set_jump,
    } });
    const alias_b = try store.addCFStmt(.{ .assign_ref = .{
        .target = b,
        .op = .{ .local = s },
        .next = set_write,
    } });
    const site = try addSetSite(&f, s, a, alias_b);

    const foreign_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const foreign_write = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = o,
        .mode = .initialize_join_param,
        .next = foreign_jump,
    } });
    const alias_o = try store.addCFStmt(.{ .assign_ref = .{
        .target = o,
        .op = .{ .local = other },
        .next = foreign_write,
    } });

    const branch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = c,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = site }}),
        .default_branch = alias_o,
        .continuation = null,
    } });
    const cond_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = c,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u8 } },
        .next = branch,
    } });
    const alias_a = try store.addCFStmt(.{ .assign_ref = .{
        .target = a,
        .op = .{ .local = out },
        .next = cond_lit,
    } });

    const entry_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const entry_set = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = init_list,
        .mode = .initialize_join_param,
        .next = entry_jump,
    } });
    const loop = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{out}),
        .body = alias_a,
        .remainder = entry_set,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{other}),
        .body = loop,
        .ret_layout = f.list,
    });

    try run(store, &f.layouts);

    const head = store.getCFStmt(loop).join;
    const dispatch = store.getCFStmt(head.body).switch_stmt;
    const copy = store.getCFStmt(GuardedList.at(store.getCFSwitchBranches(dispatch.branches), 0).body).join;

    // Inside the copy the set edge returns to the copy while the foreign
    // edge, whose list was measured fresh, re-enters the head. Its own
    // condition switch survives; only the set dispatch folded.
    const hot = try shapeOf(store, copy.body, head.id, copy.id);
    try testing.expectEqual(BodyShape{ .switches = 1, .unchecked_sets = 1, .jumps_to_head = 1, .jumps_to_copy = 1 }, hot);
    const cold = try shapeOf(store, alias_a, head.id, copy.id);
    try testing.expectEqual(BodyShape{ .switches = 2, .checked_sets = 1, .unchecked_sets = 1, .jumps_to_head = 2 }, cold);
}

test "promote initializes merged metadata on every incoming definition" {
    var f = try PromoteTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    // The merge has a tracked list_set result on one edge and an outside list
    // on the other. Both definitions must initialize the same metadata locals;
    // the outside edge must also prevent a direct back edge to the unique copy.
    const out = try store.addLocal(.{ .layout_idx = f.list });
    const initial = try store.addLocal(.{ .layout_idx = f.list });
    const other = try store.addLocal(.{ .layout_idx = f.list });
    const condition = try store.addLocal(.{ .layout_idx = .bool });
    const updated = try store.addLocal(.{ .layout_idx = f.list });
    const merged = try store.addLocal(.{ .layout_idx = f.list });
    const loop_id = f.freshJoinPointId();
    const merge_id = f.freshJoinPointId();
    const back_jump = try store.addCFStmt(.{ .jump = .{ .target = loop_id } });
    const back_write = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = merged,
        .mode = .initialize_join_param,
        .next = back_jump,
    } });
    const tracked_jump = try store.addCFStmt(.{ .jump = .{ .target = merge_id } });
    const tracked = try store.addCFStmt(.{ .assign_ref = .{
        .target = merged,
        .op = .{ .local = updated },
        .next = tracked_jump,
    } });
    const set_site = try addSetSite(&f, updated, out, tracked);
    const foreign_jump = try store.addCFStmt(.{ .jump = .{ .target = merge_id } });
    const foreign = try store.addCFStmt(.{ .assign_ref = .{
        .target = merged,
        .op = .{ .local = other },
        .next = foreign_jump,
    } });
    const branch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = condition,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = set_site }}),
        .default_branch = foreign,
        .continuation = null,
    } });
    const merge = try store.addCFStmt(.{ .join = .{
        .id = merge_id,
        .params = LIR.LocalSpan.empty(),
        .body = back_write,
        .remainder = branch,
    } });
    const entry_jump = try store.addCFStmt(.{ .jump = .{ .target = loop_id } });
    const entry = try store.addCFStmt(.{ .set_local = .{
        .target = out,
        .value = initial,
        .mode = .initialize_join_param,
        .next = entry_jump,
    } });
    const loop = try store.addCFStmt(.{ .join = .{
        .id = loop_id,
        .params = try store.addLocalSpan(&.{out}),
        .body = merge,
        .remainder = entry,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ initial, other, condition }),
        .body = loop,
        .ret_layout = f.list,
    });

    try run(store, &f.layouts);

    const prepare = store.getCFStmt(foreign).assign_low_level;
    try testing.expectEqual(LowLevelOp.list_map_prepare_reuse, prepare.op);
    try testing.expectEqual(merged, prepare.target);
    try testing.expectEqual(other, GuardedList.at(store.getLocalSpan(prepare.args), 0));
    const spare = store.getCFStmt(prepare.next).assign_low_level;
    try testing.expectEqual(LowLevelOp.list_slack_unique, spare.op);
    try testing.expectEqual(merged, GuardedList.at(store.getLocalSpan(spare.args), 0));
    const length = store.getCFStmt(spare.next).assign_low_level;
    try testing.expectEqual(LowLevelOp.list_len, length.op);
    const limit = store.getCFStmt(length.next).assign_low_level;
    try testing.expectEqual(LowLevelOp.num_int_add_wrap, limit.op);
    const owned = store.getCFStmt(limit.next).assign_low_level;
    try testing.expectEqual(LowLevelOp.list_owned_unique, owned.op);
    try testing.expectEqual(merged, GuardedList.at(store.getLocalSpan(owned.args), 0));
    try testing.expectEqual(foreign_jump, owned.next);

    const forwarded_limit = store.getCFStmt(store.getCFStmt(tracked).assign_ref.next).assign_ref;
    try testing.expectEqual(limit.target, forwarded_limit.target);
    const forwarded_owned = store.getCFStmt(forwarded_limit.next).assign_ref;
    try testing.expectEqual(owned.target, forwarded_owned.target);
    try testing.expectEqual(tracked_jump, forwarded_owned.next);
    try testing.expectEqual(limit.target, store.getCFStmt(back_write).set_local.value);
    try testing.expectEqual(owned.target, store.getCFStmt(store.getCFStmt(back_write).set_local.next).set_local.value);

    const head = store.getCFStmt(loop).join;
    const dispatch = store.getCFStmt(head.body).switch_stmt;
    const copy = store.getCFStmt(GuardedList.at(store.getCFSwitchBranches(dispatch.branches), 0).body).join;
    const hot = try shapeOf(store, copy.body, head.id, copy.id);
    try testing.expectEqual(@as(u32, 1), hot.jumps_to_head);
    try testing.expectEqual(@as(u32, 0), hot.jumps_to_copy);
}
