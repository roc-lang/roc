//! Field takes from dying aggregates.
//!
//! A payload read pays a retain whenever its result must be owned, because
//! the container keeps its stored unit. When the container itself is about
//! to die, that retain is the difference between mutating in place and
//! copying: the read result carries count 2 into the mutation's runtime
//! uniqueness check. This analysis finds containers whose whole life is
//! being read field-by-field and then dying, and marks their consuming
//! reads as takes: the read consumes the container's stored unit for that
//! field, and the container is dismantled instead of released whole.
//!
//! Like precise lifetimes, take solving is order-sensitive, so it runs in
//! the ARC stage against the solved binding modes rather than inside the
//! mode fixpoint. It is deliberately demand-driven: a local that cannot
//! benefit — wrong layout shape, borrowed, escaping, or off-spine uses —
//! contributes nothing beyond its visit in one linear statement scan, and
//! per-candidate tables exist only for locals that pass the layout gate.
//! The rules are specified in design.md's "Field Takes From Dying
//! Aggregates".

const std = @import("std");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");
const arc_solve = @import("arc_solve.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

/// Allocation errors returned while solving field takes.
pub const Error = std.mem.Allocator.Error;

const no_index: u32 = std.math.maxInt(u32);

/// One refcounted field a dismantled container still owns at its death
/// point: emission reads it into a temporary and releases the temporary.
pub const ResidualField = struct {
    /// Original field index, as `assign_ref .field` addresses it.
    field_idx: u32,
    layout_idx: layout_mod.Idx,
};

/// Residual ownership plan for one container local selected for dismantling.
pub const Container = struct {
    residual: []const ResidualField,
};

/// Per-procedure dismantling decisions consumed while emitting ARC operations.
pub const Dismantles = struct {
    arena: std.heap.ArenaAllocator,
    /// `assign_ref .field` statements whose reads consume the container's
    /// stored unit; emission skips their retain.
    takes: std.AutoHashMapUnmanaged(LIR.CFStmtId, void),
    /// Dismantled containers. At the container's death point emission
    /// releases the residual fields instead of the whole value.
    containers: std.AutoHashMapUnmanaged(LIR.LocalId, Container),
    /// Takes on containers that are proc parameters solved borrowed: valid
    /// only in emissions where the demand vector overrides the parameter to
    /// owned (mode-specialized variants). The value is the parameter local,
    /// so emission can check the override for the current variant.
    owned_only_takes: std.AutoHashMapUnmanaged(LIR.CFStmtId, LIR.LocalId),
    /// Containers behind `owned_only_takes`, keyed by the parameter local.
    owned_only_containers: std.AutoHashMapUnmanaged(LIR.LocalId, Container),

    pub fn deinit(self: *Dismantles) void {
        const gpa = self.arena.child_allocator;
        self.takes.deinit(gpa);
        self.containers.deinit(gpa);
        self.owned_only_takes.deinit(gpa);
        self.owned_only_containers.deinit(gpa);
        self.arena.deinit();
    }

    pub fn isTake(self: *const Dismantles, stmt: LIR.CFStmtId) bool {
        return self.takes.contains(stmt);
    }

    pub fn ownedOnlyTakeParam(self: *const Dismantles, stmt: LIR.CFStmtId) ?LIR.LocalId {
        return self.owned_only_takes.get(stmt);
    }

    pub fn containerOf(self: *const Dismantles, local: LIR.LocalId) ?Container {
        return self.containers.get(local);
    }

    pub fn ownedOnlyContainerOf(self: *const Dismantles, local: LIR.LocalId) ?Container {
        return self.owned_only_containers.get(local);
    }
};

const State = enum(u8) {
    unknown,
    ineligible,
    candidate,
    /// Borrowed pure same-value alias of a candidate; reads through it
    /// attribute to the root container.
    transparent_alias,
};

const Read = struct {
    stmt: LIR.CFStmtId,
    target: LIR.LocalId,
    field_idx: u32,
    consuming: bool,
};

const Candidate = struct {
    def_stmt: LIR.CFStmtId = @enumFromInt(no_index),
    def_count: u32 = 0,
    disqualified: bool = false,
    reads: std.ArrayList(Read) = .empty,
};

const Analysis = struct {
    gpa: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    solution: *const arc_solve.Solution,
    state: []State,
    /// Root container local per transparent alias, `no_index` otherwise.
    alias_root: []u32,
    candidates: std.AutoHashMapUnmanaged(u32, Candidate),
    /// Proc parameters. A parameter solved borrowed may still qualify as an
    /// owned-only candidate: mode-specialized variants re-emit it owned.
    is_param: []const bool,
    /// Locals whose value flows into join-carried state: `set_local`
    /// operands and pure-alias feeders of join parameters, closed backward
    /// over pure aliases. A take whose read target escapes this way would
    /// carry its deferred claim across a join quotient, where the certifier's
    /// per-local value model cannot follow it — such reads keep their retain
    /// and the field stays residual.
    escaped: []bool,
    /// Every pure-alias edge as (target, source), for the backward closure.
    alias_pairs: std.ArrayList([2]u32),

    fn deinit(self: *Analysis) void {
        var it = self.candidates.valueIterator();
        while (it.next()) |candidate| candidate.reads.deinit(self.gpa);
        self.candidates.deinit(self.gpa);
        self.alias_pairs.deinit(self.gpa);
        self.gpa.free(self.escaped);
        self.gpa.free(self.alias_root);
        self.gpa.free(self.state);
    }

    /// Whether the local's layout and binding shape could ever benefit from
    /// dismantling. Cheap, no allocation; the full per-candidate work only
    /// happens for locals that pass.
    fn passesGate(self: *Analysis, local: LIR.LocalId) bool {
        const local_layout = self.layouts.getLayout(self.store.getLocal(local).layout_idx);
        if (local_layout.tag != .struct_) return false;
        if (self.solution.isBorrowed(local) and !self.is_param[@intFromEnum(local)]) return false;
        if (self.solution.isJoinParam(local)) return false;
        if (self.solution.maybeUninitializedCondition(local) != null) return false;

        const info = self.layouts.getStructInfo(local_layout);
        var any_rc = false;
        for (0..info.fields.len) |i| {
            const field = info.fields.get(@intCast(i));
            if (field.index >= 64) return false;
            if (self.layouts.layoutContainsRefcounted(self.layouts.getLayout(field.layout))) {
                any_rc = true;
            }
        }
        return any_rc;
    }

    fn entryOf(self: *Analysis, local: LIR.LocalId) Error!?*Candidate {
        const index = @intFromEnum(local);
        switch (self.state[index]) {
            .ineligible, .transparent_alias => return null,
            .candidate => return self.candidates.getPtr(index).?,
            .unknown => {
                if (!self.passesGate(local)) {
                    self.state[index] = .ineligible;
                    return null;
                }
                self.state[index] = .candidate;
                const slot = try self.candidates.getOrPut(self.gpa, index);
                slot.value_ptr.* = .{};
                return slot.value_ptr;
            },
        }
    }

    /// The container a source local stands for: itself, or its alias root.
    fn resolveRoot(self: *Analysis, local: LIR.LocalId) LIR.LocalId {
        const index = @intFromEnum(local);
        if (self.state[index] == .transparent_alias) {
            return @enumFromInt(self.alias_root[index]);
        }
        return local;
    }

    fn disqualify(self: *Analysis, local: LIR.LocalId) void {
        const root = self.resolveRoot(local);
        const index = @intFromEnum(root);
        if (self.state[index] == .candidate) {
            if (self.candidates.getPtr(index)) |candidate| candidate.disqualified = true;
        }
        self.state[index] = .ineligible;
    }

    /// Any occurrence that is not a field read: the local (or the container
    /// it aliases) cannot dismantle.
    fn useWhole(self: *Analysis, local: LIR.LocalId) void {
        self.disqualify(local);
    }

    /// A definition of `local` by `stmt`. Candidates must be bound exactly
    /// once by a value-producing assignment.
    fn noteDef(self: *Analysis, local: LIR.LocalId, stmt: LIR.CFStmtId) Error!void {
        const index = @intFromEnum(local);
        if (self.state[index] == .transparent_alias) {
            // A second definition of an alias re-points it; the root can no
            // longer attribute its reads.
            self.disqualify(local);
            self.state[index] = .ineligible;
            return;
        }
        const candidate = (try self.entryOf(local)) orelse return;
        candidate.def_count += 1;
        if (candidate.def_count > 1) {
            candidate.disqualified = true;
        } else {
            candidate.def_stmt = stmt;
        }
    }

    fn noteFieldRead(self: *Analysis, stmt: LIR.CFStmtId, source: LIR.LocalId, field_idx: u32, target: LIR.LocalId) Error!void {
        const root = self.resolveRoot(source);
        const candidate = (try self.entryOf(root)) orelse return;
        try candidate.reads.append(self.gpa, .{
            .stmt = stmt,
            .target = target,
            .field_idx = field_idx,
            .consuming = !self.solution.isBorrowed(target),
        });
    }

    fn noteAliasDef(self: *Analysis, target: LIR.LocalId, source: LIR.LocalId) Error!void {
        const target_index = @intFromEnum(target);
        if (self.state[target_index] == .transparent_alias) {
            // Redefinition of an existing alias: neither its old nor its new
            // root can attribute reads through it.
            self.disqualify(target);
            self.disqualify(source);
            self.state[target_index] = .ineligible;
            return;
        }
        const root = self.resolveRoot(source);
        const transparent = self.solution.isBorrowed(target) and
            self.solution.leaderOf(target) == root and
            ((try self.entryOf(root)) != null);
        if (transparent) {
            // The alias target itself can never be a container.
            if (self.state[target_index] == .candidate) {
                if (self.candidates.getPtr(target_index)) |candidate| candidate.disqualified = true;
            }
            self.state[target_index] = .transparent_alias;
            self.alias_root[target_index] = @intFromEnum(root);
        } else {
            // An owned alias duplicates the value; the container keeps a
            // second observer.
            self.useWhole(source);
            self.disqualify(target);
        }
    }
};

/// Whether every branch of a switch (including the default) is a plain
/// statement chain that reaches the shared continuation without any control
/// flow of its own. Only then is the continuation guaranteed to run exactly
/// once on every path through the switch, which is what lets the take spine
/// cross the diamond. Branches that return, crash, jump, or branch again are
/// declined; reads past such a switch stay residual.
fn switchFallsThrough(
    store: *const LirStore,
    branches: LIR.CFSwitchBranchSpan,
    default_branch: LIR.CFStmtId,
    continuation: LIR.CFStmtId,
) bool {
    const limit = store.cfStmtCount() + 1;
    const heads = store.getCFSwitchBranches(branches);
    const branch_count = GuardedList.borrowLen(heads);
    for (0..branch_count + 1) |i| {
        var cursor = if (i < branch_count) GuardedList.at(heads, i).body else default_branch;
        var steps: usize = 0;
        while (cursor != continuation) {
            steps += 1;
            if (steps > limit) return false;
            switch (store.getCFStmt(cursor)) {
                inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| cursor = stmt.next,
                else => return false,
            }
        }
    }
    return true;
}

/// Whether a join is a branch diamond rather than a loop: every path through
/// its remainder ends by jumping to this join (a plain chain, optionally
/// through one switch whose every branch is a plain chain ending in that
/// jump), and nothing in its body jumps back to it. Such a join's body runs
/// exactly once, immediately after the remainder, so the take spine may
/// continue into it. Branch-result `if` and `match` expressions lower to
/// exactly this shape; loops fail the body scan through their back-edge.
fn joinIsDiamond(
    gpa: Allocator,
    store: *const LirStore,
    join_id: LIR.JoinPointId,
    remainder: LIR.CFStmtId,
    body: LIR.CFStmtId,
) Error!bool {
    if (!remainderRejoins(store, remainder, join_id)) return false;
    return bodyAvoidsJoin(gpa, store, body, join_id);
}

fn remainderRejoins(store: *const LirStore, first: LIR.CFStmtId, join_id: LIR.JoinPointId) bool {
    const limit = store.cfStmtCount() + 1;
    var cursor = first;
    var steps: usize = 0;
    while (true) {
        steps += 1;
        if (steps > limit) return false;
        switch (store.getCFStmt(cursor)) {
            inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| cursor = stmt.next,
            .jump => |stmt| return stmt.target == join_id,
            .switch_stmt => |stmt| {
                const heads = store.getCFSwitchBranches(stmt.branches);
                const branch_count = GuardedList.borrowLen(heads);
                for (0..branch_count + 1) |i| {
                    var branch_cursor = if (i < branch_count) GuardedList.at(heads, i).body else stmt.default_branch;
                    var branch_steps: usize = 0;
                    branch: while (true) {
                        branch_steps += 1;
                        if (branch_steps > limit) return false;
                        switch (store.getCFStmt(branch_cursor)) {
                            inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |branch_stmt| branch_cursor = branch_stmt.next,
                            .jump => |branch_stmt| {
                                if (branch_stmt.target != join_id) return false;
                                break :branch;
                            },
                            else => return false,
                        }
                    }
                }
                return true;
            },
            else => return false,
        }
    }
}

fn bodyAvoidsJoin(gpa: Allocator, store: *const LirStore, body: LIR.CFStmtId, join_id: LIR.JoinPointId) Error!bool {
    var visited = std.AutoHashMapUnmanaged(u32, void).empty;
    defer visited.deinit(gpa);
    var stack = std.ArrayList(LIR.CFStmtId).empty;
    defer stack.deinit(gpa);
    try stack.append(gpa, body);
    while (stack.pop()) |current| {
        const slot = try visited.getOrPut(gpa, @intFromEnum(current));
        if (slot.found_existing) continue;
        switch (store.getCFStmt(current)) {
            inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| try stack.append(gpa, stmt.next),
            .jump => |stmt| if (stmt.target == join_id) return false,
            .join => |stmt| {
                try stack.append(gpa, stmt.remainder);
                try stack.append(gpa, stmt.body);
            },
            .switch_stmt => |stmt| {
                const heads = store.getCFSwitchBranches(stmt.branches);
                for (0..GuardedList.borrowLen(heads)) |i| {
                    try stack.append(gpa, GuardedList.at(heads, i).body);
                }
                try stack.append(gpa, stmt.default_branch);
                if (stmt.continuation) |continuation| try stack.append(gpa, continuation);
            },
            .switch_initialized_payload => |stmt| {
                try stack.append(gpa, stmt.initialized_branch);
                try stack.append(gpa, stmt.uninitialized_branch);
            },
            .str_match => |stmt| {
                try stack.append(gpa, stmt.on_match);
                try stack.append(gpa, stmt.on_miss);
            },
            .str_match_set => |stmt| {
                const arms = store.getStrMatchArms(stmt.arms);
                for (0..GuardedList.borrowLen(arms)) |i| {
                    try stack.append(gpa, GuardedList.at(arms, i).on_match);
                }
                try stack.append(gpa, stmt.on_miss);
            },
            .boxy_tag_match => |stmt| {
                try stack.append(gpa, stmt.on_match);
                try stack.append(gpa, stmt.on_miss);
            },
            .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
        }
    }
    return true;
}

/// Solve takes for every reachable statement in the store.
pub fn compute(
    gpa: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    solution: *const arc_solve.Solution,
) Error!Dismantles {
    // Proc parameters are defined by the proc entry rather than a statement;
    // remember each parameter's body so its spine has a start. A local that
    // parameterizes more than one proc spec never dismantles.
    var param_bodies = std.AutoHashMapUnmanaged(LIR.LocalId, ?LIR.CFStmtId).empty;
    defer param_bodies.deinit(gpa);
    const is_param = try gpa.alloc(bool, store.localCount());
    defer gpa.free(is_param);
    @memset(is_param, false);
    for (0..store.procSpecCount()) |proc_index| {
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        const body = proc.body orelse continue;
        const params = store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(params)) |position| {
            const param = GuardedList.at(params, position);
            is_param[@intFromEnum(param)] = true;
            const slot = try param_bodies.getOrPut(gpa, param);
            if (slot.found_existing) {
                slot.value_ptr.* = null;
            } else {
                slot.value_ptr.* = body;
            }
        }
    }

    var analysis = Analysis{
        .gpa = gpa,
        .store = store,
        .layouts = layouts,
        .solution = solution,
        .state = try gpa.alloc(State, store.localCount()),
        .alias_root = try gpa.alloc(u32, store.localCount()),
        .candidates = .empty,
        .is_param = is_param,
        .escaped = try gpa.alloc(bool, store.localCount()),
        .alias_pairs = .empty,
    };
    defer analysis.deinit();
    @memset(analysis.state, .unknown);
    @memset(analysis.alias_root, no_index);
    @memset(analysis.escaped, false);

    // One linear scan over every reachable statement, classifying each
    // occurrence of each local. The switch is exhaustive so a new statement
    // form fails to compile rather than silently escaping classification.
    var visited = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(gpa, store.cfStmtCount());
    defer visited.deinit(gpa);
    var stack = std.ArrayList(LIR.CFStmtId).empty;
    defer stack.deinit(gpa);
    for (0..store.procSpecCount()) |proc_index| {
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        if (proc.body) |body| try stack.append(gpa, body);
    }

    while (stack.pop()) |current| {
        const stmt_index = @intFromEnum(current);
        if (visited.isSet(stmt_index)) continue;
        visited.set(stmt_index);
        switch (store.getCFStmt(current)) {
            .init_uninitialized => |stmt| {
                analysis.useWhole(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_ref => |stmt| {
                switch (stmt.op) {
                    .field => |op| {
                        try analysis.noteFieldRead(current, op.source, op.field_idx, stmt.target);
                        try analysis.noteDef(stmt.target, current);
                    },
                    .local => |source| {
                        if (stmt.target == source) {
                            analysis.useWhole(source);
                        } else {
                            try analysis.noteAliasDef(stmt.target, source);
                            try analysis.alias_pairs.append(gpa, .{ @intFromEnum(stmt.target), @intFromEnum(source) });
                            if (solution.isJoinParam(stmt.target)) {
                                analysis.escaped[@intFromEnum(source)] = true;
                            }
                        }
                    },
                    .discriminant => |op| {
                        analysis.useWhole(op.source);
                        try analysis.noteDef(stmt.target, current);
                    },
                    .tag_payload => |op| {
                        analysis.useWhole(op.source);
                        try analysis.noteDef(stmt.target, current);
                    },
                    .tag_payload_struct => |op| {
                        analysis.useWhole(op.source);
                        try analysis.noteDef(stmt.target, current);
                    },
                    .list_reinterpret => |op| {
                        analysis.useWhole(op.backing_ref);
                        analysis.disqualify(stmt.target);
                    },
                    .nominal => |op| {
                        analysis.useWhole(op.backing_ref);
                        analysis.disqualify(stmt.target);
                    },
                }
                try stack.append(gpa, stmt.next);
            },
            .assign_literal => |stmt| {
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_call => |stmt| {
                const args = store.getLocalSpan(stmt.args);
                for (0..GuardedList.borrowLen(args)) |i| analysis.useWhole(GuardedList.at(args, i));
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_call_erased => |stmt| {
                analysis.useWhole(stmt.closure);
                if (stmt.reuse_source) |reuse_source| analysis.useWhole(reuse_source);
                const args = store.getLocalSpan(stmt.args);
                for (0..GuardedList.borrowLen(args)) |i| analysis.useWhole(GuardedList.at(args, i));
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_packed_erased_fn => |stmt| {
                if (stmt.capture) |capture| analysis.useWhole(capture);
                if (stmt.reuse) |reuse| analysis.useWhole(reuse);
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_desc_ref => |stmt| {
                if (stmt.desc.localOrNull()) |local| analysis.useWhole(local);
                if (stmt.tag_residual_for) |desc| if (desc.localOrNull()) |local| analysis.useWhole(local);
                const captures = store.getLocalSpan(stmt.captures);
                for (0..GuardedList.borrowLen(captures)) |i| analysis.useWhole(GuardedList.at(captures, i));
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_dict_ref => |stmt| {
                if (stmt.dict.localOrNull()) |local| analysis.useWhole(local);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_box => |stmt| {
                analysis.useWhole(stmt.payload);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_reuse_box => |stmt| {
                analysis.useWhole(stmt.source);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_unbox => |stmt| {
                analysis.useWhole(stmt.source);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_adapt => |stmt| {
                analysis.useWhole(stmt.source);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_inspect => |stmt| {
                analysis.useWhole(stmt.source);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_eq => |stmt| {
                analysis.useWhole(stmt.lhs);
                analysis.useWhole(stmt.rhs);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_tag => |stmt| {
                if (stmt.payload) |payload| analysis.useWhole(payload);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_tag_payload => |stmt| {
                analysis.useWhole(stmt.source);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                if (stmt.target_desc) |target_desc| {
                    try analysis.noteDef(target_desc, current);
                    analysis.disqualify(target_desc);
                }
                try stack.append(gpa, stmt.next);
            },
            .assign_call_dict => |stmt| {
                if (stmt.dict.localOrNull()) |local| analysis.useWhole(local);
                const args = store.getLocalSpan(stmt.args);
                for (0..GuardedList.borrowLen(args)) |i| analysis.useWhole(GuardedList.at(args, i));
                const arg_descs = store.getLocalSpan(stmt.arg_descs);
                for (0..GuardedList.borrowLen(arg_descs)) |i| analysis.useWhole(GuardedList.at(arg_descs, i));
                const hidden_args = store.getLocalSpan(stmt.hidden_args);
                for (0..GuardedList.borrowLen(hidden_args)) |i| analysis.useWhole(GuardedList.at(hidden_args, i));
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_low_level => |stmt| {
                const args = store.getLocalSpan(stmt.args);
                for (0..GuardedList.borrowLen(args)) |i| analysis.useWhole(GuardedList.at(args, i));
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_list => |stmt| {
                const elems = store.getLocalSpan(stmt.elems);
                for (0..GuardedList.borrowLen(elems)) |i| analysis.useWhole(GuardedList.at(elems, i));
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_struct => |stmt| {
                const fields = store.getLocalSpan(stmt.fields);
                for (0..GuardedList.borrowLen(fields)) |i| analysis.useWhole(GuardedList.at(fields, i));
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_tag => |stmt| {
                if (stmt.payload) |payload| analysis.useWhole(payload);
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .store_struct => |stmt| {
                analysis.useWhole(stmt.dest);
                const fields = store.getLocalSpan(stmt.fields);
                for (0..GuardedList.borrowLen(fields)) |i| analysis.useWhole(GuardedList.at(fields, i));
                try stack.append(gpa, stmt.next);
            },
            .store_tag => |stmt| {
                analysis.useWhole(stmt.dest);
                if (stmt.payload) |payload| analysis.useWhole(payload);
                try stack.append(gpa, stmt.next);
            },
            .set_local => |stmt| {
                analysis.useWhole(stmt.value);
                analysis.useWhole(stmt.target);
                analysis.escaped[@intFromEnum(stmt.value)] = true;
                try stack.append(gpa, stmt.next);
            },
            .debug => |stmt| {
                analysis.useWhole(stmt.message);
                try stack.append(gpa, stmt.next);
            },
            .expect => |stmt| {
                analysis.useWhole(stmt.condition);
                try stack.append(gpa, stmt.next);
            },
            .expect_err => |stmt| analysis.useWhole(stmt.message),
            .runtime_error => {},
            .comptime_exhaustiveness_failed => {},
            .comptime_branch_taken => |stmt| try stack.append(gpa, stmt.next),
            // The input contract is RC-free LIR; if RC statements ever appear
            // here, classifying their operands as whole uses stays sound.
            .incref => |stmt| {
                analysis.useWhole(stmt.value);
                try stack.append(gpa, stmt.next);
            },
            .decref => |stmt| {
                analysis.useWhole(stmt.value);
                try stack.append(gpa, stmt.next);
            },
            .decref_if_initialized => |stmt| {
                analysis.useWhole(stmt.cond);
                analysis.useWhole(stmt.value);
                try stack.append(gpa, stmt.next);
            },
            .free => |stmt| {
                analysis.useWhole(stmt.value);
                try stack.append(gpa, stmt.next);
            },
            .switch_stmt => |stmt| {
                analysis.useWhole(stmt.cond);
                const branches = store.getCFSwitchBranches(stmt.branches);
                for (0..GuardedList.borrowLen(branches)) |i| {
                    try stack.append(gpa, GuardedList.at(branches, i).body);
                }
                try stack.append(gpa, stmt.default_branch);
                if (stmt.continuation) |continuation| try stack.append(gpa, continuation);
            },
            .switch_initialized_payload => |stmt| {
                analysis.useWhole(stmt.cond);
                analysis.useWhole(stmt.payload);
                try stack.append(gpa, stmt.initialized_branch);
                try stack.append(gpa, stmt.uninitialized_branch);
            },
            .str_match => |stmt| {
                analysis.useWhole(stmt.source);
                const steps = store.getStrMatchSteps(stmt.steps);
                for (0..GuardedList.borrowLen(steps)) |i| {
                    switch (GuardedList.at(steps, i).capture) {
                        .discard => {},
                        .view => |view_local| analysis.useWhole(view_local),
                    }
                }
                try stack.append(gpa, stmt.on_match);
                try stack.append(gpa, stmt.on_miss);
            },
            .str_match_set => |stmt| {
                analysis.useWhole(stmt.source);
                const arms = store.getStrMatchArms(stmt.arms);
                for (0..GuardedList.borrowLen(arms)) |arm_index| {
                    const arm = GuardedList.at(arms, arm_index);
                    const steps = store.getStrMatchSteps(arm.steps);
                    for (0..GuardedList.borrowLen(steps)) |i| {
                        switch (GuardedList.at(steps, i).capture) {
                            .discard => {},
                            .view => |view_local| analysis.useWhole(view_local),
                        }
                    }
                    try stack.append(gpa, arm.on_match);
                }
                try stack.append(gpa, stmt.on_miss);
            },
            .boxy_tag_match => |stmt| {
                analysis.useWhole(stmt.source);
                try stack.append(gpa, stmt.on_match);
                try stack.append(gpa, stmt.on_miss);
            },
            .loop_continue, .loop_break => {},
            .join => |stmt| {
                // Join parameters are excluded by the gate; the condition
                // locals are scalar presence words.
                try stack.append(gpa, stmt.body);
                try stack.append(gpa, stmt.remainder);
            },
            .jump => {},
            .ret => |stmt| analysis.useWhole(stmt.value),
            .crash => {},
        }
    }

    // Close the escape set backward over pure aliases: if an alias target's
    // value reaches join-carried state, so does its source's.
    var escape_changed = true;
    while (escape_changed) {
        escape_changed = false;
        for (analysis.alias_pairs.items) |pair| {
            if (analysis.escaped[pair[0]] and !analysis.escaped[pair[1]]) {
                analysis.escaped[pair[1]] = true;
                escape_changed = true;
            }
        }
    }

    // Second phase: verify the surviving candidates' read shapes and spines,
    // and build the output.
    var result = Dismantles{
        .arena = std.heap.ArenaAllocator.init(gpa),
        .takes = .empty,
        .containers = .empty,
        .owned_only_takes = .empty,
        .owned_only_containers = .empty,
    };
    errdefer result.deinit();

    var spine_pending = std.AutoHashMapUnmanaged(LIR.CFStmtId, void).empty;
    defer spine_pending.deinit(gpa);
    var spine_starts = std.ArrayList(LIR.CFStmtId).empty;
    defer spine_starts.deinit(gpa);
    // Diamond verdicts are a property of the join alone; one scan serves
    // every candidate whose spine crosses it.
    var diamond_joins = std.AutoHashMapUnmanaged(u32, bool).empty;
    defer diamond_joins.deinit(gpa);

    var it = analysis.candidates.iterator();
    candidates: while (it.next()) |entry| {
        const local: LIR.LocalId = @enumFromInt(entry.key_ptr.*);
        const candidate = entry.value_ptr;
        if (candidate.disqualified) continue;
        if (candidate.reads.items.len == 0) continue;

        // Payload-read definitions (`assign_ref`) are excluded: a container
        // that is itself a taken or claimable payload never holds its own
        // certifier unit, so its dismantle's claims would have nothing to
        // spend. Its whole release stays, itself claiming the outer field
        // when the outer container dismantles.
        const spine_start: LIR.CFStmtId = if (candidate.def_count == 1)
            switch (store.getCFStmt(candidate.def_stmt)) {
                inline .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag => |stmt| stmt.next,
                else => continue :candidates,
            }
        else if (candidate.def_count == 0)
            ((param_bodies.get(local) orelse continue :candidates) orelse continue :candidates)
        else
            continue :candidates;

        // A taken field must be read exactly once, by a consuming read whose
        // target stays out of join-carried state. One read needs no order
        // reasoning (a borrow after the take could observe stale bytes once
        // the taker mutates), and a non-escaping target keeps the deferred
        // claim inside one certifier walk segment. Fields failing either
        // rule simply stay residual: their reads keep their retains and the
        // dismantle releases their stored units at the death point.
        var taken_mask: u64 = 0;
        var repeat_mask: u64 = 0;
        var seen_mask: u64 = 0;
        for (candidate.reads.items) |read| {
            if (read.field_idx >= 64) continue :candidates;
            const bit = @as(u64, 1) << @intCast(read.field_idx);
            if (seen_mask & bit != 0) repeat_mask |= bit;
            seen_mask |= bit;
            if (read.consuming and !analysis.escaped[@intFromEnum(read.target)]) {
                taken_mask |= bit;
            }
        }
        taken_mask &= ~repeat_mask;

        // Only refcounted fields carry stored units worth taking; a
        // container with no refcounted take keeps its ordinary whole
        // release.
        const local_layout = layouts.getLayout(store.getLocal(local).layout_idx);
        const info = layouts.getStructInfo(local_layout);
        var rc_mask: u64 = 0;
        for (0..info.fields.len) |i| {
            const field = info.fields.get(@intCast(i));
            if (layouts.layoutContainsRefcounted(layouts.getLayout(field.layout))) {
                rc_mask |= @as(u64, 1) << @intCast(field.index);
            }
        }
        taken_mask &= rc_mask;
        if (taken_mask == 0) continue;

        // Every take must sit on the container's spine: the chain from its
        // definition through `next` edges and join remainders. That is what
        // makes each take run exactly once, in order, before the container
        // dies — the death point follows the last use on every path, so
        // residual reads may live in branches without reordering risk.
        spine_pending.clearRetainingCapacity();
        for (candidate.reads.items) |read| {
            const bit = @as(u64, 1) << @intCast(read.field_idx);
            if (taken_mask & bit == 0) continue;
            try spine_pending.put(gpa, read.stmt, {});
        }
        var remaining = spine_pending.count();
        spine_starts.clearRetainingCapacity();
        try spine_starts.append(gpa, spine_start);
        var steps: usize = 0;
        const step_limit = store.cfStmtCount() + 1;
        walk: while (remaining > 0) {
            var cursor = spine_starts.pop() orelse break;
            chain: while (remaining > 0) {
                steps += 1;
                if (steps > step_limit) break :walk;
                if (spine_pending.contains(cursor)) remaining -= 1;
                switch (store.getCFStmt(cursor)) {
                    inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| cursor = stmt.next,
                    // The remainder always runs exactly once. When the join
                    // is a branch diamond — every remainder path rejoins it
                    // and its body never loops back — the body runs exactly
                    // once too, immediately after, so takes past the rejoin
                    // are as good as takes before the branch.
                    .join => |stmt| {
                        const cached = try diamond_joins.getOrPut(gpa, @intFromEnum(cursor));
                        if (!cached.found_existing) {
                            cached.value_ptr.* = try joinIsDiamond(gpa, store, stmt.id, stmt.remainder, stmt.body);
                        }
                        if (cached.value_ptr.*) try spine_starts.append(gpa, stmt.body);
                        cursor = stmt.remainder;
                    },
                    // Likewise for a switch whose every branch falls straight
                    // through to its shared continuation.
                    .switch_stmt => |stmt| {
                        const continuation = stmt.continuation orelse break :chain;
                        if (!switchFallsThrough(store, stmt.branches, stmt.default_branch, continuation)) break :chain;
                        cursor = continuation;
                    },
                    .switch_initialized_payload, .str_match, .str_match_set, .boxy_tag_match, .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => break :chain,
                }
            }
        }
        if (remaining > 0) continue;

        // Accepted. Record the takes and the residual: every refcounted
        // field that was not taken is released at the death point.
        var residual = std.ArrayList(ResidualField).empty;
        defer residual.deinit(gpa);
        for (0..info.fields.len) |i| {
            const field = info.fields.get(@intCast(i));
            const bit = @as(u64, 1) << @intCast(field.index);
            if (taken_mask & bit != 0) continue;
            if (!layouts.layoutContainsRefcounted(layouts.getLayout(field.layout))) continue;
            try residual.append(gpa, .{ .field_idx = field.index, .layout_idx = field.layout });
        }

        // A parameter solved borrowed dismantles only in emissions whose
        // demand vector overrides it to owned; everything else applies to
        // every emission of its proc.
        const owned_only = solution.isBorrowed(local);
        const stored_residual = try result.arena.allocator().dupe(ResidualField, residual.items);
        for (candidate.reads.items) |read| {
            const bit = @as(u64, 1) << @intCast(read.field_idx);
            if (taken_mask & bit == 0) continue;
            if (owned_only) {
                try result.owned_only_takes.put(gpa, read.stmt, local);
            } else {
                try result.takes.put(gpa, read.stmt, {});
            }
        }
        if (owned_only) {
            try result.owned_only_containers.put(gpa, local, .{ .residual = stored_residual });
        } else {
            try result.containers.put(gpa, local, .{ .residual = stored_residual });
        }
    }

    return result;
}
