//! The ownership unit an owned erased call transfers.
//!
//! An owned erased call records both the callable local it loads the function
//! and capture pointers from and the local whose ownership unit it transfers,
//! and those locals must denote the same erased-callable allocation (design.md,
//! "Destination-Passing Results and Allocation Reuse"). That allocation's
//! local is the root reached from the call's closure by following `assign_ref`
//! edges that preserve exact pointer representation; a closure whose chain is
//! redefined along the way has no single root and transfers its own unit.
//! Debug LIR certification checks every owned erased call against this
//! resolution, and a transform that rewrites the definitions on such a chain
//! re-resolves its calls' reuse sources with it.

const std = @import("std");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

const State = union(enum) {
    root,
    alias: LIR.LocalId,
    ambiguous,
};

/// The erased-allocation producer relation of one procedure body.
pub const Owners = struct {
    states: collections.DenseMap(LIR.LocalId, State),

    pub fn init(allocator: Allocator) Owners {
        return .{ .states = collections.DenseMap(LIR.LocalId, State).init(allocator) };
    }

    pub fn deinit(self: *Owners) void {
        self.states.deinit();
    }

    pub fn clear(self: *Owners) void {
        self.states.clearRetainingCapacity();
    }

    /// Record one definition of `target`, as an alias of `source` when the
    /// defining operation preserves exact pointer representation.
    pub fn noteDefinition(self: *Owners, target: LIR.LocalId, source: ?LIR.LocalId) Allocator.Error!void {
        const entry = try self.states.getOrPut(target);
        if (entry.found_existing) {
            entry.value_ptr.* = .ambiguous;
        } else {
            entry.value_ptr.* = if (source) |owner| .{ .alias = owner } else .root;
        }
    }

    /// Record every definition `stmt` makes.
    pub fn noteStmt(self: *Owners, store: *const LirStore, layouts: *const layout_mod.Store, stmt: LIR.CFStmt) Allocator.Error!void {
        switch (stmt) {
            .assign_ref => |assign| try self.noteDefinition(assign.target, transparentSource(store, layouts, assign.op, assign.target)),
            .assign_literal => |assign| try self.noteDefinition(assign.target, null),
            .init_uninitialized => |uninitialized| try self.noteDefinition(uninitialized.target, null),
            .assign_call => |assign| try self.noteDefinition(assign.target, null),
            .assign_call_erased => |assign| try self.noteDefinition(assign.target, null),
            .assign_packed_erased_fn => |assign| try self.noteDefinition(assign.target, null),
            .assign_low_level => |assign| try self.noteDefinition(assign.target, null),
            .assign_list => |assign| try self.noteDefinition(assign.target, null),
            .assign_struct => |assign| try self.noteDefinition(assign.target, null),
            .assign_tag => |assign| try self.noteDefinition(assign.target, null),
            .set_local => |assign| try self.noteDefinition(assign.target, null),
            .str_match => |str_match| try self.noteStrMatchSteps(store, str_match.steps),
            .str_match_set => |str_match_set| {
                const arms = store.getStrMatchArms(str_match_set.arms);
                for (0..GuardedList.borrowLen(arms)) |arm_index| {
                    try self.noteStrMatchSteps(store, GuardedList.at(arms, arm_index).steps);
                }
            },
            .join => |join_stmt| {
                const params = store.getLocalSpan(join_stmt.params);
                for (0..GuardedList.borrowLen(params)) |param_index| {
                    try self.noteDefinition(GuardedList.at(params, param_index), null);
                }
            },
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_record_update,
            .assign_boxy_reuse_box,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_hash,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .boxy_tag_match,
            .assign_call_dict,
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
            .loop_continue,
            .loop_break,
            .jump,
            .ret,
            .crash,
            => {},
        }
    }

    fn noteStrMatchSteps(self: *Owners, store: *const LirStore, span: LIR.StrMatchStepSpan) Allocator.Error!void {
        const steps = store.getStrMatchSteps(span);
        for (0..GuardedList.borrowLen(steps)) |step_index| {
            switch (GuardedList.at(steps, step_index).capture) {
                .discard => {},
                .view => |local| try self.noteDefinition(local, null),
            }
        }
    }

    /// The local whose ownership unit an owned erased call through `closure`
    /// transfers: the closure's refcounted root, or the closure itself when
    /// its chain has no single root.
    pub fn reuseSource(self: *const Owners, store: *const LirStore, layouts: *const layout_mod.Store, closure: LIR.LocalId) LIR.LocalId {
        var current = closure;
        for (0..self.states.count() + 1) |_| {
            const state = self.states.get(current) orelse return refcountedOwner(store, layouts, current) orelse closure;
            switch (state) {
                .root => return refcountedOwner(store, layouts, current) orelse closure,
                .alias => |source| current = source,
                .ambiguous => return closure,
            }
        }
        return closure;
    }
};

/// The local `op` reads when the read preserves the exact pointer
/// representation of a single erased-callable allocation.
pub fn transparentSource(store: *const LirStore, layouts: *const layout_mod.Store, op: LIR.RefOp, target: LIR.LocalId) ?LIR.LocalId {
    const source = switch (op) {
        .local => |local| local,
        .nominal => |nominal| nominal.backing_ref,
        inline .tag_payload, .tag_payload_struct => |payload| blk: {
            if (payload.variant_index != 0) break :blk null;
            const source_layout = layouts.getLayout(store.getLocal(payload.source).layout_idx);
            if (source_layout.tag != .tag_union) break :blk null;
            const data = layouts.getTagUnionData(source_layout.getTagUnion().idx);
            if (data.discriminant_size != 0) break :blk null;
            break :blk payload.source;
        },
        .discriminant, .field, .list_reinterpret => null,
    } orelse return null;

    const source_size = layouts.layoutSizeAlign(layouts.getLayout(store.getLocal(source).layout_idx)).size;
    const target_size = layouts.layoutSizeAlign(layouts.getLayout(store.getLocal(target).layout_idx)).size;
    return if (source_size == layouts.targetUsize().size() and source_size == target_size) source else null;
}

fn refcountedOwner(store: *const LirStore, layouts: *const layout_mod.Store, local: LIR.LocalId) ?LIR.LocalId {
    return if (layouts.layoutContainsRefcounted(layouts.getLayout(store.getLocal(local).layout_idx))) local else null;
}

/// Set every owned erased call's reuse source in `proc_id`'s body to the
/// ownership unit its closure resolves to.
pub fn resolveProcReuseSources(
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout_mod.Store,
    proc_id: LIR.LirProcSpecId,
) Allocator.Error!void {
    const proc = store.getProcSpec(proc_id);
    const body = proc.body orelse return;

    var owners = Owners.init(allocator);
    defer owners.deinit();
    const args = store.getLocalSpan(proc.args);
    for (0..GuardedList.borrowLen(args)) |arg_index| {
        try owners.noteDefinition(GuardedList.at(args, arg_index), null);
    }

    var calls = std.ArrayList(LIR.CFStmtId).empty;
    defer calls.deinit(allocator);
    var visited = collections.DenseMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();
    var stack = std.ArrayList(LIR.CFStmtId).empty;
    defer stack.deinit(allocator);
    try stack.append(allocator, body);
    while (stack.pop()) |current| {
        if ((try visited.getOrPut(current)).found_existing) continue;
        const stmt = store.getCFStmt(current);
        try owners.noteStmt(store, layouts, stmt);
        if (stmt == .assign_call_erased and stmt.assign_call_erased.reuse_source != null) {
            try calls.append(allocator, current);
        }
        try appendSuccessors(allocator, store, stmt, &stack);
    }

    for (calls.items) |call_id| {
        const call = &store.getCFStmtPtr(call_id).assign_call_erased;
        call.reuse_source = owners.reuseSource(store, layouts, call.closure);
    }
}

fn appendSuccessors(allocator: Allocator, store: *const LirStore, stmt: LIR.CFStmt, stack: *std.ArrayList(LIR.CFStmtId)) Allocator.Error!void {
    switch (stmt) {
        .switch_stmt => |switch_stmt| {
            const branches = store.getCFSwitchBranches(switch_stmt.branches);
            for (0..GuardedList.borrowLen(branches)) |branch_index| {
                try stack.append(allocator, GuardedList.at(branches, branch_index).body);
            }
            try stack.append(allocator, switch_stmt.default_branch);
            if (switch_stmt.continuation) |continuation| try stack.append(allocator, continuation);
        },
        .switch_initialized_payload => |switch_stmt| {
            try stack.append(allocator, switch_stmt.initialized_branch);
            try stack.append(allocator, switch_stmt.uninitialized_branch);
        },
        .str_match => |str_match| {
            try stack.append(allocator, str_match.on_match);
            try stack.append(allocator, str_match.on_miss);
        },
        .boxy_tag_match => |tag_match| {
            try stack.append(allocator, tag_match.on_match);
            try stack.append(allocator, tag_match.on_miss);
        },
        .str_match_set => |str_match_set| {
            const arms = store.getStrMatchArms(str_match_set.arms);
            for (0..GuardedList.borrowLen(arms)) |arm_index| {
                try stack.append(allocator, GuardedList.at(arms, arm_index).on_match);
            }
            try stack.append(allocator, str_match_set.on_miss);
        },
        .join => |join_stmt| {
            try stack.append(allocator, join_stmt.body);
            try stack.append(allocator, join_stmt.remainder);
        },
        .ret, .crash, .jump, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break, .expect_err => {},
        .comptime_branch_taken => |marker| try stack.append(allocator, marker.next),
        inline .init_uninitialized,
        .assign_ref,
        .assign_literal,
        .assign_call,
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_boxy_desc_ref,
        .assign_boxy_dict_ref,
        .assign_boxy_box,
        .assign_boxy_record_update,
        .assign_boxy_reuse_box,
        .assign_boxy_unbox,
        .assign_boxy_adapt,
        .assign_boxy_inspect,
        .assign_boxy_eq,
        .assign_boxy_hash,
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
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        => |s| try stack.append(allocator, s.next),
    }
}
