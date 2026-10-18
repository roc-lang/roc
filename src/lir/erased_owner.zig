//! The ownership unit an owned erased call transfers.
//!
//! An owned erased call records both the callable local it loads the function
//! and capture pointers from and the local whose ownership unit it transfers,
//! and those locals must denote the same erased-callable allocation (design.md,
//! "Destination-Passing Results and Allocation Reuse"). That allocation's
//! local is the root reached from the call's closure by following `assign_ref`
//! edges that preserve exact pointer representation (`core.ErasedOwnerEdge`,
//! the same edge the lowerer records); a closure whose chain is
//! redefined along the way has no single root and transfers its own unit.
//! Debug LIR certification checks every owned erased call against this
//! resolution, and a transform that rewrites the definitions on such a chain
//! re-resolves its calls' reuse sources with it.

const std = @import("std");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");
const BodyClone = @import("body_clone.zig");

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
            .assign_ref => |assign| try self.noteDefinition(assign.target, core.ErasedOwnerEdge.transparentSource(store, layouts, assign.op, assign.target)),
            .init_uninitialized,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_record_update,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_hash,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .boxy_tag_match,
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
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .join,
            .jump,
            .ret,
            .crash,
            => try BodyClone.tryForEachStmtDef(store, stmt, self, noteRoot),
        }
    }

    fn noteRoot(self: *Owners, local: LIR.LocalId) Allocator.Error!void {
        return self.noteDefinition(local, null);
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
    var stmts = try BodyClone.ReachableStmts.initWithAllocator(store, body, allocator);
    defer stmts.deinit();
    while (try stmts.next()) |current| {
        const stmt = store.getCFStmt(current);
        try owners.noteStmt(store, layouts, stmt);
        if (stmt == .assign_call_erased and stmt.assign_call_erased.reuse_source != null) {
            try calls.append(allocator, current);
        }
    }

    for (calls.items) |call_id| {
        const call = &store.getCFStmtPtr(call_id).assign_call_erased;
        call.reuse_source = owners.reuseSource(store, layouts, call.closure);
    }
}
