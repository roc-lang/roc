//! Removes join parameters that no statement reads.
//!
//! Lowering carries every `var` an `if` or `match` reassigns out of the
//! construct as a join parameter, whether or not anything reads the variable
//! afterwards. A parameter nothing reads is still written on every entry, and
//! that `set_local` is a use of the written value after everything that
//! precedes it on the entry path. When the entry path passes the same value to
//! a consuming call first, ARC must retain it for the call so the write still
//! has a value to store, and the call sees a shared value it must copy.
//!
//! This pass runs after direct LIR lowering and before ARC insertion. A join
//! parameter qualifies when no reachable statement reads it and every write
//! to it is an explicit `set_local`: the parameter is removed from its join,
//! and each of its writes is deleted by routing the write's incoming edges to
//! the write's successor. Deleting a write can leave the written value unread,
//! so the pass repeats until no parameter qualifies.
//!
//! A parameter is never removed when anything reads it outside an operand
//! position: a join's retained or maybe-uninitialized lists, another local's
//! descriptor, or the implicit carry of a `loop_continue` or `loop_break`,
//! which keeps every current definition live. A procedure argument that is
//! also a join parameter is defined by the call as well as by its writes, so
//! it is never removed either.

const std = @import("std");
const collections = @import("collections");
const core = @import("lir_core");
const BodyClone = @import("body_clone.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;

const LocalSet = collections.DenseMap(LocalId, void);

/// Prune one procedure with task-local scratch; rewritten LIR stays in the store.
pub fn runProc(store: *LirStore, proc_id: LIR.LirProcSpecId, scratch_allocator: Allocator) Allocator.Error!void {
    var pass = Pass.init(store, proc_id, scratch_allocator);
    defer pass.deinit();
    while (try pass.round()) {}
}

const Pass = struct {
    store: *LirStore,
    proc_id: LIR.LirProcSpecId,
    allocator: Allocator,
    stmts: std.ArrayList(CFStmtId) = .empty,
    reads: collections.DenseMap(LocalId, u32),
    /// Parameters of any reachable join.
    params: LocalSet,
    /// Locals read somewhere other than an operand position, or defined by
    /// something other than a join binding or a `set_local`.
    pinned: LocalSet,
    /// Parameters this round removes.
    pruned: LocalSet,
    /// Writes this round deletes, mapped to their successor.
    deleted: collections.DenseMap(CFStmtId, CFStmtId),

    fn init(store: *LirStore, proc_id: LIR.LirProcSpecId, allocator: Allocator) Pass {
        return .{
            .store = store,
            .proc_id = proc_id,
            .allocator = allocator,
            .reads = collections.DenseMap(LocalId, u32).init(allocator),
            .params = LocalSet.init(allocator),
            .pinned = LocalSet.init(allocator),
            .pruned = LocalSet.init(allocator),
            .deleted = collections.DenseMap(CFStmtId, CFStmtId).init(allocator),
        };
    }

    fn deinit(self: *Pass) void {
        self.stmts.deinit(self.allocator);
        self.reads.deinit();
        self.params.deinit();
        self.pinned.deinit();
        self.pruned.deinit();
        self.deleted.deinit();
    }

    /// Prune every parameter that qualifies now; report whether any did.
    fn round(self: *Pass) Allocator.Error!bool {
        self.stmts.clearRetainingCapacity();
        self.reads.clearRetainingCapacity();
        self.params.clearRetainingCapacity();
        self.pinned.clearRetainingCapacity();
        self.pruned.clearRetainingCapacity();
        self.deleted.clearRetainingCapacity();

        const store = self.store;
        const proc = store.getProcSpec(self.proc_id);
        const body = proc.body orelse return false;

        const args = store.getLocalSpan(proc.args);
        for (0..args.len) |index| try self.pinDefinition(GuardedList.at(args, index));

        var walk = try BodyClone.ReachableStmts.initWithAllocator(store, body, self.allocator);
        defer walk.deinit();
        while (try walk.next()) |stmt_id| {
            try self.stmts.append(self.allocator, stmt_id);
            const stmt = store.getCFStmt(stmt_id);
            var reads: ReadNote = .{ .reads = &self.reads };
            BodyClone.forEachStmtRead(store, stmt, &reads, ReadNote.note);
            if (reads.failure) |err| return err;
            switch (stmt) {
                .loop_continue, .loop_break => return false,
                .join => |join| {
                    const params = store.getLocalSpan(join.params);
                    for (0..params.len) |index| {
                        const param = GuardedList.at(params, index);
                        try self.params.put(param, {});
                        try self.pinDescriptor(param);
                    }
                    try self.pinSpan(join.retained);
                    try self.pinSpan(join.maybe_uninitialized_params);
                    try self.pinSpan(join.maybe_uninitialized_conditions);
                },
                .set_local => |set| try self.pinDescriptor(set.target),
                .init_uninitialized,
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
                .jump,
                .ret,
                .crash,
                => {
                    var defs: DefNote = .{ .pass = self };
                    BodyClone.forEachStmtDef(store, stmt, &defs, DefNote.note);
                    if (defs.failure) |err| return err;
                },
            }
        }

        var params = self.params.iterator();
        while (params.next()) |entry| {
            const param = entry.key_ptr.*;
            if (self.pinned.contains(param)) continue;
            if ((self.reads.get(param) orelse 0) != 0) continue;
            try self.pruned.put(param, {});
        }
        if (self.pruned.count() == 0) return false;

        for (self.stmts.items) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .join) try self.pruneJoinParams(stmt_id, stmt.join.params);
            if (stmt == .set_local and self.pruned.contains(stmt.set_local.target)) {
                try self.deleted.put(stmt_id, stmt.set_local.next);
            }
        }

        for (self.stmts.items) |stmt_id| {
            if (self.deleted.contains(stmt_id)) continue;
            BodyClone.redirectSuccessors(store, stmt_id, self, resolve);
        }
        store.getProcSpecPtr(self.proc_id).body = resolve(self, body);
        return true;
    }

    fn pruneJoinParams(self: *Pass, join_stmt: CFStmtId, span: LIR.LocalSpan) Allocator.Error!void {
        var kept = std.ArrayList(LocalId).empty;
        defer kept.deinit(self.allocator);
        const params = self.store.getLocalSpan(span);
        for (0..params.len) |index| {
            const param = GuardedList.at(params, index);
            if (!self.pruned.contains(param)) try kept.append(self.allocator, param);
        }
        if (kept.items.len == params.len) return;
        const kept_span = try self.store.addLocalSpan(kept.items);
        self.store.getCFStmtPtr(join_stmt).join.params = kept_span;
    }

    /// The first statement at or after `id` that this round keeps. Deleted
    /// writes each move strictly forward along their `next` edge, so a run of
    /// them ends at a kept statement.
    fn resolve(self: *Pass, id: CFStmtId) CFStmtId {
        var current = id;
        while (self.deleted.get(current)) |next| current = next;
        return current;
    }

    fn pinSpan(self: *Pass, span: LIR.LocalSpan) Allocator.Error!void {
        const locals = self.store.getLocalSpan(span);
        for (0..locals.len) |index| try self.pinned.put(GuardedList.at(locals, index), {});
    }

    fn pinDefinition(self: *Pass, local: LocalId) Allocator.Error!void {
        try self.pinned.put(local, {});
        try self.pinDescriptor(local);
    }

    /// A local's descriptor reads the descriptor local wherever the local is
    /// used, so a parameter serving as a descriptor is never unread.
    fn pinDescriptor(self: *Pass, local: LocalId) Allocator.Error!void {
        const desc = self.store.getLocal(local).boxy_desc orelse return;
        if (desc.localOrNull()) |desc_local| try self.pinned.put(desc_local, {});
    }
};

const ReadNote = struct {
    reads: *collections.DenseMap(LocalId, u32),
    failure: ?Allocator.Error = null,

    fn note(self: *ReadNote, local: LocalId) void {
        if (self.failure != null) return;
        const entry = self.reads.getOrPut(local) catch |err| {
            self.failure = err;
            return;
        };
        if (!entry.found_existing) entry.value_ptr.* = 0;
        entry.value_ptr.* += 1;
    }
};

const DefNote = struct {
    pass: *Pass,
    failure: ?Allocator.Error = null,

    fn note(self: *DefNote, local: LocalId) void {
        if (self.failure != null) return;
        self.pass.pinDefinition(local) catch |err| {
            self.failure = err;
        };
    }
};

test {
    std.testing.refAllDecls(@This());
}
