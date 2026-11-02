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
//! parameter qualifies when no live statement reads it and every write to it
//! is an explicit `set_local`: the parameter is removed from its join, and
//! each of its writes is deleted by routing the write's incoming edges to the
//! write's successor. A pure definition whose target nothing live reads is
//! dead, and its reads are not live: a join body that binds an unread
//! variable's value to a local nothing uses still leaves the variable unread.
//! Every round in a procedure with join parameters deletes the dead
//! definitions, whether or not a parameter qualifies, since a dead alias is
//! still a read ARC keeps its value alive for. A procedure without join
//! parameters is left unchanged.
//!
//! Deleting a write can leave the written value unread. When that value is
//! defined once, by a statement that only computes it from its operands, the
//! definition is deleted too, and so on back through its operands: a write's
//! value is often a fresh read of the variable's current value, and that read,
//! left after a consuming call, would keep the call's argument shared just as
//! the write did. A written value that is itself a join parameter qualifies in
//! the same round, so a chain of nested joins is pruned in one walk of the
//! body; the pass repeats only until a round changes nothing.
//!
//! A parameter is never removed when anything reads it outside an operand
//! position: a join's retained or maybe-uninitialized lists. A procedure argument that is also a join parameter is defined by
//! the call as well as by its writes, so it is never removed either.

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

/// One `set_local`, chained to the next write of its target.
const Write = struct { stmt: CFStmtId, next: u32 };
const no_write = std.math.maxInt(u32);

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
    /// Locals read somewhere other than an operand position.
    observed: LocalSet,
    /// Each defined local's defining statement when it has exactly one
    /// definition and that statement is pure, else null.
    defs: collections.DenseMap(LocalId, ?CFStmtId),
    /// Pure definitions whose targets nothing live reads.
    dead: std.ArrayList(CFStmtId) = .empty,
    /// Locals that lost a read to a deleted statement, one entry per read.
    orphans: std.ArrayList(LocalId) = .empty,
    /// Parameters this round removes.
    pruned: LocalSet,
    /// Parameters that qualify and whose writes are not deleted yet.
    qualified: std.ArrayList(LocalId) = .empty,
    /// The first of each parameter's writes in `writes`.
    first_write: collections.DenseMap(LocalId, u32),
    /// Every reachable `set_local` of a parameter, chained by target.
    writes: std.ArrayList(Write) = .empty,
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
            .observed = LocalSet.init(allocator),
            .defs = collections.DenseMap(LocalId, ?CFStmtId).init(allocator),
            .pruned = LocalSet.init(allocator),
            .first_write = collections.DenseMap(LocalId, u32).init(allocator),
            .deleted = collections.DenseMap(CFStmtId, CFStmtId).init(allocator),
        };
    }

    fn deinit(self: *Pass) void {
        self.stmts.deinit(self.allocator);
        self.reads.deinit();
        self.params.deinit();
        self.pinned.deinit();
        self.observed.deinit();
        self.defs.deinit();
        self.orphans.deinit(self.allocator);
        self.dead.deinit(self.allocator);
        self.pruned.deinit();
        self.qualified.deinit(self.allocator);
        self.first_write.deinit();
        self.writes.deinit(self.allocator);
        self.deleted.deinit();
    }

    /// Prune every parameter that qualifies now and delete every dead
    /// definition; report whether anything changed.
    fn round(self: *Pass) Allocator.Error!bool {
        self.stmts.clearRetainingCapacity();
        self.reads.clearRetainingCapacity();
        self.params.clearRetainingCapacity();
        self.pinned.clearRetainingCapacity();
        self.observed.clearRetainingCapacity();
        self.defs.clearRetainingCapacity();
        self.orphans.clearRetainingCapacity();
        self.dead.clearRetainingCapacity();
        self.pruned.clearRetainingCapacity();
        self.qualified.clearRetainingCapacity();
        self.first_write.clearRetainingCapacity();
        self.writes.clearRetainingCapacity();
        self.deleted.clearRetainingCapacity();

        const store = self.store;
        const proc = store.getProcSpecImmediate(self.proc_id);
        const body = proc.body orelse return false;

        const args = store.getLocalSpan(proc.args);
        for (0..args.len) |index| try self.pinDefinition(GuardedList.at(args, index));

        var walk = try BodyClone.ReachableStmts.initWithAllocator(store, body, self.allocator);
        defer walk.deinit();
        while (try walk.next()) |stmt_id| {
            try self.stmts.append(self.allocator, stmt_id);
            const stmt = store.getCFStmt(stmt_id);
            try BodyClone.tryForEachStmtRead(store, stmt, &self.reads, noteRead);
            switch (stmt) {
                .join => |join| {
                    const params = store.getLocalSpan(join.params);
                    for (0..params.len) |index| {
                        const param = GuardedList.at(params, index);
                        try self.params.put(param, {});
                    }
                    const extra = store.stmtRowOrDefault(join.extra);
                    try self.pinSpan(extra.retained);
                    try self.pinSpan(extra.maybe_uninitialized_params);
                    try self.pinSpan(extra.maybe_uninitialized_conditions);
                },
                .set_local => {},
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
                .dyn,
                .store_struct,
                .store_tag,
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
                .jump,
                .ret,
                .crash,
                => try BodyClone.tryForEachStmtDef(store, stmt, self, Pass.pinDefinition),
            }
        }

        if (self.params.count() == 0) return false;
        for (self.stmts.items) |stmt_id| {
            var defs: DefCollector = .{ .pass = self, .stmt = stmt_id };
            try BodyClone.tryForEachStmtDef(store, store.getCFStmt(stmt_id), &defs, DefCollector.note);
        }
        try self.markDeadDefs();

        for (self.stmts.items) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (stmt != .set_local) continue;
            const first = try self.first_write.getOrPut(stmt.set_local.target);
            try self.writes.append(self.allocator, .{ .stmt = stmt_id, .next = if (first.found_existing) first.value_ptr.* else no_write });
            first.value_ptr.* = @intCast(self.writes.items.len - 1);
        }

        var params = self.params.iterator();
        while (params.next()) |entry| {
            if ((self.reads.get(entry.key_ptr.*) orelse 0) == 0) try self.qualify(entry.key_ptr.*);
        }
        if (self.qualified.items.len == 0 and self.dead.items.len == 0) return false;

        for (self.dead.items) |def_id| try self.deleted.put(def_id, pureNext(store.getCFStmt(def_id)).?);
        try self.pruneQualified();
        for (self.stmts.items) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .join) try self.pruneJoinParams(stmt_id, stmt.join.params);
        }

        for (self.stmts.items) |stmt_id| {
            if (self.deleted.contains(stmt_id)) continue;
            BodyClone.redirectSuccessors(store, stmt_id, self, resolve);
        }
        store.getProcSpecPtr(self.proc_id).body = resolve(self, body);
        return true;
    }

    /// Collect every pure definition nothing live reads, withdrawing its own
    /// reads so the definitions it alone fed are dead in turn.
    fn markDeadDefs(self: *Pass) Allocator.Error!void {
        var defs = self.defs.iterator();
        while (defs.next()) |entry| {
            if ((self.reads.get(entry.key_ptr.*) orelse 0) == 0) try self.markDead(entry.key_ptr.*);
        }
        while (self.orphans.pop()) |local| {
            const reads = self.reads.getPtr(local).?;
            reads.* -= 1;
            if (reads.* == 0) try self.markDead(local);
        }
    }

    fn markDead(self: *Pass, local: LocalId) Allocator.Error!void {
        if (self.observed.contains(local) or self.params.contains(local)) return;
        const def_id = (self.defs.get(local) orelse return) orelse return;
        try self.dead.append(self.allocator, def_id);
        try BodyClone.tryForEachStmtRead(self.store, self.store.getCFStmt(def_id), self, Pass.noteOrphan);
    }

    /// Record `param` for removal unless a read outside an operand position
    /// or a definition other than a write keeps it.
    fn qualify(self: *Pass, param: LocalId) Allocator.Error!void {
        if (self.pinned.contains(param)) return;
        if ((try self.pruned.getOrPut(param)).found_existing) return;
        try self.qualified.append(self.allocator, param);
    }

    /// Delete the writes of every qualified parameter, and whatever that
    /// leaves unread: a pure definition, followed through its own operands,
    /// or a parameter, whose writes go the same way.
    fn pruneQualified(self: *Pass) Allocator.Error!void {
        const store = self.store;
        while (true) {
            if (self.qualified.pop()) |param| {
                var cursor = self.first_write.get(param) orelse no_write;
                while (cursor != no_write) : (cursor = self.writes.items[cursor].next) {
                    const write = self.writes.items[cursor].stmt;
                    const stmt = store.getCFStmt(write).set_local;
                    try self.deleted.put(write, stmt.next);
                    try self.orphans.append(self.allocator, stmt.value);
                }
                continue;
            }
            const local = self.orphans.pop() orelse return;
            const reads = self.reads.getPtr(local).?;
            reads.* -= 1;
            if (reads.* != 0) continue;
            if (self.params.contains(local)) {
                try self.qualify(local);
                continue;
            }
            if (self.observed.contains(local)) continue;
            const def_id = (self.defs.get(local) orelse continue) orelse continue;
            const def = store.getCFStmt(def_id);
            try self.deleted.put(def_id, pureNext(def).?);
            try BodyClone.tryForEachStmtRead(store, def, self, Pass.noteOrphan);
        }
    }

    fn noteOrphan(self: *Pass, local: LocalId) Allocator.Error!void {
        try self.orphans.append(self.allocator, local);
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
        for (0..locals.len) |index| {
            const local = GuardedList.at(locals, index);
            try self.pinned.put(local, {});
            try self.observed.put(local, {});
        }
    }

    fn pinDefinition(self: *Pass, local: LocalId) Allocator.Error!void {
        try self.pinned.put(local, {});
    }
};

const DefCollector = struct {
    pass: *Pass,
    stmt: CFStmtId,

    fn note(self: *DefCollector, local: LocalId) Allocator.Error!void {
        const entry = try self.pass.defs.getOrPut(local);
        if (entry.found_existing) {
            entry.value_ptr.* = null;
        } else {
            entry.value_ptr.* = if (pureNext(self.pass.store.getCFStmt(self.stmt)) != null) self.stmt else null;
        }
    }
};

/// The successor of a statement that only computes its target from its
/// operands, with no effect, failure, or divergence; null for any other
/// statement.
fn pureNext(stmt: LIR.CFStmt) ?CFStmtId {
    return switch (stmt) {
        .assign_ref => |assign| assign.next,
        .assign_literal => |assign| assign.next,
        .assign_list => |assign| assign.next,
        .assign_struct => |assign| assign.next,
        .assign_tag => |assign| assign.next,
        .init_uninitialized,
        .set_local,
        .dyn,
        .join,
        .assign_call,
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_low_level,
        .store_struct,
        .store_tag,
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
        .jump,
        .ret,
        .crash,
        => null,
    };
}

fn noteRead(reads: *collections.DenseMap(LocalId, u32), local: LocalId) Allocator.Error!void {
    const entry = try reads.getOrPut(local);
    if (!entry.found_existing) entry.value_ptr.* = 0;
    entry.value_ptr.* += 1;
}

test {
    std.testing.refAllDecls(@This());
}
