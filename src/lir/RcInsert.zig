//! Insert explicit ownership drops into statement-only LIR.
//!
//! Backends must only follow explicit `incref`/`decref`/`free` statements.
//! This pass computes last-use drops from the existing statement graph and
//! rewrites each proc body to include the required `decref` statements.
//!
//! Ownership boundary:
//! - this pass is the only non-builtin stage allowed to turn ownership facts
//!   into explicit RC statements
//! - later backends/interpreters must execute the resulting statements
//!   mechanically rather than inferring more ownership behavior

const std = @import("std");
const builtin = @import("builtin");
const layout = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const CFStmt = LIR.CFStmt;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const LirProcSpecId = LIR.LirProcSpecId;
const LoopContinueVisitKey = struct {
    stmt_id: CFStmtId,
    innermost_for: ?CFStmtId,
};

pub fn run(
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout.Store,
) Allocator.Error!void {
    const proc_count = store.getProcSpecs().len;
    var proc_index: usize = 0;
    while (proc_index < proc_count) : (proc_index += 1) {
        const proc_id: LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        var pass = try ProcPass.init(allocator, store, layouts, proc_id);
        defer pass.deinit();
        try pass.computeLiveness();
        try pass.rewriteProcBody();
    }
}

const ProcPass = struct {
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout.Store,
    proc_id: LirProcSpecId,
    proc: LIR.LirProcSpec,
    local_count: usize,
    reachable_stmt_ids: []CFStmtId,
    stmt_slots: std.AutoHashMap(u32, usize),
    live_backing: []?LocalId,
    exact_owner: []?LocalId,
    storage_locals: std.DynamicBitSetUnmanaged,
    owned_locals: std.DynamicBitSetUnmanaged,
    live_in_direct: []std.DynamicBitSetUnmanaged,
    live_out_direct: []std.DynamicBitSetUnmanaged,
    live_in: []std.DynamicBitSetUnmanaged,
    live_out: []std.DynamicBitSetUnmanaged,
    rewritten_stmt_ids: std.AutoHashMap(u32, CFStmtId),
    loop_continue_targets: std.AutoHashMap(u32, CFStmtId),
    join_params_by_id: std.AutoHashMap(u32, LIR.LocalSpan),
    join_bodies_by_id: std.AutoHashMap(u32, CFStmtId),
    join_params_by_body: std.AutoHashMap(u32, LIR.LocalSpan),

    fn init(
        allocator: Allocator,
        store: *LirStore,
        layouts: *const layout.Store,
        proc_id: LirProcSpecId,
    ) Allocator.Error!ProcPass {
        const proc = store.getProcSpec(proc_id);
        const local_count = store.locals.items.len;
        const reachable_stmt_ids = if (proc.body) |body|
            try collectReachableStmtIds(allocator, store, body)
        else
            try allocator.alloc(CFStmtId, 0);
        errdefer allocator.free(reachable_stmt_ids);

        var stmt_slots = std.AutoHashMap(u32, usize).init(allocator);
        errdefer stmt_slots.deinit();
        for (reachable_stmt_ids, 0..) |stmt_id, slot| {
            try stmt_slots.put(@intFromEnum(stmt_id), slot);
        }

        const stmt_count = reachable_stmt_ids.len;
        const live_backing = try allocator.alloc(?LocalId, local_count);
        errdefer allocator.free(live_backing);
        @memset(live_backing, null);

        const exact_owner = try allocator.alloc(?LocalId, local_count);
        errdefer allocator.free(exact_owner);
        @memset(exact_owner, null);

        const live_in_direct = try allocator.alloc(std.DynamicBitSetUnmanaged, stmt_count);
        errdefer allocator.free(live_in_direct);
        const live_out_direct = try allocator.alloc(std.DynamicBitSetUnmanaged, stmt_count);
        errdefer allocator.free(live_out_direct);
        const live_in = try allocator.alloc(std.DynamicBitSetUnmanaged, stmt_count);
        errdefer allocator.free(live_in);
        const live_out = try allocator.alloc(std.DynamicBitSetUnmanaged, stmt_count);
        errdefer allocator.free(live_out);

        for (live_in_direct) |*in_direct_bits| {
            in_direct_bits.* = try std.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
            errdefer in_direct_bits.deinit(allocator);
        }
        for (live_out_direct) |*out_direct_bits| {
            out_direct_bits.* = try std.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
            errdefer out_direct_bits.deinit(allocator);
        }
        for (live_in, live_out) |*in_bits, *out_bits| {
            in_bits.* = try std.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
            errdefer in_bits.deinit(allocator);
            out_bits.* = try std.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
            errdefer out_bits.deinit(allocator);
        }

        var pass = ProcPass{
            .allocator = allocator,
            .store = store,
            .layouts = layouts,
            .proc_id = proc_id,
            .proc = proc,
            .local_count = local_count,
            .reachable_stmt_ids = reachable_stmt_ids,
            .stmt_slots = stmt_slots,
            .live_backing = live_backing,
            .exact_owner = exact_owner,
            .storage_locals = try std.DynamicBitSetUnmanaged.initEmpty(allocator, local_count),
            .owned_locals = try std.DynamicBitSetUnmanaged.initEmpty(allocator, local_count),
            .live_in_direct = live_in_direct,
            .live_out_direct = live_out_direct,
            .live_in = live_in,
            .live_out = live_out,
            .rewritten_stmt_ids = std.AutoHashMap(u32, CFStmtId).init(allocator),
            .loop_continue_targets = std.AutoHashMap(u32, CFStmtId).init(allocator),
            .join_params_by_id = std.AutoHashMap(u32, LIR.LocalSpan).init(allocator),
            .join_bodies_by_id = std.AutoHashMap(u32, CFStmtId).init(allocator),
            .join_params_by_body = std.AutoHashMap(u32, LIR.LocalSpan).init(allocator),
        };
        errdefer pass.storage_locals.deinit(allocator);
        errdefer pass.owned_locals.deinit(allocator);
        errdefer pass.rewritten_stmt_ids.deinit();
        errdefer pass.loop_continue_targets.deinit();
        errdefer pass.join_params_by_id.deinit();
        errdefer pass.join_bodies_by_id.deinit();
        errdefer pass.join_params_by_body.deinit();

        pass.computeOwnedLocals();
        pass.computeProvenanceOwners();
        if (proc.body) |body| {
            try pass.collectLoopContinueTargets(body, &.{});
        }
        return pass;
    }

    fn deinit(self: *ProcPass) void {
        self.allocator.free(self.reachable_stmt_ids);
        self.stmt_slots.deinit();
        self.allocator.free(self.live_backing);
        self.allocator.free(self.exact_owner);
        self.storage_locals.deinit(self.allocator);
        self.owned_locals.deinit(self.allocator);
        self.rewritten_stmt_ids.deinit();
        self.loop_continue_targets.deinit();
        self.join_params_by_id.deinit();
        self.join_bodies_by_id.deinit();
        self.join_params_by_body.deinit();
        for (self.live_in_direct) |*in_bits| {
            in_bits.deinit(self.allocator);
        }
        self.allocator.free(self.live_in_direct);
        for (self.live_out_direct) |*out_bits| {
            out_bits.deinit(self.allocator);
        }
        self.allocator.free(self.live_out_direct);
        for (self.live_in, self.live_out) |*in_bits, *out_bits| {
            in_bits.deinit(self.allocator);
            out_bits.deinit(self.allocator);
        }
        self.allocator.free(self.live_in);
        self.allocator.free(self.live_out);
    }

    fn computeOwnedLocals(self: *ProcPass) void {
        for (self.store.cf_stmts.items) |stmt| {
            switch (stmt) {
                .set_local => |assign| self.storage_locals.set(@intFromEnum(assign.target)),
                else => {},
            }
        }

        var local_idx: usize = 0;
        while (local_idx < self.local_count) : (local_idx += 1) {
            if (!self.storage_locals.isSet(local_idx)) continue;
            const local: LocalId = @enumFromInt(@as(u32, @intCast(local_idx)));
            self.markOwnedLocal(local);
        }

        for (self.store.getLocalSpan(self.proc.owned_params)) |param| {
            self.markOwnedLocal(param);
        }

        for (self.store.cf_stmts.items) |stmt| {
            switch (stmt) {
                .assign_ref => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_literal => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_call => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_call_indirect => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_low_level => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_list => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_struct => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_tag => |assign| self.markOwnedFresh(assign.target, assign.result),
                .for_list => |for_stmt| self.markOwnedFresh(for_stmt.elem, for_stmt.elem_result),
                .join => |join| {
                    _ = self.join_params_by_id.put(@intFromEnum(join.id), join.params) catch unreachable;
                    _ = self.join_bodies_by_id.put(@intFromEnum(join.id), join.body) catch unreachable;
                    _ = self.join_params_by_body.put(@intFromEnum(join.body), join.params) catch unreachable;
                },
                else => {},
            }
        }

        self.computeJoinOwnedLocals();
    }

    fn computeJoinOwnedLocals(self: *ProcPass) void {
        for (self.reachable_stmt_ids) |stmt_id| {
            const join = switch (self.store.getCFStmt(stmt_id)) {
                .join => |join| join,
                else => continue,
            };

            for (self.store.getLocalSpan(join.params)) |param| {
                const layout_idx = self.store.getLocal(param).layout_idx;
                if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) continue;
                self.owned_locals.set(@intFromEnum(param));
            }
        }
    }

    fn markOwnedFresh(self: *ProcPass, target: LocalId, result: LIR.ResultSemantics) void {
        if (result != .fresh) return;
        const local_idx = @intFromEnum(target);
        if (self.storage_locals.isSet(local_idx)) return;
        const layout_idx = self.store.getLocal(target).layout_idx;
        if (self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) {
            self.owned_locals.set(local_idx);
        }
    }

    fn markOwnedLocal(self: *ProcPass, target: LocalId) void {
        const local_idx = @intFromEnum(target);
        const layout_idx = self.store.getLocal(target).layout_idx;
        if (self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) {
            self.owned_locals.set(local_idx);
        }
    }

    fn computeProvenanceOwners(self: *ProcPass) void {
        for (self.reachable_stmt_ids) |stmt_id| {
            switch (self.store.getCFStmt(stmt_id)) {
                .assign_ref => |assign| self.recordAssignRefOwner(assign.target, assign.op, assign.result),
                .assign_call => |assign| self.recordWholeValueOwner(assign.target, assign.result),
                .assign_call_indirect => |assign| self.recordWholeValueOwner(assign.target, assign.result),
                .assign_low_level => |assign| self.recordWholeValueOwner(assign.target, assign.result),
                .assign_literal => |assign| self.clearOwnerFacts(assign.target),
                .assign_symbol => |assign| self.clearOwnerFacts(assign.target),
                .assign_list => |assign| self.clearOwnerFacts(assign.target),
                .assign_struct => |assign| self.clearOwnerFacts(assign.target),
                .assign_tag => |assign| self.clearOwnerFacts(assign.target),
                .set_local => |assign| self.clearOwnerFacts(assign.target),
                .for_list => |loop_stmt| {
                    if (loop_stmt.elem_result == .fresh) {
                        self.clearOwnerFacts(loop_stmt.elem);
                    } else {
                        self.recordProjectedOwner(loop_stmt.elem, loop_stmt.elem_result);
                    }
                },
                else => {},
            }
        }
        self.computeJoinParamProvenance();
    }

    fn computeJoinParamProvenance(self: *ProcPass) void {
        var changed = true;
        while (changed) {
            changed = false;
            for (self.reachable_stmt_ids) |stmt_id| {
                const join = switch (self.store.getCFStmt(stmt_id)) {
                    .join => |join| join,
                    else => continue,
                };

                const params = self.store.getLocalSpan(join.params);
                if (params.len == 0) continue;

                var live_candidates = self.allocator.alloc(?LocalId, params.len) catch unreachable;
                defer self.allocator.free(live_candidates);
                @memset(live_candidates, null);

                var exact_candidates = self.allocator.alloc(?LocalId, params.len) catch unreachable;
                defer self.allocator.free(exact_candidates);
                @memset(exact_candidates, null);

                var incoming_count: usize = 0;
                for (self.reachable_stmt_ids) |incoming_stmt_id| {
                    const jump = switch (self.store.getCFStmt(incoming_stmt_id)) {
                        .jump => |jump| if (jump.target == join.id) jump else continue,
                        else => continue,
                    };

                    const args = self.store.getLocalSpan(jump.args);
                    if (args.len != params.len) continue;
                    incoming_count += 1;

                    for (args, 0..) |arg_local, i| {
                        const live_owner = self.liveOwnerForLocal(arg_local);
                        const exact_owner = self.exactOwnerForLocal(arg_local);
                        if (incoming_count == 1) {
                            live_candidates[i] = live_owner;
                            exact_candidates[i] = exact_owner;
                        } else {
                            if (live_candidates[i] != live_owner) live_candidates[i] = null;
                            if (exact_candidates[i] != exact_owner) exact_candidates[i] = null;
                        }
                    }
                }

                if (incoming_count == 0) continue;

                for (params, 0..) |param, i| {
                    const param_idx = @intFromEnum(param);
                    if (self.live_backing[param_idx] != live_candidates[i]) {
                        self.live_backing[param_idx] = live_candidates[i];
                        changed = true;
                    }
                    if (self.exact_owner[param_idx] != exact_candidates[i]) {
                        self.exact_owner[param_idx] = exact_candidates[i];
                        changed = true;
                    }
                }
            }
        }
    }

    fn explicitOwnerForLocal(self: *const ProcPass, local: LocalId) ?LocalId {
        return self.exactOwnerForLocal(local);
    }

    fn exactOwnerForLocal(self: *const ProcPass, local: LocalId) ?LocalId {
        var current = local;
        var hops_remaining: usize = self.local_count;
        while (hops_remaining > 0) : (hops_remaining -= 1) {
            if (self.exact_owner[@intFromEnum(current)]) |owner| {
                if (owner == current) break;
                current = owner;
                continue;
            }

            const current_idx = @intFromEnum(current);
            if (self.owned_locals.isSet(current_idx)) {
                const layout_idx = self.store.getLocal(current).layout_idx;
                if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) return null;
                return current;
            }

            return null;
        }

        unreachable;
    }

    fn liveOwnerForLocal(self: *const ProcPass, local: LocalId) ?LocalId {
        var current = local;
        var hops_remaining: usize = self.local_count;
        while (hops_remaining > 0) : (hops_remaining -= 1) {
            if (self.live_backing[@intFromEnum(current)]) |owner| {
                if (owner == current) break;
                current = owner;
                continue;
            }

            return self.exactOwnerForLocal(current);
        }

        unreachable;
    }

    fn clearOwnerFacts(self: *ProcPass, target: LocalId) void {
        self.live_backing[@intFromEnum(target)] = null;
        self.exact_owner[@intFromEnum(target)] = null;
    }

    fn backingFromResult(result: LIR.ResultSemantics) ?LocalId {
        return switch (result) {
            .alias_of => |alias| alias.owner,
            .borrow_of => |borrow| borrow.owner,
            .fresh => null,
        };
    }

    fn recordWholeValueOwner(self: *ProcPass, target: LocalId, result: LIR.ResultSemantics) void {
        const owner = backingFromResult(result);
        self.live_backing[@intFromEnum(target)] = owner;
        self.exact_owner[@intFromEnum(target)] = owner;
    }

    fn recordProjectedOwner(self: *ProcPass, target: LocalId, result: LIR.ResultSemantics) void {
        self.live_backing[@intFromEnum(target)] = backingFromResult(result);
        self.exact_owner[@intFromEnum(target)] = null;
    }

    fn recordAssignRefOwner(
        self: *ProcPass,
        target: LocalId,
        op: LIR.RefOp,
        result: LIR.ResultSemantics,
    ) void {
        if (result == .fresh) {
            self.clearOwnerFacts(target);
            return;
        }

        switch (op) {
            .local,
            .list_reinterpret,
            .nominal,
            => self.recordWholeValueOwner(target, result),

            .discriminant,
            .field,
            .tag_payload,
            .tag_payload_struct,
            => self.recordProjectedOwner(target, result),
        }
    }

    fn computeLiveness(self: *ProcPass) Allocator.Error!void {
        var changed = true;
        while (changed) {
            changed = false;
            var stmt_idx = self.reachable_stmt_ids.len;
            while (stmt_idx > 0) {
                stmt_idx -= 1;
                const id = self.reachable_stmt_ids[stmt_idx];

                var new_out_direct = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
                defer new_out_direct.deinit(self.allocator);
                try self.computeLiveOutDirect(id, &new_out_direct);

                var new_out = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
                defer new_out.deinit(self.allocator);
                try self.computeLiveOut(id, &new_out);

                var new_in_direct = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
                defer new_in_direct.deinit(self.allocator);
                var new_in = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
                defer new_in.deinit(self.allocator);
                try self.transfer(id, &new_out_direct, &new_in_direct, &new_in);

                if (!bitsetsEqual(self.live_out_direct[stmt_idx], new_out_direct)) {
                    self.live_out_direct[stmt_idx].unsetAll();
                    self.live_out_direct[stmt_idx].setUnion(new_out_direct);
                    changed = true;
                }

                if (!bitsetsEqual(self.live_out[stmt_idx], new_out)) {
                    self.live_out[stmt_idx].unsetAll();
                    self.live_out[stmt_idx].setUnion(new_out);
                    changed = true;
                }
                if (!bitsetsEqual(self.live_in_direct[stmt_idx], new_in_direct)) {
                    self.live_in_direct[stmt_idx].unsetAll();
                    self.live_in_direct[stmt_idx].setUnion(new_in_direct);
                    changed = true;
                }
                if (!bitsetsEqual(self.live_in[stmt_idx], new_in)) {
                    self.live_in[stmt_idx].unsetAll();
                    self.live_in[stmt_idx].setUnion(new_in);
                    changed = true;
                }
            }
        }
    }

    fn computeLiveOutDirect(self: *ProcPass, stmt_id: CFStmtId, out: *std.DynamicBitSetUnmanaged) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| self.unionLiveInDirectFor(assign.next, out),
            .assign_ref => |assign| self.unionLiveInDirectFor(assign.next, out),
            .assign_literal => |assign| self.unionLiveInDirectFor(assign.next, out),
            .assign_call => |assign| self.unionLiveInDirectFor(assign.next, out),
            .assign_call_indirect => |assign| self.unionLiveInDirectFor(assign.next, out),
            .assign_low_level => |assign| self.unionLiveInDirectFor(assign.next, out),
            .assign_list => |assign| self.unionLiveInDirectFor(assign.next, out),
            .assign_struct => |assign| self.unionLiveInDirectFor(assign.next, out),
            .assign_tag => |assign| self.unionLiveInDirectFor(assign.next, out),
            .set_local => |assign| self.unionLiveInDirectFor(assign.next, out),
            .debug => |debug_stmt| self.unionLiveInDirectFor(debug_stmt.next, out),
            .expect => |expect_stmt| self.unionLiveInDirectFor(expect_stmt.next, out),
            .incref => |inc| self.unionLiveInDirectFor(inc.next, out),
            .decref => |dec| self.unionLiveInDirectFor(dec.next, out),
            .free => |free_stmt| self.unionLiveInDirectFor(free_stmt.next, out),
            .switch_stmt => |sw| {
                for (self.store.getCFSwitchBranches(sw.branches)) |branch| {
                    self.unionLiveInDirectFor(branch.body, out);
                }
                self.unionLiveInDirectFor(sw.default_branch, out);
            },
            .borrow_scope => |scope| {
                self.unionLiveInDirectFor(scope.body, out);
                self.unionLiveInDirectFor(scope.remainder, out);
            },
            .for_list => |for_stmt| {
                self.unionLiveInDirectFor(for_stmt.body, out);
                self.unionLiveInDirectFor(for_stmt.next, out);
            },
            .join => |join| {
                self.unionLiveInDirectFor(join.body, out);
                self.unionLiveInDirectFor(join.remainder, out);
            },
            .loop_continue => {
                const target = self.loop_continue_targets.get(@intFromEnum(stmt_id)) orelse return;
                self.unionLiveInDirectFor(target, out);
            },
            .jump => |jump| {
                const target = self.join_bodies_by_id.get(@intFromEnum(jump.target)) orelse return;
                self.unionLiveInDirectFor(target, out);
            },
            .scope_exit, .ret, .runtime_error, .crash => {},
        }
    }

    fn computeLiveOut(self: *ProcPass, stmt_id: CFStmtId, out: *std.DynamicBitSetUnmanaged) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| self.unionLiveInFor(assign.next, out),
            .assign_ref => |assign| self.unionLiveInFor(assign.next, out),
            .assign_literal => |assign| self.unionLiveInFor(assign.next, out),
            .assign_call => |assign| self.unionLiveInFor(assign.next, out),
            .assign_call_indirect => |assign| self.unionLiveInFor(assign.next, out),
            .assign_low_level => |assign| self.unionLiveInFor(assign.next, out),
            .assign_list => |assign| self.unionLiveInFor(assign.next, out),
            .assign_struct => |assign| self.unionLiveInFor(assign.next, out),
            .assign_tag => |assign| self.unionLiveInFor(assign.next, out),
            .set_local => |assign| self.unionLiveInFor(assign.next, out),
            .debug => |debug_stmt| self.unionLiveInFor(debug_stmt.next, out),
            .expect => |expect_stmt| self.unionLiveInFor(expect_stmt.next, out),
            .incref => |inc| self.unionLiveInFor(inc.next, out),
            .decref => |dec| self.unionLiveInFor(dec.next, out),
            .free => |free_stmt| self.unionLiveInFor(free_stmt.next, out),
            .switch_stmt => |sw| {
                for (self.store.getCFSwitchBranches(sw.branches)) |branch| {
                    self.unionLiveInFor(branch.body, out);
                }
                self.unionLiveInFor(sw.default_branch, out);
            },
            .borrow_scope => |scope| {
                self.unionLiveInFor(scope.body, out);
                self.unionLiveInFor(scope.remainder, out);
            },
            .for_list => |for_stmt| {
                self.unionLiveInFor(for_stmt.body, out);
                self.unionLiveInFor(for_stmt.next, out);
            },
            .join => |join| {
                self.unionLiveInFor(join.body, out);
                self.unionLiveInFor(join.remainder, out);
            },
            .loop_continue => {
                const target = self.loop_continue_targets.get(@intFromEnum(stmt_id)) orelse return;
                self.unionLiveInFor(target, out);
            },
            .jump => |jump| {
                const target = self.join_bodies_by_id.get(@intFromEnum(jump.target)) orelse return;
                self.unionLiveInFor(target, out);
            },
            .scope_exit, .ret, .runtime_error, .crash => {},
        }
    }

    fn unionLiveInFor(self: *const ProcPass, stmt_id: CFStmtId, out: *std.DynamicBitSetUnmanaged) void {
        const slot = self.stmtSlot(stmt_id) orelse return;
        out.setUnion(self.live_in[slot]);
    }

    fn unionLiveInDirectFor(self: *const ProcPass, stmt_id: CFStmtId, out: *std.DynamicBitSetUnmanaged) void {
        const slot = self.stmtSlot(stmt_id) orelse return;
        out.setUnion(self.live_in_direct[slot]);
    }

    fn stmtSlot(self: *const ProcPass, stmt_id: CFStmtId) ?usize {
        return self.stmt_slots.get(@intFromEnum(stmt_id));
    }

    fn transfer(
        self: *ProcPass,
        stmt_id: CFStmtId,
        out_direct: *const std.DynamicBitSetUnmanaged,
        in_direct: *std.DynamicBitSetUnmanaged,
        in: *std.DynamicBitSetUnmanaged,
    ) Allocator.Error!void {
        in_direct.setUnion(out_direct.*);

        var defs = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer defs.deinit(self.allocator);
        var uses = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer uses.deinit(self.allocator);
        var ownership_passed = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer ownership_passed.deinit(self.allocator);

        self.collectDefsUses(stmt_id, &defs, &uses, &ownership_passed);
        setDifferenceInPlace(in_direct, defs);
        setDifferenceInPlace(in_direct, ownership_passed);
        in_direct.setUnion(uses);
        self.addJoinBodyOwnedParams(stmt_id, in_direct);
        in.setUnion(in_direct.*);
        self.expandLiveOwners(in);
    }

    fn addJoinBodyOwnedParams(
        self: *const ProcPass,
        stmt_id: CFStmtId,
        bits: *std.DynamicBitSetUnmanaged,
    ) void {
        const params_span = self.join_params_by_body.get(@intFromEnum(stmt_id)) orelse return;
        for (self.store.getLocalSpan(params_span)) |param| {
            if (!self.localCanOwnRefcount(param)) continue;
            const layout_idx = self.store.getLocal(param).layout_idx;
            if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) continue;
            bits.set(@intFromEnum(param));
        }
        self.expandLiveOwners(bits);
    }

    fn expandLiveOwners(self: *const ProcPass, bits: *std.DynamicBitSetUnmanaged) void {
        var changed = true;
        while (changed) {
            changed = false;
            var local_idx: usize = 0;
            while (local_idx < bits.bit_length) : (local_idx += 1) {
                if (!bits.isSet(local_idx)) continue;
                const owner = self.live_backing[local_idx] orelse continue;
                const owner_idx = @intFromEnum(owner);
                if (bits.isSet(owner_idx)) continue;
                bits.set(owner_idx);
                changed = true;
            }
        }
    }

    fn collectDefsUses(
        self: *ProcPass,
        stmt_id: CFStmtId,
        defs: *std.DynamicBitSetUnmanaged,
        uses: *std.DynamicBitSetUnmanaged,
        ownership_passed: *std.DynamicBitSetUnmanaged,
    ) void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| defs.set(@intFromEnum(assign.target)),
            .assign_ref => |assign| {
                defs.set(@intFromEnum(assign.target));
                markRefOpUses(assign.op, uses);
                self.markSpanOwnershipPassed(assign.ownership.consumed_owned_inputs, ownership_passed);
            },
            .assign_literal => |assign| defs.set(@intFromEnum(assign.target)),
            .assign_call => |assign| {
                defs.set(@intFromEnum(assign.target));
                self.markSpanUses(assign.args, uses);
                self.markOwnedCallArgsPassed(assign.proc, assign.args, ownership_passed);
                self.markOwnedCallArgsConsumed(assign.proc, assign.args, defs);
            },
            .assign_call_indirect => |assign| {
                defs.set(@intFromEnum(assign.target));
                uses.set(@intFromEnum(assign.closure));
                self.markSpanUses(assign.args, uses);
            },
            .assign_low_level => |assign| {
                defs.set(@intFromEnum(assign.target));
                self.markSpanUses(assign.args, uses);
                self.markSpanOwnershipPassed(assign.ownership.consumed_owned_inputs, ownership_passed);
                self.markSpanOwnershipConsumed(assign.ownership.consumed_owned_inputs, defs);
            },
            .assign_list => |assign| {
                defs.set(@intFromEnum(assign.target));
                self.markSpanUses(assign.elems, uses);
                self.markSpanOwnershipPassed(assign.ownership.consumed_owned_inputs, ownership_passed);
                self.markSpanOwnershipConsumed(assign.ownership.consumed_owned_inputs, defs);
            },
            .assign_struct => |assign| {
                defs.set(@intFromEnum(assign.target));
                self.markSpanUses(assign.fields, uses);
                self.markSpanOwnershipPassed(assign.ownership.consumed_owned_inputs, ownership_passed);
                self.markSpanOwnershipConsumed(assign.ownership.consumed_owned_inputs, defs);
            },
            .assign_tag => |assign| {
                defs.set(@intFromEnum(assign.target));
                if (assign.payload) |payload| {
                    uses.set(@intFromEnum(payload));
                }
                self.markSpanOwnershipPassed(assign.ownership.consumed_owned_inputs, ownership_passed);
                self.markSpanOwnershipConsumed(assign.ownership.consumed_owned_inputs, defs);
            },
            .set_local => |assign| {
                defs.set(@intFromEnum(assign.target));
                uses.set(@intFromEnum(assign.value));
                self.markOwnershipPassedValue(assign.value, ownership_passed);
                self.markOwnershipConsumedValue(assign.value, defs);
            },
            .debug => |debug_stmt| uses.set(@intFromEnum(debug_stmt.message)),
            .expect => |expect_stmt| uses.set(@intFromEnum(expect_stmt.condition)),
            .incref => |inc| uses.set(@intFromEnum(inc.value)),
            .decref => |dec| uses.set(@intFromEnum(dec.value)),
            .free => |free_stmt| uses.set(@intFromEnum(free_stmt.value)),
            .switch_stmt => |sw| uses.set(@intFromEnum(sw.cond)),
            .borrow_scope => {},
            .for_list => |for_stmt| {
                defs.set(@intFromEnum(for_stmt.elem));
                uses.set(@intFromEnum(for_stmt.iterable));
            },
            .join => |join| {
                for (self.store.getLocalSpan(join.params)) |param| defs.set(@intFromEnum(param));
            },
            .jump => |jump| {
                const args = self.store.getLocalSpan(jump.args);
                for (args) |arg| uses.set(@intFromEnum(arg));
                if (self.join_params_by_id.get(@intFromEnum(jump.target))) |params_span| {
                    const params = self.store.getLocalSpan(params_span);
                    if (params.len == args.len) {
                        for (args, params) |arg, param| {
                            if (self.localCanOwnRefcount(param)) {
                                self.markOwnershipPassedValue(arg, ownership_passed);
                                self.markOwnershipConsumedValue(arg, defs);
                            }
                        }
                    }
                }
            },
            .ret => |ret_stmt| uses.set(@intFromEnum(ret_stmt.value)),
            .scope_exit, .runtime_error, .crash, .loop_continue => {},
        }
    }

    fn markRefOpUses(op: LIR.RefOp, uses: *std.DynamicBitSetUnmanaged) void {
        switch (op) {
            .local => |local| uses.set(@intFromEnum(local)),
            .discriminant => |info| uses.set(@intFromEnum(info.source)),
            .field => |info| uses.set(@intFromEnum(info.source)),
            .tag_payload => |info| uses.set(@intFromEnum(info.source)),
            .tag_payload_struct => |info| uses.set(@intFromEnum(info.source)),
            .list_reinterpret => |info| uses.set(@intFromEnum(info.backing_ref)),
            .nominal => |info| uses.set(@intFromEnum(info.backing_ref)),
        }
    }

    fn markSpanUses(self: *ProcPass, span: LIR.LocalSpan, uses: *std.DynamicBitSetUnmanaged) void {
        for (self.store.getLocalSpan(span)) |local| uses.set(@intFromEnum(local));
    }

    fn markSpanOwnershipPassed(self: *ProcPass, span: LIR.LocalSpan, ownership_passed: *std.DynamicBitSetUnmanaged) void {
        for (self.store.getLocalSpan(span)) |local| {
            self.markOwnershipPassedValue(local, ownership_passed);
        }
    }

    fn markSpanOwnershipConsumed(self: *ProcPass, span: LIR.LocalSpan, defs: *std.DynamicBitSetUnmanaged) void {
        for (self.store.getLocalSpan(span)) |local| {
            self.markOwnershipConsumedValue(local, defs);
        }
    }

    fn markOwnershipPassedLocal(self: *ProcPass, local: LocalId, ownership_passed: *std.DynamicBitSetUnmanaged) void {
        if (!self.localCanOwnRefcount(local)) return;
        const layout_idx = self.store.getLocal(local).layout_idx;
        if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) return;
        ownership_passed.set(@intFromEnum(local));
    }

    fn markOwnershipPassedValue(self: *ProcPass, local: LocalId, ownership_passed: *std.DynamicBitSetUnmanaged) void {
        const owner = self.explicitOwnerForLocal(local) orelse local;
        self.markOwnershipPassedLocal(owner, ownership_passed);
    }

    fn markOwnershipConsumedValue(self: *ProcPass, local: LocalId, defs: *std.DynamicBitSetUnmanaged) void {
        const owner = self.explicitOwnerForLocal(local) orelse local;
        const layout_idx = self.store.getLocal(owner).layout_idx;
        if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) return;
        defs.set(@intFromEnum(owner));
    }

    fn markOwnedCallArgsPassed(
        self: *ProcPass,
        proc_id: LIR.LirProcSpecId,
        args_span: LIR.LocalSpan,
        ownership_passed: *std.DynamicBitSetUnmanaged,
    ) void {
        const proc = self.store.getProcSpec(proc_id);
        const args = self.store.getLocalSpan(args_span);
        const params = self.store.getLocalSpan(proc.args);
        const owned_params = self.store.getLocalSpan(proc.owned_params);

        for (owned_params) |owned_param| {
            const param_index = indexOfLocal(params, owned_param) orelse continue;
            if (param_index >= args.len) continue;
            self.markOwnershipPassedValue(args[param_index], ownership_passed);
        }
    }

    fn markOwnedCallArgsConsumed(
        self: *ProcPass,
        proc_id: LIR.LirProcSpecId,
        args_span: LIR.LocalSpan,
        defs: *std.DynamicBitSetUnmanaged,
    ) void {
        const proc = self.store.getProcSpec(proc_id);
        const args = self.store.getLocalSpan(args_span);
        const params = self.store.getLocalSpan(proc.args);
        const owned_params = self.store.getLocalSpan(proc.owned_params);

        for (owned_params) |owned_param| {
            const param_index = indexOfLocal(params, owned_param) orelse continue;
            if (param_index >= args.len) continue;
            self.markOwnershipConsumedValue(args[param_index], defs);
        }
    }

    fn localCanOwnRefcount(self: *const ProcPass, local: LocalId) bool {
        const idx = @intFromEnum(local);
        return self.owned_locals.isSet(idx);
    }

    fn indexOfLocal(locals: []const LocalId, target: LocalId) ?usize {
        for (locals, 0..) |local, i| {
            if (local == target) return i;
        }
        return null;
    }

    fn rewriteProcBody(self: *ProcPass) Allocator.Error!void {
        if (self.proc.body == null) return;
        self.rewritten_stmt_ids.clearRetainingCapacity();
        self.store.getProcSpecPtr(self.proc_id).body = try self.rewriteStmt(self.proc.body.?);
    }

    fn cloneStmtSkeleton(self: *ProcPass, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        return switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| self.store.addCFStmt(.{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = assign.next,
            } }),
            .assign_ref => |assign| self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .op = assign.op,
                .next = assign.next,
            } }),
            .assign_literal => |assign| self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = assign.next,
            } }),
            .assign_call => |assign| self.store.addCFStmt(.{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = assign.next,
            } }),
            .assign_call_indirect => |assign| self.store.addCFStmt(.{ .assign_call_indirect = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .closure = assign.closure,
                .args = assign.args,
                .capture_layout = assign.capture_layout,
                .next = assign.next,
            } }),
            .assign_low_level => |assign| self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .op = assign.op,
                .args = assign.args,
                .next = assign.next,
            } }),
            .assign_list => |assign| self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .elems = assign.elems,
                .next = assign.next,
            } }),
            .assign_struct => |assign| self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .fields = assign.fields,
                .next = assign.next,
            } }),
            .assign_tag => |assign| self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .discriminant = assign.discriminant,
                .payload = assign.payload,
                .next = assign.next,
            } }),
            .set_local => |assign| self.store.addCFStmt(.{ .set_local = .{
                .target = assign.target,
                .value = assign.value,
                .next = assign.next,
            } }),
            .debug => |debug_stmt| self.store.addCFStmt(.{ .debug = .{
                .message = debug_stmt.message,
                .next = debug_stmt.next,
            } }),
            .expect => |expect_stmt| self.store.addCFStmt(.{ .expect = .{
                .condition = expect_stmt.condition,
                .next = expect_stmt.next,
            } }),
            .incref => |inc| self.store.addCFStmt(.{ .incref = .{
                .value = inc.value,
                .count = inc.count,
                .next = inc.next,
            } }),
            .decref => |dec| self.store.addCFStmt(.{ .decref = .{
                .value = dec.value,
                .next = dec.next,
            } }),
            .free => |free_stmt| self.store.addCFStmt(.{ .free = .{
                .value = free_stmt.value,
                .next = free_stmt.next,
            } }),
            .switch_stmt => |sw| self.store.addCFStmt(.{ .switch_stmt = .{
                .cond = sw.cond,
                .branches = sw.branches,
                .default_branch = sw.default_branch,
            } }),
            .borrow_scope => |scope| self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = scope.body,
                .remainder = scope.remainder,
            } }),
            .scope_exit => |scope_exit| self.store.addCFStmt(.{ .scope_exit = .{ .id = scope_exit.id } }),
            .for_list => |for_stmt| self.store.addCFStmt(.{ .for_list = .{
                .elem = for_stmt.elem,
                .elem_result = for_stmt.elem_result,
                .elem_ownership = for_stmt.elem_ownership,
                .iterable = for_stmt.iterable,
                .iterable_elem_layout = for_stmt.iterable_elem_layout,
                .body = for_stmt.body,
                .next = for_stmt.next,
            } }),
            .loop_continue => self.store.addCFStmt(.loop_continue),
            .join => |join| self.store.addCFStmt(.{ .join = .{
                .id = join.id,
                .params = join.params,
                .body = join.body,
                .remainder = join.remainder,
            } }),
            .jump => |jump| self.store.addCFStmt(.{ .jump = .{
                .target = jump.target,
                .args = jump.args,
            } }),
            .ret => |ret_stmt| self.store.addCFStmt(.{ .ret = .{ .value = ret_stmt.value } }),
            .runtime_error => self.store.addCFStmt(.runtime_error),
            .crash => |crash| self.store.addCFStmt(.{ .crash = .{ .msg = crash.msg } }),
        };
    }

    fn rewriteStmt(self: *ProcPass, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        if (self.rewritten_stmt_ids.get(@intFromEnum(stmt_id))) |rewritten| {
            return rewritten;
        }

        const rewritten = switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.rewriteLinear(stmt_id, .{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = undefined,
            } }, assign.next),
            .assign_ref => |assign| try self.rewriteLinear(stmt_id, .{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .op = assign.op,
                .next = undefined,
            } }, assign.next),
            .assign_literal => |assign| try self.rewriteLinear(stmt_id, .{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = undefined,
            } }, assign.next),
            .assign_call => |assign| try self.rewriteLinear(stmt_id, .{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = undefined,
            } }, assign.next),
            .assign_call_indirect => |assign| try self.rewriteLinear(stmt_id, .{ .assign_call_indirect = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .closure = assign.closure,
                .args = assign.args,
                .capture_layout = assign.capture_layout,
                .next = undefined,
            } }, assign.next),
            .assign_low_level => |assign| try self.rewriteLinear(stmt_id, .{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .op = assign.op,
                .args = assign.args,
                .next = undefined,
            } }, assign.next),
            .assign_list => |assign| try self.rewriteLinear(stmt_id, .{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .elems = assign.elems,
                .next = undefined,
            } }, assign.next),
            .assign_struct => |assign| try self.rewriteLinear(stmt_id, .{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .fields = assign.fields,
                .next = undefined,
            } }, assign.next),
            .assign_tag => |assign| try self.rewriteLinear(stmt_id, .{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .discriminant = assign.discriminant,
                .payload = assign.payload,
                .next = undefined,
            } }, assign.next),
            .set_local => |assign| try self.rewriteLinear(stmt_id, .{ .set_local = .{
                .target = assign.target,
                .value = assign.value,
                .next = undefined,
            } }, assign.next),
            .debug => |debug_stmt| try self.rewriteLinear(stmt_id, .{ .debug = .{
                .message = debug_stmt.message,
                .next = undefined,
            } }, debug_stmt.next),
            .expect => |expect_stmt| try self.rewriteLinear(stmt_id, .{ .expect = .{
                .condition = expect_stmt.condition,
                .next = undefined,
            } }, expect_stmt.next),
            .incref => |inc| try self.rewriteLinear(stmt_id, .{ .incref = .{
                .value = inc.value,
                .count = inc.count,
                .next = undefined,
            } }, inc.next),
            .decref => |dec| try self.rewriteLinear(stmt_id, .{ .decref = .{
                .value = dec.value,
                .next = undefined,
            } }, dec.next),
            .free => |free_stmt| try self.rewriteLinear(stmt_id, .{ .free = .{
                .value = free_stmt.value,
                .next = undefined,
            } }, free_stmt.next),
            .switch_stmt => |sw| blk: {
                const rewritten_switch = try self.cloneStmtSkeleton(stmt_id);
                try self.rewritten_stmt_ids.put(@intFromEnum(stmt_id), rewritten_switch);

                const branches = try self.allocator.dupe(LIR.CFSwitchBranch, self.store.getCFSwitchBranches(sw.branches));
                defer self.allocator.free(branches);
                for (branches) |*branch| {
                    const body = try self.rewriteStmt(branch.body);
                    branch.body = try self.prependEdgeDrops(stmt_id, body, branch.body);
                }
                const default_body = try self.rewriteStmt(sw.default_branch);
                const default_with_drops = try self.prependEdgeDrops(stmt_id, default_body, sw.default_branch);
                const rewritten_branches = try self.store.addCFSwitchBranches(branches);
                const rewritten_ptr = self.store.getCFStmtPtr(rewritten_switch);
                rewritten_ptr.switch_stmt.branches = rewritten_branches;
                rewritten_ptr.switch_stmt.default_branch = default_with_drops;
                break :blk rewritten_switch;
            },
            .borrow_scope => |scope| blk: {
                const rewritten_scope = try self.cloneStmtSkeleton(stmt_id);
                try self.rewritten_stmt_ids.put(@intFromEnum(stmt_id), rewritten_scope);

                const rewritten_body = try self.rewriteStmt(scope.body);
                const rewritten_remainder = try self.rewriteStmt(scope.remainder);
                const rewritten_ptr = self.store.getCFStmtPtr(rewritten_scope);
                rewritten_ptr.borrow_scope.body = rewritten_body;
                rewritten_ptr.borrow_scope.remainder = rewritten_remainder;
                break :blk rewritten_scope;
            },
            .for_list => |for_stmt| blk: {
                const rewritten_loop = try self.cloneStmtSkeleton(stmt_id);
                try self.rewritten_stmt_ids.put(@intFromEnum(stmt_id), rewritten_loop);

                const raw_body = try self.rewriteStmt(for_stmt.body);
                const body = try self.prependMaterializationRetain(for_stmt.elem, for_stmt.elem_result, for_stmt.elem_ownership, raw_body);
                const rewritten_next = try self.rewriteStmt(for_stmt.next);
                const next = try self.prependEdgeDrops(stmt_id, rewritten_next, for_stmt.next);
                const rewritten_ptr = self.store.getCFStmtPtr(rewritten_loop);
                rewritten_ptr.for_list.body = body;
                rewritten_ptr.for_list.next = next;
                break :blk rewritten_loop;
            },
            .join => |join| blk: {
                const rewritten_join = try self.cloneStmtSkeleton(stmt_id);
                try self.rewritten_stmt_ids.put(@intFromEnum(stmt_id), rewritten_join);

                const rewritten_body = try self.rewriteStmt(join.body);
                const rewritten_remainder = try self.rewriteStmt(join.remainder);
                const rewritten_ptr = self.store.getCFStmtPtr(rewritten_join);
                rewritten_ptr.join.body = rewritten_body;
                rewritten_ptr.join.remainder = rewritten_remainder;
                break :blk rewritten_join;
            },
            .scope_exit => |scope_exit| blk: {
                const terminal = try self.store.addCFStmt(.{ .scope_exit = .{ .id = scope_exit.id } });
                break :blk try self.prependTerminalDrops(stmt_id, terminal);
            },
            .jump => |jump| blk: {
                const rewritten_jump = try self.store.addCFStmt(.{ .jump = .{
                    .target = jump.target,
                    .args = jump.args,
                } });
                break :blk try self.prependJumpOwnershipRetains(stmt_id, rewritten_jump);
            },
            .ret => |ret_stmt| blk: {
                const terminal = try self.store.addCFStmt(.{ .ret = .{ .value = ret_stmt.value } });
                break :blk try self.prependTerminalDrops(stmt_id, terminal);
            },
            .runtime_error => blk: {
                const terminal = try self.store.addCFStmt(.runtime_error);
                break :blk try self.prependTerminalDrops(stmt_id, terminal);
            },
            .crash => |crash| blk: {
                const terminal = try self.store.addCFStmt(.{ .crash = .{ .msg = crash.msg } });
                break :blk try self.prependTerminalDrops(stmt_id, terminal);
            },
            .loop_continue => stmt_id,
        };

        if (!self.rewritten_stmt_ids.contains(@intFromEnum(stmt_id))) {
            try self.rewritten_stmt_ids.put(@intFromEnum(stmt_id), rewritten);
        }
        return rewritten;
    }

    fn rewriteLinear(self: *ProcPass, original_stmt_id: CFStmtId, stmt_template: CFStmt, next_stmt: CFStmtId) Allocator.Error!CFStmtId {
        const rewritten_next = try self.rewriteStmt(next_stmt);
        const with_drops = try self.prependEdgeDrops(original_stmt_id, rewritten_next, next_stmt);
        const next_for_stmt = switch (stmt_template) {
            .assign_ref => |assign| try self.prependMaterializationRetain(assign.target, assign.result, assign.ownership, with_drops),
            .assign_call_indirect => |assign| try self.prependMaterializationRetain(assign.target, assign.result, assign.ownership, with_drops),
            .assign_low_level => |assign| try self.prependMaterializationRetain(assign.target, assign.result, assign.ownership, with_drops),
            .assign_list => |assign| try self.prependMaterializationRetain(assign.target, assign.result, assign.ownership, with_drops),
            .assign_struct => |assign| try self.prependMaterializationRetain(assign.target, assign.result, assign.ownership, with_drops),
            .assign_tag => |assign| try self.prependMaterializationRetain(assign.target, assign.result, assign.ownership, with_drops),
            .set_local => |assign| try self.prependSetLocalOverwriteDrop(original_stmt_id, assign.target, assign.value, with_drops),
            else => with_drops,
        };
        const rewritten_stmt = try switch (stmt_template) {
            .assign_symbol => |assign| self.store.addCFStmt(.{ .assign_symbol = .{ .target = assign.target, .symbol = assign.symbol, .next = next_for_stmt } }),
            .assign_ref => |assign| self.store.addCFStmt(.{ .assign_ref = .{ .target = assign.target, .result = assign.result, .ownership = assign.ownership, .op = assign.op, .next = next_for_stmt } }),
            .assign_literal => |assign| self.store.addCFStmt(.{ .assign_literal = .{ .target = assign.target, .result = assign.result, .value = assign.value, .next = next_for_stmt } }),
            .assign_call => |assign| self.store.addCFStmt(.{ .assign_call = .{ .target = assign.target, .result = assign.result, .proc = assign.proc, .args = assign.args, .next = next_for_stmt } }),
            .assign_call_indirect => |assign| self.store.addCFStmt(.{ .assign_call_indirect = .{ .target = assign.target, .result = assign.result, .ownership = assign.ownership, .closure = assign.closure, .args = assign.args, .capture_layout = assign.capture_layout, .next = next_for_stmt } }),
            .assign_low_level => |assign| self.store.addCFStmt(.{ .assign_low_level = .{ .target = assign.target, .result = assign.result, .ownership = assign.ownership, .op = assign.op, .args = assign.args, .next = next_for_stmt } }),
            .assign_list => |assign| self.store.addCFStmt(.{ .assign_list = .{ .target = assign.target, .result = assign.result, .ownership = assign.ownership, .elems = assign.elems, .next = next_for_stmt } }),
            .assign_struct => |assign| self.store.addCFStmt(.{ .assign_struct = .{ .target = assign.target, .result = assign.result, .ownership = assign.ownership, .fields = assign.fields, .next = next_for_stmt } }),
            .assign_tag => |assign| self.store.addCFStmt(.{ .assign_tag = .{ .target = assign.target, .result = assign.result, .ownership = assign.ownership, .discriminant = assign.discriminant, .payload = assign.payload, .next = next_for_stmt } }),
            .set_local => |assign| self.store.addCFStmt(.{ .set_local = .{ .target = assign.target, .value = assign.value, .next = next_for_stmt } }),
            .debug => |debug_stmt| self.store.addCFStmt(.{ .debug = .{ .message = debug_stmt.message, .next = next_for_stmt } }),
            .expect => |expect_stmt| self.store.addCFStmt(.{ .expect = .{ .condition = expect_stmt.condition, .next = next_for_stmt } }),
            .for_list => |for_stmt| self.store.addCFStmt(.{ .for_list = .{
                .elem = for_stmt.elem,
                .elem_result = for_stmt.elem_result,
                .elem_ownership = for_stmt.elem_ownership,
                .iterable = for_stmt.iterable,
                .iterable_elem_layout = for_stmt.iterable_elem_layout,
                .body = for_stmt.body,
                .next = next_for_stmt,
            } }),
            .incref => |inc| self.store.addCFStmt(.{ .incref = .{ .value = inc.value, .count = inc.count, .next = next_for_stmt } }),
            .decref => |dec| self.store.addCFStmt(.{ .decref = .{ .value = dec.value, .next = next_for_stmt } }),
            .free => |free_stmt| self.store.addCFStmt(.{ .free = .{ .value = free_stmt.value, .next = next_for_stmt } }),
            else => unreachable,
        };
        return try self.prependOwnershipRetains(original_stmt_id, rewritten_stmt, next_stmt);
    }

    fn prependSetLocalOverwriteDrop(
        self: *ProcPass,
        stmt_id: CFStmtId,
        target: LocalId,
        value: LocalId,
        next: CFStmtId,
    ) Allocator.Error!CFStmtId {
        const slot = self.stmtSlot(stmt_id) orelse return next;
        if (!self.live_in[slot].isSet(@intFromEnum(target))) return next;
        if (!self.localCanOwnRefcount(target)) return next;

        const incoming_owner = self.explicitOwnerForLocal(value) orelse value;
        if (incoming_owner == target) return next;

        return self.store.addCFStmt(.{ .decref = .{
            .value = target,
            .next = next,
        } });
    }

    fn prependEdgeDrops(self: *ProcPass, from_stmt: CFStmtId, successor_rewritten: CFStmtId, original_successor: CFStmtId) Allocator.Error!CFStmtId {
        const from_slot = self.stmtSlot(from_stmt) orelse return successor_rewritten;
        const succ_slot = self.stmtSlot(original_successor) orelse return successor_rewritten;

        var dead = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer dead.deinit(self.allocator);
        var defs = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer defs.deinit(self.allocator);
        var uses = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer uses.deinit(self.allocator);
        var ownership_passed = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer ownership_passed.deinit(self.allocator);

        self.collectDefsUses(from_stmt, &defs, &uses, &ownership_passed);
        switch (self.store.getCFStmt(from_stmt)) {
            .for_list => |for_stmt| defs.unset(@intFromEnum(for_stmt.elem)),
            else => {},
        }

        dead.setUnion(self.live_in[from_slot]);
        defs.setIntersection(self.owned_locals);
        dead.setUnion(defs);

        var dead_owners = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer dead_owners.deinit(self.allocator);
        self.collectLiveOwnerSet(dead, &dead_owners);

        var succ_live_owners = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer succ_live_owners.deinit(self.allocator);
        self.collectLiveOwnerSet(self.live_in[succ_slot], &succ_live_owners);

        var passed_owners = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer passed_owners.deinit(self.allocator);
        self.collectExactOwnerSet(ownership_passed, &passed_owners);

        setDifferenceInPlace(&dead_owners, succ_live_owners);
        setDifferenceInPlace(&dead_owners, passed_owners);
        try self.excludeSkippedEdgeOwnerDefs(from_stmt, original_successor, &dead_owners);

        return try self.emitOwnerDrops(dead_owners, successor_rewritten);
    }

    fn excludeSkippedEdgeOwnerDefs(
        self: *ProcPass,
        from_stmt: CFStmtId,
        original_successor: CFStmtId,
        dead_owners: *std.DynamicBitSetUnmanaged,
    ) Allocator.Error!void {
        var skipped_stmt_ids = std.ArrayListUnmanaged(CFStmtId).empty;
        defer skipped_stmt_ids.deinit(self.allocator);

        switch (self.store.getCFStmt(from_stmt)) {
            .for_list => |for_stmt| {
                if (for_stmt.next != original_successor) return;
                try appendReachableStmtIds(self.allocator, self.store, &skipped_stmt_ids, for_stmt.body);
            },
            .switch_stmt => |sw| {
                if (sw.default_branch == original_successor) {
                    for (self.store.getCFSwitchBranches(sw.branches)) |branch| {
                        try appendReachableStmtIds(self.allocator, self.store, &skipped_stmt_ids, branch.body);
                    }
                } else {
                    var matched_branch = false;
                    for (self.store.getCFSwitchBranches(sw.branches)) |branch| {
                        if (branch.body == original_successor) {
                            matched_branch = true;
                            continue;
                        }
                        try appendReachableStmtIds(self.allocator, self.store, &skipped_stmt_ids, branch.body);
                    }
                    if (!matched_branch) return;
                    try appendReachableStmtIds(self.allocator, self.store, &skipped_stmt_ids, sw.default_branch);
                }
            },
            else => return,
        }

        var local_idx: usize = 0;
        while (local_idx < dead_owners.bit_length) : (local_idx += 1) {
            if (!dead_owners.isSet(local_idx)) continue;
            const local: LocalId = @enumFromInt(@as(u32, @intCast(local_idx)));
            const producer = findProducerStmt(self.store, local) orelse continue;
            if (containsStmtId(skipped_stmt_ids.items, producer)) {
                dead_owners.unset(local_idx);
            }
        }
    }

    fn prependTerminalDrops(self: *ProcPass, stmt_id: CFStmtId, terminal_rewritten: CFStmtId) Allocator.Error!CFStmtId {
        const slot = self.stmtSlot(stmt_id) orelse return terminal_rewritten;

        var defs = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer defs.deinit(self.allocator);
        var uses = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer uses.deinit(self.allocator);
        var ownership_passed = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer ownership_passed.deinit(self.allocator);

        self.collectDefsUses(stmt_id, &defs, &uses, &ownership_passed);

        var dead_owners = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer dead_owners.deinit(self.allocator);
        self.collectLiveOwnerSet(self.live_in[slot], &dead_owners);

        var used_owners = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer used_owners.deinit(self.allocator);
        self.collectLiveOwnerSet(uses, &used_owners);

        var passed_owners = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer passed_owners.deinit(self.allocator);
        self.collectExactOwnerSet(ownership_passed, &passed_owners);

        setDifferenceInPlace(&dead_owners, used_owners);
        setDifferenceInPlace(&dead_owners, passed_owners);

        return try self.emitOwnerDrops(dead_owners, terminal_rewritten);
    }

    fn collectLiveOwnerSet(
        self: *ProcPass,
        locals: std.DynamicBitSetUnmanaged,
        out: *std.DynamicBitSetUnmanaged,
    ) void {
        var local_idx: usize = 0;
        while (local_idx < locals.bit_length) : (local_idx += 1) {
            if (!locals.isSet(local_idx)) continue;
            const local: LocalId = @enumFromInt(@as(u32, @intCast(local_idx)));
            const owner = self.liveOwnerForLocal(local) orelse continue;
            out.set(@intFromEnum(owner));
        }
    }

    fn collectExactOwnerSet(
        self: *ProcPass,
        locals: std.DynamicBitSetUnmanaged,
        out: *std.DynamicBitSetUnmanaged,
    ) void {
        var local_idx: usize = 0;
        while (local_idx < locals.bit_length) : (local_idx += 1) {
            if (!locals.isSet(local_idx)) continue;
            const local: LocalId = @enumFromInt(@as(u32, @intCast(local_idx)));
            const owner = self.exactOwnerForLocal(local) orelse continue;
            out.set(@intFromEnum(owner));
        }
    }

    fn emitOwnerDrops(
        self: *ProcPass,
        dead_owners: std.DynamicBitSetUnmanaged,
        successor: CFStmtId,
    ) Allocator.Error!CFStmtId {
        var cursor = successor;
        var local_idx = self.local_count;
        while (local_idx > 0) {
            local_idx -= 1;
            if (!dead_owners.isSet(local_idx)) continue;
            cursor = try self.store.addCFStmt(.{ .decref = .{
                .value = @enumFromInt(@as(u32, @intCast(local_idx))),
                .next = cursor,
            } });
        }
        return cursor;
    }

    fn prependOwnershipRetains(self: *ProcPass, stmt_id: CFStmtId, successor: CFStmtId, original_successor: CFStmtId) Allocator.Error!CFStmtId {
        const succ_slot = self.stmtSlot(original_successor) orelse return successor;

        var ownership_passed = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer ownership_passed.deinit(self.allocator);
        try self.collectOwnershipPassed(stmt_id, &ownership_passed);

        const retained_counts = try self.allocator.alloc(u16, self.local_count);
        defer self.allocator.free(retained_counts);
        @memset(retained_counts, 0);
        try self.collectRetainedInputIncrefs(stmt_id, retained_counts);
        try self.collectRequiredOwnedInputIncrefs(stmt_id, retained_counts);

        var cursor = successor;

        var local_idx = self.local_count;
        while (local_idx > 0) {
            local_idx -= 1;
            if (ownership_passed.isSet(local_idx) and self.live_in_direct[succ_slot].isSet(local_idx)) {
                cursor = try self.store.addCFStmt(.{ .incref = .{
                    .value = @enumFromInt(@as(u32, @intCast(local_idx))),
                    .count = 1,
                    .next = cursor,
                } });
            }
        }

        local_idx = self.local_count;
        while (local_idx > 0) {
            local_idx -= 1;
            const retain_count = retained_counts[local_idx];
            if (retain_count == 0) continue;
            cursor = try self.store.addCFStmt(.{ .incref = .{
                .value = @enumFromInt(@as(u32, @intCast(local_idx))),
                .count = retain_count,
                .next = cursor,
            } });
        }

        return cursor;
    }

    fn prependJumpOwnershipRetains(self: *ProcPass, stmt_id: CFStmtId, successor: CFStmtId) Allocator.Error!CFStmtId {
        const retained_counts = try self.allocator.alloc(u16, self.local_count);
        defer self.allocator.free(retained_counts);
        @memset(retained_counts, 0);
        try self.collectRequiredOwnedInputIncrefs(stmt_id, retained_counts);

        var cursor = successor;
        var local_idx = self.local_count;
        while (local_idx > 0) {
            local_idx -= 1;
            const retain_count = retained_counts[local_idx];
            if (retain_count == 0) continue;
            cursor = try self.store.addCFStmt(.{ .incref = .{
                .value = @enumFromInt(@as(u32, @intCast(local_idx))),
                .count = retain_count,
                .next = cursor,
            } });
        }

        return cursor;
    }

    fn collectOwnershipPassed(self: *ProcPass, stmt_id: CFStmtId, ownership_passed: *std.DynamicBitSetUnmanaged) Allocator.Error!void {
        var defs = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer defs.deinit(self.allocator);
        var uses = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer uses.deinit(self.allocator);
        self.collectDefsUses(stmt_id, &defs, &uses, ownership_passed);
    }

    fn collectRetainedInputIncrefs(self: *ProcPass, stmt_id: CFStmtId, retained_counts: []u16) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_ref => |assign| try self.accumulateFreshInputRetains(self.store.getLocalSpan(assign.ownership.consumed_owned_inputs), retained_counts),
            .assign_list => |assign| {
                try self.accumulateFreshInputRetains(self.store.getLocalSpan(assign.ownership.consumed_owned_inputs), retained_counts);
                try accumulateRetainedBorrows(self.store.getLocalSpan(assign.ownership.retained_borrows), retained_counts);
            },
            .assign_struct => |assign| {
                try self.accumulateFreshInputRetains(self.store.getLocalSpan(assign.ownership.consumed_owned_inputs), retained_counts);
                try accumulateRetainedBorrows(self.store.getLocalSpan(assign.ownership.retained_borrows), retained_counts);
            },
            .assign_tag => |assign| {
                try self.accumulateFreshInputRetains(self.store.getLocalSpan(assign.ownership.consumed_owned_inputs), retained_counts);
                try accumulateRetainedBorrows(self.store.getLocalSpan(assign.ownership.retained_borrows), retained_counts);
            },
            .assign_low_level => |assign| try accumulateRetainedBorrows(self.store.getLocalSpan(assign.ownership.retained_borrows), retained_counts),
            else => {},
        }
    }

    fn prependMaterializationRetain(
        self: *ProcPass,
        target: LocalId,
        result: LIR.ResultSemantics,
        ownership: LIR.OwnershipSemantics,
        next: CFStmtId,
    ) Allocator.Error!CFStmtId {
        if (result != .fresh) return next;
        if (ownership.materialization != .copy_from_borrowed_input) return next;

        const layout_idx = self.store.getLocal(target).layout_idx;
        if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) return next;

        return self.store.addCFStmt(.{ .incref = .{
            .value = target,
            .count = 1,
            .next = next,
        } });
    }

    fn accumulateRetainedBorrows(retained_borrows: []const LocalId, retained_counts: []u16) Allocator.Error!void {
        for (retained_borrows) |local| {
            incrementRetainCount(local, retained_counts, 1);
        }
    }

    fn collectRequiredOwnedInputIncrefs(self: *ProcPass, stmt_id: CFStmtId, retained_counts: []u16) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_call => |assign| {
                const callee = self.store.getProcSpec(assign.proc);
                const args = self.store.getLocalSpan(assign.args);
                const params = self.store.getLocalSpan(callee.args);
                const owned_params = self.store.getLocalSpan(callee.owned_params);

                for (owned_params) |owned_param| {
                    const param_index = indexOfLocal(params, owned_param) orelse continue;
                    if (param_index >= args.len) continue;
                    self.accumulateBorrowedConsumeRetain(args[param_index], retained_counts);
                }
            },
            .assign_low_level => |assign| {
                for (self.store.getLocalSpan(assign.ownership.consumed_owned_inputs)) |local| {
                    if (resultContinuesOwnershipFromInput(assign.result, local)) continue;
                    self.accumulateBorrowedConsumeRetain(local, retained_counts);
                }
            },
            .set_local => |assign| self.accumulateBorrowedConsumeRetain(assign.value, retained_counts),
            .jump => |jump| {
                if (self.join_params_by_id.get(@intFromEnum(jump.target))) |params_span| {
                    const params = self.store.getLocalSpan(params_span);
                    const args = self.store.getLocalSpan(jump.args);
                    if (params.len != args.len) return;
                    for (args, params) |arg, param| {
                        if (!self.localCanOwnRefcount(param)) continue;
                        self.accumulateBorrowedConsumeRetain(arg, retained_counts);
                    }
                }
            },
            else => {},
        }
    }

    fn resultContinuesOwnershipFromInput(result: LIR.ResultSemantics, input: LocalId) bool {
        return switch (result) {
            .alias_of => |alias| alias.owner == input,
            else => false,
        };
    }

    fn accumulateFreshInputRetains(self: *ProcPass, locals: []const LocalId, retained_counts: []u16) Allocator.Error!void {
        const occurrences = try self.allocator.alloc(u16, self.local_count);
        defer self.allocator.free(occurrences);
        @memset(occurrences, 0);

        for (locals) |local| {
            const local_idx = @intFromEnum(local);
            const next = occurrences[local_idx] + 1;
            std.debug.assert(next > occurrences[local_idx]);
            occurrences[local_idx] = next;
        }

        for (locals) |local| {
            const local_idx = @intFromEnum(local);
            const occurrence_count = occurrences[local_idx];
            if (occurrence_count == 0) continue;
            occurrences[local_idx] = 0;

            const retain_count = self.freshInputRetainCount(local, occurrence_count);
            if (retain_count == 0) continue;
            incrementRetainCount(local, retained_counts, retain_count);
        }
    }

    fn accumulateBorrowedConsumeRetain(self: *ProcPass, local: LocalId, retained_counts: []u16) void {
        const layout_idx = self.store.getLocal(local).layout_idx;
        if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) return;
        if (self.explicitOwnerForLocal(local) != null) return;
        incrementRetainCount(local, retained_counts, 1);
    }

    fn freshInputRetainCount(self: *ProcPass, local: LocalId, occurrence_count: u16) u16 {
        const layout_idx = self.store.getLocal(local).layout_idx;
        if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) return 0;

        if (self.localCanOwnRefcount(local)) {
            return occurrence_count -| 1;
        }

        return occurrence_count;
    }

    fn incrementRetainCount(local: LocalId, retained_counts: []u16, delta: u16) void {
        const local_idx = @intFromEnum(local);
        const next = retained_counts[local_idx] + delta;
        std.debug.assert(next >= retained_counts[local_idx]);
        retained_counts[local_idx] = next;
    }

    fn collectLoopContinueTargets(
        self: *ProcPass,
        stmt_id: CFStmtId,
        for_stack: []const CFStmtId,
    ) Allocator.Error!void {
        var visited = std.AutoHashMap(LoopContinueVisitKey, void).init(self.allocator);
        defer visited.deinit();
        try self.collectLoopContinueTargetsRec(stmt_id, for_stack, &visited);
    }

    fn collectLoopContinueTargetsRec(
        self: *ProcPass,
        stmt_id: CFStmtId,
        for_stack: []const CFStmtId,
        visited: *std.AutoHashMap(LoopContinueVisitKey, void),
    ) Allocator.Error!void {
        const innermost_for = if (for_stack.len == 0) null else for_stack[for_stack.len - 1];
        const gop = try visited.getOrPut(.{
            .stmt_id = stmt_id,
            .innermost_for = innermost_for,
        });
        if (gop.found_existing) {
            return;
        }

        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.collectLoopContinueTargetsRec(assign.next, for_stack, visited),
            .assign_ref => |assign| try self.collectLoopContinueTargetsRec(assign.next, for_stack, visited),
            .assign_literal => |assign| try self.collectLoopContinueTargetsRec(assign.next, for_stack, visited),
            .assign_call => |assign| try self.collectLoopContinueTargetsRec(assign.next, for_stack, visited),
            .assign_call_indirect => |assign| try self.collectLoopContinueTargetsRec(assign.next, for_stack, visited),
            .assign_low_level => |assign| try self.collectLoopContinueTargetsRec(assign.next, for_stack, visited),
            .assign_list => |assign| try self.collectLoopContinueTargetsRec(assign.next, for_stack, visited),
            .assign_struct => |assign| try self.collectLoopContinueTargetsRec(assign.next, for_stack, visited),
            .assign_tag => |assign| try self.collectLoopContinueTargetsRec(assign.next, for_stack, visited),
            .set_local => |assign| try self.collectLoopContinueTargetsRec(assign.next, for_stack, visited),
            .debug => |debug_stmt| try self.collectLoopContinueTargetsRec(debug_stmt.next, for_stack, visited),
            .expect => |expect_stmt| try self.collectLoopContinueTargetsRec(expect_stmt.next, for_stack, visited),
            .incref => |inc| try self.collectLoopContinueTargetsRec(inc.next, for_stack, visited),
            .decref => |dec| try self.collectLoopContinueTargetsRec(dec.next, for_stack, visited),
            .free => |free_stmt| try self.collectLoopContinueTargetsRec(free_stmt.next, for_stack, visited),
            .switch_stmt => |sw| {
                try self.collectLoopContinueTargetsRec(sw.default_branch, for_stack, visited);
                for (self.store.getCFSwitchBranches(sw.branches)) |branch| {
                    try self.collectLoopContinueTargetsRec(branch.body, for_stack, visited);
                }
            },
            .borrow_scope => |scope| {
                try self.collectLoopContinueTargetsRec(scope.body, for_stack, visited);
                try self.collectLoopContinueTargetsRec(scope.remainder, for_stack, visited);
            },
            .for_list => |for_stmt| {
                var nested = try self.allocator.alloc(CFStmtId, for_stack.len + 1);
                defer self.allocator.free(nested);
                @memcpy(nested[0..for_stack.len], for_stack);
                nested[for_stack.len] = stmt_id;
                try self.collectLoopContinueTargetsRec(for_stmt.body, nested, visited);
                try self.collectLoopContinueTargetsRec(for_stmt.next, for_stack, visited);
            },
            .join => |join| {
                try self.collectLoopContinueTargetsRec(join.body, for_stack, visited);
                try self.collectLoopContinueTargetsRec(join.remainder, for_stack, visited);
            },
            .loop_continue => {
                const target = for_stack[for_stack.len - 1];
                try self.loop_continue_targets.put(@intFromEnum(stmt_id), target);
            },
            .scope_exit, .jump, .ret, .runtime_error, .crash => {},
        }
    }

};

fn collectReachableStmtIds(
    allocator: Allocator,
    store: *const LirStore,
    body: CFStmtId,
) Allocator.Error![]CFStmtId {
    var out = std.ArrayListUnmanaged(CFStmtId).empty;
    errdefer out.deinit(allocator);
    try appendReachableStmtIds(allocator, store, &out, body);
    return out.toOwnedSlice(allocator);
}

fn appendReachableStmtIds(
    allocator: Allocator,
    store: *const LirStore,
    out: *std.ArrayListUnmanaged(CFStmtId),
    body: CFStmtId,
) Allocator.Error!void {
    var stack = std.ArrayListUnmanaged(CFStmtId).empty;
    defer stack.deinit(allocator);

    var visited = std.AutoHashMap(u32, void).init(allocator);
    defer visited.deinit();

    try stack.append(allocator, body);

    while (stack.pop()) |stmt_id| {
        const gop = try visited.getOrPut(@intFromEnum(stmt_id));
        if (gop.found_existing) continue;

        try out.append(allocator, stmt_id);
        switch (store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try stack.append(allocator, assign.next),
            .assign_ref => |assign| try stack.append(allocator, assign.next),
            .assign_literal => |assign| try stack.append(allocator, assign.next),
            .assign_call => |assign| try stack.append(allocator, assign.next),
            .assign_call_indirect => |assign| try stack.append(allocator, assign.next),
            .assign_low_level => |assign| try stack.append(allocator, assign.next),
            .assign_list => |assign| try stack.append(allocator, assign.next),
            .assign_struct => |assign| try stack.append(allocator, assign.next),
            .assign_tag => |assign| try stack.append(allocator, assign.next),
            .set_local => |assign| try stack.append(allocator, assign.next),
            .debug => |debug_stmt| try stack.append(allocator, debug_stmt.next),
            .expect => |expect_stmt| try stack.append(allocator, expect_stmt.next),
            .incref => |inc| try stack.append(allocator, inc.next),
            .decref => |dec| try stack.append(allocator, dec.next),
            .free => |free_stmt| try stack.append(allocator, free_stmt.next),
            .switch_stmt => |sw| {
                try stack.append(allocator, sw.default_branch);
                for (store.getCFSwitchBranches(sw.branches)) |branch| {
                    try stack.append(allocator, branch.body);
                }
            },
            .borrow_scope => |scope| {
                try stack.append(allocator, scope.body);
                try stack.append(allocator, scope.remainder);
            },
            .for_list => |for_stmt| {
                try stack.append(allocator, for_stmt.body);
                try stack.append(allocator, for_stmt.next);
            },
            .join => |join| {
                try stack.append(allocator, join.body);
                try stack.append(allocator, join.remainder);
            },
            .scope_exit, .jump, .ret, .runtime_error, .crash, .loop_continue => {},
        }
    }
}

fn findProducerStmt(store: *const LirStore, target: LocalId) ?CFStmtId {
    for (store.cf_stmts.items, 0..) |stmt, idx| {
        switch (stmt) {
            .assign_symbol => |assign| if (assign.target == target) return @enumFromInt(@as(u32, @intCast(idx))),
            .assign_ref => |assign| if (assign.target == target) return @enumFromInt(@as(u32, @intCast(idx))),
            .assign_literal => |assign| if (assign.target == target) return @enumFromInt(@as(u32, @intCast(idx))),
            .assign_call => |assign| if (assign.target == target) return @enumFromInt(@as(u32, @intCast(idx))),
            .assign_call_indirect => |assign| if (assign.target == target) return @enumFromInt(@as(u32, @intCast(idx))),
            .assign_low_level => |assign| if (assign.target == target) return @enumFromInt(@as(u32, @intCast(idx))),
            .assign_list => |assign| if (assign.target == target) return @enumFromInt(@as(u32, @intCast(idx))),
            .assign_struct => |assign| if (assign.target == target) return @enumFromInt(@as(u32, @intCast(idx))),
            .assign_tag => |assign| if (assign.target == target) return @enumFromInt(@as(u32, @intCast(idx))),
            .set_local => |assign| if (assign.target == target) return @enumFromInt(@as(u32, @intCast(idx))),
            .for_list => |for_stmt| if (for_stmt.elem == target) return @enumFromInt(@as(u32, @intCast(idx))),
            else => {},
        }
    }

    return null;
}

fn containsStmtId(stmt_ids: []const CFStmtId, target: CFStmtId) bool {
    for (stmt_ids) |stmt_id| {
        if (stmt_id == target) return true;
    }
    return false;
}


fn bitsetsEqual(a: std.DynamicBitSetUnmanaged, b: std.DynamicBitSetUnmanaged) bool {
    if (a.bit_length != b.bit_length) return false;
    return a.eql(b);
}

fn setDifferenceInPlace(target: *std.DynamicBitSetUnmanaged, subtract: std.DynamicBitSetUnmanaged) void {
    std.debug.assert(target.bit_length == subtract.bit_length);
    const num_masks = (target.bit_length + (@bitSizeOf(std.DynamicBitSetUnmanaged.MaskInt) - 1)) / @bitSizeOf(std.DynamicBitSetUnmanaged.MaskInt);
    var i: usize = 0;
    while (i < num_masks) : (i += 1) {
        target.masks[i] &= ~subtract.masks[i];
    }
}
