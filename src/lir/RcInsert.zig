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
const layout = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const CFStmt = LIR.CFStmt;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const LirProcSpecId = LIR.LirProcSpecId;

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
    provenance_owner: []?LocalId,
    owned_locals: std.DynamicBitSetUnmanaged,
    live_in: []std.DynamicBitSetUnmanaged,
    live_out: []std.DynamicBitSetUnmanaged,
    rewritten_stmt_ids: std.AutoHashMap(u32, CFStmtId),
    loop_continue_targets: std.AutoHashMap(u32, CFStmtId),
    join_params_by_id: std.AutoHashMap(u32, LIR.LocalSpan),
    join_bodies_by_id: std.AutoHashMap(u32, CFStmtId),

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
        const provenance_owner = try allocator.alloc(?LocalId, local_count);
        errdefer allocator.free(provenance_owner);
        @memset(provenance_owner, null);

        const live_in = try allocator.alloc(std.DynamicBitSetUnmanaged, stmt_count);
        errdefer allocator.free(live_in);
        const live_out = try allocator.alloc(std.DynamicBitSetUnmanaged, stmt_count);
        errdefer allocator.free(live_out);

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
            .provenance_owner = provenance_owner,
            .owned_locals = try std.DynamicBitSetUnmanaged.initEmpty(allocator, local_count),
            .live_in = live_in,
            .live_out = live_out,
            .rewritten_stmt_ids = std.AutoHashMap(u32, CFStmtId).init(allocator),
            .loop_continue_targets = std.AutoHashMap(u32, CFStmtId).init(allocator),
            .join_params_by_id = std.AutoHashMap(u32, LIR.LocalSpan).init(allocator),
            .join_bodies_by_id = std.AutoHashMap(u32, CFStmtId).init(allocator),
        };
        errdefer pass.owned_locals.deinit(allocator);
        errdefer pass.rewritten_stmt_ids.deinit();
        errdefer pass.loop_continue_targets.deinit();
        errdefer pass.join_params_by_id.deinit();
        errdefer pass.join_bodies_by_id.deinit();

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
        self.allocator.free(self.provenance_owner);
        self.owned_locals.deinit(self.allocator);
        self.rewritten_stmt_ids.deinit();
        self.loop_continue_targets.deinit();
        self.join_params_by_id.deinit();
        self.join_bodies_by_id.deinit();
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
                .assign_ref => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_literal => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_call => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_call_indirect => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_low_level => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_list => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_struct => |assign| self.markOwnedFresh(assign.target, assign.result),
                .assign_tag => |assign| self.markOwnedFresh(assign.target, assign.result),
                .set_local => |assign| self.markOwnedLocal(assign.target),
                .join => |join| {
                    _ = self.join_params_by_id.put(@intFromEnum(join.id), join.params) catch unreachable;
                    _ = self.join_bodies_by_id.put(@intFromEnum(join.id), join.body) catch unreachable;
                    for (self.store.getLocalSpan(join.params)) |param| {
                        self.markOwnedLocal(param);
                    }
                },
                else => {},
            }
        }
    }

    fn markOwnedFresh(self: *ProcPass, target: LocalId, result: LIR.ResultSemantics) void {
        if (result != .fresh) return;
        const local_idx = @intFromEnum(target);
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
                .assign_ref => |assign| self.recordResultOwner(assign.target, assign.result),
                .assign_call => |assign| self.recordResultOwner(assign.target, assign.result),
                .assign_call_indirect => |assign| self.recordResultOwner(assign.target, assign.result),
                .assign_low_level => |assign| self.recordResultOwner(assign.target, assign.result),
                .assign_literal => |assign| self.provenance_owner[@intFromEnum(assign.target)] = null,
                .assign_symbol => |assign| self.provenance_owner[@intFromEnum(assign.target)] = null,
                .assign_list => |assign| self.provenance_owner[@intFromEnum(assign.target)] = null,
                .assign_struct => |assign| self.provenance_owner[@intFromEnum(assign.target)] = null,
                .assign_tag => |assign| self.provenance_owner[@intFromEnum(assign.target)] = null,
                .set_local => |assign| self.provenance_owner[@intFromEnum(assign.target)] = null,
                .for_list => |loop_stmt| self.provenance_owner[@intFromEnum(loop_stmt.elem)] = loop_stmt.iterable,
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

                var candidates = self.allocator.alloc(?LocalId, params.len) catch unreachable;
                defer self.allocator.free(candidates);
                @memset(candidates, null);

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
                        const owner = self.explicitOwnerForLocal(arg_local);
                        if (incoming_count == 1) {
                            candidates[i] = owner;
                        } else if (candidates[i] != owner) {
                            candidates[i] = null;
                        }
                    }
                }

                if (incoming_count == 0) continue;

                for (params, 0..) |param, i| {
                    if (self.localCanOwnRefcount(param)) {
                        self.provenance_owner[@intFromEnum(param)] = null;
                        continue;
                    }
                    const param_idx = @intFromEnum(param);
                    if (self.provenance_owner[param_idx] == candidates[i]) continue;
                    self.provenance_owner[param_idx] = candidates[i];
                    changed = true;
                }
            }
        }
    }

    fn explicitOwnerForLocal(self: *const ProcPass, local: LocalId) ?LocalId {
        if (self.provenance_owner[@intFromEnum(local)]) |owner| return owner;

        const local_idx = @intFromEnum(local);
        if (!self.owned_locals.isSet(local_idx)) return null;

        const layout_idx = self.store.getLocal(local).layout_idx;
        if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) return null;
        return local;
    }

    fn recordResultOwner(self: *ProcPass, target: LocalId, result: LIR.ResultSemantics) void {
        self.provenance_owner[@intFromEnum(target)] = switch (result) {
            .alias_of => |alias| alias.owner,
            .borrow_of => |borrow| borrow.owner,
            .fresh => null,
        };
    }

    fn computeLiveness(self: *ProcPass) Allocator.Error!void {
        var changed = true;
        while (changed) {
            changed = false;
            var stmt_idx = self.reachable_stmt_ids.len;
            while (stmt_idx > 0) {
                stmt_idx -= 1;
                const id = self.reachable_stmt_ids[stmt_idx];

                var new_out = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
                defer new_out.deinit(self.allocator);
                try self.computeLiveOut(id, &new_out);

                var new_in = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
                defer new_in.deinit(self.allocator);
                try self.transfer(id, &new_out, &new_in);

                if (!bitsetsEqual(self.live_out[stmt_idx], new_out)) {
                    self.live_out[stmt_idx].unsetAll();
                    self.live_out[stmt_idx].setUnion(new_out);
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

    fn stmtSlot(self: *const ProcPass, stmt_id: CFStmtId) ?usize {
        return self.stmt_slots.get(@intFromEnum(stmt_id));
    }

    fn transfer(
        self: *ProcPass,
        stmt_id: CFStmtId,
        out: *const std.DynamicBitSetUnmanaged,
        in: *std.DynamicBitSetUnmanaged,
    ) Allocator.Error!void {
        in.setUnion(out.*);

        var defs = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer defs.deinit(self.allocator);
        var uses = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer uses.deinit(self.allocator);
        var ownership_passed = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer ownership_passed.deinit(self.allocator);

        self.collectDefsUses(stmt_id, &defs, &uses, &ownership_passed);
        setDifferenceInPlace(in, defs);
        setDifferenceInPlace(in, ownership_passed);
        in.setUnion(uses);
        self.expandLiveOwners(in);
    }

    fn expandLiveOwners(self: *const ProcPass, bits: *std.DynamicBitSetUnmanaged) void {
        var changed = true;
        while (changed) {
            changed = false;
            var local_idx: usize = 0;
            while (local_idx < bits.bit_length) : (local_idx += 1) {
                if (!bits.isSet(local_idx)) continue;
                const owner = self.provenance_owner[local_idx] orelse continue;
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
                self.markRefOpUses(assign.op, uses);
                if (assign.result == .fresh) {
                    switch (assign.op) {
                        .local => |source| self.markOwnershipPassedLocal(source, ownership_passed),
                        else => {},
                    }
                }
            },
            .assign_literal => |assign| defs.set(@intFromEnum(assign.target)),
            .assign_call => |assign| {
                defs.set(@intFromEnum(assign.target));
                self.markSpanUses(assign.args, uses);
            },
            .assign_call_indirect => |assign| {
                defs.set(@intFromEnum(assign.target));
                uses.set(@intFromEnum(assign.closure));
                self.markSpanUses(assign.args, uses);
            },
            .assign_low_level => |assign| {
                defs.set(@intFromEnum(assign.target));
                const args = self.store.getLocalSpan(assign.args);
                for (args, 0..) |arg, i| {
                    uses.set(@intFromEnum(arg));
                    if (i < assign.op.getArgOwnership().len and assign.op.getArgOwnership()[i] == .consume) {
                        self.markOwnershipPassedLocal(arg, ownership_passed);
                    }
                }
            },
            .assign_list => |assign| {
                defs.set(@intFromEnum(assign.target));
                self.markSpanUses(assign.elems, uses);
                if (assign.result == .fresh) {
                    self.markSpanOwnershipPassed(assign.elems, ownership_passed);
                }
            },
            .assign_struct => |assign| {
                defs.set(@intFromEnum(assign.target));
                self.markSpanUses(assign.fields, uses);
                if (assign.result == .fresh) {
                    self.markSpanOwnershipPassed(assign.fields, ownership_passed);
                }
            },
            .assign_tag => |assign| {
                defs.set(@intFromEnum(assign.target));
                if (assign.payload) |payload| {
                    uses.set(@intFromEnum(payload));
                    if (assign.result == .fresh) {
                        self.markOwnershipPassedLocal(payload, ownership_passed);
                    }
                }
            },
            .set_local => |assign| {
                defs.set(@intFromEnum(assign.target));
                uses.set(@intFromEnum(assign.value));
                self.markOwnershipPassedLocal(assign.value, ownership_passed);
            },
            .debug => |debug_stmt| uses.set(@intFromEnum(debug_stmt.message)),
            .expect => |expect_stmt| uses.set(@intFromEnum(expect_stmt.condition)),
            .incref => |inc| uses.set(@intFromEnum(inc.value)),
            .decref => |dec| uses.set(@intFromEnum(dec.value)),
            .free => |free_stmt| uses.set(@intFromEnum(free_stmt.value)),
            .switch_stmt => |sw| uses.set(@intFromEnum(sw.cond)),
            .borrow_scope => {},
            .for_list => |for_stmt| {
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
                                self.markOwnershipPassedLocal(arg, ownership_passed);
                            }
                        }
                    }
                }
            },
            .ret => |ret_stmt| uses.set(@intFromEnum(ret_stmt.value)),
            .scope_exit, .runtime_error, .crash, .loop_continue => {},
        }
    }

    fn markRefOpUses(self: *ProcPass, op: LIR.RefOp, uses: *std.DynamicBitSetUnmanaged) void {
        _ = self;
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
            self.markOwnershipPassedLocal(local, ownership_passed);
        }
    }

    fn markOwnershipPassedLocal(self: *ProcPass, local: LocalId, ownership_passed: *std.DynamicBitSetUnmanaged) void {
        if (!self.localCanOwnRefcount(local)) return;
        const layout_idx = self.store.getLocal(local).layout_idx;
        if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) return;
        ownership_passed.set(@intFromEnum(local));
    }

    fn localCanOwnRefcount(self: *const ProcPass, local: LocalId) bool {
        const idx = @intFromEnum(local);
        return self.owned_locals.isSet(idx);
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

                const body = try self.rewriteStmt(for_stmt.body);
                const next = try self.rewriteStmt(for_stmt.next);
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
            .scope_exit, .jump, .ret, .runtime_error, .crash, .loop_continue => stmt_id,
        };

        if (!self.rewritten_stmt_ids.contains(@intFromEnum(stmt_id))) {
            try self.rewritten_stmt_ids.put(@intFromEnum(stmt_id), rewritten);
        }
        return rewritten;
    }

    fn rewriteLinear(self: *ProcPass, original_stmt_id: CFStmtId, stmt_template: CFStmt, next_stmt: CFStmtId) Allocator.Error!CFStmtId {
        const rewritten_next = try self.rewriteStmt(next_stmt);
        const with_drops = try self.prependEdgeDrops(original_stmt_id, rewritten_next, next_stmt);
        const rewritten_stmt = try switch (stmt_template) {
            .assign_symbol => |assign| self.store.addCFStmt(.{ .assign_symbol = .{ .target = assign.target, .symbol = assign.symbol, .next = with_drops } }),
            .assign_ref => |assign| self.store.addCFStmt(.{ .assign_ref = .{ .target = assign.target, .result = assign.result, .op = assign.op, .next = with_drops } }),
            .assign_literal => |assign| self.store.addCFStmt(.{ .assign_literal = .{ .target = assign.target, .result = assign.result, .value = assign.value, .next = with_drops } }),
            .assign_call => |assign| self.store.addCFStmt(.{ .assign_call = .{ .target = assign.target, .result = assign.result, .proc = assign.proc, .args = assign.args, .next = with_drops } }),
            .assign_call_indirect => |assign| self.store.addCFStmt(.{ .assign_call_indirect = .{ .target = assign.target, .result = assign.result, .ownership = assign.ownership, .closure = assign.closure, .args = assign.args, .capture_layout = assign.capture_layout, .next = with_drops } }),
            .assign_low_level => |assign| self.store.addCFStmt(.{ .assign_low_level = .{ .target = assign.target, .result = assign.result, .ownership = assign.ownership, .op = assign.op, .args = assign.args, .next = with_drops } }),
            .assign_list => |assign| self.store.addCFStmt(.{ .assign_list = .{ .target = assign.target, .result = assign.result, .ownership = assign.ownership, .elems = assign.elems, .next = with_drops } }),
            .assign_struct => |assign| self.store.addCFStmt(.{ .assign_struct = .{ .target = assign.target, .result = assign.result, .ownership = assign.ownership, .fields = assign.fields, .next = with_drops } }),
            .assign_tag => |assign| self.store.addCFStmt(.{ .assign_tag = .{ .target = assign.target, .result = assign.result, .ownership = assign.ownership, .discriminant = assign.discriminant, .payload = assign.payload, .next = with_drops } }),
            .set_local => |assign| self.store.addCFStmt(.{ .set_local = .{ .target = assign.target, .value = assign.value, .next = with_drops } }),
            .debug => |debug_stmt| self.store.addCFStmt(.{ .debug = .{ .message = debug_stmt.message, .next = with_drops } }),
            .expect => |expect_stmt| self.store.addCFStmt(.{ .expect = .{ .condition = expect_stmt.condition, .next = with_drops } }),
            .incref => |inc| self.store.addCFStmt(.{ .incref = .{ .value = inc.value, .count = inc.count, .next = with_drops } }),
            .decref => |dec| self.store.addCFStmt(.{ .decref = .{ .value = dec.value, .next = with_drops } }),
            .free => |free_stmt| self.store.addCFStmt(.{ .free = .{ .value = free_stmt.value, .next = with_drops } }),
            else => unreachable,
        };
        return try self.prependOwnershipRetains(original_stmt_id, rewritten_stmt, next_stmt);
    }

    fn prependEdgeDrops(self: *ProcPass, from_stmt: CFStmtId, successor_rewritten: CFStmtId, original_successor: CFStmtId) Allocator.Error!CFStmtId {
        const from_slot = self.stmtSlot(from_stmt) orelse return successor_rewritten;
        const succ_slot = self.stmtSlot(original_successor) orelse return successor_rewritten;

        var dead = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer dead.deinit(self.allocator);
        var ownership_passed = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer ownership_passed.deinit(self.allocator);

        try self.collectOwnershipPassed(from_stmt, &ownership_passed);

        dead.setUnion(self.live_in[from_slot]);
        setDifferenceInPlace(&dead, self.live_in[succ_slot]);
        dead.setIntersection(self.owned_locals);
        setDifferenceInPlace(&dead, ownership_passed);

        var cursor = successor_rewritten;
        var local_idx = self.local_count;
        while (local_idx > 0) {
            local_idx -= 1;
            if (!dead.isSet(local_idx)) continue;
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
            if (ownership_passed.isSet(local_idx) and self.live_in[succ_slot].isSet(local_idx)) {
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

    fn collectOwnershipPassed(self: *ProcPass, stmt_id: CFStmtId, ownership_passed: *std.DynamicBitSetUnmanaged) Allocator.Error!void {
        var defs = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer defs.deinit(self.allocator);
        var uses = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.local_count);
        defer uses.deinit(self.allocator);
        self.collectDefsUses(stmt_id, &defs, &uses, ownership_passed);
    }

    fn collectRetainedInputIncrefs(self: *ProcPass, stmt_id: CFStmtId, retained_counts: []u16) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_ref => |assign| {
                if (assign.result != .fresh) return;
                switch (assign.op) {
                    .local => |source| self.accumulateFreshInputRetain(source, retained_counts),
                    else => {},
                }
            },
            .assign_list => |assign| {
                if (assign.result != .fresh) return;
                try self.accumulateFreshInputRetains(self.store.getLocalSpan(assign.elems), retained_counts);
            },
            .assign_struct => |assign| {
                if (assign.result != .fresh) return;
                try self.accumulateFreshInputRetains(self.store.getLocalSpan(assign.fields), retained_counts);
            },
            .assign_tag => |assign| {
                if (assign.result != .fresh) return;
                const payload = assign.payload orelse return;
                self.accumulateFreshInputRetain(payload, retained_counts);
            },
            .assign_low_level => |assign| {
                const args = self.store.getLocalSpan(assign.args);
                for (args, 0..) |arg, i| {
                    if (!assign.op.borrowedArgRetainedByResult(i)) continue;
                    self.incrementRetainCount(arg, retained_counts, 1);
                }
            },
            else => {},
        }
    }

    fn collectRequiredOwnedInputIncrefs(self: *ProcPass, stmt_id: CFStmtId, retained_counts: []u16) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_low_level => |assign| {
                const args = self.store.getLocalSpan(assign.args);
                for (args, 0..) |arg, i| {
                    if (i >= assign.op.getArgOwnership().len) continue;
                    if (assign.op.getArgOwnership()[i] != .consume) continue;
                    self.accumulateBorrowedConsumeRetain(arg, retained_counts);
                }
            },
            .set_local => |assign| self.accumulateBorrowedConsumeRetain(assign.value, retained_counts),
            else => {},
        }
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
            self.incrementRetainCount(local, retained_counts, retain_count);
        }
    }

    fn accumulateFreshInputRetain(self: *ProcPass, local: LocalId, retained_counts: []u16) void {
        const retain_count = self.freshInputRetainCount(local, 1);
        if (retain_count == 0) return;
        self.incrementRetainCount(local, retained_counts, retain_count);
    }

    fn accumulateBorrowedConsumeRetain(self: *ProcPass, local: LocalId, retained_counts: []u16) void {
        const layout_idx = self.store.getLocal(local).layout_idx;
        if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) return;
        if (self.localCanOwnRefcount(local)) return;
        self.incrementRetainCount(local, retained_counts, 1);
    }

    fn freshInputRetainCount(self: *ProcPass, local: LocalId, occurrence_count: u16) u16 {
        const layout_idx = self.store.getLocal(local).layout_idx;
        if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) return 0;

        if (self.localCanOwnRefcount(local)) {
            return occurrence_count -| 1;
        }

        return occurrence_count;
    }

    fn incrementRetainCount(self: *ProcPass, local: LocalId, retained_counts: []u16, delta: u16) void {
        _ = self;
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
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.collectLoopContinueTargets(assign.next, for_stack),
            .assign_ref => |assign| try self.collectLoopContinueTargets(assign.next, for_stack),
            .assign_literal => |assign| try self.collectLoopContinueTargets(assign.next, for_stack),
            .assign_call => |assign| try self.collectLoopContinueTargets(assign.next, for_stack),
            .assign_call_indirect => |assign| try self.collectLoopContinueTargets(assign.next, for_stack),
            .assign_low_level => |assign| try self.collectLoopContinueTargets(assign.next, for_stack),
            .assign_list => |assign| try self.collectLoopContinueTargets(assign.next, for_stack),
            .assign_struct => |assign| try self.collectLoopContinueTargets(assign.next, for_stack),
            .assign_tag => |assign| try self.collectLoopContinueTargets(assign.next, for_stack),
            .set_local => |assign| try self.collectLoopContinueTargets(assign.next, for_stack),
            .debug => |debug_stmt| try self.collectLoopContinueTargets(debug_stmt.next, for_stack),
            .expect => |expect_stmt| try self.collectLoopContinueTargets(expect_stmt.next, for_stack),
            .incref => |inc| try self.collectLoopContinueTargets(inc.next, for_stack),
            .decref => |dec| try self.collectLoopContinueTargets(dec.next, for_stack),
            .free => |free_stmt| try self.collectLoopContinueTargets(free_stmt.next, for_stack),
            .switch_stmt => |sw| {
                try self.collectLoopContinueTargets(sw.default_branch, for_stack);
                for (self.store.getCFSwitchBranches(sw.branches)) |branch| {
                    try self.collectLoopContinueTargets(branch.body, for_stack);
                }
            },
            .borrow_scope => |scope| {
                try self.collectLoopContinueTargets(scope.body, for_stack);
                try self.collectLoopContinueTargets(scope.remainder, for_stack);
            },
            .for_list => |for_stmt| {
                var nested = try self.allocator.alloc(CFStmtId, for_stack.len + 1);
                defer self.allocator.free(nested);
                @memcpy(nested[0..for_stack.len], for_stack);
                nested[for_stack.len] = stmt_id;
                try self.collectLoopContinueTargets(for_stmt.body, nested);
                try self.collectLoopContinueTargets(for_stmt.next, for_stack);
            },
            .join => |join| {
                try self.collectLoopContinueTargets(join.body, for_stack);
                try self.collectLoopContinueTargets(join.remainder, for_stack);
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
    var ordered = std.ArrayListUnmanaged(CFStmtId).empty;
    defer ordered.deinit(allocator);

    var stack = std.ArrayListUnmanaged(CFStmtId).empty;
    defer stack.deinit(allocator);

    var visited = std.AutoHashMap(u32, void).init(allocator);
    defer visited.deinit();

    try stack.append(allocator, body);

    while (stack.pop()) |stmt_id| {
        const gop = try visited.getOrPut(@intFromEnum(stmt_id));
        if (gop.found_existing) continue;

        try ordered.append(allocator, stmt_id);
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

    return ordered.toOwnedSlice(allocator);
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
