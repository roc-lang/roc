//! Removes LIR switch edges whose discriminants cannot be produced.
//!
//! The pass is intentionally layout-neutral: it tracks values that LIR
//! statements construct or return, but it never changes tag-union layouts,
//! variant slots, or discriminants. It runs before ARC so deleted branches do
//! not need ownership repair.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const core = @import("lir_core");
const BodyClone = @import("body_clone.zig");

const LIR = core.LIR;
const LirProgram = core.Program;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

/// Analyze possible tag values and bypass switches that cannot take some of
/// their outgoing edges.
pub fn run(result: *LirProgram.Result) Allocator.Error!void {
    var pass = try Pass.init(result);
    defer pass.deinit();
    try pass.run();
}

const payload_struct_slot = std.math.maxInt(u32);

const PayloadSlot = struct {
    variant: u32,
    payload: u32,
};

const TagSet = struct {
    all: bool = false,
    /// Known discriminants in first-seen order.
    values: std.AutoArrayHashMapUnmanaged(u32, void) = .empty,

    fn deinit(self: *TagSet, allocator: Allocator) void {
        self.values.deinit(allocator);
    }

    fn markAll(self: *TagSet, allocator: Allocator) bool {
        if (self.all) return false;
        self.values.clearAndFree(allocator);
        self.all = true;
        return true;
    }

    fn add(self: *TagSet, allocator: Allocator, value: u32) Allocator.Error!bool {
        if (self.all) return false;
        const entry = try self.values.getOrPut(allocator, value);
        return !entry.found_existing;
    }

    fn mergeFrom(self: *TagSet, allocator: Allocator, other: *const TagSet) Allocator.Error!bool {
        if (self.all) return false;
        if (other.all) return self.markAll(allocator);
        var changed = false;
        for (other.values.keys()) |value| {
            if (try self.add(allocator, value)) changed = true;
        }
        return changed;
    }

    fn contains(self: *const TagSet, value: u32) bool {
        if (self.all) return true;
        return self.values.contains(value);
    }

    fn singleton(self: *const TagSet) ?u32 {
        if (self.all or self.values.count() != 1) return null;
        return self.values.keys()[0];
    }

    fn hasKnownValues(self: *const TagSet) bool {
        return !self.all and self.values.count() != 0;
    }
};

/// Producer facts for one whole LIR value. `constructions` names the finite LIR
/// statements that can construct the value; projections follow those graph
/// edges on demand. Keeping references instead of recursively copying child
/// facts is essential for recursive values and shared constructor DAGs: their
/// abstract state must be bounded by the finite LIR graph, not by the number of
/// paths through an infinitely unfolding value tree.
const ValueInfo = struct {
    tags: TagSet = .{},
    constructions: std.ArrayList(LIR.CFStmtId) = .empty,

    fn deinit(self: *ValueInfo, allocator: Allocator) void {
        self.constructions.deinit(allocator);
        self.tags.deinit(allocator);
    }

    fn markAll(self: *ValueInfo, allocator: Allocator) bool {
        var changed = self.tags.markAll(allocator);
        if (self.constructions.items.len != 0) {
            self.constructions.clearAndFree(allocator);
            changed = true;
        }
        return changed;
    }

    fn mergeFrom(self: *ValueInfo, allocator: Allocator, other: *const ValueInfo) Allocator.Error!bool {
        if (self.tags.all) return false;
        if (other.tags.all) return self.markAll(allocator);
        var changed = try self.tags.mergeFrom(allocator, &other.tags);
        for (other.constructions.items) |construction| {
            if (try self.addConstruction(allocator, construction)) changed = true;
        }
        return changed;
    }

    fn addConstruction(self: *ValueInfo, allocator: Allocator, construction: LIR.CFStmtId) Allocator.Error!bool {
        if (self.tags.all) return false;
        for (self.constructions.items) |existing| {
            if (existing == construction) return false;
        }
        try self.constructions.append(allocator, construction);
        return true;
    }
};

const Pass = struct {
    result: *LirProgram.Result,
    store: *LirStore,
    allocator: Allocator,
    local_info: []ValueInfo,
    proc_returns: []ValueInfo,
    analysis: BodyClone.AnalysisScratch,
    redirects: collections.DenseMap(LIR.CFStmtId, LIR.CFStmtId),
    use_counts: []u32,

    fn init(result: *LirProgram.Result) Allocator.Error!Pass {
        const allocator = result.store.allocator;
        const local_info = try allocator.alloc(ValueInfo, result.store.localCount());
        errdefer allocator.free(local_info);
        @memset(local_info, .{});

        const proc_returns = try allocator.alloc(ValueInfo, result.store.procSpecCount());
        errdefer allocator.free(proc_returns);
        @memset(proc_returns, .{});

        const use_counts = try allocator.alloc(u32, result.store.localCount());
        errdefer allocator.free(use_counts);
        @memset(use_counts, 0);

        return .{
            .result = result,
            .store = &result.store,
            .allocator = allocator,
            .local_info = local_info,
            .proc_returns = proc_returns,
            .analysis = BodyClone.AnalysisScratch.init(allocator),
            .redirects = collections.DenseMap(LIR.CFStmtId, LIR.CFStmtId).init(allocator),
            .use_counts = use_counts,
        };
    }

    fn deinit(self: *Pass) void {
        self.allocator.free(self.use_counts);
        self.redirects.deinit();
        self.analysis.deinit();
        for (self.proc_returns) |*ret| ret.deinit(self.allocator);
        self.allocator.free(self.proc_returns);
        for (self.local_info) |*info| info.deinit(self.allocator);
        self.allocator.free(self.local_info);
    }

    fn run(self: *Pass) Allocator.Error!void {
        try self.seedBoundaries();
        try self.analyze();
        self.collectUseCounts();
        try self.rewriteSwitches();
        try self.removeDeadDiscriminantReads();
        self.patchRedirects();
    }

    fn seedBoundaries(self: *Pass) Allocator.Error!void {
        for (0..self.store.procSpecCount()) |proc_index| {
            const proc = self.store.getProcSpec(@fromBackingInt(@intCast(@as(u32, @intCast(proc_index)))));
            if (proc.body == null or proc.hosted != null) {
                _ = self.proc_returns[proc_index].markAll(self.allocator);
            }
            const args = self.store.getLocalSpan(proc.args);
            for (0..args.len) |index| {
                const arg = GuardedList.at(args, index);
                _ = self.local_info[@backingInt(arg)].markAll(self.allocator);
            }
        }
    }

    fn analyze(self: *Pass) Allocator.Error!void {
        var changed = true;
        while (changed) {
            changed = false;
            for (0..self.store.procSpecCount()) |proc_index| {
                const proc = self.store.getProcSpec(@fromBackingInt(@intCast(@as(u32, @intCast(proc_index)))));
                const body = proc.body orelse continue;
                if (try self.analyzeProc(@fromBackingInt(@intCast(@as(u32, @intCast(proc_index)))), body)) {
                    changed = true;
                }
            }
        }
    }

    fn analyzeProc(self: *Pass, proc_id: LIR.LirProcSpecId, body: LIR.CFStmtId) Allocator.Error!bool {
        var changed = false;
        var stmts = try BodyClone.ReachableStmts.initWithScratch(self.store, body, &self.analysis);
        defer stmts.deinit();
        while (try stmts.next()) |current| {
            if (try self.analyzeStmt(proc_id, current, self.store.getCFStmt(current))) changed = true;
        }
        return changed;
    }

    /// What one statement contributes to the values its target or the
    /// procedure's return can hold; successors are the walker's concern.
    fn analyzeStmt(self: *Pass, proc_id: LIR.LirProcSpecId, stmt_id: LIR.CFStmtId, stmt: LIR.CFStmt) Allocator.Error!bool {
        switch (stmt) {
            .assign_ref => |s| return try self.mergeRef(s.target, s.op),
            .assign_call => |s| {
                const callee_index = @backingInt(s.proc);
                if (callee_index >= self.proc_returns.len) tagReachabilityInvariant("callee proc id exceeded proc table");
                return try self.localInfoMut(s.target).mergeFrom(self.allocator, &self.proc_returns[callee_index]);
            },
            .assign_call_erased => |inline_s| {
                const s = self.store.stmtRow(inline_s.data);
                var changed = self.localInfoMut(inline_s.target).markAll(self.allocator);
                if (s.out_desc) |out_desc| {
                    if (self.localInfoMut(out_desc).markAll(self.allocator)) changed = true;
                }
                return changed;
            },
            .assign_struct => |s| return try self.localInfoMut(s.target).addConstruction(self.allocator, stmt_id),
            .assign_tag => |s| {
                const target = self.localInfoMut(s.target);
                var changed = try target.tags.add(self.allocator, s.discriminant);
                if (try target.addConstruction(self.allocator, stmt_id)) changed = true;
                return changed;
            },
            .set_local => |s| return try self.localInfoMut(s.target).mergeFrom(self.allocator, self.localInfo(s.value)),
            .ret => |s| return try self.proc_returns[@backingInt(proc_id)].mergeFrom(self.allocator, self.localInfo(s.value)),
            // Every other single-target definition can hold any value.
            inline .init_uninitialized,
            .assign_literal,
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
            .assign_call_dict,
            .assign_low_level,
            .assign_list,
            => |s| return self.localInfoMut(s.target).markAll(self.allocator),
            .boxy_tag_match,
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
            .join,
            .jump,
            .crash,
            => return false,
        }
    }

    fn mergeRef(self: *Pass, target: LIR.LocalId, op: LIR.RefOp) Allocator.Error!bool {
        return switch (op.kind) {
            .local => self.localInfoMut(target).mergeFrom(self.allocator, self.localInfo(op.source)),
            .nominal => self.localInfoMut(target).mergeFrom(self.allocator, self.localInfo(op.source)),
            .list_reinterpret => self.localInfoMut(target).mergeFrom(self.allocator, self.localInfo(op.source)),
            .discriminant => self.localInfoMut(target).tags.mergeFrom(self.allocator, &self.localInfo(op.source).tags),
            .field => self.mergeFieldRead(target, op.source, op.index),
            .tag_payload => self.mergePayloadRead(target, op.source, .{
                .variant = op.tag_discriminant,
                .payload = op.index,
            }),
            .tag_payload_struct => self.mergePayloadRead(target, op.source, .{
                .variant = op.tag_discriminant,
                .payload = payload_struct_slot,
            }),
        };
    }

    fn mergeFieldRead(self: *Pass, target: LIR.LocalId, source: LIR.LocalId, field_index: u32) Allocator.Error!bool {
        const source_info = self.localInfo(source);
        if (source_info.tags.all) return self.localInfoMut(target).markAll(self.allocator);
        return self.mergeFieldFromInfo(target, source_info, field_index);
    }

    fn mergeFieldFromInfo(self: *Pass, target: LIR.LocalId, source: *const ValueInfo, field_index: u32) Allocator.Error!bool {
        if (source.tags.all) return self.localInfoMut(target).markAll(self.allocator);
        var changed = false;
        for (source.constructions.items) |construction| {
            const stmt = self.store.getCFStmt(construction);
            if (stmt != .assign_struct) continue;
            const fields = self.store.getLocalSpan(stmt.assign_struct.fields);
            if (field_index >= fields.len) tagReachabilityInvariant("field projection exceeded its constructor arity");
            const field = GuardedList.at(fields, field_index);
            if (try self.localInfoMut(target).mergeFrom(self.allocator, self.localInfo(field))) changed = true;
        }
        return changed;
    }

    fn mergePayloadRead(self: *Pass, target: LIR.LocalId, source: LIR.LocalId, slot: PayloadSlot) Allocator.Error!bool {
        const source_info = self.localInfo(source);
        if (source_info.tags.all) return self.localInfoMut(target).markAll(self.allocator);
        if (!source_info.tags.contains(slot.variant)) return false;
        var changed = false;
        for (source_info.constructions.items) |construction| {
            const stmt = self.store.getCFStmt(construction);
            if (stmt != .assign_tag or stmt.assign_tag.discriminant != slot.variant) continue;
            const payload = stmt.assign_tag.payload orelse continue;
            if (slot.payload == payload_struct_slot) {
                if (try self.localInfoMut(target).mergeFrom(self.allocator, self.localInfo(payload))) changed = true;
            } else if (try self.mergeFieldFromInfo(target, self.localInfo(payload), slot.payload)) {
                changed = true;
            }
        }
        return changed;
    }

    fn collectUseCounts(self: *Pass) void {
        @memset(self.use_counts, 0);
        for (self.store.getCFStmts()) |stmt| BodyClone.forEachStmtRead(self.store, stmt, self, noteUse);
    }

    fn rewriteSwitches(self: *Pass) Allocator.Error!void {
        var stmt_index: usize = 0;
        while (stmt_index < self.store.cfStmtCount()) : (stmt_index += 1) {
            const stmt_id: LIR.CFStmtId = @fromBackingInt(@intCast(@as(u32, @intCast(stmt_index))));
            const stmt = self.store.getCFStmt(stmt_id);
            if (stmt != .switch_stmt) continue;
            const switch_stmt = stmt.switch_stmt;
            const tags = &self.localInfo(switch_stmt.cond).tags;
            if (!tags.hasKnownValues()) continue;

            if (tags.singleton()) |value| {
                try self.redirects.put(stmt_id, self.targetForDiscriminant(switch_stmt, value));
                continue;
            }

            const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
            var kept = std.ArrayList(LIR.CFSwitchBranch).empty;
            defer kept.deinit(self.allocator);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                if (branch.value > std.math.maxInt(u32)) continue;
                if (tags.contains(@intCast(branch.value))) {
                    try kept.append(self.allocator, branch);
                }
            }

            if (kept.items.len == 0) {
                try self.redirects.put(stmt_id, switch_stmt.default_branch);
            } else if (kept.items.len != branches.len) {
                self.store.getCFStmtPtr(stmt_id).* = .{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = try self.store.addCFSwitchBranches(kept.items),
                    .default_branch = switch_stmt.default_branch,
                    .continuation = switch_stmt.continuation,
                } };
            }
        }
    }

    fn targetForDiscriminant(self: *Pass, switch_stmt: anytype, value: u32) LIR.CFStmtId {
        const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
        for (0..branches.len) |index| {
            const branch = GuardedList.at(branches, index);
            if (branch.value == value) return branch.body;
        }
        return switch_stmt.default_branch;
    }

    fn removeDeadDiscriminantReads(self: *Pass) Allocator.Error!void {
        var stmt_index: usize = 0;
        while (stmt_index < self.store.cfStmtCount()) : (stmt_index += 1) {
            const stmt_id: LIR.CFStmtId = @fromBackingInt(@intCast(@as(u32, @intCast(stmt_index))));
            const stmt = self.store.getCFStmt(stmt_id);
            if (stmt != .assign_ref) continue;
            const assign = stmt.assign_ref;
            if (assign.op.kind != .discriminant) continue;
            const next_stmt = self.store.getCFStmt(assign.next);
            if (next_stmt != .switch_stmt) continue;
            const switch_stmt = next_stmt.switch_stmt;
            if (switch_stmt.cond != assign.target) continue;
            if (self.useCount(assign.target) != 1) continue;
            const next = self.resolveRedirect(assign.next);
            if (next != assign.next) try self.redirects.put(stmt_id, next);
        }
    }

    fn patchRedirects(self: *Pass) void {
        if (self.redirects.count() == 0) return;

        for (0..self.store.procSpecCount()) |proc_index| {
            const proc = self.store.getProcSpecPtr(@fromBackingInt(@intCast(@as(u32, @intCast(proc_index)))));
            if (proc.body) |body| proc.body = self.resolveRedirect(body);
            const join_points = self.store.getJoinPointSpanMut(proc.join_points);
            for (0..join_points.len) |index| {
                const join_point = GuardedList.atPtr(join_points, index);
                join_point.body = self.resolveRedirect(join_point.body);
            }
        }

        for (0..self.store.cfStmtCount()) |stmt_index| {
            BodyClone.redirectSuccessors(self.store, @fromBackingInt(@intCast(@as(u32, @intCast(stmt_index)))), self, resolveRedirect);
        }
    }

    fn resolveRedirect(self: *Pass, stmt: LIR.CFStmtId) LIR.CFStmtId {
        var current = stmt;
        var steps: usize = 0;
        while (self.redirects.get(current)) |next| {
            current = next;
            steps += 1;
            if (steps > self.redirects.count()) tagReachabilityInvariant("redirect cycle detected");
        }
        return current;
    }

    fn localInfo(self: *const Pass, local: LIR.LocalId) *const ValueInfo {
        return &self.local_info[@backingInt(local)];
    }

    fn localInfoMut(self: *Pass, local: LIR.LocalId) *ValueInfo {
        return &self.local_info[@backingInt(local)];
    }

    fn noteUse(self: *Pass, local: LIR.LocalId) void {
        self.use_counts[@backingInt(local)] += 1;
    }

    fn useCount(self: *Pass, local: LIR.LocalId) u32 {
        return self.use_counts[@backingInt(local)];
    }
};

fn tagReachabilityInvariant(comptime message: []const u8) noreturn {
    base.invariant("tag reachability invariant violated: {s}", .{message});
}

test "tag reachability declarations are referenced" {
    std.testing.refAllDecls(@This());
}

const testing = std.testing;
const layout = @import("layout");

const TestProgram = struct {
    result: LirProgram.Result,
    inner_layout: layout.Idx,
    outer_layout: layout.Idx,

    fn init(allocator: Allocator) Allocator.Error!TestProgram {
        var result = try LirProgram.Result.init(allocator, .u64);
        errdefer result.deinit();
        const inner_layout = layout.Idx.bool;
        const outer_layout = try result.layouts.putTagUnion(&[_]layout.Idx{ inner_layout, .zst });
        return .{
            .result = result,
            .inner_layout = inner_layout,
            .outer_layout = outer_layout,
        };
    }

    fn deinit(self: *TestProgram) void {
        self.result.deinit();
    }

    fn local(self: *TestProgram, layout_idx: layout.Idx) Allocator.Error!LIR.LocalId {
        return try self.result.store.addLocal(.{ .layout_idx = layout_idx });
    }
};

test "tag reachability bypasses call result and direct payload switches" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const callee_inner = try f.local(f.inner_layout);
    const callee_ret = try f.local(f.outer_layout);
    const callee_ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = callee_ret } }, .test_fixture);
    const callee_assign_ret = try store.addCFStmt(.{ .assign_tag = .{
        .target = callee_ret,
        .variant_index = 0,
        .discriminant = 0,
        .payload = callee_inner,
        .next = callee_ret_stmt,
    } }, .test_fixture);
    const callee_body = try store.addCFStmt(.{ .assign_tag = .{
        .target = callee_inner,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = callee_assign_ret,
    } }, .test_fixture);
    const callee = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .identity = LIR.ProcIdentity.forTest(8),
        .args = LIR.LocalSpan.empty(),
        .body = callee_body,
        .ret_layout = f.outer_layout,
    }, .none);

    const caller_ret = try f.local(f.inner_layout);
    const call_result = try f.local(f.outer_layout);
    const outer_disc = try f.local(.u16);
    const payload = try f.local(f.inner_layout);
    const inner_disc = try f.local(.u16);

    const success = try store.addCFStmt(.{ .ret = .{ .value = caller_ret } }, .test_fixture);
    const bad_outer = try store.addCFStmt(.{ .runtime_error = {} }, .test_fixture);
    const bad_inner = try store.addCFStmt(.{ .runtime_error = {} }, .test_fixture);
    const inner_switch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = inner_disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 0, .body = bad_inner }}),
        .default_branch = success,
    } }, .test_fixture);
    const inner_disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = inner_disc,
        .op = .discriminant(payload),
        .next = inner_switch,
    } }, .test_fixture);
    const read_payload = try store.addCFStmt(.{ .assign_ref = .{
        .target = payload,
        .op = .tagPayloadStruct(call_result, 0, 0),
        .next = inner_disc_read,
    } }, .test_fixture);
    const outer_switch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = outer_disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 1, .body = bad_outer }}),
        .default_branch = read_payload,
    } }, .test_fixture);
    const outer_disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = outer_disc,
        .op = .discriminant(call_result),
        .next = outer_switch,
    } }, .test_fixture);
    const caller_body = try store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = callee,
        .args = LIR.LocalSpan.empty(),
        .next = outer_disc_read,
    } }, .test_fixture);
    const caller = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(2),
        .identity = LIR.ProcIdentity.forTest(3),
        .args = try store.addLocalSpan(&[_]LIR.LocalId{caller_ret}),
        .body = caller_body,
        .ret_layout = f.inner_layout,
    }, .none);
    try f.result.root_procs.append(testing.allocator, caller);

    try run(&f.result);

    const call_stmt = store.getCFStmtImmediate(caller_body).assign_call;
    try testing.expectEqual(read_payload, call_stmt.next);

    const payload_stmt = store.getCFStmtImmediate(read_payload).assign_ref;
    try testing.expectEqual(success, payload_stmt.next);
}

test "tag reachability tracks per-payload tags through payload structs" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const pair_layout = try f.result.layouts.putStructFields(&[_]layout.StructField{
        .{ .index = 0, .layout = f.inner_layout },
        .{ .index = 1, .layout = f.inner_layout },
    });
    const outer_layout = try f.result.layouts.putTagUnion(&[_]layout.Idx{pair_layout});

    const first = try f.local(f.inner_layout);
    const second = try f.local(f.inner_layout);
    const pair = try f.local(pair_layout);
    const callee_ret = try f.local(outer_layout);
    const callee_ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = callee_ret } }, .test_fixture);
    const callee_assign_ret = try store.addCFStmt(.{ .assign_tag = .{
        .target = callee_ret,
        .variant_index = 0,
        .discriminant = 0,
        .payload = pair,
        .next = callee_ret_stmt,
    } }, .test_fixture);
    const fields = try store.addLocalSpan(&[_]LIR.LocalId{ first, second });
    const assign_pair = try store.addCFStmt(.{ .assign_struct = .{
        .target = pair,
        .fields = fields,
        .next = callee_assign_ret,
    } }, .test_fixture);
    const assign_second = try store.addCFStmt(.{ .assign_tag = .{
        .target = second,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = assign_pair,
    } }, .test_fixture);
    const callee_body = try store.addCFStmt(.{ .assign_tag = .{
        .target = first,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = assign_second,
    } }, .test_fixture);
    const callee = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(4),
        .identity = LIR.ProcIdentity.forTest(7),
        .args = LIR.LocalSpan.empty(),
        .body = callee_body,
        .ret_layout = outer_layout,
    }, .none);

    const call_result = try f.local(outer_layout);
    const payload = try f.local(f.inner_layout);
    const disc = try f.local(.u16);
    const success = try store.addCFStmt(.{ .ret = .{ .value = payload } }, .test_fixture);
    const bad = try store.addCFStmt(.{ .runtime_error = {} }, .test_fixture);
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 0, .body = bad }}),
        .default_branch = success,
    } }, .test_fixture);
    const disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .discriminant(payload),
        .next = switch_stmt,
    } }, .test_fixture);
    const read_payload = try store.addCFStmt(.{ .assign_ref = .{
        .target = payload,
        .op = .tagPayload(call_result, 1, 0, 0),
        .next = disc_read,
    } }, .test_fixture);
    const caller_body = try store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = callee,
        .args = LIR.LocalSpan.empty(),
        .next = read_payload,
    } }, .test_fixture);
    const caller = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(5),
        .identity = LIR.ProcIdentity.forTest(2),
        .args = LIR.LocalSpan.empty(),
        .body = caller_body,
        .ret_layout = f.inner_layout,
    }, .none);
    try f.result.root_procs.append(testing.allocator, caller);

    try run(&f.result);

    const payload_stmt = store.getCFStmtImmediate(read_payload).assign_ref;
    try testing.expectEqual(success, payload_stmt.next);
}

test "tag reachability preserves nested tag variants extracted across a loop join" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const wrapper_layout = try f.result.layouts.putStructFields(&[_]layout.StructField{
        .{ .index = 0, .layout = f.inner_layout },
    });
    const pair_layout = try f.result.layouts.putStructFields(&[_]layout.StructField{
        .{ .index = 0, .layout = .zst },
        .{ .index = 1, .layout = wrapper_layout },
    });
    const step_layout = try f.result.layouts.putTagUnion(&[_]layout.Idx{pair_layout});

    const initial_tag = try f.local(f.inner_layout);
    const initial_wrapper = try f.local(wrapper_layout);
    const loop_slot = try f.local(wrapper_layout);
    const successor_tag = try f.local(f.inner_layout);
    const successor_wrapper = try f.local(wrapper_layout);
    const item = try f.local(.zst);
    const pair = try f.local(pair_layout);
    const step = try f.local(step_layout);
    const extracted_pair = try f.local(pair_layout);
    const extracted_wrapper = try f.local(wrapper_layout);
    const loop_inner = try f.local(f.inner_layout);
    const disc = try f.local(.u16);

    const branch_zero = try store.addCFStmt(.{ .ret = .{ .value = loop_inner } }, .test_fixture);
    const branch_one = try store.addCFStmt(.{ .ret = .{ .value = loop_inner } }, .test_fixture);
    const bad_default = try store.addCFStmt(.{ .runtime_error = {} }, .test_fixture);
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 0, .body = branch_zero },
            .{ .value = 1, .body = branch_one },
        }),
        .default_branch = bad_default,
    } }, .test_fixture);
    const disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .discriminant(loop_inner),
        .next = switch_stmt,
    } }, .test_fixture);
    const read_loop_inner = try store.addCFStmt(.{ .assign_ref = .{
        .target = loop_inner,
        .op = .field(loop_slot, 0),
        .next = disc_read,
    } }, .test_fixture);
    const merge_successor = try store.addCFStmt(.{ .set_local = .{
        .target = loop_slot,
        .value = extracted_wrapper,
        .mode = .replace_existing,
        .next = read_loop_inner,
    } }, .test_fixture);
    const read_successor = try store.addCFStmt(.{ .assign_ref = .{
        .target = extracted_wrapper,
        .op = .field(extracted_pair, 1),
        .next = merge_successor,
    } }, .test_fixture);
    const read_step_payload = try store.addCFStmt(.{ .assign_ref = .{
        .target = extracted_pair,
        .op = .tagPayloadStruct(step, 0, 0),
        .next = read_successor,
    } }, .test_fixture);
    const assign_step = try store.addCFStmt(.{ .assign_tag = .{
        .target = step,
        .variant_index = 0,
        .discriminant = 0,
        .payload = pair,
        .next = read_step_payload,
    } }, .test_fixture);
    const assign_pair = try store.addCFStmt(.{ .assign_struct = .{
        .target = pair,
        .fields = try store.addLocalSpan(&[_]LIR.LocalId{ item, successor_wrapper }),
        .next = assign_step,
    } }, .test_fixture);
    const assign_successor_wrapper = try store.addCFStmt(.{ .assign_struct = .{
        .target = successor_wrapper,
        .fields = try store.addLocalSpan(&[_]LIR.LocalId{successor_tag}),
        .next = assign_pair,
    } }, .test_fixture);
    const assign_successor_tag = try store.addCFStmt(.{ .assign_tag = .{
        .target = successor_tag,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = assign_successor_wrapper,
    } }, .test_fixture);
    const seed_loop_slot = try store.addCFStmt(.{ .set_local = .{
        .target = loop_slot,
        .value = initial_wrapper,
        .mode = .initialize_join_param,
        .next = assign_successor_tag,
    } }, .test_fixture);
    const assign_initial_wrapper = try store.addCFStmt(.{ .assign_struct = .{
        .target = initial_wrapper,
        .fields = try store.addLocalSpan(&[_]LIR.LocalId{initial_tag}),
        .next = seed_loop_slot,
    } }, .test_fixture);
    const body = try store.addCFStmt(.{ .assign_tag = .{
        .target = initial_tag,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = assign_initial_wrapper,
    } }, .test_fixture);
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(11),
        .identity = LIR.ProcIdentity.forTest(6),
        .args = LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = f.inner_layout,
    }, .none);
    try f.result.root_procs.append(testing.allocator, proc);

    try run(&f.result);

    try testing.expectEqual(disc_read, store.getCFStmtImmediate(read_loop_inner).assign_ref.next);
    const branches = store.getCFSwitchBranches(store.getCFStmtImmediate(switch_stmt).switch_stmt.branches);
    try testing.expectEqual(@as(usize, 2), branches.len);
}

test "tag reachability removes impossible explicit branches from multi-value sets" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const local = try f.local(f.inner_layout);
    const disc = try f.local(.u16);
    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
    const branch_zero = try store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
    const branch_two = try store.addCFStmt(.{ .runtime_error = {} }, .test_fixture);
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 0, .body = branch_zero },
            .{ .value = 2, .body = branch_two },
        }),
        .default_branch = ret_stmt,
    } }, .test_fixture);
    const disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .discriminant(local),
        .next = switch_stmt,
    } }, .test_fixture);
    const assign_one = try store.addCFStmt(.{ .assign_tag = .{
        .target = local,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = disc_read,
    } }, .test_fixture);
    const body = try store.addCFStmt(.{ .assign_tag = .{
        .target = local,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = assign_one,
    } }, .test_fixture);
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(3),
        .identity = LIR.ProcIdentity.forTest(5),
        .args = LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = f.inner_layout,
    }, .none);
    try f.result.root_procs.append(testing.allocator, proc);

    try run(&f.result);

    const rewritten = store.getCFStmtImmediate(switch_stmt).switch_stmt;
    const branches = store.getCFSwitchBranches(rewritten.branches);
    try testing.expectEqual(@as(usize, 1), branches.len);
    try testing.expectEqual(@as(u64, 0), GuardedList.at(branches, 0).value);
}

test "tag reachability keeps unrelated live discriminant reads" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const source = try f.local(f.outer_layout);
    const other = try f.local(f.outer_layout);
    const disc = try f.local(.u16);
    const other_disc = try f.local(.u16);

    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = disc } }, .test_fixture);
    const bad = try store.addCFStmt(.{ .runtime_error = {} }, .test_fixture);
    const other_switch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = other_disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 1, .body = ret_stmt }}),
        .default_branch = bad,
    } }, .test_fixture);
    const disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .discriminant(source),
        .next = other_switch,
    } }, .test_fixture);
    const other_disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = other_disc,
        .op = .discriminant(other),
        .next = disc_read,
    } }, .test_fixture);
    const assign_other = try store.addCFStmt(.{ .assign_tag = .{
        .target = other,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = other_disc_read,
    } }, .test_fixture);
    const body = try store.addCFStmt(.{ .assign_tag = .{
        .target = source,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = assign_other,
    } }, .test_fixture);
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(6),
        .identity = LIR.ProcIdentity.forTest(4),
        .args = LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = .u16,
    }, .none);
    try f.result.root_procs.append(testing.allocator, proc);

    try run(&f.result);

    const other_disc_stmt = store.getCFStmtImmediate(other_disc_read).assign_ref;
    try testing.expectEqual(disc_read, other_disc_stmt.next);

    const disc_stmt = store.getCFStmtImmediate(disc_read).assign_ref;
    try testing.expectEqual(ret_stmt, disc_stmt.next);
}

test "tag reachability retains branches for arg-derived tags" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const arg = try f.local(f.outer_layout);
    const disc = try f.local(.u16);

    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = arg } }, .test_fixture);
    const branch_zero = try store.addCFStmt(.{ .runtime_error = {} }, .test_fixture);
    const branch_one = try store.addCFStmt(.{ .runtime_error = {} }, .test_fixture);
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 0, .body = branch_zero },
            .{ .value = 1, .body = branch_one },
        }),
        .default_branch = ret_stmt,
    } }, .test_fixture);
    const body = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .discriminant(arg),
        .next = switch_stmt,
    } }, .test_fixture);
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(7),
        .identity = LIR.ProcIdentity.forTest(3),
        .args = try store.addLocalSpan(&[_]LIR.LocalId{arg}),
        .body = body,
        .ret_layout = f.outer_layout,
    }, .none);
    try f.result.root_procs.append(testing.allocator, proc);

    try run(&f.result);

    const read_stmt = store.getCFStmtImmediate(body).assign_ref;
    try testing.expectEqual(switch_stmt, read_stmt.next);

    const rewritten = store.getCFStmtImmediate(switch_stmt).switch_stmt;
    const branches = store.getCFSwitchBranches(rewritten.branches);
    try testing.expectEqual(@as(usize, 2), branches.len);
}

test "tag reachability retains branches for hosted call results" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const hosted = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(8),
        .identity = LIR.ProcIdentity.forTest(2),
        .args = LIR.LocalSpan.empty(),
        .body = null,
        .ret_layout = f.outer_layout,
        .hosted = .{
            .symbol = try store.insertString("roc_test_hosted"),
            .dispatch_index = 0,
        },
    }, .none);

    const result = try f.local(f.outer_layout);
    const disc = try f.local(.u16);

    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = result } }, .test_fixture);
    const bad = try store.addCFStmt(.{ .runtime_error = {} }, .test_fixture);
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 0, .body = bad }}),
        .default_branch = ret_stmt,
    } }, .test_fixture);
    const disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .discriminant(result),
        .next = switch_stmt,
    } }, .test_fixture);
    const body = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = hosted,
        .args = LIR.LocalSpan.empty(),
        .next = disc_read,
    } }, .test_fixture);
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(9),
        .identity = LIR.ProcIdentity.forTest(1),
        .args = LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = f.outer_layout,
    }, .none);
    try f.result.root_procs.append(testing.allocator, proc);

    try run(&f.result);

    const call_stmt = store.getCFStmtImmediate(body).assign_call;
    try testing.expectEqual(disc_read, call_stmt.next);

    const read_stmt = store.getCFStmtImmediate(disc_read).assign_ref;
    try testing.expectEqual(switch_stmt, read_stmt.next);
}

test "tag reachability retains branches for erased call results" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const closure = try f.local(f.outer_layout);
    const result = try f.local(f.outer_layout);
    const disc = try f.local(.u16);
    const arg_plan = try store.internErasedCallArgsPlan(&f.result.layouts, &.{});

    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = result } }, .test_fixture);
    const bad = try store.addCFStmt(.{ .runtime_error = {} }, .test_fixture);
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 0, .body = bad }}),
        .default_branch = ret_stmt,
    } }, .test_fixture);
    const disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .discriminant(result),
        .next = switch_stmt,
    } }, .test_fixture);
    const body = try store.addCFStmt(.{ .assign_call_erased = .{
        .target = result,
        .data = try store.addStmtRow(LIR.ErasedCallData, .{
            .closure = closure,
            .args = LIR.LocalSpan.empty(),
            .arg_plan = arg_plan,
        }),
        .next = disc_read,
    } }, .test_fixture);
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(10),
        .identity = LIR.ProcIdentity.forTest(1),
        .args = try store.addLocalSpan(&[_]LIR.LocalId{closure}),
        .body = body,
        .ret_layout = f.outer_layout,
    }, .none);
    try f.result.root_procs.append(testing.allocator, proc);

    try run(&f.result);

    const call_stmt = store.getCFStmtImmediate(body).assign_call_erased;
    try testing.expectEqual(disc_read, call_stmt.next);

    const read_stmt = store.getCFStmtImmediate(disc_read).assign_ref;
    try testing.expectEqual(switch_stmt, read_stmt.next);
}
