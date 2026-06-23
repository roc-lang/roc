//! Compacts LIR proc specs to the set reachable from explicit LIR roots.
//!
//! This runs after structural LIR rewrites and before ARC insertion so ARC,
//! codegen, LirImage, and interpreters all see one dense proc-id space that
//! contains only procs demanded by roots and explicit callable data.

const std = @import("std");
const base = @import("base");
const core = @import("lir_core");

const Allocator = std.mem.Allocator;
const LIR = core.LIR;
const LirProgram = core.Program;
const LirStore = core.LirStore;

/// Remove proc specs unreachable from `root_procs` and explicit constant
/// callable plans, then rewrite every retained proc reference to the compact
/// id space.
pub fn run(result: *LirProgram.Result) Allocator.Error!void {
    var pass = try Pass.init(result);
    defer pass.deinit();
    try pass.run();
}

const Pass = struct {
    result: *LirProgram.Result,
    store: *LirStore,
    allocator: Allocator,
    reachable: []bool,
    old_to_new: []?LIR.LirProcSpecId,
    reachable_stmts: []bool,
    old_stmt_to_new: []?LIR.CFStmtId,
    visited_stmts: []bool,
    visited_plans: []bool,
    proc_queue: std.ArrayList(LIR.LirProcSpecId),
    stmt_stack: std.ArrayList(LIR.CFStmtId),

    fn init(result: *LirProgram.Result) Allocator.Error!Pass {
        const allocator = result.store.allocator;
        const proc_count = result.store.proc_specs.items.len;
        const stmt_count = result.store.cf_stmts.items.len;
        const plan_count = result.const_plans.items.len;

        const reachable = try allocator.alloc(bool, proc_count);
        errdefer allocator.free(reachable);
        @memset(reachable, false);

        const old_to_new = try allocator.alloc(?LIR.LirProcSpecId, proc_count);
        errdefer allocator.free(old_to_new);
        @memset(old_to_new, null);

        const visited_stmts = try allocator.alloc(bool, stmt_count);
        errdefer allocator.free(visited_stmts);
        @memset(visited_stmts, false);

        const reachable_stmts = try allocator.alloc(bool, stmt_count);
        errdefer allocator.free(reachable_stmts);
        @memset(reachable_stmts, false);

        const old_stmt_to_new = try allocator.alloc(?LIR.CFStmtId, stmt_count);
        errdefer allocator.free(old_stmt_to_new);
        @memset(old_stmt_to_new, null);

        const visited_plans = try allocator.alloc(bool, plan_count);
        errdefer allocator.free(visited_plans);
        @memset(visited_plans, false);

        return .{
            .result = result,
            .store = &result.store,
            .allocator = allocator,
            .reachable = reachable,
            .old_to_new = old_to_new,
            .reachable_stmts = reachable_stmts,
            .old_stmt_to_new = old_stmt_to_new,
            .visited_stmts = visited_stmts,
            .visited_plans = visited_plans,
            .proc_queue = .empty,
            .stmt_stack = .empty,
        };
    }

    fn deinit(self: *Pass) void {
        self.stmt_stack.deinit(self.allocator);
        self.proc_queue.deinit(self.allocator);
        self.allocator.free(self.visited_plans);
        self.allocator.free(self.visited_stmts);
        self.allocator.free(self.old_stmt_to_new);
        self.allocator.free(self.reachable_stmts);
        self.allocator.free(self.old_to_new);
        self.allocator.free(self.reachable);
    }

    fn run(self: *Pass) Allocator.Error!void {
        for (self.result.root_procs.items) |proc| {
            try self.markProc(proc);
        }
        for (self.result.const_roots.items) |root| {
            try self.markProc(root.proc);
            try self.markConstPlan(root.plan);
        }
        for (self.result.requested_layouts.items) |request| {
            try self.markConstPlan(request.plan);
        }

        try self.drainProcQueue();
        self.assignCompactProcIds();
        self.assignCompactStmtIds();
        try self.remapReachableProcBodies();
        self.remapReachableProcStmtRefs();
        self.remapRootProcs();
        self.remapConstRoots();
        try self.remapErasedFns();
        self.remapComptimeSites();
        self.remapProcDebugNames();
        self.compactProcSpecs();
        self.compactCFStmts();
        self.verifyReachableProcRefs();
    }

    fn markProc(self: *Pass, proc: LIR.LirProcSpecId) Allocator.Error!void {
        const index = @intFromEnum(proc);
        if (index >= self.reachable.len) reachableProcInvariant("proc reference exceeds proc_specs len");
        if (self.reachable[index]) return;
        self.reachable[index] = true;
        try self.proc_queue.append(self.allocator, proc);
    }

    fn drainProcQueue(self: *Pass) Allocator.Error!void {
        while (self.proc_queue.pop()) |proc| {
            try self.markProcBodyEdges(proc);
        }
    }

    fn markProcBodyEdges(self: *Pass, proc_id: LIR.LirProcSpecId) Allocator.Error!void {
        const proc = self.store.getProcSpec(proc_id);
        if (proc.body) |body| try self.stmt_stack.append(self.allocator, body);
        for (self.store.getJoinPointSpan(proc.join_points)) |join_point| {
            try self.stmt_stack.append(self.allocator, join_point.body);
        }

        while (self.stmt_stack.pop()) |stmt_id| {
            const stmt_index = @intFromEnum(stmt_id);
            if (stmt_index >= self.visited_stmts.len) reachableProcInvariant("stmt reference exceeds cf_stmts len");
            if (self.visited_stmts[stmt_index]) continue;
            self.visited_stmts[stmt_index] = true;
            self.reachable_stmts[stmt_index] = true;

            const stmt = self.store.getCFStmt(stmt_id);
            switch (stmt) {
                .init_uninitialized => |s| try self.pushStmt(s.next),
                .assign_ref => |s| try self.pushStmt(s.next),
                .assign_literal => |s| {
                    if (s.value == .proc_ref) try self.markProc(s.value.proc_ref);
                    try self.pushStmt(s.next);
                },
                .assign_call => |s| {
                    try self.markProc(s.proc);
                    try self.pushStmt(s.next);
                },
                .assign_call_erased => |s| try self.pushStmt(s.next),
                .assign_packed_erased_fn => |s| {
                    try self.markProc(s.proc);
                    try self.pushStmt(s.next);
                },
                .assign_low_level => |s| try self.pushStmt(s.next),
                .assign_list => |s| try self.pushStmt(s.next),
                .assign_struct => |s| try self.pushStmt(s.next),
                .assign_tag => |s| try self.pushStmt(s.next),
                .set_local => |s| try self.pushStmt(s.next),
                .debug => |s| try self.pushStmt(s.next),
                .expect => |s| try self.pushStmt(s.next),
                .comptime_branch_taken => |s| try self.pushStmt(s.next),
                .incref => |s| try self.pushStmt(s.next),
                .decref => |s| try self.pushStmt(s.next),
                .decref_if_initialized => |s| try self.pushStmt(s.next),
                .free => |s| try self.pushStmt(s.next),
                .switch_stmt => |s| {
                    if (s.continuation) |continuation| try self.pushStmt(continuation);
                    try self.pushStmt(s.default_branch);
                    for (self.store.getCFSwitchBranches(s.branches)) |branch| {
                        try self.pushStmt(branch.body);
                    }
                },
                .switch_initialized_payload => |s| {
                    try self.pushStmt(s.initialized_branch);
                    try self.pushStmt(s.uninitialized_branch);
                },
                .str_match => |s| {
                    try self.pushStmt(s.on_match);
                    try self.pushStmt(s.on_miss);
                },
                .str_match_set => |s| {
                    for (self.store.getStrMatchArms(s.arms)) |arm| {
                        try self.pushStmt(arm.on_match);
                    }
                    try self.pushStmt(s.on_miss);
                },
                .join => |s| {
                    try self.pushStmt(s.body);
                    try self.pushStmt(s.remainder);
                },
                .ret,
                .jump,
                .crash,
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .loop_continue,
                .loop_break,
                => {},
            }
        }
    }

    fn pushStmt(self: *Pass, stmt: LIR.CFStmtId) Allocator.Error!void {
        try self.stmt_stack.append(self.allocator, stmt);
    }

    fn markConstPlan(self: *Pass, plan_id: LirProgram.ConstPlanId) Allocator.Error!void {
        const index = @intFromEnum(plan_id);
        if (index >= self.result.const_plans.items.len) reachableProcInvariant("const plan reference exceeds const_plans len");
        if (self.visited_plans[index]) return;
        self.visited_plans[index] = true;

        switch (self.result.const_plans.items[index]) {
            .pending,
            .zst,
            .scalar,
            .str,
            => {},
            .list => |elem| try self.markConstPlan(elem),
            .box => |boxed| try self.markConstPlan(boxed),
            .tuple => |items| for (items) |item| try self.markConstPlan(item),
            .record => |fields| for (fields) |field| try self.markConstPlan(field),
            .tag_union => |variants| {
                for (variants) |variant| {
                    for (variant.payloads) |payload| try self.markConstPlan(payload);
                }
            },
            .named => |named| try self.markConstPlan(named.backing),
            .fn_value => |set_id| try self.markFnSet(set_id),
            .erased_fn => |set_id| try self.markErasedFns(set_id),
        }
    }

    fn markFnSet(self: *Pass, set_id: LirProgram.FnSetId) Allocator.Error!void {
        const set = self.result.fn_sets.items[@intFromEnum(set_id)];
        for (set.variants) |variant| {
            for (variant.captures) |capture| try self.markConstPlan(capture.plan);
        }
    }

    fn markErasedFns(self: *Pass, set_id: LirProgram.ErasedFnsId) Allocator.Error!void {
        const set = self.result.erased_fns.items[@intFromEnum(set_id)];
        for (set.entries) |entry| {
            try self.markProc(entry.entry);
            for (entry.captures) |capture| try self.markConstPlan(capture.plan);
        }
    }

    fn assignCompactProcIds(self: *Pass) void {
        var next: u32 = 0;
        for (self.reachable, 0..) |is_reachable, old_index| {
            if (!is_reachable) continue;
            self.old_to_new[old_index] = @enumFromInt(next);
            next += 1;
        }
    }

    fn assignCompactStmtIds(self: *Pass) void {
        var next: u32 = 0;
        for (self.reachable_stmts, 0..) |is_reachable, old_index| {
            if (!is_reachable) continue;
            self.old_stmt_to_new[old_index] = @enumFromInt(next);
            next += 1;
        }
    }

    fn remapReachableProcBodies(self: *Pass) Allocator.Error!void {
        @memset(self.visited_stmts, false);
        for (self.store.proc_specs.items, 0..) |proc, index| {
            if (!self.reachable[index]) continue;
            const body = proc.body orelse continue;
            try self.remapStmtProcRefs(body);
        }
    }

    fn remapStmtProcRefs(self: *Pass, body: LIR.CFStmtId) Allocator.Error!void {
        try self.stmt_stack.append(self.allocator, body);
        while (self.stmt_stack.pop()) |stmt_id| {
            const stmt_index = @intFromEnum(stmt_id);
            if (self.visited_stmts[stmt_index]) continue;
            self.visited_stmts[stmt_index] = true;

            const stmt = self.store.getCFStmtPtr(stmt_id);
            switch (stmt.*) {
                .assign_literal => |*s| {
                    if (s.value == .proc_ref) s.value.proc_ref = self.remapProc(s.value.proc_ref);
                    const next = s.next;
                    s.next = self.remapStmt(next);
                    try self.pushStmt(next);
                },
                .assign_call => |*s| {
                    s.proc = self.remapProc(s.proc);
                    const next = s.next;
                    s.next = self.remapStmt(next);
                    try self.pushStmt(next);
                },
                .assign_packed_erased_fn => |*s| {
                    s.proc = self.remapProc(s.proc);
                    const next = s.next;
                    s.next = self.remapStmt(next);
                    try self.pushStmt(next);
                },
                .switch_stmt => |*s| {
                    if (s.continuation) |continuation| {
                        s.continuation = self.remapStmt(continuation);
                        try self.pushStmt(continuation);
                    }
                    const default_branch = s.default_branch;
                    s.default_branch = self.remapStmt(default_branch);
                    try self.pushStmt(default_branch);
                    for (self.store.getCFSwitchBranchesMut(s.branches)) |*branch| {
                        const branch_body = branch.body;
                        branch.body = self.remapStmt(branch_body);
                        try self.pushStmt(branch_body);
                    }
                },
                .switch_initialized_payload => |*s| {
                    const initialized = s.initialized_branch;
                    const uninitialized = s.uninitialized_branch;
                    s.initialized_branch = self.remapStmt(initialized);
                    s.uninitialized_branch = self.remapStmt(uninitialized);
                    try self.pushStmt(initialized);
                    try self.pushStmt(uninitialized);
                },
                .str_match => |*s| {
                    const on_match = s.on_match;
                    const on_miss = s.on_miss;
                    s.on_match = self.remapStmt(on_match);
                    s.on_miss = self.remapStmt(on_miss);
                    try self.pushStmt(on_match);
                    try self.pushStmt(on_miss);
                },
                .str_match_set => |*s| {
                    for (self.store.getStrMatchArmsMut(s.arms)) |*arm| {
                        const on_match = arm.on_match;
                        arm.on_match = self.remapStmt(on_match);
                        try self.pushStmt(on_match);
                    }
                    const on_miss = s.on_miss;
                    s.on_miss = self.remapStmt(on_miss);
                    try self.pushStmt(on_miss);
                },
                .join => |*s| {
                    const body_stmt = s.body;
                    const remainder = s.remainder;
                    s.body = self.remapStmt(body_stmt);
                    s.remainder = self.remapStmt(remainder);
                    try self.pushStmt(body_stmt);
                    try self.pushStmt(remainder);
                },
                inline .init_uninitialized,
                .assign_ref,
                .assign_call_erased,
                .assign_low_level,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .set_local,
                .debug,
                .expect,
                .comptime_branch_taken,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                => |*s| {
                    const next = s.next;
                    s.next = self.remapStmt(next);
                    try self.pushStmt(next);
                },
                .ret,
                .jump,
                .crash,
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .loop_continue,
                .loop_break,
                => {},
            }
        }
    }

    fn remapReachableProcStmtRefs(self: *Pass) void {
        for (self.store.proc_specs.items, 0..) |*proc, index| {
            if (!self.reachable[index]) continue;
            if (proc.body) |body| proc.body = self.remapStmt(body);
            for (self.store.getJoinPointSpanMut(proc.join_points)) |*join_point| {
                join_point.body = self.remapStmt(join_point.body);
            }
        }
    }

    fn remapRootProcs(self: *Pass) void {
        for (self.result.root_procs.items) |*proc| {
            proc.* = self.remapProc(proc.*);
        }
    }

    fn remapConstRoots(self: *Pass) void {
        for (self.result.const_roots.items) |*root| {
            root.proc = self.remapProc(root.proc);
        }
    }

    fn remapErasedFns(self: *Pass) Allocator.Error!void {
        for (self.result.erased_fns.items) |*set| {
            var kept_count: usize = 0;
            for (set.entries) |entry| {
                if (self.maybeRemapProc(entry.entry) != null) kept_count += 1;
            }

            const old_entries = set.entries;
            if (kept_count == 0) {
                for (old_entries) |entry| {
                    if (entry.captures.len > 0) self.allocator.free(entry.captures);
                }
                if (old_entries.len > 0) self.allocator.free(old_entries);
                set.entries = &.{};
                continue;
            }

            const new_entries = try self.allocator.alloc(LirProgram.ErasedFn, kept_count);
            var write: usize = 0;
            for (old_entries) |entry| {
                if (self.maybeRemapProc(entry.entry)) |new_proc| {
                    new_entries[write] = entry;
                    new_entries[write].entry = new_proc;
                    write += 1;
                } else if (entry.captures.len > 0) {
                    self.allocator.free(entry.captures);
                }
            }
            if (old_entries.len > 0) self.allocator.free(old_entries);
            set.entries = new_entries;
        }
    }

    fn remapComptimeSites(self: *Pass) void {
        for (self.result.comptime_sites.items) |*site| {
            if (self.maybeRemapProc(site.proc)) |proc| {
                site.proc = proc;
            }
        }
    }

    fn remapProcDebugNames(self: *Pass) void {
        var write: usize = 0;
        for (self.store.proc_debug_names.items) |entry| {
            const old_proc: LIR.LirProcSpecId = @enumFromInt(entry.proc);
            const new_proc = self.maybeRemapProc(old_proc) orelse continue;
            self.store.proc_debug_names.items[write] = .{
                .proc = @intFromEnum(new_proc),
                .string = entry.string,
            };
            write += 1;
        }
        self.store.proc_debug_names.shrinkRetainingCapacity(write);
    }

    fn compactProcSpecs(self: *Pass) void {
        var write: usize = 0;
        for (self.store.proc_specs.items, self.store.proc_locs.items, 0..) |proc, loc, index| {
            if (!self.reachable[index]) continue;
            self.store.proc_specs.items[write] = proc;
            self.store.proc_locs.items[write] = loc;
            write += 1;
        }
        self.store.proc_specs.shrinkRetainingCapacity(write);
        self.store.proc_locs.shrinkRetainingCapacity(write);
    }

    fn compactCFStmts(self: *Pass) void {
        var write: usize = 0;
        for (self.store.cf_stmts.items, 0..) |stmt, index| {
            if (!self.reachable_stmts[index]) continue;
            self.store.cf_stmts.items[write] = stmt;
            write += 1;
        }
        self.store.cf_stmts.shrinkRetainingCapacity(write);
    }

    fn verifyReachableProcRefs(self: *Pass) void {
        const proc_count = self.store.proc_specs.items.len;
        const stmt_count = self.store.cf_stmts.items.len;
        for (self.result.root_procs.items) |proc| {
            if (@intFromEnum(proc) >= proc_count) reachableProcInvariant("root proc exceeds compact proc_specs len");
        }
        for (self.result.const_roots.items) |root| {
            if (@intFromEnum(root.proc) >= proc_count) reachableProcInvariant("const root proc exceeds compact proc_specs len");
        }
        for (self.store.proc_debug_names.items) |entry| {
            if (entry.proc >= proc_count) reachableProcInvariant("proc debug name exceeds compact proc_specs len");
        }
        for (self.store.proc_specs.items) |proc| {
            if (proc.body) |body| self.verifyStmtRef(body, stmt_count);
            for (self.store.getJoinPointSpan(proc.join_points)) |join_point| {
                self.verifyStmtRef(join_point.body, stmt_count);
            }
        }
        for (self.store.cf_stmts.items) |stmt| {
            self.verifyStmtRefs(stmt, proc_count, stmt_count);
        }
    }

    fn remapProc(self: *Pass, old: LIR.LirProcSpecId) LIR.LirProcSpecId {
        return self.maybeRemapProc(old) orelse reachableProcInvariant("reachable proc edge pointed at pruned proc");
    }

    fn maybeRemapProc(self: *Pass, old: LIR.LirProcSpecId) ?LIR.LirProcSpecId {
        const index = @intFromEnum(old);
        if (index >= self.old_to_new.len) reachableProcInvariant("proc reference exceeds old proc_specs len");
        return self.old_to_new[index];
    }

    fn remapStmt(self: *Pass, old: LIR.CFStmtId) LIR.CFStmtId {
        const index = @intFromEnum(old);
        if (index >= self.old_stmt_to_new.len) reachableProcInvariant("stmt reference exceeds old cf_stmts len");
        return self.old_stmt_to_new[index] orelse reachableProcInvariant("reachable stmt edge pointed at pruned stmt");
    }

    fn verifyProcRef(_: *Pass, proc: LIR.LirProcSpecId, proc_count: usize) void {
        if (@intFromEnum(proc) >= proc_count) reachableProcInvariant("stmt proc reference exceeds compact proc_specs len");
    }

    fn verifyStmtRef(_: *Pass, stmt: LIR.CFStmtId, stmt_count: usize) void {
        if (@intFromEnum(stmt) >= stmt_count) reachableProcInvariant("stmt edge exceeds compact cf_stmts len");
    }

    fn verifyStmtRefs(self: *Pass, stmt: LIR.CFStmt, proc_count: usize, stmt_count: usize) void {
        switch (stmt) {
            .assign_literal => |s| {
                if (s.value == .proc_ref) self.verifyProcRef(s.value.proc_ref, proc_count);
                self.verifyStmtRef(s.next, stmt_count);
            },
            .assign_call => |s| {
                self.verifyProcRef(s.proc, proc_count);
                self.verifyStmtRef(s.next, stmt_count);
            },
            .assign_packed_erased_fn => |s| {
                self.verifyProcRef(s.proc, proc_count);
                self.verifyStmtRef(s.next, stmt_count);
            },
            inline .init_uninitialized,
            .assign_ref,
            .assign_call_erased,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .set_local,
            .debug,
            .expect,
            .comptime_branch_taken,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            => |s| self.verifyStmtRef(s.next, stmt_count),
            .switch_stmt => |s| {
                if (s.continuation) |continuation| self.verifyStmtRef(continuation, stmt_count);
                self.verifyStmtRef(s.default_branch, stmt_count);
                for (self.store.getCFSwitchBranches(s.branches)) |branch| {
                    self.verifyStmtRef(branch.body, stmt_count);
                }
            },
            .switch_initialized_payload => |s| {
                self.verifyStmtRef(s.initialized_branch, stmt_count);
                self.verifyStmtRef(s.uninitialized_branch, stmt_count);
            },
            .str_match => |s| {
                self.verifyStmtRef(s.on_match, stmt_count);
                self.verifyStmtRef(s.on_miss, stmt_count);
            },
            .str_match_set => |s| {
                for (self.store.getStrMatchArms(s.arms)) |arm| {
                    self.verifyStmtRef(arm.on_match, stmt_count);
                }
                self.verifyStmtRef(s.on_miss, stmt_count);
            },
            .join => |s| {
                self.verifyStmtRef(s.body, stmt_count);
                self.verifyStmtRef(s.remainder, stmt_count);
            },
            .ret,
            .jump,
            .crash,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .loop_continue,
            .loop_break,
            => {},
        }
    }
};

fn reachableProcInvariant(msg: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("reachable procs invariant violated: {s}", .{msg});
    }
    unreachable;
}

test "reachable proc pass compacts proc specs and remaps root ids" {
    var result = try LirProgram.Result.init(std.testing.allocator, base.target.TargetUsize.native);
    defer result.deinit();

    const value = try result.store.addLocal(.{ .layout_idx = .zst });
    const live_body = try result.store.addCFStmt(.{ .ret = .{ .value = value } });
    const live_proc = try result.store.addProcSpec(.{
        .name = result.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = live_body,
        .ret_layout = .zst,
    });

    const dead_callee_body = try result.store.addCFStmt(.runtime_error);
    const dead_callee = try result.store.addProcSpec(.{
        .name = result.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = dead_callee_body,
        .ret_layout = .zst,
    });
    const dead_caller_ret = try result.store.addCFStmt(.{ .ret = .{ .value = value } });
    const dead_caller_body = try result.store.addCFStmt(.{ .assign_call = .{
        .target = value,
        .proc = dead_callee,
        .args = LIR.LocalSpan.empty(),
        .next = dead_caller_ret,
    } });
    _ = try result.store.addProcSpec(.{
        .name = result.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = dead_caller_body,
        .ret_layout = .zst,
    });

    const root_ret = try result.store.addCFStmt(.{ .ret = .{ .value = value } });
    const root_body = try result.store.addCFStmt(.{ .assign_call = .{
        .target = value,
        .proc = live_proc,
        .args = LIR.LocalSpan.empty(),
        .next = root_ret,
    } });
    const root_proc = try result.store.addProcSpec(.{
        .name = result.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = root_body,
        .ret_layout = .zst,
    });
    try result.root_procs.append(std.testing.allocator, root_proc);

    try run(&result);

    try std.testing.expectEqual(@as(usize, 2), result.store.proc_specs.items.len);
    try std.testing.expectEqual(@as(usize, 3), result.store.cf_stmts.items.len);
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(result.root_procs.items[0]));
    const compact_root = result.store.getProcSpec(result.root_procs.items[0]);
    const call = result.store.getCFStmt(compact_root.body.?).assign_call;
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(call.proc));
}
