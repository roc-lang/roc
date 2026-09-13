//! Exact literal backing demand of the declared procedure bodies.
//! The literal pool also owns compiler names and bodies discarded by later
//! passes. Pool membership alone never authorizes emitting readonly data.
const std = @import("std");
const base = @import("base");
const core = @import("lir_core");
const GuardedList = @import("collections").GuardedList;
const LIR = core.LIR;
const Allocator = std.mem.Allocator;

pub fn collect(allocator: Allocator, store: *const core.LirStore) Allocator.Error![]base.StringLiteral.Idx {
    var collector: Collector = .{ .allocator = allocator, .store = store };
    defer collector.visited.deinit(allocator);
    defer collector.backings.deinit(allocator);
    defer collector.stack.deinit(allocator);
    for (store.getProcSpecs()) |proc| {
        if (proc.body) |body| try collector.pushStmt(body);
        const joins = store.getJoinPointSpan(proc.join_points);
        for (0..joins.len) |index| try collector.pushStmt(GuardedList.at(joins, index).body);
    }
    try collector.run();
    return allocator.dupe(base.StringLiteral.Idx, collector.backings.keys());
}

const Collector = struct {
    allocator: Allocator,
    store: *const core.LirStore,
    visited: std.AutoHashMapUnmanaged(LIR.CFStmtId, void) = .empty,
    backings: std.AutoArrayHashMapUnmanaged(base.StringLiteral.Idx, void) = .empty,
    stack: std.ArrayList(LIR.CFStmtId) = .empty,

    fn mark(self: *Collector, backing: base.StringLiteral.Idx) Allocator.Error!void {
        try self.backings.put(self.allocator, backing, {});
    }
    fn markSteps(self: *Collector, span: LIR.StrMatchStepSpan) Allocator.Error!void {
        const steps = self.store.getStrMatchSteps(span);
        for (0..steps.len) |index| try self.mark(GuardedList.at(steps, index).delimiter.backing);
    }
    fn pushStmt(self: *Collector, stmt: LIR.CFStmtId) Allocator.Error!void {
        const entry = try self.visited.getOrPut(self.allocator, stmt);
        if (!entry.found_existing) try self.stack.append(self.allocator, stmt);
    }
    fn run(self: *Collector) Allocator.Error!void {
        while (self.stack.pop()) |stmt_id| {
            const stmt = self.store.getCFStmt(stmt_id);
            switch (stmt) {
                .init_uninitialized => |s| try self.pushStmt(s.next),
                .assign_ref => |s| try self.pushStmt(s.next),
                .assign_literal => |s| {
                    switch (s.value) {
                        .str_literal => |literal| try self.mark(literal.backing),
                        .bytes_literal => |literal| try self.mark(literal.bytes.backing),
                        else => {},
                    }
                    try self.pushStmt(s.next);
                },
                .assign_call => |s| {
                    try self.pushStmt(s.next);
                },
                .assign_call_erased => |s| try self.pushStmt(s.next),
                .assign_packed_erased_fn => |s| {
                    try self.pushStmt(s.next);
                },
                inline .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level => |s| try self.pushStmt(s.next),
                .boxy_tag_match => |s| {
                    try self.pushStmt(s.on_match);
                    try self.pushStmt(s.on_miss);
                },
                .assign_list => |s| try self.pushStmt(s.next),
                .assign_struct => |s| try self.pushStmt(s.next),
                .assign_tag => |s| try self.pushStmt(s.next),
                .store_struct => |s| try self.pushStmt(s.next),
                .store_tag => |s| try self.pushStmt(s.next),
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
                    const branches = self.store.getCFSwitchBranches(s.branches);
                    for (0..branches.len) |index| {
                        const branch = GuardedList.at(branches, index);
                        try self.pushStmt(branch.body);
                    }
                },
                .switch_initialized_payload => |s| {
                    try self.pushStmt(s.initialized_branch);
                    try self.pushStmt(s.uninitialized_branch);
                },
                .str_match => |s| {
                    try self.mark(s.prefix.backing);
                    try self.markSteps(s.steps);
                    try self.pushStmt(s.on_match);
                    try self.pushStmt(s.on_miss);
                },
                .str_match_set => |s| {
                    const arms = self.store.getStrMatchArms(s.arms);
                    for (0..arms.len) |index| {
                        const arm = GuardedList.at(arms, index);
                        try self.mark(arm.prefix.backing);
                        try self.markSteps(arm.steps);
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
};
