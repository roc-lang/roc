//! Exact literal backing demand of the declared procedure bodies.
//! The literal pool also owns compiler names and bodies discarded by later
//! passes. Pool membership alone never authorizes emitting readonly data.
const std = @import("std");
const base = @import("base");
const core = @import("lir_core");
const BodyClone = @import("body_clone.zig");
const GuardedList = @import("collections").GuardedList;
const DenseMap = @import("collections").DenseMap;
const LIR = core.LIR;
const Allocator = std.mem.Allocator;

/// Collect exact string backings referenced by the declared procedure bodies.
pub fn collect(allocator: Allocator, store: *const core.LirStore) Allocator.Error![]base.StringLiteral.Idx {
    var collector: Collector = .{ .allocator = allocator, .store = store, .visited = .init(allocator), .backing_set = .init(allocator) };
    defer collector.visited.deinit();
    defer collector.backing_set.deinit();
    defer collector.backings.deinit(allocator);
    defer collector.stack.deinit(allocator);
    defer collector.successors.deinit(allocator);
    for (store.getProcSpecs()) |proc| {
        if (proc.body) |body| try collector.pushStmt(body);
        const joins = store.getJoinPointSpan(proc.join_points);
        for (0..joins.len) |index| try collector.pushStmt(GuardedList.at(joins, index).body);
    }
    try collector.run();
    return collector.backings.toOwnedSlice(allocator);
}

const Collector = struct {
    allocator: Allocator,
    store: *const core.LirStore,
    visited: DenseMap(LIR.CFStmtId, void),
    backing_set: DenseMap(base.StringLiteral.Idx, void),
    backings: std.ArrayList(base.StringLiteral.Idx) = .empty,
    stack: std.ArrayList(LIR.CFStmtId) = .empty,
    successors: std.ArrayList(LIR.CFStmtId) = .empty,

    fn mark(self: *Collector, backing: base.StringLiteral.Idx) Allocator.Error!void {
        if (!(try self.backing_set.getOrPut(backing)).found_existing)
            try self.backings.append(self.allocator, backing);
    }
    fn markSteps(self: *Collector, span: LIR.StrMatchStepSpan) Allocator.Error!void {
        const steps = self.store.getStrMatchSteps(span);
        for (0..steps.len) |index| try self.mark(GuardedList.at(steps, index).delimiter.backing);
    }
    fn pushStmt(self: *Collector, stmt: LIR.CFStmtId) Allocator.Error!void {
        const entry = try self.visited.getOrPut(stmt);
        if (!entry.found_existing) try self.stack.append(self.allocator, stmt);
    }
    fn run(self: *Collector) Allocator.Error!void {
        while (self.stack.pop()) |stmt_id| {
            const stmt = self.store.getCFStmt(stmt_id);
            switch (stmt) {
                .assign_literal => |s| switch (s.value) {
                    .str_literal => |literal| try self.mark(literal.backing),
                    .bytes_literal => |literal| try self.mark(literal.bytes.backing),
                    .i64_literal, .i128_literal, .f64_literal, .f32_literal, .dec_literal, .boxy_dynamic_num_literal, .boxy_dynamic_frac_literal, .static_data, .proc_ref => {},
                },
                .str_match => |s| {
                    try self.mark(s.prefix.backing);
                    try self.markSteps(s.steps);
                },
                .str_match_set => |s| {
                    const arms = self.store.getStrMatchArms(s.arms);
                    for (0..arms.len) |index| {
                        const arm = GuardedList.at(arms, index);
                        try self.mark(arm.prefix.backing);
                        try self.markSteps(arm.steps);
                    }
                },
                .init_uninitialized,
                .assign_ref,
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
                .comptime_branch_taken,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                .switch_stmt,
                .switch_initialized_payload,
                .loop_continue,
                .loop_break,
                .join,
                .jump,
                .ret,
                .crash,
                => {},
            }
            // Successors join the stack in the shared edge order, each one
            // once, so the backing order a program yields is stable.
            self.successors.clearRetainingCapacity();
            try BodyClone.appendSuccessors(self.store, &self.successors, stmt_id, self.allocator);
            for (self.successors.items) |next| try self.pushStmt(next);
        }
    }
};
