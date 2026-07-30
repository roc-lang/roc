//! Regression coverage for platform-provided boxed model update wrappers.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");
const GuardedList = lir.LirStore.GuardedList;

const harness = @import("lower_to_lir_harness.zig");

test "platform boxed update wrapper prepares in-place update" {
    try harness.runAppPathLirInspection("test/int/app.roc", .{ .inline_mode = .wrappers }, expectBoxPrepareUpdate);
}

fn expectBoxPrepareUpdate(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    var prepare_update_count: usize = 0;

    for (0..store.getProcSpecs().len) |index| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const proc = store.getProcSpec(proc_id);
        const body = proc.body orelse continue;

        var work = std.ArrayList(lir.LIR.CFStmtId).empty;
        defer work.deinit(std.testing.allocator);
        var visited = std.AutoHashMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
        defer visited.deinit();

        try work.append(std.testing.allocator, body);
        while (work.pop()) |stmt_id| {
            const entry = try visited.getOrPut(stmt_id);
            if (entry.found_existing) continue;

            switch (store.getCFStmt(stmt_id)) {
                inline .assign_ref,
                .assign_literal,
                .init_uninitialized,
                .assign_call,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .set_local,
                .debug,
                .expect,
                .comptime_branch_taken,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                => |stmt| try work.append(std.testing.allocator, stmt.next),
                .assign_low_level => |stmt| {
                    if (stmt.op == .box_prepare_update) prepare_update_count += 1;
                    try work.append(std.testing.allocator, stmt.next);
                },
                .switch_stmt => |stmt| {
                    if (stmt.continuation) |continuation| try work.append(std.testing.allocator, continuation);
                    const cases = store.getCFSwitchBranches(stmt.branches);
                    for (0..cases.len) |case_index| {
                        try work.append(std.testing.allocator, GuardedList.at(cases, case_index).body);
                    }
                    try work.append(std.testing.allocator, stmt.default_branch);
                },
                .switch_initialized_payload => |stmt| {
                    try work.append(std.testing.allocator, stmt.initialized_branch);
                    try work.append(std.testing.allocator, stmt.uninitialized_branch);
                },
                .str_match => |stmt| {
                    try work.append(std.testing.allocator, stmt.on_match);
                    try work.append(std.testing.allocator, stmt.on_miss);
                },
                .str_match_set => |stmt| {
                    const arms = store.getStrMatchArms(stmt.arms);
                    for (0..arms.len) |arm_index| {
                        try work.append(std.testing.allocator, GuardedList.at(arms, arm_index).on_match);
                    }
                    try work.append(std.testing.allocator, stmt.on_miss);
                },
                .join => |stmt| {
                    try work.append(std.testing.allocator, stmt.body);
                    try work.append(std.testing.allocator, stmt.remainder);
                },
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .loop_continue,
                .loop_break,
                .jump,
                .ret,
                .crash,
                .expect_err,
                => {},
            }
        }
    }

    try std.testing.expectEqual(@as(usize, 1), prepare_update_count);
}
