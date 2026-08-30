//! Regression test for issue #10848.

const std = @import("std");
const collections = @import("collections");
const layout = @import("layout");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = collections.GuardedList;

const ReachTask = union(enum) {
    proc: lir.LIR.LirProcSpecId,
    stmt: lir.LIR.CFStmtId,
};

fn expectCallBuiltRecordListSetHasNoRetain(
    store: *const lir.LirStore,
    _: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    const allocator = std.testing.allocator;
    const seen_procs = try allocator.alloc(bool, store.procSpecCount());
    defer allocator.free(seen_procs);
    @memset(seen_procs, false);
    const seen_stmts = try allocator.alloc(bool, store.cfStmtCount());
    defer allocator.free(seen_stmts);
    @memset(seen_stmts, false);
    var stack = std.ArrayList(ReachTask).empty;
    defer stack.deinit(allocator);
    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        if (store.getProcSpec(proc_id).body != null) try stack.append(allocator, .{ .proc = proc_id });
    }

    var saw_root = false;
    var saw_list_set = false;
    var retained_list_sets: usize = 0;
    while (stack.pop()) |task| switch (task) {
        .proc => |proc_id| {
            const index = @intFromEnum(proc_id);
            if (seen_procs[index]) continue;
            seen_procs[index] = true;
            saw_root = true;
            if (store.getProcSpec(proc_id).body) |body| {
                try stack.append(allocator, .{ .stmt = body });
            }
        },
        .stmt => |stmt_id| {
            const index = @intFromEnum(stmt_id);
            if (seen_stmts[index]) continue;
            seen_stmts[index] = true;
            switch (store.getCFStmt(stmt_id)) {
                .assign_low_level => |assign| {
                    if (assign.op == .list_set) {
                        saw_list_set = true;
                    }
                    try stack.append(allocator, .{ .stmt = assign.next });
                },
                .incref => |stmt| {
                    const next = store.getCFStmt(stmt.next);
                    if (next == .assign_low_level and next.assign_low_level.op == .list_set) {
                        const args = store.getLocalSpan(next.assign_low_level.args);
                        if (GuardedList.borrowLen(args) > 0 and
                            GuardedList.at(args, 0) == stmt.value)
                        {
                            retained_list_sets += 1;
                        }
                    }
                    try stack.append(allocator, .{ .stmt = stmt.next });
                },
                .assign_call => |assign| {
                    try stack.append(allocator, .{ .proc = assign.proc });
                    try stack.append(allocator, .{ .stmt = assign.next });
                },
                .switch_stmt => |switch_stmt| {
                    const branches = store.getCFSwitchBranches(switch_stmt.branches);
                    for (0..GuardedList.borrowLen(branches)) |branch_index| {
                        try stack.append(allocator, .{ .stmt = GuardedList.at(branches, branch_index).body });
                    }
                    try stack.append(allocator, .{ .stmt = switch_stmt.default_branch });
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(allocator, .{ .stmt = continuation });
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    try stack.append(allocator, .{ .stmt = switch_stmt.initialized_branch });
                    try stack.append(allocator, .{ .stmt = switch_stmt.uninitialized_branch });
                },
                .str_match => |str_match| {
                    try stack.append(allocator, .{ .stmt = str_match.on_match });
                    try stack.append(allocator, .{ .stmt = str_match.on_miss });
                },
                .str_match_set => |str_match_set| {
                    const arms = store.getStrMatchArms(str_match_set.arms);
                    for (0..GuardedList.borrowLen(arms)) |arm_index| {
                        try stack.append(allocator, .{ .stmt = GuardedList.at(arms, arm_index).on_match });
                    }
                    try stack.append(allocator, .{ .stmt = str_match_set.on_miss });
                },
                .boxy_tag_match => |tag_match| {
                    try stack.append(allocator, .{ .stmt = tag_match.on_match });
                    try stack.append(allocator, .{ .stmt = tag_match.on_miss });
                },
                .join => |join_stmt| {
                    try stack.append(allocator, .{ .stmt = join_stmt.body });
                    try stack.append(allocator, .{ .stmt = join_stmt.remainder });
                },
                inline .assign_ref,
                .assign_literal,
                .init_uninitialized,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_tag,
                .assign_boxy_tag_payload,
                .assign_call_dict,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .set_local,
                .debug,
                .expect,
                .comptime_branch_taken,
                .decref,
                .decref_if_initialized,
                .free,
                => |stmt| try stack.append(allocator, .{ .stmt = stmt.next }),
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
        },
    };

    try std.testing.expect(saw_root);
    try std.testing.expect(saw_list_set);
    try std.testing.expectEqual(@as(usize, 0), retained_list_sets);
}

test "issue 10848: a helper-built record does not retain List.set's list" {
    // Repro for https://github.com/roc-lang/roc/issues/10848.
    try harness.expectLirInspectionWithOptions(
        \\new = |n| { x: List.repeat(0.U64, n), y: 0 }
        \\
        \\read = |s| s.x.get(s.y) ?? 0
        \\
        \\main! = |args| {
        \\    len = U64.from_str(args.get(0)?)?
        \\    n = U64.from_str(args.get(1)?)?
        \\    var $s = new(len)
        \\    var $i = 0
        \\    while $i < n {
        \\        $s = { ..$s, x: $s.x.set(0, $i) ?? $s.x }
        \\        $i = $i + 1
        \\    }
        \\    echo!(read($s).to_str())
        \\    Ok({})
        \\}
    , .{ .inline_mode = .wrappers, .proc_debug_names = true }, expectCallBuiltRecordListSetHasNoRetain);
}
