//! Regression test for issue #10218.

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

fn procReachesListSet(store: *const lir.LirStore, start: lir.LIR.LirProcSpecId) std.mem.Allocator.Error!bool {
    const allocator = std.testing.allocator;
    const seen_procs = try allocator.alloc(bool, store.procSpecCount());
    defer allocator.free(seen_procs);
    @memset(seen_procs, false);
    const seen_stmts = try allocator.alloc(bool, store.cfStmtCount());
    defer allocator.free(seen_stmts);
    @memset(seen_stmts, false);

    var stack = std.ArrayList(ReachTask).empty;
    defer stack.deinit(allocator);
    try stack.append(allocator, .{ .proc = start });

    while (stack.pop()) |task| switch (task) {
        .proc => |proc_id| {
            const index = @intFromEnum(proc_id);
            if (seen_procs[index]) continue;
            seen_procs[index] = true;
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
                    if (assign.op == .list_set) return true;
                    try stack.append(allocator, .{ .stmt = assign.next });
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
                .join => |join_stmt| {
                    try stack.append(allocator, .{ .stmt = join_stmt.body });
                    try stack.append(allocator, .{ .stmt = join_stmt.remainder });
                },
                inline .assign_ref,
                .assign_literal,
                .init_uninitialized,
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
    return false;
}

fn spanContainsLocal(store: *const lir.LirStore, span: lir.LIR.LocalSpan, local: lir.LIR.LocalId) bool {
    const locals = store.getLocalSpan(span);
    for (0..GuardedList.borrowLen(locals)) |index| {
        if (GuardedList.at(locals, index) == local) return true;
    }
    return false;
}

fn retainReachesListSetCall(
    store: *const lir.LirStore,
    start: lir.LIR.CFStmtId,
    retained: lir.LIR.LocalId,
) std.mem.Allocator.Error!bool {
    var current = start;
    var remaining = store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        switch (store.getCFStmt(current)) {
            .assign_call => |assign| {
                if (spanContainsLocal(store, assign.args, retained) and
                    try procReachesListSet(store, assign.proc))
                {
                    return true;
                }
                if (assign.target == retained) return false;
                current = assign.next;
            },
            .incref => |rc| current = rc.next,
            .decref => |rc| {
                if (rc.value == retained) return false;
                current = rc.next;
            },
            .decref_if_initialized => |rc| {
                if (rc.value == retained) return false;
                current = rc.next;
            },
            .free => |rc| {
                if (rc.value == retained) return false;
                current = rc.next;
            },
            inline .assign_ref,
            .assign_literal,
            .init_uninitialized,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            => |assign| {
                if (assign.target == retained) return false;
                current = assign.next;
            },
            .set_local => |assign| {
                if (assign.target == retained) return false;
                current = assign.next;
            },
            inline .debug,
            .expect,
            .comptime_branch_taken,
            .store_struct,
            .store_tag,
            => |stmt| current = stmt.next,
            .ret,
            .jump,
            .crash,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .loop_continue,
            .loop_break,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .join,
            => return false,
        }
    }
    return false;
}

fn expectNoRetainBeforeListSet(
    store: *const lir.LirStore,
    _: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    var saw_list_set = false;
    var retains_before_list_set: usize = 0;
    for (0..store.cfStmtCount()) |stmt_index| {
        const stmt = store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
        switch (stmt) {
            .assign_low_level => |assign| {
                if (assign.op == .list_set) saw_list_set = true;
            },
            .incref => |retain| if (try retainReachesListSetCall(store, retain.next, retain.value)) {
                retains_before_list_set += 1;
            },
            else => {},
        }
    }
    try std.testing.expect(saw_list_set);
    try std.testing.expectEqual(@as(usize, 0), retains_before_list_set);
}

test "issue 10218: reading a loop-carried list preserves uniqueness for a later set" {
    // Repro for https://github.com/roc-lang/roc/issues/10218.
    try harness.expectLirInspection(
        \\loop : List(U64), U64, U64 -> U64
        \\loop = |table, i, acc|
        \\    if i == 0 {
        \\        acc
        \\    } else {
        \\        len = table.len()
        \\        updated = match table.set(0, i) { Ok(t) => t, Err(_) => [] }
        \\        loop(updated, i - 1, acc + len)
        \\    }
        \\
        \\main! = |args| {
        \\    echo!(loop(List.repeat(0, 8), 4 + args.len(), 0).to_str())
        \\    Ok({})
        \\}
    , expectNoRetainBeforeListSet);
}
