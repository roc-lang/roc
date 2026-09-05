//! Regression test for issue #10991.
//!
//! Dev builds run post-check inlining and SpecConstr with its value-aware
//! clones scoped to iterator fusion. That scope must still collapse a
//! `List.fold` into a direct index loop: the procedure that reads elements
//! with `list_get_unsafe` may not call any procedure, because a per-element
//! out-of-line call is exactly the overhead the collapse exists to remove.

const std = @import("std");
const collections = @import("collections");
const layout = @import("layout");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = collections.GuardedList;

/// Walk one procedure's own control-flow statements (never following calls
/// into other procedures) and report whether it reads list elements directly
/// and how many procedure calls it makes.
const ProcScan = struct {
    reads_list_elements: bool = false,
    call_count: usize = 0,

    fn scan(
        store: *const lir.LirStore,
        proc_id: lir.LIR.LirProcSpecId,
        seen_stmts: []bool,
    ) std.mem.Allocator.Error!ProcScan {
        const allocator = std.testing.allocator;
        var result = ProcScan{};
        var stack = std.ArrayList(lir.LIR.CFStmtId).empty;
        defer stack.deinit(allocator);
        const body = store.getProcSpec(proc_id).body orelse return result;
        try stack.append(allocator, body);

        while (stack.pop()) |stmt_id| {
            const index = @intFromEnum(stmt_id);
            if (seen_stmts[index]) continue;
            seen_stmts[index] = true;
            switch (store.getCFStmt(stmt_id)) {
                .assign_low_level => |assign| {
                    if (assign.op == .list_get_unsafe) result.reads_list_elements = true;
                    try stack.append(allocator, assign.next);
                },
                .assign_call => |assign| {
                    result.call_count += 1;
                    try stack.append(allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    result.call_count += 1;
                    try stack.append(allocator, assign.next);
                },
                .assign_call_dict => |assign| {
                    result.call_count += 1;
                    try stack.append(allocator, assign.next);
                },
                .switch_stmt => |switch_stmt| {
                    const branches = store.getCFSwitchBranches(switch_stmt.branches);
                    for (0..GuardedList.borrowLen(branches)) |branch_index| {
                        try stack.append(allocator, GuardedList.at(branches, branch_index).body);
                    }
                    try stack.append(allocator, switch_stmt.default_branch);
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(allocator, continuation);
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    try stack.append(allocator, switch_stmt.initialized_branch);
                    try stack.append(allocator, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    try stack.append(allocator, str_match.on_match);
                    try stack.append(allocator, str_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    const arms = store.getStrMatchArms(str_match_set.arms);
                    for (0..GuardedList.borrowLen(arms)) |arm_index| {
                        try stack.append(allocator, GuardedList.at(arms, arm_index).on_match);
                    }
                    try stack.append(allocator, str_match_set.on_miss);
                },
                .boxy_tag_match => |tag_match| {
                    try stack.append(allocator, tag_match.on_match);
                    try stack.append(allocator, tag_match.on_miss);
                },
                .join => |join_stmt| {
                    try stack.append(allocator, join_stmt.body);
                    try stack.append(allocator, join_stmt.remainder);
                },
                inline .assign_ref,
                .assign_literal,
                .init_uninitialized,
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
                => |stmt| try stack.append(allocator, stmt.next),
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
        return result;
    }
};

fn expectFoldCollapsedToCallFreeIndexLoop(
    store: *const lir.LirStore,
    _: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    const allocator = std.testing.allocator;
    const seen_stmts = try allocator.alloc(bool, store.cfStmtCount());
    defer allocator.free(seen_stmts);
    @memset(seen_stmts, false);

    // The collapsed loop's home depends on the clone scope: `.all_calls`
    // chases the known `List.fold` callee into `sum_list`, while
    // `.iterator_fusion` collapses `List.fold`'s own specialization in place
    // and leaves `sum_list` as a wrapper the inline plan then dissolves.
    var collapsed_fold_procs: usize = 0;
    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        const name = store.procDebugName(proc_id) orelse continue;
        if (!std.mem.eql(u8, name, "sum_list") and
            !std.mem.eql(u8, name, "Builtin.List.fold")) continue;
        const scan = try ProcScan.scan(store, proc_id, seen_stmts);
        // Without the collapse, folding never reads an element by index: it
        // pulls elements through the step-closure protocol instead.
        if (!scan.reads_list_elements) continue;
        collapsed_fold_procs += 1;
        // With the collapse, the folding procedure is a self-contained index
        // loop of low-level operations; a per-element out-of-line call is
        // exactly the overhead the collapse exists to remove.
        try std.testing.expectEqual(@as(usize, 0), scan.call_count);
    }
    try std.testing.expect(collapsed_fold_procs > 0);
}

const fold_app_body =
    \\sum_list = |list| list.fold(0.U64, |acc, d| acc + d)
    \\
    \\main! = |args| {
    \\    a = List.repeat(1.U64, args.len() + 3)
    \\    b = List.repeat(2.U64, args.len() + 5)
    \\    echo!((sum_list(a) + sum_list(b)).to_str())
    \\    Ok({})
    \\}
;

test "issue 10991: dev pipeline collapses List.fold to a call-free index loop" {
    try harness.expectLirInspectionWithOptions(
        fold_app_body,
        .{ .inline_mode = .wrappers, .spec_constr_clone_inlining = .iterator_fusion, .proc_debug_names = true },
        expectFoldCollapsedToCallFreeIndexLoop,
    );
}

test "issue 10991: optimized pipeline also collapses List.fold to a call-free index loop" {
    try harness.expectLirInspectionWithOptions(
        fold_app_body,
        .{ .inline_mode = .wrappers, .spec_constr_clone_inlining = .all_calls, .proc_debug_names = true },
        expectFoldCollapsedToCallFreeIndexLoop,
    );
}

// A fold whose accumulator survives an arm as a borrowed alias of the loop
// argument produces a loop-carried borrow whose anchor value is no longer
// named by any local at the back edge. The certifier must express that
// liveness on the crossing entry itself instead of referencing a local the
// join summary does not carry; lowering this cleanly (the harness panics on
// any certification failure) is the regression check.
test "issue 10991: loop-carried borrow of a claimed payload certifies under the dev pipeline" {
    try harness.expectLowersToLirWithOptions(
        \\Transform := [
        \\    Uppercase,
        \\    Wrap(Str, Str),
        \\    Prefix(Str),
        \\].{
        \\    apply : Transform, Str -> Str
        \\    apply = |transform, val|
        \\        match transform {
        \\            Uppercase => {
        \\                bytes = val.to_utf8().map(|b| if b >= 'a' and b <= 'z' b - 32 else b)
        \\                match Str.from_utf8(bytes) {
        \\                    Ok(upper) => upper
        \\                    Err(_) => val
        \\                }
        \\            }
        \\            Wrap(pre, suf) => "${pre}${val}${suf}"
        \\            Prefix(p) => "${p}${val}"
        \\        }
        \\
        \\    apply_all : List(Transform), Str -> Str
        \\    apply_all = |transforms, val|
        \\        transforms.fold(val, |acc, t| apply(t, acc))
        \\}
        \\
        \\main! = |args| {
        \\    transforms = [Prefix("pre "), Uppercase, Wrap("<", ">")]
        \\    out = Transform.apply_all(transforms, "value ${args.len().to_str()}")
        \\    echo!(out)
        \\    Ok({})
        \\}
    ,
        .{ .inline_mode = .wrappers, .spec_constr_clone_inlining = .iterator_fusion },
    );
}
