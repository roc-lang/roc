//! Regression test for issue #10849.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const Counts = struct {
    list_set: usize = 0,
    list_replace: usize = 0,
    prepare_reuse: usize = 0,
    can_reuse: usize = 0,
    extract: usize = 0,
    write: usize = 0,
    list_retain: usize = 0,
};

fn findNamedProc(store: *const lir.LirStore, expected_name: []const u8) ?lir.LIR.LirProcSpecId {
    for (store.getProcSpecs(), 0..) |_, index| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const name = store.procDebugName(proc_id) orelse continue;
        if (std.mem.eql(u8, name, expected_name)) return proc_id;
    }
    return null;
}

fn countReachableOps(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    root: lir.LIR.LirProcSpecId,
) std.mem.Allocator.Error!Counts {
    const allocator = std.testing.allocator;
    const seen_procs = try allocator.alloc(bool, store.procSpecCount());
    defer allocator.free(seen_procs);
    @memset(seen_procs, false);
    const seen_stmts = try allocator.alloc(bool, store.cfStmtCount());
    defer allocator.free(seen_stmts);
    @memset(seen_stmts, false);

    var proc_work = std.ArrayList(lir.LIR.LirProcSpecId).empty;
    defer proc_work.deinit(allocator);
    var stmt_work = std.ArrayList(lir.LIR.CFStmtId).empty;
    defer stmt_work.deinit(allocator);
    try proc_work.append(allocator, root);

    var counts = Counts{};
    while (proc_work.pop()) |proc_id| {
        const proc_index = @intFromEnum(proc_id);
        if (seen_procs[proc_index]) continue;
        seen_procs[proc_index] = true;
        if (store.getProcSpec(proc_id).body) |body| try stmt_work.append(allocator, body);

        while (stmt_work.pop()) |stmt_id| {
            const stmt_index = @intFromEnum(stmt_id);
            if (seen_stmts[stmt_index]) continue;
            seen_stmts[stmt_index] = true;

            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .assign_call) {
                try proc_work.append(allocator, stmt.assign_call.proc);
            } else if (stmt == .assign_low_level) {
                const op = stmt.assign_low_level.op;
                if (op == .list_set) counts.list_set += 1;
                if (op == .list_replace_unsafe) counts.list_replace += 1;
                if (op == .list_map_prepare_reuse) counts.prepare_reuse += 1;
                if (op == .list_map_can_reuse) counts.can_reuse += 1;
                if (op == .list_map_extract_unsafe) counts.extract += 1;
                if (op == .list_map_write_unsafe) counts.write += 1;
            } else if (stmt == .incref) {
                const retained = store.getLocal(stmt.incref.value);
                if (layouts.getLayout(retained.layout_idx).tag == .list) counts.list_retain += 1;
            }
            try lir.BodyClone.appendSuccessors(@constCast(store), &stmt_work, stmt_id);
        }
    }
    return counts;
}

fn expectUniqueUpdateMovesInnerElement(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    const update = findNamedProc(store, "Builtin.List.update") orelse return error.TestUnexpectedResult;
    const counts = try countReachableOps(store, layouts, update);

    const moves_inner_element = counts.prepare_reuse >= 1 and
        counts.can_reuse >= 1 and
        counts.extract >= 1 and
        counts.write >= 1;
    if (!moves_inner_element) {
        std.debug.print("List.update has no ownership-moving path for its selected element; reachable ops: {any}\n", .{counts});
    }
    try std.testing.expect(moves_inner_element);
}

fn expectDisabledBoxyUpdateDropsReuseBranch(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    const update = findNamedProc(store, "Builtin.List.update") orelse return error.TestUnexpectedResult;
    const counts = try countReachableOps(store, layouts, update);

    try std.testing.expect(counts.prepare_reuse >= 1);
    try std.testing.expect(counts.list_replace >= 1);
    try std.testing.expectEqual(@as(usize, 0), counts.can_reuse);
    try std.testing.expectEqual(@as(usize, 0), counts.extract);
    try std.testing.expectEqual(@as(usize, 0), counts.write);
}

fn expectEnabledBoxyUpdateKeepsReuseBranch(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    const update = findNamedProc(store, "Builtin.List.update") orelse return error.TestUnexpectedResult;
    const counts = try countReachableOps(store, layouts, update);

    try std.testing.expect(counts.prepare_reuse >= 1);
    try std.testing.expect(counts.list_replace >= 1);
    try std.testing.expect(counts.extract >= 1);
    try std.testing.expect(counts.write >= 1);
}

const nested_list_update_app =
    \\main! = |args| {
    \\    inner = List.repeat(0.U64, 8)
    \\    outer = [inner]
    \\    updated = outer.update(0, |item| item.set(0, args.len()) ?? item)?
    \\    updated_inner = updated.get(0)?
    \\    echo!((updated_inner.get(0)?).to_str())
    \\    Ok({})
    \\}
;

test "issue 10849: List.update can move a unique inner list into its updater" {
    // Repro for https://github.com/roc-lang/roc/issues/10849.
    try harness.expectLirInspectionWithOptions(nested_list_update_app, .{
        .list_in_place_map = true,
        .proc_debug_names = true,
        .prove_ranges = true,
    }, expectUniqueUpdateMovesInnerElement);
}

test "issue 10849: disabled Boxy list transforms omit the unreachable reuse branch" {
    try harness.expectLirInspectionWithOptions(
        nested_list_update_app,
        .{
            .specialization_strategy = .boxy,
            .list_in_place_map = true,
            .proc_debug_names = true,
        },
        expectEnabledBoxyUpdateKeepsReuseBranch,
    );
    try harness.expectLirInspectionWithOptions(
        nested_list_update_app,
        .{ .specialization_strategy = .boxy, .proc_debug_names = true },
        expectDisabledBoxyUpdateDropsReuseBranch,
    );
}
