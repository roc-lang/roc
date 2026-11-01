//! Unary erased calls use independent argument storage directly, while
//! borrowed views and multiple arguments still need a complete packed block.
const std = @import("std");
const collections = @import("collections");
const lir = @import("lir");
const layout = @import("layout");
const harness = @import("lower_to_lir_harness.zig");

test "boxy unary argument blocks preserve views and omit independent packing" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\made = |f, x| f({ value: x })
            \\viewed = |f, x| f(x.value)
            \\pair = |f, x, y| f(x, y)
            \\main! = |args| {
            \\    echo!(Str.inspect(made(|x| x, args)))
            \\    echo!(Str.inspect(viewed(|x| x, { value: args })))
            \\    echo!(Str.inspect(pair(|x, y| { x, y }, args, args)))
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .proc_debug_names = true, .target_usize = width }, inspect);
    }
}

fn inspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    const names = [_][]const u8{ "made", "viewed", "pair" };
    var found: [names.len]bool = @splat(false);
    for (store.getProcSpecs(), 0..) |proc, index| {
        const name = store.procDebugName(@fromBackingInt(@intCast(index))) orelse continue;
        const matched = for (names, 0..) |expected, i| {
            if (std.mem.eql(u8, name, expected) or
                (std.mem.endsWith(u8, name, expected) and name[name.len - expected.len - 1] == '.')) break i;
        } else continue;
        found[matched] = true;
        var work: std.ArrayList(lir.LIR.CFStmtId) = .empty;
        defer work.deinit(std.testing.allocator);
        var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
        defer visited.deinit();
        try work.append(std.testing.allocator, proc.body.?);
        var records: usize = 0;
        var calls: usize = 0;
        var constructed: ?lir.LIR.LocalId = null;
        while (work.pop()) |id| {
            if ((try visited.getOrPut(id)).found_existing) continue;
            const stmt = store.getCFStmt(id);
            if (stmt == .dyn) switch (store.stmtRow(stmt.dyn.data)) {
                .record => |record| {
                    records += 1;
                    constructed = stmt.dyn.target;
                    const fields = store.getLocalSpan(record.fields);
                    try std.testing.expectEqual(@as(usize, if (matched == 2) 2 else 1), fields.len);
                    if (matched == 1) try std.testing.expectEqual(lir.LIR.DynStorage.view, (store.localDyn(collections.GuardedList.at(fields, 0)) orelse return error.TestUnexpectedResult).storage);
                },
                .call_erased => |call| {
                    calls += 1;
                    try std.testing.expectEqual((constructed orelse return error.TestUnexpectedResult), call.args.?);
                    try std.testing.expectEqual(lir.LIR.DynStorage.frame, (store.localDyn(call.args.?) orelse return error.TestUnexpectedResult).storage);
                },
                else => {},
            };
            try lir.BodyClone.appendSuccessors(store, &work, id, std.testing.allocator);
        }
        try std.testing.expectEqual(@as(usize, 1), records);
        try std.testing.expectEqual(@as(usize, 1), calls);
    }
    for (found) |present| try std.testing.expect(present);
}
