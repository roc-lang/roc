//! Composition slots remain explicit, distinct mutable data after LIR passes.
const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

fn inspect(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    const store = &lowered.lir_result.store;
    const exports = lowered.frozen_static_data.?.exports;
    var slots: std.ArrayList(lir.LIR.StaticDataId) = .empty;
    defer slots.deinit(std.testing.allocator);
    var names = std.StringHashMap(void).init(std.testing.allocator);
    defer names.deinit();
    for (exports) |item| {
        if (!item.is_mutable) continue;
        try std.testing.expect(!names.contains(item.symbol_name));
        try names.put(item.symbol_name, {});
        try std.testing.expect(std.mem.startsWith(u8, item.symbol_name, "roc__boxy_site_"));
        try std.testing.expect(std.mem.allEqual(u8, item.bytes, 0));
        try slots.append(std.testing.allocator, item.value_id.?);
    }
    var descriptors: usize = 0;
    var dictionaries: usize = 0;
    for (0..store.cfStmtCount()) |i| {
        const stmt = store.getCFStmt(@fromBackingInt(@intCast(i)));
        if (stmt != .dyn) continue;
        const op = store.stmtRow(stmt.dyn.data);
        if (!lir.DynOps.isComposition(op)) {
            try std.testing.expectEqual(null, stmt.dyn.composition_cache);
            continue;
        }
        try std.testing.expect(std.mem.findScalar(lir.LIR.StaticDataId, slots.items, stmt.dyn.composition_cache.?) != null);
        if (op == .dict_compose or op == .dict_record) dictionaries += 1 else descriptors += 1;
    }
    try std.testing.expect(descriptors > 0);
    try std.testing.expect(dictionaries > 0);
}

test "issue 12734: boxy composition sites survive ARC and static data compaction" {
    try harness.runLoweredInspection(
        \\pair = |x| { first: x, second: [x] }
        \\render = |x| Str.inspect(pair(x))
        \\main! = |args| {
        \\    echo!(render(args))
        \\    echo!(render(args.len()))
        \\    echo!(render(args))
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy }, inspect);
}
