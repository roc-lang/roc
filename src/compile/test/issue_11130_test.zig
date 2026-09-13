//! Regression tests for record update normalization (issue #11130).
const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

fn updateChain(allocator: std.mem.Allocator, fields: usize, updates: usize) std.mem.Allocator.Error![]u8 {
    var source: std.ArrayList(u8) = .empty;
    errdefer source.deinit(allocator);
    try source.appendSlice(allocator, "Big : {\n");
    for (0..fields) |i| try source.print(allocator, "f{d}: Try(Str, [Null]),\n", .{i});
    try source.appendSlice(allocator, "}\nmk : Str -> Big\nmk = |s| {\n");
    for (0..fields) |i| try source.print(allocator, "f{d}: Ok(s),\n", .{i});
    try source.appendSlice(allocator, "}\nbig : Str -> Str\nbig = |s| {\na0 = mk(s)\n");
    for (1..updates + 1) |i| try source.print(allocator, "a{d} = {{ ..a{d}, f{d}: Ok(\"{d}\") }}\n", .{ i, i - 1, i % fields, i });
    try source.print(allocator,
        \\match a{d}.f0 {{ Ok(x) => x, Err(Null) => "" }}
        \\}}
        \\main! = |args| {{
        \\echo!(big(args.len().to_str()))
        \\Ok({{}})
        \\}}
    , .{updates});
    return source.toOwnedSlice(allocator);
}

fn lirPassNs(fields: usize, updates: usize) harness.LowerToLirHarnessError!u64 {
    const source = try updateChain(std.testing.allocator, fields, updates);
    defer std.testing.allocator.free(source);
    var timing: lir.CheckedPipeline.TimingSnapshot = .{};
    try harness.expectLowersToLirWithOptions(source, .{ .timing_out = &timing });
    return timing.lir_passes_ns;
}

test "issue 11130: chained record updates cost LIR pass time proportional to the chain" {
    const at_30 = try lirPassNs(10, 30);
    const at_60 = try lirPassNs(10, 60);
    const at_120 = try lirPassNs(10, 120);
    if (at_120 > at_30 *| 6) {
        std.debug.print("LIR passes took {d}us, {d}us and {d}us at 30, 60 and 120 updates\n", .{ at_30 / 1000, at_60 / 1000, at_120 / 1000 });
        return error.LirPassTimeGrewSuperlinearly;
    }
}

test "issue 11130: record versions survive whole uses and branching projections" {
    try harness.expectLowersToLir(
        \\main! = |args| {
        \\a = { x: args, y: ["before"] }
        \\b = { ..a, y: ["after"] }
        \\echo!(Str.inspect(a))
        \\echo!(Str.inspect(b))
        \\echo!(Str.inspect(if args.is_empty() { a.y } else { b.y }))
        \\Ok({})
        \\}
    );
}

test "issue 11130: overwritten record fields still evaluate effects" {
    try harness.expectLowersToLir(
        \\main! = |args| {
        \\a = { x: args, y: {
        \\echo!("first")
        \\"old"
        \\} }
        \\b = { ..a, y: {
        \\echo!("second")
        \\"new"
        \\} }
        \\echo!(b.y)
        \\Ok({})
        \\}
    );
}

fn inspectWideRecordConstruction(store: *const lir.LirStore, layouts: *const @import("layout").Store) harness.LowerToLirHarnessError!void {
    var builds: usize = 0;
    var projections: usize = 0;
    for (0..store.cfStmtCount()) |index| {
        const stmt = store.getCFStmt(@enumFromInt(@as(u32, @intCast(index))));
        if (stmt == .assign_struct and stmt.assign_struct.fields.len == 50) builds += 1;
        if (stmt != .assign_ref or stmt.assign_ref.op != .field) continue;
        const source = store.getLocal(stmt.assign_ref.op.field.source);
        const source_layout = layouts.getLayout(source.layout_idx);
        if (source_layout.tag == .struct_ and layouts.getStructInfo(source_layout).fields.len == 50) projections += 1;
    }
    // The store is append-only: even statements later deleted by normalization
    // count here. Updating one field must not emit a complete record each time.
    try std.testing.expect(builds <= 2);
    try std.testing.expect(projections <= 100);
}

test "issue 11130: lowering avoids constructing and projecting every record version" {
    const source = try updateChain(std.testing.allocator, 50, 120);
    defer std.testing.allocator.free(source);
    try harness.expectLirInspection(source, inspectWideRecordConstruction);
}
