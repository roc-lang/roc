//! Boxy descriptors and dictionaries are immutable static data after LIR
//! passes: a program composes none at run time.
const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

/// How many type table words the program's bodies read.
fn tableReads(store: *const lir.LirStore) usize {
    var count: usize = 0;
    for (0..store.cfStmtCount()) |i| {
        const stmt = store.getCFStmt(@fromBackingInt(@intCast(i)));
        if (stmt == .dyn and store.stmtRow(stmt.dyn.data) == .table_entry) count += 1;
    }
    return count;
}

fn expectTableReads(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    // The generic bodies read their argument records' descriptors from
    // their type tables.
    try std.testing.expect(tableReads(&lowered.lir_result.store) > 0);
}

test "issue 12734: boxy descriptors survive ARC and static data compaction as immutable static data" {
    try harness.runLoweredInspection(
        \\pair = |x| { first: x, second: [x] }
        \\both = |f, x| f(x, x)
        \\render = |x| Str.inspect(both(|a, b| { a, b }, pair(x)))
        \\main! = |args| {
        \\    echo!(render(args))
        \\    echo!(render(args.len()))
        \\    echo!(render(args))
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy }, expectTableReads);
}

fn staticCallables(exports: []const lir.Program.StaticDataExport) usize {
    var count: usize = 0;
    for (exports) |item| {
        for (item.relocations) |relocation| {
            if (relocation.callable_capture_offset != null) count += 1;
        }
    }
    return count;
}

fn expectClosedDictionariesStatic(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    try std.testing.expect(staticCallables(lowered.frozen_static_data.?.exports) > 0);
}

fn expectOpenDictionaryFromTable(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    try std.testing.expect(tableReads(&lowered.lir_result.store) > 0);
}

test "a dictionary of closed types is static data, even inside a generic body" {
    try harness.runLoweredInspection(
        \\Point := { x : U64 }.{
        \\    is_eq = |a, b| a.x == b.x
        \\}
        \\
        \\equal = |a, b| a == b
        \\
        \\tagged = |value| {
        \\    same = equal(Point({ x: 1 }), Point({ x: 1 }))
        \\    { value, same }
        \\}
        \\
        \\main! = |args| {
        \\    echo!(Str.inspect(tagged(args.len()).same))
        \\    echo!(Str.inspect(tagged(args).same))
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy }, expectClosedDictionariesStatic);
}

test "a dictionary that captures a body's hidden parameter is a word of its type table" {
    try harness.runLoweredInspection(
        \\equal = |a, b| a == b
        \\
        \\pairs_equal = |a, b| equal({ first: a }, { first: b })
        \\
        \\main! = |args| {
        \\    echo!(Str.inspect(pairs_equal(args.len(), args.len())))
        \\    echo!(Str.inspect(pairs_equal(args, args)))
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy }, expectOpenDictionaryFromTable);
}
