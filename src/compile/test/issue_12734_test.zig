//! Composition slots remain explicit, distinct mutable data after LIR passes.
const std = @import("std");
const collections = @import("collections");
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
    // Argument records' descriptors are composed; every dictionary is a
    // word of static data or of a type table.
    try std.testing.expect(descriptors > 0);
    try std.testing.expectEqual(@as(usize, 0), dictionaries);
}

test "issue 12734: boxy composition sites survive ARC and static data compaction" {
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
    , .{ .specialization_strategy = .boxy }, inspect);
}

fn expectNoSites(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    for (lowered.frozen_static_data.?.exports) |item| try std.testing.expect(!item.is_mutable);
    const store = &lowered.lir_result.store;
    var compositions: usize = 0;
    for (0..store.cfStmtCount()) |i| {
        const stmt = store.getCFStmt(@fromBackingInt(@intCast(i)));
        if (stmt != .dyn) continue;
        try std.testing.expectEqual(null, stmt.dyn.composition_cache);
        if (lir.DynOps.isComposition(store.stmtRow(stmt.dyn.data))) compositions += 1;
    }
    try std.testing.expect(compositions > 0);
}

test "a consumer that reads no composition sites gets no site slots" {
    try harness.runLoweredInspection(
        \\pair = |x| { first: x, second: [x] }
        \\both = |f, x| f(x, x)
        \\render = |x| Str.inspect(both(|a, b| { a, b }, pair(x)))
        \\main! = |args| {
        \\    echo!(render(args))
        \\    echo!(render(args.len()))
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy, .boxy_composition_sites = false }, expectNoSites);
}

/// How many dictionary compositions read only static data (`closed`), and how
/// many read a local the program computes (`open`).
const Compositions = struct { closed: usize = 0, open: usize = 0 };

fn dictionaryCompositions(store: *const lir.LirStore) std.mem.Allocator.Error!Compositions {
    var static_locals: collections.DenseMapUnmanaged(lir.LIR.LocalId, void) = .empty;
    defer static_locals.deinit(std.testing.allocator);
    for (0..store.cfStmtCount()) |i| {
        const stmt = store.getCFStmt(@fromBackingInt(@intCast(i)));
        if (stmt == .assign_literal and stmt.assign_literal.value == .static_data) try static_locals.put(std.testing.allocator, stmt.assign_literal.target, {});
    }
    var result: Compositions = .{};
    for (0..store.cfStmtCount()) |i| {
        const stmt = store.getCFStmt(@fromBackingInt(@intCast(i)));
        if (stmt != .dyn) continue;
        const op = store.stmtRow(stmt.dyn.data);
        if (op != .dict_compose) continue;
        const captures = store.getLocalSpan(op.dict_compose.captures);
        var closed = true;
        for (0..captures.len) |capture| closed = closed and static_locals.contains(lir.LirStore.GuardedList.at(captures, capture));
        if (closed) result.closed += 1 else result.open += 1;
    }
    return result;
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
    const compositions = try dictionaryCompositions(&lowered.lir_result.store);
    try std.testing.expectEqual(@as(usize, 0), compositions.closed);
    try std.testing.expect(staticCallables(lowered.frozen_static_data.?.exports) > 0);
}

fn expectOpenDictionaryFromTable(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    const store = &lowered.lir_result.store;
    const compositions = try dictionaryCompositions(store);
    try std.testing.expectEqual(@as(usize, 0), compositions.open + compositions.closed);
    var table_reads: usize = 0;
    for (0..store.cfStmtCount()) |i| {
        const stmt = store.getCFStmt(@fromBackingInt(@intCast(i)));
        if (stmt == .dyn and store.stmtRow(stmt.dyn.data) == .table_entry) table_reads += 1;
    }
    try std.testing.expect(table_reads > 0);
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
