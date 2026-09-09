//! ARC work for a record parser derived by `Json.parse` scales with the
//! record's field count.

const std = @import("std");
const builtin = @import("builtin");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const testing = std.testing;

/// An app that parses one record of `field_count` optional string fields, the
/// shape reported in https://github.com/roc-lang/roc/issues/11088.
fn optionalFieldRecordApp(gpa: std.mem.Allocator, field_count: usize) (std.mem.Allocator.Error || std.Io.Writer.Error)![]u8 {
    var source: std.Io.Writer.Allocating = .init(gpa);
    errdefer source.deinit();
    const writer = &source.writer;
    try writer.writeAll("R : {\n");
    for (0..field_count) |index| {
        try writer.print("    f{d} : Try(Str, [Missing]),\n", .{index});
    }
    try writer.writeAll(
        \\}
        \\
        \\main! = |_| {
        \\    parsed : Try(R, _)
        \\    parsed = Json.parse("")
        \\    match parsed {
        \\        Ok(_) => Ok({})
        \\        Err(_) => Ok({})
        \\    }
        \\}
        \\
    );
    return source.toOwnedSlice();
}

fn derivedRecordParseArcWork(field_count: usize) (harness.LowerToLirHarnessError || std.Io.Writer.Error)!u64 {
    const gpa = testing.allocator;
    const source = try optionalFieldRecordApp(gpa, field_count);
    defer gpa.free(source);

    // The delta over the process-global counter is meaningful because the test
    // runner executes tests in one thread; nothing else certifies between the
    // two reads.
    const before = lir.ArcCertify.balance_queries_certified;
    try harness.expectLowersToLir(source);
    return lir.ArcCertify.balance_queries_certified - before;
}

test "ARC work for a derived record parser grows with the record's field count" {
    // The pipeline certifies its ARC output, and so moves this counter, only
    // in debug builds.
    if (builtin.mode != .Debug) return;

    const narrow = try derivedRecordParseArcWork(6);
    const wide = try derivedRecordParseArcWork(12);

    try testing.expect(narrow > 0);
    // Doubling the field count doubles the derived parser's size. A 4x bound
    // leaves room for per-field state work that is quadratic in the field
    // count, and still rejects a whole-procedure rescan per field, which is
    // what makes the reported 24-field record take a minute to build.
    if (wide > narrow * 4) {
        std.debug.print(
            "ARC work grew superlinearly: {d} ownership-balance queries at 6 optional fields, {d} at 12\n",
            .{ narrow, wide },
        );
    }
    try testing.expect(wide <= narrow * 4);
}

fn derivedRecordParseSparseWork(field_count: usize) (harness.LowerToLirHarnessError || std.Io.Writer.Error)!u64 {
    const before = lir.ArcCertify.ownership_entries_certified + lir.ArcCertify.join_constraint_steps;
    const queries = try derivedRecordParseArcWork(field_count);
    return queries + lir.ArcCertify.ownership_entries_certified + lir.ArcCertify.join_constraint_steps - before;
}

test "derived record parser keeps sparse ownership and constraint work bounded" {
    if (builtin.mode != .Debug) return;
    const narrow = try derivedRecordParseSparseWork(12);
    const wide = try derivedRecordParseSparseWork(24);
    if (wide > narrow * 4) {
        std.debug.print("ARC sparse work grew from {d} at 12 fields to {d} at 24\n", .{ narrow, wide });
    }
    try testing.expect(narrow > 0);
    try testing.expect(wide <= narrow * 4);
}
