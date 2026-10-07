//! Tests that `List.prefetched` is free in the lowered program: it becomes a
//! hint that borrows the list plus an alias of the list, so it adds no
//! reference counting and leaves a loop's in-place writes in place.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");

const harness = @import("lower_to_lir_harness.zig");

/// A loop that overwrites one table entry per step, optionally hinting the
/// entry it will write next.
fn fillApp(comptime hint: []const u8) []const u8 {
    return "fill : List(U16), U64 -> List(U16)\n" ++
        "fill = |table_0, n| {\n" ++
        "    var $table = table_0\n" ++
        "    var $i = 0.U64\n" ++
        "    while $i < n {\n" ++
        hint ++
        "        $table = List.set($table, $i, $i.to_u16_wrap()) ?? $table\n" ++
        "        $i = $i + 1\n" ++
        "    }\n" ++
        "    $table\n" ++
        "}\n" ++
        "\n" ++
        "main! : List(Str) => Try({}, [Exit(I8), ..])\n" ++
        "main! = |args| {\n" ++
        "    table = fill(List.repeat(0.U16, args.len() + 64), args.len() + 64)\n" ++
        "    echo!(Str.inspect(List.len(table)))\n" ++
        "    Ok({})\n" ++
        "}\n";
}

const Shape = struct {
    found: bool = false,
    hints: usize = 0,
    in_place_sets: usize = 0,
    checked_sets: usize = 0,
    increfs: usize = 0,
    decrefs: usize = 0,
};

var counted: Shape = .{};

fn countFillShape(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    counted = .{};
    const gpa = std.testing.allocator;
    const buf = try gpa.alloc(u8, 1 << 22);
    defer gpa.free(buf);
    for (0..store.getProcSpecs().len) |index| {
        var writer = std.Io.Writer.fixed(buf);
        try lir.DebugPrint.writeProc(gpa, store, layouts, @enumFromInt(@as(u32, @intCast(index))), &writer);
        const text = writer.buffered();
        if (std.mem.count(u8, text, "u64_to_u16_wrap") == 0) continue;
        if (std.c.getenv("PREFETCHED_DUMP") != null) std.debug.print("\n===== fill proc =====\n{s}\n", .{text});
        counted = .{
            .found = true,
            .hints = std.mem.count(u8, text, "list_prefetch("),
            .in_place_sets = std.mem.count(u8, text, "list_set_in_place_unsafe("),
            .checked_sets = std.mem.count(u8, text, "low_level list_set("),
            .increfs = std.mem.count(u8, text, "incref "),
            .decrefs = std.mem.count(u8, text, "decref "),
        };
        return;
    }
}

test "List.prefetched adds a hint and nothing else to a loop that writes in place" {
    const opts: harness.LirLoweringOptions = .{ .inline_mode = .wrappers, .prove_ranges = true };

    try harness.expectLirInspectionWithOptions(fillApp(""), opts, countFillShape);
    try std.testing.expect(counted.found);
    const plain = counted;
    try std.testing.expectEqual(@as(usize, 0), plain.hints);
    // The loop's write is promoted to an in-place store.
    try std.testing.expect(plain.in_place_sets > 0);

    try harness.expectLirInspectionWithOptions(fillApp("        $table = List.prefetched($table, $i + 8)\n"), opts, countFillShape);
    try std.testing.expect(counted.found);
    // One hint per copy of the loop the promoter keeps.
    try std.testing.expect(counted.hints > 0);
    // The hint is a read of the list, so the write stays in place, and the
    // result is an alias of the list, so nothing is retained or released
    // that the plain loop did not already retain or release.
    try std.testing.expectEqual(plain.in_place_sets, counted.in_place_sets);
    try std.testing.expectEqual(plain.checked_sets, counted.checked_sets);
    try std.testing.expectEqual(plain.increfs, counted.increfs);
    try std.testing.expectEqual(plain.decrefs, counted.decrefs);
}
