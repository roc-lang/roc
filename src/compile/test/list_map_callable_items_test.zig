//! `List.map`'s in-place branch reads each input item out of a buffer typed
//! by the output item, so the item's callable set comes from the input list
//! the extraction names. Optimized lowering specializes `List.map` for a
//! known transform and gives each use of that transform its own instance, so
//! nothing but the input list relates the extracted item to the closures the
//! list holds. These tests lower a map over capturing closures with a
//! capture-free transform and require every extracted item to keep the
//! closures' representation rather than an empty, uncallable one.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

fn expectExtractedItemsKeepTheirCallables(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    var extracts: usize = 0;
    for (0..store.cfStmtCount()) |index| {
        const stmt = store.getCFStmt(@fromBackingInt(@intCast(index)));
        if (stmt != .assign_low_level or stmt.assign_low_level.op != .list_map_extract_unsafe) continue;
        extracts += 1;
        const item_layout = layouts.getLayout(store.getLocal(stmt.assign_low_level.target).layout_idx);
        try std.testing.expect(item_layout.tag != .zst);
    }
    try std.testing.expect(extracts >= 1);
}

test "List.map over capturing closures extracts items at the closures' representation" {
    try harness.expectLirInspectionWithOptions(
        \\make_adder : U64 -> (U64 -> U64)
        \\make_adder = |n| |x| x + n
        \\
        \\main! = |args| {
        \\    adders = [make_adder(args.len())]
        \\    sums = adders.map(|add| add(1))
        \\    match sums.len() {
        \\        0 => Err(Exit(1))
        \\        _ => Ok({})
        \\    }
        \\}
    , .{ .list_in_place_map = true, .inline_mode = .wrappers }, expectExtractedItemsKeepTheirCallables);
}

test "List.map over capturing closures extracts items at the closures' representation without inlining" {
    try harness.expectLirInspectionWithOptions(
        \\make_adder : U64 -> (U64 -> U64)
        \\make_adder = |n| |x| x + n
        \\
        \\main! = |args| {
        \\    adders = [make_adder(args.len())]
        \\    sums = adders.map(|add| add(1))
        \\    match sums.len() {
        \\        0 => Err(Exit(1))
        \\        _ => Ok({})
        \\    }
        \\}
    , .{ .list_in_place_map = true }, expectExtractedItemsKeepTheirCallables);
}
