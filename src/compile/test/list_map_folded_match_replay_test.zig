//! Debug lowering checks the direct solved-to-LIR path against a materialized
//! Lambda Mono tree. When the direct path folds `List.map`'s in-place reuse
//! match away (the element layouts are not interchangeable), it records the
//! fold and the materializer replays it, asserting that the record names a
//! `List.map` reuse site resolved to its constant-0 branch. These tests lower
//! programs whose reuse match folds and whose reuse match stays, so the replay
//! channel and its assertion run on real Builtin code, and confirm from the
//! LIR which of the two happened.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

fn countListMapCanReuse(store: *const lir.LirStore) usize {
    var count: usize = 0;
    for (0..store.cfStmtCount()) |index| {
        const stmt = store.getCFStmt(@fromBackingInt(@intCast(index)));
        if (stmt == .assign_low_level and stmt.assign_low_level.op == .list_map_can_reuse) count += 1;
    }
    return count;
}

fn expectReuseMatchFolded(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    try std.testing.expectEqual(@as(usize, 0), countListMapCanReuse(store));
}

fn expectReuseMatchKept(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    try std.testing.expect(countListMapCanReuse(store) >= 1);
}

test "List.map to a non-interchangeable element folds its reuse match and replays the fold" {
    try harness.expectLirInspectionWithOptions(
        \\main! = |_args| {
        \\    nums : List(U64)
        \\    nums = [1, 2, 3]
        \\    strs : List(Str)
        \\    strs = nums.map(|n| n.to_str())
        \\    match strs.len() {
        \\        0 => Err(Exit(1))
        \\        _ => Ok({})
        \\    }
        \\}
    , .{ .list_in_place_map = true }, expectReuseMatchFolded);
}

test "List.map to an interchangeable element keeps its reuse match" {
    try harness.expectLirInspectionWithOptions(
        \\main! = |_args| {
        \\    nums : List(U64)
        \\    nums = [1, 2, 3]
        \\    doubled : List(U64)
        \\    doubled = nums.map(|n| n * 2)
        \\    match doubled.len() {
        \\        0 => Err(Exit(1))
        \\        _ => Ok({})
        \\    }
        \\}
    , .{ .list_in_place_map = true }, expectReuseMatchKept);
}
