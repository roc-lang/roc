//! Regression test for issue #10831.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");

const harness = @import("lower_to_lir_harness.zig");

fn expectSingleSourceRefreshSpecialization(
    store: *const lir.LirStore,
    _: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    var refresh_count: usize = 0;
    for (0..store.procSpecCount()) |index| {
        const proc: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const name = store.procDebugName(proc) orelse continue;
        if (std.mem.eql(u8, name, "refresh")) refresh_count += 1;
    }

    // One source-level specialization lowers to two LIR procedures. Both
    // calls have the same closed type, so they must stay at that baseline.
    try std.testing.expectEqual(@as(usize, 2), refresh_count);
}

test "issue 10831: repeated annotated calls reuse one specialization" {
    // Repro for https://github.com/roc-lang/roc/issues/10831.
    try harness.expectLirInspectionWithOptions(
        \\Item : { value : F64, summary : Str }
        \\
        \\Store : { items : List(Item) }
        \\
        \\refresh = |store| { ..store, items: store.items.map(run_item) }
        \\
        \\update : Store, Str -> (Store, Str)
        \\update = |store, _msg| (refresh(refresh(store)), "")
        \\
        \\run_item = |item| match compute(item) {
        \\    Ok(text) => { ..item, summary: text }
        \\    Err(_) => item
        \\}
        \\
        \\compute = |item| match item.value > 0 {
        \\    True => Ok(describe([item.value]))
        \\    False => Ok(describe([item.value]))
        \\}
        \\
        \\describe = |points| {
        \\    texts = points.map(|p| "{ x: ${p.to_str()}, y: ${p.to_str()} }")
        \\    "points: [${Str.join_with(texts, ", ")}], n: ${texts.len().to_str()}"
        \\}
        \\
        \\main! = |args| {
        \\    final = (update({ items: [{ value: 0, summary: "" }] }, args.len().to_str())).0
        \\    echo!(if final.items.len() == 1 { "ok" } else { "bad" })
        \\    Ok({})
        \\}
    , .{ .proc_debug_names = true }, expectSingleSourceRefreshSpecialization);
}
