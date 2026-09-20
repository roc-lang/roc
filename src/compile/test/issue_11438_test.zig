//! Regression coverage for duplicate closed Monotype specializations (#11438).
const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

fn expectSingleSourceRefreshSpecialization(
    prepared: *const lir.CheckedPipeline.PreparedMonotype,
) harness.LowerToLirHarnessError!void {
    const program = prepared.program.view();
    var refresh_count: usize = 0;
    for (program.defs) |def| {
        const name = program.procDebugName(def.symbol) orelse continue;
        if (std.mem.eql(u8, program.names.exportNameText(name), "refresh")) refresh_count += 1;
    }

    // Both calls have the same closed type. Count at Monotype's boundary:
    // later closure lifting and ARC may legitimately add same-named procedures.
    try std.testing.expectEqual(@as(usize, 1), refresh_count);
}

test "issue 11438: equal closed requests reuse one specialization across alias annotations" {
    // https://github.com/roc-lang/roc/issues/11438
    // Count is transparent: both calls request the same U64 -> U64 body,
    // regardless of the checked source type at each call site.
    try harness.expectLowersToLirWithOptions(
        \\Count : U64
        \\refresh = |value| value + 1
        \\first : Count -> Count
        \\first = |value| refresh(value)
        \\second : U64 -> U64
        \\second = |value| refresh(value)
        \\main! = |args| {
        \\    echo!((first(args.len()) + second(args.len())).to_str())
        \\    Ok({})
        \\}
    , .{ .proc_debug_names = true, .prepared_inspect = expectSingleSourceRefreshSpecialization });
}
