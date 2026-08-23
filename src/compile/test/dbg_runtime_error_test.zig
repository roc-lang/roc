//! Regression test for dbg statements containing runtime error expressions.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "dbg statement inspecting identifier not in scope lowers to runtime error LIR without compiler panic" {
    try expectLowersToLirWithOptions(
        \\main! = |_| {
        \\    dbg undefined_variable
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true });
}
