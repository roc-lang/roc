//! Regression test for issue #10346.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "issue 10346: numeric literal dispatching to annotation-only method does not panic publication or lowering" {
    try expectLowersToLirWithOptions(
        \\MyNum := U64.{
        \\    from_numeral : _
        \\}
        \\
        \\a = {
        \\    x : MyNum
        \\    x = 42
        \\    x
        \\}
        \\
        \\main! = |_| {
        \\    a
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true });
}
