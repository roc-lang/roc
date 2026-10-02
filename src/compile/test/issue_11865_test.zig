//! Regression test for https://github.com/roc-lang/roc/issues/11865.

const harness = @import("lower_to_lir_harness.zig");

test "issue 11865: method parameter pattern rejected by its annotation lowers to a checked crash" {
    try harness.expectLowersToLirWithOptions(
        \\Nom := { x : U64, y : Str }.{
        \\    get_x : Nom -> U64
        \\    get_x = |{ x }| x
        \\}
        \\
        \\main! = |_| {
        \\    _ = Nom.get_x(Nom.{ x: 1, y: "a" })
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Type Mismatch" });
}
