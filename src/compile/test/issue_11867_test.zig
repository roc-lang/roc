//! Regression test for https://github.com/roc-lang/roc/issues/11867.

const harness = @import("lower_to_lir_harness.zig");

test "issue 11867: optional access of a required nominal record field lowers to a checked crash" {
    try harness.expectLowersToLirWithOptions(
        \\Point := { x : U64, y : U64 }
        \\
        \\describe : Point -> Str
        \\describe = |point| Str.inspect(point.?x)
        \\
        \\main! = |_args| {
        \\    echo!(describe(Point.{ x: 1, y: 2 }))
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Optional Access Of Required Field" });
}

test "issue 11867: optional access of a required record field lowers to a checked crash" {
    try harness.expectLowersToLirWithOptions(
        \\describe : { x : U64 } -> Str
        \\describe = |r| Str.inspect(r.?x)
        \\
        \\main! = |_args| {
        \\    echo!(describe({ x: 1 }))
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Optional Access Of Required Field" });
}
