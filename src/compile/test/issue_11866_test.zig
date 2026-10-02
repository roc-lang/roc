//! Regression test for https://github.com/roc-lang/roc/issues/11866.

const harness = @import("lower_to_lir_harness.zig");

test "issue 11866: binder of an effectful top-level destructure lowers to a checked crash" {
    try harness.expectLowersToLirWithOptions(
        \\Ok(text) = echo!("hello")
        \\
        \\main! = |_args| {
        \\    echo!(text)
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Effectful Top Level Value" });
}

test "issue 11866: binder of an effectful top-level record destructure lowers to a checked crash" {
    try harness.expectLowersToLirWithOptions(
        \\{ a } = echo!("hello")
        \\
        \\main! = |_args| {
        \\    _ = a
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Effectful Top Level Value" });
}

test "issue 11866: effectful annotated top-level value lowers to a checked crash" {
    try harness.expectLowersToLirWithOptions(
        \\text : {}
        \\text = echo!("hello")
        \\
        \\main! = |_args| {
        \\    _ = text
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Effectful Top Level Value" });
}
