//! Regression coverage for https://github.com/roc-lang/roc/issues/12988.
//! An interpolation whose target is the builtin `Str` but whose
//! `from_interpolation` dispatch checking rejected keeps that dispatch, so
//! lowering crashes at it instead of concatenating its parts.
//!
//! Only the second interpolation's parts get a type mismatch report, which
//! retires them. The first interpolation's dispatch is rejected without a
//! report at its parts, so it reaches lowering with a `checked_error` plan.

const harness = @import("lower_to_lir_harness.zig");

test "issue 12988: rejected builtin Str interpolation lowers without a compiler crash" {
    try harness.expectLowersToLirWithOptions(
        \\recite : List(Str) -> Str
        \\recite = |strings| {
        \\    var $output = ""
        \\
        \\    count = strings.len()
        \\
        \\    if count == 0 {
        \\        ""
        \\    } else {
        \\        for x in 1..=count {
        \\            if x == count {
        \\                $output = "$output\nAnd all for the want of a ${Ok(strings.get(x - 1))}"
        \\            } else {
        \\                $output = "$output\nFor want of a ${Ok(strings.get(x - 1))} the ${Ok(strings.get(x))} was lost."
        \\            }
        \\        }
        \\    }
        \\}
        \\
        \\main! = |args| {
        \\    echo!(recite(args))
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Type Mismatch" });
}
