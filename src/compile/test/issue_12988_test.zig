//! Regression coverage for https://github.com/roc-lang/roc/issues/12988.
//! Both interpolations are assigned to a local whose type defaults to `Str`
//! at the definition's generalization boundary. Each must be checked against
//! `Str` on its own, so each rejected value is reported and its interpolation
//! retired, and lowering never meets a rejected builtin `Str` interpolation.

const harness = @import("lower_to_lir_harness.zig");

test "issue 12988: interpolations assigned to a defaulted local are checked separately" {
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
