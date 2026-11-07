//! Regression tests for https://github.com/roc-lang/roc/issues/12727.
//!
//! A `var` list of number literals reassigned through a top-level function
//! that appends a non-number element is a type mismatch. The program reports
//! that mismatch and still lowers to LIR, with the mismatch becoming a runtime
//! error, rather than crashing the compiler in post-check monotype lowering.

const harness = @import("lower_to_lir_harness.zig");

test "issue 12727: a var number list given a non-number element by a top-level function reports the mismatch and lowers" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |_args| {
        \\    var $xs = [1]
        \\    $xs = bad($xs)
        \\    Ok({})
        \\}
        \\
        \\bad = |xs| xs.append(Bool.True)
    , .{ .allow_user_errors = true, .expected_report_title = "Type Mismatch" });
}

test "issue 12727: a var number list given a list element by a top-level function reports the mismatch and lowers" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |_args| {
        \\    var $xs = [1]
        \\    $xs = bad($xs)
        \\    Ok({})
        \\}
        \\
        \\bad = |xs| xs.append([2])
    , .{ .allow_user_errors = true, .expected_report_title = "Type Mismatch" });
}

test "issue 12727: a var number list given a tag element by a top-level function reports the mismatch and lowers" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |_args| {
        \\    var $xs = [1]
        \\    $xs = bad($xs)
        \\    Ok({})
        \\}
        \\
        \\bad = |xs| xs.append(Ok(1))
    , .{ .allow_user_errors = true, .expected_report_title = "Type Mismatch" });
}

test "issue 12727: a var number list given a record element by a top-level function reports the mismatch and lowers" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |_args| {
        \\    var $xs = [1]
        \\    $xs = bad($xs)
        \\    Ok({})
        \\}
        \\
        \\bad = |xs| xs.append({ a: 1 })
    , .{ .allow_user_errors = true, .expected_report_title = "Type Mismatch" });
}
