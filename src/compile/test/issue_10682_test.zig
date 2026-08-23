//! Regression test for issue #10682.

const harness = @import("lower_to_lir_harness.zig");

test "issue 10682: local closure interpolating two dynamic segments lowers under wrapper inlining" {
    // Repro for https://github.com/roc-lang/roc/issues/10682.
    try harness.expectLowersToLirWithOptions(
        \\run : Str -> Str
        \\run = |s| {
        \\    f = |a, b| "${a}${b}"
        \\    f("x", s)
        \\}
        \\
        \\main! = |args| {
        \\    s = match args {
        \\        [first, ..] => first
        \\        [] => "y"
        \\    }
        \\    echo!(run(s))
        \\    Ok({})
        \\}
    , .{ .inline_mode = .wrappers });
}
