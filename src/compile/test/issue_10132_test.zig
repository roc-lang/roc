//! Regression test for issue #10132.

const harness = @import("lower_to_lir_harness.zig");

test "issue 10132: try suffix in capturing effectful closure lowers with specialization" {
    // Repro for https://github.com/roc-lang/roc/issues/10132.
    try harness.expectLowersToLirWithOptions(
        \\check : Str -> Try({}, [Empty])
        \\check = |s| if Str.is_empty(s) { Err(Empty) } else { Ok({}) }
        \\
        \\main! = |_args| {
        \\    name = "x"
        \\    helper! = || {
        \\        echo!("hi ${name}")
        \\        check(name)?
        \\        Ok({})
        \\    }
        \\    match helper!() {
        \\        Ok({}) => Ok({})
        \\        Err(_) => Ok({})
        \\    }
        \\}
    , .{ .inline_mode = .wrappers });
}
