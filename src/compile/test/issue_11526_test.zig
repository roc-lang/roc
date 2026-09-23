//! Regression test for issue #11526.

const harness = @import("lower_to_lir_harness.zig");

test "issue 11526: annotated recursion can return its structural tag union argument" {
    // Repro for https://github.com/roc-lang/roc/issues/11526
    try harness.expectLowersToLirWithOptions(
        \\walk : [Open, Close] -> [Open, Close]
        \\walk = |token| match token {
        \\    Open => walk(Close)
        \\    Close => token
        \\}
        \\main! = |args| {
        \\    start = if args.is_empty() Close else Open
        \\    echo!(Str.inspect(walk(start)))
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .lss, .monotype_only = true });
}
