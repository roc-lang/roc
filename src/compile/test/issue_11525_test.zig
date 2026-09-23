//! Regression test for https://github.com/roc-lang/roc/issues/11525.

const harness = @import("lower_to_lir_harness.zig");

test "issue 11525: omitted nominal default through tag union alias lowers to Monotype" {
    const source =
        \\Mode : [Fast, Slow]
        \\Options := { mode : Mode ?? Fast }
        \\
        \\describe : Options -> Str
        \\describe = |{ mode: _ }| "ok"
        \\
        \\main! = |_args| {
        \\    echo!(describe({}))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .monotype_only = true });
}
