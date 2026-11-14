//! Repro test for https://github.com/roc-lang/roc/issues/13051
//!
//! `weak` is unannotated, so the dispatches `f` makes on it wait for module
//! finalization to settle `weak`'s type. Each call of `f` instantiates `f`
//! before that, and returns a list of its own callback's result type. Boxy
//! evaluates `lengths` and `selves` at compile time, calling `f` at each
//! call's own instantiation.
const harness = @import("lower_to_lir_harness.zig");

test "issue 13051: Boxy calls a function that maps a method result of an unannotated string constant at each call's callback type" {
    try harness.expectLowersToLirWithOptions(
        \\weak = "a,b,c"
        \\f = |g| weak.split_on(",").map(g)
        \\lengths = f(|s| s.count_utf8_bytes())
        \\selves = f(|s| s)
        \\
        \\main! = |_args| {
        \\    echo!("${lengths.len().to_str()} ${selves.len().to_str()}")
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy });
}
