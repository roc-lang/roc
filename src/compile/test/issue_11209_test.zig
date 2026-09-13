//! Regression test for issue #11209.

const harness = @import("lower_to_lir_harness.zig");

// repro for https://github.com/roc-lang/roc/issues/11209
//
// A `for` loop whose body discards a destructured `List.fold_until` tuple
// result lowers all the way to LIR. `projectUnusedLoopResults` clones that
// discarded body, and the nested fold's selected exit must still carry its
// compiler-generated tuple state through the clone. The options mirror the
// post-check settings `roc build` uses at its default `--opt speed`.
test "issue 11209: discarded for-loop body destructuring a fold_until tuple lowers to LIR" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    for _ in args {
        \\        (res, _) = List.fold_until(
        \\            args,
        \\            (0, 0),
        \\            |(p, f), v| {
        \\                n = if v == "" 1 else 0
        \\                if n <= p (if f == 1 Break((0, 1)) else Continue((p, 1))) else Continue((n, f))
        \\            },
        \\        )
        \\        if res == 0 0 else 0
        \\    }
        \\    Ok({})
        \\}
    , .{ .inline_mode = .wrappers, .spec_constr_clone_inlining = .all_calls });
}
