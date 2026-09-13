//! Regression test for issue #11235.

const harness = @import("lower_to_lir_harness.zig");

test "issue 11235: disjoint error propagation lowers through LIR" {
    try harness.expectLowersToLir(
        \\f : Try({}, [..a]) -> Try({}, [A(Str), ..a])
        \\f = |x| {
        \\    x?
        \\    Err(A("new"))
        \\}
        \\
        \\main! = |_args| f(Err(B))
    );
}

test "issue 11235: an error tag supplied by both the annotation and the extension variable lowers to Monotype" {
    // Repro for https://github.com/roc-lang/roc/issues/11235.
    //
    // `f` adds `A` to whatever error tags its argument carries, and the call
    // site passes `Err(A)`, so the extension variable `a` resolves to a union
    // that already contains `A`. The sealed error union is `[A]`, and Monotype
    // lowering seals it once rather than building a union that repeats `A`.
    try harness.expectLowersToLirWithOptions(
        \\f : Try({}, [..a]) -> Try({}, [A, ..a])
        \\f = |x| {
        \\    x?
        \\    Err(A)
        \\}
        \\
        \\main! = |_args| {
        \\    f(Err(A))
        \\}
    , .{ .monotype_only = true });
}

test "issue 11235: overlapping payload tags lower through LIR" {
    try harness.expectLowersToLir(
        \\f : Try({}, [..a]) -> Try({}, [A(Str), ..a])
        \\f = |x| {
        \\    x?
        \\    Err(A("new"))
        \\}
        \\
        \\main! = |_args| {
        \\    f(Err(A("existing")))
        \\}
    );
}

test "issue 11235: independent calls preserve overlapping and disjoint error rows" {
    try harness.expectLowersToLir(
        \\f : Try({}, [..a]) -> Try({}, [A(Str), ..a])
        \\f = |x| {
        \\    x?
        \\    Err(A("new"))
        \\}
        \\
        \\main! = |args| match args {
        \\    [] => f(Err(A("existing")))
        \\    [_] => f(Err(B("propagated")))
        \\    _ => f(Ok({}))
        \\}
    );
}
