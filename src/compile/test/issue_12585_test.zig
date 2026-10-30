//! Regression tests for https://github.com/roc-lang/roc/issues/12585.
//!
//! Mutually recursive functions where one uses `?` on a call whose error row
//! is closed must check and lower cleanly through post-check monotype.
const harness = @import("lower_to_lir_harness.zig");

const closed_row_app =
    \\fails : Str -> Try(U64, [Oops(Str)])
    \\fails = |s| if s.is_empty() Err(Oops(s)) else Ok(1)
    \\
    \\a = |s| {
    \\    n = fails(s)?
    \\    b(n)
    \\}
    \\
    \\b = |n| Ok(n + (a("") ?? 0))
    \\
    \\main! = |_args| {
    \\    _ = a("x")
    \\    Ok({})
    \\}
;

const unannotated_app =
    \\list = |s| if s.is_empty() Err(NotFound) else Ok([])
    \\
    \\a = |s| {
    \\    xs = list(s)?
    \\    b(xs)
    \\}
    \\
    \\b = |xs| {
    \\    _ = a(xs.first() ?? "")
    \\    Ok({})
    \\}
    \\
    \\main! = |_args| {
    \\    _ = a("x")
    \\    Ok({})
    \\}
;

const two_callers_app =
    \\oops : Str -> Try(U64, [Oops(Str)])
    \\oops = |s| if s.is_empty() Err(Oops(s)) else Ok(1)
    \\
    \\nope : Str -> Try(U64, [Nope])
    \\nope = |s| if s.is_empty() Err(Nope) else Ok(2)
    \\
    \\a = |s| {
    \\    n = oops(s)?
    \\    b(n)
    \\}
    \\
    \\c = |s| {
    \\    n = nope(s)?
    \\    b(n)
    \\}
    \\
    \\b = |n| Ok(n + (a("") ?? 0) + (c("") ?? 0))
    \\
    \\main! = |_args| {
    \\    _ = a("x")
    \\    _ = c("y")
    \\    Ok({})
    \\}
;

test "issue 12585: mutual recursion through `?` on a closed error row lowers" {
    try harness.expectLowersToLir(closed_row_app);
}

test "issue 12585: mutual recursion through `?` on an inferred error row lowers" {
    try harness.expectLowersToLir(unannotated_app);
}

test "issue 12585: two recursive callers tail-call through `?` on different error rows" {
    try harness.expectLowersToLir(two_callers_app);
}
