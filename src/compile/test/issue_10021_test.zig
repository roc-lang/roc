//! Regression test for issue #10021.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 10021: stored closure mapping a function argument error lowers to LIR" {
    // Repro for https://github.com/roc-lang/roc/issues/10021.
    // A well-typed closure stored in and called through a record field must
    // preserve the mapped Try error type through post-check lowering.
    try expectLowersToLir(
        \\foo : Str -> Try({}, [FooFailed])
        \\foo = |_s| Ok({})
        \\
        \\bar = |f| {
        \\    baz = || f("hi").map_err(|_| BazFailed)
        \\    { baz }
        \\}
        \\
        \\main! = |_args| {
        \\    wrapped = bar(foo)
        \\    baz = wrapped.baz
        \\    match baz() {
        \\        Ok(_) => Ok({})
        \\        Err(_) => Ok({})
        \\    }
        \\}
    );
}
