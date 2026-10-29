//! Regression test for https://github.com/roc-lang/roc/issues/12625.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 12625: returning Err of a parameter after a `?` lowers to LIR" {
    // `fail` early-returns `Err(Early)` through `?`, then returns `Err(tag)`
    // built from its parameter. The app checks cleanly and must lower through
    // Monotype to LIR without widening a closed tag union.
    try expectLowersToLir(
        \\fail = |tag| {
        \\    Err(Early)?
        \\    Err(tag)
        \\}
        \\
        \\main! = |_args| {
        \\    echo!(Str.inspect(fail(Missing)))
        \\    Ok({})
        \\}
    );
}
