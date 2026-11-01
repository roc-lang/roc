//! Regression test for https://github.com/roc-lang/roc/issues/12638.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 12638: a constant referencing a constant of a nominal type over Box lowers to LIR" {
    // `stub2` is a top-level constant whose value is the compile-time
    // constant `stub`, of nominal type `Handle :: Box(U64)`. Finalizing the
    // compile-time roots must transcode `stub2` against Handle's box layout.
    try expectLowersToLir(
        \\Handle :: Box(U64)
        \\
        \\stub : Handle
        \\stub = Handle.(Box.box(0))
        \\
        \\stub2 = stub
        \\
        \\main! = |_args| {
        \\    echo!("hello")
        \\    Ok({})
        \\}
    );
}

test "issue 12638: a constant referencing a recursive constant holding a nominal type over Box lowers to LIR" {
    // `Chain` recurses through its own payload, so its layout boxes the
    // recursion while `Handle`'s explicit Box stays part of each value.
    try expectLowersToLir(
        \\Handle :: Box(U64)
        \\
        \\Chain := [End, Link(Handle, Chain)]
        \\
        \\chain : Chain
        \\chain = Chain.Link(Handle.(Box.box(1)), Chain.Link(Handle.(Box.box(2)), Chain.End))
        \\
        \\chain2 = chain
        \\
        \\main! = |_args| {
        \\    echo!("hello")
        \\    Ok({})
        \\}
    );
}
