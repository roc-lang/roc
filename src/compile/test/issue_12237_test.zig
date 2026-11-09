//! Regression test for https://github.com/roc-lang/roc/issues/12237.
//!
//! A nominal type that refers to a nominal type declared later in the file,
//! where both name their type variable the same, checks cleanly and must
//! lower to LIR: the later declaration's formal is its own variable, not the
//! earlier declaration's variable of the same name.
const harness = @import("lower_to_lir_harness.zig");

test "issue 12237: nominal referring to a later nominal holding a boxed function lowers to LIR" {
    try harness.expectLowersToLir(
        \\H(s) := [
        \\    T(Box((s -> Str))),
        \\    E(List(A(s))),
        \\]
        \\A(s) := [C(Box((s -> Str)))]
        \\
        \\render : H(s), s -> Str
        \\render = |node, model|
        \\    match node {
        \\        T(h) => (Box.unbox(h))(model)
        \\        E(attrs) => attrs.fold("", |acc, a| match a {
        \\            C(h) => Str.concat(acc, (Box.unbox(h))(model))
        \\        })
        \\    }
        \\
        \\main! = |_| {
        \\    echo!(render(T(Box.box(|t| t)), "x"))
        \\    Ok({})
        \\}
    );
}

test "issue 12237: later nominal reusing an earlier nominal's variable name keeps its own formal" {
    try harness.expectLowersToLir(
        \\H(s) := [T(s), E(A(s))]
        \\A(s) := [C(List(s))]
        \\
        \\render : H(s), s -> List(s)
        \\render = |node, model|
        \\    match node {
        \\        T(_) => [model]
        \\        E(C(items)) => items
        \\    }
        \\
        \\main! = |_| {
        \\    _items = render(T("y"), "x")
        \\    Ok({})
        \\}
    );
}
