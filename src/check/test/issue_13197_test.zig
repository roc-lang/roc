//! Regression tests for issue 13197.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/13197
//
// Static dispatch is type-directed, so the check order does not put an
// annotated method's group before a definition that dispatches to it. The
// dispatch instantiates the method's predeclared scheme, and the boundary that
// publishes the method's own scheme relates the use to it. When the method's
// body closes the implicitly opened row of its result, relating unifies the
// use's copy of that row with the closed row, so the copy must not have been
// generalized by then: that would merge a finished scheme with a body still
// being checked.

test "issue 13197: a dispatch to an unchecked annotated method is related before the caller generalizes" {
    // `total` is checked before `Reader.next`, and `next` returns the closed
    // row of its receiver's field.
    const source =
        \\total : Reader -> U64
        \\total = |opened| match opened.next() {
        \\    Item(n) => n
        \\    Done => 0
        \\}
        \\
        \\Reader :: { current : [Item(U64), Done] }.{
        \\    next : Reader -> [Item(U64), Done]
        \\    next = |reader| reader.current
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
    try test_env.assertDefType("total", "Reader -> U64");
}

test "issue 13197: an annotated method and a function that dispatch to each other are related before either generalizes" {
    // `first_overlay` dispatches to `Index.get` before `get` is checked, and
    // `get` calls the still-open `first_overlay` by name. Each body closes the
    // row of its result.
    const source =
        \\first_overlay : { first : Index, fallback : [Found(Str), Missing] }, U64 -> [Found(Str), Missing]
        \\first_overlay = |view, position| match view.first.get(position) {
        \\    Found(value) => Found(value)
        \\    Missing => view.fallback
        \\}
        \\
        \\Index :: [Flat({ only : [Found(Str), Missing] }), Overlay({ first : Index, fallback : [Found(Str), Missing] })].{
        \\    get : Index, U64 -> [Found(Str), Missing]
        \\    get = |index, position| match index {
        \\        Flat(flat) => flat.only
        \\        Overlay(view) => first_overlay(view, position)
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}
