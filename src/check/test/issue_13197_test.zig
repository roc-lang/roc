//! Regression tests for issue 13197.

const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/13197
//
// Static dispatch is type-directed, so the check order does not put an
// annotated method's group before a definition that dispatches to it. The
// dispatch instantiates the method's predeclared scheme, and the boundary that
// publishes the method's own scheme relates the use to it. When the method's
// body closes an implicitly opened row of its result, relating unifies the
// use's copy of that row with the closed row, so the copy must not have been
// generalized by then.

test "issue 13197: a dispatch to an unchecked annotated method is related before the caller generalizes" {
    // `total` is checked before `Reader.next`. The row of the `prefix` field
    // in `next`'s result is implicitly opened, and `next`'s body closes it by
    // returning the receiver's own `prefix`, whose row is the closed one the
    // nominal's backing declares.
    const source =
        \\total : Reader -> U64
        \\total = |reader| match reader.next() {
        \\    Item(_) => 1
        \\    Done => 0
        \\}
        \\
        \\Reader :: { count : U64, prefix : [Prefix(Str), Unprefixed] }.{
        \\    next : Reader -> [Item({ prefix : [Prefix(Str), Unprefixed] }), Done]
        \\    next = |reader| if reader.count == 0 Done else Item({ prefix: reader.prefix })
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
    try test_env.assertDefType("total", "Reader -> U64");
}
