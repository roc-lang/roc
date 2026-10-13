//! Regression test for https://github.com/roc-lang/roc/issues/12223.
//!
//! An annotated tag-union value defined after a function that appends both
//! pattern-bound and literal tags to an annotated list must type-check cleanly.
const TestEnv = @import("TestEnv.zig");

test "issue 12223: annotated value after a function appending tags to an annotated list checks cleanly" {
    const source =
        \\Foo :: [].{
        \\    Inner : [A, B]
        \\    Outer : [None, Some(Inner)]
        \\}
        \\
        \\step : List(Foo.Inner), Foo.Outer, U64 -> List(Foo.Inner)
        \\step = |items, outer, n| {
        \\    if n == 0 {
        \\        return match outer {
        \\            Some(x) => items.append(x)
        \\            _ => items
        \\        }
        \\    }
        \\    var $items = items
        \\    match outer {
        \\        Some(x) => {
        \\            $items = $items.append(x)
        \\        }
        \\        _ => {}
        \\    }
        \\    $items = $items.append(B)
        \\    $items
        \\}
        \\
        \\value : Foo.Outer
        \\value = None
    ;
    var env = try TestEnv.init("Foo", source);
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("step", "List(Foo.Inner), Foo.Outer, U64 -> List(Foo.Inner)");
    try env.assertDefType("value", "Foo.Outer");
}
