//! Regression tests for https://github.com/roc-lang/roc/issues/11489.
//!
//! A local recursive function's argument can become concrete only through its
//! own recursive call (an accumulator passed as `[]`). The local binding group
//! generalizes once, after that recursive link is unified and the dispatches it
//! pinned are resolved, so the enclosing function's type relates the input
//! elements to the grouped output.

const TestEnv = @import("./TestEnv.zig");

test "issue 11489 - local recursive helper's nested appends relate input and output elements" {
    const source =
        \\split = |items, fun| {
        \\    aux = |rest, inner_acc, acc| match rest {
        \\        [] => acc
        \\        [h] => acc.append(inner_acc.append(h))
        \\        [f, s, .. as t] => {
        \\            if (fun(f, s)) {
        \\                aux(t.prepend(s), inner_acc.append(f), acc)
        \\            } else {
        \\                aux(t.prepend(s), [], acc.append(inner_acc.append(f)))
        \\            }
        \\        }
        \\    }
        \\    aux(items, [], [])
        \\}
    ;

    var test_env = try TestEnv.init("Split", source);
    defer test_env.deinit();

    try test_env.assertDefType("split", "List(a), (a, a -> Bool) -> List(List(a))");
}

test "issue 11489 - local recursive helper pinned only by its recursive call" {
    const source =
        \\split = |items| {
        \\    aux = |rest, inner_acc, acc| match rest {
        \\        [] => acc
        \\        [h, .. as t] => aux(t, [], acc.append(inner_acc.append(h)))
        \\    }
        \\    aux(items, [], [])
        \\}
    ;

    var test_env = try TestEnv.init("Split", source);
    defer test_env.deinit();

    try test_env.assertDefType("split", "List(a) -> List(List(a))");
}
