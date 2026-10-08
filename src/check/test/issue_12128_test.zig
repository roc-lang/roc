//! Regression tests for issue 12128.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/12128
//
// A loop body only includes statements; it has no final expression. Every
// expression written in a loop body, including the last one, is a statement,
// so a value it produces and nothing uses is reported like any other unused
// statement value.

test "issue 12128: an unused value at the end of a for loop body reports" {
    const source =
        \\f = || True
        \\
        \\g = |_| {
        \\    for _ in [1, 2, 3] {
        \\        f()
        \\    }
        \\    {}
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorHighlightsWithin("Type Mismatch", .{ .line = 5, .start_column = 9, .end_column = 12 });
}

test "issue 12128: an unused value at the end of a for loop expression body reports" {
    const source =
        \\f = || True
        \\
        \\g = |_| {
        \\    for _ in [1, 2, 3] {
        \\        f()
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorHighlightsWithin("Type Mismatch", .{ .line = 5, .start_column = 9, .end_column = 12 });
}

test "issue 12128: an unused value at the end of a while loop body reports" {
    const source =
        \\f = || True
        \\
        \\g = |_| {
        \\    var $i = 0
        \\    while $i < 3 {
        \\        $i = $i + 1
        \\        f()
        \\    }
        \\    {}
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorHighlightsWithin("Type Mismatch", .{ .line = 7, .start_column = 9, .end_column = 12 });
}

test "issue 12128: a for loop body ending in a {} expression checks cleanly" {
    const source =
        \\g = |xs| {
        \\    var $total = 0
        \\    for x in xs {
        \\        if x > 2 {
        \\            $total = $total + x
        \\        }
        \\    }
        \\    $total
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

// A statement written as a method call on a `var` whose value could be
// assigned back to that `var` discards an updated copy of it. That is a
// warning (design.md "Discarded Var Update"); any other unused value stays an
// error.

test "issue 12128: discarding an updated copy of a var warns and suggests reassigning it" {
    const source =
        \\g = |elements| {
        \\    var $acc = []
        \\    for e in elements {
        \\        $acc.append(e)
        \\    }
        \\    $acc
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeWarning("Discarded Var Update");
    try test_env.assertOneTypeProblemMsgContains("Discarded Var Update", "$acc = $acc.append(e)");
}

test "issue 12128: discarding an updated copy of a var outside a loop warns" {
    const source =
        \\g = |x| {
        \\    var $acc = [1, 2]
        \\    $acc.append(x)
        \\    $acc
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeWarning("Discarded Var Update");
}

test "issue 12128: discarding a method result of another type on a var is an error" {
    const source =
        \\g = |_| {
        \\    var $items = [1, 2]
        \\    $items.len()
        \\    $items
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

test "issue 12128: discarding an updated copy of an immutable binding is an error" {
    const source =
        \\g = |x| {
        \\    acc = [1, 2]
        \\    acc.append(x)
        \\    acc
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

test "issue 12128: discarding an operator result on a var is an error" {
    const source =
        \\g : I64 -> I64
        \\g = |n| {
        \\    var $count = n
        \\    $count + n
        \\    $count
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}
