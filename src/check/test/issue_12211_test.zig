//! Regression tests for https://github.com/roc-lang/roc/issues/12211.
//!
//! An expression's own dispatch (an operator or a method call) belongs to that
//! expression and fails at it, even when its receiver's type is only decided
//! by a later expression whose unification grounds it. A `for` loop over a
//! range whose bounds have no range constructor therefore reports only the
//! missing `range_inclusive_to` method, at the range. The iteration method's
//! own where clause exists only because of the rejected range, so it is not
//! reported, neither at the range nor at an expression in the loop body.
const TestEnv = @import("TestEnv.zig");

test "issue 12211: for over a rejected Str range reports only the missing range method" {
    const source =
        \\echo! : Str => {}
        \\echo! = |_| {}
        \\
        \\main! = |_| {
        \\    for letter in "a"..="z" {
        \\        echo!(letter)
        \\    }
        \\    Ok({})
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorHighlightsWithin("Missing Method", .{ .line = 5, .start_column = 19, .end_column = 28 });
}

test "issue 12211: a method call on a literal whose type a later use decides fails at the method call" {
    const source =
        \\echo! : Str => {}
        \\echo! = |_| {}
        \\
        \\main! = |_| {
        \\    x = "a"
        \\    _y = x.shout()
        \\    echo!(x)
        \\    Ok({})
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorHighlightsWithin("Missing Method", .{ .line = 6, .start_column = 10, .end_column = 19 });
}

test "issue 12211: an operator on a literal whose type a later use decides fails at the operator" {
    const source =
        \\echo! : Str => {}
        \\echo! = |_| {}
        \\
        \\main! = |_| {
        \\    x = "a"
        \\    _y = x - "b"
        \\    echo!(x)
        \\    Ok({})
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorHighlightsWithin("Missing Method", .{ .line = 6, .start_column = 10, .end_column = 17 });
}

test "issue 12211: a for loop's own iter dispatch on a value a later use decides fails at the iterable" {
    const source =
        \\echo! : Str => {}
        \\echo! = |_| {}
        \\
        \\main! = |_| {
        \\    x = "a"
        \\    for c in x {
        \\        _d = c
        \\    }
        \\    echo!(x)
        \\    Ok({})
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorHighlightsWithin("Missing Method", .{ .line = 6, .start_column = 14, .end_column = 15 });
}

test "issue 12211: an equality requirement its comparison derived fails at the comparison when a later use decides the item type" {
    const source =
        \\main! = |_| {
        \\    x = []
        \\    _same = x == x
        \\    _y = List.append(x, |z| z)
        \\    Ok({})
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorHighlightsWithin("Type Does Not Support Equality", .{ .line = 3, .start_column = 13, .end_column = 19 });
}

test "issue 12211: an operator requirement a function's scheme inferred fails at the use, as a declared one does" {
    const source =
        \\sub = |a, b| a - b
        \\
        \\main! = |_| {
        \\    x : Str
        \\    x = "a"
        \\    _y = sub(x, "b")
        \\    Ok({})
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorHighlightsWithin("Missing Method", .{ .line = 6, .start_column = 10, .end_column = 21 });
}
