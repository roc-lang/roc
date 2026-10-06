//! Regression tests for rejections decided after the rejected value was
//! checked, and for consumers of erroneous values. A literal's conversion can
//! be rejected after the names bound from it were checked; every name bound
//! from it, through any chain of aliases and destructures, binds nothing, so
//! its uses add no report. An erroneous value already owns its report, so a
//! `return`, a branch, a function body, or an annotation that consumes it
//! reports nothing further, and no report prints the erroneous type.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

fn expectOnlyTypeProblems(source: []const u8, titles: []const []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertTypeErrorTitles(titles);
    try test_env.assertNoReportRendersErrorType();
}

fn expectOnlyCanError(source: []const u8, title: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneCanError(title);
    try std.testing.expectEqual(@as(usize, 0), try test_env.typeProblemCount());
    try test_env.assertNoReportRendersErrorType();
}

test "a name `?` unwraps from a string literal rejected as a Try adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    y = "abc"?
        \\    Ok(y.to_str())
        \\}
    , &.{"Type Mismatch"});
}

test "a name `?` unwraps from a numeral rejected as a Try adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    y = 5?
        \\    Ok(y.to_str())
        \\}
    , &.{"Type Mismatch"});
}

test "a match branch binder of a literal its patterns reject adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x|
        \\    match "abc" {
        \\        Ok(y) => y.to_str()
        \\        Err(_) => ""
        \\    }
    , &.{ "Unconditional Condition", "Missing Method" });
}

test "a destructured binder of a literal its pattern rejects adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    (a, _b) = "abc"
        \\    a.to_str()
        \\}
    , &.{"Missing Method"});
}

test "a name bound from a local whose literal a later `?` rejects adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    s = "abc"
        \\    y = s?
        \\    Ok(y.to_str())
        \\}
    , &.{"Type Mismatch"});
}

test "a name bound through a chain of aliases from a literal a later `?` rejects adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    s = "abc"
        \\    t = s
        \\    y = t?
        \\    Ok(y.to_str())
        \\}
    , &.{"Type Mismatch"});
}

test "a name bound through a destructure of an alias of a rejected literal adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    p = "abc"
        \\    (a, _b) = p
        \\    c = a
        \\    c.to_str()
        \\}
    , &.{"Missing Method"});
}

test "a loop binder of a literal whose default is rejected adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    var $acc = ""
        \\    for y in 5 {
        \\        $acc = y.to_str()
        \\    }
        \\    $acc
        \\}
    , &.{"Type Not Determined"});
}

test "returning an erroneous value from a closed result adds no mismatch" {
    try expectOnlyCanError(
        \\f : I64 -> [Blue]
        \\f = |x| {
        \\    if x > 0 {
        \\        return Err(undefined_fn(x))
        \\    }
        \\    Blue
        \\}
    , "Name Not In Scope");
}

test "an erroneous branch of a closed result adds no mismatch" {
    try expectOnlyCanError(
        \\g : I64 -> [Blue]
        \\g = |x| if x > 0 Err(undefined_fn(x)) else Blue
    , "Name Not In Scope");
}

test "an erroneous function body of a closed result adds no mismatch" {
    try expectOnlyCanError(
        \\h : [Blue] -> [Blue]
        \\h = |_x| Err(undefined_fn(1))
    , "Name Not In Scope");
}

test "an annotation of an erroneous function value adds no mismatch" {
    try expectOnlyCanError(
        \\k : I64, I64 -> I64
        \\k = |x| undefined_fn(x)
    , "Name Not In Scope");
}

test "a valid `?` on a numeral-producing call still checks" {
    var test_env = try TestEnv.init("Test",
        \\parse : Str -> Try(I64, [Bad])
        \\parse = |s| if s == "" Err(Bad) else Ok(41)
        \\
        \\run = |x| {
        \\    y = parse(x)?
        \\    Ok((y + 1).to_str())
        \\}
    );
    defer test_env.deinit();

    try test_env.assertDefType("run", "Str -> Try(Str, [Bad])");
}
