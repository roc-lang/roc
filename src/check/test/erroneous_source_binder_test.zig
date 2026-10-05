//! Regression tests for names bound by matching an erroneous value. Such a
//! pattern binds nothing, so every name it introduces is erroneous, and each
//! use of such a name is erroneous at the use: a call-like consumer is retired
//! before it introduces a dispatch relation, so the one mistake that made the
//! value erroneous is the only report.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

fn expectOnlyCanError(source: []const u8, title: []const u8) !void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneCanError(title);
    try std.testing.expectEqual(@as(usize, 0), try test_env.typeProblemCount());
}

test "a name unwrapped by `?` from an erroneous value adds no method report" {
    try expectOnlyCanError(
        \\run = |x| {
        \\    y = undefined_fn(x)?
        \\    Ok(y.to_str())
        \\}
    , "Name Not In Scope");
}

test "a match branch binder of an erroneous scrutinee adds no method report" {
    try expectOnlyCanError(
        \\run = |x| {
        \\    match undefined_fn(x) {
        \\        Ok(y) => y.to_str()
        \\        Err(_) => ""
        \\    }
        \\}
    , "Name Not In Scope");
}

test "a destructured binder of an erroneous value adds no method report" {
    try expectOnlyCanError(
        \\run = |x| {
        \\    (a, _b) = undefined_fn(x)
        \\    Ok(a.to_str())
        \\}
    , "Name Not In Scope");
}

test "a loop binder of an erroneous iterable adds no method report" {
    try expectOnlyCanError(
        \\run = |x| {
        \\    var $acc = ""
        \\    for y in undefined_fn(x) {
        \\        $acc = y.to_str()
        \\    }
        \\    $acc
        \\}
    , "Name Not In Scope");
}

test "a name unwrapped by `?` from a call with a mismatched argument reports only the mismatch" {
    var test_env = try TestEnv.init("Test",
        \\parse : Str -> Try(I64, [Bad])
        \\parse = |s| if s == "" Err(Bad) else Ok(1)
        \\
        \\run = |_x| {
        \\    y = parse(42)?
        \\    Ok(y.to_str())
        \\}
    );
    defer test_env.deinit();

    try test_env.assertOneTypeError("Type Mismatch");
}

test "a name unwrapped by `?` from a valid value keeps its type" {
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
