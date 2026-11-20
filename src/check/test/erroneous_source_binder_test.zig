//! Regression tests for names bound by matching an erroneous value. Such a
//! pattern binds nothing, so every name it introduces is erroneous, and each
//! use of such a name is erroneous at the use: a call-like consumer is retired
//! before it introduces a dispatch relation, so the one mistake that made the
//! value erroneous is the only report.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

fn expectOnlyCanError(source: []const u8, title: []const u8) TestEnv.TestEnvError!void {
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

test "error recovery: direct rejected try payload" {
    try expectOnlyCanError("size = || Ok(unknown?.len())", "Name Not In Scope");
}

test "error recovery: no branch supplies a receiver" {
    try expectOnlyCanError(
        \\size = |b| {
        \\    x = unknown
        \\    v = if b x else x
        \\    v.len()
        \\}
    , "Name Not In Scope");
    try expectOnlyCanError(
        \\size = |b| {
        \\    x = unknown
        \\    v = match b {
        \\        Bool.True => x
        \\        Bool.False => x
        \\    }
        \\    v.len()
        \\}
    , "Name Not In Scope");
}

test "error recovery: late rejected try payload has no receiver" {
    var env = try TestEnv.init("Test",
        \\size = || {
        \\    s = "abc"
        \\    Ok(s?.len())
        \\}
    );
    defer env.deinit();
    try env.assertTypeErrorTitles(&.{"Type Mismatch"});
}

test "error recovery: a valid sibling or annotation still supplies a receiver" {
    const sources = .{
        "run = |b| (if b unknown else [1.U64]).no_such()",
        "run = |b| {\n v : U64\n v = if b unknown else unknown\n v.no_such()\n}",
    };
    inline for (sources) |source| {
        var env = try TestEnv.init("Test", source);
        defer env.deinit();
        try env.assertTypeErrorTitles(&.{"Missing Method"});
    }
}

test "error recovery: short circuit warnings respect the evaluated operand" {
    const cases = .{
        .{ "if unknown and Bool.True 1 else 0", false },
        .{ "if unknown or Bool.False 1 else 0", false },
        .{ "if Bool.True and unknown 1 else 0", false },
        .{ "if Bool.False or unknown 1 else 0", false },
        .{ "if Bool.False and unknown 1 else 0", true },
        .{ "if Bool.True or unknown 1 else 0", true },
        .{ "{ b = unknown and Bool.True\n if b 1 else 0 }", false },
    };
    inline for (cases) |case| {
        var env = try TestEnv.init("Test", "choose = || " ++ case[0]);
        defer env.deinit();
        try env.assertOneCanError("Name Not In Scope");
        errdefer std.debug.print("short-circuit case: {s}\n", .{case[0]});
        try std.testing.expectEqual(@as(usize, @intFromBool(case[1])), env.checker.problems.problems.items.len);
        if (case[1]) try std.testing.expect(env.checker.problems.problems.items[0] == .comptime_condition);
    }
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
