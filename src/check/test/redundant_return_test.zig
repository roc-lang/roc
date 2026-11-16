//! A `return` in its function's result position is the same program as its
//! operand: canonicalization keeps the warning and produces no return node, so
//! checking sees identical types, while early returns and returns the parser did
//! not record as redundant keep their meaning and diagnostics.
const std = @import("std");

const TestEnv = @import("./TestEnv.zig");

const testing = std.testing;

/// Count return nodes, expression and statement forms, in the module's CIR.
fn returnNodeCount(test_env: *const TestEnv) usize {
    var count: usize = 0;
    for (test_env.module_env.store.nodes.field(.tag)) |tag| {
        if (tag == .expr_return or tag == .statement_return) count += 1;
    }
    return count;
}

fn countRedundantReturnWarnings(test_env: *TestEnv) std.mem.Allocator.Error!usize {
    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);
    var count: usize = 0;
    for (diagnostics) |diagnostic| {
        if (diagnostic == .redundant_return) count += 1;
    }
    return count;
}

const Pair = struct {
    with_return: []const u8,
    without_return: []const u8,
    redundant: usize,
    early: usize,
};

const pairs = [_]Pair{
    .{
        .with_return = "f = |x| { y = x + 1\n    return y\n}",
        .without_return = "f = |x| { y = x + 1\n    y\n}",
        .redundant = 1,
        .early = 0,
    },
    .{
        .with_return = "f = |x| return x",
        .without_return = "f = |x| x",
        .redundant = 1,
        .early = 0,
    },
    .{
        .with_return = "f : I64 -> Str\nf = |x| if x > 0 { return \"pos\" } else { return \"neg\" }",
        .without_return = "f : I64 -> Str\nf = |x| if x > 0 { \"pos\" } else { \"neg\" }",
        .redundant = 2,
        .early = 0,
    },
    .{
        .with_return = "f = |x| match x { A => return 1, B => { return 2 } }",
        .without_return = "f = |x| match x { A => 1, B => { 2 } }",
        .redundant = 2,
        .early = 0,
    },
    .{
        .with_return = "f = |x| { if x > 0 { return 1 }\n    return 2\n}",
        .without_return = "f = |x| { if x > 0 { return 1 }\n    2\n}",
        .redundant = 1,
        .early = 1,
    },
    .{
        .with_return = "f = |x| { g = |y| { return y + 1 }\n    return g(x)\n}",
        .without_return = "f = |x| { g = |y| { y + 1 }\n    g(x)\n}",
        .redundant = 2,
        .early = 0,
    },
};

test "a redundant return checks to the same type as its operand" {
    for (pairs) |pair| {
        var with_env = try TestEnv.init("Test", pair.with_return);
        defer with_env.deinit();
        var without_env = try TestEnv.init("Test", pair.without_return);
        defer without_env.deinit();

        try testing.expectEqual(pair.redundant, try countRedundantReturnWarnings(&with_env));
        try testing.expectEqual(@as(usize, 0), try countRedundantReturnWarnings(&without_env));
        try testing.expectEqual(@as(usize, 0), try with_env.typeProblemCount());
        try testing.expectEqual(@as(usize, 0), try without_env.typeProblemCount());

        const with_type = try with_env.allocDefType(testing.allocator, "f");
        defer testing.allocator.free(with_type);
        const without_type = try without_env.allocDefType(testing.allocator, "f");
        defer testing.allocator.free(without_type);
        try testing.expectEqualStrings(without_type, with_type);

        // Only early returns remain return nodes.
        try testing.expectEqual(pair.early, returnNodeCount(&with_env));
        try testing.expectEqual(pair.early, returnNodeCount(&without_env));
    }
}

test "a redundant return with a mismatched annotation reports the same number of problems" {
    var with_env = try TestEnv.init("Test", "f : I64 -> Str\nf = |x| return x");
    defer with_env.deinit();
    var without_env = try TestEnv.init("Test", "f : I64 -> Str\nf = |x| x");
    defer without_env.deinit();
    try testing.expectEqual(@as(usize, 1), try without_env.typeProblemCount());
    try testing.expectEqual(try without_env.typeProblemCount(), try with_env.typeProblemCount());
}

test "an early return stays a return node" {
    var test_env = try TestEnv.init("Test", "f = |x| {\n    if x > 0 {\n        return 1\n    }\n    2\n}");
    defer test_env.deinit();
    try testing.expectEqual(@as(usize, 0), try countRedundantReturnWarnings(&test_env));
    try testing.expectEqual(@as(usize, 1), returnNodeCount(&test_env));
    try testing.expectEqual(@as(usize, 0), try test_env.typeProblemCount());
}

test "a return directly inside expect keeps its diagnostic" {
    var test_env = try TestEnv.init("Test", "expect {\n    return 1\n}");
    defer test_env.deinit();
    try test_env.assertOneCanError("Return In Expect");
    try testing.expectEqual(@as(usize, 0), returnNodeCount(&test_env));
}

test "a function inside expect still drops its redundant return" {
    var test_env = try TestEnv.init("Test", "expect {\n    g = |y| { return y + 1 }\n    g(1) == 2\n}");
    defer test_env.deinit();
    try testing.expectEqual(@as(usize, 1), try countRedundantReturnWarnings(&test_env));
    try testing.expectEqual(@as(usize, 0), returnNodeCount(&test_env));
}

test "a return outside any function keeps its diagnostic" {
    var test_env = try TestEnv.init("Test", "x = { return 1 }");
    defer test_env.deinit();
    try test_env.assertOneCanError("Return Outside Function");
    try testing.expectEqual(@as(usize, 0), returnNodeCount(&test_env));
}
