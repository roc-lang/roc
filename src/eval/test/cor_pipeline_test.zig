//! Focused eval tests for the cor-style lowering pipeline.

const std = @import("std");
const helpers = @import("helpers.zig");

const testing = std.testing;

fn expectInspect(comptime source: []const u8, expected: []const u8) !void {
    var compiled = try helpers.compileInspectedExpr(testing.allocator, source);
    defer compiled.deinit(testing.allocator);
    const actual = try helpers.lirInterpreterInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);
    try testing.expectEqualStrings(expected, actual);
}

test "cor pipeline - recursive lambda factorial" {
    try expectInspect(
        \\{
        \\    factorial = |n| if (n <= 1.I64) 1.I64 else n * factorial(n - 1.I64)
        \\    factorial(5.I64)
        \\}
        ,
        "120",
    );
}

test "cor pipeline - mutual recursion in local lambdas" {
    try expectInspect(
        \\{
        \\    is_even = |n| if (n == 0.I64) True else is_odd(n - 1.I64)
        \\    is_odd = |n| if (n == 0.I64) False else is_even(n - 1.I64)
        \\    is_even(6.I64)
        \\}
        ,
        "True",
    );
}

test "cor pipeline - for loop sums list" {
    try expectInspect(
        \\{
        \\    var $sum = 0.I64
        \\    for item in [10.I64, 20.I64, 30.I64] {
        \\        $sum = $sum + item
        \\    }
        \\    $sum
        \\}
        ,
        "60",
    );
}

test "cor pipeline - for loop in lambda body" {
    try expectInspect(
        \\{
        \\    sum = |xs| {
        \\        var $sum = 0.I64
        \\        for item in xs {
        \\            $sum = $sum + item
        \\        }
        \\        $sum
        \\    }
        \\    sum([1.I64, 2.I64, 3.I64, 4.I64])
        \\}
        ,
        "10",
    );
}

test "cor pipeline - recursive lambda with record" {
    try expectInspect(
        \\{
        \\    f = |n|
        \\        if n <= 0.I64
        \\            0.I64
        \\        else
        \\            { a: n, b: n * 2.I64, c: n * 3.I64, d: n * 4.I64 }.a + f(n - 1.I64)
        \\    f(100.I64)
        \\}
        ,
        "5050",
    );
}

test "cor pipeline - for loop early return" {
    try expectInspect(
        \\{
        \\    f = |list| {
        \\        for _item in list {
        \\            if True { return True }
        \\        }
        \\        False
        \\    }
        \\    f([1.I64, 2.I64, 3.I64])
        \\}
        ,
        "True",
    );
}

test "cor pipeline - for loop closure early return" {
    try expectInspect(
        \\{
        \\    f = |list, pred| {
        \\        for item in list {
        \\            if pred(item) { return True }
        \\        }
        \\        False
        \\    }
        \\    f([1.I64, 2.I64, 3.I64], |_x| True)
        \\}
        ,
        "True",
    );
}
