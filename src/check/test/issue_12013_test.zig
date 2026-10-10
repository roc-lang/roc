//! Regression tests for issue 12013.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/12013
//
// An `if` condition, a `while` condition, a match guard, and an `expect` body
// each demand a `Bool` from a value they only consult. A rejected demand
// reports at the operand and retires it (or, for a guard, its match) without
// poisoning the operand's solved class, which can be shared with its producer:
// a call's result is its callee's return slot.

test "issue 12013: a tag used as an if condition reports a mismatch" {
    const source =
        \\f = |_| if Verbose "details" else "none"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");

    const store = &test_env.module_env.store;
    const lambda = store.getExpr(try test_env.defExpr("f"));
    try std.testing.expect(lambda == .e_lambda);
    const if_expr = store.getExpr(lambda.e_lambda.body);
    try std.testing.expect(if_expr == .e_if);
    const branch = store.getIfBranch(store.sliceIfBranches(if_expr.e_if.branches)[0]);
    try std.testing.expect(store.getExpr(branch.cond) == .e_runtime_error);
}

test "issue 12013: a rejected if condition leaves its callee's type intact" {
    const source =
        \\describe : U64 -> [Verbose(U64)]
        \\describe = |n| Verbose(n)
        \\
        \\f = |_| if describe(1) "details" else "none"
        \\
        \\g = |n| describe(n)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
    try test_env.assertDefTypeOptions("describe", "U64 -> [Verbose(U64)]", .{ .allow_type_errors = true });
    try test_env.assertDefTypeOptions("g", "U64 -> [Verbose(U64)]", .{ .allow_type_errors = true });
}

test "issue 12013: a rejected match guard leaves its callee's type intact" {
    const source =
        \\describe : U64 -> [Verbose(U64)]
        \\describe = |n| Verbose(n)
        \\
        \\f = |x| match x {
        \\    _ if describe(1) => "details"
        \\    _ => "none"
        \\}
        \\
        \\g = |n| describe(n)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
    try test_env.assertDefTypeOptions("g", "U64 -> [Verbose(U64)]", .{ .allow_type_errors = true });
}

test "issue 12013: a tag used as a while condition reports a mismatch" {
    const source =
        \\f = |_| {
        \\    var $i = 0
        \\    while Verbose {
        \\        $i = $i + 1
        \\    }
        \\    $i
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

test "issue 12013: a rejected expect body leaves its callee's type intact" {
    const source =
        \\describe : U64 -> [Verbose(U64)]
        \\describe = |n| Verbose(n)
        \\
        \\expect describe(1)
        \\
        \\g = |n| describe(n)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
    try test_env.assertDefTypeOptions("g", "U64 -> [Verbose(U64)]", .{ .allow_type_errors = true });
}
