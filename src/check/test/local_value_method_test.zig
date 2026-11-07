//! Tests for the rule that a dispatch never selects an associated binding of
//! a type declared in a function body that is a value of that body rather
//! than a local procedure (design.md "Methods Never Capture"). A top-level
//! type's method may be any function value: its compile-time root evaluates
//! it, and dispatches call that value.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

const local_value_title = "Method Is a Local Value";

fn expectLocalValueMethod(source: []const u8, method_name: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertTypeErrorTitles(&.{local_value_title});
    const idents = test_env.module_env.getIdentStoreConst();
    const problem = test_env.checker.problems.problems.items[0];
    try std.testing.expectEqualStrings(method_name, idents.getText(problem.static_dispatch.local_value_method.method_name));
}

fn expectNoErrors(source: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "calling a method computed in the enclosing function body is reported" {
    try expectLocalValueMethod(
        \\make : () -> ({ count : U64 } -> U64)
        \\make = || |counter| counter.count + 1
        \\
        \\run = |n| {
        \\    Counter := { count : U64 }.{
        \\        value = make()
        \\    }
        \\    Counter.{ count: n }.value()
        \\}
    , "value");
}

test "equality through a method computed in the enclosing function body is reported" {
    try expectLocalValueMethod(
        \\same : () -> ({ count : U64 }, { count : U64 } -> Bool)
        \\same = || |_a, _b| Bool.False
        \\
        \\run = |n| {
        \\    Counter := { count : U64 }.{
        \\        is_eq = same()
        \\    }
        \\    Counter.{ count: n } == Counter.{ count: n }
        \\}
    , "is_eq");
}

test "a method bound to a local function computed from the enclosing body is a local procedure" {
    try expectNoErrors(
        \\run = |n| {
        \\    bump = |counter| counter.count + 1
        \\    Counter := { count : U64 }.{
        \\        value = bump
        \\    }
        \\    Counter.{ count: n }.value()
        \\}
    );
}

test "a top-level type's method may be a computed callable" {
    try expectNoErrors(
        \\make : () -> (Counter -> U64)
        \\make = || |counter| counter.count + 1
        \\
        \\Counter := { count : U64 }.{
        \\    value = make()
        \\}
        \\
        \\run = |n| Counter.{ count: n }.value()
    );
}
