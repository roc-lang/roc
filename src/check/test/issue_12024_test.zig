//! Regression tests for issue 12024.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/12024
//
// An uninitialized `var` whose annotation names an undeclared type has no type
// to give its binder, and no initializer to infer one from. The undeclared
// type is the only report; the declaration binds nothing and becomes an
// explicit runtime error, and the binder never adopts the erroneous type.

test "issue 12024: uninitialized var with an undeclared annotation type" {
    const source =
        \\f = |_| {
        \\    $count : UnknownType
        \\    var $count
        \\    $count = 1
        \\    {}
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertCanErrors(&.{ "Undeclared Type", "Unused Variable" });
    try std.testing.expectEqual(@as(usize, 0), try test_env.typeProblemCount());
    try test_env.assertDefTypeOptions("f", "_arg -> {}", .{ .allow_type_errors = true, .allow_can_errors = true });
    try std.testing.expect(try test_env.lambdaBodyStatement("f", 0) == .s_runtime_error);
}

test "issue 12024: uninitialized var with an undeclared type nested in its annotation" {
    const source =
        \\f = |_| {
        \\    $items : List(UnknownType)
        \\    var $items
        \\    $items = []
        \\    $items
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneCanError("Undeclared Type");
    try std.testing.expectEqual(@as(usize, 0), try test_env.typeProblemCount());
    // The body's value is a use of a name that binds nothing, so the use is
    // erroneous and so is the function value it produces.
    try std.testing.expect(test_env.module_env.store.getExpr(try test_env.defExpr("f")) == .e_runtime_error);
}
