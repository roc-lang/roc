//! Regression tests for issue #10853.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "issue 10853: a declaration in a duplicate associated owner cannot satisfy a forward lookup" {
    const src =
        \\T := [].{
        \\    A : T.A
        \\}
        \\T := [].{
        \\    A : A
        \\}
    ;

    var test_env = try TestEnv.init("T", src);
    defer test_env.deinit();

    try test_env.assertCanErrors(&.{ "Missing Nested Type", "Type Redeclared" });
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.problems.problems.items.len);
    try std.testing.expectEqual(@as(u32, 0), test_env.module_env.forward_type_decls.span.len);
}

test "issue 10853: a duplicate declaration cannot satisfy its earlier alias lookup" {
    const src =
        \\T := [].{
        \\    A : T.A
        \\    A : U8
        \\}
    ;

    var test_env = try TestEnv.init("T", src);
    defer test_env.deinit();

    try test_env.assertCanErrors(&.{ "Missing Nested Type", "Type Redeclared" });
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.problems.problems.items.len);
    try std.testing.expectEqual(@as(u32, 0), test_env.module_env.forward_type_decls.span.len);
}

test "issue 10853: the selected associated owner still supports a valid forward lookup" {
    const src =
        \\T := [].{
        \\    A : T.B
        \\    B : U8
        \\}
    ;

    var test_env = try TestEnv.init("T", src);
    defer test_env.deinit();

    try test_env.assertCanErrors(&.{});
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.problems.problems.items.len);
    try std.testing.expectEqual(@as(u32, 1), test_env.module_env.forward_type_decls.span.len);
}
