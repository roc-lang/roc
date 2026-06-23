//! Regression test for issue #9705.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "issue 9705: Bool shadowing warns but boolean operators still use builtin Bool" {
    const src =
        \\Bool : U8
        \\
        \\main! = |_args| {
        \\    a = True
        \\    b = False
        \\    _ = a or b
        \\    Ok({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
    try std.testing.expect(diagnostics[0] == .shadowing_warning);
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.problems.problems.items.len);
}

test "issue 9705: nested Bool shadowing warns but succeeds" {
    const src =
        \\main! = |_args| {
        \\    Bool : U8
        \\    _ = True
        \\    Ok({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
    try std.testing.expect(diagnostics[0] == .shadowing_warning);
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.problems.problems.items.len);
}
