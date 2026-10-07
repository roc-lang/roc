//! Regression tests for issue 12014.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/12014
//
// An expression statement whose expression is already erroneous (its checked
// type contains an error, such as a tag wrapping an undefined name) has no
// value to discard. The statement introduces no `{}` relation for it, so the
// original error is the only report, and the statement itself becomes an
// explicit runtime error.

test "issue 12014: an erroneous expression statement reports only the undefined name" {
    const source =
        \\parse_name = |text| {
        \\    Err(InvalidName(txt))
        \\    Ok(text)
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneCanError("Name Not In Scope");
    try std.testing.expectEqual(@as(usize, 0), try test_env.typeProblemCount());
    try std.testing.expect(try test_env.lambdaBodyStatement("parse_name", 0) == .s_runtime_error);
}

test "issue 12014: an erroneous record expression statement reports only the undefined name" {
    const source =
        \\f = |_| {
        \\    { a: txt }
        \\    {}
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneCanError("Name Not In Scope");
    try std.testing.expectEqual(@as(usize, 0), try test_env.typeProblemCount());
    try std.testing.expect(try test_env.lambdaBodyStatement("f", 0) == .s_runtime_error);
}

test "issue 12014: an error-free unused value statement still reports" {
    const source =
        \\f = |_| {
        \\    InvalidName(1)
        \\    {}
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}
