//! Regression tests for https://github.com/roc-lang/roc/issues/12318.
//!
//! A type defaulted from a string interpolation that then fails a requirement
//! is reported at the interpolation, like a type defaulted from any other
//! literal is reported at that literal.
const TestEnv = @import("TestEnv.zig");

test "issue 12318: undetermined type from an interpolation beside a quote reports at the interpolation" {
    const source =
        \\x = "a ${"b"}"
        \\y = x < x
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertOneTypeErrorHighlightsWithin("Missing Method", .{ .line = 1, .start_column = 5, .end_column = 15 });
}

test "issue 12318: undetermined type from an interpolation in a body reports at the interpolation" {
    const source =
        \\f = |n| {
        \\    x = "a ${n}"
        \\    x < x
        \\}
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertOneTypeErrorHighlightsWithin("Missing Method", .{ .line = 2, .start_column = 9, .end_column = 17 });
}
