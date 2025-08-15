//! Tests for verifying correct lambda vs closure canonicalization

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

test "top-level function canonicalizes successfully" {
    const source =
        \\helper = |x| x + 1
        \\
        \\main = |y| helper(y)
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    // Just check that canonicalization succeeds - this will help us debug
    const result = try test_env.canonicalizeExpr();
    std.testing.expect(result != null) catch |err| {
        if (test_env.hasParseErrors()) {
            std.debug.print("Parse errors found in test\n", .{});
        }
        return err;
    };
}
