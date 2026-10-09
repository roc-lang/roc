//! Tests for if statements without else in statement position
const std = @import("std");

const testing = std.testing;

const TestEnv = @import("TestEnv.zig").TestEnv;

test "if without else in lambda block statement position should succeed" {
    // This matches the pattern in Builtin.roc's List.is_eq:
    // is_eq = |self, other| {
    //     if self.len() != other.len() {
    //         return False
    //     }
    //     True
    // }
    // The if without else is in statement position - its value is not used.
    //
    // The bug: When canonicalizing a module-level declaration's RHS, in_statement_position
    // is set to false. The lambda body block should reset in_statement_position to true
    // for its statements, but it doesn't. This causes the `if` statement to be incorrectly
    // flagged as needing an `else` branch.
    //
    // To simulate this, we set in_statement_position = false before canonicalizing,
    // which is what happens when canonicalizing the RHS of a module-level declaration.
    const source =
        \\|x, y| {
        \\    if x != y {
        \\        return False
        \\    }
        \\    True
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    // Simulate the condition when canonicalizing a declaration's RHS:
    // in_statement_position is set to false before entering the lambda
    test_env.can.in_statement_position = false;

    const result = try test_env.canonicalizeExpr();

    // Canonicalization should succeed
    try testing.expect(result != null);

    // There should be no diagnostics (specifically no if_expr_without_else)
    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);
    try testing.expectEqual(@as(usize, 0), diagnostics.len);
}

test "issue 12106: warn only on returns in function result positions" {
    const cases = [_]struct { source: []const u8, count: usize }{
        .{ .source = "|x| { return x }", .count = 1 },
        .{ .source = "|x| return x", .count = 1 },
        .{ .source = "|x| (return x)", .count = 1 },
        .{ .source = "|x| (return x, 2)", .count = 0 },
        .{ .source = "|x| match x { y if y < 0 => { return 1 }, _ => { return 2 } }", .count = 2 },
        .{ .source = "|x| { if x { return 1 }\n return 2 }", .count = 1 },
        .{ .source = "|x| if x { return 1 } else { return 2 }", .count = 2 },
        .{ .source = "|x| match x { True => return 1, False => { return 2 } }", .count = 2 },
        .{ .source = "|x| { return if x { return 1 } else { return 2 } }", .count = 3 },
        .{ .source = "|x| { inner = |y| { return y }\n return inner(x) }", .count = 2 },
        .{ .source = "|x| { y = { return x }\n y }", .count = 0 },
        .{ .source = "|x| [{ return x }]", .count = 0 },
        .{ .source = "|x| { for y in x { return y }\n 0 }", .count = 0 },
        .{ .source = "|x| { while x { return 1 }\n 0 }", .count = 0 },
        .{ .source = "|x| { if x { return 1 } else { return 2 }\n 3 }", .count = 0 },
        .{ .source = "|x| if x { return 1 }", .count = 0 },
    };
    for (cases) |case| {
        var test_env = try TestEnv.init(case.source);
        defer test_env.deinit();
        testing.expect((try test_env.canonicalizeExpr()) != null) catch |err| {
            std.debug.print("Source did not parse: {s}\n", .{case.source});
            return err;
        };
        const diagnostics = try test_env.getDiagnostics();
        defer testing.allocator.free(diagnostics);
        var count: usize = 0;
        for (diagnostics) |diagnostic| {
            if (diagnostic == .redundant_return) {
                count += 1;
                const region = diagnostic.redundant_return.region;
                try testing.expect(std.mem.startsWith(u8, case.source[region.start.offset..region.end.offset], "return"));
                var report = try test_env.module_env.diagnosticToReport(diagnostic, testing.allocator, "test.roc");
                defer report.deinit();
                try testing.expectEqual(.warning, report.severity);
            }
        }
        try testing.expectEqual(case.count, count);
    }
}
