//! Tests for if statements without else in statement position
const std = @import("std");
const CIR = @import("../CIR.zig");

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
