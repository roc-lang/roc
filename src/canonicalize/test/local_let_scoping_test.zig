//! Tests for sequential scoping of local `let` definitions inside a block.
//!
//! Local definitions are evaluated in order: a definition is in scope only
//! after itself (self-reference) and after earlier definitions (backward
//! reference). Forward references and mutual recursion between local
//! definitions are NOT allowed and are reported with dedicated diagnostics.

const std = @import("std");
const TestEnv = @import("TestEnv.zig").TestEnv;

const testing = std.testing;

const ScopingTestError = std.mem.Allocator.Error || error{
    TestExpectedEqual,
};

const Counts = struct {
    forward_ref: usize = 0,
    mutual: usize = 0,
};

fn scopingDiagnosticCounts(source: []const u8) ScopingTestError!Counts {
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    _ = try test_env.canonicalizeExpr();

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var counts = Counts{};
    for (diagnostics) |diag| {
        switch (diag) {
            .local_reference_before_definition => counts.forward_ref += 1,
            .mutually_recursive_local_definitions => counts.mutual += 1,
            else => {},
        }
    }
    return counts;
}

test "nested self-recursion is allowed" {
    const counts = try scopingDiagnosticCounts(
        \\|_| {
        \\    outer = |o| {
        \\        inner = |x| if (x <= 1) 1 else inner(x - 1)
        \\        inner(o)
        \\    }
        \\    outer(5)
        \\}
    );
    try testing.expectEqual(@as(usize, 0), counts.forward_ref);
    try testing.expectEqual(@as(usize, 0), counts.mutual);
}
