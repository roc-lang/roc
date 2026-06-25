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

test "local self-recursion is allowed" {
    const counts = try scopingDiagnosticCounts(
        \\|n| {
        \\    fac = |x| if (x <= 1) 1 else x * fac(x - 1)
        \\    fac(n)
        \\}
    );
    try testing.expectEqual(@as(usize, 0), counts.forward_ref);
    try testing.expectEqual(@as(usize, 0), counts.mutual);
}

test "backward reference to an earlier local def is allowed" {
    const counts = try scopingDiagnosticCounts(
        \\|_| {
        \\    f = |x| x + 1
        \\    g = |x| f(x)
        \\    g(1)
        \\}
    );
    try testing.expectEqual(@as(usize, 0), counts.forward_ref);
    try testing.expectEqual(@as(usize, 0), counts.mutual);
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

test "forward reference to a later local def is use-before-definition" {
    const counts = try scopingDiagnosticCounts(
        \\|_| {
        \\    g = |x| f(x)
        \\    f = |x| x + 1
        \\    g(1)
        \\}
    );
    try testing.expectEqual(@as(usize, 1), counts.forward_ref);
    try testing.expectEqual(@as(usize, 0), counts.mutual);
}

test "mutual recursion between local defs is reported" {
    const counts = try scopingDiagnosticCounts(
        \\|_| {
        \\    is_even = |n| if (n == 0) True else is_odd(n - 1)
        \\    is_odd = |n| if (n == 0) False else is_even(n - 1)
        \\    is_even(4)
        \\}
    );
    try testing.expectEqual(@as(usize, 0), counts.forward_ref);
    try testing.expectEqual(@as(usize, 1), counts.mutual);
}
