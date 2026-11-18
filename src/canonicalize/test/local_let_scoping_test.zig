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
        if (diag == .local_reference_before_definition) counts.forward_ref += 1;
        if (diag == .mutually_recursive_local_definitions) counts.mutual += 1;
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

test "issue 13165: a match branch cannot read its enclosing unfinished binding" {
    // https://github.com/roc-lang/roc/issues/13165
    var test_env = try TestEnv.init(
        \\|take_early| {
        \\    (val, _state) = match take_early {
        \\        True => return val
        \\        False => (123, Unchanged)
        \\    }
        \\    val
        \\}
    );
    defer test_env.deinit();
    _ = try test_env.canonicalizeExpr();

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);
    var self_references: usize = 0;
    for (diagnostics) |diag| {
        if (diag == .self_referential_definition) self_references += 1;
    }
    try testing.expectEqual(@as(usize, 1), self_references);
}

test "singleton initializer contexts need no membership index" {
    var env = try TestEnv.init(
        \\|input| {
        \\    first = input + 1
        \\    second = first + 1
        \\    second
        \\}
    );
    defer env.deinit();
    _ = try env.canonicalizeExpr();
    const diagnostics = try env.getDiagnostics();
    defer testing.allocator.free(diagnostics);
    try testing.expectEqual(@as(usize, 0), diagnostics.len);
    try testing.expectEqual(@as(u32, 0), env.can.defining_bound_positions.capacity());
}

test "unfinished bindings survive eager blocks guards loops and nested initializers" {
    const sources = [_][]const u8{
        \\|take_early| {
        \\    (val, _state) = if take_early { return val } else { (123, Unchanged) }
        \\    val
        \\}
        ,
        \\|input| {
        \\    val = match input {
        \\        n if val == n => 1
        \\        _ => 2
        \\    }
        \\    val
        \\}
        ,
        \\|input| {
        \\    val = {
        \\        inner = val
        \\        inner + input
        \\    }
        \\    val
        \\}
        ,
        \\|input| {
        \\    val = {
        \\        for item in [input] {
        \\            return val + item
        \\        }
        \\        0
        \\    }
        \\    val
        \\}
        ,
        \\|input| {
        \\    val = {
        \\        while input {
        \\            return val
        \\        }
        \\        0
        \\    }
        \\    val
        \\}
        ,
    };
    for (sources) |source| {
        var env = try TestEnv.init(source);
        defer env.deinit();
        _ = try env.canonicalizeExpr();
        const diagnostics = try env.getDiagnostics();
        defer testing.allocator.free(diagnostics);
        var count: usize = 0;
        for (diagnostics) |diagnostic| {
            if (diagnostic == .self_referential_definition) count += 1;
        }
        try testing.expectEqual(@as(usize, 1), count);
        try testing.expectEqual(@as(u32, 0), env.can.defining_bound_positions.count());
    }
}

test "unfinished binding contexts restore after lambdas without rejecting old var values" {
    const sources = [_][]const u8{
        \\|input| {
        \\    var $val = input
        \\    ($val, other) = ($val + 1, 2)
        \\    $val + other
        \\}
        ,
        \\|input| {
        \\    recurse = |n| if n == 0 0 else recurse(n - 1)
        \\    recurse(input)
        \\}
        ,
        \\|input| {
        \\    val = {
        \\        read_later = || val
        \\        _unused = read_later
        \\        input
        \\    }
        \\    val
        \\}
        ,
    };
    for (sources) |source| {
        var env = try TestEnv.init(source);
        defer env.deinit();
        _ = try env.canonicalizeExpr();
        const diagnostics = try env.getDiagnostics();
        defer testing.allocator.free(diagnostics);
        for (diagnostics) |diagnostic| {
            std.debug.print("Unexpected {s} in:\n{s}\n", .{ @tagName(diagnostic), source });
        }
        try testing.expectEqual(@as(usize, 0), diagnostics.len);
        try testing.expectEqual(@as(u32, 0), env.can.defining_bound_positions.count());
    }

    var env = try TestEnv.init(
        \\|input| {
        \\    val = {
        \\        read_later = || val
        \\        _unused = read_later
        \\        val + input
        \\    }
        \\    val
        \\}
    );
    defer env.deinit();
    _ = try env.canonicalizeExpr();
    const diagnostics = try env.getDiagnostics();
    defer testing.allocator.free(diagnostics);
    var count: usize = 0;
    for (diagnostics) |diagnostic| {
        if (diagnostic == .self_referential_definition) count += 1;
    }
    try testing.expectEqual(@as(usize, 1), count);
}
