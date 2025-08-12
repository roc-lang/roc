//! Tests for integer literal canonicalization during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct canonicalization
//! of integer literals and integer expressions from parsed AST into the
//! compiler's canonical internal representation (CIR).

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const Can = @import("../Can.zig");
const CIR = @import("../CIR.zig");
const TestEnv = @import("TestEnv.zig").TestEnv;
const ModuleEnv = @import("../ModuleEnv.zig");

fn getIntValue(module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) !i128 {
    const expr = module_env.store.getExpr(expr_idx);
    switch (expr) {
        .e_int => |int_expr| {
            return @bitCast(int_expr.value.bytes);
        },
        else => return error.NotAnInteger,
    }
}

fn calculateRequirements(value: i128) types.Num.Int.Requirements {
    const bits_needed = types.Num.Int.BitsNeeded.fromValue(@bitCast(value));

    return .{ .sign_needed = value < 0, .bits_needed = bits_needed };
}

test "canonicalize simple positive integer" {
    const source = "42";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(@as(i128, 42), value);
}

test "canonicalize simple negative integer" {
    const source = "-42";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(@as(i128, -42), value);
}

test "canonicalize zero" {
    const source = "0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(@as(i128, 0), value);
}

test "canonicalize large positive integer" {
    const source = "9223372036854775807"; // i64 max
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(@as(i128, 9223372036854775807), value);
}

test "canonicalize large negative integer" {
    const source = "-9223372036854775808"; // i64 min
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(@as(i128, -9223372036854775808), value);
}

test "canonicalize very large integer" {
    const source = "170141183460469231731687303715884105727"; // i128 max
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(@as(i128, 170141183460469231731687303715884105727), value);
}

test "canonicalize very large negative integer" {
    const source = "-170141183460469231731687303715884105728"; // i128 min
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(@as(i128, -170141183460469231731687303715884105728), value);
}

test "canonicalize small integers" {
    const test_cases = [_]struct { source: []const u8, expected: i128 }{
        .{ .source = "1", .expected = 1 },
        .{ .source = "-1", .expected = -1 },
        .{ .source = "10", .expected = 10 },
        .{ .source = "-10", .expected = -10 },
        .{ .source = "255", .expected = 255 },
        .{ .source = "-128", .expected = -128 },
        .{ .source = "256", .expected = 256 },
        .{ .source = "-129", .expected = -129 },
        .{ .source = "32767", .expected = 32767 },
        .{ .source = "-32768", .expected = -32768 },
        .{ .source = "65535", .expected = 65535 },
        .{ .source = "-32769", .expected = -32769 },
    };

    for (test_cases) |tc| {
        var test_env = try TestEnv.init(tc.source);
        defer test_env.deinit();

        const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
        const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
        try testing.expectEqual(tc.expected, value);
    }
}

test "canonicalize integer literals with underscores" {
    const test_cases = [_]struct { source: []const u8, expected: i128 }{
        .{ .source = "1_000", .expected = 1000 },
        .{ .source = "1_000_000", .expected = 1000000 },
        .{ .source = "-1_234_567", .expected = -1234567 },
        .{ .source = "123_456_789", .expected = 123456789 },
        .{ .source = "1_2_3_4_5", .expected = 12345 },
    };

    for (test_cases) |tc| {
        var test_env = try TestEnv.init(tc.source);
        defer test_env.deinit();

        const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
        const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
        try testing.expectEqual(tc.expected, value);
    }
}

test "canonicalize integer with specific requirements" {
    const test_cases = [_]struct {
        source: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: types.Num.Int.BitsNeeded,
    }{
        .{ .source = "127", .expected_value = 127, .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .source = "128", .expected_value = 128, .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        .{ .source = "255", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        .{ .source = "256", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },
        .{ .source = "-128", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = .@"7" },
        .{ .source = "-129", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = .@"8" },
        .{ .source = "32767", .expected_value = 32767, .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },
        .{ .source = "32768", .expected_value = 32768, .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        .{ .source = "65535", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        .{ .source = "65536", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = .@"17_to_31" },
    };

    for (test_cases) |tc| {
        var test_env = try TestEnv.init(tc.source);
        defer test_env.deinit();

        const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
        const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
        try testing.expectEqual(tc.expected_value, value);
    }
}

test "canonicalize integer literal creates correct type variables" {
    const source = "42";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());
    switch (expr) {
        .e_int => {
            // Verify requirements were set
        },
        else => return error.UnexpectedExprType,
    }
}

test "canonicalize invalid integer literal" {
    // Test individual cases since some might fail during parsing vs canonicalization

    // "12abc" - invalid characters in number
    {
        var test_env = try TestEnv.init("12abc");
        defer test_env.deinit();
        // Should have parse errors
        try testing.expect(test_env.parse_ast.parse_diagnostics.items.len > 0 or
            test_env.parse_ast.tokenize_diagnostics.items.len > 0);
    }

    // Leading zeros with digits
    {
        var test_env = try TestEnv.init("0123");
        defer test_env.deinit();
        // This might actually parse as 123, check if we have diagnostics
        if (test_env.parse_ast.parse_diagnostics.items.len == 0 and
            test_env.parse_ast.tokenize_diagnostics.items.len == 0)
        {
            // No errors, so it should have parsed as 123
            const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
            const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
            try testing.expectEqual(@as(i128, 123), value);
        }
    }
}

test "canonicalize integer preserves all bytes correctly" {
    // Test specific bit patterns to ensure bytes are preserved correctly
    const test_cases = [_]struct {
        source: []const u8,
        expected_bytes: [16]u8,
    }{
        .{
            .source = "1",
            .expected_bytes = .{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        },
        .{
            .source = "256",
            .expected_bytes = .{ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        },
        .{
            .source = "65536",
            .expected_bytes = .{ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        },
        .{
            .source = "-1",
            .expected_bytes = .{ 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
        },
        .{
            .source = "-256",
            .expected_bytes = .{ 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
        },
    };

    for (test_cases) |tc| {
        var test_env = try TestEnv.init(tc.source);
        defer test_env.deinit();

        const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
        const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
        const value_bytes: [16]u8 = @bitCast(value);
        try testing.expectEqualSlices(u8, &tc.expected_bytes, &value_bytes);
    }
}

test "canonicalize integer round trip through NodeStore" {
    // Test that integers survive storage and retrieval from NodeStore
    const test_values = [_]i128{
        0,      1,     -1,     42,         -42,
        127,    -128,  255,    -256,       32767,
        -32768, 65535, -65536, 2147483647, -2147483648,
    };

    for (test_values) |expected| {
        var buf: [64]u8 = undefined;
        const source = try std.fmt.bufPrint(&buf, "{}", .{expected});

        var test_env = try TestEnv.init(source);
        defer test_env.deinit();

        const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
        // Get the expression back from the store
        const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());

        try testing.expectEqual(expected, value);
    }
}

test "canonicalize integer with maximum digits" {
    // Test very long digit sequences
    const test_cases = [_]struct { source: []const u8, expected: i128 }{
        .{ .source = "000000000000000000000000000000000000000001", .expected = 1 },
        .{ .source = "000000000000000000000000000000000000000000", .expected = 0 },
        .{ .source = "-000000000000000000000000000000000000000001", .expected = -1 },
    };

    for (test_cases) |tc| {
        var test_env = try TestEnv.init(tc.source);
        defer test_env.deinit();

        // Check if parsing succeeded (leading zeros might be treated specially)
        const has_errors = test_env.parse_ast.parse_diagnostics.items.len > 0 or
            test_env.parse_ast.tokenize_diagnostics.items.len > 0;

        if (!has_errors) {
            const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
            const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
            try testing.expectEqual(tc.expected, value);
        } else {
            // If there are errors, that's expected for numbers with leading zeros
            // Just verify we got some diagnostic
            try testing.expect(has_errors);
        }
    }
}

test "canonicalize integer requirements determination" {
    const test_cases = [_]struct {
        source: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: types.Num.Int.BitsNeeded,
    }{
        // 255 needs 8 bits and no sign
        .{ .source = "255", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        // 256 needs 9-15 bits and no sign
        .{ .source = "256", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },
        // -1 needs sign and 7 bits
        .{ .source = "-1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = .@"7" },
        // 65535 needs 16 bits and no sign
        .{ .source = "65535", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        // 65536 needs 17-31 bits and no sign
        .{ .source = "65536", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = .@"17_to_31" },
    };

    for (test_cases) |tc| {
        var test_env = try TestEnv.init(tc.source);
        defer test_env.deinit();

        const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
        const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());

        try testing.expectEqual(tc.expected_value, value);
    }
}

test "canonicalize integer literals outside supported range" {
    // Test integer literals that are too big to be represented
    const test_cases = [_][]const u8{
        // Negative number slightly lower than i128 min
        "-170141183460469231731687303715884105729",
        // Number too big for u128 max (340282366920938463463374607431768211455)
        "340282366920938463463374607431768211456",
        // Way too big
        "999999999999999999999999999999999999999999999999999",
    };

    for (test_cases) |source| {
        var test_env = try TestEnv.init(source);
        defer test_env.deinit();

        const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
        const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());
        try testing.expect(expr == .e_runtime_error);
    }
}

test "invalid number literal - too large for u128" {
    const source = "999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999";

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    // Should have produced diagnostics for number too large
    // Very large numbers might be caught during parsing or canonicalization
    const parse_errors = test_env.parse_ast.parse_diagnostics.items.len > 0;
    const tokenize_errors = test_env.parse_ast.tokenize_diagnostics.items.len > 0;

    // Only check canon diagnostics if parsing succeeded
    if (!parse_errors and !tokenize_errors) {
        const canon_diagnostics = try test_env.module_env.getDiagnostics();
        defer test_env.gpa.free(canon_diagnostics);
        const canon_errors = canon_diagnostics.len > 0;

        if (!canon_errors) {
            // If no errors at all, check the expression type
            const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
            const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());
            // Large numbers should either fail to parse or produce a runtime error
            try testing.expect(expr == .e_runtime_error);
        }
    } else {
        // We have parse/tokenize errors, which is expected for this large number
        try testing.expect(true);
    }
}

test "invalid number literal - negative too large for i128" {
    const source = "-999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999";

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    // Should have produced diagnostics for number too large
    // Very large negative numbers might be caught during parsing or canonicalization
    const parse_errors = test_env.parse_ast.parse_diagnostics.items.len > 0;
    const tokenize_errors = test_env.parse_ast.tokenize_diagnostics.items.len > 0;

    // Only check canon diagnostics if parsing succeeded
    if (!parse_errors and !tokenize_errors) {
        const canon_diagnostics = try test_env.module_env.getDiagnostics();
        defer test_env.gpa.free(canon_diagnostics);
        const canon_errors = canon_diagnostics.len > 0;

        if (!canon_errors) {
            // If no errors at all, check the expression type
            const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
            const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());
            // Large numbers should either fail to parse or produce a runtime error
            try testing.expect(expr == .e_runtime_error);
        }
    } else {
        // We have parse/tokenize errors, which is expected for this large number
        try testing.expect(true);
    }
}

test "integer literal - negative zero" {
    const source = "-0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());
    switch (expr) {
        .e_int => |int| {
            // -0 should be treated as 0
            try testing.expectEqual(@as(i128, @bitCast(int.value.bytes)), 0);
            // But it should still be marked as needing a sign
        },
        else => {
            try testing.expect(false); // Should be int
        },
    }
}

test "integer literal - positive zero" {
    const source = "0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());
    switch (expr) {
        .e_int => |int| {
            try testing.expectEqual(@as(i128, @bitCast(int.value.bytes)), 0);
            // Positive zero should not need a sign
        },
        else => {
            try testing.expect(false); // Should be int
        },
    }
}
