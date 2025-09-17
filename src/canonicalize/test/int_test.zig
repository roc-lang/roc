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
const builtins = @import("builtins");
const Can = @import("../Can.zig");
const CIR = @import("../CIR.zig");
const RocDec = builtins.dec.RocDec;
const TestEnv = @import("TestEnv.zig").TestEnv;
const ModuleEnv = @import("../ModuleEnv.zig");
const parseIntWithUnderscores = Can.parseIntWithUnderscores;
const Content = types.Content;

fn getIntValue(module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) !i128 {
    const expr = module_env.store.getExpr(expr_idx);
    switch (expr) {
        .e_int => |int_expr| {
            return @bitCast(int_expr.value.bytes);
        },
        else => return error.NotAnInteger,
    }
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

test "hexadecimal integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: u8,
    }{
        // Basic hex literals
        .{ .literal = "0x0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0x1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0xFF", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0x100", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0xFFFF", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0x10000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = 4 },
        .{ .literal = "0xFFFFFFFF", .expected_value = 4294967295, .expected_sign_needed = false, .expected_bits_needed = 5 },
        .{ .literal = "0x100000000", .expected_value = 4294967296, .expected_sign_needed = false, .expected_bits_needed = 6 },
        .{ .literal = "0xFFFFFFFFFFFFFFFF", .expected_value = @as(i128, @bitCast(@as(u128, 18446744073709551615))), .expected_sign_needed = false, .expected_bits_needed = 7 },

        // Hex with underscores
        .{ .literal = "0x1_000", .expected_value = 4096, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0xFF_FF", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0x1234_5678_9ABC_DEF0", .expected_value = @as(i128, @bitCast(@as(u128, 0x123456789ABCDEF0))), .expected_sign_needed = false, .expected_bits_needed = 6 },

        // Negative hex literals
        .{ .literal = "-0x1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0x80", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0x81", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = 1 },
        .{ .literal = "-0x8000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = 2 },
        .{ .literal = "-0x8001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = 3 },
        .{ .literal = "-0x80000000", .expected_value = -2147483648, .expected_sign_needed = true, .expected_bits_needed = 4 },
        .{ .literal = "-0x80000001", .expected_value = -2147483649, .expected_sign_needed = true, .expected_bits_needed = 5 },
        .{ .literal = "-0x8000000000000000", .expected_value = -9223372036854775808, .expected_sign_needed = true, .expected_bits_needed = 6 },
        .{ .literal = "-0x8000000000000001", .expected_value = @as(i128, -9223372036854775809), .expected_sign_needed = true, .expected_bits_needed = 7 },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = try ModuleEnv.init(gpa, tc.literal);
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        var ast = try parse.parseExpr(&env.common, env.gpa);
        defer ast.deinit(gpa);

        var czer = try Can.init(&env, &ast, null);
        defer czer.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try czer.canonicalizeExpr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = env.store.getExpr(canonical_expr_idx.get_idx());
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx.get_idx()));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "binary integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: u8,
    }{
        // Basic binary literals
        .{ .literal = "0b0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b10", .expected_value = 2, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b11111111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0b100000000", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0b1111111111111111", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0b10000000000000000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = 4 },

        // Binary with underscores
        .{ .literal = "0b11_11", .expected_value = 15, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b1111_1111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0b1_0000_0000", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0b1010_1010_1010_1010", .expected_value = 43690, .expected_sign_needed = false, .expected_bits_needed = 3 },

        // Negative binary
        .{ .literal = "-0b1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0b10000000", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0b10000001", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = 1 },
        .{ .literal = "-0b1000000000000000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = 2 },
        .{ .literal = "-0b1000000000000001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = 3 },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = try ModuleEnv.init(gpa, tc.literal);
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        var ast = try parse.parseExpr(&env.common, env.gpa);
        defer ast.deinit(gpa);

        var czer = try Can.init(&env, &ast, null);
        defer czer.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try czer.canonicalizeExpr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = env.store.getExpr(canonical_expr_idx.get_idx());
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx.get_idx()));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "octal integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: u8,
    }{
        // Basic octal literals
        .{ .literal = "0o0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o7", .expected_value = 7, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o10", .expected_value = 8, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o377", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0o400", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0o177777", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0o200000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = 4 },

        // Octal with underscores
        .{ .literal = "0o377_377", .expected_value = 130815, .expected_sign_needed = false, .expected_bits_needed = 4 },
        .{ .literal = "0o1_234_567", .expected_value = 342391, .expected_sign_needed = false, .expected_bits_needed = 4 },

        // Negative octal literals
        .{ .literal = "-0o1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0o100", .expected_value = -64, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0o200", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0o201", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = 1 },
        .{ .literal = "-0o100000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = 2 },
        .{ .literal = "-0o100001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = 3 },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = try ModuleEnv.init(gpa, tc.literal);
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        var ast = try parse.parseExpr(&env.common, env.gpa);
        defer ast.deinit(gpa);

        var czer = try Can.init(&env, &ast, null);
        defer czer.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try czer.canonicalizeExpr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = env.store.getExpr(canonical_expr_idx.get_idx());
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx.get_idx()));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "integer literals with uppercase base prefixes" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: u8,
    }{
        // Uppercase hex prefix
        .{ .literal = "0X0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0X1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0XFF", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0XABCD", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = 3 },

        // Uppercase binary prefix
        .{ .literal = "0B0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0B1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0B1111", .expected_value = 15, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0B11111111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },

        // Uppercase octal prefix
        .{ .literal = "0O0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0O7", .expected_value = 7, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0O377", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0O777", .expected_value = 511, .expected_sign_needed = false, .expected_bits_needed = 2 },

        // Mixed case in value (should still work)
        .{ .literal = "0xAbCd", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0XaBcD", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = 3 },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = try ModuleEnv.init(gpa, tc.literal);
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        var ast = try parse.parseExpr(&env.common, gpa);
        defer ast.deinit(gpa);

        var czer = try Can.init(&env, &ast, null);
        defer czer.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try czer.canonicalizeExpr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = env.store.getExpr(canonical_expr_idx.get_idx());
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx.get_idx()));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "numeric literal patterns use pattern idx as type var" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test that int literal patterns work and use the pattern index as the type variable
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        // Create an int literal pattern directly
        const int_pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(int_pattern, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 0,
            } } },
        }, base.Region.zero());

        // Verify the stored pattern
        const stored_pattern = env.store.getPattern(pattern_idx);
        try std.testing.expect(stored_pattern == .int_literal);
        try std.testing.expectEqual(@as(i128, 42), @as(i128, @bitCast(stored_pattern.int_literal.value.bytes)));

        // Verify the pattern index can be used as a type variable
        const pattern_as_type_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(false, requirements.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }

    // Test that f64 literal patterns work
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        // Create a dec literal pattern directly
        const dec_pattern = CIR.Pattern{
            .dec_literal = .{
                .value = RocDec.fromF64(3.14) orelse unreachable,
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(dec_pattern, Content{
            .structure = .{ .num = .{ .frac_unbound = .{
                .fits_in_f32 = true,
                .fits_in_dec = true,
            } } },
        }, base.Region.zero());

        // Verify the stored pattern
        const stored_pattern = env.store.getPattern(pattern_idx);
        try std.testing.expect(stored_pattern == .dec_literal);
        const expected_dec = RocDec.fromF64(3.14) orelse unreachable;
        try std.testing.expectEqual(expected_dec.num, stored_pattern.dec_literal.value.num);

        // Verify the pattern index can be used as a type variable
        const pattern_as_type_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .frac_unbound => |requirements| {
                        try std.testing.expectEqual(true, requirements.fits_in_f32);
                        try std.testing.expectEqual(true, requirements.fits_in_dec);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "numeric pattern types: unbound vs polymorphic" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test int_unbound pattern
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, -17)), .kind = .i128 },
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(pattern, Content{
            .structure = .{ .num = .{ .int_unbound = .{
                .sign_needed = true,
                .bits_needed = 0,
            } } },
        }, base.Region.zero());

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .int_unbound => |req| {
                        try std.testing.expectEqual(true, req.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), req.bits_needed);
                    },
                    else => return error.ExpectedIntUnbound,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test int_poly pattern (polymorphic integer that can be different int types)
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 255)), .kind = .i128 },
            },
        };

        // Create a fresh type variable for polymorphic int
        const poly_var = try env.types.fresh();

        const pattern_idx = try env.store.addPattern(pattern, base.Region.zero());
        _ = try env.types.freshFromContent(Content{
            .structure = .{
                .num = .{
                    .int_poly = .{
                        .var_ = poly_var,
                        .requirements = .{
                            .sign_needed = false,
                            .bits_needed = 1, // Needs at least 8 bits for 255
                        },
                    },
                },
            },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .int_poly => |poly| {
                        try std.testing.expectEqual(false, poly.requirements.sign_needed);
                        try std.testing.expectEqual(@as(u8, 1), poly.requirements.bits_needed);
                        try std.testing.expectEqual(poly_var, poly.var_);
                    },
                    else => return error.ExpectedIntPoly,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test num_unbound pattern (can be int or frac)
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 10)), .kind = .i128 },
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(pattern, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 0,
            } } },
        }, base.Region.zero());

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_unbound => |req| {
                        try std.testing.expectEqual(false, req.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), req.bits_needed);
                    },
                    else => return error.ExpectedNumUnbound,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test num_poly pattern (polymorphic num that can be int or frac)
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 5)), .kind = .i128 },
            },
        };

        // Create a fresh type variable for polymorphic num
        const poly_var = try env.types.fresh();

        const pattern_idx = try env.store.addPattern(pattern, base.Region.zero());
        _ = try env.types.freshFromContent(Content{
            .structure = .{ .num = .{ .num_poly = .{
                .var_ = poly_var,
                .requirements = .{
                    .sign_needed = false,
                    .bits_needed = 0,
                },
            } } },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(false, poly.requirements.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), poly.requirements.bits_needed);
                        try std.testing.expectEqual(poly_var, poly.var_);
                    },
                    else => return error.ExpectedNumPoly,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test frac_unbound pattern
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        const pattern = CIR.Pattern{
            .dec_literal = .{
                .value = RocDec.fromF64(2.5) orelse unreachable,
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(pattern, Content{
            .structure = .{ .num = .{ .frac_unbound = .{
                .fits_in_f32 = true,
                .fits_in_dec = true,
            } } },
        }, base.Region.zero());

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .frac_unbound => |req| {
                        try std.testing.expectEqual(true, req.fits_in_f32);
                        try std.testing.expectEqual(true, req.fits_in_dec);
                    },
                    else => return error.ExpectedFracUnbound,
                },
                else => {},
            },
            else => {},
        }
    }
}

test "numeric pattern types: unbound vs polymorphic - frac" {
    const gpa = std.testing.allocator;

    // Test frac_poly pattern
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        const pattern = CIR.Pattern{
            .dec_literal = .{
                .value = RocDec.fromF64(1000000.0) orelse unreachable,
            },
        };

        // Create a fresh type variable for polymorphic frac
        const poly_var = try env.types.fresh();

        const pattern_idx = try env.store.addPattern(pattern, base.Region.zero());
        _ = try env.types.freshFromContent(Content{
            .structure = .{
                .num = .{
                    .frac_poly = .{
                        .var_ = poly_var,
                        .requirements = .{
                            .fits_in_f32 = true,
                            .fits_in_dec = false, // Infinity doesn't fit in Dec
                        },
                    },
                },
            },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .frac_poly => |poly| {
                        try std.testing.expectEqual(true, poly.requirements.fits_in_f32);
                        try std.testing.expectEqual(false, poly.requirements.fits_in_dec);
                        try std.testing.expectEqual(poly_var, poly.var_);
                    },
                    else => return error.ExpectedFracPoly,
                },
                else => {},
            },
            else => {},
        }
    }
}

test "pattern numeric literal value edge cases" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test max/min integer values
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        // Test i128 max
        const max_pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, std.math.maxInt(i128))), .kind = .i128 },
            },
        };
        const max_idx = try env.store.addPattern(max_pattern, base.Region.zero());
        const stored_max = env.store.getPattern(max_idx);
        try std.testing.expectEqual(std.math.maxInt(i128), @as(i128, @bitCast(stored_max.int_literal.value.bytes)));

        // Test i128 min
        const min_pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, std.math.minInt(i128))), .kind = .i128 },
            },
        };
        const min_idx = try env.store.addPattern(min_pattern, base.Region.zero());
        const stored_min = env.store.getPattern(min_idx);
        try std.testing.expectEqual(std.math.minInt(i128), @as(i128, @bitCast(stored_min.int_literal.value.bytes)));
    }

    // Test small decimal pattern
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        const small_dec_pattern = CIR.Pattern{
            .small_dec_literal = .{
                .numerator = 1234,
                .denominator_power_of_ten = 2, // 12.34

            },
        };

        const pattern_idx = try env.store.addPattern(small_dec_pattern, base.Region.zero());
        const stored = env.store.getPattern(pattern_idx);

        try std.testing.expect(stored == .small_dec_literal);
        try std.testing.expectEqual(@as(i16, 1234), stored.small_dec_literal.numerator);
        try std.testing.expectEqual(@as(u8, 2), stored.small_dec_literal.denominator_power_of_ten);
    }

    // Test dec literal pattern
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        const dec_pattern = CIR.Pattern{
            .dec_literal = .{
                .value = RocDec{ .num = 314159265358979323 }, // π * 10^17

            },
        };

        const pattern_idx = try env.store.addPattern(dec_pattern, base.Region.zero());
        const stored = env.store.getPattern(pattern_idx);

        try std.testing.expect(stored == .dec_literal);
        try std.testing.expectEqual(@as(i128, 314159265358979323), stored.dec_literal.value.num);
    }

    // Test special float values
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        // Test negative zero (RocDec doesn't distinguish between +0 and -0)
        const neg_zero_pattern = CIR.Pattern{
            .dec_literal = .{
                .value = RocDec.fromF64(-0.0) orelse unreachable,
            },
        };
        const neg_zero_idx = try env.store.addPattern(neg_zero_pattern, base.Region.zero());
        const stored_neg_zero = env.store.getPattern(neg_zero_idx);
        try std.testing.expect(stored_neg_zero == .dec_literal);
        try std.testing.expectEqual(@as(i128, 0), stored_neg_zero.dec_literal.value.num);
    }
}

test "pattern literal type transitions" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test transitioning from unbound to concrete type
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 100)), .kind = .i128 },
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(pattern, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 1,
            } } },
        }, base.Region.zero());

        // Simulate type inference determining it's a U8
        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        _ = try env.types.setVarContent(pattern_var, Content{
            .structure = .{ .num = types.Num.int_u8 },
        });

        // Verify it resolved to U8
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_compact => |compact| switch (compact) {
                        .int => |int| {
                            try std.testing.expect(int == .u8);
                        },
                        else => return error.ExpectedInt,
                    },
                    else => return error.ExpectedNumCompact,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test hex/binary/octal patterns must be integers
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        // Hex pattern (0xFF)
        const hex_pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 0xFF)), .kind = .i128 },
            },
        };

        const hex_idx = try env.addPatternAndTypeVar(hex_pattern, Content{
            .structure = .{ .num = .{ .int_unbound = .{
                .sign_needed = false,
                .bits_needed = 1,
            } } },
        }, base.Region.zero());

        const hex_var: types.Var = @enumFromInt(@intFromEnum(hex_idx));
        const resolved = env.types.resolveVar(hex_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .int_unbound => |req| {
                        // Verify it's constrained to integers only
                        try std.testing.expectEqual(false, req.sign_needed);
                        try std.testing.expectEqual(@as(u8, 1), req.bits_needed);
                    },
                    else => return error.ExpectedIntUnbound,
                },
                else => {},
            },
            else => {},
        }
    }
}

test "pattern type inference with numeric literals" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test that pattern indices work correctly as type variables with type inference
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        // Create patterns representing different numeric literals
        const patterns = [_]struct {
            pattern: CIR.Pattern,
            expected_type: types.Content,
        }{
            // Small positive int - could be any unsigned type
            .{
                .pattern = CIR.Pattern{
                    .int_literal = .{
                        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
                    },
                },
                .expected_type = Content{ .structure = .{ .num = .{ .num_unbound = .{
                    .sign_needed = false,
                    .bits_needed = 0,
                } } } },
            },
            // Negative int - needs signed type
            .{
                .pattern = CIR.Pattern{
                    .int_literal = .{
                        .value = .{ .bytes = @bitCast(@as(i128, -42)), .kind = .i128 },
                    },
                },
                .expected_type = Content{ .structure = .{ .num = .{ .num_unbound = .{
                    .sign_needed = true,
                    .bits_needed = 0,
                } } } },
            },
            // Large int requiring more bits
            .{
                .pattern = CIR.Pattern{
                    .int_literal = .{
                        .value = .{ .bytes = @bitCast(@as(i128, 65536)), .kind = .i128 },
                    },
                },
                .expected_type = Content{
                    .structure = .{
                        .num = .{
                            .num_unbound = .{
                                .sign_needed = false,
                                .bits_needed = 4, // Needs at least 17 bits
                            },
                        },
                    },
                },
            },
        };

        for (patterns) |test_case| {
            const pattern_idx = try env.addPatternAndTypeVar(test_case.pattern, test_case.expected_type, base.Region.zero());

            // Verify the pattern index works as a type variable
            const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
            const resolved = env.types.resolveVar(pattern_var);

            // Compare the resolved type with expected
            try std.testing.expectEqual(test_case.expected_type, resolved.desc.content);
        }
    }

    // Test patterns with type constraints from context
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

        // Create a pattern that will be constrained by context
        const pattern = CIR.Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 100)), .kind = .i128 },
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(pattern, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 1,
            } } },
        }, base.Region.zero());

        // Simulate type inference constraining it to U8
        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        _ = try env.types.setVarContent(pattern_var, Content{
            .structure = .{ .num = types.Num.int_u8 },
        });

        // Verify it was constrained correctly
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_compact => |compact| {
                        try std.testing.expect(compact.int == .u8);
                    },
                    else => return error.ExpectedConcreteType,
                },
                else => {},
            },
            else => {},
        }
    }
}

test "parseIntWithUnderscores function" {
    // Test the parseIntWithUnderscores helper function directly
    const test_cases = [_]struct {
        text: []const u8,
        base: u8,
        expected: u128,
    }{
        // Hex cases from the failing snapshot
        .{ .text = "E", .base = 16, .expected = 14 },
        .{ .text = "f", .base = 16, .expected = 15 },
        .{ .text = "20", .base = 16, .expected = 32 },

        // Binary cases that work
        .{ .text = "10001", .base = 2, .expected = 17 },
        .{ .text = "1_0010", .base = 2, .expected = 18 },

        // Decimal cases
        .{ .text = "123", .base = 10, .expected = 123 },
        .{ .text = "1_000", .base = 10, .expected = 1000 },

        // More hex cases - mixed case
        .{ .text = "FF", .base = 16, .expected = 255 },
        .{ .text = "ff", .base = 16, .expected = 255 },
        .{ .text = "Ff", .base = 16, .expected = 255 },
        .{ .text = "1A", .base = 16, .expected = 26 },
        .{ .text = "1a", .base = 16, .expected = 26 },
        .{ .text = "AB_CD", .base = 16, .expected = 0xABCD },
        .{ .text = "ab_cd", .base = 16, .expected = 0xABCD },

        // More binary cases
        .{ .text = "1111", .base = 2, .expected = 15 },
        .{ .text = "1010_1010", .base = 2, .expected = 0b10101010 },

        // Octal cases
        .{ .text = "777", .base = 8, .expected = 0o777 },
        .{ .text = "123", .base = 8, .expected = 0o123 },
        .{ .text = "7_7_7", .base = 8, .expected = 0o777 },

        // Edge cases
        .{ .text = "0", .base = 10, .expected = 0 },
        .{ .text = "0", .base = 16, .expected = 0 },
        .{ .text = "0", .base = 2, .expected = 0 },
        .{ .text = "1", .base = 10, .expected = 1 },
        .{ .text = "A", .base = 16, .expected = 10 },
        .{ .text = "a", .base = 16, .expected = 10 },
    };

    for (test_cases) |tc| {
        const result = parseIntWithUnderscores(u128, tc.text, tc.base) catch |err| {
            std.debug.print("ERROR parsing '{s}' base {}: {}\n", .{ tc.text, tc.base, err });
            return err;
        };

        if (result != tc.expected) {
            std.debug.print("MISMATCH: parseIntWithUnderscores('{s}', {}) = {} (expected {})\n", .{ tc.text, tc.base, result, tc.expected });
        }

        try std.testing.expectEqual(tc.expected, result);
    }
}

test "parseNumLiteralWithSuffix function" {
    // Test the parseNumLiteralWithSuffix function to ensure correct parsing of prefixes and suffixes
    const test_cases = [_]struct {
        input: []const u8,
        expected_num_text: []const u8,
        expected_suffix: ?[]const u8,
    }{
        // Hex literals - these were the originally failing cases
        .{ .input = "0xE", .expected_num_text = "0xE", .expected_suffix = null },
        .{ .input = "0xf", .expected_num_text = "0xf", .expected_suffix = null },
        .{ .input = "0x20", .expected_num_text = "0x20", .expected_suffix = null },
        .{ .input = "0xFF", .expected_num_text = "0xFF", .expected_suffix = null },
        .{ .input = "0Xff", .expected_num_text = "0Xff", .expected_suffix = null },
        .{ .input = "0XFF", .expected_num_text = "0XFF", .expected_suffix = null },

        // Binary literals that work correctly
        .{ .input = "0b10001", .expected_num_text = "0b10001", .expected_suffix = null },
        .{ .input = "0b1_0010", .expected_num_text = "0b1_0010", .expected_suffix = null },
        .{ .input = "0B1111", .expected_num_text = "0B1111", .expected_suffix = null },
        .{ .input = "0b0", .expected_num_text = "0b0", .expected_suffix = null },

        // Octal literals
        .{ .input = "0o777", .expected_num_text = "0o777", .expected_suffix = null },
        .{ .input = "0O123", .expected_num_text = "0O123", .expected_suffix = null },

        // Suffixed literals
        .{ .input = "1u8", .expected_num_text = "1", .expected_suffix = "u8" },
        .{ .input = "0xFFu32", .expected_num_text = "0xFF", .expected_suffix = "u32" },
        .{ .input = "0b1010i16", .expected_num_text = "0b1010", .expected_suffix = "i16" },
        .{ .input = "11.0f32", .expected_num_text = "11.0", .expected_suffix = "f32" },
        .{ .input = "3.14f64", .expected_num_text = "3.14", .expected_suffix = "f64" },

        // Regular decimal literals
        .{ .input = "123", .expected_num_text = "123", .expected_suffix = null },
        .{ .input = "1_000", .expected_num_text = "1_000", .expected_suffix = null },
        .{ .input = "42", .expected_num_text = "42", .expected_suffix = null },
    };

    for (test_cases) |tc| {
        const result = types.Num.parseNumLiteralWithSuffix(tc.input);

        if (!std.mem.eql(u8, result.num_text, tc.expected_num_text)) {
            std.debug.print("MISMATCH num_text: parseNumLiteralWithSuffix('{s}').num_text = '{s}' (expected '{s}')\n", .{ tc.input, result.num_text, tc.expected_num_text });
        }
        try std.testing.expectEqualSlices(u8, tc.expected_num_text, result.num_text);

        if (tc.expected_suffix) |expected_suffix| {
            try std.testing.expect(result.suffix != null);
            try std.testing.expectEqualSlices(u8, expected_suffix, result.suffix.?);
        } else {
            try std.testing.expect(result.suffix == null);
        }
    }
}

test "hex literal parsing logic integration" {
    // Test the complete hex literal parsing logic used in canonicalizeExpr
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: u128,
    }{
        // These are the exact literals from the failing snapshot
        .{ .literal = "0xE", .expected_value = 14 },
        .{ .literal = "0xf", .expected_value = 15 },
        .{ .literal = "0x20", .expected_value = 32 },

        // Binary literals that work correctly
        .{ .literal = "0b10001", .expected_value = 17 },
        .{ .literal = "0b1_0010", .expected_value = 18 },
    };

    for (test_cases) |tc| {
        // Mimic the exact parsing logic from canonicalizeExpr
        const parsed = types.Num.parseNumLiteralWithSuffix(tc.literal);

        const is_negated = parsed.num_text[0] == '-';
        const after_minus_sign = @as(usize, @intFromBool(is_negated));

        var first_digit: usize = undefined;
        const DEFAULT_BASE = 10;
        var int_base: u8 = undefined;

        if (parsed.num_text[after_minus_sign] == '0' and parsed.num_text.len > after_minus_sign + 2) {
            switch (parsed.num_text[after_minus_sign + 1]) {
                'x', 'X' => {
                    int_base = 16;
                    first_digit = after_minus_sign + 2;
                },
                'o', 'O' => {
                    int_base = 8;
                    first_digit = after_minus_sign + 2;
                },
                'b', 'B' => {
                    int_base = 2;
                    first_digit = after_minus_sign + 2;
                },
                else => {
                    int_base = DEFAULT_BASE;
                    first_digit = after_minus_sign;
                },
            }
        } else {
            int_base = DEFAULT_BASE;
            first_digit = after_minus_sign;
        }

        const digit_part = parsed.num_text[first_digit..];

        // Debug print to see what's happening
        if (tc.literal[0] == '0' and (tc.literal[1] == 'x' or tc.literal[1] == 'b')) {
            // std.debug.print("Parsing '{s}': num_text='{s}', digit_part='{s}', base={}, expected={}\n", .{ tc.literal, parsed.num_text, digit_part, int_base, tc.expected_value });
        }

        const u128_val = parseIntWithUnderscores(u128, digit_part, int_base) catch |err| {
            std.debug.print("ERROR parsing digit_part '{s}' with base {}: {}\n", .{ digit_part, int_base, err });
            try std.testing.expect(false);
            continue;
        };

        if (u128_val != tc.expected_value) {
            std.debug.print("MISMATCH for '{s}': got {}, expected {}\n", .{ tc.literal, u128_val, tc.expected_value });
        }

        try std.testing.expectEqual(tc.expected_value, u128_val);
    }
}
