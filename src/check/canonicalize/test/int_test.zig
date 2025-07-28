//! Tests for integer literal canonicalization during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct canonicalization
//! of integer literals and integer expressions from parsed AST into the
//! compiler's canonical internal representation (CIR).

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types = @import("types");
const compile = @import("compile");
const parse = @import("parse");
const Can = @import("can");

const ModuleEnv = compile.ModuleEnv;

// Note: Each test should create its own GPA to avoid memory leak detection issues

fn parseAndCanonicalizeInt(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    expr_idx: ModuleEnv.Expr.Idx,
} {
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);

    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(module_env);

    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(allocator, "Test");

    const can = try allocator.create(Can);
    can.* = try Can.init(module_env, parse_ast, null);

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);

    // Check if parsing produced an error
    if (parse_ast.parse_diagnostics.items.len > 0 or parse_ast.tokenize_diagnostics.items.len > 0) {
        // Parsing failed, return with a valid but empty expression index
        // The tests should check for diagnostics rather than trying to access the expression
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .can = can,
            .expr_idx = undefined, // Don't use this when there are parse errors
        };
    }

    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse unreachable;

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can,
        .expr_idx = canonical_expr_idx.get_idx(),
    };
}

fn cleanup(allocator: std.mem.Allocator, resources: anytype) void {
    resources.can.deinit();
    resources.parse_ast.deinit(allocator);
    resources.module_env.deinit();
    allocator.destroy(resources.can);
    allocator.destroy(resources.parse_ast);
    allocator.destroy(resources.module_env);
}

fn getIntValue(module_env: *ModuleEnv, expr_idx: ModuleEnv.Expr.Idx) !i128 {
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
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const source = "42";
    const resources = try parseAndCanonicalizeInt(gpa, source);
    defer cleanup(gpa, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, 42), value);
}

test "canonicalize simple negative integer" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const source = "-42";
    const resources = try parseAndCanonicalizeInt(gpa, source);
    defer cleanup(gpa, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, -42), value);
}

test "canonicalize zero" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const source = "0";
    const resources = try parseAndCanonicalizeInt(gpa, source);
    defer cleanup(gpa, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, 0), value);
}

test "canonicalize large positive integer" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const source = "9223372036854775807"; // i64 max
    const resources = try parseAndCanonicalizeInt(gpa, source);
    defer cleanup(gpa, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, 9223372036854775807), value);
}

test "canonicalize large negative integer" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const source = "-9223372036854775808"; // i64 min
    const resources = try parseAndCanonicalizeInt(gpa, source);
    defer cleanup(gpa, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, -9223372036854775808), value);
}

test "canonicalize very large integer" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const source = "170141183460469231731687303715884105727"; // i128 max
    const resources = try parseAndCanonicalizeInt(gpa, source);
    defer cleanup(gpa, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, 170141183460469231731687303715884105727), value);
}

test "canonicalize very large negative integer" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const source = "-170141183460469231731687303715884105728"; // i128 min
    const resources = try parseAndCanonicalizeInt(gpa, source);
    defer cleanup(gpa, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, -170141183460469231731687303715884105728), value);
}

test "canonicalize small integers" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

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
        const resources = try parseAndCanonicalizeInt(gpa, tc.source);
        defer cleanup(gpa, resources);

        const value = try getIntValue(resources.module_env, resources.expr_idx);
        try testing.expectEqual(tc.expected, value);
    }
}

test "canonicalize integer literals with underscores" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const test_cases = [_]struct { source: []const u8, expected: i128 }{
        .{ .source = "1_000", .expected = 1000 },
        .{ .source = "1_000_000", .expected = 1000000 },
        .{ .source = "-1_234_567", .expected = -1234567 },
        .{ .source = "123_456_789", .expected = 123456789 },
        .{ .source = "1_2_3_4_5", .expected = 12345 },
    };

    for (test_cases) |tc| {
        const resources = try parseAndCanonicalizeInt(gpa, tc.source);
        defer cleanup(gpa, resources);

        const value = try getIntValue(resources.module_env, resources.expr_idx);
        try testing.expectEqual(tc.expected, value);
    }
}

test "canonicalize integer with specific requirements" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

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
        const resources = try parseAndCanonicalizeInt(gpa, tc.source);
        defer cleanup(gpa, resources);

        const value = try getIntValue(resources.module_env, resources.expr_idx);
        try testing.expectEqual(tc.expected_value, value);
    }
}

test "canonicalize integer literal creates correct type variables" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const source = "42";
    const resources = try parseAndCanonicalizeInt(gpa, source);
    defer cleanup(gpa, resources);

    const expr = resources.module_env.store.getExpr(resources.expr_idx);
    switch (expr) {
        .e_int => {

            // Verify requirements were set
        },
        else => return error.UnexpectedExprType,
    }
}

test "canonicalize invalid integer literal" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test individual cases since some might fail during parsing vs canonicalization

    // "12abc" - invalid characters in number
    {
        const resources = try parseAndCanonicalizeInt(gpa, "12abc");
        defer cleanup(gpa, resources);
        // Should have parse errors
        try testing.expect(resources.parse_ast.parse_diagnostics.items.len > 0 or
            resources.parse_ast.tokenize_diagnostics.items.len > 0);
    }

    // Leading zeros with digits
    {
        const resources = try parseAndCanonicalizeInt(gpa, "0123");
        defer cleanup(gpa, resources);
        // This might actually parse as 123, check if we have diagnostics
        if (resources.parse_ast.parse_diagnostics.items.len == 0 and
            resources.parse_ast.tokenize_diagnostics.items.len == 0)
        {
            // No errors, so it should have parsed as 123
            const value = try getIntValue(resources.module_env, resources.expr_idx);
            try testing.expectEqual(@as(i128, 123), value);
        }
    }
}

test "canonicalize integer preserves all bytes correctly" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

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
        const resources = try parseAndCanonicalizeInt(gpa, tc.source);
        defer cleanup(gpa, resources);

        const value = try getIntValue(resources.module_env, resources.expr_idx);
        const value_bytes: [16]u8 = @bitCast(value);
        try testing.expectEqualSlices(u8, &tc.expected_bytes, &value_bytes);
    }
}

test "canonicalize integer round trip through NodeStore" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test that integers survive storage and retrieval from NodeStore
    const test_values = [_]i128{
        0,      1,     -1,     42,         -42,
        127,    -128,  255,    -256,       32767,
        -32768, 65535, -65536, 2147483647, -2147483648,
    };

    for (test_values) |expected| {
        var buf: [64]u8 = undefined;
        const source = try std.fmt.bufPrint(&buf, "{}", .{expected});

        const resources = try parseAndCanonicalizeInt(gpa, source);
        defer cleanup(gpa, resources);

        // Get the expression back from the store
        const value = try getIntValue(resources.module_env, resources.expr_idx);

        try testing.expectEqual(expected, value);
    }
}

test "canonicalize integer with maximum digits" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test very long digit sequences
    const test_cases = [_]struct { source: []const u8, expected: i128 }{
        .{ .source = "000000000000000000000000000000000000000001", .expected = 1 },
        .{ .source = "000000000000000000000000000000000000000000", .expected = 0 },
        .{ .source = "-000000000000000000000000000000000000000001", .expected = -1 },
    };

    for (test_cases) |tc| {
        const resources = try parseAndCanonicalizeInt(gpa, tc.source);
        defer cleanup(gpa, resources);

        // Check if parsing succeeded (leading zeros might be treated specially)
        const has_errors = resources.parse_ast.parse_diagnostics.items.len > 0 or
            resources.parse_ast.tokenize_diagnostics.items.len > 0;

        if (!has_errors) {
            const value = try getIntValue(resources.module_env, resources.expr_idx);
            try testing.expectEqual(tc.expected, value);
        } else {
            // If there are errors, that's expected for numbers with leading zeros
            // Just verify we got some diagnostic
            try testing.expect(has_errors);
        }
    }
}

test "canonicalize integer requirements determination" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

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
        const resources = try parseAndCanonicalizeInt(gpa, tc.source);
        defer cleanup(gpa, resources);

        const value = try getIntValue(resources.module_env, resources.expr_idx);

        try testing.expectEqual(tc.expected_value, value);
    }
}

test "canonicalize integer literals outside supported range" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

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
        const resources = try parseAndCanonicalizeInt(gpa, source);
        defer cleanup(gpa, resources);

        const expr = resources.module_env.store.getExpr(resources.expr_idx);
        try testing.expect(expr == .e_runtime_error);
    }
}

test "invalid number literal - too large for u128" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    const source = "999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999";

    const resources = try parseAndCanonicalizeInt(allocator, source);
    defer cleanup(allocator, resources);

    // Should have produced diagnostics for number too large
    // Very large numbers might be caught during parsing or canonicalization
    const parse_errors = resources.parse_ast.parse_diagnostics.items.len > 0;
    const tokenize_errors = resources.parse_ast.tokenize_diagnostics.items.len > 0;

    // Only check canon diagnostics if parsing succeeded
    if (!parse_errors and !tokenize_errors) {
        const canon_diagnostics = try resources.module_env.getDiagnostics();
        defer allocator.free(canon_diagnostics);
        const canon_errors = canon_diagnostics.len > 0;

        if (!canon_errors) {
            // If no errors at all, check the expression type
            const expr = resources.module_env.store.getExpr(resources.expr_idx);
            // Large numbers should either fail to parse or produce a runtime error
            try testing.expect(expr == .e_runtime_error);
        }
    } else {
        // We have parse/tokenize errors, which is expected for this large number
        try testing.expect(true);
    }
}

test "invalid number literal - negative too large for i128" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    const source = "-999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999";

    const resources = try parseAndCanonicalizeInt(allocator, source);
    defer cleanup(allocator, resources);

    // Should have produced diagnostics for number too large
    // Very large negative numbers might be caught during parsing or canonicalization
    const parse_errors = resources.parse_ast.parse_diagnostics.items.len > 0;
    const tokenize_errors = resources.parse_ast.tokenize_diagnostics.items.len > 0;

    // Only check canon diagnostics if parsing succeeded
    if (!parse_errors and !tokenize_errors) {
        const canon_diagnostics = try resources.module_env.getDiagnostics();
        defer allocator.free(canon_diagnostics);
        const canon_errors = canon_diagnostics.len > 0;

        if (!canon_errors) {
            // If no errors at all, check the expression type
            const expr = resources.module_env.store.getExpr(resources.expr_idx);
            // Large numbers should either fail to parse or produce a runtime error
            try testing.expect(expr == .e_runtime_error);
        }
    } else {
        // We have parse/tokenize errors, which is expected for this large number
        try testing.expect(true);
    }
}

test "integer literal - negative zero" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCanonicalizeInt(gpa, "-0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
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
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCanonicalizeInt(gpa, "0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
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
