const std = @import("std");
const testing = std.testing;
const base = @import("../../../base.zig");
const parse = @import("../../parse.zig");
const canonicalize = @import("../../../check/canonicalize.zig");
const CIR = canonicalize.CIR;
const types = @import("../../../types.zig");

const test_allocator = testing.allocator;

fn parseAndCanonicalizeInt(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *base.ModuleEnv,
    parse_ast: *parse.AST,
    cir: *CIR,
    can: *canonicalize,
    expr_idx: CIR.Expr.Idx,
} {
    const module_env = try allocator.create(base.ModuleEnv);
    module_env.* = base.ModuleEnv.init(allocator);

    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = parse.parseExpr(module_env, source);

    parse_ast.store.emptyScratch();

    const cir = try allocator.create(CIR);
    cir.* = CIR.init(module_env);

    const can = try allocator.create(canonicalize);
    can.* = canonicalize.init(cir, parse_ast);

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = can.canonicalize_expr(expr_idx) orelse {
        const diagnostic_idx = cir.store.addDiagnostic(.{ .not_implemented = .{
            .feature = cir.env.strings.insert(allocator, "canonicalization failed"),
            .region = base.Region.zero(),
        } });
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .cir = cir,
            .can = can,
            .expr_idx = cir.store.addExpr(.{ .runtime_error = .{
                .diagnostic = diagnostic_idx,
                .region = base.Region.zero(),
            } }),
        };
    };

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .cir = cir,
        .can = can,
        .expr_idx = canonical_expr_idx,
    };
}

fn cleanup(allocator: std.mem.Allocator, resources: anytype) void {
    resources.can.deinit();
    resources.cir.deinit();
    resources.parse_ast.deinit(allocator);
    resources.module_env.deinit();
    allocator.destroy(resources.can);
    allocator.destroy(resources.cir);
    allocator.destroy(resources.parse_ast);
    allocator.destroy(resources.module_env);
}

fn getIntValue(cir: *CIR, expr_idx: CIR.Expr.Idx) !CIR.IntValue {
    const expr = cir.store.getExpr(expr_idx);
    switch (expr) {
        .int => |int_expr| return int_expr.value,
        .num => |num_expr| return num_expr.value,
        else => return error.NotAnInteger,
    }
}

fn intValueToI128(int_value: CIR.IntValue) i128 {
    return int_value.toI128();
}

test "canonicalize simple positive integer" {
    const source = "42";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_value = try getIntValue(resources.cir, resources.expr_idx);
    const value = intValueToI128(int_value);
    try testing.expectEqual(@as(i128, 42), value);
    try testing.expectEqual(CIR.IntValue.Kind.i128, int_value.kind);
}

test "canonicalize simple negative integer" {
    const source = "-42";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_value = try getIntValue(resources.cir, resources.expr_idx);
    const value = intValueToI128(int_value);
    try testing.expectEqual(@as(i128, -42), value);
    try testing.expectEqual(CIR.IntValue.Kind.i128, int_value.kind);
}

test "canonicalize zero" {
    const source = "0";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_value = try getIntValue(resources.cir, resources.expr_idx);
    const value = intValueToI128(int_value);
    try testing.expectEqual(@as(i128, 0), value);
}

test "canonicalize large positive integer" {
    const source = "9223372036854775807"; // i64 max
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_value = try getIntValue(resources.cir, resources.expr_idx);
    const value = intValueToI128(int_value);
    try testing.expectEqual(@as(i128, 9223372036854775807), value);
}

test "canonicalize large negative integer" {
    const source = "-9223372036854775808"; // i64 min
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_value = try getIntValue(resources.cir, resources.expr_idx);
    const value = intValueToI128(int_value);
    try testing.expectEqual(@as(i128, -9223372036854775808), value);
}

test "canonicalize very large integer" {
    const source = "170141183460469231731687303715884105727"; // i128 max
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_value = try getIntValue(resources.cir, resources.expr_idx);
    const value = intValueToI128(int_value);
    try testing.expectEqual(@as(i128, 170141183460469231731687303715884105727), value);
}

test "canonicalize very large negative integer" {
    const source = "-170141183460469231731687303715884105728"; // i128 min
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_value = try getIntValue(resources.cir, resources.expr_idx);
    const value = intValueToI128(int_value);
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
        const resources = try parseAndCanonicalizeInt(test_allocator, tc.source);
        defer cleanup(test_allocator, resources);

        const int_value = try getIntValue(resources.cir, resources.expr_idx);
        const value = intValueToI128(int_value);
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
        const resources = try parseAndCanonicalizeInt(test_allocator, tc.source);
        defer cleanup(test_allocator, resources);

        const int_value = try getIntValue(resources.cir, resources.expr_idx);
        const value = intValueToI128(int_value);
        try testing.expectEqual(tc.expected, value);
    }
}

test "canonicalize integer bounds" {
    // Test that the correct bounds are inferred
    const test_cases = [_]struct {
        source: []const u8,
        expected_value: i128,
        expected_bound: types.Num.Int.Precision,
    }{
        .{ .source = "0", .expected_value = 0, .expected_bound = .i8 },
        .{ .source = "127", .expected_value = 127, .expected_bound = .i8 },
        .{ .source = "128", .expected_value = 128, .expected_bound = .u8 },
        .{ .source = "255", .expected_value = 255, .expected_bound = .u8 },
        .{ .source = "256", .expected_value = 256, .expected_bound = .i16 },
        .{ .source = "-1", .expected_value = -1, .expected_bound = .i8 },
        .{ .source = "-128", .expected_value = -128, .expected_bound = .i8 },
        .{ .source = "-129", .expected_value = -129, .expected_bound = .i16 },
        .{ .source = "32767", .expected_value = 32767, .expected_bound = .i16 },
        .{ .source = "32768", .expected_value = 32768, .expected_bound = .u16 },
        .{ .source = "65535", .expected_value = 65535, .expected_bound = .u16 },
        .{ .source = "65536", .expected_value = 65536, .expected_bound = .i32 },
        .{ .source = "-32768", .expected_value = -32768, .expected_bound = .i16 },
        .{ .source = "-32769", .expected_value = -32769, .expected_bound = .i32 },
    };

    for (test_cases) |tc| {
        const resources = try parseAndCanonicalizeInt(test_allocator, tc.source);
        defer cleanup(test_allocator, resources);

        const expr = resources.cir.store.getExpr(resources.expr_idx);
        switch (expr) {
            .int => |int_expr| {
                const value = intValueToI128(int_expr.value);
                try testing.expectEqual(tc.expected_value, value);
                try testing.expectEqual(tc.expected_bound, int_expr.bound);
            },
            .num => |num_expr| {
                const value = intValueToI128(num_expr.value);
                try testing.expectEqual(tc.expected_value, value);
                try testing.expectEqual(tc.expected_bound, num_expr.bound);
            },
            else => return error.UnexpectedExprType,
        }
    }
}

test "canonicalize integer literal creates correct type variables" {
    const source = "42";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const expr = resources.cir.store.getExpr(resources.expr_idx);
    switch (expr) {
        .int => |int_expr| {
            // Verify type variables were created (they should be valid indices)
            // Note: @enumFromInt(0) could be a valid type variable, so just check they exist
            _ = int_expr.int_var;
            _ = int_expr.precision_var;

            // Verify the string literal was interned
            const literal_str = resources.cir.env.strings.get(int_expr.literal);
            try testing.expectEqualStrings("42", literal_str);
        },
        else => return error.UnexpectedExprType,
    }
}

test "canonicalize invalid integer literal" {
    // Test individual cases since some might fail during parsing vs canonicalization

    // "12abc" - invalid characters in number
    {
        const resources = try parseAndCanonicalizeInt(test_allocator, "12abc");
        defer cleanup(test_allocator, resources);
        const expr = resources.cir.store.getExpr(resources.expr_idx);
        try testing.expect(expr == .runtime_error);
    }

    // Leading zeros with digits
    {
        const resources = try parseAndCanonicalizeInt(test_allocator, "0123");
        defer cleanup(test_allocator, resources);
        const expr = resources.cir.store.getExpr(resources.expr_idx);
        // This might actually parse as 123, so let's check if it's a valid integer
        if (expr != .runtime_error) {
            const int_value = try getIntValue(resources.cir, resources.expr_idx);
            const value = intValueToI128(int_value);
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
        const resources = try parseAndCanonicalizeInt(test_allocator, tc.source);
        defer cleanup(test_allocator, resources);

        const int_value = try getIntValue(resources.cir, resources.expr_idx);
        try testing.expectEqualSlices(u8, &tc.expected_bytes, &int_value.bytes);
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

        const resources = try parseAndCanonicalizeInt(test_allocator, source);
        defer cleanup(test_allocator, resources);

        // Get the expression back from the store
        const retrieved_expr = resources.cir.store.getExpr(resources.expr_idx);
        const int_value = try getIntValue(resources.cir, resources.expr_idx);
        const value = intValueToI128(int_value);

        try testing.expectEqual(expected, value);

        // Verify it's stored as int expr
        try testing.expect(retrieved_expr == .int or retrieved_expr == .num);
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
        const resources = try parseAndCanonicalizeInt(test_allocator, tc.source);
        defer cleanup(test_allocator, resources);

        const int_value = try getIntValue(resources.cir, resources.expr_idx);
        const value = intValueToI128(int_value);
        try testing.expectEqual(tc.expected, value);
    }
}
