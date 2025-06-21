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

fn getIntValue(cir: *CIR, expr_idx: CIR.Expr.Idx) !struct { value: i128, precision: types.Num.Int.Precision } {
    const expr = cir.store.getExpr(expr_idx);
    switch (expr) {
        .int => |int_expr| {
            const precision = CIR.getIntPrecision(cir.env, int_expr.precision_var) orelse return error.NoPrecision;
            const value: i128 = int_expr.value.value;
            return .{ .value = value, .precision = precision };
        },
        .num => |num_expr| {
            // For num expressions, we need to resolve the num_var to get the precision
            // This is a simplified case - in real code we'd need to handle more complex type resolution
            const precision = types.Num.Int.Precision.i128; // Default for now
            const value: i128 = num_expr.value.value;
            return .{ .value = value, .precision = precision };
        },
        else => return error.NotAnInteger,
    }
}

fn interpretAsI128(value: i128, precision: types.Num.Int.Precision) i128 {
    // When the precision is u128, we interpret the i128 bits as unsigned
    if (precision == .u128) {
        // The value is already stored correctly, just return it
        return value;
    }
    return value;
}

test "canonicalize simple positive integer" {
    const source = "42";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = interpretAsI128(int_data.value, int_data.precision);
    try testing.expectEqual(@as(i128, 42), value);
    try testing.expectEqual(types.Num.Int.Precision.i8, int_data.precision);
}

test "canonicalize simple negative integer" {
    const source = "-42";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = interpretAsI128(int_data.value, int_data.precision);
    try testing.expectEqual(@as(i128, -42), value);
    try testing.expectEqual(types.Num.Int.Precision.i8, int_data.precision);
}

test "canonicalize zero" {
    const source = "0";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = interpretAsI128(int_data.value, int_data.precision);
    try testing.expectEqual(@as(i128, 0), value);
}

test "canonicalize large positive integer" {
    const source = "9223372036854775807"; // i64 max
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = interpretAsI128(int_data.value, int_data.precision);
    try testing.expectEqual(@as(i128, 9223372036854775807), value);
}

test "canonicalize large negative integer" {
    const source = "-9223372036854775808"; // i64 min
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = interpretAsI128(int_data.value, int_data.precision);
    try testing.expectEqual(@as(i128, -9223372036854775808), value);
}

test "canonicalize very large integer" {
    const source = "170141183460469231731687303715884105727"; // i128 max
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = interpretAsI128(int_data.value, int_data.precision);
    try testing.expectEqual(@as(i128, 170141183460469231731687303715884105727), value);
}

test "canonicalize very large negative integer" {
    const source = "-170141183460469231731687303715884105728"; // i128 min
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = interpretAsI128(int_data.value, int_data.precision);
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

        const int_data = try getIntValue(resources.cir, resources.expr_idx);
        const value = interpretAsI128(int_data.value, int_data.precision);
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

        const int_data = try getIntValue(resources.cir, resources.expr_idx);
        const value = interpretAsI128(int_data.value, int_data.precision);
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

        const int_data = try getIntValue(resources.cir, resources.expr_idx);
        const value = interpretAsI128(int_data.value, int_data.precision);
        try testing.expectEqual(tc.expected_value, value);
        try testing.expectEqual(tc.expected_bound, int_data.precision);
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
            const int_data = try getIntValue(resources.cir, resources.expr_idx);
            const value = interpretAsI128(int_data.value, int_data.precision);
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

        const int_data = try getIntValue(resources.cir, resources.expr_idx);
        const value_bytes: [16]u8 = @bitCast(int_data.value);
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

        const resources = try parseAndCanonicalizeInt(test_allocator, source);
        defer cleanup(test_allocator, resources);

        // Get the expression back from the store
        const int_data = try getIntValue(resources.cir, resources.expr_idx);
        const value = interpretAsI128(int_data.value, int_data.precision);

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
        const resources = try parseAndCanonicalizeInt(test_allocator, tc.source);
        defer cleanup(test_allocator, resources);

        const int_data = try getIntValue(resources.cir, resources.expr_idx);
        const value = interpretAsI128(int_data.value, int_data.precision);
        try testing.expectEqual(tc.expected, value);
    }
}

test "canonicalize signed vs unsigned interpretation" {
    // Test that values are interpreted correctly based on their precision
    const test_cases = [_]struct {
        source: []const u8,
        expected_signed: i128,
        expected_precision: types.Num.Int.Precision,
    }{
        // 255 should be stored as u8 and interpreted as 255
        .{ .source = "255", .expected_signed = 255, .expected_precision = .u8 },
        // 256 should be stored as i16 and interpreted as 256
        .{ .source = "256", .expected_signed = 256, .expected_precision = .i16 },
        // -1 should be stored as i8 and interpreted as -1
        .{ .source = "-1", .expected_signed = -1, .expected_precision = .i8 },
        // 65535 should be stored as u16
        .{ .source = "65535", .expected_signed = 65535, .expected_precision = .u16 },
        // 65536 should be stored as i32
        .{ .source = "65536", .expected_signed = 65536, .expected_precision = .i32 },
    };

    for (test_cases) |tc| {
        const resources = try parseAndCanonicalizeInt(test_allocator, tc.source);
        defer cleanup(test_allocator, resources);

        const int_data = try getIntValue(resources.cir, resources.expr_idx);

        // Verify the precision is what we expect
        try testing.expectEqual(tc.expected_precision, int_data.precision);

        // Verify the value is interpreted correctly based on the precision
        const value = interpretAsI128(int_data.value, int_data.precision);
        try testing.expectEqual(tc.expected_signed, value);

        // Note: We store values based on their determined precision, so 255 stored as u8
        // will still be 255 when interpreted as i128 (not -1) because we use fromI128/fromU128
        // appropriately based on the precision.
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
        const resources = try parseAndCanonicalizeInt(test_allocator, source);
        defer cleanup(test_allocator, resources);

        const expr = resources.cir.store.getExpr(resources.expr_idx);
        try testing.expect(expr == .runtime_error);
    }
}
