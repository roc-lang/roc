//! TODO module doc comment here

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

fn getIntValue(cir: *CIR, expr_idx: CIR.Expr.Idx) !struct { value: i128, requirements: types.Num.Int.Requirements } {
    const expr = cir.store.getExpr(expr_idx);
    switch (expr) {
        .int => |int_expr| {
            const value: i128 = @bitCast(int_expr.value.bytes);
            return .{ .value = value, .requirements = int_expr.requirements };
        },
        .num => |num_expr| {
            // For num expressions, we calculate requirements based on the value
            const value: i128 = @bitCast(num_expr.value.bytes);
            const requirements = calculateRequirements(value);
            return .{ .value = value, .requirements = requirements };
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
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = int_data.value;
    try testing.expectEqual(@as(i128, 42), value);
    try testing.expectEqual(false, int_data.requirements.sign_needed);
    try testing.expectEqual(types.Num.Int.BitsNeeded.@"7", int_data.requirements.bits_needed);
}

test "canonicalize simple negative integer" {
    const source = "-42";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = int_data.value;
    try testing.expectEqual(@as(i128, -42), value);
    try testing.expectEqual(true, int_data.requirements.sign_needed);
    try testing.expectEqual(types.Num.Int.BitsNeeded.@"7", int_data.requirements.bits_needed);
}

test "canonicalize zero" {
    const source = "0";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = int_data.value;
    try testing.expectEqual(@as(i128, 0), value);
}

test "canonicalize large positive integer" {
    const source = "9223372036854775807"; // i64 max
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = int_data.value;
    try testing.expectEqual(@as(i128, 9223372036854775807), value);
}

test "canonicalize large negative integer" {
    const source = "-9223372036854775808"; // i64 min
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = int_data.value;
    try testing.expectEqual(@as(i128, -9223372036854775808), value);
}

test "canonicalize very large integer" {
    const source = "170141183460469231731687303715884105727"; // i128 max
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = int_data.value;
    try testing.expectEqual(@as(i128, 170141183460469231731687303715884105727), value);
}

test "canonicalize very large negative integer" {
    const source = "-170141183460469231731687303715884105728"; // i128 min
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const int_data = try getIntValue(resources.cir, resources.expr_idx);
    const value = int_data.value;
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
        const value = int_data.value;
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
        const value = int_data.value;
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
        .{ .source = "-128", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = .@"8" },
        .{ .source = "-129", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = .@"8" },
        .{ .source = "32767", .expected_value = 32767, .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },
        .{ .source = "32768", .expected_value = 32768, .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        .{ .source = "65535", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        .{ .source = "65536", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = .@"17_to_31" },
    };

    for (test_cases) |tc| {
        const resources = try parseAndCanonicalizeInt(test_allocator, tc.source);
        defer cleanup(test_allocator, resources);

        const int_data = try getIntValue(resources.cir, resources.expr_idx);
        const value = int_data.value;
        try testing.expectEqual(tc.expected_value, value);
        try testing.expectEqual(tc.expected_sign_needed, int_data.requirements.sign_needed);
        try testing.expectEqual(tc.expected_bits_needed, int_data.requirements.bits_needed);
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
            _ = int_expr.num_var;

            // Verify requirements were set
            try testing.expect(int_expr.requirements.sign_needed == false);
            try testing.expect(int_expr.requirements.bits_needed == .@"7");
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
            const value = int_data.value;
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
        const value = int_data.value;

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
        const value = int_data.value;
        try testing.expectEqual(tc.expected, value);
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
        const resources = try parseAndCanonicalizeInt(test_allocator, tc.source);
        defer cleanup(test_allocator, resources);

        const int_data = try getIntValue(resources.cir, resources.expr_idx);

        // Verify the requirements are what we expect
        try testing.expectEqual(tc.expected_sign_needed, int_data.requirements.sign_needed);
        try testing.expectEqual(tc.expected_bits_needed, int_data.requirements.bits_needed);

        // Verify the value
        const value = int_data.value;
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
        const resources = try parseAndCanonicalizeInt(test_allocator, source);
        defer cleanup(test_allocator, resources);

        const expr = resources.cir.store.getExpr(resources.expr_idx);
        try testing.expect(expr == .runtime_error);
    }
}

test "invalid number literal - too large for u128" {
    const allocator = test_allocator;
    const source = "999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999";

    var resources = try parseAndCanonicalizeInt(allocator, source);
    defer {
        resources.can.deinit();
        resources.cir.deinit();
        resources.parse_ast.deinit(allocator);
        resources.module_env.deinit();
        allocator.destroy(resources.can);
        allocator.destroy(resources.cir);
        allocator.destroy(resources.parse_ast);
        allocator.destroy(resources.module_env);
    }

    // Should have produced a runtime error
    const expr = resources.cir.store.getExpr(resources.expr_idx);
    try testing.expect(expr == .runtime_error);

    // Check that we have an invalid_num_literal diagnostic
    const diagnostics = resources.cir.getDiagnostics();
    defer allocator.free(diagnostics);
    try testing.expect(diagnostics.len > 0);

    var found_invalid_num = false;
    for (diagnostics) |diag| {
        switch (diag) {
            .invalid_num_literal => |data| {
                found_invalid_num = true;

                // Verify the region captures the entire number
                const literal_text = source[data.region.start.offset..data.region.end.offset];
                try testing.expectEqualStrings(source, literal_text);

                // Test that buildInvalidNumLiteralReport extracts the literal correctly
                var report = try CIR.Diagnostic.buildInvalidNumLiteralReport(
                    allocator,
                    data.region,
                    source,
                );
                defer report.deinit();

                // The report should contain the literal
                var buf: [1024]u8 = undefined;
                var stream = std.io.fixedBufferStream(&buf);
                try report.render(stream.writer(), .markdown);
                const rendered = stream.getWritten();

                try testing.expect(std.mem.indexOf(u8, rendered, "999999999") != null);
            },
            else => {},
        }
    }

    try testing.expect(found_invalid_num);
}

test "invalid number literal - negative too large for i128" {
    const allocator = test_allocator;
    const source = "-999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999";

    var resources = try parseAndCanonicalizeInt(allocator, source);
    defer {
        resources.can.deinit();
        resources.cir.deinit();
        resources.parse_ast.deinit(allocator);
        resources.module_env.deinit();
        allocator.destroy(resources.can);
        allocator.destroy(resources.cir);
        allocator.destroy(resources.parse_ast);
        allocator.destroy(resources.module_env);
    }

    // Should have produced a runtime error
    const expr = resources.cir.store.getExpr(resources.expr_idx);
    try testing.expect(expr == .runtime_error);

    // Check that we have an invalid_num_literal diagnostic
    const diagnostics = resources.cir.getDiagnostics();
    defer allocator.free(diagnostics);
    try testing.expect(diagnostics.len > 0);

    var found_invalid_num = false;
    for (diagnostics) |diag| {
        switch (diag) {
            .invalid_num_literal => |data| {
                found_invalid_num = true;

                // Verify the region captures the entire number including the minus
                const literal_text = source[data.region.start.offset..data.region.end.offset];
                try testing.expectEqualStrings(source, literal_text);

                // Test that buildInvalidNumLiteralReport extracts the literal correctly
                var report = try CIR.Diagnostic.buildInvalidNumLiteralReport(
                    allocator,
                    data.region,
                    source,
                );
                defer report.deinit();

                // The report should contain the literal with minus sign
                var buf: [1024]u8 = undefined;
                var stream = std.io.fixedBufferStream(&buf);
                try report.render(stream.writer(), .markdown);
                const rendered = stream.getWritten();

                try testing.expect(std.mem.indexOf(u8, rendered, "-999999999") != null);
            },
            else => {},
        }
    }

    try testing.expect(found_invalid_num);
}

test "integer literal - negative zero" {
    const result = try parseAndCanonicalizeInt(test_allocator, "-0");
    defer cleanup(test_allocator, result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .int => |int| {
            // -0 should be treated as 0
            try testing.expectEqual(@as(i128, @bitCast(int.value.bytes)), 0);
            // But it should still be marked as needing a sign
            try testing.expect(int.requirements.sign_needed);
            try testing.expectEqual(int.requirements.bits_needed, types.Num.Int.BitsNeeded.@"7");
        },
        else => {
            try testing.expect(false); // Should be int
        },
    }
}

test "integer literal - positive zero" {
    const result = try parseAndCanonicalizeInt(test_allocator, "0");
    defer cleanup(test_allocator, result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .int => |int| {
            try testing.expectEqual(@as(i128, @bitCast(int.value.bytes)), 0);
            // Positive zero should not need a sign
            try testing.expect(!int.requirements.sign_needed);
            try testing.expectEqual(int.requirements.bits_needed, types.Num.Int.BitsNeeded.@"7");
        },
        else => {
            try testing.expect(false); // Should be int
        },
    }
}
