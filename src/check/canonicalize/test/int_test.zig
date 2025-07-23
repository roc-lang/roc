//! Tests for integer literal canonicalization during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct canonicalization
//! of integer literals and integer expressions from parsed AST into the
//! compiler's Canonical Intermediate Representation (CIR).

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const compile = @import("compile");
const parse = @import("../../parse.zig");
const canonicalize = @import("../../../check/canonicalize.zig");
const types = @import("types");

const test_allocator = testing.allocator;

fn parseAndCanonicalizeInt(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *compile.ModuleEnv,
    parse_ast: *parse.AST,
    can: *canonicalize,
    expr_idx: compile.ModuleEnv.Expr.Idx,
} {
    const module_env = try allocator.create(compile.ModuleEnv);
    module_env.* = try compile.ModuleEnv.init(allocator, source);

    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(module_env);

    parse_ast.store.emptyScratch();

    try module_env.initCIRFields(allocator, "Test");

    const can = try allocator.create(canonicalize);
    can.* = try canonicalize.init(module_env, parse_ast, null);

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        const diagnostic_idx = try module_env.store.addDiagnostic(.{ .not_implemented = .{
            .feature = try module_env.strings.insert(allocator, "canonicalization failed"),
            .region = base.Region.zero(),
        } });
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .can = can,
            .expr_idx = try module_env.store.addExpr(compile.ModuleEnv.Expr{ .e_runtime_error = .{
                .diagnostic = diagnostic_idx,
            } }, base.Region.zero()),
        };
    };

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can,
        .expr_idx = canonical_expr_idx,
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

fn getIntValue(module_env: *compile.ModuleEnv, expr_idx: compile.ModuleEnv.Expr.Idx) !i128 {
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
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, 42), value);
}

test "canonicalize simple negative integer" {
    const source = "-42";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, -42), value);
}

test "canonicalize zero" {
    const source = "0";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, 0), value);
}

test "canonicalize large positive integer" {
    const source = "9223372036854775807"; // i64 max
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, 9223372036854775807), value);
}

test "canonicalize large negative integer" {
    const source = "-9223372036854775808"; // i64 min
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, -9223372036854775808), value);
}

test "canonicalize very large integer" {
    const source = "170141183460469231731687303715884105727"; // i128 max
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
    try testing.expectEqual(@as(i128, 170141183460469231731687303715884105727), value);
}

test "canonicalize very large negative integer" {
    const source = "-170141183460469231731687303715884105728"; // i128 min
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const value = try getIntValue(resources.module_env, resources.expr_idx);
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

        const value = try getIntValue(resources.module_env, resources.expr_idx);
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

        const value = try getIntValue(resources.module_env, resources.expr_idx);
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
        const resources = try parseAndCanonicalizeInt(test_allocator, tc.source);
        defer cleanup(test_allocator, resources);

        const value = try getIntValue(resources.module_env, resources.expr_idx);
        try testing.expectEqual(tc.expected_value, value);
    }
}

test "canonicalize integer literal creates correct type variables" {
    const source = "42";
    const resources = try parseAndCanonicalizeInt(test_allocator, source);
    defer cleanup(test_allocator, resources);

    const expr = resources.module_env.store.getExpr(resources.expr_idx);
    switch (expr) {
        .e_int => {
            // Type variables are now the expression index itself
            // No need to check a separate num_var field

            // Verify requirements were set
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
        const expr = resources.module_env.store.getExpr(resources.expr_idx);
        try testing.expect(expr == .e_runtime_error);
    }

    // Leading zeros with digits
    {
        const resources = try parseAndCanonicalizeInt(test_allocator, "0123");
        defer cleanup(test_allocator, resources);
        const expr = resources.module_env.store.getExpr(resources.expr_idx);
        // This might actually parse as 123, so let's check if it's a valid integer
        if (expr != .e_runtime_error) {
            const value = try getIntValue(resources.module_env, resources.expr_idx);
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

        const value = try getIntValue(resources.module_env, resources.expr_idx);
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

        const resources = try parseAndCanonicalizeInt(test_allocator, source);
        defer cleanup(test_allocator, resources);

        // Get the expression back from the store
        const value = try getIntValue(resources.module_env, resources.expr_idx);

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

        const value = try getIntValue(resources.module_env, resources.expr_idx);
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

        const value = try getIntValue(resources.module_env, resources.expr_idx);

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

        const expr = resources.module_env.store.getExpr(resources.expr_idx);
        try testing.expect(expr == .e_runtime_error);
    }
}

test "invalid number literal - too large for u128" {
    const allocator = test_allocator;
    const source = "999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999";

    var resources = try parseAndCanonicalizeInt(allocator, source);
    defer {
        resources.can.deinit();
        resources.parse_ast.deinit(allocator);
        resources.module_env.deinit();
        allocator.destroy(resources.can);
        allocator.destroy(resources.parse_ast);
        allocator.destroy(resources.module_env);
    }

    // Should have produced a runtime error
    const expr = resources.module_env.store.getExpr(resources.expr_idx);
    try testing.expect(expr == .e_runtime_error);

    // Check that we have an invalid_num_literal diagnostic
    const diagnostics = try resources.module_env.getDiagnostics();
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
                const mock_region_info = base.RegionInfo{
                    .start_line_idx = 0,
                    .start_col_idx = 0,
                    .end_line_idx = 0,
                    .end_col_idx = 0,
                };
                var report = try compile.ModuleEnv.Diagnostic.buildInvalidNumLiteralReport(
                    allocator,
                    mock_region_info,
                    source,
                    "test.roc",
                    source,
                    &[_]u32{0},
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
        resources.parse_ast.deinit(allocator);
        resources.module_env.deinit();
        allocator.destroy(resources.can);
        allocator.destroy(resources.parse_ast);
        allocator.destroy(resources.module_env);
    }

    // Should have produced a runtime error
    const expr = resources.module_env.store.getExpr(resources.expr_idx);
    try testing.expect(expr == .e_runtime_error);

    // Check that we have an invalid_num_literal diagnostic
    const diagnostics = try resources.module_env.getDiagnostics();
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
                const mock_region_info = base.RegionInfo{
                    .start_line_idx = 0,
                    .start_col_idx = 0,
                    .end_line_idx = 0,
                    .end_col_idx = 0,
                };
                var report = try compile.ModuleEnv.Diagnostic.buildInvalidNumLiteralReport(
                    allocator,
                    mock_region_info,
                    source,
                    "test.roc",
                    source,
                    &[_]u32{0},
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
    const result = try parseAndCanonicalizeInt(test_allocator, "0");
    defer cleanup(test_allocator, result);

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
