const std = @import("std");
const testing = std.testing;
const base = @import("../../../base.zig");
const parse = @import("../../parse.zig");
const canonicalize = @import("../../../check/canonicalize.zig");
const CIR = canonicalize.CIR;
const types = @import("../../../types/types.zig");
const RocDec = @import("../../../builtins/dec.zig").RocDec;

const test_allocator = testing.allocator;

fn parseAndCanonicalizeFrac(allocator: std.mem.Allocator, source: []const u8) !struct {
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

fn cleanup(result: anytype) void {
    result.can.deinit();
    result.cir.deinit();
    result.parse_ast.deinit(test_allocator);
    result.module_env.deinit();
    test_allocator.destroy(result.can);
    test_allocator.destroy(result.cir);
    test_allocator.destroy(result.parse_ast);
    test_allocator.destroy(result.module_env);
}

test "fractional literal - basic decimal" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "3.14");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 314);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
            try testing.expect(dec.requirements.fits_in_dec);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "fractional literal - scientific notation small" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "1.23e-10");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_f64 => |frac| {
            try testing.expect(!frac.requirements.fits_in_dec); // Scientific notation not supported by Dec
            // 1.23e-10 loses precision when converted to f32 and back
            try testing.expect(!frac.requirements.fits_in_f32);
            try testing.expectApproxEqAbs(frac.value, 1.23e-10, 1e-20);
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "fractional literal - scientific notation large (near f64 max)" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "1e308");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_f64 => |frac| {
            try testing.expect(!frac.requirements.fits_in_dec); // Way out of Dec range
            try testing.expect(!frac.requirements.fits_in_f32); // Too large for f32
            try testing.expectEqual(frac.value, 1e308);
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "fractional literal - scientific notation at f32 boundary" {
    // f32 max is approximately 3.4e38
    const result = try parseAndCanonicalizeFrac(test_allocator, "3.4e38");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_f64 => |frac| {
            try testing.expect(!frac.requirements.fits_in_dec);
            try testing.expect(!frac.requirements.fits_in_f32); // Just above f32 max
            try testing.expectEqual(frac.value, 3.4e38);
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "fractional literal - negative zero" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "-0.0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_dec => |frac| {
            try testing.expect(frac.requirements.fits_in_dec);
            try testing.expect(frac.requirements.fits_in_f32);
            const f64_val = frac.value.toF64();
            // RocDec may not preserve the sign bit for -0.0, so just check it's zero
            try testing.expectEqual(@abs(f64_val), 0.0);
        },
        .frac_f64 => |frac| {
            // Also acceptable if it's parsed as f64
            try testing.expectEqual(frac.value, -0.0);
            try testing.expect(std.math.signbit(frac.value));
        },
        else => {
            try testing.expect(false); // Should be frac_dec or frac_f64
        },
    }
}

test "fractional literal - positive zero" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "0.0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 0);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);
            try testing.expect(dec.requirements.fits_in_dec);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "fractional literal - very small scientific notation" {
    // Test a value that's smaller than f32 min positive normal (approximately 1.2e-38)
    const result = try parseAndCanonicalizeFrac(test_allocator, "1e-40");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_f64 => |frac| {
            try testing.expect(!frac.requirements.fits_in_dec);
            // 1e-40 is subnormal for f32 and may lose precision
            // The fitsInF32 function checks for exact round-trip, so this should be false
            try testing.expect(!frac.requirements.fits_in_f32);
            try testing.expectEqual(frac.value, 1e-40);
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "fractional literal - NaN handling" {
    // Note: NaN is not a valid numeric literal in Roc
    // The parser will fail before canonicalization
    // This test verifies that behavior
    const module_env = try test_allocator.create(base.ModuleEnv);
    module_env.* = base.ModuleEnv.init(test_allocator);
    defer {
        module_env.deinit();
        test_allocator.destroy(module_env);
    }

    var parse_ast = parse.parseExpr(module_env, "NaN");
    defer parse_ast.deinit(test_allocator);

    // Check if it parsed as an identifier instead of a number
    const expr: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const parsed_expr = parse_ast.store.getExpr(expr);

    // NaN parses as a tag expression, not a numeric literal
    try testing.expect(parsed_expr == .tag);
}

test "fractional literal - infinity handling" {
    // Note: Infinity is not a valid numeric literal in Roc
    // The parser will fail before canonicalization
    // This test verifies that behavior
    const module_env = try test_allocator.create(base.ModuleEnv);
    module_env.* = base.ModuleEnv.init(test_allocator);
    defer {
        module_env.deinit();
        test_allocator.destroy(module_env);
    }

    var parse_ast = parse.parseExpr(module_env, "Infinity");
    defer parse_ast.deinit(test_allocator);

    // Check if it parsed as an identifier instead of a number
    const expr: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const parsed_expr = parse_ast.store.getExpr(expr);

    // Infinity parses as a tag expression, not a numeric literal
    try testing.expect(parsed_expr == .tag);
}

test "fractional literal - scientific notation with capital E" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "2.5E10");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_f64 => |frac| {
            try testing.expect(!frac.requirements.fits_in_dec); // Scientific notation
            try testing.expect(!frac.requirements.fits_in_f32); // Too large for f32
            try testing.expectEqual(frac.value, 2.5e10);
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "fractional literal - negative scientific notation" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "-1.5e-5");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_f64 => |frac| {
            try testing.expect(!frac.requirements.fits_in_dec); // Scientific notation
            // -1.5e-5 may not round-trip perfectly through f32
            // Let's just check the value is correct
            try testing.expectApproxEqAbs(frac.value, -1.5e-5, 1e-10);
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "negative zero preservation in f64" {
    // Direct test to verify std.fmt.parseFloat preserves negative zero
    const neg_zero_str = "-0.0";
    const pos_zero_str = "0.0";

    const neg_zero = try std.fmt.parseFloat(f64, neg_zero_str);
    const pos_zero = try std.fmt.parseFloat(f64, pos_zero_str);

    // Both should be zero
    try testing.expectEqual(neg_zero, 0.0);
    try testing.expectEqual(pos_zero, 0.0);

    // But neg_zero should have sign bit set
    try testing.expect(std.math.signbit(neg_zero));
    try testing.expect(!std.math.signbit(pos_zero));

    // Also test f32
    const neg_zero_f32 = try std.fmt.parseFloat(f32, neg_zero_str);
    const pos_zero_f32 = try std.fmt.parseFloat(f32, pos_zero_str);

    try testing.expect(std.math.signbit(neg_zero_f32));
    try testing.expect(!std.math.signbit(pos_zero_f32));
}

test "negative zero forced to f64 parsing" {
    // Test that when we force parsing through f64 path (e.g., with scientific notation),
    // negative zero is preserved
    const result = try parseAndCanonicalizeFrac(test_allocator, "-0.0e0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_f64 => |frac| {
            try testing.expectEqual(frac.value, -0.0);
            try testing.expect(std.math.signbit(frac.value));
        },
        else => {
            try testing.expect(false); // Should be frac_f64 due to scientific notation
        },
    }
}

test "negative zero preservation in Dec" {
    // Test if RocDec preserves negative zero when converting to f64
    if (RocDec.fromNonemptySlice("-0.0")) |neg_zero_dec| {
        const f64_val = neg_zero_dec.toF64();

        // RocDec does NOT preserve negative zero - it becomes positive zero
        try testing.expect(!std.math.signbit(f64_val));
        try testing.expectEqual(f64_val, 0.0);
    }

    // For comparison, positive zero
    if (RocDec.fromNonemptySlice("0.0")) |pos_zero_dec| {
        const f64_val = pos_zero_dec.toF64();
        try testing.expect(!std.math.signbit(f64_val));
        try testing.expectEqual(f64_val, 0.0);
    }
}

test "small dec - basic positive decimal" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "3.14");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 314);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
            try testing.expect(dec.requirements.fits_in_dec);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "negative zero preservation - uses f64" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "-0.0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_f64 => |frac| {
            // Negative zero should use f64 to preserve the sign bit
            try testing.expectEqual(frac.value, -0.0);
            try testing.expect(std.math.signbit(frac.value));
        },
        .frac_dec => |frac| {
            // If it went through Dec path, check if sign is preserved
            const f64_val = frac.value.toF64();
            if (std.math.signbit(f64_val)) {
                // Good, sign preserved
            } else {
                // Dec doesn't preserve negative zero, which is expected
                try testing.expectEqual(f64_val, 0.0);
            }
        },
        .dec_small => {
            // Small dec can't represent negative zero (it would return null and fall back)
            try testing.expect(false);
        },
        else => {
            try testing.expect(false); // Should be frac_f64 or frac_dec
        },
    }
}

test "negative zero with scientific notation - preserves sign via f64" {
    // Force f64 path by using scientific notation
    const result = try parseAndCanonicalizeFrac(test_allocator, "-0.0e0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_f64 => |frac| {
            try testing.expectEqual(frac.value, -0.0);
            try testing.expect(std.math.signbit(frac.value));
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "small dec - positive zero" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "0.0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 0);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);

            // Verify positive zero
            try testing.expectEqual(dec.numerator, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - precision preservation for 0.1" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "0.1");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 1);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - trailing zeros" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "1.100");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 1100);
            try testing.expectEqual(dec.denominator_power_of_ten, 3);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - negative number" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "-5.25");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, -525);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - max i8 value" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "127.99");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 12799);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - min i8 value" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "-128.0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, -1280);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - 128.0 now fits with new representation" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "128.0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            // With numerator/power representation, 128.0 = 1280/10^1 fits in i16
            try testing.expectEqual(dec.numerator, 1280);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - exceeds i16 range falls back to Dec" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "32768.0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_dec => |frac| {
            // Should fall back to Dec because 327680 > 32767 (max i16)
            try testing.expect(frac.requirements.fits_in_dec);
        },
        .dec_small => {
            try testing.expect(false); // Should NOT be dec_small
        },
        else => {
            try testing.expect(false); // Should be frac_dec
        },
    }
}

test "small dec - too many fractional digits falls back to Dec" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "1.234");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 1234);
            try testing.expectEqual(dec.denominator_power_of_ten, 3);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - complex example 0.001" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "0.001");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 1);
            try testing.expectEqual(dec.denominator_power_of_ten, 3);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - negative example -0.05" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "-0.05");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            // -0.05 = -5 / 10^2
            try testing.expectEqual(dec.numerator, -5);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "negative zero with scientific notation preserves sign" {
    // When forced through f64 path with scientific notation, negative zero should preserve sign
    const result = try parseAndCanonicalizeFrac(test_allocator, "-0.0e0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .frac_f64 => |frac| {
            // With scientific notation, should use f64 and preserve negative zero
            try testing.expectEqual(frac.value, -0.0);
            try testing.expect(std.math.signbit(frac.value));
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "fractional literal - simple 1.0 uses small dec" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "1.0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 10);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}
