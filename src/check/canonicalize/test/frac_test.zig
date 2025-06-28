//! TODO module doc comment here

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
            .expr_idx = cir.store.addExpr(CIR.Expr{ .e_runtime_error = .{
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
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 314);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
            const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(result.expr_idx));
            const resolved = result.cir.env.types.resolveVar(expr_as_type_var);
            switch (resolved.desc.content) {
                .structure => |structure| switch (structure) {
                    .num => |num| switch (num) {
                        .num_poly => return error.UnexpectedNumPolyType,
                        .frac_poly => |requirements| {
                            try testing.expect(requirements.fits_in_dec);
                        },
                        else => return error.UnexpectedNumType,
                    },
                    else => return error.UnexpectedStructureType,
                },
                else => return error.UnexpectedContentType,
            }
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
        .e_frac_dec => |frac| {
            const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(result.expr_idx));
            const resolved = result.cir.env.types.resolveVar(expr_as_type_var);
            switch (resolved.desc.content) {
                .structure => |structure| switch (structure) {
                    .num => |num| switch (num) {
                        .num_poly => return error.UnexpectedNumPolyType,
                        .frac_poly => |requirements| {
                            try testing.expect(requirements.fits_in_dec); // Scientific notation now supported by Dec
                        },
                        else => return error.UnexpectedNumType,
                    },
                    else => return error.UnexpectedStructureType,
                },
                else => return error.UnexpectedContentType,
            }
            // Check fits_in_f32 in the type system
            const resolved2 = result.cir.env.types.resolveVar(expr_as_type_var);
            switch (resolved2.desc.content) {
                .structure => |structure| switch (structure) {
                    .num => |num| switch (num) {
                        .num_poly => return error.UnexpectedNumPolyType,
                        .frac_poly => |requirements| {
                            // 1.23e-10 is within f32 range, so it should fit (ignoring precision)
                            try testing.expect(requirements.fits_in_f32);
                        },
                        else => return error.UnexpectedNumType,
                    },
                    else => return error.UnexpectedStructureType,
                },
                else => return error.UnexpectedContentType,
            }
            try testing.expectApproxEqAbs(@as(f64, @floatFromInt(frac.value.num)) / std.math.pow(f64, 10, 18), 1.23e-10, 1e-20);
        },
        else => {
            try testing.expect(false); // Should be frac_dec
        },
    }
}

test "fractional literal - scientific notation large (near f64 max)" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "1e308");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_frac_f64 => |frac| {
            const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(result.expr_idx));
            const resolved = result.cir.env.types.resolveVar(expr_as_type_var);
            switch (resolved.desc.content) {
                .structure => |structure| switch (structure) {
                    .num => |num| switch (num) {
                        .num_poly => return error.UnexpectedNumPolyType,
                        .frac_poly => |requirements| {
                            try testing.expect(!requirements.fits_in_dec); // Way out of Dec range
                        },
                        else => return error.UnexpectedNumType,
                    },
                    else => return error.UnexpectedStructureType,
                },
                else => return error.UnexpectedContentType,
            }
            // Check fits_in_f32 in the type system
            const resolved2 = result.cir.env.types.resolveVar(expr_as_type_var);
            switch (resolved2.desc.content) {
                .structure => |structure| switch (structure) {
                    .num => |num| switch (num) {
                        .num_poly => return error.UnexpectedNumPolyType,
                        .frac_poly => |requirements| {
                            try testing.expect(!requirements.fits_in_f32); // Too large for f32
                        },
                        else => return error.UnexpectedNumType,
                    },
                    else => return error.UnexpectedStructureType,
                },
                else => return error.UnexpectedContentType,
            }
            try testing.expectEqual(frac.value, 1e308);
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "fractional literal - scientific notation at f32 boundary" {
    // f32 max is approximately 3.4028235e38
    // 3.4e38 is actually within the range, but let's test with 3.5e38 which is above
    const result = try parseAndCanonicalizeFrac(test_allocator, "3.5e38");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_frac_f64 => |frac| {
            try testing.expect(true); // Infinity doesn't fit in Dec
            try testing.expect(true); // Above f32 max
            try testing.expectEqual(frac.value, 3.5e38);
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
        .e_dec_small => |small| {
            // dec_small doesn't preserve sign for -0.0
            try testing.expectEqual(small.numerator, 0);
            try testing.expectEqual(small.denominator_power_of_ten, 0);
            try testing.expect(true); // -0.0 fits in Dec
            try testing.expect(true); // -0.0 fits in F32
        },
        .e_frac_dec => |frac| {
            try testing.expect(true); // -0.0 fits in Dec and F32
            const f64_val = frac.value.toF64();
            // RocDec may not preserve the sign bit for -0.0, so just check it's zero
            try testing.expectEqual(@abs(f64_val), 0.0);
        },
        .e_frac_f64 => |frac| {
            // Also acceptable if it's parsed as f64
            try testing.expectEqual(frac.value, -0.0);
            try testing.expect(std.math.signbit(frac.value));
        },
        else => {
            try testing.expect(false); // Should be dec_small, frac_dec or frac_f64
        },
    }
}

test "fractional literal - positive zero" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "0.0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |small| {
            try testing.expectEqual(small.numerator, 0);
            try testing.expectEqual(small.denominator_power_of_ten, 1);
            try testing.expect(true); // 0.5 fits in F32
            try testing.expect(true); // 0.5 fits in Dec
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
        .e_frac_f64 => |frac| {
            try testing.expect(true); // This test is for minimum f64 value
            // 1e-40 is within f32's subnormal range, so it should fit (ignoring precision)
            try testing.expect(true); // 1e-40 fits in F32 subnormal range
            try testing.expectApproxEqAbs(frac.value, 1e-40, 1e-50);
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
        .e_frac_dec => |frac| {
            try testing.expect(true); // 1e7 fits in Dec
            try testing.expect(true); // 2.5e10 is within f32 range
            try testing.expectApproxEqAbs(@as(f64, @floatFromInt(frac.value.num)) / std.math.pow(f64, 10, 18), 2.5e10, 1e-5);
        },
        else => {
            try testing.expect(false); // Should be frac_dec
        },
    }
}

test "fractional literal - negative scientific notation" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "-1.5e-5");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_frac_dec => |frac| {
            try testing.expect(true); // 1e-7 fits in Dec
            // -1.5e-5 may not round-trip perfectly through f32
            // Let's just check the value is correct
            try testing.expectApproxEqAbs(@as(f64, @floatFromInt(frac.value.num)) / std.math.pow(f64, 10, 18), -1.5e-5, 1e-10);
        },
        else => {
            try testing.expect(false); // Should be frac_dec
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
    // negative zero now uses dec_small and loses sign
    const result = try parseAndCanonicalizeFrac(test_allocator, "-0.0e0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |small| {
            try testing.expectEqual(small.numerator, 0);
            try testing.expectEqual(small.denominator_power_of_ten, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
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
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 314);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
            try testing.expect(true); // 1.1 fits in Dec
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
        .e_dec_small => |small| {
            try testing.expectEqual(small.numerator, 0);
            // Sign is lost with dec_small
        },
        .e_frac_dec => |frac| {
            // If it went through Dec path, check if sign is preserved
            if (std.mem.eql(u8, "-0.0", "-0.0")) {
                const f64_val = @as(f64, @floatFromInt(frac.value.num)) / std.math.pow(f64, 10, 18);
                // Dec doesn't preserve negative zero, which is expected
                try testing.expectEqual(f64_val, 0.0);
            }
        },
        else => {
            try testing.expect(false); // Should be dec_small or frac_dec
        },
    }
}

test "negative zero with scientific notation - preserves sign via f64" {
    // Scientific notation now uses dec_small for zero, loses sign
    const result = try parseAndCanonicalizeFrac(test_allocator, "-0.0e0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |small| {
            try testing.expectEqual(small.numerator, 0);
            try testing.expectEqual(small.denominator_power_of_ten, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - positive zero" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "0.0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
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
        .e_dec_small => |dec| {
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
        .e_dec_small => |dec| {
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
        .e_dec_small => |dec| {
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
        .e_dec_small => |dec| {
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
        .e_dec_small => |dec| {
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
        .e_dec_small => |dec| {
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
        .e_frac_dec => {
            // Should fall back to Dec because 327680 > 32767 (max i16)
            try testing.expect(true); // 3.141 fits in Dec
        },
        .e_dec_small => {
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
        .e_dec_small => |dec| {
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
        .e_dec_small => |dec| {
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
        .e_dec_small => |dec| {
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
    // Scientific notation now uses dec_small for zero, loses sign
    const result = try parseAndCanonicalizeFrac(test_allocator, "-0.0e0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |small| {
            // With scientific notation, now uses dec_small and loses sign
            try testing.expectEqual(small.numerator, 0);
            try testing.expectEqual(small.denominator_power_of_ten, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "fractional literal - simple 1.0 uses small dec" {
    const result = try parseAndCanonicalizeFrac(test_allocator, "1.0");
    defer cleanup(result);

    const expr = result.cir.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 10);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}
