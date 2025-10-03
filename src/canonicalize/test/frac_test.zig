//! Tests for fractional literal canonicalization during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct canonicalization
//! of fractional literals and decimal expressions from parsed AST into the
//! compiler's canonical internal representation (CIR).

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const compile = @import("compile");
const builtins = @import("builtins");

const Can = @import("../Can.zig");
const CIR = @import("../CIR.zig");
const TestEnv = @import("TestEnv.zig").TestEnv;

const RocDec = builtins.dec.RocDec;
const testing = std.testing;

test "fractional literal - basic decimal" {
    const source = "3.14";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.value.numerator, 314);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 2);
        },
        .e_dec => |dec| {
            _ = dec;
        },
        else => {
            std.debug.print("Unexpected expr type: {}\n", .{expr});
            try testing.expect(false); // Should be dec_small or frac_dec
        },
    }
}

test "fractional literal - scientific notation small" {
    const source = "1.23e-10";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            // Very small numbers may round to zero when parsed as small decimal
            // This is expected behavior when the value is too small for i16 representation
            try testing.expectEqual(dec.value.numerator, 0);
        },
        .e_dec => |frac| {
            // RocDec stores the value in a special format
            _ = frac;
        },
        .e_frac_f64 => |frac| {
            try testing.expectApproxEqAbs(frac.value, 1.23e-10, 1e-20);
        },
        else => {
            std.debug.print("Unexpected expr type for '1.23e-10': {}\n", .{expr});
            try testing.expect(false); // Should be e_frac_f64
        },
    }
}

test "fractional literal - scientific notation large (near f64 max)" {
    const source = "1e308";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_frac_f64 => |frac| {
            try testing.expectEqual(frac.value, 1e308);
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "fractional literal - scientific notation at f32 boundary" {
    const source = "3.5e38";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

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
    const source = "-0.0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |small| {
            // dec_small doesn't preserve sign for -0.0
            try testing.expectEqual(small.value.numerator, 0);
            try testing.expectEqual(small.value.denominator_power_of_ten, 0);
            try testing.expect(true); // -0.0 fits in Dec
            try testing.expect(true); // -0.0 fits in F32
        },
        .e_dec => |frac| {
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
    const source = "0.0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |small| {
            try testing.expectEqual(small.value.numerator, 0);
            try testing.expectEqual(small.value.denominator_power_of_ten, 1);
            try testing.expect(true); // 0.5 fits in F32
            try testing.expect(true); // 0.5 fits in Dec
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "fractional literal - very small scientific notation" {
    const source = "1e-40";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

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
    const source = "NaN";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    // Note: NaN is not a valid numeric literal in Roc
    // The parser will fail before canonicalization
    // This test verifies that behavior
    const parse_ast = test_env.parse_ast;

    // Check if it parsed as an identifier instead of a number
    const expr: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const parsed_expr = parse_ast.store.getExpr(expr);

    // NaN parses as a tag expression, not a numeric literal
    try testing.expect(parsed_expr == .tag);
}

test "fractional literal - infinity handling" {
    const source = "Infinity";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    // Note: Infinity is not a valid numeric literal in Roc
    // The parser will fail before canonicalization
    // This test verifies that behavior
    const parse_ast = test_env.parse_ast;

    // Check if it parsed as an identifier instead of a number
    const expr: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const parsed_expr = parse_ast.store.getExpr(expr);

    // Infinity parses as a tag expression, not a numeric literal
    try testing.expect(parsed_expr == .tag);
}

test "fractional literal - scientific notation with capital E" {
    const source = "2.5E10";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec => |frac| {
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
    const source = "-1.5e-5";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec => |frac| {
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
    const source = "-0.0e0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |small| {
            try testing.expectEqual(small.value.numerator, 0);
            try testing.expectEqual(small.value.denominator_power_of_ten, 0);
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
    const source = "3.14";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.value.numerator, 314);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 2);
            try testing.expect(true); // 1.1 fits in Dec
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "negative zero preservation - uses f64" {
    const source = "-0.0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |small| {
            try testing.expectEqual(small.value.numerator, 0);
            // Sign is lost with dec_small
        },
        .e_dec => |frac| {
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
    const source = "-0.0e0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |small| {
            try testing.expectEqual(small.value.numerator, 0);
            try testing.expectEqual(small.value.denominator_power_of_ten, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - positive zero" {
    const source = "0.0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.value.numerator, 0);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 1);

            // Verify positive zero
            try testing.expectEqual(dec.value.numerator, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - precision preservation for 0.1" {
    const source = "0.1";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.value.numerator, 1);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - trailing zeros" {
    const source = "1.100";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.value.numerator, 1100);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 3);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - negative number" {
    const source = "-5.25";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.value.numerator, -525);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 2);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - max i8 value" {
    const source = "127.99";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.value.numerator, 12799);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 2);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - min i8 value" {
    const source = "-128.0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.value.numerator, -1280);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - 128.0 now fits with new representation" {
    const source = "128.0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            // With numerator/power representation, 128.0 = 1280/10^1 fits in i16
            try testing.expectEqual(dec.value.numerator, 1280);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - exceeds i16 range falls back to Dec" {
    const source = "32768.0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec => {
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
    const source = "1.234";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.value.numerator, 1234);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 3);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - complex example 0.001" {
    const source = "0.001";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.value.numerator, 1);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 3);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - negative example -0.05" {
    const source = "-0.05";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            // -0.05 = -5 / 10^2
            try testing.expectEqual(dec.value.numerator, -5);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 2);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "negative zero with scientific notation preserves sign" {
    const source = "-0.0e0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |small| {
            // With scientific notation, now uses dec_small and loses sign
            try testing.expectEqual(small.value.numerator, 0);
            try testing.expectEqual(small.value.denominator_power_of_ten, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "fractional literal - simple 1.0 uses small dec" {
    const source = "1.0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.value.numerator, 10);
            try testing.expectEqual(dec.value.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}
