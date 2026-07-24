//! Tests for fractional literal canonicalization during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct canonicalization
//! of fractional literals and decimal expressions from parsed AST into the
//! compiler's canonical internal representation (CIR).

const std = @import("std");
const builtins = @import("builtins");
const parse = @import("parse");

const TestEnv = @import("TestEnv.zig").TestEnv;

const testing = std.testing;
const ModuleEnv = @import("../ModuleEnv.zig");

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
        .e_dec => {},
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
            try testing.expectEqual(@as(i16, 123), dec.value.numerator);
            try testing.expectEqual(@as(u8, 12), dec.value.denominator_power_of_ten);
        },
        .e_dec => {
            // RocDec stores the value in a special format
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
        .e_num_from_numeral => {
            const literal = test_env.module_env.numeralLiteralForNode(ModuleEnv.nodeIdxFrom(canonical_expr.get_idx())) orelse return error.MissingNumeralLiteral;
            try testing.expect(literal.isFractional());
            try testing.expect(!literal.isNegative());
            try testing.expectEqual(@as(u64, 0), literal.after_decimal_digit_count);
        },
        else => {
            try testing.expect(false); // Should be exact from_numeral
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
        .e_num_from_numeral => {
            const literal = test_env.module_env.numeralLiteralForNode(ModuleEnv.nodeIdxFrom(canonical_expr.get_idx())) orelse return error.MissingNumeralLiteral;
            try testing.expect(literal.isFractional());
            try testing.expect(!literal.isNegative());
            try testing.expectEqual(@as(u64, 0), literal.after_decimal_digit_count);
        },
        else => {
            try testing.expect(false); // Should be exact from_numeral
        },
    }
}

test "fractional literal - very small scientific notation" {
    // 1e-40 needs 40 fractional decimal places — beyond Dec's 18 — so it must
    // NOT take a compact dec fast path (which cannot represent it); it keeps
    // its exact digits and converts at a concrete float type, or fails Dec fit
    // validation with a proper error.
    const source = "1e-40";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_num_from_numeral => {
            const literal = test_env.module_env.numeralLiteralForNode(ModuleEnv.nodeIdxFrom(canonical_expr.get_idx())) orelse return error.MissingNumeralLiteral;
            try testing.expectEqual(@as(u32, 40), literal.after_decimal_digit_count);
            try testing.expectEqualSlices(u8, &.{1}, test_env.module_env.numeralDigitsAfter(literal));
            try testing.expectEqualSlices(u8, &.{}, test_env.module_env.numeralDigitsBefore(literal));
        },
        else => {
            try testing.expect(false); // Must keep exact digits, not a compact dec payload
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
            try testing.expectApproxEqAbs(builtins.compiler_rt_128.i128_to_f64(frac.value.num) / std.math.pow(f64, 10, 18), 2.5e10, 1e-5);
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
        .e_dec_small => |dec| {
            try testing.expectEqual(@as(i16, -15), dec.value.numerator);
            try testing.expectEqual(@as(u8, 6), dec.value.denominator_power_of_ten);
        },
        else => {
            try testing.expect(false); // Should be exact small decimal
        },
    }
}

test "negative zero with scientific notation - value is zero, sign recorded on numeral" {
    const source = "-0.0e0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec_small => |small| {
            // The compact dec payload has no sign bit for zero...
            try testing.expectEqual(small.value.numerator, 0);
            try testing.expectEqual(small.value.denominator_power_of_ten, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }

    // ...but the recorded exact numeral keeps the literal's sign.
    const literal = test_env.module_env.numeralLiteralForNode(ModuleEnv.nodeIdxFrom(canonical_expr.get_idx())) orelse return error.MissingNumeralLiteral;
    try testing.expect(literal.isNegative());
    try testing.expect(literal.isFractional());
}

test "small dec - exceeds i16 range falls back to Dec" {
    const source = "32768.0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_dec => {
            // Falls back to Dec because 32768 > 32767 (max i16)
        },
        .e_dec_small => {
            try testing.expect(false); // Should NOT be dec_small
        },
        else => {
            try testing.expect(false); // Should be frac_dec
        },
    }
}
