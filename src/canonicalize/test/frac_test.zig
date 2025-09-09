//! Tests for fractional literal canonicalization during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct canonicalization
//! of fractional literals and decimal expressions from parsed AST into the
//! compiler's canonical internal representation (CIR).

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const compile = @import("compile");
const types = @import("types");
const builtins = @import("builtins");

const Can = @import("../mod.zig").Can;
const CIR = @import("../mod.zig").CIR;
const TestEnv = @import("TestEnv.zig").TestEnv;

const RocDec = builtins.dec.RocDec;
const testing = std.testing;

test "fractional literal - basic decimal" {
    const source = "3.14";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Check that it's a small fraction literal
    try testing.expectEqual(CIR.Expr.Tag.frac_literal_small, expr.tag);
}

test "fractional literal - fits in SmallDec" {
    const source = "0.12345";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Should be stored as a small decimal
    try testing.expectEqual(CIR.Expr.Tag.frac_literal_small, expr.tag);
}

test "fractional literal - scientific notation" {
    const source = "1.23e4";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Scientific notation should parse
    try testing.expect(expr.tag == CIR.Expr.Tag.frac_literal_small or
        expr.tag == CIR.Expr.Tag.frac_literal_big);
}

test "fractional literal - negative exponent" {
    const source = "5.5e-2";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Should handle negative exponents
    try testing.expect(expr.tag == CIR.Expr.Tag.frac_literal_small or
        expr.tag == CIR.Expr.Tag.frac_literal_big);
}

test "fractional literal - zero" {
    const source = "0.0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Zero should be a valid decimal
    try testing.expectEqual(CIR.Expr.Tag.frac_literal_small, expr.tag);
}

test "fractional literal - large precision decimal" {
    const source = "3.141592653589793238462643383279502884197169399375105820974944592307816406286";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Large precision should use big decimal
    try testing.expectEqual(CIR.Expr.Tag.frac_literal_big, expr.tag);
}

test "fractional literal - decimal with leading zero" {
    const source = "0.123";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.frac_literal_small, expr.tag);
}

test "fractional literal - decimal without leading zero" {
    const source = ".456";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.frac_literal_small, expr.tag);
}

test "fractional literal - trailing zeros" {
    const source = "1.2300";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Trailing zeros should be preserved in the literal
    try testing.expectEqual(CIR.Expr.Tag.frac_literal_small, expr.tag);
}

test "fractional literal - very small decimal" {
    const source = "0.000000000001";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expect(expr.tag == CIR.Expr.Tag.frac_literal_small or
        expr.tag == CIR.Expr.Tag.frac_literal_big);
}

test "fractional literal - very large decimal" {
    const source = "999999999999999999999999999999.999999999999999999999999999999";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Very large numbers should use big decimal
    try testing.expectEqual(CIR.Expr.Tag.frac_literal_big, expr.tag);
}

test "fractional literal - negative decimal" {
    const source = "-3.14";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Negative decimals should be handled
    // Note: This might be a unary_neg around a frac_literal
    try testing.expect(expr.tag == CIR.Expr.Tag.unary_neg or
        expr.tag == CIR.Expr.Tag.frac_literal_small);
}

test "fractional literal - NaN handling" {
    const source = "NaN";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    // The parser will fail before canonicalization
    // This test verifies that behavior
    const parse_ast = test_env.parse_ast;

    // Check if it parsed as an identifier instead of a number
    const expr: parse.AST.Node.Idx = @enumFromInt(parse_ast.root_node_idx);
    // NaN parses as a tag expression, not a numeric literal
    try testing.expect(parse_ast.tag(expr) == .uc);
}

test "fractional literal - infinity handling" {
    const source = "Infinity";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    // The parser will fail before canonicalization
    // This test verifies that behavior
    const parse_ast = test_env.parse_ast;

    // Check if it parsed as an identifier instead of a number
    const inf_expr: parse.AST.Node.Idx = @enumFromInt(parse_ast.root_node_idx);
    // Infinity parses as a tag expression, not a numeric literal
    try testing.expect(parse_ast.tag(inf_expr) == .uc);
}

test "fractional literal - scientific notation with capital E" {
    const source = "2.5E10";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Capital E should work same as lowercase e
    try testing.expect(expr.tag == CIR.Expr.Tag.frac_literal_small or
        expr.tag == CIR.Expr.Tag.frac_literal_big);
}

test "fractional literal - multiple decimals in expression" {
    const source = "3.14 + 2.71";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // This should be a binary operation with two decimal operands
    try testing.expectEqual(CIR.Expr.Tag.binop_plus, expr.tag);
}

test "fractional literal - decimal in function call" {
    const source = "Num.abs(3.14)";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // This should be a function call
    try testing.expectEqual(CIR.Expr.Tag.fn_call, expr.tag);
}

test "fractional literal - decimal in list" {
    const source = "[1.1, 2.2, 3.3]";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // This should be a list literal
    try testing.expectEqual(CIR.Expr.Tag.list_literal, expr.tag);
}

test "fractional literal - decimal in record" {
    const source = "{ x: 1.5, y: 2.5 }";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // This should be a record literal
    try testing.expectEqual(CIR.Expr.Tag.record_literal, expr.tag);
}

test "fractional literal - decimal in tuple" {
    const source = "(1.1, 2.2)";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // This should be a tuple literal
    try testing.expectEqual(CIR.Expr.Tag.tuple_literal, expr.tag);
}

test "fractional literal - decimal comparison" {
    const source = "3.14 > 2.71";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // This should be a comparison operation
    try testing.expectEqual(CIR.Expr.Tag.binop_gt, expr.tag);
}

test "fractional literal - decimal pattern match" {
    const source =
        \\when x is
        \\    0.0 -> "zero"
        \\    _ -> "nonzero"
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // This should be a when expression
    try testing.expectEqual(CIR.Expr.Tag.match, expr.tag);
}

test "fractional literal - decimal with underscore separators" {
    const source = "1_234.567_890";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Underscores should be allowed as separators
    try testing.expect(expr.tag == CIR.Expr.Tag.frac_literal_small or
        expr.tag == CIR.Expr.Tag.frac_literal_big);
}

test "fractional literal - zero with exponent" {
    const source = "0e10";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Zero with exponent should still be zero
    try testing.expect(expr.tag == CIR.Expr.Tag.num_literal_i32 or
        expr.tag == CIR.Expr.Tag.frac_literal_small);
}

test "fractional literal - decimal in if expression" {
    const source = "if x > 0.0 then 1.0 else -1.0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // This should be an if expression
    try testing.expectEqual(CIR.Expr.Tag.if_else, expr.tag);
}

test "fractional literal - decimal in lambda" {
    const source = "|x| x + 0.5";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // This should be a lambda expression
    try testing.expectEqual(CIR.Expr.Tag.lambda, expr.tag);
}
