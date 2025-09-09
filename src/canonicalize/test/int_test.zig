//! Tests for integer literal canonicalization.
//!
//! This module contains unit tests that verify the correct canonicalization
//! of integer expressions from parsed AST into the compiler's canonical
//! internal representation (CIR).

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types = @import("types");
const builtins = @import("builtins");

const Can = @import("../mod.zig").Can;
const CIR = @import("../mod.zig").CIR;
const TestEnv = @import("TestEnv.zig").TestEnv;

test "integer literal - basic positive" {
    const source = "42";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.num_literal_i32, expr.tag);
}

test "integer literal - basic negative" {
    const source = "-42";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Negative numbers might be unary_neg around a num_literal
    try testing.expect(expr.tag == CIR.Expr.Tag.unary_neg or
        expr.tag == CIR.Expr.Tag.num_literal_i32);
}

test "integer literal - zero" {
    const source = "0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.num_literal_i32, expr.tag);
}

test "integer literal - hexadecimal" {
    const source = "0xFF";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.int_literal_i32, expr.tag);
}

test "integer literal - binary" {
    const source = "0b1010";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.int_literal_i32, expr.tag);
}

test "integer literal - octal" {
    const source = "0o777";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.int_literal_i32, expr.tag);
}

test "integer literal - with underscores" {
    const source = "1_234_567";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expect(expr.tag == CIR.Expr.Tag.num_literal_i32 or
        expr.tag == CIR.Expr.Tag.num_literal_big);
}

test "integer literal - large number" {
    const source = "999999999999999999999999999999";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.num_literal_big, expr.tag);
}

test "integer literal - max i32" {
    const source = "2147483647";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.num_literal_i32, expr.tag);
}

test "integer literal - min i32" {
    const source = "-2147483648";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // This might be unary_neg around a literal
    try testing.expect(expr.tag == CIR.Expr.Tag.unary_neg or
        expr.tag == CIR.Expr.Tag.num_literal_i32);
}

test "integer literal - in arithmetic expression" {
    const source = "1 + 2";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.binop_plus, expr.tag);
}

test "integer literal - in comparison" {
    const source = "10 > 5";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.binop_gt, expr.tag);
}

test "integer literal - in list" {
    const source = "[1, 2, 3]";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.list_literal, expr.tag);
}

test "integer literal - in record" {
    const source = "{ x: 100, y: 200 }";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.record_literal, expr.tag);
}

test "integer literal - in tuple" {
    const source = "(1, 2)";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.tuple_literal, expr.tag);
}

test "integer literal - in if expression" {
    const source = "if x > 0 then 1 else -1";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.if_else, expr.tag);
}

test "integer literal - in match expression" {
    const source =
        \\when x is
        \\    0 -> "zero"
        \\    1 -> "one"
        \\    _ -> "other"
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.match, expr.tag);
}

test "integer literal - in lambda" {
    const source = "|x| x + 42";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    try testing.expectEqual(CIR.Expr.Tag.lambda, expr.tag);
}

test "integer literal - scientific notation" {
    const source = "1e6";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Scientific notation might parse as a float
    try testing.expect(expr.tag == CIR.Expr.Tag.frac_literal_small or
        expr.tag == CIR.Expr.Tag.frac_literal_big or
        expr.tag == CIR.Expr.Tag.num_literal_i32 or
        expr.tag == CIR.Expr.Tag.num_literal_big);
}
