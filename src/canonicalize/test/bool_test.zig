//! Tests for boolean canonicalization.
//!
//! This module contains unit tests that verify the correct canonicalization
//! of boolean expressions from parsed AST into the compiler's canonical
//! internal representation (CIR).

const std = @import("std");
const TestEnv = @import("TestEnv.zig").TestEnv;
const CIR = @import("../CIR.zig");
const testing = std.testing;

test "canonicalize Bool.True" {
    const source = "Bool.True";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Check if it's a module access (Bool.True)
    try testing.expectEqual(CIR.Expr.Tag.module_access, expr.tag);
}

test "canonicalize Bool.False" {
    const source = "Bool.False";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Check if it's a module access (Bool.False)
    try testing.expectEqual(CIR.Expr.Tag.module_access, expr.tag);
}

test "canonicalize boolean comparison" {
    const source = "Bool.True == Bool.False";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Check if it's an equality comparison
    try testing.expectEqual(CIR.Expr.Tag.binop_double_equals, expr.tag);
}

test "canonicalize boolean and" {
    const source = "Bool.True && Bool.False";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Check if it's a logical and
    try testing.expectEqual(CIR.Expr.Tag.binop_and, expr.tag);
}

test "canonicalize boolean or" {
    const source = "Bool.True || Bool.False";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Check if it's a logical or
    try testing.expectEqual(CIR.Expr.Tag.binop_or, expr.tag);
}

test "canonicalize boolean not" {
    const source = "!Bool.True";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Check if it's a logical not
    try testing.expectEqual(CIR.Expr.Tag.not_lookup, expr.tag);
}

test "canonicalize if expression with booleans" {
    const source = "if Bool.True then 1 else 0";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Check if it's an if expression
    try testing.expectEqual(CIR.Expr.Tag.if_else, expr.tag);
}

test "canonicalize when expression with booleans" {
    const source =
        \\when x is
        \\    Bool.True -> 1
        \\    Bool.False -> 0
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr);

    // Check if it's a when expression
    try testing.expectEqual(CIR.Expr.Tag.match, expr.tag);
}
