//! Tests for string interpolation canonicalization.
//!
//! These tests verify that string interpolation expressions are correctly
//! canonicalized, including various combinations of literals and expressions.

const std = @import("std");
const testing = std.testing;
const TestEnv = @import("TestEnv.zig").TestEnv;

test "simple string interpolation with variable" {
    const source =
        \\"Hello, ${name}!"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    // String interpolation should canonicalize (may have lookup errors for undefined name)
    try testing.expect(canonical_expr != null);
}

test "string interpolation with multiple variables" {
    const source =
        \\"${greeting}, ${name}! How is ${thing}?"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string interpolation with expression" {
    const source =
        \\"The answer is ${40 + 2}."
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string interpolation with function call" {
    const source =
        \\"Result: ${f(x)}"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string interpolation with nested strings" {
    const source =
        \\"Outer: ${inner}"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "empty string" {
    const source =
        \\""
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string with escape sequences" {
    const source =
        \\"Hello\nWorld\t!"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "multiline string literal" {
    const source =
        \\"""
        \\Hello
        \\World
        \\"""
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "multiline string with interpolation" {
    const source =
        \\"""
        \\Hello ${name}
        \\How are you?
        \\"""
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string interpolation at start" {
    const source =
        \\"${x} is the value"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string interpolation at end" {
    const source =
        \\"The value is ${x}"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "consecutive interpolations" {
    const source =
        \\"${a}${b}${c}"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string interpolation with record access" {
    const source =
        \\"Name: ${person.name}"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string interpolation with parenthesized expression" {
    const source =
        \\"Value: ${(x)}"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string interpolation with lambda" {
    const source =
        \\"Lambda: ${|x| x}"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string interpolation with list" {
    const source =
        \\"Items: ${items}"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string only interpolation" {
    const source =
        \\"${x}"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string with unicode" {
    const source =
        \\"Hello 世界 🌍"
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "string interpolation with match" {
    const source =
        \\"""
        \\Result: ${match x {
        \\    Ok(v) => v
        \\    Err(_) => "error"
        \\}}
        \\"""
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}
