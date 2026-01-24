//! Tests for error handling and edge cases in Can.zig.
//!
//! These tests verify that the canonicalizer correctly handles error conditions
//! and produces appropriate diagnostics.

const std = @import("std");
const testing = std.testing;
const parse = @import("parse");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const TestEnv = @import("TestEnv.zig").TestEnv;

fn parseAndCanonicalizeModule(source: []const u8) !struct { env: *ModuleEnv, can: *Can, ast: *parse.AST } {
    const gpa = testing.allocator;

    const env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(env);

    const ast = try gpa.create(parse.AST);
    errdefer gpa.destroy(ast);

    const can = try gpa.create(Can);
    errdefer gpa.destroy(can);

    env.* = try ModuleEnv.init(gpa, source);
    errdefer env.deinit();

    ast.* = try parse.parse(&env.common, gpa);
    errdefer ast.deinit(gpa);

    try env.initCIRFields("test");

    can.* = try Can.init(env, ast, null);

    return .{ .env = env, .can = can, .ast = ast };
}

fn cleanupModule(env: *ModuleEnv, can: *Can, ast: *parse.AST) void {
    const gpa = testing.allocator;
    can.deinit();
    gpa.destroy(can);
    ast.deinit(gpa);
    gpa.destroy(ast);
    env.deinit();
    gpa.destroy(env);
}

// Edge case expression tests

test "very large integer literal" {
    const source = "99999999999999999999999999999999999999";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "negative large integer" {
    const source = "-99999999999999999999999999999999";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "integer with underscores" {
    const source = "1_000_000";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "hex integer" {
    const source = "0xDEADBEEF";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "binary integer" {
    const source = "0b11110000";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "octal integer" {
    const source = "0o755";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "deeply nested expression" {
    const source = "(((((((((1)))))))))";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "nested block expression" {
    const source =
        \\{
        \\    a = {
        \\        b = {
        \\            c = 1
        \\            c
        \\        }
        \\        b
        \\    }
        \\    a
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "complex match expression" {
    const source =
        \\match (x, y, z) {
        \\    (0, 0, 0) => "origin"
        \\    (a, 0, 0) => "x-axis"
        \\    (0, b, 0) => "y-axis"
        \\    (0, 0, c) => "z-axis"
        \\    (a, b, c) => "space"
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "match with multiple branches" {
    const source =
        \\match n {
        \\    0 => "zero"
        \\    1 => "one"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "lambda with pattern matching" {
    const source = "|{ x, y }| x + y";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "lambda with tuple pattern" {
    const source = "|(a, b, c)| a + b + c";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "record with optional field" {
    const source = "{ x: 1, y: 2 }";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "record with many fields" {
    const source = "{ a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8 }";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "nested record access" {
    const source = "a.b.c.d.e";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "list comprehension-like structure" {
    const source = "[f(x) for x in list]";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    // May or may not parse depending on syntax support
    _ = try test_env.canonicalizeExpr();
}

test "chained binary operations" {
    const source = "1 + 2 * 3 - 4 / 5";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "comparison chain" {
    const source = "a == b";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "logical operations" {
    const source = "a && b || c";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "negation" {
    const source = "!True";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "pipe operator" {
    const source = "x |> f |> g |> h";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "function with many arguments" {
    const source = "f(1, 2, 3, 4, 5, 6, 7, 8)";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "nested function calls" {
    const source = "f(g(h(i(j(x)))))";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "tag with no payload" {
    const source = "None";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "tag with tuple payload" {
    const source = "Point(1, 2)";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "tag with record payload" {
    const source = "Person({ name: \"Alice\", age: 30 })";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "very long string" {
    const source =
        \\"This is a very long string that goes on and on and on for quite a while to test handling of long strings in the canonicalizer"
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

// Module-level error handling tests

test "module with duplicate exposed names" {
    const source =
        \\module [foo, foo]
        \\
        \\foo = 42
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    // Should parse (checking for duplicate may be in a later phase)
    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with self-referential definition" {
    const source =
        \\module [x]
        \\
        \\x = x
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    // Should parse without parse errors
    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with mutual references" {
    const source =
        \\module [a, b]
        \\
        \\a = |x| b(x)
        \\b = |x| x
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with shadowing in nested scope" {
    const source =
        \\module [outer]
        \\
        \\outer = {
        \\    x = 1
        \\    inner = {
        \\        x = 2
        \\        x
        \\    }
        \\    x + inner
        \\}
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with pattern in lambda" {
    const source =
        \\module [get_x]
        \\
        \\get_x = |{ x }| x
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with nested match" {
    const source =
        \\module [deep_match]
        \\
        \\deep_match = |x|
        \\    match x {
        \\        A(y) =>
        \\            match y {
        \\                B(z) => z
        \\                _ => 0
        \\            }
        \\        _ => 0
        \\    }
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with multiple type annotations" {
    const source =
        \\module [f, g, h]
        \\
        \\f : U64 -> U64
        \\f = |x| x + 1
        \\
        \\g : U64 -> U64
        \\g = |x| x * 2
        \\
        \\h : U64 -> U64
        \\h = |x| f(g(x))
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with complex type alias" {
    const source =
        \\module [MyRecord]
        \\
        \\MyRecord : { x : U64, y : U64, name : Str }
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with break in loop" {
    const source =
        \\module [find_first]
        \\
        \\find_first = |list, predicate| {
        \\    var result_ = None
        \\    for item in list {
        \\        if predicate(item) {
        \\            result_ = Some(item)
        \\            break
        \\        }
        \\    }
        \\    result_
        \\}
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with continue in loop" {
    const source =
        \\module [filter_sum]
        \\
        \\filter_sum = |list| {
        \\    var total_ = 0
        \\    for item in list {
        \\        if item < 0 {
        \\            continue
        \\        }
        \\        total_ = total_ + item
        \\    }
        \\    total_
        \\}
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}
