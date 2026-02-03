//! Tests for edge cases in Can.zig to improve coverage.
//!
//! These tests target specific uncovered code paths in the canonicalizer.

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

test "module with where clause on declaration" {
    const source =
        \\module [eq]
        \\
        \\eq : a, a -> Bool where a implements Eq
        \\eq = |x, y| x == y
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    // This canonicalizes successfully (where clauses are handled)
    _ = try result.can.canonicalizeFile();
}

test "module with nested type in opaque" {
    const source =
        \\module [Outer]
        \\
        \\Outer := {
        \\    inner : Inner
        \\}
        \\
        \\Inner : { value : U64 }
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with associated function" {
    const source =
        \\module [Counter, Counter.new, Counter.increment]
        \\
        \\Counter := U64
        \\
        \\Counter.new : {} -> Counter
        \\Counter.new = |{}| @Counter(0)
        \\
        \\Counter.increment : Counter -> Counter
        \\Counter.increment = |@Counter(n)| @Counter(n + 1)
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with type declaration and annotation" {
    const source =
        \\module [Point, origin]
        \\
        \\Point : { x : F64, y : F64 }
        \\
        \\origin : Point
        \\origin = { x: 0.0, y: 0.0 }
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with opaque type unwrap" {
    const source =
        \\module [Age, get_years]
        \\
        \\Age := U64
        \\
        \\get_years : Age -> U64
        \\get_years = |@Age(years)| years
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with deeply nested scopes" {
    const source =
        \\module [deep]
        \\
        \\deep = {
        \\    a = {
        \\        b = {
        \\            c = {
        \\                d = {
        \\                    e = 42
        \\                    e
        \\                }
        \\                d
        \\            }
        \\            c
        \\        }
        \\        b
        \\    }
        \\    a
        \\}
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with complex pattern matching" {
    const source =
        \\module [process]
        \\
        \\process = |input|
        \\    match input {
        \\        { status: Ok, data: { items: [first, .. as rest] } } => first
        \\        { status: Err, message } => 0
        \\        _ => 0
        \\    }
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with effectful function annotation" {
    const source =
        \\module [print_line]
        \\
        \\print_line! : Str -> {}
        \\print_line! = |_| {}
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with multiple type parameters" {
    const source =
        \\module [map_pair]
        \\
        \\map_pair : (a -> c), (b -> d), (a, b) -> (c, d)
        \\map_pair = |f, g, pair|
        \\    (f(pair.0), g(pair.1))
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with recursive type reference" {
    const source =
        \\module [Tree]
        \\
        \\Tree a : [Leaf a, Node (Tree a) (Tree a)]
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with anonymous lambda in declaration" {
    const source =
        \\module [apply_twice]
        \\
        \\apply_twice = |f, x| f(f(x))
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with record pattern in lambda" {
    const source =
        \\module [get_x]
        \\
        \\get_x = |{ x, y: _ }| x
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with tuple type" {
    const source =
        \\module [swap]
        \\
        \\swap : (a, b) -> (b, a)
        \\swap = |(a, b)| (b, a)
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with extensible record" {
    const source =
        \\module [with_name]
        \\
        \\with_name : { r }a, Str -> { r, name : Str }a
        \\with_name = |record, name| { record & name }
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "module with annotation only declaration" {
    const source =
        \\module [external_fn]
        \\
        \\external_fn : U64 -> U64
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);
    _ = try result.can.canonicalizeFile();
}

test "expression with very long identifier" {
    const source = "this_is_a_very_long_identifier_name_that_goes_on_for_quite_a_while";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "expression with simple identifier" {
    const source = "simple_var";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "expression with method chain" {
    const source = "x.method1().method2().method3()";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "expression with complex record" {
    const source =
        \\{
        \\    name: "test",
        \\    values: [1, 2, 3],
        \\    nested: {
        \\        a: True,
        \\        b: False
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "expression with tag with complex payload" {
    const source = "Result({ ok: True, value: [1, 2, 3] })";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}

test "expression with nested conditionals" {
    const source =
        \\if a then
        \\    if b then 1 else 2
        \\else
        \\    if c then 3 else 4
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr();
    try testing.expect(canonical_expr != null);
}
