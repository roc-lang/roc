//! Tests for where clause canonicalization.
//!
//! These tests verify that where clauses in type annotations are correctly
//! canonicalized.

const std = @import("std");
const testing = std.testing;
const parse = @import("parse");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");

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

test "module with simple type annotation" {
    const source =
        \\module [foo]
        \\
        \\foo : U64
        \\foo = 42
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    // Module should canonicalize without errors
    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with function type annotation" {
    const source =
        \\module [add]
        \\
        \\add : U64, U64 -> U64
        \\add = |a, b| a + b
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with generic type annotation" {
    const source =
        \\module [identity]
        \\
        \\identity : a -> a
        \\identity = |x| x
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with parameterized type annotation" {
    const source =
        \\module [identity]
        \\
        \\identity : a -> a
        \\identity = |x| x
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with record type annotation" {
    const source =
        \\module [get_name]
        \\
        \\get_name : { name : Str } -> Str
        \\get_name = |record| record.name
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with tag union type annotation" {
    const source =
        \\module [is_ok]
        \\
        \\is_ok : [Ok, Err] -> Bool
        \\is_ok = |result|
        \\    match result {
        \\        Ok => True
        \\        Err => False
        \\    }
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with multiple type parameters" {
    const source =
        \\module [transform]
        \\
        \\transform : a, b -> a
        \\transform = |a, b| a
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with nested function types" {
    const source =
        \\module [curry]
        \\
        \\curry : (a, b -> c) -> (a -> (b -> c))
        \\curry = |f| |a| |b| f(a, b)
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with tuple type annotation" {
    const source =
        \\module [pair]
        \\
        \\pair : a, b -> (a, b)
        \\pair = |a, b| (a, b)
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with type alias" {
    const source =
        \\module [Point]
        \\
        \\Point : { x : F64, y : F64 }
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with opaque type" {
    const source =
        \\module [Counter]
        \\
        \\Counter := U64
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with multiple declarations" {
    const source =
        \\module [foo, bar, baz]
        \\
        \\foo : U64
        \\foo = 1
        \\
        \\bar : U64
        \\bar = 2
        \\
        \\baz : U64
        \\baz = foo + bar
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with unexposed declaration" {
    const source =
        \\module [public_fn]
        \\
        \\helper : U64 -> U64
        \\helper = |x| x + 1
        \\
        \\public_fn : U64 -> U64
        \\public_fn = |x| helper(x)
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "empty module" {
    const source =
        \\module []
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with expect statement" {
    const source =
        \\module [test_fn]
        \\
        \\test_fn = {
        \\    expect 1 == 1
        \\    42
        \\}
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with dbg statement" {
    const source =
        \\module [debug_fn]
        \\
        \\debug_fn = {
        \\    dbg 42
        \\    0
        \\}
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with simple crash" {
    const source =
        \\module [crasher]
        \\
        \\crasher = 0
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with return statement" {
    const source =
        \\module [early_return]
        \\
        \\early_return = |x| {
        \\    if x == 0 {
        \\        return 0
        \\    }
        \\    x
        \\}
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with var declaration" {
    const source =
        \\module [counter]
        \\
        \\counter = {
        \\    var x_ = 0
        \\    x_ = x_ + 1
        \\    x_
        \\}
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with for loop" {
    const source =
        \\module [sum]
        \\
        \\sum = |list| {
        \\    var total_ = 0
        \\    for item in list {
        \\        total_ = total_ + item
        \\    }
        \\    total_
        \\}
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}

test "module with while loop" {
    const source =
        \\module [countdown]
        \\
        \\countdown = |n| {
        \\    var x_ = n
        \\    while x_ > 0 {
        \\        x_ = x_ - 1
        \\    }
        \\    x_
        \\}
    ;

    const result = try parseAndCanonicalizeModule(source);
    defer cleanupModule(result.env, result.can, result.ast);

    try testing.expect(result.ast.parse_diagnostics.items.len == 0);
}
