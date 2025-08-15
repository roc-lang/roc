//! Integration tests for let-polymorphism that parse, canonicalize, and type-check
//! actual code to ensure polymorphic values work correctly in practice.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const Can = can.Can;
const ModuleEnv = can.ModuleEnv;
const CanonicalizedExpr = can.Can.CanonicalizedExpr;
const testing = std.testing;
const test_allocator = testing.allocator;

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
fn typeCheck(allocator: std.mem.Allocator, source: []const u8) !bool {
    // Set up module environment
    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    // Parse
    var parse_ast = try parse.parseExpr(&module_env.common, allocator);
    defer parse_ast.deinit(allocator);
    if (parse_ast.hasErrors()) return false;

    // Canonicalize
    var czer = try Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canon_expr = try czer.canonicalizeExpr(expr_idx) orelse return false;

    // Type check
    var checker = try Check.init(allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();

    _ = try checker.checkExpr(canon_expr.get_idx());

    return checker.problems.problems.items.len == 0;
}

test "direct polymorphic identity usage" {
    const source =
        \\{
        \\    id = |x| x
        \\    a = id(1)
        \\    b = id("x")
        \\    { a, b }
        \\}
    ;
    try testing.expect(try typeCheck(test_allocator, source));
}

test "higher-order function with polymorphic identity" {
    const source =
        \\{
        \\    id = |x| x
        \\    f = |g, v| g(v)
        \\    a = f(id, 1)
        \\    b = f(id, "x")
        \\    { a, b }
        \\}
    ;
    try testing.expect(try typeCheck(test_allocator, source));
}

test "let-polymorphism with function composition" {
    const source =
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    double = |x| x * 2
        \\    add_one = |x| x + 1
        \\    num_compose = compose(double, add_one)
        \\    result1 = num_compose(5)
        \\    { result1 }
        \\}
    ;
    try testing.expect(try typeCheck(test_allocator, source));
}
