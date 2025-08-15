//! Test to understand exactly when and how polymorphic values are instantiated

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const testing = std.testing;
const test_allocator = testing.allocator;
const ModuleEnv = can.ModuleEnv;

test "each use of polymorphic identity should get fresh instantiation" {
    // This should work - each use of identity gets its own instantiation
    const source =
        \\{
        \\    id = |x| x
        \\    a = id(42)
        \\    b = id("hello")
        \\    { a, b }
        \\}
    ;

    var module_env = try ModuleEnv.init(test_allocator, source);
    defer module_env.deinit();

    var parse_ast = try parse.parseExpr(&module_env.common, test_allocator);
    defer parse_ast.deinit(test_allocator);

    parse_ast.store.emptyScratch();
    try module_env.initCIRFields(test_allocator, "test");

    var czer = try can.Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canon_expr = try czer.canonicalizeExpr(expr_idx);

    var checker = try Check.init(test_allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();

    if (canon_expr) |expr| {
        _ = try checker.checkExpr(expr.get_idx());
    }

    // This MUST work - it's basic let-polymorphism
    try testing.expect(checker.problems.problems.items.len == 0);
}

test "nested polymorphic instantiation with type mismatch should fail" {
    // From test_nested_instantiation_crash.md
    // The function `composed` is annotated to return Str but actually returns List(a)
    // This MUST produce a type error
    const source =
        \\{
        \\    make_record : a -> { value: a, tag: Str }
        \\    make_record = |x| { value: x, tag: "data" }
        \\
        \\    get_value : { value: a, tag: Str } -> a
        \\    get_value = |r| r.value
        \\
        \\    composed : List(a) -> Str
        \\    composed = |n| get_value(make_record(n))
        \\
        \\    composed([42])
        \\}
    ;

    var module_env = try ModuleEnv.init(test_allocator, source);
    defer module_env.deinit();

    var parse_ast = try parse.parseExpr(&module_env.common, test_allocator);
    defer parse_ast.deinit(test_allocator);

    parse_ast.store.emptyScratch();
    try module_env.initCIRFields(test_allocator, "test");

    var czer = try can.Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canon_expr = try czer.canonicalizeExpr(expr_idx);

    var checker = try Check.init(test_allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();

    if (canon_expr) |expr| {
        _ = try checker.checkExpr(expr.get_idx());
    }

    // This MUST fail with a type error!
    // composed returns List(a) but is annotated as returning Str
    try testing.expect(checker.problems.problems.items.len > 0);
}

test "polymorphic value passed to function and used twice" {
    // This should also work in standard Hindley-Milner
    const source =
        \\{
        \\    id = |x| x
        \\    app = |f, val| f(val)
        \\    a = app(id, 42)
        \\    b = app(id, "hello")
        \\    { a, b }
        \\}
    ;

    var module_env = try ModuleEnv.init(test_allocator, source);
    defer module_env.deinit();

    var parse_ast = try parse.parseExpr(&module_env.common, test_allocator);
    defer parse_ast.deinit(test_allocator);

    parse_ast.store.emptyScratch();
    try module_env.initCIRFields(test_allocator, "test");

    var czer = try can.Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canon_expr = try czer.canonicalizeExpr(expr_idx);

    var checker = try Check.init(test_allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();

    if (canon_expr) |expr| {
        _ = try checker.checkExpr(expr.get_idx());
    }

    // This should work - each call to apply should instantiate identity independently
    try testing.expect(checker.problems.problems.items.len == 0);
}
