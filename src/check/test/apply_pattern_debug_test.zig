//! Detailed debugging for the apply pattern issue

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const testing = std.testing;
const test_allocator = testing.allocator;
const ModuleEnv = can.ModuleEnv;

fn dumpVar(module_env: *ModuleEnv, label: []const u8, var_: types.Var) void {
    const resolved = module_env.types.resolveVar(var_);
    std.debug.print("  {s}: Var({}) rank={} content={}\n", .{ label, @intFromEnum(var_), resolved.desc.rank, resolved.desc.content });
}

test "trace: simple apply pattern" {
    // Minimal reproduction of the issue
    const source =
        \\{
        \\    id = |x| x
        \\    app = |f, v| f(v)
        \\    a = app(id, 1)
        \\    b = app(id, "x")
        \\    { a, b }
        \\}
    ;

    // std.debug.print("\n=== TRACE: Simple Apply Pattern ===\n", .{});

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

    // std.debug.print("Type errors: {}\n", .{checker.problems.problems.items.len});
    // if (checker.problems.problems.items.len > 0) {
    //     const problem = checker.problems.problems.items[0];
    //     std.debug.print("First error: {any}\n", .{problem});
    // }

    // Should pass but currently fails
    try testing.expect(checker.problems.problems.items.len == 0);
}

test "direct polymorphic usage works" {
    // Test that direct polymorphic usage works correctly
    const source =
        \\{
        \\    id = |x| x
        \\    a = id(1)
        \\    b = id("x")
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

    try testing.expect(checker.problems.problems.items.len == 0);
}

test "comments parsing with function calls" {
    const source =
        \\{
        \\    id = |x| x
        \\    apply = |f, val| f(val)
        \\
        \\    # First call to apply with identity and a number
        \\    num1 = apply(id, 10)
        \\}
    ;

    var module_env = try ModuleEnv.init(test_allocator, source);
    defer module_env.deinit();

    var parse_ast = try parse.parseExpr(&module_env.common, test_allocator);
    defer parse_ast.deinit(test_allocator);

    try testing.expect(parse_ast.parse_diagnostics.items.len == 0);
    try testing.expect(parse_ast.tokenize_diagnostics.items.len == 0);
}
