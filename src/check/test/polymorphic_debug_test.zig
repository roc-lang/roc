//! Debug test to understand why polymorphic types aren't working correctly

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const testing = std.testing;
const test_allocator = testing.allocator;
const ModuleEnv = can.ModuleEnv;

test "debug: identity function polymorphism" {
    const source = 
        \\{
        \\    identity = |x| x
        \\    num = identity(42)
        \\    str = identity("hello")
        \\    { num, str }
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
    
    // Debug output
    std.debug.print("\n=== Debug: Identity Function Polymorphism ===\n", .{});
    std.debug.print("Number of type errors: {}\n", .{checker.problems.problems.items.len});
    
    if (checker.problems.problems.items.len > 0) {
        std.debug.print("Type errors found:\n", .{});
        for (checker.problems.problems.items) |problem| {
            std.debug.print("  Problem: {any}\n", .{problem});
        }
    }
    
    // The test should pass with no type errors
    try testing.expect(checker.problems.problems.items.len == 0);
}

test "debug: simple identity usage" {
    const source = 
        \\{
        \\    identity = |x| x
        \\    identity(42)
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
    
    std.debug.print("\n=== Debug: Simple Identity Usage ===\n", .{});
    std.debug.print("Number of type errors: {}\n", .{checker.problems.problems.items.len});
    
    // Should have no errors
    try testing.expect(checker.problems.problems.items.len == 0);
}

test "debug: apply with identity" {
    const source = 
        \\{
        \\    identity = |x| x
        \\    apply = |f, val| f(val)
        \\    num = apply(identity, 42)
        \\    str = apply(identity, "hello")
        \\    { num, str }
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
    
    std.debug.print("\n=== Debug: Apply with Identity ===\n", .{});
    std.debug.print("Number of type errors: {}\n", .{checker.problems.problems.items.len});
    
    if (checker.problems.problems.items.len > 0) {
        std.debug.print("First error details:\n", .{});
        const problem = checker.problems.problems.items[0];
        std.debug.print("  Problem type: {any}\n", .{problem});
    }
    
    // Should have no errors
    try testing.expect(checker.problems.problems.items.len == 0);
}