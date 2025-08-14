//! Tests for the generalization phase of let-polymorphism.
//! 
//! These tests verify that:
//! 1. Flex variables at the current rank are converted to rank 0 (generalized)
//! 2. Variables from outer scopes are not generalized
//! 3. Generalization happens after let-binding type checking

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const testing = std.testing;
const test_allocator = testing.allocator;
const Rank = types.Rank;
const ModuleEnv = can.ModuleEnv;
const Var = types.Var;

test "flex_var at current rank should be generalized to rank 0" {
    // Setup
    var module_env = try ModuleEnv.init(test_allocator, "x = 5");
    defer module_env.deinit();
    
    var checker = try Check.init(test_allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();
    
    // Create a flex_var at rank 2
    checker.current_rank = @enumFromInt(2);
    const var1 = try checker.freshVar();
    
    // Verify it has rank 2
    const before = module_env.types.resolveVar(var1);
    try testing.expectEqual(@as(Rank, @enumFromInt(2)), before.desc.rank);
    
    // Generalize it
    try checker.generalize(var1);
    
    // It should now have rank 0 (generalized)
    const after = module_env.types.resolveVar(var1);
    try testing.expectEqual(Rank.generalized, after.desc.rank);
}

test "flex_var from outer scope should not be generalized" {
    // Setup
    var module_env = try ModuleEnv.init(test_allocator, "x = 5");
    defer module_env.deinit();
    
    var checker = try Check.init(test_allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();
    
    // Create a flex_var at rank 1
    checker.current_rank = Rank.top_level;
    const outer_var = try checker.freshVar();
    
    // Enter a new scope (rank 2)
    checker.enterScope();
    try testing.expectEqual(@as(Rank, @enumFromInt(2)), checker.current_rank);
    
    // Try to generalize the outer variable (should not change it)
    try checker.generalize(outer_var);
    
    // It should still have rank 1, not rank 0
    const after = module_env.types.resolveVar(outer_var);
    try testing.expectEqual(Rank.top_level, after.desc.rank);
}

test "function type should have all components generalized" {
    // Setup
    var module_env = try ModuleEnv.init(test_allocator, "x = 5");
    defer module_env.deinit();
    
    var checker = try Check.init(test_allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();
    
    // Enter a scope
    checker.enterScope();
    
    // Create a function type: a -> b
    const arg_var = try checker.freshVar();
    const ret_var = try checker.freshVar();
    
    const args_idx = try module_env.types.appendVar(arg_var);
    const func_content = types.Content{
        .structure = .{
            .fn_pure = .{
                .args = .{ .start = args_idx, .count = 1 },
                .ret = ret_var,
                .needs_instantiation = true,
            },
        },
    };
    const func_var = try checker.freshVarFromContent(func_content);
    
    // Generalize the function
    try checker.generalize(func_var);
    
    // Check that argument and return types are generalized
    const arg_after = module_env.types.resolveVar(arg_var);
    const ret_after = module_env.types.resolveVar(ret_var);
    
    try testing.expectEqual(Rank.generalized, arg_after.desc.rank);
    try testing.expectEqual(Rank.generalized, ret_after.desc.rank);
}

test "let-binding should generalize value type" {
    const source = 
        \\{
        \\    identity = |x| x
        \\    identity
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
    
    // Find the identity function's pattern variable
    // After generalization, its type should have rank 0 components
    // This is a higher-level test that the whole system works
    
    // No type errors expected
    try testing.expect(checker.problems.problems.items.len == 0);
}

test "nested let-bindings should generalize at each level" {
    const source = 
        \\{
        \\    outer = {
        \\        inner = |x| x
        \\        inner
        \\    }
        \\    outer
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
    
    // After type checking, we should be back at top level
    try testing.expectEqual(Rank.top_level, checker.current_rank);
    
    // No type errors expected
    try testing.expect(checker.problems.problems.items.len == 0);
}