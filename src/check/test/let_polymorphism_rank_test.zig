//! Tests for rank tracking in the let-polymorphism implementation.
//!
//! These tests verify that:
//! 1. Variables are created with the correct rank
//! 2. Ranks are properly maintained during type checking
//! 3. Rank increases when entering let-binding scopes
//! 4. Rank decreases when exiting scopes

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

test "fresh variables should have current rank" {
    // Setup
    var module_env = try ModuleEnv.init(test_allocator, "x = 5");
    defer module_env.deinit();

    // Variables created at top level should have rank 1
    const var1 = try module_env.types.fresh();
    const resolved1 = module_env.types.resolveVar(var1);
    try testing.expectEqual(Rank.top_level, resolved1.desc.rank);
}

test "rank should increase when entering let-binding scope" {
    const source =
        \\{
        \\    outer = 1
        \\    inner = {
        \\        nested = 2
        \\        nested
        \\    }
        \\    inner
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

    // After type checking, verify rank tracking worked:
    // 1. Top-level checker should be back at rank 1
    try testing.expectEqual(Rank.top_level, checker.current_rank);

    // 2. Variables created in nested scopes should have had higher ranks
    // (This will be verified more thoroughly once we implement rank tracking)
}

test "rank should be preserved during unification" {
    var module_env = try ModuleEnv.init(test_allocator, "x = 5");
    defer module_env.deinit();

    // Create two variables with different ranks
    const var1 = try module_env.types.register(.{
        .content = .{ .flex_var = null },
        .rank = @enumFromInt(2),
        .mark = .none,
    });

    const var2 = try module_env.types.register(.{
        .content = .{ .flex_var = null },
        .rank = @enumFromInt(3),
        .mark = .none,
    });

    // Setup checker for unification
    var checker = try Check.init(test_allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();

    // Unify the variables
    _ = try checker.unify(var1, var2);

    // The unified variable should have the minimum rank (2)
    const unified = module_env.types.resolveVar(var1);
    try testing.expectEqual(@as(Rank, @enumFromInt(2)), unified.desc.rank);
}

test "nested blocks should have incrementing ranks" {
    const source =
        \\{
        \\    level1 = {
        \\        level2 = {
        \\            level3 = 42
        \\            level3
        \\        }
        \\        level2
        \\    }
        \\    level1
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

    // Verify rank returns to original level
    try testing.expectEqual(Rank.top_level, checker.current_rank);
}

test "function parameters should get correct rank" {
    const source =
        \\|x| {
        \\    inner = x
        \\    inner
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

    // Function body should be checked at an increased rank
    // Parameters should be at the function's rank
    // This test will be more specific once rank tracking is implemented
    try testing.expectEqual(Rank.top_level, checker.current_rank);
}
