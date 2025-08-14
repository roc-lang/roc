//! Simplified test to demonstrate the polymorphic type inference bug
//! This test shows that nested polymorphic functions get 'err' types

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const testing = std.testing;
const test_allocator = testing.allocator;

test "identity in apply should type check correctly" {
    // This test verifies that the identity function works correctly when used inside apply
    
    const source = 
        \\{
        \\    identity = |x| x
        \\    apply = |f, val| f(val)
        \\    apply(identity, 10)
        \\}
    ;
    
    // Parse
    var module_env = try can.ModuleEnv.init(test_allocator, source);
    defer module_env.deinit();
    
    var parse_ast = try parse.parseExpr(&module_env.common, test_allocator);
    defer parse_ast.deinit(test_allocator);
    
    parse_ast.store.emptyScratch();
    try module_env.initCIRFields(test_allocator, "test");
    
    // Canonicalize
    var czer = try can.Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();
    
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canon_expr = try czer.canonicalizeExpr(expr_idx);
    
    // Type check
    var checker = try Check.init(test_allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();
    
    if (canon_expr) |expr| {
        _ = try checker.checkExpr(expr.get_idx());
    }
    
    const has_errors = checker.problems.problems.items.len > 0;
    
    // Should type check without errors
    try testing.expect(!has_errors);
}