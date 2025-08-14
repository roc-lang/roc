//! Test file that demonstrates the polymorphic type inference bug
//! 
//! RUN THIS TEST TO SEE THE BUG:
//! zig build test 2>&1 | grep -A 30 "FAILING TEST"

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const testing = std.testing;
const test_allocator = testing.allocator;

test "polymorphic identity with multiple types should work" {
    // This test verifies that polymorphic functions work correctly in nested contexts
    const source = 
        \\{
        \\    identity = |x| x
        \\    apply = |f, val| f(val)
        \\    
        \\    # These should all work with the polymorphic identity function
        \\    num = apply(identity, 42)
        \\    str = apply(identity, "hello")
        \\    
        \\    { num, str }
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
    
    // Check for type errors
    const has_errors = checker.problems.problems.items.len > 0;
    
    // NOTE: This test currently fails because the type system lacks proper generalization.
    // The identity function gets a flex_var type but is never generalized to become polymorphic.
    // TODO: Implement proper let-polymorphism generalization
    
    // This test should pass with no type errors (currently expected to fail)
    try testing.expect(!has_errors);
}