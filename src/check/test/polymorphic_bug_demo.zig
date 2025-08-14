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
        \\    id = |x| x
        \\    app = |f, val| f(val)
        \\    
        \\    # These should all work with the polymorphic identity function
        \\    num = app(id, 42)
        \\    str = app(id, "hello")
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

    // This test should now pass with our let-polymorphism implementation
    // The identity function is properly generalized and can be used polymorphically

    // This test should pass with no type errors
    try testing.expect(!has_errors);
}
