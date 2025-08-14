//! Tests that demonstrate the polymorphic type inference bug
//! where polymorphic functions get 'err' types instead of proper type variables

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const Can = can.Can;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const testing = std.testing;
const test_allocator = testing.allocator;

/// Helper to parse, canonicalize, and type check source code
fn typeCheckSource(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    czer: *Can,
    checker: *Check,
    has_type_errors: bool,
    canon_expr_idx: Can.CanonicalizedExpr,
} {
    // Set up module environment
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);

    // Parse
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(&module_env.common, module_env.gpa);

    // Empty scratch space
    parse_ast.store.emptyScratch();

    // Initialize CIR fields
    try module_env.initCIRFields(allocator, "test");

    // Canonicalize
    const czer = try allocator.create(Can);
    czer.* = try Can.init(module_env, parse_ast, null);

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canon_result = try czer.canonicalizeExpr(expr_idx);
    const canon_expr = canon_result orelse return error.CanonicalizeError;

    // Type check
    const checker = try allocator.create(Check);
    const empty_modules: []const *ModuleEnv = &.{};
    checker.* = try Check.init(allocator, &module_env.types, module_env, empty_modules, &module_env.store.regions);

    _ = try checker.checkExpr(canon_expr.get_idx());

    const has_type_errors = checker.problems.problems.items.len > 0;

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .czer = czer,
        .checker = checker,
        .has_type_errors = has_type_errors,
        .canon_expr_idx = canon_expr,
    };
}

fn cleanup(result: anytype, allocator: std.mem.Allocator) void {
    result.checker.deinit();
    allocator.destroy(result.checker);
    result.czer.deinit();
    allocator.destroy(result.czer);
    result.parse_ast.deinit(allocator);
    allocator.destroy(result.parse_ast);
    result.module_env.deinit();
    allocator.destroy(result.module_env);
}


test "identity function should have polymorphic parameter type" {
    const source = "|x| x";
    
    const result = try typeCheckSource(test_allocator, source);
    defer cleanup(result, test_allocator);
    
    // The canonicalized expression is already computed in typeCheckSource
    const expr_idx = result.canon_expr_idx.get_idx();
    const cir_expr = result.module_env.store.getExpr(expr_idx);
    
    // Verify it's a lambda
    try testing.expect(cir_expr == .e_lambda);
    
    // Get the lambda's type
    const lambda_var = ModuleEnv.varFrom(expr_idx);
    const lambda_resolved = result.module_env.types.resolveVar(lambda_var);
    
    // The lambda should have a function type
    try testing.expect(lambda_resolved.desc.content == .structure);
    
    const structure = lambda_resolved.desc.content.structure;
    const func = switch (structure) {
        .fn_pure => |f| f,
        .fn_effectful => |f| f,
        .fn_unbound => |f| f,
        else => return error.NotAFunction,
    };
    
    // Get the parameter type
    const param_types = result.module_env.types.sliceVars(func.args);
    try testing.expect(param_types.len == 1);
    
    const param_var = param_types[0];
    const param_resolved = result.module_env.types.resolveVar(param_var);
    
    // The parameter should be a polymorphic variable (flex_var or rigid_var)
    // NOT an error type!
    const is_polymorphic = param_resolved.desc.content == .flex_var or 
                           param_resolved.desc.content == .rigid_var;
    
    try testing.expect(is_polymorphic);
}

test "nested identity in apply function should have polymorphic types" {
    const source = 
        \\{
        \\    identity = |x| x
        \\    apply = |f, val| f(val)
        \\    apply(identity, 10)
        \\}
    ;
    
    const result = try typeCheckSource(test_allocator, source);
    defer cleanup(result, test_allocator);
    
    // Find the identity function in the canonicalized output
    // This is more complex, but we want to check that when identity is used
    // inside apply, it still has polymorphic types
    
    // Should type check without errors
    try testing.expect(!result.has_type_errors);
}

test "apply function with identity should resolve types correctly" {
    const source = 
        \\{
        \\    identity = |x| x
        \\    apply = |f, val| f(val)
        \\    num = apply(identity, 42)
        \\    str = apply(identity, "hello")
        \\    { num, str }
        \\}
    ;
    
    const result = try typeCheckSource(test_allocator, source);
    defer cleanup(result, test_allocator);
    
    // NOTE: This test currently fails because the type system lacks proper generalization.
    // The identity function gets a flex_var type but is never generalized to become polymorphic.
    // The fix in Check.zig helps with already-polymorphic types but doesn't create new ones.
    // TODO: Implement proper let-polymorphism generalization
    
    // Should type check without errors (currently expected to fail)
    try testing.expect(!result.has_type_errors);
}

test "deeply nested polymorphic functions" {
    const source = 
        \\{
        \\    identity = |x| x
        \\    apply = |f, val| f(val)
        \\    twice = |f, val| f(f(val))
        \\    
        \\    result1 = twice(identity, 42)
        \\    result2 = apply(|x| apply(identity, x), 100)
        \\    
        \\    { result1, result2 }
        \\}
    ;
    
    const result = try typeCheckSource(test_allocator, source);
    defer cleanup(result, test_allocator);
    
    // Should type check without errors
    try testing.expect(!result.has_type_errors);
}