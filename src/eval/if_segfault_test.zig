//! Tests for if expression evaluation to ensure no segmentation faults occur
const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const check_types = @import("../check/check_types.zig");
const CIR = canonicalize.CIR;
const types = @import("../types.zig");
const eval = @import("eval.zig");
const stack = @import("stack.zig");
const layout_store = @import("../layout/store.zig");

test "minimal if expression segfault" {
    const allocator = testing.allocator;
    const source = "if True 1 else 0";

    // Initialize ModuleEnv
    const owned_source = try allocator.dupe(u8, source);
    var module_env = try base.ModuleEnv.init(allocator, owned_source);
    defer module_env.deinit();

    // Parse
    var parse_ast = try parse.parseExpr(&module_env, source);
    defer parse_ast.deinit(allocator);
    parse_ast.store.emptyScratch();

    // Create CIR
    var cir = try CIR.init(&module_env, "test");
    defer cir.deinit();

    // Canonicalize
    var can = try canonicalize.init(&cir, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    // Type check
    var checker = try check_types.init(allocator, &module_env.types, &cir, &.{}, &cir.store.regions);
    defer checker.deinit();
    _ = try checker.checkExpr(canonical_expr_idx);

    // Verify the if expression has a proper type after type checking
    const if_expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
    _ = module_env.types.resolveVar(if_expr_var);

    // Create stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    // Try to evaluate
    const result = try eval.eval(allocator, &cir, canonical_expr_idx, &eval_stack, &layout_cache, &module_env.types);

    // Read the result
    const value = @as(*i128, @ptrCast(@alignCast(result.ptr))).*;
    try testing.expectEqual(@as(i128, 1), value);
}

test "check parseAndCanonicalizeExpr cleanup order" {
    const allocator = testing.allocator;
    const source = "42";

    // Initialize ModuleEnv
    const owned_source = try allocator.dupe(u8, source);
    var module_env = try base.ModuleEnv.init(allocator, owned_source);

    // Parse
    var parse_ast = try parse.parseExpr(&module_env, source);
    parse_ast.store.emptyScratch();

    // Create CIR
    var cir = try CIR.init(&module_env, "test");

    // Canonicalize
    var can = try canonicalize.init(&cir, &parse_ast, null);

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse unreachable;

    // Type check
    var checker = try check_types.init(allocator, &module_env.types, &cir, &.{}, &cir.store.regions);
    _ = try checker.checkExpr(canonical_expr_idx);

    // Test evaluation
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);

    const result = try eval.eval(allocator, &cir, canonical_expr_idx, &eval_stack, &layout_cache, &module_env.types);

    // Copy the value out of the stack before we clean up
    const value = @as(*i128, @ptrCast(@alignCast(result.ptr))).*;
    try testing.expectEqual(@as(i128, 42), value);

    // Clean up in correct order
    layout_cache.deinit();
    eval_stack.deinit();
    checker.deinit();
    can.deinit();
    cir.deinit();
    parse_ast.deinit(allocator);
    // NOTE: owned_source is owned by module_env and will be freed when module_env.deinit() is called
    // allocator.free(owned_source);
    module_env.deinit();
}
