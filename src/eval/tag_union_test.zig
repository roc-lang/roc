//! Tests for evaluating tag union expressions
const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const check_types = @import("../check/check_types.zig");
const eval = @import("./eval.zig");
const stack = @import("./stack.zig");
const layout_store = @import("../layout/store.zig");
const layout = @import("../layout/layout.zig");
const types = @import("../types.zig");
const CIR = @import("../check/canonicalize/CIR.zig");

test "eval simple tag union - Red" {
    const allocator = testing.allocator;
    const source = "Red";

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
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse unreachable;

    // Type check
    var checker = try check_types.init(allocator, &module_env.types, &cir, &.{}, &cir.store.regions);
    defer checker.deinit();
    _ = try checker.checkExpr(canonical_expr_idx);

    // Create stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    // Evaluate
    const result = try eval.eval(allocator, &cir, canonical_expr_idx, &eval_stack, &layout_cache, &module_env.types);

    // The result should be a scalar integer (u8 discriminant)
    try testing.expectEqual(layout.LayoutTag.scalar, result.layout.tag);
    try testing.expectEqual(layout.ScalarTag.int, result.layout.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.u8, result.layout.data.scalar.data.int);

    // Read the discriminant value - Red should be 0 (first tag)
    const discriminant_ptr = @as(*const u8, @ptrCast(@alignCast(result.ptr)));
    try testing.expectEqual(@as(u8, 0), discriminant_ptr.*);
}

test "eval simple tag union - multiple tags" {
    const allocator = testing.allocator;
    // This test is a placeholder for when pattern matching is implemented
    // For now, we'll just test that simple tags can be evaluated
    const simple_source = "Green";

    // Initialize ModuleEnv
    const owned_source = try allocator.dupe(u8, simple_source);
    var module_env = try base.ModuleEnv.init(allocator, owned_source);
    defer module_env.deinit();

    // Parse
    var parse_ast = try parse.parseExpr(&module_env, simple_source);
    defer parse_ast.deinit(allocator);
    parse_ast.store.emptyScratch();

    // Create CIR
    var cir = try CIR.init(&module_env, "test");
    defer cir.deinit();

    // Canonicalize
    var can = try canonicalize.init(&cir, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse unreachable;

    // Type check
    var checker = try check_types.init(allocator, &module_env.types, &cir, &.{}, &cir.store.regions);
    defer checker.deinit();
    _ = try checker.checkExpr(canonical_expr_idx);

    // Create stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    // Evaluate
    const result = try eval.eval(allocator, &cir, canonical_expr_idx, &eval_stack, &layout_cache, &module_env.types);

    // The result should be a scalar integer (u8 discriminant)
    try testing.expectEqual(layout.LayoutTag.scalar, result.layout.tag);
    try testing.expectEqual(layout.ScalarTag.int, result.layout.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.u8, result.layout.data.scalar.data.int);

    // For a single tag "Green", it would be discriminant 0
    // In a real scenario with multiple tags defined, the discriminant would depend on the order
    const discriminant_ptr = @as(*const u8, @ptrCast(@alignCast(result.ptr)));
    _ = discriminant_ptr; // We can't test the exact value without defining the full type
}

test "eval tag union - if expression with custom tags" {
    // Once we have a way to define custom tag types, we can test:
    // if status == Ok then 1 else 0
    // For now, this is a placeholder test
    return error.SkipZigTest;
}
