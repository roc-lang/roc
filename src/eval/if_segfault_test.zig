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

// test "minimal if expression segfault" {
//     const allocator = testing.allocator;
//     const source = "if True then 1 else 0";
//
//     // Initialize ModuleEnv
//     const owned_source = try allocator.dupe(u8, source);
//     defer allocator.free(owned_source);
//     var module_env = try base.ModuleEnv.init(allocator, owned_source);
//     defer module_env.deinit();
//
//     // Parse
//     var parse_ast = try parse.parseExpr(&module_env, source);
//     defer parse_ast.deinit(allocator);
//     parse_ast.store.emptyScratch();
//
//     // Create CIR
//     var cir = try CIR.init(&module_env, "test");
//     defer cir.deinit();
//
//     // Canonicalize
//     var can = try canonicalize.init(&cir, &parse_ast, null);
//     defer can.deinit();
//
//     const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
//     const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
//         std.debug.print("Failed to canonicalize expression\n", .{});
//         return error.CanonicalizeError;
//     };
//
//     // Type check
//     var checker = try check_types.init(allocator, &module_env.types, &cir, &.{}, &cir.store.regions);
//     defer checker.deinit();
//     _ = try checker.checkExpr(canonical_expr_idx);
//
//     // WORKAROUND: Set the type of the if expression manually since type checker doesn't set it
//     const if_expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
//     const if_expr = cir.store.getExpr(canonical_expr_idx);
//     if (if_expr == .e_if) {
//         // For if expressions, use the type of the then branch
//         const branches = cir.store.sliceIfBranches(if_expr.e_if.branches);
//         if (branches.len > 0) {
//             const branch_node_idx = CIR.nodeIdxFrom(branches[0]);
//             const first_branch = cir.store.nodes.get(branch_node_idx);
//             const then_body_idx = cir.store.extra_data.items[first_branch.data_1 + 1];
//             const then_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(then_body_idx));
//             const then_var = @as(types.Var, @enumFromInt(@intFromEnum(then_expr_idx)));
//             const then_resolved = module_env.types.resolveVar(then_var);
//             try module_env.types.setVarContent(if_expr_var, then_resolved.desc.content);
//         }
//     }
//
//     // Create stack
//     var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
//     defer eval_stack.deinit();
//
//     // Create layout store
//     var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
//     defer layout_cache.deinit();
//
//     // Try to evaluate
//     std.debug.print("\n=== Before eval if ===\n", .{});
//     const result = try eval.eval(allocator, &cir, canonical_expr_idx, &eval_stack, &layout_cache, &module_env.types);
//     std.debug.print("=== After eval if ===\n", .{});
//     std.debug.print("result.ptr: {*}\n", .{result.ptr});
//
//     // Read the result
//     const value = @as(*i128, @ptrCast(@alignCast(result.ptr))).*;
//     std.debug.print("Result value: {}\n", .{value});
//     try testing.expectEqual(@as(i128, 1), value);
// }

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

    // Debug: Check if the pointer is within the stack
    const stack_start = @intFromPtr(eval_stack.start);
    const stack_end = stack_start + eval_stack.capacity;
    const result_ptr_addr = @intFromPtr(result.ptr);
    std.debug.print("Stack range: 0x{x} - 0x{x}\n", .{ stack_start, stack_end });
    std.debug.print("Result ptr: 0x{x}\n", .{result_ptr_addr});
    const is_in_stack = result_ptr_addr >= stack_start and result_ptr_addr < stack_end;
    std.debug.print("Result pointer is in stack: {}\n", .{is_in_stack});

    // Clean up in correct order
    std.debug.print("Starting cleanup...\n", .{});

    std.debug.print("Cleaning up layout_cache...\n", .{});
    layout_cache.deinit();

    std.debug.print("Cleaning up eval_stack...\n", .{});
    eval_stack.deinit();

    std.debug.print("Cleaning up checker...\n", .{});
    checker.deinit();

    std.debug.print("Cleaning up can...\n", .{});
    can.deinit();

    std.debug.print("Cleaning up cir...\n", .{});
    cir.deinit();

    std.debug.print("Cleaning up parse_ast...\n", .{});
    parse_ast.deinit(allocator);

    std.debug.print("Freeing owned_source...\n", .{});
    // NOTE: owned_source is owned by module_env and will be freed when module_env.deinit() is called
    // allocator.free(owned_source);

    std.debug.print("Cleaning up module_env...\n", .{});
    module_env.deinit();

    std.debug.print("Cleanup completed successfully\n", .{});
}
