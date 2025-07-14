//! Tests for the eval module to verify it correctly uses real layouts from the type checker

const std = @import("std");
const testing = std.testing;
const CIR = @import("../check/canonicalize/CIR.zig");
const eval = @import("eval.zig");
const stack = @import("stack.zig");
const layout = @import("../layout/layout.zig");
const layout_store = @import("../layout/store.zig");
const types = @import("../types/types.zig");
const type_store = @import("../types/store.zig");
const base = @import("../base.zig");
const target = @import("../base/target.zig");

test "eval uses real layouts - integer literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create module environment
    var env = base.ModuleEnv.init(allocator, "test");
    defer env.deinit();

    // Create CIR
    var cir = CIR.init(&env, "test");
    defer cir.deinit();

    // Create type store
    var types_store = type_store.Store.init(allocator);
    defer types_store.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(&env, &types_store);
    defer layout_cache.deinit();

    // Add an integer literal expression with u8 type
    const int_expr = CIR.Expr{ .e_int = .{ .value = CIR.IntValue.fromI128(42) } };
    const int_content = types.Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } };
    const expr_idx = try cir.addExprAndTypeVar(int_expr, int_content, .{ .start = .{ .offset = 0 }, .end = .{ .offset = 2 } });

    // Create eval stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Evaluate the expression
    const result = try eval.eval(allocator, &cir, expr_idx, &eval_stack, &layout_cache, &types_store);

    // Verify the layout is correct (should be u8, not the default i128)
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.layout.data.scalar.tag == .int);
    try testing.expect(result.layout.data.scalar.data.int == .u8);

    // Verify the value is correct
    const value_ptr = @as(*u8, @ptrCast(@alignCast(result.ptr)));
    try testing.expectEqual(@as(u8, 42), value_ptr.*);
}

test "eval uses real layouts - float literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create module environment
    var env = base.ModuleEnv.init(allocator, "test");
    defer env.deinit();

    // Create CIR
    var cir = CIR.init(&env, "test");
    defer cir.deinit();

    // Create type store
    var types_store = type_store.Store.init(allocator);
    defer types_store.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(&env, &types_store);
    defer layout_cache.deinit();

    // Add a float literal expression with f64 type
    const float_expr = CIR.Expr{ .e_frac_f64 = .{ .value = 3.14 } };
    const float_content = types.Content{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } };
    const expr_idx = try cir.addExprAndTypeVar(float_expr, float_content, .{ .start = .{ .offset = 0 }, .end = .{ .offset = 4 } });

    // Create eval stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Evaluate the expression
    const result = try eval.eval(allocator, &cir, expr_idx, &eval_stack, &layout_cache, &types_store);

    // Verify the layout is correct (should be f64)
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.layout.data.scalar.tag == .frac);
    try testing.expect(result.layout.data.scalar.data.frac == .f64);

    // Verify the value is correct
    const value_ptr = @as(*f64, @ptrCast(@alignCast(result.ptr)));
    try testing.expectApproxEqAbs(@as(f64, 3.14), value_ptr.*, 0.001);
}

test "eval uses real layouts - empty record" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create module environment
    var env = base.ModuleEnv.init(allocator, "test");
    defer env.deinit();

    // Create CIR
    var cir = CIR.init(&env, "test");
    defer cir.deinit();

    // Create type store
    var types_store = type_store.Store.init(allocator);
    defer types_store.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(&env, &types_store);
    defer layout_cache.deinit();

    // Add empty record expression
    const record_expr = CIR.Expr{ .e_empty_record = .{} };
    const record_content = types.Content{ .structure = .{ .empty_record = {} } };
    const expr_idx = try cir.addExprAndTypeVar(record_expr, record_content, .{ .start = .{ .offset = 0 }, .end = .{ .offset = 2 } });

    // Create eval stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Evaluate the expression
    const result = try eval.eval(allocator, &cir, expr_idx, &eval_stack, &layout_cache, &types_store);

    // Verify the layout is a record with size 0
    try testing.expect(result.layout.tag == .record);
    const record_data = layout_cache.getRecordData(result.layout.data.record.idx);
    try testing.expectEqual(@as(u32, 0), record_data.size);
}

test "eval uses real layouts - binop addition" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create module environment
    var env = base.ModuleEnv.init(allocator, "test");
    defer env.deinit();

    // Create CIR
    var cir = CIR.init(&env, "test");
    defer cir.deinit();

    // Create type store
    var types_store = type_store.Store.init(allocator);
    defer types_store.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(&env, &types_store);
    defer layout_cache.deinit();

    // Add left operand: 5u8
    const left_expr = CIR.Expr{ .e_int = .{ .value = CIR.IntValue.fromI128(5) } };
    const u8_content = types.Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } };
    const left_idx = try cir.addExprAndTypeVar(left_expr, u8_content, .{ .start = .{ .offset = 0 }, .end = .{ .offset = 1 } });

    // Add right operand: 3u8
    const right_expr = CIR.Expr{ .e_int = .{ .value = CIR.IntValue.fromI128(3) } };
    const right_idx = try cir.addExprAndTypeVar(right_expr, u8_content, .{ .start = .{ .offset = 2 }, .end = .{ .offset = 3 } });

    // Add binop expression
    const binop_expr = CIR.Expr{
        .e_binop = .{
            .op = .add,
            .lhs = left_idx,
            .rhs = right_idx,
        },
    };
    const binop_idx = try cir.addExprAndTypeVar(binop_expr, u8_content, .{ .start = .{ .offset = 0 }, .end = .{ .offset = 3 } });

    // No need to update whole_expr_idx as it doesn't exist in Binop

    // Create eval stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Evaluate the expression
    const result = try eval.eval(allocator, &cir, binop_idx, &eval_stack, &layout_cache, &types_store);

    // Verify the layout is u8
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.layout.data.scalar.tag == .int);
    try testing.expect(result.layout.data.scalar.data.int == .u8);

    // Verify the result is 8
    const value_ptr = @as(*u8, @ptrCast(@alignCast(result.ptr)));
    try testing.expectEqual(@as(u8, 8), value_ptr.*);
}
