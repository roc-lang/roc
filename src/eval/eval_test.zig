const std = @import("std");
const testing = std.testing;
const eval = @import("eval.zig");
const CIR = @import("../check/canonicalize/CIR.zig");
const base = @import("../base.zig");
const types = @import("../types.zig");

const test_allocator = testing.allocator;

test "eval string segment - already primitive" {
    var module_env = base.ModuleEnv.init(test_allocator);
    defer module_env.deinit();

    var cir = CIR.init(&module_env);
    defer cir.deinit();

    // Create a string segment node
    const str_segment_idx = cir.store.addExpr(.{ .str_segment = .{
        .literal = cir.env.strings.insert(test_allocator, "Hello, World!"),
        .region = base.Region.zero(),
    } });

    // Evaluating a string segment should return the same index (it's already primitive)
    const evaluated = try eval.eval(test_allocator, &cir, str_segment_idx);
    try testing.expectEqual(str_segment_idx, evaluated);
}

test "eval string literal - already primitive" {
    var module_env = base.ModuleEnv.init(test_allocator);
    defer module_env.deinit();

    var cir = CIR.init(&module_env);
    defer cir.deinit();

    // Create a string segment node first
    const str_segment_idx = cir.store.addExpr(.{ .str_segment = .{
        .literal = cir.env.strings.insert(test_allocator, "Hello"),
        .region = base.Region.zero(),
    } });

    // Create a string node that references the segment
    const str_expr_idx = cir.store.addExpr(.{ .str = .{
        .span = .{ .span = .{ .start = @intFromEnum(str_segment_idx), .len = 1 } },
        .region = base.Region.zero(),
    } });

    // Evaluating a string literal should return the same index (it's already primitive)
    const evaluated = try eval.eval(test_allocator, &cir, str_expr_idx);
    try testing.expectEqual(str_expr_idx, evaluated);
}

test "eval runtime error - returns crash error" {
    var module_env = base.ModuleEnv.init(test_allocator);
    defer module_env.deinit();

    var cir = CIR.init(&module_env);
    defer cir.deinit();

    // Create a diagnostic
    const diagnostic_idx = cir.store.addDiagnostic(.{ .not_implemented = .{
        .feature = cir.env.strings.insert(test_allocator, "test feature"),
        .region = base.Region.zero(),
    } });

    // Create a runtime error node
    const runtime_error_idx = cir.store.addExpr(.{ .runtime_error = .{
        .diagnostic = diagnostic_idx,
        .region = base.Region.zero(),
    } });

    // Evaluating a runtime error should return an error
    const result = eval.eval(test_allocator, &cir, runtime_error_idx);
    try testing.expectError(eval.EvalError.Crash, result);
}

test "eval tag - already primitive" {
    var module_env = base.ModuleEnv.init(test_allocator);
    defer module_env.deinit();

    var cir = CIR.init(&module_env);
    defer cir.deinit();

    // Create a tag node
    const tag_name: base.Ident.Idx = @bitCast(@as(u32, 42));

    const tag_idx = cir.store.addExpr(.{
        .tag = .{
            .ext_var = @enumFromInt(0),
            .name = tag_name,
            .args = .{ .span = .{ .start = 0, .len = 0 } }, // Empty args
            .region = base.Region.zero(),
        },
    });

    // Evaluating a tag should return the same index (for now, not yet implemented)
    const evaluated = try eval.eval(test_allocator, &cir, tag_idx);
    try testing.expectEqual(tag_idx, evaluated);
}

test "eval binop - not yet implemented" {
    var module_env = base.ModuleEnv.init(test_allocator);
    defer module_env.deinit();

    var cir = CIR.init(&module_env);
    defer cir.deinit();

    // Create two string segments as operands
    const lhs_idx = cir.store.addExpr(.{ .str_segment = .{
        .literal = cir.env.strings.insert(test_allocator, "5"),
        .region = base.Region.zero(),
    } });

    const rhs_idx = cir.store.addExpr(.{ .str_segment = .{
        .literal = cir.env.strings.insert(test_allocator, "3"),
        .region = base.Region.zero(),
    } });

    // Create a binop node
    const binop_idx = cir.store.addExpr(.{ .binop = CIR.Expr.Binop.init(.add, lhs_idx, rhs_idx, base.Region.zero()) });

    // For now, binop evaluation is not implemented, so it should return the same index
    const evaluated = try eval.eval(test_allocator, &cir, binop_idx);
    try testing.expectEqual(binop_idx, evaluated);
}

test "eval call - not yet implemented" {
    var module_env = base.ModuleEnv.init(test_allocator);
    defer module_env.deinit();

    var cir = CIR.init(&module_env);
    defer cir.deinit();

    // Create a call node with empty args
    const call_idx = cir.store.addExpr(.{ .call = .{
        .args = .{ .span = .{ .start = 0, .len = 0 } },
        .called_via = .apply,
        .region = base.Region.zero(),
    } });

    // For now, call evaluation is not implemented, so it should return the same index
    const evaluated = try eval.eval(test_allocator, &cir, call_idx);
    try testing.expectEqual(call_idx, evaluated);
}

test "eval multiple string segments" {
    var module_env = base.ModuleEnv.init(test_allocator);
    defer module_env.deinit();

    var cir = CIR.init(&module_env);
    defer cir.deinit();

    // Create multiple string segments
    const seg1_idx = cir.store.addExpr(.{ .str_segment = .{
        .literal = cir.env.strings.insert(test_allocator, "Hello, "),
        .region = base.Region.zero(),
    } });

    _ = cir.store.addExpr(.{ .str_segment = .{
        .literal = cir.env.strings.insert(test_allocator, "World!"),
        .region = base.Region.zero(),
    } });

    // Create a string that spans both segments
    const str_expr_idx = cir.store.addExpr(.{ .str = .{
        .span = .{ .span = .{ .start = @intFromEnum(seg1_idx), .len = 2 } },
        .region = base.Region.zero(),
    } });

    // Evaluating should return the same index (it's already primitive)
    const evaluated = try eval.eval(test_allocator, &cir, str_expr_idx);
    try testing.expectEqual(str_expr_idx, evaluated);
}
