//! Unit tests for the Roc emitter
//!
//! These tests verify that the emitter correctly converts CIR expressions
//! to valid Roc source code using manually constructed CIR nodes.

const std = @import("std");
const base = @import("base");
const types = @import("types");

const Emitter = @import("../RocEmitter.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const CIR = @import("../CIR.zig");

const testing = std.testing;
const test_allocator = testing.allocator;

fn createTestEnv(allocator: std.mem.Allocator, source: []const u8) !*ModuleEnv {
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);
    return module_env;
}

fn destroyTestEnv(allocator: std.mem.Allocator, module_env: *ModuleEnv) void {
    module_env.deinit();
    allocator.destroy(module_env);
}

// Basic expression tests

test "emit integer literal" {
    const module_env = try createTestEnv(test_allocator, "42");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const int_value = CIR.IntValue{
        .bytes = @bitCast(@as(i128, 42)),
        .kind = .i128,
    };
    const expr_idx = try module_env.store.addExpr(.{
        .e_num = .{ .value = int_value, .kind = .i64 },
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("42", emitter.getOutput());
}

test "emit negative integer" {
    const module_env = try createTestEnv(test_allocator, "-123");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const int_value = CIR.IntValue{
        .bytes = @bitCast(@as(i128, -123)),
        .kind = .i128,
    };
    const expr_idx = try module_env.store.addExpr(.{
        .e_num = .{ .value = int_value, .kind = .i64 },
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("-123", emitter.getOutput());
}

test "emit empty record" {
    const module_env = try createTestEnv(test_allocator, "{}");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const expr_idx = try module_env.store.addExpr(.{
        .e_empty_record = .{},
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("{}", emitter.getOutput());
}

test "emit empty list" {
    const module_env = try createTestEnv(test_allocator, "[]");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const expr_idx = try module_env.store.addExpr(.{
        .e_empty_list = .{},
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("[]", emitter.getOutput());
}

test "emit identity lambda" {
    const module_env = try createTestEnv(test_allocator, "|x| x");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    // Create pattern for 'x'
    const x_ident = try module_env.insertIdent(base.Ident.for_text("x"));
    const x_pattern_idx = try module_env.store.addPattern(.{
        .assign = .{ .ident = x_ident },
    }, base.Region.zero());

    // Create lookup expression for body
    const body_idx = try module_env.store.addExpr(.{
        .e_lookup_local = .{ .pattern_idx = x_pattern_idx },
    }, base.Region.zero());

    // Create lambda expression using scratch system
    const start = module_env.store.scratchPatternTop();
    try module_env.store.addScratchPattern(x_pattern_idx);
    const args_span = try module_env.store.patternSpanFrom(start);

    const lambda_idx = try module_env.store.addExpr(.{
        .e_lambda = .{ .args = args_span, .body = body_idx },
    }, base.Region.zero());

    try emitter.emitExpr(lambda_idx);
    try testing.expectEqualStrings("|x| x", emitter.getOutput());
}

test "emit tag with no arguments" {
    const module_env = try createTestEnv(test_allocator, "True");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const true_ident = try module_env.insertIdent(base.Ident.for_text("True"));
    const expr_idx = try module_env.store.addExpr(.{
        .e_zero_argument_tag = .{
            .closure_name = true_ident,
            .variant_var = undefined, // not read by emitter
            .ext_var = undefined, // not read by emitter
            .name = true_ident,
        },
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("True", emitter.getOutput());
}

test "emit list with elements" {
    const module_env = try createTestEnv(test_allocator, "[1, 2]");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    // Create element expressions
    const int_value_1 = CIR.IntValue{
        .bytes = @bitCast(@as(i128, 1)),
        .kind = .i128,
    };
    const elem1_idx = try module_env.store.addExpr(.{
        .e_num = .{ .value = int_value_1, .kind = .i64 },
    }, base.Region.zero());

    const int_value_2 = CIR.IntValue{
        .bytes = @bitCast(@as(i128, 2)),
        .kind = .i128,
    };
    const elem2_idx = try module_env.store.addExpr(.{
        .e_num = .{ .value = int_value_2, .kind = .i64 },
    }, base.Region.zero());

    // Create list using scratch system
    const start = module_env.store.scratchExprTop();
    try module_env.store.addScratchExpr(elem1_idx);
    try module_env.store.addScratchExpr(elem2_idx);
    const elems_span = try module_env.store.exprSpanFrom(start);

    const list_idx = try module_env.store.addExpr(.{
        .e_list = .{ .elems = elems_span },
    }, base.Region.zero());

    try emitter.emitExpr(list_idx);
    try testing.expectEqualStrings("[1, 2]", emitter.getOutput());
}

test "emit function application" {
    const module_env = try createTestEnv(test_allocator, "f(42)");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    // Create pattern for 'f' (the function we're calling)
    const f_ident = try module_env.insertIdent(base.Ident.for_text("f"));
    const f_pattern_idx = try module_env.store.addPattern(.{
        .assign = .{ .ident = f_ident },
    }, base.Region.zero());

    // Create function expression (lookup of f)
    const func_idx = try module_env.store.addExpr(.{
        .e_lookup_local = .{ .pattern_idx = f_pattern_idx },
    }, base.Region.zero());

    // Create argument expression
    const int_value = CIR.IntValue{
        .bytes = @bitCast(@as(i128, 42)),
        .kind = .i128,
    };
    const arg_idx = try module_env.store.addExpr(.{
        .e_num = .{ .value = int_value, .kind = .i64 },
    }, base.Region.zero());

    // Create call expression using scratch system
    const start = module_env.store.scratchExprTop();
    try module_env.store.addScratchExpr(arg_idx);
    const args_span = try module_env.store.exprSpanFrom(start);

    const call_idx = try module_env.store.addExpr(.{
        .e_call = .{
            .func = func_idx,
            .args = args_span,
            .called_via = .apply,
        },
    }, base.Region.zero());

    try emitter.emitExpr(call_idx);
    try testing.expectEqualStrings("f(42)", emitter.getOutput());
}
