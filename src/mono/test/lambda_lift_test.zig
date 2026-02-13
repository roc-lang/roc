//! Unit tests for lambda lifting pass
//!
//! Tests verify that the lambda lifting algorithm correctly:
//! - Identifies all lambdas in expressions
//! - Computes free variables (captures) accurately
//! - Deduplicates captures
//! - Generates unique lifted symbols
//! - Maintains structural invariants

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const layout_mod = @import("layout");
const mono = @import("../mod.zig");
const can = @import("can");

const Allocator = std.mem.Allocator;
const LayoutIdx = layout_mod.Idx;
const MonoPatternId = mono.MonoPatternId;
const MonoSymbol = mono.MonoSymbol;
const MonoExprStore = mono.MonoExprStore;
const LambdaLift = mono.LambdaLift;

const dummy_region = base.Region{
    .start = .{ .offset = 0 },
    .end = .{ .offset = 0 },
};

/// Helper to create a test symbol
fn testSymbol(module_idx: u16, ident_idx: u29) MonoSymbol {
    return MonoSymbol{
        .module_idx = module_idx,
        .ident_idx = base.Ident.Idx{
            .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
            .idx = ident_idx,
        },
    };
}

test "lambda lifting: simple lambda with no captures" {
    const allocator = testing.allocator;

    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Create a lambda: |x| x + 1
    // This has no free variables (x is a parameter)
    const param_pattern = try store.addPattern(.{ .bind = .{
        .symbol = testSymbol(0, 100),
        .layout_idx = .i64,
    } }, dummy_region);
    const param_span = try store.addPatternSpan(&[_]MonoPatternId{param_pattern});

    const one_literal_id = try store.addExpr(.{ .i64_literal = 1 }, dummy_region);
    const x_lookup_id = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 100),
        .layout_idx = .i64,
    } }, dummy_region);

    const add_binop_id = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = x_lookup_id,
        .rhs = one_literal_id,
        .result_layout = .i64,
    } }, dummy_region);

    const lambda_id = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = param_span,
        .body = add_binop_id,
        .ret_layout = .i64,
    } }, dummy_region);

    // Run lambda lifting
    var lifter = LambdaLift.init(allocator, &store, &[_]*can.ModuleEnv{}, null, null, null);
    defer lifter.deinit();
    try lifter.liftAllLambdas();

    // Verify lambda was lifted
    try testing.expect(lifter.isLiftedLambda(lambda_id));

    // Verify it has no captures (no free variables)
    const lifted = lifter.getLiftedLambda(lambda_id) orelse return error.TestExpectedNonNull;
    try testing.expectEqual(@as(usize, 0), lifted.captures.len);
}

test "lambda lifting: lambda with single capture" {
    const allocator = testing.allocator;

    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Create: |x| y + x
    // where y is captured from outer scope
    const param_pattern = try store.addPattern(.{
        .bind = .{
            .symbol = testSymbol(0, 100), // x (parameter)
            .layout_idx = .i64,
        },
    }, dummy_region);
    const param_span = try store.addPatternSpan(&[_]MonoPatternId{param_pattern});

    // y lookup (free variable - captured)
    const y_lookup_id = try store.addExpr(.{
        .lookup = .{
            .symbol = testSymbol(0, 200), // y (free variable)
            .layout_idx = .i64,
        },
    }, dummy_region);

    // x lookup (parameter)
    const x_lookup_id = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 100),
        .layout_idx = .i64,
    } }, dummy_region);

    // y + x
    const add_id = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = y_lookup_id,
        .rhs = x_lookup_id,
        .result_layout = .i64,
    } }, dummy_region);

    const lambda_id = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = param_span,
        .body = add_id,
        .ret_layout = .i64,
    } }, dummy_region);

    // Run lambda lifting
    var lifter = LambdaLift.init(allocator, &store, &[_]*can.ModuleEnv{}, null, null, null);
    defer lifter.deinit();
    try lifter.liftAllLambdas();

    // Verify lambda was lifted
    try testing.expect(lifter.isLiftedLambda(lambda_id));

    // Verify it has one capture (y)
    const lifted = lifter.getLiftedLambda(lambda_id) orelse return error.TestExpectedNonNull;
    try testing.expectEqual(@as(usize, 1), lifted.captures.len);
    try testing.expect(lifted.captures[0].symbol.ident_idx.idx == 200);
}

test "lambda lifting: lambda with multiple captures" {
    const allocator = testing.allocator;

    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Create: |x| y + z + x
    // where y and z are captured
    const param_pattern = try store.addPattern(.{ .bind = .{ .symbol = testSymbol(0, 100), .layout_idx = .i64 } }, dummy_region);
    const param_span = try store.addPatternSpan(&[_]MonoPatternId{param_pattern});

    const y_lookup = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 200),
        .layout_idx = .i64,
    } }, dummy_region);

    const z_lookup = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 300),
        .layout_idx = .i64,
    } }, dummy_region);

    const x_lookup = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 100),
        .layout_idx = .i64,
    } }, dummy_region);

    // y + z
    const y_plus_z = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = y_lookup,
        .rhs = z_lookup,
        .result_layout = .i64,
    } }, dummy_region);

    // (y + z) + x
    const final = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = y_plus_z,
        .rhs = x_lookup,
        .result_layout = .i64,
    } }, dummy_region);

    const lambda_id = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = param_span,
        .body = final,
        .ret_layout = .i64,
    } }, dummy_region);

    var lifter = LambdaLift.init(allocator, &store, &[_]*can.ModuleEnv{}, null, null, null);
    defer lifter.deinit();
    try lifter.liftAllLambdas();

    try testing.expect(lifter.isLiftedLambda(lambda_id));

    const lifted = lifter.getLiftedLambda(lambda_id) orelse return error.TestExpectedNonNull;
    try testing.expectEqual(@as(usize, 2), lifted.captures.len);
}

test "lambda lifting: nested lambda captures outer scope variables" {
    const allocator = testing.allocator;

    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Create outer lambda: |x| inner
    // where inner = |y| x + y

    // Inner lambda parameter (y)
    const inner_param = try store.addPattern(.{ .bind = .{ .symbol = testSymbol(0, 101), .layout_idx = .i64 } }, dummy_region);
    const inner_param_span = try store.addPatternSpan(&[_]MonoPatternId{inner_param});

    // x lookup (captured from outer)
    const x_from_inner = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 100),
        .layout_idx = .i64,
    } }, dummy_region);

    // y lookup (inner parameter)
    const y_from_inner = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 101),
        .layout_idx = .i64,
    } }, dummy_region);

    // x + y
    const inner_body = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = x_from_inner,
        .rhs = y_from_inner,
        .result_layout = .i64,
    } }, dummy_region);

    // Inner lambda definition
    const inner_lambda = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = inner_param_span,
        .body = inner_body,
        .ret_layout = .i64,
    } }, dummy_region);

    // Outer lambda parameter (x)
    const outer_param = try store.addPattern(.{ .bind = .{
        .symbol = testSymbol(0, 100),
        .layout_idx = .i64,
    } }, dummy_region);
    const outer_param_span = try store.addPatternSpan(&[_]MonoPatternId{outer_param});

    // Outer lambda body is just the inner lambda expression
    const outer_lambda = try store.addExpr(.{
        .lambda = .{
            .fn_layout = .zst,
            .params = outer_param_span,
            .body = inner_lambda,
            .ret_layout = .zst, // Returns a function
        },
    }, dummy_region);

    var lifter = LambdaLift.init(allocator, &store, &[_]*can.ModuleEnv{}, null, null, null);
    defer lifter.deinit();
    try lifter.liftAllLambdas();

    // Both lambdas should be lifted
    try testing.expect(lifter.isLiftedLambda(outer_lambda));
    try testing.expect(lifter.isLiftedLambda(inner_lambda));

    // Outer lambda has no captures (x is its parameter)
    const outer_lifted = lifter.getLiftedLambda(outer_lambda) orelse return error.TestExpectedNonNull;
    try testing.expectEqual(@as(usize, 0), outer_lifted.captures.len);

    // Inner lambda has one capture (x from outer scope)
    const inner_lifted = lifter.getLiftedLambda(inner_lambda) orelse return error.TestExpectedNonNull;
    try testing.expectEqual(@as(usize, 1), inner_lifted.captures.len);
}

test "lambda lifting: no duplicate captures" {
    const allocator = testing.allocator;

    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Create: |x| y + y + z
    // where y appears twice but should only be captured once
    const param_pattern = try store.addPattern(.{ .bind = .{ .layout_idx = .i64, .symbol = testSymbol(0, 100) } }, dummy_region);
    const param_span = try store.addPatternSpan(&[_]MonoPatternId{param_pattern});

    const y_first = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 200),
        .layout_idx = .i64,
    } }, dummy_region);

    const y_second = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 200),
        .layout_idx = .i64,
    } }, dummy_region);

    const z_lookup = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 300),
        .layout_idx = .i64,
    } }, dummy_region);

    // y + y
    const y_plus_y = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = y_first,
        .rhs = y_second,
        .result_layout = .i64,
    } }, dummy_region);

    // (y + y) + z
    const final = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = y_plus_y,
        .rhs = z_lookup,
        .result_layout = .i64,
    } }, dummy_region);

    const lambda_id = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = param_span,
        .body = final,
        .ret_layout = .i64,
    } }, dummy_region);

    var lifter = LambdaLift.init(allocator, &store, &[_]*can.ModuleEnv{}, null, null, null);
    defer lifter.deinit();
    try lifter.liftAllLambdas();

    const lifted = lifter.getLiftedLambda(lambda_id) orelse return error.TestExpectedNonNull;

    // Should have 2 captures (y and z), not 3
    try testing.expectEqual(@as(usize, 2), lifted.captures.len);

    // Verify the captures are y and z
    var seen_y = false;
    var seen_z = false;
    for (lifted.captures) |cap| {
        if (cap.symbol.ident_idx.idx == 200) seen_y = true;
        if (cap.symbol.ident_idx.idx == 300) seen_z = true;
    }
    try testing.expect(seen_y and seen_z);
}

test "lambda lifting: lifted symbols are unique" {
    const allocator = testing.allocator;

    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Create two separate lambdas
    const param1 = try store.addPattern(.{ .bind = .{ .layout_idx = .i64, .symbol = testSymbol(0, 100) } }, dummy_region);
    const param_span1 = try store.addPatternSpan(&[_]MonoPatternId{param1});

    const one = try store.addExpr(.{ .i64_literal = 1 }, dummy_region);
    const x1 = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 100),
        .layout_idx = .i64,
    } }, dummy_region);

    const body1 = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = x1,
        .rhs = one,
        .result_layout = .i64,
    } }, dummy_region);

    const lambda1 = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = param_span1,
        .body = body1,
        .ret_layout = .i64,
    } }, dummy_region);

    // Second lambda (slightly different)
    const param2 = try store.addPattern(.{ .bind = .{
        .symbol = testSymbol(0, 101),
        .layout_idx = .i64,
    } }, dummy_region);
    const param_span2 = try store.addPatternSpan(&[_]MonoPatternId{param2});

    const two = try store.addExpr(.{ .i64_literal = 2 }, dummy_region);
    const x2 = try store.addExpr(.{ .lookup = .{
        .symbol = testSymbol(0, 101),
        .layout_idx = .i64,
    } }, dummy_region);

    const body2 = try store.addExpr(.{ .binop = .{
        .op = .mul,
        .lhs = x2,
        .rhs = two,
        .result_layout = .i64,
    } }, dummy_region);

    const lambda2 = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = param_span2,
        .body = body2,
        .ret_layout = .i64,
    } }, dummy_region);

    var lifter = LambdaLift.init(allocator, &store, &[_]*can.ModuleEnv{}, null, null, null);
    defer lifter.deinit();
    try lifter.liftAllLambdas();

    const lifted1 = lifter.getLiftedLambda(lambda1) orelse return error.TestExpectedNonNull;
    const lifted2 = lifter.getLiftedLambda(lambda2) orelse return error.TestExpectedNonNull;

    // Verify the symbols are different
    try testing.expect(!MonoSymbol.eql(lifted1.lifted_symbol, lifted2.lifted_symbol));
}

test "lambda lifting: no nested lambdas after lifting" {
    const allocator = testing.allocator;

    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Create nested lambda structure
    const inner_param = try store.addPattern(.{ .bind = .{ .symbol = testSymbol(0, 200), .layout_idx = .i64 } }, dummy_region);
    const inner_param_span = try store.addPatternSpan(&[_]MonoPatternId{inner_param});

    const inner_body = try store.addExpr(.{ .i64_literal = 42 }, dummy_region);

    const inner_lambda = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = inner_param_span,
        .body = inner_body,
        .ret_layout = .i64,
    } }, dummy_region);

    // Outer lambda returns the inner lambda
    const outer_param = try store.addPattern(.{ .bind = .{
        .symbol = testSymbol(0, 100),
        .layout_idx = .i64,
    } }, dummy_region);
    const outer_param_span = try store.addPatternSpan(&[_]MonoPatternId{outer_param});

    const outer_lambda = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = outer_param_span,
        .body = inner_lambda,
        .ret_layout = .zst,
    } }, dummy_region);

    var lifter = LambdaLift.init(allocator, &store, &[_]*can.ModuleEnv{}, null, null, null);
    defer lifter.deinit();
    try lifter.liftAllLambdas();

    // Both should be lifted
    try testing.expect(lifter.isLiftedLambda(outer_lambda));
    try testing.expect(lifter.isLiftedLambda(inner_lambda));

    // Verify outer lambda body - if it's a lambda, it should be marked as lifted
    const outer_lifted = lifter.getLiftedLambda(outer_lambda) orelse return error.TestExpectedNonNull;
    const outer_body = store.getExpr(outer_lifted.body);
    if (outer_body == .lambda) {
        // If outer body is still a lambda, it should be marked as lifted
        try testing.expect(lifter.isLiftedLambda(outer_lifted.body));
    }
}

test "lambda lifting: captures type information" {
    const allocator = testing.allocator;

    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Create: |x| y + x where y has layout f64
    const param = try store.addPattern(.{ .bind = .{ .layout_idx = .i64, .symbol = testSymbol(0, 100) } }, dummy_region);
    const param_span = try store.addPatternSpan(&[_]MonoPatternId{param});

    const y_lookup = try store.addExpr(.{
        .lookup = .{
            .symbol = testSymbol(0, 200),
            .layout_idx = .f64, // y is f64
        },
    }, dummy_region);

    const x_lookup = try store.addExpr(.{
        .lookup = .{
            .symbol = testSymbol(0, 100),
            .layout_idx = .i64, // x is i64
        },
    }, dummy_region);

    const body = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = y_lookup,
        .rhs = x_lookup,
        .result_layout = .i64,
    } }, dummy_region);

    const lambda_id = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = param_span,
        .body = body,
        .ret_layout = .i64,
    } }, dummy_region);

    var lifter = LambdaLift.init(allocator, &store, &[_]*can.ModuleEnv{}, null, null, null);
    defer lifter.deinit();
    try lifter.liftAllLambdas();

    const lifted = lifter.getLiftedLambda(lambda_id) orelse return error.TestExpectedNonNull;

    // Verify capture has the correct layout (f64 for y)
    try testing.expectEqual(@as(usize, 1), lifted.captures.len);
    try testing.expectEqual(@as(LayoutIdx, .f64), lifted.captures[0].layout_idx);
}
