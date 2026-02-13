//! Tests for lambda lifting of polymorphic expressions.
//!
//! These tests verify that the lambda lifter correctly processes the Mono IR
//! produced by lowering polymorphic let-bindings. This is the stage between
//! mono lowering and RC insertion in the pipeline.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const types_mod = @import("types");
const mono = @import("mono");

const helpers = @import("helpers.zig");

const MonoExprStore = mono.MonoExprStore;
const MonoLower = mono.Lower;
const MonoIR = mono.MonoIR;
const MonoExpr = MonoIR.MonoExpr;
const MonoExprId = MonoIR.MonoExprId;
const LambdaLift = mono.LambdaLift;

const ModuleEnv = can.ModuleEnv;

const testing = std.testing;
const test_allocator = helpers.interpreter_allocator;

/// Result of running parse → can → typecheck → mono lower → specializations → lambda lift
const LiftResult = struct {
    mono_store: MonoExprStore,
    lowered_expr_id: MonoExprId,
    lambda_lifter: LambdaLift,
    instantiator: mono.Instantiate,
    layout_store: *layout.Store,
    all_module_envs: [2]*ModuleEnv,
    resources: @TypeOf(helpers.parseAndCanonicalizeExpr(undefined, undefined) catch unreachable),

    fn deinit(self: *LiftResult) void {
        self.lambda_lifter.deinit();
        self.instantiator.deinit();
        self.mono_store.deinit();
        test_allocator.destroy(self.layout_store);
        helpers.cleanupParseAndCanonical(test_allocator, self.resources);
    }
};

fn lowerAndLiftFromSource(source: []const u8) !LiftResult {
    const resources = try helpers.parseAndCanonicalizeExpr(test_allocator, source);
    errdefer helpers.cleanupParseAndCanonical(test_allocator, resources);

    var result: LiftResult = undefined;
    result.resources = resources;
    result.all_module_envs = .{ resources.module_env, @constCast(@as(*const ModuleEnv, resources.builtin_module.env)) };

    // Create global layout store (same as dev evaluator)
    const builtin_str = result.all_module_envs[0].idents.builtin_str;
    result.layout_store = try test_allocator.create(layout.Store);
    errdefer test_allocator.destroy(result.layout_store);
    result.layout_store.* = try layout.Store.init(&result.all_module_envs, builtin_str, test_allocator, base.target.TargetUsize.native);

    result.mono_store = MonoExprStore.init(test_allocator);
    errdefer result.mono_store.deinit();

    result.instantiator = mono.Instantiate.init(test_allocator, &result.all_module_envs);
    errdefer result.instantiator.deinit();

    // Lower CIR to Mono IR
    {
        var lowerer = MonoLower.init(test_allocator, &result.mono_store, &result.all_module_envs, null, result.layout_store, null, null);
        defer lowerer.deinit();
        lowerer.instantiate = &result.instantiator;

        result.lowered_expr_id = try lowerer.lowerExpr(0, resources.expr_idx);
    }

    // Solve specializations (same as dev evaluator)
    while (result.instantiator.nextNeededSpecialization()) |spec| {
        const spec_module_env = result.all_module_envs[@as(usize, spec.module_idx)];

        var spec_lowerer = MonoLower.init(test_allocator, &result.mono_store, &result.all_module_envs, null, result.layout_store, null, null);
        defer spec_lowerer.deinit();

        const def_type_var = ModuleEnv.varFrom(spec.def_expr);
        const def_resolved = spec_module_env.types.resolveVar(def_type_var);

        if (def_resolved.desc.content.unwrapFunc()) |generic_func_type| {
            const generic_param_vars = spec_module_env.types.sliceVars(generic_func_type.args);
            const generic_ret_var = generic_func_type.ret;

            var spec_scope = types_mod.VarMap.init(test_allocator);

            for (generic_param_vars) |generic| {
                try spec_scope.put(generic, types_mod.ModuleVar{
                    .module_idx = spec.module_idx,
                    .var_ = spec.type_var,
                });
            }

            try spec_scope.put(generic_ret_var, types_mod.ModuleVar{
                .module_idx = spec.module_idx,
                .var_ = spec.type_var,
            });

            try spec_lowerer.type_scope.scopes.append(spec_scope);
            spec_lowerer.use_type_scope = true;
        }

        const lowered_spec = try spec_lowerer.lowerExpr(spec.module_idx, spec.def_expr);
        spec.lowered = lowered_spec;

        const symbol_key: u48 = @bitCast(spec.name_new);
        try result.mono_store.symbol_defs.put(symbol_key, lowered_spec);
    }

    // Lambda lifting (same as dev evaluator)
    result.lambda_lifter = LambdaLift.init(
        test_allocator,
        &result.mono_store,
        &result.all_module_envs,
        null,
        result.layout_store,
        null,
    );
    errdefer result.lambda_lifter.deinit();
    try result.lambda_lifter.liftAllLambdas();

    return result;
}

test "lambda lift: polymorphic identity with if-else - lifting succeeds" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerAndLiftFromSource(source);
    defer result.deinit();

    // Lambda lifting should succeed without errors (we got here)
    // The top-level expression should still be a block
    const top_expr = result.mono_store.getExpr(result.lowered_expr_id);
    try testing.expect(top_expr == .block);
}

test "lambda lift: polymorphic identity with if-else - block structure preserved" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerAndLiftFromSource(source);
    defer result.deinit();

    const top_expr = result.mono_store.getExpr(result.lowered_expr_id);
    const block = top_expr.block;

    // Block result layout should still be Str after lifting
    try testing.expectEqual(layout.Idx.str, block.result_layout);

    // Final expression should still be if_then_else
    const final_expr = result.mono_store.getExpr(block.final_expr);
    try testing.expect(final_expr == .if_then_else);

    // if_then_else result layout should still be Str
    try testing.expectEqual(layout.Idx.str, final_expr.if_then_else.result_layout);
}

test "lambda lift: polymorphic identity with if-else - call return layouts preserved" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerAndLiftFromSource(source);
    defer result.deinit();

    const top_expr = result.mono_store.getExpr(result.lowered_expr_id);
    const block = top_expr.block;
    const stmts = result.mono_store.getStmts(block.stmts);

    var found_dec_call = false;
    var found_str_call = false;

    for (stmts) |stmt| {
        const expr = result.mono_store.getExpr(stmt.expr);
        switch (expr) {
            .call => |call_expr| {
                if (call_expr.ret_layout == .dec) found_dec_call = true;
                if (call_expr.ret_layout == .str) found_str_call = true;
            },
            else => {},
        }
    }

    // identity(5) should still have ret_layout = Dec after lifting
    try testing.expect(found_dec_call);
    // identity("Hello") should still have ret_layout = Str after lifting
    try testing.expect(found_str_call);
}

test "lambda lift: polymorphic identity with if-else - specialization lambdas lifted" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerAndLiftFromSource(source);
    defer result.deinit();

    // Check that the specialization lambdas were lifted
    const specs = result.instantiator.solvedSpecializations();
    try testing.expectEqual(@as(usize, 2), specs.len);

    var dec_spec_lifted = false;
    var str_spec_lifted = false;

    for (specs) |spec| {
        const lowered_id = spec.lowered.?;
        const lowered_expr = result.mono_store.getExpr(lowered_id);

        switch (lowered_expr) {
            .lambda => |lambda| {
                const is_lifted = result.lambda_lifter.isLiftedLambda(lowered_id);
                if (lambda.ret_layout == .dec) {
                    dec_spec_lifted = is_lifted;
                }
                if (lambda.ret_layout == .str) {
                    str_spec_lifted = is_lifted;
                }
            },
            else => {},
        }
    }

    // Both specializations should have their lambdas lifted
    // (identity has no captures, so these are simple lifts)
    try testing.expect(dec_spec_lifted);
    try testing.expect(str_spec_lifted);
}
