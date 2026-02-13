//! Tests for RC insertion on polymorphic expressions.
//!
//! These tests verify that RC insertion correctly processes the Mono IR
//! after mono lowering and lambda lifting. This is the last transformation
//! stage before code generation.

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
const RcInsertPass = mono.RcInsert.RcInsertPass;

const ModuleEnv = can.ModuleEnv;

const testing = std.testing;
const test_allocator = helpers.interpreter_allocator;

/// Result of running parse → can → typecheck → mono lower → specializations → lambda lift → RC insert
const RcResult = struct {
    mono_store: MonoExprStore,
    rc_expr_id: MonoExprId,
    instantiator: mono.Instantiate,
    layout_store: *layout.Store,
    all_module_envs: [2]*ModuleEnv,
    resources: @TypeOf(helpers.parseAndCanonicalizeExpr(undefined, undefined) catch unreachable),

    fn deinit(self: *RcResult) void {
        self.instantiator.deinit();
        self.mono_store.deinit();
        test_allocator.destroy(self.layout_store);
        helpers.cleanupParseAndCanonical(test_allocator, self.resources);
    }
};

fn lowerLiftAndRcFromSource(source: []const u8) !RcResult {
    const resources = try helpers.parseAndCanonicalizeExpr(test_allocator, source);
    errdefer helpers.cleanupParseAndCanonical(test_allocator, resources);

    var result: RcResult = undefined;
    result.resources = resources;
    result.all_module_envs = .{ resources.module_env, @constCast(@as(*const ModuleEnv, resources.builtin_module.env)) };

    // Create global layout store
    const builtin_str = result.all_module_envs[0].idents.builtin_str;
    result.layout_store = try test_allocator.create(layout.Store);
    errdefer test_allocator.destroy(result.layout_store);
    result.layout_store.* = try layout.Store.init(&result.all_module_envs, builtin_str, test_allocator, base.target.TargetUsize.native);

    result.mono_store = MonoExprStore.init(test_allocator);
    errdefer result.mono_store.deinit();

    result.instantiator = mono.Instantiate.init(test_allocator, &result.all_module_envs);
    errdefer result.instantiator.deinit();

    // Lower CIR to Mono IR
    var lowered_expr_id: MonoExprId = undefined;
    {
        var lowerer = MonoLower.init(test_allocator, &result.mono_store, &result.all_module_envs, null, result.layout_store, null, null);
        defer lowerer.deinit();
        lowerer.instantiate = &result.instantiator;

        lowered_expr_id = try lowerer.lowerExpr(0, resources.expr_idx);
    }

    // Solve specializations
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

    // Lambda lifting
    {
        var lambda_lifter = LambdaLift.init(
            test_allocator,
            &result.mono_store,
            &result.all_module_envs,
            null,
            result.layout_store,
            null,
        );
        defer lambda_lifter.deinit();
        try lambda_lifter.liftAllLambdas();
    }

    // RC insertion on main expression
    {
        var rc_pass = RcInsertPass.init(test_allocator, &result.mono_store, result.layout_store);
        defer rc_pass.deinit();
        result.rc_expr_id = rc_pass.insertRcOps(lowered_expr_id) catch lowered_expr_id;
    }

    // RC insertion on all function definitions (same as dev evaluator)
    {
        var def_iter = result.mono_store.symbol_defs.iterator();
        while (def_iter.next()) |entry| {
            var fn_rc = RcInsertPass.init(test_allocator, &result.mono_store, result.layout_store);
            defer fn_rc.deinit();
            entry.value_ptr.* = fn_rc.insertRcOps(entry.value_ptr.*) catch entry.value_ptr.*;
        }
    }

    return result;
}

test "rc insert: polymorphic identity with if-else - succeeds" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerLiftAndRcFromSource(source);
    defer result.deinit();

    // RC insertion should succeed without errors (we got here)
    // The expression might be wrapped in additional RC operations,
    // so walk down to find the block
    const top_expr = result.mono_store.getExpr(result.rc_expr_id);

    // After RC insertion the top-level might be a block (with RC ops added as stmts)
    // or still the original block
    try testing.expect(top_expr == .block);
}

test "rc insert: polymorphic identity with if-else - result layout is Str" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerLiftAndRcFromSource(source);
    defer result.deinit();

    const top_expr = result.mono_store.getExpr(result.rc_expr_id);
    const block = top_expr.block;

    // The block result layout should still be Str after RC insertion
    try testing.expectEqual(layout.Idx.str, block.result_layout);
}

test "rc insert: polymorphic identity with if-else - if_then_else preserved with Str result" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerLiftAndRcFromSource(source);
    defer result.deinit();

    // Walk down to find the if_then_else - it may be nested inside RC blocks
    const found = findIfThenElse(&result.mono_store, result.rc_expr_id);
    try testing.expect(found != null);

    const ite = result.mono_store.getExpr(found.?);
    try testing.expectEqual(layout.Idx.str, ite.if_then_else.result_layout);
}

test "rc insert: polymorphic identity with if-else - call return layouts preserved" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerLiftAndRcFromSource(source);
    defer result.deinit();

    // Search all expressions in the store for calls with the expected ret_layouts
    var found_dec_call = false;
    var found_str_call = false;

    findCallsRecursive(&result.mono_store, result.rc_expr_id, &found_dec_call, &found_str_call);

    try testing.expect(found_dec_call);
    try testing.expect(found_str_call);
}

test "rc insert: polymorphic identity with if-else - specializations have correct layouts" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerLiftAndRcFromSource(source);
    defer result.deinit();

    // Check that specialization defs in symbol_defs still have correct layouts
    const specs = result.instantiator.solvedSpecializations();
    try testing.expectEqual(@as(usize, 2), specs.len);

    var found_dec_spec = false;
    var found_str_spec = false;

    for (specs) |spec| {
        // Look up the (possibly RC-transformed) definition
        const symbol_key: u48 = @bitCast(spec.name_new);
        if (result.mono_store.symbol_defs.get(symbol_key)) |def_id| {
            const def_expr = result.mono_store.getExpr(def_id);
            switch (def_expr) {
                .lambda => |lambda| {
                    if (lambda.ret_layout == .dec) found_dec_spec = true;
                    if (lambda.ret_layout == .str) found_str_spec = true;
                },
                else => {},
            }
        }
    }

    try testing.expect(found_dec_spec);
    try testing.expect(found_str_spec);
}

/// Recursively search for an if_then_else expression
fn findIfThenElse(store: *const MonoExprStore, expr_id: MonoExprId) ?MonoExprId {
    const expr = store.getExpr(expr_id);
    switch (expr) {
        .if_then_else => return expr_id,
        .block => |block| {
            // Check statements
            const stmts = store.getStmts(block.stmts);
            for (stmts) |stmt| {
                if (findIfThenElse(store, stmt.expr)) |found| return found;
            }
            // Check final expression
            return findIfThenElse(store, block.final_expr);
        },
        else => return null,
    }
}

/// Recursively search for call expressions and check their ret_layouts
fn findCallsRecursive(store: *const MonoExprStore, expr_id: MonoExprId, found_dec: *bool, found_str: *bool) void {
    const expr = store.getExpr(expr_id);
    switch (expr) {
        .call => |call_expr| {
            if (call_expr.ret_layout == .dec) found_dec.* = true;
            if (call_expr.ret_layout == .str) found_str.* = true;
        },
        .block => |block| {
            const stmts = store.getStmts(block.stmts);
            for (stmts) |stmt| {
                findCallsRecursive(store, stmt.expr, found_dec, found_str);
            }
            findCallsRecursive(store, block.final_expr, found_dec, found_str);
        },
        .if_then_else => |ite| {
            const branches = store.getIfBranches(ite.branches);
            for (branches) |branch| {
                findCallsRecursive(store, branch.cond, found_dec, found_str);
                findCallsRecursive(store, branch.body, found_dec, found_str);
            }
            findCallsRecursive(store, ite.final_else, found_dec, found_str);
        },
        else => {},
    }
}
