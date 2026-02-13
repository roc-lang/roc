//! Tests for Mono IR lowering of polymorphic expressions.
//!
//! These tests verify that the mono lowerer correctly lowers polymorphic
//! let-bindings by checking the structure and layouts of the resulting
//! Mono IR. This is the stage between type-checking and code generation.

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

const ModuleEnv = can.ModuleEnv;

const testing = std.testing;
const test_allocator = helpers.interpreter_allocator;

/// Lower a source expression through the full pipeline (parse → can → typecheck → mono lower + specializations)
/// and return the mono store + lowered expression ID for inspection.
const LowerResult = struct {
    mono_store: MonoExprStore,
    lowered_expr_id: MonoExprId,
    instantiator: mono.Instantiate,
    lowerer: MonoLower,
    layout_store: *layout.Store,
    resources: @TypeOf(helpers.parseAndCanonicalizeExpr(undefined, undefined) catch unreachable),

    fn deinit(self: *LowerResult) void {
        self.instantiator.deinit();
        self.lowerer.deinit();
        self.mono_store.deinit();
        test_allocator.destroy(self.layout_store);
        helpers.cleanupParseAndCanonical(test_allocator, self.resources);
    }
};

fn lowerFromSource(source: []const u8) !LowerResult {
    const resources = try helpers.parseAndCanonicalizeExpr(test_allocator, source);
    errdefer helpers.cleanupParseAndCanonical(test_allocator, resources);

    const all_module_envs = [_]*ModuleEnv{ resources.module_env, @constCast(@as(*const ModuleEnv, resources.builtin_module.env)) };

    // Create global layout store (same as dev evaluator)
    const builtin_str = all_module_envs[0].idents.builtin_str;
    const layout_store = try test_allocator.create(layout.Store);
    errdefer test_allocator.destroy(layout_store);
    layout_store.* = try layout.Store.init(&all_module_envs, builtin_str, test_allocator, base.target.TargetUsize.native);

    var mono_store = MonoExprStore.init(test_allocator);
    errdefer mono_store.deinit();

    var instantiator = mono.Instantiate.init(test_allocator, &all_module_envs);
    errdefer instantiator.deinit();

    var lowerer = MonoLower.init(test_allocator, &mono_store, &all_module_envs, null, layout_store, null, null);
    errdefer lowerer.deinit();
    lowerer.instantiate = &instantiator;

    // Lower CIR to Mono IR
    const lowered_expr_id = try lowerer.lowerExpr(0, resources.expr_idx);

    // Solve specializations (same as dev evaluator)
    while (instantiator.nextNeededSpecialization()) |spec| {
        const spec_module_env = all_module_envs[@as(usize, spec.module_idx)];

        var spec_lowerer = MonoLower.init(test_allocator, &mono_store, &all_module_envs, null, layout_store, null, null);
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
        try mono_store.symbol_defs.put(symbol_key, lowered_spec);
    }

    return .{
        .mono_store = mono_store,
        .lowered_expr_id = lowered_expr_id,
        .instantiator = instantiator,
        .lowerer = lowerer,
        .layout_store = layout_store,
        .resources = resources,
    };
}

test "mono lower: polymorphic identity with if-else - block structure" {
    // Exact source from the failing eval test "polymorphic identity function"
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerFromSource(source);
    defer result.deinit();

    // The top-level expression should be a block
    const top_expr = result.mono_store.getExpr(result.lowered_expr_id);
    try testing.expect(top_expr == .block);

    const block = top_expr.block;

    // Block should have statements (let bindings) and a final expression
    // We expect: identity = ..., num = identity(5), str = identity("Hello")
    // with final expr being: if (num > 0) str else ""
    const stmts = result.mono_store.getStmts(block.stmts);
    try testing.expect(stmts.len >= 2); // At least num and str bindings

    // The final expression should be an if_then_else
    const final_expr = result.mono_store.getExpr(block.final_expr);
    try testing.expect(final_expr == .if_then_else);

    // The result layout of the block should be Str
    try testing.expectEqual(layout.Idx.str, block.result_layout);
}

test "mono lower: polymorphic identity with if-else - specializations created" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerFromSource(source);
    defer result.deinit();

    // Two specializations should have been created:
    // one for Dec (identity(5)) and one for Str (identity("Hello"))
    const specs = result.instantiator.solvedSpecializations();
    try testing.expectEqual(@as(usize, 2), specs.len);

    // Both should have been solved (lowered != null)
    for (specs) |spec| {
        try testing.expect(spec.lowered != null);
    }
}

test "mono lower: polymorphic identity with if-else - specialization layouts" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerFromSource(source);
    defer result.deinit();

    const specs = result.instantiator.solvedSpecializations();
    try testing.expectEqual(@as(usize, 2), specs.len);

    // Check the lowered specializations - each should be a lambda
    // The Dec specialization should have a Dec return layout
    // The Str specialization should have a Str return layout
    var found_dec = false;
    var found_str = false;

    for (specs) |spec| {
        const lowered_id = spec.lowered.?;
        const lowered_expr = result.mono_store.getExpr(lowered_id);

        switch (lowered_expr) {
            .lambda => |lambda| {
                if (lambda.ret_layout == .dec) found_dec = true;
                if (lambda.ret_layout == .str) found_str = true;
            },
            else => {
                // Specialization might be wrapped differently, check other patterns
                std.debug.print("Unexpected specialization expr type: {s}\n", .{@tagName(lowered_expr)});
            },
        }
    }

    try testing.expect(found_dec);
    try testing.expect(found_str);
}

test "mono lower: polymorphic identity with if-else - call return layouts" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerFromSource(source);
    defer result.deinit();

    // Walk the block statements to find the call expressions
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

    // identity(5) should produce a call with ret_layout = Dec
    try testing.expect(found_dec_call);
    // identity("Hello") should produce a call with ret_layout = Str
    try testing.expect(found_str_call);
}

test "mono lower: polymorphic identity with if-else - if result is Str" {
    const source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;

    var result = try lowerFromSource(source);
    defer result.deinit();

    const top_expr = result.mono_store.getExpr(result.lowered_expr_id);
    const block = top_expr.block;
    const final_expr = result.mono_store.getExpr(block.final_expr);

    // The if_then_else result layout should be Str
    try testing.expect(final_expr == .if_then_else);
    try testing.expectEqual(layout.Idx.str, final_expr.if_then_else.result_layout);
}
