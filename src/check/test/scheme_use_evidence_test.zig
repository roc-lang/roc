//! Tests for the scheme-use evidence records checking persists to
//! `ModuleEnv.scheme_uses`.
//!
//! A constrained scheme used at a value or dispatch-target edge leaves a
//! record. Instantiated uses carry scheme-var-to-fresh-var pairs; shared uses
//! carry the exact monomorphic root and no pairs. Checked-artifact construction
//! resolves those records after checking settles.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");
const checked_artifact = @import("../checked_artifact.zig");
const types = @import("types");
const ModuleEnv = can.ModuleEnv;
const TestEnv = @import("./TestEnv.zig");

const Slot = ModuleEnv.SchemeUseRecord.Slot;

fn recordsWithSlot(env: *const ModuleEnv, slot: Slot) usize {
    var count: usize = 0;
    for (env.scheme_uses.items.items) |record| {
        if (record.slot_kind == @intFromEnum(slot)) count += 1;
    }
    return count;
}

fn whereRecordForRawCallable(env: *const ModuleEnv, raw_fn_var: u32) ?ModuleEnv.SchemeUseRecord {
    for (env.scheme_uses.items.items) |record| {
        if (record.slot_kind == @intFromEnum(Slot.where_method_use) and
            record.slot_data == raw_fn_var)
        {
            return record;
        }
    }
    return null;
}

fn assertBuiltinIterExtremum(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    method_name: []const u8,
) !void {
    const env = artifact.moduleEnvConst();
    const source = env.getSourceAll();
    var needle_buffer: [64]u8 = undefined;
    const needle = try std.fmt.bufPrint(&needle_buffer, "best_so_far.{s}(item)", .{method_name});
    const wanted_offset = std.mem.find(u8, source, needle) orelse return error.TestUnexpectedResult;
    var found_plan = false;

    for (artifact.static_dispatch_plans.by_expr) |entry| {
        const expr_idx: can.CIR.Expr.Idx = @enumFromInt(entry.key);
        const expr = env.store.getExpr(expr_idx);
        if (expr != .e_dispatch_call) continue;
        if (!std.mem.eql(u8, env.getIdent(expr.e_dispatch_call.method_name), method_name)) continue;

        const receiver_region = env.store.getExprRegion(expr.e_dispatch_call.receiver);
        const receiver_snippet = source[receiver_region.start.offset..receiver_region.end.offset];
        if (!std.mem.eql(u8, receiver_snippet, "best_so_far")) continue;
        if (receiver_region.start.offset != wanted_offset) continue;

        try std.testing.expect(!found_plan);
        found_plan = true;
        const raw_plan_fn = @intFromEnum(expr.e_dispatch_call.constraint_fn_var);
        const plan_root = env.types.resolveVar(expr.e_dispatch_call.constraint_fn_var).var_;

        // The checked plan is a third raw callable in the omitted class; it
        // is not itself a where-use key. The dedicated target-share producer
        // records an exact raw partition endpoint for that whole omitted class,
        // while its proof remains keyed by one exact body-use raw.
        try std.testing.expect(whereRecordForRawCallable(env, raw_plan_fn) == null);
        var body_use_count: usize = 0;
        for (env.scheme_uses.items.items) |record| {
            if (record.slot_kind != @intFromEnum(Slot.where_method_use)) continue;
            const pairs = env.scheme_use_pairs.items.items[record.pairs_start .. record.pairs_start + record.pairs_len];
            var reaches_plan_class = false;
            for (pairs) |pair| {
                if (env.types.resolveVar(@as(types.Var, @enumFromInt(pair.fresh_var))).var_ == plan_root) {
                    reaches_plan_class = true;
                    break;
                }
            }
            if (reaches_plan_class) body_use_count += 1;
        }
        try std.testing.expect(body_use_count >= 2);

        var found_share = false;
        for (env.generalized_dispatch_target_shares.items.items) |share| {
            const share_method: base.Ident.Idx = @bitCast(share.method_ident);
            if (!std.mem.eql(u8, env.getIdent(share_method), method_name)) continue;
            if (env.types.resolveVar(@as(types.Var, @enumFromInt(share.omitted_fn_var))).var_ != plan_root) continue;
            try std.testing.expectEqual(
                @intFromEnum(ModuleEnv.GeneralizedDispatchTargetShare.ProofKind.where_method_use),
                share.proof_kind,
            );
            try std.testing.expect(share.proof_fn_var != raw_plan_fn);
            const proof = whereRecordForRawCallable(env, share.proof_fn_var) orelse
                return error.TestUnexpectedResult;
            try std.testing.expectEqual(share.retained_fn_var, proof.scheme_root);

            // Validate the approved complete-map proof by its raw key: the
            // exact pristine retained callable must map to this omitted class.
            var copied_retained_to_omitted = false;
            const pairs = env.scheme_use_pairs.items.items[proof.pairs_start .. proof.pairs_start + proof.pairs_len];
            for (pairs) |pair| {
                if (env.types.resolveVar(@as(types.Var, @enumFromInt(pair.old_var))).var_ !=
                    env.types.resolveVar(@as(types.Var, @enumFromInt(share.retained_fn_var))).var_)
                {
                    continue;
                }
                try std.testing.expectEqual(
                    plan_root,
                    env.types.resolveVar(@as(types.Var, @enumFromInt(pair.fresh_var))).var_,
                );
                copied_retained_to_omitted = true;
            }
            try std.testing.expect(copied_retained_to_omitted);
            found_share = true;
        }
        try std.testing.expect(found_share);

        const plan = artifact.static_dispatch_plans.plans[entry.val];
        switch (plan.resolution) {
            .evidence_dependent => |resolution| {
                try std.testing.expect(resolution.independent_callable);
                try std.testing.expect(resolution.reuse_slot_nested_evidence);
            },
            else => return error.TestUnexpectedResult,
        }
    }
    try std.testing.expect(found_plan);
}

test "scheme use evidence resolves Builtin Iter min and max through exact raw where-use proofs" {
    const gpa = std.testing.allocator;
    var builtin_module = try can.BuiltinStatic.moduleView(
        gpa,
        compiled_builtins.builtin_bin[0..],
        "Builtin",
        compiled_builtins.builtin_source,
    );
    var artifact_owns_module = false;
    errdefer if (!artifact_owns_module) builtin_module.deinit();

    const blob = compiled_builtins.builtin_artifact_bin[0..];
    const serialized_bytes = try checked_artifact.CheckedModuleArtifact.splitVersionTrailer(blob);
    const backing: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8 =
        @alignCast(blob[0..serialized_bytes.len]);
    const serialized: *const checked_artifact.CheckedModuleArtifact.Serialized = @ptrCast(@alignCast(backing.ptr));
    try serialized.validate(backing.len);
    var artifact = serialized.deserializeStatic(
        backing,
        gpa,
        .{ .static_builtin = builtin_module.env },
    );
    artifact_owns_module = true;
    defer artifact.deinit(gpa);

    try assertBuiltinIterExtremum(&artifact, "min");
    try assertBuiltinIterExtremum(&artifact, "max");
}

test "generalized dispatch target share records attached shape-only deduplication" {
    const source =
        \\render_twice = |x| Str.concat(x.render(), x.render())
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();

    const env = test_env.module_env;
    var render_shares: usize = 0;
    for (env.generalized_dispatch_target_shares.items.items) |share| {
        const method: base.Ident.Idx = @bitCast(share.method_ident);
        if (!std.mem.eql(u8, env.getIdent(method), "render")) continue;
        try std.testing.expectEqual(
            @intFromEnum(ModuleEnv.GeneralizedDispatchTargetShare.ProofKind.shape_only),
            share.proof_kind,
        );
        try std.testing.expectEqual(share.omitted_fn_var, share.proof_fn_var);
        try std.testing.expect(
            env.types.resolveVar(@as(types.Var, @enumFromInt(share.omitted_fn_var))).var_ !=
                env.types.resolveVar(@as(types.Var, @enumFromInt(share.retained_fn_var))).var_,
        );
        render_shares += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), render_shares);
}

test "value use of a where-clause generic records instantiation evidence" {
    const source =
        \\Thing := [Val(Str)].{
        \\  to_str : Thing -> Str
        \\  to_str = |Thing.Val(s)| s
        \\}
        \\
        \\helper : a -> Str where [a.to_str : a -> Str]
        \\helper = |x| x.to_str()
        \\
        \\main : Str
        \\main = helper(Thing.Val("hello"))
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("main", "Str");

    const env = test_env.module_env;
    try std.testing.expect(recordsWithSlot(env, .value_use) >= 1);

    // The record for `helper`'s instantiation at the call site pairs the
    // scheme's constrained receiver var with a fresh var that, once checking
    // settled, resolved to the concrete `Thing` nominal.
    var found_resolved_pair = false;
    for (env.scheme_uses.items.items) |record| {
        if (record.slot_kind != @intFromEnum(Slot.value_use)) continue;
        try std.testing.expect(record.pairs_len >= 1);
        const pairs = env.scheme_use_pairs.items.items[record.pairs_start .. record.pairs_start + record.pairs_len];
        for (pairs) |pair| {
            const resolved = env.types.resolveVar(@enumFromInt(pair.fresh_var));
            if (resolved.desc.content == .structure) found_resolved_pair = true;
        }
    }
    try std.testing.expect(found_resolved_pair);
}

test "source-forward annotated recursive use records the complete body scheme" {
    const source =
        \\weak = "a,b,c"
        \\forward : (Str -> b), U64 -> List(b)
        \\forward = |g, n| f(g, n)
        \\f : (Str -> b), U64 -> List(b)
        \\f = |g, n|
        \\    if n == 0
        \\        weak.split_on(",").map(g)
        \\    else
        \\        forward(g, n - 1)
        \\lengths = forward(|s| s.count_utf8_bytes(), 1)
        \\selves = forward(|s| s, 1)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();

    const env = test_env.module_env;
    const idents = env.getIdentStoreConst();
    var f_expr_var: ?u32 = null;
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern != .assign) continue;
        if (std.mem.eql(u8, idents.getText(pattern.assign.ident), "f")) {
            f_expr_var = @intFromEnum(ModuleEnv.varFrom(def.expr));
            break;
        }
    }
    try std.testing.expect(f_expr_var != null);

    var found_complete_forward_use = false;
    for (env.scheme_uses.items.items) |record| {
        if (record.slot_kind != @intFromEnum(Slot.value_use)) continue;
        if (record.scheme_root != f_expr_var.?) continue;
        try std.testing.expect(record.pairs_len > 0);
        found_complete_forward_use = true;
    }
    try std.testing.expect(found_complete_forward_use);
}

test "discharging a dispatch constraint onto a constrained method target records dispatch_target evidence" {
    const source =
        \\Thing := [Val(Str)].{
        \\  to_str : Thing -> Str
        \\  to_str = |Thing.Val(s)| s
        \\}
        \\
        \\Wrap(a) := [W(a)].{
        \\  unwrap : Wrap(a) -> Str where [a.to_str : a -> Str]
        \\  unwrap = |Wrap.W(x)| x.to_str()
        \\}
        \\
        \\main : Str
        \\main = Wrap.W(Thing.Val("hi")).unwrap()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("main", "Str");

    const env = test_env.module_env;
    try std.testing.expect(recordsWithSlot(env, .dispatch_target) >= 1);

    // The `unwrap` target scheme instantiation is keyed by the discharged
    // constraint's fn var and pairs `a` with a fresh var that resolved to the
    // concrete `Thing` nominal.
    var found_resolved_pair = false;
    for (env.scheme_uses.items.items) |record| {
        if (record.slot_kind != @intFromEnum(Slot.dispatch_target)) continue;
        if (record.pairs_len == 0) continue;
        try std.testing.expect(record.slot_data != 0);
        const pairs = env.scheme_use_pairs.items.items[record.pairs_start .. record.pairs_start + record.pairs_len];
        for (pairs) |pair| {
            const resolved = env.types.resolveVar(@enumFromInt(pair.fresh_var));
            if (resolved.desc.content == .structure) found_resolved_pair = true;
        }
    }
    try std.testing.expect(found_resolved_pair);
}

test "block-local attached procedures record their dispatch target edges" {
    const source =
        \\first = {
        \\    Local := [First(U64)].{
        \\        get : Local -> U64
        \\        get = |Local.First(n)| n
        \\    }
        \\    Local.First(5).get()
        \\}
        \\
        \\second = {
        \\    Local := [Second(U64)].{
        \\        get : Local -> U64
        \\        get = |Local.Second(n)| n + 100
        \\    }
        \\    Local.Second(8).get()
        \\}
        \\
        \\main = (first, second)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("main", "(U64, U64)");

    const env = test_env.module_env;
    var zero_pair_targets: usize = 0;
    for (env.scheme_uses.items.items) |record| {
        if (record.slot_kind != @intFromEnum(Slot.dispatch_target)) continue;
        if (record.pairs_len == 0) zero_pair_targets += 1;
    }
    try std.testing.expect(zero_pair_targets >= 2);
}
