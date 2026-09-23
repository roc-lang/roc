//! Tests for the scheme-use evidence records checking persists to
//! `ModuleEnv.scheme_uses`.
//!
//! A constrained scheme used at a value or dispatch-target edge leaves a
//! record. Instantiated uses carry scheme-var-to-fresh-var pairs; shared uses
//! carry the exact monomorphic root and no pairs. Checked-artifact construction
//! resolves those records after checking settles.

const std = @import("std");
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

test "issue 11311: forwarding evidence belongs to the value and not its annotation" {
    const source =
        \\Model(a) := { parse : Str -> Try(a, Str), render : a -> Str }
        \\make : (Str -> Try(a, Str)), (a -> Str) -> Model(a)
        \\make = |parse, render| Model.{ parse, render }
        \\forward : (Str -> Try(a, Str)), (a -> Str) -> Model(a)
        \\forward = make
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();

    const env = test_env.module_env;
    const idents = env.getIdentStoreConst();
    var make_var: ?@import("types").Var = null;
    var forward_node: ?u32 = null;
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern != .assign) continue;
        const name = idents.getText(pattern.assign.ident);
        if (std.mem.eql(u8, name, "make")) make_var = ModuleEnv.varFrom(def_idx);
        if (std.mem.eql(u8, name, "forward")) forward_node = @intFromEnum(def.expr);
    }
    try std.testing.expect(make_var != null);
    try std.testing.expect(forward_node != null);

    var count: usize = 0;
    for (env.scheme_uses.items.items) |record| {
        if (record.node_idx != forward_node.? or record.slot_kind != @intFromEnum(Slot.value_use)) continue;
        count += 1;
        try std.testing.expectEqual(
            env.types.resolveVar(make_var.?).var_,
            env.types.resolveVar(@enumFromInt(record.scheme_root)).var_,
        );
        try std.testing.expectEqual(@as(u32, 1), record.pairs_len);
    }
    try std.testing.expectEqual(@as(usize, 1), count);
}

const constrained_forwarding =
    \\a.Stringable : where [a.to_str : a -> Str]
    \\render : Try(a, Str) -> Str where [a.Stringable]
    \\render = |result| match result {
    \\    Ok(value) => value.to_str()
    \\    Err(message) => message
    \\}
    \\forward : Try(a, Str) -> Str where [a.Stringable]
    \\forward = render
    \\
;

test "issue 11311: forwarding with type applications preserves where-alias requirements" {
    var test_env = try TestEnv.init("Test", constrained_forwarding ++
        \\Thing := [Thing].{
        \\    to_str : Thing -> Str
        \\    to_str = |_| "thing"
        \\}
        \\result = forward(Ok(Thing.Thing))
    );
    defer test_env.deinit();
    try test_env.assertDefType("result", "Str");
}

test "issue 11311: forwarding with type applications rejects missing where-alias methods" {
    var test_env = try TestEnv.init("Test", constrained_forwarding ++
        \\Missing := [Missing]
        \\result = forward(Ok(Missing.Missing))
    );
    defer test_env.deinit();
    try test_env.assertHasTypeError("Missing Method");
}

test "concrete recursive dispatch records a shared method instance without copying requirements" {
    var test_env = try TestEnv.init("Test",
        \\Expr := [Leaf(Str), Next(Expr)].{
        \\  is_eq = |left, right|
        \\    match (left, right) {
        \\      (Leaf(a), Leaf(b)) => a == b
        \\      (Next(a), Next(b)) => a == b
        \\      _ => False
        \\    }
        \\}
        \\main = Expr.Next(Expr.Leaf("a")) == Expr.Next(Expr.Leaf("a"))
    );
    defer test_env.deinit();
    try test_env.assertNoErrors();

    const env = test_env.module_env;
    try std.testing.expect(recordsWithSlot(env, .recursive_dispatch_target) > 0);
    for (env.scheme_uses.items.items) |record| {
        if (record.slot_kind != @intFromEnum(Slot.recursive_dispatch_target)) continue;
        try std.testing.expectEqual(@as(u32, 0), record.pairs_len);
        var found_ancestor_instance = false;
        for (test_env.checker.dispatch_target_instantiations.items) |instance| {
            if (@intFromEnum(instance.constraint_fn_var) == record.slot_data) continue;
            if (@intFromEnum(instance.method_var) == record.scheme_root) found_ancestor_instance = true;
        }
        try std.testing.expect(found_ancestor_instance);
    }
}

fn assertBuiltinIterExtremum(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    method_name: []const u8,
) error{ NoSpaceLeft, TestUnexpectedResult }!void {
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
        const plan_root = env.types.resolveVar(expr.e_dispatch_call.constraint_fn_var).var_;

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
        // Per-use instantiation copies the signature at EVERY body use, so a
        // method used twice in one body leaves two records reaching the plan's
        // class. `>= 1` would pass even if per-use copying silently collapsed
        // to sharing, which is the property this whole branch exists to
        // establish.
        try std.testing.expect(body_use_count >= 2);

        const plan = artifact.static_dispatch_plans.plans[entry.val];
        switch (plan.resolution) {
            .evidence_dependent => |resolution| {
                try std.testing.expect(resolution.independent_callable);
                try std.testing.expect(resolution.reuse_slot_nested_evidence);
            },
            .direct_pending,
            .direct_closed,
            .direct_parametric,
            .structural,
            .checked_error,
            .@"unreachable",
            => return error.TestUnexpectedResult,
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

test "annotated recursive self use whose body closes the annotation row records the body scheme" {
    // Issue #11526: `walk`'s predeclared annotation opens its result row (a
    // quantified identity variable), but the body returns the closed input
    // parameter, so the completed scheme has no quantified variables. The
    // in-flight self-use must name the body's scheme, not the standalone
    // annotation, so artifact construction gives it an empty substitution.
    const source =
        \\walk : [Open, Close] -> [Open, Close]
        \\walk = |token|
        \\    match token {
        \\        Open => walk(Close)
        \\        Close => token
        \\    }
        \\main = walk(Open)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();

    const env = test_env.module_env;
    const idents = env.getIdentStoreConst();
    var walk_expr_var: ?u32 = null;
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern != .assign) continue;
        if (std.mem.eql(u8, idents.getText(pattern.assign.ident), "walk")) {
            walk_expr_var = @intFromEnum(ModuleEnv.varFrom(def.expr));
            break;
        }
    }
    try std.testing.expect(walk_expr_var != null);

    const self_use_offset = std.mem.find(u8, source, "walk(Close)") orelse unreachable;
    var walk_self_use_records: usize = 0;
    for (env.scheme_uses.items.items) |record| {
        if (record.slot_kind != @intFromEnum(Slot.value_use)) continue;
        const region = env.store.getNodeRegion(@enumFromInt(record.node_idx));
        if (region.start.offset != self_use_offset) continue;
        try std.testing.expectEqual(walk_expr_var.?, record.scheme_root);
        const scheme_info = try @import("../canonical_type_keys.zig").fromVarInfo(
            std.testing.allocator,
            &env.types,
            env,
            @enumFromInt(record.scheme_root),
        );
        try std.testing.expect(!scheme_info.contains_identity_variables);
        walk_self_use_records += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), walk_self_use_records);
}

test "recursive reference provenance marks the annotated self use but not an external call" {
    const source =
        \\grow : U64 -> U64
        \\grow = |n| if n == 0 { 0 } else { grow(n - 1) }
        \\main = grow(2)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("main", "U64");

    const self_use_offset = std.mem.find(u8, source, "grow(n - 1)") orelse unreachable;
    const env = test_env.module_env;
    var recursive_records: usize = 0;
    for (env.scheme_uses.items.items) |record| {
        if (record.slot_kind != @intFromEnum(Slot.recursive_reference)) continue;
        recursive_records += 1;
        const region = env.store.getNodeRegion(@enumFromInt(record.node_idx));
        try std.testing.expectEqual(@as(u32, @intCast(self_use_offset)), region.start.offset);
    }
    try std.testing.expectEqual(@as(usize, 1), recursive_records);
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
