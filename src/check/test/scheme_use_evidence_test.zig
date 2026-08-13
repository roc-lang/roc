//! Tests for the scheme-use evidence records checking persists to
//! `ModuleEnv.scheme_uses`.
//!
//! A constrained scheme used at a value or dispatch-target edge leaves a
//! record. Instantiated uses carry scheme-var-to-fresh-var pairs; shared uses
//! carry the exact monomorphic root and no pairs. Publication resolves those
//! records after checking settles.

const std = @import("std");
const ModuleEnv = @import("can").ModuleEnv;
const TestEnv = @import("./TestEnv.zig");

const Slot = ModuleEnv.SchemeUseRecord.Slot;

fn recordsWithSlot(env: *const ModuleEnv, slot: Slot) usize {
    var count: usize = 0;
    for (env.scheme_uses.items.items) |record| {
        if (record.slot_kind == @intFromEnum(slot)) count += 1;
    }
    return count;
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
