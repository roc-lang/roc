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

test "phantom recursive dispatch records a shared target use" {
    const source =
        \\Counter := [Mk(U64)].{
        \\    step = |c, k| match c {
        \\        Counter.Mk(n) => if k == 0 { n } else { ping(c, k - 1) }
        \\    }
        \\}
        \\
        \\ping = |c, k| c.step(k)
        \\
        \\main = ping(Counter.Mk(7), 3)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("main", "U64");

    const env = test_env.module_env;
    var found_shared_target = false;
    for (env.scheme_uses.items.items) |record| {
        if (record.slot_kind != @intFromEnum(Slot.dispatch_target)) continue;
        if (record.pairs_len == 0) found_shared_target = true;
    }
    try std.testing.expect(found_shared_target);
}

test "constrained self recursion records a shared value use" {
    const source =
        \\Thing := [Val(U64)].{
        \\    step = |Thing.Val(n)| n
        \\}
        \\
        \\loop = |x, n| if n == 0 { x.step() } else { loop(x, n - 1) }
        \\
        \\main = loop(Thing.Val(7), 3)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("main", "U64");

    try std.testing.expect(recordsWithSlot(test_env.module_env, .shared_value_use) >= 1);
}
