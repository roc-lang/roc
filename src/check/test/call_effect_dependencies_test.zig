//! Call effect dependencies (design.md "Checking Effects And Const Roots"): a
//! call of a function whose scheme it instantiates depends on the instance's
//! dependencies, not on the instance, and every other call depends on its
//! callee.

const std = @import("std");
const can = @import("can");
const types = @import("types");
const TestEnv = @import("./TestEnv.zig");

const ModuleEnv = can.ModuleEnv;

/// The number of effect dependencies of the top-level function `name`.
fn effectDepCount(env: *TestEnv, name: []const u8) error{TestUnexpectedResult}!usize {
    const idents = env.module_env.getIdentStoreConst();
    for (env.module_env.store.sliceDefs(env.module_env.all_defs)) |def_idx| {
        const def = env.module_env.store.getDef(def_idx);
        const pattern = env.module_env.store.getPattern(def.pattern);
        if (pattern != .assign) continue;
        if (!std.mem.eql(u8, name, idents.getText(pattern.assign.ident))) continue;
        const func = env.module_env.types.resolveVar(ModuleEnv.varFrom(def_idx)).desc.content.unwrapFunc() orelse return error.TestUnexpectedResult;
        return func.effect_deps.len();
    }
    return error.TestUnexpectedResult;
}

test "call effect dependencies - a call of a generalized local function leaves no dependency" {
    var env = try TestEnv.init("Test",
        \\outer = |_| {
        \\    inner = |_| 1.U64
        \\    inner(0)
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), try effectDepCount(&env, "outer"));
}

test "call effect dependencies - a chain of nested calls leaves no dependency" {
    var env = try TestEnv.init("Test",
        \\outer = |_| {
        \\    middle = |_| {
        \\        inner = |_| 1.U64
        \\        inner(0)
        \\    }
        \\    middle(0)
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), try effectDepCount(&env, "outer"));
}

test "call effect dependencies - a call of a callback argument stays a dependency" {
    var env = try TestEnv.init("Test",
        \\apply = |f, x| f(x)
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), try effectDepCount(&env, "apply"));
}

test "issue 13299: a diagnostic preserves the effect of an instantiated callback" {
    var env = try TestEnv.init("Test",
        \\e! : U64 => U64
        \\e! = |n| n
        \\w = |f| |x| f(x)
        \\h = w(e!)
        \\g : U64 -> U64
        \\g = h
    );
    defer env.deinit();
    try env.assertOneTypeProblemMsgContains("Type Mismatch", "U64 => U64");
}

test "call effect dependencies - a local function calling a captured callback stays a dependency" {
    var env = try TestEnv.init("Test",
        \\wrap = |f| {
        \\    inner = |x| f(x)
        \\    inner(0.U64)
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), try effectDepCount(&env, "wrap"));
}

test "call effect dependencies - an unresolved method call stays a dependency" {
    var env = try TestEnv.init("Test",
        \\call_go = |x| x.go()
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), try effectDepCount(&env, "call_go"));
}
