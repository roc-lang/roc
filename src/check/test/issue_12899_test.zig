//! Lexical type captures are explicit nominal arguments before types are sealed.

const std = @import("std");
const TestEnv = @import("TestEnv.zig");
const can = @import("can");
const Captures = @import("../nominal_captures.zig").Table;

fn expectParameters(env: *const TestEnv, name: []const u8, source: u32, captures: u31) error{ TestExpectedEqual, TestUnexpectedResult }!void {
    for (env.module_env.types.nominal_decls.items.items) |decl| {
        if (!std.mem.eql(u8, env.module_env.getIdent(decl.ident.ident_idx), name)) continue;
        try std.testing.expectEqual(captures, decl.flags.capture_count);
        try std.testing.expectEqual(source + captures, decl.formals.count);
        return;
    }
    return error.TestUnexpectedResult;
}

test "issue 12899: a local nominal publishes its lexical type capture" {
    var env = try TestEnv.init("Test",
        \\wrapped : a -> Str
        \\wrapped = |x| {
        \\    Wrap := [Wrap(a)]
        \\    Str.inspect([Wrap.Wrap(x)])
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try expectParameters(&env, "Wrap", 0, 1);
}

test "issue 12899: captures propagate through declarations without capturing their formals" {
    var env = try TestEnv.init("Test",
        \\wrapped : a, b -> Str
        \\wrapped = |x, _unused| {
        \\    Inner(t) := [Inner(a, t)]
        \\    Outer := [Outer(Inner(Str))]
        \\    Independent(t) := [Independent(t)]
        \\    Str.inspect([Outer.Outer(Inner.Inner(x, "hi"))])
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try expectParameters(&env, "Inner", 1, 1);
    try expectParameters(&env, "Outer", 0, 1);
    try expectParameters(&env, "Independent", 1, 0);
    try env.assertDefType("wrapped", "a, b -> Str");
}

test "issue 12899: recursive local declarations retain one capture" {
    var env = try TestEnv.init("Test",
        \\wrapped : a -> Str
        \\wrapped = |x| {
        \\    Chain := [End, Next(a, Chain)]
        \\    Str.inspect(Chain.Next(x, Chain.End))
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try expectParameters(&env, "Chain", 0, 1);
}

test "issue 12899: a padding-only dependency is a capture" {
    var env = try TestEnv.init("Test",
        \\wrapped : a -> _
        \\wrapped = |_x| {
        \\    Padded := { value : U8, _ : a }
        \\    Padded.{ value: 1.U8 }
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try expectParameters(&env, "Padded", 0, 1);
    // No value field relates the capture to the argument. Its identity must
    // come from lexical binding, not inference from a constructor payload.
    var found = false;
    const store = env.module_env.types;
    for (env.module_env.store.sliceDefs(env.module_env.all_defs)) |def_idx| {
        const def = env.module_env.store.getDef(def_idx);
        const pattern = env.module_env.store.getPattern(def.pattern);
        if (pattern != .assign or !std.mem.eql(u8, env.module_env.getIdent(pattern.assign.ident), "wrapped")) continue;
        const function = store.resolveVar(can.ModuleEnv.varFrom(def_idx)).desc.content.structure.fn_pure;
        const nominal = store.resolveVar(function.ret).desc.content.structure.nominal_type;
        const args = store.sliceVars(function.args);
        const captures = store.sliceNominalArgs(nominal);
        try std.testing.expectEqual(@as(usize, 1), captures.len);
        try std.testing.expectEqual(store.resolveVar(args[0]).var_, store.resolveVar(captures[0]).var_);
        found = true;
    }
    try std.testing.expect(found);
}

test "issue 12899: imported local nominal applications preserve their complete argument tuple" {
    var producer = try TestEnv.init("Producer",
        \\Producer := [].{
        \\    make : a -> _
        \\    make = |x| {
        \\        Wrap := [Wrap(a)]
        \\        Wrap.Wrap(x)
        \\    }
        \\}
    );
    defer producer.deinit();
    try producer.assertNoErrors();
    var consumer = try TestEnv.initWithImport("Consumer",
        \\import Producer
        \\first = Producer.make(5.U64)
        \\second = Producer.make("hi")
    , "Producer", &producer);
    defer consumer.deinit();
    try consumer.assertNoErrors();
    try expectParameters(&consumer, "Wrap", 0, 1);
}

test "issue 12899: an enclosing binder is not replaced by a same-named phantom parameter" {
    var env = try TestEnv.init("Test",
        \\render : a -> Str
        \\render = |x| {
        \\    Captured := [Captured(a)]
        \\    Pair(a) := [Pair(Captured, a)]
        \\    value : Pair(Str)
        \\    value = Pair.Pair(Captured.Captured(x), x)
        \\    Str.inspect(value)
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try expectParameters(&env, "Pair", 1, 1);
    try env.assertDefType("render", "a -> Str");
}

fn exerciseCaptureAllocations(allocator: std.mem.Allocator, module: *const can.ModuleEnv) error{ OutOfMemory, TestUnexpectedResult, TestExpectedEqual }!void {
    var table = Captures{};
    defer table.deinit(allocator);
    try table.ensure(allocator, module);
    const count = table.captures.count();
    try std.testing.expect(count != 0);
    try table.ensure(allocator, module);
    try std.testing.expectEqual(count, table.captures.count());
}

test "issue 12899: capture analysis releases partial results on allocation failure" {
    var env = try TestEnv.init("Test",
        \\wrapped : a -> Str
        \\wrapped = |x| {
        \\    Inner := [Inner(a)]
        \\    Outer := [Outer(Inner)]
        \\    Str.inspect(Outer.Outer(Inner.Inner(x)))
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.checkAllAllocationFailures(std.testing.allocator, exerciseCaptureAllocations, .{env.module_env});
}
