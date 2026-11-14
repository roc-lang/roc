//! A body-local nominal lists the enclosing functions' type variables it
//! depends on as context arguments after its declared ones
//! (design.md "Body-Local Type Context").

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

fn expectParameters(env: *const TestEnv, name: []const u8, declared: u32, context: u16) error{ TestExpectedEqual, TestUnexpectedResult }!void {
    for (env.module_env.types.nominal_decls.items()) |decl| {
        if (!std.mem.eql(u8, env.module_env.getIdent(decl.ident.ident_idx), name)) continue;
        try std.testing.expectEqual(context, decl.flags.context_len);
        try std.testing.expectEqual(declared + context, decl.formals.count);
        return;
    }
    return error.TestUnexpectedResult;
}

test "issue 12899: a local nominal lists its enclosing type variable as context" {
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
    try env.assertDefType("wrapped", "a -> Str");
}

test "issue 12899: context propagates through declarations without including their own parameters" {
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

test "issue 12899: a recursive local declaration has one context variable" {
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

test "issue 12899: a padding-only dependency is context" {
    var env = try TestEnv.init("Test",
        \\wrapped : a -> Str
        \\wrapped = |_x| {
        \\    Padded := { value : U8, _ : a }
        \\    Str.inspect(Padded.{ value: 1.U8 })
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try expectParameters(&env, "Padded", 0, 1);
}

test "issue 12899: an enclosing type variable is not replaced by a same-named parameter" {
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
    try expectParameters(&env, "Captured", 0, 1);
    try expectParameters(&env, "Pair", 1, 1);
    try env.assertDefType("render", "a -> Str");
}

test "issue 12899: a body-local nominal of an imported function is used at two types" {
    var producer = try TestEnv.init("Producer",
        \\Producer := [].{
        \\    render : a -> Str
        \\    render = |x| {
        \\        Wrap := [Wrap(a)]
        \\        Str.inspect(Wrap.Wrap(x))
        \\    }
        \\}
    );
    defer producer.deinit();
    try producer.assertNoErrors();
    try expectParameters(&producer, "Wrap", 0, 1);
    var consumer = try TestEnv.initWithImport("Consumer",
        \\import Producer
        \\first = Producer.render(5.U64)
        \\second = Producer.render("hi")
    , "Producer", &producer);
    defer consumer.deinit();
    try consumer.assertNoErrors();
}
