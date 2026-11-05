//! Tests for the rule that a type declared in a function body whose context
//! is not empty never leaves the functions whose type variables it depends on
//! (design.md "Body-Local Types Stay In Their Context").

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

const escape_title = "Local Type Escapes";

/// Expect one escape report per entry of `names`, in order, each at that
/// binding's name and naming the type variable the type depends on.
fn expectEscapes(source: []const u8, names: []const []const u8, vars: []const []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const titles = try std.testing.allocator.alloc([]const u8, names.len);
    defer std.testing.allocator.free(titles);
    @memset(titles, escape_title);
    try test_env.assertTypeErrorTitles(titles);
    const idents = test_env.module_env.getIdentStoreConst();
    for (test_env.checker.problems.problems.items, names, vars) |problem, name, var_name| {
        const escape = problem.local_type_escape;
        try std.testing.expectEqualStrings(name, source[escape.region.start.offset..escape.region.end.offset]);
        try std.testing.expectEqualStrings(var_name, idents.getText(escape.var_name));
    }
}

fn expectNoErrors(source: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "local type whose backing names the function's variable may not be returned" {
    try expectEscapes(
        \\make : a -> _
        \\make = |x| {
        \\    Local := [L(a)]
        \\    Local.L(x)
        \\}
    , &.{"make"}, &.{"a"});
}

test "local type whose method names the function's variable may not be returned inside a record" {
    try expectEscapes(
        \\make : a -> _
        \\make = |_x| {
        \\    Local := [L].{
        \\        size : Local -> U64
        \\        size = |_| {
        \\            empty : List(a)
        \\            empty = []
        \\            List.len(empty)
        \\        }
        \\    }
        \\    { value: Local.L }
        \\}
    , &.{"make"}, &.{"a"});
}

test "local type may not be returned inside a closure" {
    try expectEscapes(
        \\make : a -> _
        \\make = |x| {
        \\    Local := [L(a)]
        \\    |{}| [Local.L(x)]
        \\}
    , &.{"make"}, &.{"a"});
}

test "nested function may return a local type of the outer function's variable" {
    try expectNoErrors(
        \\count : a -> U64
        \\count = |x| {
        \\    Local := [L(a)]
        \\    wrap : a -> Local
        \\    wrap = |v| Local.L(v)
        \\    List.len([wrap(x), wrap(x)])
        \\}
    );
}

test "outer function may not return a local type of its own variable from a nested function" {
    try expectEscapes(
        \\make : a -> _
        \\make = |x| {
        \\    Local := [L(a)]
        \\    wrap : a -> Local
        \\    wrap = |v| Local.L(v)
        \\    wrap(x)
        \\}
    , &.{"make"}, &.{"a"});
}

test "local type of a nested function's variable may not reach the enclosing body" {
    try expectEscapes(
        \\count : U64 -> U64
        \\count = |n| {
        \\    make : b -> _
        \\    make = |y| {
        \\        Local := [L(b)]
        \\        Local.L(y)
        \\    }
        \\    List.len([make(n)])
        \\}
    , &.{"make"}, &.{"b"});
}

test "local type with an empty context may be returned" {
    try expectNoErrors(
        \\make : a -> _
        \\make = |_x| {
        \\    Plain := [P]
        \\    Plain.P
        \\}
    );
}

test "local type's own method variables are not its context" {
    try expectNoErrors(
        \\render : a -> a
        \\render = |value| {
        \\    Label(x) := { text : x }.{
        \\        text_of : Label(b) -> b
        \\        text_of = |label| label.text
        \\    }
        \\    Label.text_of(Label.{ text: value })
        \\}
    );
}

/// Expect the problems' titles in order, with each escape report at the
/// binding named by the matching entry of `escape_names`.
fn expectTitlesAndEscapes(source: []const u8, titles: []const []const u8, escape_names: []const []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertTypeErrorTitles(titles);
    var escape_index: usize = 0;
    for (test_env.checker.problems.problems.items) |problem| {
        if (problem != .local_type_escape) continue;
        const escape = problem.local_type_escape;
        try std.testing.expectEqualStrings(escape_names[escape_index], source[escape.region.start.offset..escape.region.end.offset]);
        escape_index += 1;
    }
    try std.testing.expectEqual(escape_names.len, escape_index);
}

test "a mismatch against the variable inside its function is reported alongside the escape" {
    try expectTitlesAndEscapes(
        \\make : a -> _
        \\make = |x| {
        \\    Local := [L(a)]
        \\    named : Str
        \\    named = x
        \\    Local.L(x)
        \\}
    , &.{ "Type Mismatch", escape_title }, &.{"make"});
}

test "a missing method on the variable inside its function is reported alongside the escape" {
    try expectTitlesAndEscapes(
        \\make : a -> _
        \\make = |x| {
        \\    Local := [L(a)]
        \\    _shown = x.show()
        \\    Local.L(x)
        \\}
    , &.{ "Missing Method", escape_title }, &.{"make"});
}

test "a mismatch inside a nested function whose local values reach the enclosing body is reported alongside the escape" {
    try expectTitlesAndEscapes(
        \\count : U64 -> U64
        \\count = |n| {
        \\    acc = []
        \\    add : b -> U64
        \\    add = |y| {
        \\        Local := [L(b)]
        \\        named : Str
        \\        named = y
        \\        List.len(List.append(acc, Local.L(y)))
        \\    }
        \\    add(1.U8) + add("s") + n
        \\}
    , &.{ "Type Mismatch", escape_title }, &.{"acc"});
}
