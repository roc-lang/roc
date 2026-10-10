//! Deferred judgments are made by the frame that owns them: a tuple access is
//! related to its receiver's element before the definition reading it
//! generalizes, and an annotation predeclared inside a body judges nothing that
//! belongs to the enclosing body.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "a tuple access pinned later in its definition types that definition's scheme" {
    const source =
        \\takes_pair : (I64, Str) -> {}
        \\takes_pair = |_| {}
        \\
        \\first = |t| {
        \\    r = t.0
        \\    _ = takes_pair(t)
        \\    r
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("first", "(I64, Str) -> I64");
}

test "a use of a tuple access's definition sees the element type the receiver was pinned to" {
    const source =
        \\takes_pair : (I64, Str) -> {}
        \\takes_pair = |_| {}
        \\
        \\first = |t| {
        \\    r = t.0
        \\    _ = takes_pair(t)
        \\    r
        \\}
        \\
        \\wrong : Str
        \\wrong = first((1, "x"))
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

test "a nested definition's tuple access on an enclosing receiver waits for the receiver's frame" {
    const source =
        \\takes_pair : (I64, Str) -> {}
        \\takes_pair = |_| {}
        \\
        \\outer = |t| {
        \\    get = |_| t.0
        \\    _ = takes_pair(t)
        \\    get({})
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("outer", "(I64, Str) -> I64");
}

test "a tuple access whose receiver generalizes with it still needs an annotation" {
    const source =
        \\second = |p| p.1
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Ambiguous Tuple Access");
}

test "a predeclared local annotation does not judge the enclosing body's optional access" {
    const source =
        \\get : { a : I64 } -> Try(I64, [MissingField])
        \\get = |r| {
        \\    y = r.?a
        \\    id : I64 -> I64
        \\    id = |n| n
        \\    _ = id(1)
        \\    y
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Optional Access Of Required Field");
}
