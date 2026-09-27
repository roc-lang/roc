//! Regression for https://github.com/roc-lang/roc/issues/11728.
const TestEnv = @import("TestEnv.zig");
const std = @import("std");

const token =
    \\Token := { raw : Str }.{
    \\    parser_for : encoding -> (state -> Try({ value : Token, rest : state }, err))
    \\        where [encoding.parse_str : encoding, state -> Try({ value : Str, rest : state }, err)]
    \\    parser_for = |encoding| {
    \\        Encoding : encoding
    \\        |state| {
    \\            parsed = Encoding.parse_str(encoding, state)?
    \\            Ok({ value: Token.{ raw: parsed.value }, rest: parsed.rest })
    \\        }
    \\    }
    \\}
    \\
;

test "issue 11728: format-generic custom parser as a record field" {
    var env = try TestEnv.init("Test", token ++
        \\parse : Str -> Try({ t : Token }, _)
        \\parse = |json| Json.parse(json)
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11728: format-generic custom parser alone and in nested containers" {
    var env = try TestEnv.init("Test", token ++
        \\alone : Str -> Try(Token, _)
        \\alone = |json| Json.parse(json)
        \\nested : Str -> Try({ tokens : List(Token) }, _)
        \\nested = |json| Json.parse(json)
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11728: imported format-generic custom parser as a record field" {
    var imported = try TestEnv.init("Token", token);
    defer imported.deinit();
    try imported.assertNoErrors();
    var env = try TestEnv.initWithImport("Test",
        \\import Token
        \\parse : Str -> Try({ t : Token }, _)
        \\parse = |json| Json.parse(json)
    , "Token", &imported);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11728: parent cannot exclude the custom parser format error" {
    var env = try TestEnv.init("Test", token ++
        \\parse : Str -> Try({ t : Token }, [MissingRequiredField(Str)])
        \\parse = |json| Json.parse(json)
    );
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "issue 11728: parent cannot change the custom parser error payload" {
    var env = try TestEnv.init("Test", token ++
        \\parse : Str -> Try({ t : Token }, [InvalidJson(U64), MissingRequiredField(Str)])
        \\parse = |json| Json.parse(json)
    );
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

// The format method has its own requirement, so the error becomes known only
// after dispatching Token.parser_for -> Format.parse_str -> State.read.
// Collection methods are infallible: ReadFailed comes only from the child.
const transitive = token ++
    \\State := {}.{
    \\    read : State -> Try({ value : Str, rest : State }, [ReadFailed(Str)])
    \\    read = |_| Err(ReadFailed("failed"))
    \\}
    \\Format := {}.{
    \\    parse_str : Format, state -> Try({ value : Str, rest : state }, err)
    \\        where [state.read : state -> Try({ value : Str, rest : state }, err)]
    \\    parse_str = |_, state| {
    \\        S : state
    \\        S.read(state)
    \\    }
    \\    parse_list_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [])
    \\    parse_list_start = |_, state| Ok(Counted({ len: 1, rest: state }))
    \\    parse_list_next : Format, State -> Try([Item(State), Done(State)], [])
    \\    parse_list_next = |_, state| Ok(Done(state))
    \\    parse_list_after_item : Format, State -> Try([Continue(State), Done(State)], [])
    \\    parse_list_after_item = |_, state| Ok(Done(state))
    \\}
    \\parse : {} -> Try({ value : List(Token), rest : State }, [ReadFailed(Str)])
    \\parse = |_| {
    \\    T : List(Token)
    \\    parser = T.parser_for(Format.{})
    \\    parser(State.{})
    \\}
;

test "issue 11728: custom parser errors settle through transitive requirements" {
    var env = try TestEnv.init("Test", transitive);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11728: format parser errors settle before row composition" {
    const source = try std.mem.replaceOwned(u8, std.testing.allocator, transitive, "List(Token)", "List(Str)");
    defer std.testing.allocator.free(source);
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11728: parent rejects an error contributed only by the custom parser" {
    const source = try std.mem.replaceOwned(u8, std.testing.allocator, transitive, "rest : State }, [ReadFailed(Str)])\nparse =", "rest : State }, [])\nparse =");
    defer std.testing.allocator.free(source);
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Row Missing Tag");
}
