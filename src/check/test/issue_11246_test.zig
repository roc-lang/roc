//! Error composition at generated parser protocol boundaries (issue 11246).
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

const source =
    \\
    \\State := { bytes : List(U8) }.{ is_eq : _ }
    \\
    \\Format := {}.{
    \\    parse_u8 : Format, State -> Try({ value : U8, rest : State }, [BadByte(U8), NoByte])
    \\    parse_u8 = |_, state| match state.bytes {
    \\        [] => Err(NoByte)
    \\        [255, ..] => Err(BadByte(255))
    \\        [value, .. as bytes] => Ok({ value, rest: State.{ bytes } })
    \\    }
    \\
    \\    parse_list_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [Start(U64)])
    \\    parse_list_start = |_, state| match state.bytes {
    \\        [254, ..] => Err(Start(254))
    \\        _ => Ok(Uncounted(state))
    \\    }
    \\
    \\    parse_list_next : Format, State -> Try([Item(State), Done(State)], [])
    \\    parse_list_next = |_, state| match state.bytes {
    \\        [] => Ok(Done(state))
    \\        _ => Ok(Item(state))
    \\    }
    \\
    \\    parse_list_after_item : Format, State -> Try([Continue(State), Done(State)], [Separator(Str)])
    \\    parse_list_after_item = |_, state| match state.bytes {
    \\        [] => Ok(Done(state))
    \\        [0, .. as bytes] => Ok(Continue(State.{ bytes }))
    \\        _ => Err(Separator("expected zero"))
    \\    }
    \\}
    \\
    \\parse : List(U8) -> Try({ value : List(U8), rest : State }, [BadByte(U8), NoByte, Separator(Str), Start(U64)])
    \\parse = |bytes| {
    \\    T : List(U8)
    \\    parse_ = T.parser_for(Format.{})
    \\    parse_(State.{ bytes })
    \\}
;

test "issue 11246: infallible and distinct closed format errors compose" {
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11246: parent cannot exclude a format error" {
    const input = try std.mem.replaceOwned(u8, std.testing.allocator, source, "NoByte, Separator(Str), Start(U64)])", "NoByte, Separator(Str)])");
    defer std.testing.allocator.free(input);
    var env = try TestEnv.init("Test", input);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "issue 11246: parent cannot change an error payload" {
    const input = try std.mem.replaceOwned(u8, std.testing.allocator, source, "NoByte, Separator(Str), Start(U64)])", "NoByte, Separator(Str), Start(Str)])");
    defer std.testing.allocator.free(input);
    var env = try TestEnv.init("Test", input);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}
