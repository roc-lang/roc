//! Error composition at generated parser protocol boundaries (issue 11246).
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

/// Formats whose four protocol methods fail with distinct closed error rows,
/// one of them infallible. Shared by every case below; only the tail that
/// consumes the generated parser differs.
const prelude =
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
;

const source = prelude ++
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

// The three cases below are the other side of the same replay: a composed
// error row that only the generated parser decides, reaching a binding that
// did not produce it. The row still settles inside
// `finalizeGeneratedCodecConstraintsToQuiescence`, i.e. after both narrowing
// passes, so the late audit sees it widen; but the expression that introduced
// the relation lies outside the annotated binding, which is an ordinary caller
// using an output-position row at a wider union.

/// `seed`'s single tag reaches the composed error row through a CALLER
/// (`compose`), whose generated parser decides that row at finalization. The
/// annotation bounds `seed` itself, which produced only `NoByte`.
const caller_widens_top_level_value = prelude ++
    \\seed : [NoByte]
    \\seed = NoByte
    \\
    \\compose : List(U8) -> Str
    \\compose = |bytes| {
    \\    T : List(U8)
    \\    parse_ = T.parser_for(Format.{})
    \\    match parse_(State.{ bytes }) {
    \\        Ok(_) => "ok"
    \\        Err(e) => {
    \\            _ = [e, seed]
    \\            "err"
    \\        }
    \\    }
    \\}
;

/// The same widening with `seed` a LOCAL binding sharing one definition and
/// one binding group with the parser that widens it: writer identity has to be
/// finer than the enclosing definition to tell these apart.
const caller_widens_local_value = prelude ++
    \\compose : List(U8) -> Str
    \\compose = |bytes| {
    \\    seed : [NoByte]
    \\    seed = NoByte
    \\    T : List(U8)
    \\    parse_ = T.parser_for(Format.{})
    \\    match parse_(State.{ bytes }) {
    \\        Ok(_) => "ok"
    \\        Err(e) => {
    \\            _ = [e, seed]
    \\            "err"
    \\        }
    \\    }
    \\}
;

/// The function form of the same program. A function's opened row is
/// quantified, so each call widens a fresh copy and the binding's own row
/// never gains the caller's tags. It is accepted for a different reason than
/// the two above, and it pins that reason.
const caller_widens_function = prelude ++
    \\seed : {} -> [NoByte]
    \\seed = |_| NoByte
    \\
    \\compose : List(U8) -> Str
    \\compose = |bytes| {
    \\    T : List(U8)
    \\    parse_ = T.parser_for(Format.{})
    \\    match parse_(State.{ bytes }) {
    \\        Ok(_) => "ok"
    \\        Err(e) => {
    \\            _ = [e, seed({})]
    \\            "err"
    \\        }
    \\    }
    \\}
;

/// The same caller-side widening, but now the widened binding's OWN
/// right-hand side also holds a generated parser. That parser is punted to the
/// same late window, so the binding owns a writer; the writer composes the row
/// behind `v.parse` and never touches `v.seed`, which only the caller widens.
/// Containment alone cannot separate the two: the writer has to be matched
/// against the row it could actually have written.
const caller_widens_value_that_owns_a_writer = prelude ++
    \\v : { parse : State -> Try({ value : List(U8), rest : State }, [BadByte(U8), NoByte, Separator(Str), Start(U64)]), seed : [NoByte] }
    \\v = {
    \\    T : List(U8)
    \\    parse_ = T.parser_for(Format.{})
    \\    { parse: parse_, seed: NoByte }
    \\}
    \\
    \\compose : List(U8) -> Str
    \\compose = |bytes| {
    \\    T : List(U8)
    \\    p = T.parser_for(Format.{})
    \\    match p(State.{ bytes }) {
    \\        Ok(_) => "ok"
    \\        Err(e) => {
    \\            _ = [e, v.seed]
    \\            "err"
    \\        }
    \\    }
    \\}
;

/// Both rows of the same binding move in the same late window, for opposite
/// reasons: `v.parse`'s row is widened by `v`'s own parser past what `v`'s
/// annotation lists (blamed), while `v.seed`'s row is widened by the caller
/// (not blamed). Exactly one report, and it is the first row's.
const owned_writer_widens_one_row_caller_widens_the_other = prelude ++
    \\v : { parse : State -> Try({ value : List(U8), rest : State }, [BadByte(U8), NoByte, Separator(Str)]), seed : [NoByte] }
    \\v = {
    \\    T : List(U8)
    \\    parse_ = T.parser_for(Format.{})
    \\    { parse: parse_, seed: NoByte }
    \\}
    \\
    \\compose : List(U8) -> Str
    \\compose = |bytes| {
    \\    T : List(U8)
    \\    p = T.parser_for(Format.{})
    \\    match p(State.{ bytes }) {
    \\        Ok(_) => "ok"
    \\        Err(e) => {
    \\            _ = [e, v.seed]
    \\            "err"
    \\        }
    \\    }
    \\}
;

test "issue 11246: a caller's late composed error row does not blame a top-level value" {
    var env = try TestEnv.init("Test", caller_widens_top_level_value);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11246: a caller's late composed error row does not blame a local value" {
    var env = try TestEnv.init("Test", caller_widens_local_value);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11246: a caller's late composed error row does not blame a function" {
    var env = try TestEnv.init("Test", caller_widens_function);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11246: a binding that owns a writer is not blamed for a caller's widening" {
    var env = try TestEnv.init("Test", caller_widens_value_that_owns_a_writer);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11246: only the row the owned writer reaches is blamed" {
    var env = try TestEnv.init("Test", owned_writer_widens_one_row_caller_widens_the_other);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}
