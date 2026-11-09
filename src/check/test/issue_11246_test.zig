//! Uniform format-owned errors at generated parser protocol boundaries.
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

/// Operations share the format error even when their implementations produce
/// only some of its variants.
const prelude =
    \\
    \\State := { bytes : List(U8) }.{ is_eq : _ }
    \\
    \\Format := {}.{
    \\    parse_u8 : Format, State -> Try({ value : U8, rest : State }, [BadByte(U8), NoByte, Separator(Str), Start(U64)])
    \\    parse_u8 = |_, state| match state.bytes {
    \\        [] => Err(NoByte)
    \\        [255, ..] => Err(BadByte(255))
    \\        [value, .. as bytes] => Ok({ value, rest: State.{ bytes } })
    \\    }
    \\
    \\    parse_list_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [BadByte(U8), NoByte, Separator(Str), Start(U64)])
    \\    parse_list_start = |_, state| match state.bytes {
    \\        [254, ..] => Err(Start(254))
    \\        _ => Ok(Uncounted(state))
    \\    }
    \\
    \\    parse_list_next : Format, State -> Try([Item(State), Done(State)], [BadByte(U8), NoByte, Separator(Str), Start(U64)])
    \\    parse_list_next = |_, state| match state.bytes {
    \\        [] => Ok(Done(state))
    \\        _ => Ok(Item(state))
    \\    }
    \\
    \\    parse_list_after_item : Format, State -> Try([Continue(State), Done(State)], [BadByte(U8), NoByte, Separator(Str), Start(U64)])
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

test "issue 11246: all protocol operations share the format error" {
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11246: parent cannot exclude a format error" {
    const input = try std.mem.replaceOwned(u8, std.testing.allocator, source, "parse : List(U8) -> Try({ value : List(U8), rest : State }, [BadByte(U8), NoByte, Separator(Str), Start(U64)])", "parse : List(U8) -> Try({ value : List(U8), rest : State }, [BadByte(U8), NoByte, Separator(Str)])");
    defer std.testing.allocator.free(input);
    var env = try TestEnv.init("Test", input);
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Type Mismatch");
}

test "issue 11246: parent cannot change an error payload" {
    const input = try std.mem.replaceOwned(u8, std.testing.allocator, source, "parse : List(U8) -> Try({ value : List(U8), rest : State }, [BadByte(U8), NoByte, Separator(Str), Start(U64)])", "parse : List(U8) -> Try({ value : List(U8), rest : State }, [BadByte(U8), NoByte, Separator(Str), Start(Str)])");
    defer std.testing.allocator.free(input);
    var env = try TestEnv.init("Test", input);
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Type Mismatch");
}

// The cases below are the other side of the same replay: a composed error row
// that only the generated parser decides, reaching a binding that did not
// produce it. The row still settles inside
// `finalizeGeneratedCodecConstraintsToQuiescence`, i.e. after both narrowing
// passes. An annotated VALUE's row is the annotation's at every use (design.md
// "Value Rows: Uses Share One Row"), so a caller that relates the parser's
// error row to such a value pins that row to the annotation's tags, and the
// generated parser's own tags no longer fit: the report is the parser's, at
// the parser, and never blames the value for a tag it did not produce. A
// FUNCTION's row is quantified, so each call widens a fresh copy.

/// `seed`'s single tag reaches the composed error row through a CALLER
/// (`compose`), whose generated parser decides that row at finalization. The
/// annotation bounds `seed` at every use, so the parser's row is pinned to
/// `[NoByte]` and the parser reports the tags that do not fit.
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

/// The same program with `seed` a LOCAL binding sharing one definition and
/// one binding group with the parser: a local annotated value's row is the
/// annotation's at every use too.
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

/// The same caller-side relation, but now the related binding's OWN
/// right-hand side also holds a generated parser. That parser is punted to the
/// same late window, so the binding owns a writer; the writer composes the row
/// behind `v.parse`, which lists every tag it produces, and never touches
/// `v.seed`, which the caller pins the other parser's row to. Only that other
/// parser reports.
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

/// Both rows of the same binding are decided in the same late window:
/// `v`'s own parser produces tags past what `v`'s annotation lists for
/// `v.parse`, and `v.seed`'s row pins the caller's parser. Each parser
/// reports the tags its bounded error row refuses: two reports, one per row.
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

test "issue 11246: a caller's parser row pinned to an annotated top-level value reports at the parser" {
    var env = try TestEnv.init("Test", caller_widens_top_level_value);
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Type Mismatch");
}

test "issue 11246: a caller's parser row pinned to an annotated local value reports at the parser" {
    var env = try TestEnv.init("Test", caller_widens_local_value);
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Type Mismatch");
}

test "issue 11246: a caller's late composed error row does not blame a function" {
    var env = try TestEnv.init("Test", caller_widens_function);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11246: a binding that owns a writer is not blamed for a caller's parser row" {
    var env = try TestEnv.init("Test", caller_widens_value_that_owns_a_writer);
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Type Mismatch");
}

test "issue 11246: each parser whose error row an annotation pins reports its own tags" {
    var env = try TestEnv.init("Test", owned_writer_widens_one_row_caller_widens_the_other);
    defer env.deinit();
    try env.assertTypeErrorTitles(&.{ "Parser Error Type Mismatch", "Parser Error Type Mismatch" });
}

test "issue 11246: heterogeneous format operations are rejected" {
    const input = try std.mem.replaceOwned(u8, std.testing.allocator, source, "parse_list_next : Format, State -> Try([Item(State), Done(State)], [BadByte(U8), NoByte, Separator(Str), Start(U64)])", "parse_list_next : Format, State -> Try([Item(State), Done(State)], [])");
    defer std.testing.allocator.free(input);
    var env = try TestEnv.init("Test", input);
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Type Mismatch");
}

test "issue 11246: a tag callback cannot hide incompatible payload errors" {
    const input =
        \\Format := {}.{
        \\    parse_tag_union : Format, Encoding.ParseTagUnionSpec(a), Str -> Try({ value : a, rest : Str }, [Syntax(Str)])
        \\    parse_tag_union = |encoding, spec, state| {
        \\        result = Encoding.ParseTagUnionSpec.parse(spec, {
        \\            tag: "Item", encoding, state,
        \\            start_payloads: |s, _| Ok(s),
        \\            next_payload: |s, _, _| Ok(s),
        \\            finish_payloads: |s, _| Ok(s),
        \\            missing: Syntax("unknown tag"),
        \\        })
        \\        match result {
        \\            Ok(value) => Ok(value)
        \\            Err(Syntax(message)) => Err(Syntax("caught ${message}"))
        \\        }
        \\    }
        \\}
        \\Payload := {}.{
        \\    parser_for : Format -> (Str -> Try({ value : Payload, rest : Str }, [PayloadError(Str)]))
        \\    parser_for = |_| |_| Err(PayloadError("child"))
        \\}
        \\expect {
        \\    Shape : [Item(Payload)]
        \\    parse = Shape.parser_for(Format.{})
        \\    match parse("") {
        \\        Err(PayloadError("child")) => True
        \\        _ => False
        \\    }
        \\}
    ;
    var env = try TestEnv.init("Test", input);
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Type Mismatch");
}

const error_constructors =
    \\Format := {}.{
    \\    parse_u8 : Format, Str -> Try({ value : U8, rest : Str }, [Bad, Absent(Str)])
    \\    parse_u8 = |_, _| ...
    \\    parse_tuple_start : Format, Str, U64 -> Try(Str, [Bad, Absent(Str)])
    \\    parse_tuple_start = |_, _, _| ...
    \\    parse_tuple_next : Format, Str, U64, U64 -> Try(Str, [Bad, Absent(Str)])
    \\    parse_tuple_next = |_, _, _, _| ...
    \\    parse_tuple_end : Format, Str, U64 -> Try(Str, [Bad, Absent(Str)])
    \\    parse_tuple_end = |_, _, _| ...
    \\    parse_record_start : Format, Str -> Try([Counted({ len : U64, rest : Str }), Uncounted(Str)], [Bad, Absent(Str)])
    \\    parse_record_start = |_, _| ...
    \\    parse_record_field : Format, Encoding.FieldName.FieldNames(_shape), Str -> Try([Done(Str)], [Bad, Absent(Str)])
    \\    parse_record_field = |_, _, _| ...
    \\    parse_record_after_field : Format, Str -> Try([Continue(Str), Done(Str)], [Bad, Absent(Str)])
    \\    parse_record_after_field = |_, _| ...
    \\    skip_record_field : Format, Str -> Try(Str, [Bad, Absent(Str)])
    \\    skip_record_field = |_, _| ...
    \\    rename_field : Format, Str -> Str
    \\    rename_field = |_, name| name
    \\    missing_field : Format, Str -> [Bad, Absent(Str)]
    \\    missing_field = |_, name| Absent(name)
    \\    invalid_value : Format, Str -> [Bad, Absent(Str)]
    \\    invalid_value = |_, _| Bad
    \\}
    \\expect {
    \\    Shape : { count : U8, pair : (U8, U8) }
    \\    parse = Shape.parser_for(Format.{})
    \\    match parse("") {
    \\        Ok(_) => True
    \\        Err(_) => False
    \\    }
    \\}
;

test "format errors: error constructors accept the complete format error" {
    var env = try TestEnv.init("Test", error_constructors);
    defer env.deinit();
    try env.assertNoErrors();
}

test "format errors: missing_field cannot infer a narrower error" {
    const input = try std.mem.replaceOwned(u8, std.testing.allocator, error_constructors, "missing_field : Format, Str -> [Bad, Absent(Str)]", "missing_field : Format, Str -> _");
    defer std.testing.allocator.free(input);
    var env = try TestEnv.init("Test", input);
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Type Mismatch");
}

test "format errors: invalid_value cannot infer a narrower error" {
    const input = try std.mem.replaceOwned(u8, std.testing.allocator, error_constructors, "invalid_value : Format, Str -> [Bad, Absent(Str)]", "invalid_value : Format, Str -> _");
    defer std.testing.allocator.free(input);
    var env = try TestEnv.init("Test", input);
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Type Mismatch");
}
