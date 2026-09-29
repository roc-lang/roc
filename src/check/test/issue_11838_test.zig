//! Regression test for https://github.com/roc-lang/roc/issues/11838.
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

// A nominal type whose `parser_for` is derived calls into a nested nominal
// type's own derived `parser_for`. `Json.parse` at the top level of `Try(Wrap,
// _)` checks fine, but wrapping the same shape in a `List` (or a record with an
// empty error row) makes the checker demand the nested `parse_tag_union` call
// have an impossible empty error row: the nested parser can genuinely produce
// `InvalidJson(Str)`, so the `List(Wrap)` parse must type check.
const source =
    \\Flag := [On, Off].{
    \\    parser_for : _
    \\}
    \\
    \\Wrap := [W(Flag)].{
    \\    parser_for = |encoding| {
    \\        parse_flag = Flag.parser_for(encoding)
    \\        |state| {
    \\            parsed = parse_flag(state)?
    \\            Ok({ value: W(parsed.value), rest: parsed.rest })
    \\        }
    \\    }
    \\}
    \\
    \\alone : Str -> Try(Wrap, _)
    \\alone = |json| Json.parse(json)
    \\
    \\many : Str -> Try(List(Wrap), _)
    \\many = |json| Json.parse(json)
    \\
;

test "issue 11838: custom parser_for over a derived parser_for parses a List at the top level" {
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11838: custom parser_for over a derived parser_for parses a record field" {
    var env = try TestEnv.init("Test",
        \\Flag := [On, Off].{
        \\    parser_for : _
        \\}
        \\
        \\Wrap := [W(Flag)].{
        \\    parser_for = |encoding| {
        \\        parse_flag = Flag.parser_for(encoding)
        \\        |state| {
        \\            parsed = parse_flag(state)?
        \\            Ok({ value: W(parsed.value), rest: parsed.rest })
        \\        }
        \\    }
        \\}
        \\
        \\field : Str -> Try({ w : Wrap }, _)
        \\field = |json| Json.parse(json)
        \\
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11838: custom parser_for over a derived record parser_for parses a List" {
    var env = try TestEnv.init("Test",
        \\Flag := { on : Bool }.{
        \\    parser_for : _
        \\}
        \\
        \\Wrap := [W(Flag)].{
        \\    parser_for = |encoding| {
        \\        parse_flag = Flag.parser_for(encoding)
        \\        |state| {
        \\            parsed = parse_flag(state)?
        \\            Ok({ value: W(parsed.value), rest: parsed.rest })
        \\        }
        \\    }
        \\}
        \\
        \\many : Str -> Try(List(Wrap), _)
        \\many = |json| Json.parse(json)
        \\
    );
    defer env.deinit();
    try env.assertNoErrors();
}
