//! Regression tests for issue #11923: a definition whose body produces a tag
//! its annotation does not list keeps its annotated type, so lowering a call
//! that reaches it through a generated codec relates one signature. A derived
//! codec called on a type name takes its type arguments from its result.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "issue 11923: a runtime parser reaching a format method that produces an unlisted tag lowers" {
    try expectLowersToLirWithOptions(
        \\Format := [Default, Other].{
        \\    parse_u8 : Format, {} -> Try({ value : U8, rest : {} }, [Bad])
        \\    parse_u8 = |_, _| Err(OtherErr)
        \\}
        \\
        \\main! = |args| {
        \\    fmt = if args.len() > 5 Format.Other else Format.Default
        \\    result = (U8.parser_for(fmt))({})
        \\    echo!(if result == Err(Bad) "bad" else "other")
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true });
}

test "a list parser whose element type comes from a later use lowers" {
    try expectLowersToLirWithOptions(
        \\Format := [Default].{
        \\    parse_u8 : Format, {} -> Try({ value : U8, rest : {} }, [Bad])
        \\    parse_u8 = |_, rest| Ok({ value: 7, rest })
        \\
        \\    parse_list_start : Format, {} -> Try([Counted({ len : U64, rest : {} }), Uncounted({})], [Bad])
        \\    parse_list_start = |_, rest| Ok(Counted({ len: 2, rest }))
        \\
        \\    parse_list_next : Format, {} -> Try([Item({}), Done({})], [Bad])
        \\    parse_list_next = |_, _| Err(Bad)
        \\
        \\    parse_list_after_item : Format, {} -> Try([Continue({}), Done({})], [Bad])
        \\    parse_list_after_item = |_, _| Err(Bad)
        \\}
        \\
        \\main! = |_args| {
        \\    parsed = (List.parser_for(Format.Default))({})
        \\    echo!(match parsed {
        \\        Ok({ value, rest: _ }) => if value == [7.U8, 7] "parsed" else "wrong"
        \\        Err(_) => "failed"
        \\    })
        \\    Ok({})
        \\}
    , .{});
}
