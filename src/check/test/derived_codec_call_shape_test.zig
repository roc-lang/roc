//! A derived `parser_for` or `encoder_for` called on a type name relates the
//! derived method's signature to the call, so the parsed or encoded value's
//! type determines the receiver's type arguments.
const TestEnv = @import("TestEnv.zig");

const prelude =
    \\
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
;

test "a list parser's element type comes from the annotated result" {
    var env = try TestEnv.init("Test", prelude ++
        \\parsed : Try({ value : List(U8), rest : {} }, [Bad])
        \\parsed = (List.parser_for(Format.Default))({})
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "a list parser's element type comes from a later use of the result" {
    var env = try TestEnv.init("Test", prelude ++
        \\first : U8
        \\first = match (List.parser_for(Format.Default))({}) {
        \\    Ok({ value, rest: _ }) => value.first() ?? 0
        \\    Err(_) => 0
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "a list parser whose element type is never determined is reported" {
    var env = try TestEnv.init("Test", prelude ++
        \\parsed_ok : Bool
        \\parsed_ok = (List.parser_for(Format.Default))({}) == Err(Bad)
    );
    defer env.deinit();
    try env.assertOneTypeError("Undetermined Codec Type");
}
