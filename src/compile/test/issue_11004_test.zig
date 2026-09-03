//! Regression test for issue #11004.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 11004: structural encoder for a record with an optional field lowers to LIR" {
    try expectLowersToLir(
        \\Xml := {}.{
        \\    tag : { class ?: Str } -> Xml
        \\    tag = |attrs| tag2(attrs)
        \\
        \\    tag2 : attrs -> Xml where [attrs.encoder_for : Attrs -> (attrs, Attrs.State -> Try(Attrs.State, []))]
        \\    tag2 = |_attrs| {
        \\        T : attrs
        \\        _encode = T.encoder_for(Attrs.{})
        \\        crash "tag2 implementation unfinished"
        \\    }
        \\}
        \\
        \\Attrs := {}.{
        \\    State : {}
        \\
        \\    rename_field : Attrs, Str -> Str
        \\    rename_field = |_, _| crash "rename_field not implemented"
        \\
        \\    encode_record : State, U64, (State, (State, Str, (State -> Try(State, [])) -> Try(State, [])) -> Try(State, [])) -> Try(State, [])
        \\    encode_record = |_, _, _| crash "encode_record not implemented"
        \\
        \\    encode_str : Str, State -> Try(State, [])
        \\    encode_str = |_, _| crash "encode_str not implemented"
        \\}
        \\
        \\main! = |_args| {
        \\    _ = Xml.tag({ class: "top" })
        \\    Ok({})
        \\}
    );
}
