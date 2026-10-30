//! repro for https://github.com/roc-lang/roc/issues/12677
//! A generic helper that forwards a callback's `[Bad]` error with `?` and adds
//! its own `Err(Bad)` composes both into the single `Bad` alternative, so an
//! annotated caller expecting `Try(value, [Bad])` checks cleanly, even when it
//! first propagates another error with `?`. The call's error row repeats `Bad`
//! along its extension chain (design.md "Row Union Normalization"); the `?`
//! composition and every rendering of the row treat the repeat as one tag.
const TestEnv = @import("TestEnv.zig");

const decode =
    \\decode = |flag, parse| {
    \\    value = parse(flag)?
    \\    if flag { Err(Bad) } else { Ok(value) }
    \\}
    \\
;

test "issue 12677: inferred encoder/decoder error composition yields a single Bad tag" {
    var env = try TestEnv.init("Union",
        \\Union :: {}.{
        \\    Problem : [Bad]
        \\    Codec :: [Default]
        \\
        \\    run : params, Bool, (Bool -> Try(value, Problem)) -> Try(value, Problem)
        \\        where [params.encoder_for : Codec -> (params, {} -> Try({}, Problem))]
        \\    run = |params, flag, parse| {
        \\        _ = encode(params)?
        \\        decode(flag, parse)
        \\    }
        \\}
        \\
        \\encode : params -> Try({}, _)
        \\    where [params.encoder_for : Union.Codec -> (params, {} -> Try({}, _))]
        \\encode = |params| {
        \\    Params : params
        \\    apply = Params.encoder_for(Union.Codec.Default)
        \\    apply(params, {})
        \\}
        \\
    ++ decode);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12677: a repeated tag reaching an annotated ? composition is one tag" {
    var env = try TestEnv.init("Test",
        \\Problem : [Bad]
        \\
        \\run : Str, Bool, (Bool -> Try(value, Problem)) -> Try(value, Problem)
        \\run = |s, flag, parse| {
        \\    _ = enc(s)?
        \\    decode(flag, parse)
        \\}
        \\
        \\enc : Str -> Try({}, [Bad])
        \\enc = |s| if s == "" Ok({}) else Err(Bad)
        \\
    ++ decode);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12677: a repeated tag reaching an unannotated ? composition is one tag" {
    var env = try TestEnv.init("Test",
        \\run = |s, flag, parse| {
        \\    _ = enc(s)?
        \\    decode(flag, parse)
        \\}
        \\
        \\use : Str, Bool, (Bool -> Try(U64, [Bad])) -> Try(U64, [Bad])
        \\use = |s, flag, parse| run(s, flag, parse)
        \\
        \\enc : Str -> Try({}, [Bad])
        \\enc = |s| if s == "" Ok({}) else Err(Bad)
        \\
    ++ decode);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12677: a repeated tag with conflicting payloads in a ? composition is one conflict" {
    var env = try TestEnv.init("Test",
        \\run : Str, Bool, (Bool -> Try(value, [Bad(Str)])) -> Try(value, [Bad(Str)])
        \\run = |s, flag, parse| {
        \\    _ = enc(s)?
        \\    decode_num(flag, parse)
        \\}
        \\
        \\enc : Str -> Try({}, [Bad(Str)])
        \\enc = |s| if s == "" Ok({}) else Err(Bad(s))
        \\
        \\decode_num = |flag, parse| {
        \\    value = parse(flag)?
        \\    if flag { Err(Bad(flag)) } else { Ok(value) }
        \\}
    );
    defer env.deinit();
    try env.assertOneTypeError("Conflicting Tag");
}

test "issue 12677: a type whose row repeats a tag renders the tag once" {
    var env = try TestEnv.init("Test",
        \\run : Bool, (Bool -> Try(value, [Bad])) -> Str
        \\run = |flag, parse| {
        \\    x : Str
        \\    x = decode(flag, parse)
        \\    x
        \\}
        \\
    ++ decode);
    defer env.deinit();
    try env.assertOneTypeProblemMsgContains("Type Mismatch",
        \\It has the type:
        \\
        \\    Try(value, [Bad])
        \\
    );
}

test "issue 12677: a repeated tag with conflicting payloads related without ? is one conflict" {
    var env = try TestEnv.init("Test",
        \\run : Bool, (Bool -> Try(value, [Bad(Str)])) -> Try(value, [Bad(Str)])
        \\run = |flag, parse| decode_num(flag, parse)
        \\
        \\decode_num = |flag, parse| {
        \\    value = parse(flag)?
        \\    if flag { Err(Bad(flag)) } else { Ok(value) }
        \\}
    );
    defer env.deinit();
    try env.assertOneTypeError("Conflicting Tag");
}
