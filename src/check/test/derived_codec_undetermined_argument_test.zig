//! A derived nominal codec reads the arguments its backing uses as data. An
//! argument nothing ever determines leaves part of a parser's shape
//! undetermined, so the parser is rejected at the final codec boundary. An
//! encoder is accepted: no value has the part of the shape that carries such
//! an argument. An argument the backing never reads, or one a generic
//! definition's uses each determine, blocks neither derivation.

const TestEnv = @import("TestEnv.zig");

const my_try =
    \\MyTry(ok, err) := [Ok(ok), Err(err)].{
    \\    encoder_for : _
    \\    parser_for : _
    \\}
    \\
;

test "derived parser rejects a user nominal field whose error argument is never determined" {
    var test_env = try TestEnv.init("Test", my_try ++
        \\pb : Try({ x : U64, y : MyTry(U64, _) }, _)
        \\pb = Json.parse("{\"x\":1}")
    );
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Missing Method", "never resolved to a concrete type");
}

test "derived parser rejects a user nominal field whose payload argument is never determined" {
    var test_env = try TestEnv.init("Test", my_try ++
        \\pb : Try({ x : U64, y : MyTry(_, [Missing]) }, _)
        \\pb = Json.parse("{\"x\":1}")
    );
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Missing Method", "never resolved to a concrete type");
}

test "derived parser rejects an undetermined user nominal argument in a list and at the top level" {
    var test_env = try TestEnv.init("Test", my_try ++
        \\in_list : Try(List({ x : U64, y : MyTry(U64, _) }), _)
        \\in_list = Json.parse("[]")
        \\
        \\top : Try(MyTry(U64, _), _)
        \\top = Json.parse("{\"Ok\":1}")
    );
    defer test_env.deinit();

    try test_env.assertTypeErrorTitles(&.{ "Missing Method", "Missing Method" });
}

test "derived parser rejects a builtin Try outside the codec conventions whose error argument is never determined" {
    var test_env = try TestEnv.init("Test",
        \\pb : Try(Try(U64, _), _)
        \\pb = Json.parse("1")
    );
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Missing Method", "never resolved to a concrete type");
}

test "derived encoder accepts a user nominal whose error argument is never determined" {
    var test_env = try TestEnv.init("Test", my_try ++
        \\value : { x : U64, y : MyTry(U64, _) }
        \\value = { x: 1, y: MyTry.Ok(2) }
        \\
        \\top : MyTry(U64, _)
        \\top = MyTry.Ok(2)
        \\
        \\out = Json.to_str(value)
        \\
        \\top_out = Json.to_str(top)
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "derived encoder accepts a builtin Try whose error argument is never determined" {
    var test_env = try TestEnv.init("Test",
        \\value : { x : U64, y : Try(U64, _) }
        \\value = { x: 1, y: Try.Ok(2) }
        \\
        \\out = Json.to_str(value)
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "derived codecs accept a user nominal whose arguments are determined" {
    var test_env = try TestEnv.init("Test", my_try ++
        \\pb : Try({ x : U64, y : MyTry(U64, [Missing]) }, _)
        \\pb = Json.parse("{\"x\":1}")
        \\
        \\value : { x : U64, y : MyTry(U64, [Missing]) }
        \\value = { x: 1, y: MyTry.Ok(2) }
        \\
        \\out = Json.to_str(value)
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "derived codecs accept an undetermined argument the backing never reads" {
    var test_env = try TestEnv.init("Test",
        \\Tagged(phantom) := { x : U64 }.{
        \\    encoder_for : _
        \\    parser_for : _
        \\}
        \\
        \\tagged : Try(Tagged(_), _)
        \\tagged = Json.parse("{\"x\":3}")
        \\
        \\tagged_value : Tagged(_)
        \\tagged_value = Tagged.{ x: 4 }
        \\
        \\out = Json.to_str(tagged_value)
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "derived codecs accept an argument a generic definition's uses determine" {
    var test_env = try TestEnv.init("Test",
        \\Wrap(a) := { x : a }.{
        \\    encoder_for : _
        \\    parser_for : _
        \\}
        \\
        \\encode_wrapped = |v| Json.to_str(Wrap.{ x: v })
        \\
        \\parse_any = |s| Json.parse(s)
        \\
        \\wrapped : Try(Wrap(U64), _)
        \\wrapped = parse_any("{\"x\":5}")
        \\
        \\out = encode_wrapped("s")
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
}
