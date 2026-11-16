//! A derived codec reads a type variable of the generic definition it is
//! derived in through the requirement the variable's where clause declares
//! (design.md "Structural Serialization Methods"). A variable without such a
//! requirement, or whose requirement is for another codec, is rejected.

const TestEnv = @import("TestEnv.zig");

test "issue 13007 forwarding: a parser reads a constrained type variable" {
    var env = try TestEnv.init("Test",
        \\value_of : Str -> Try(a, [InvalidJson(Str)]) where [a.Json.Parseable([InvalidJson(Str)])]
        \\value_of = |text| {
        \\    parsed : Try({ value : a }, [InvalidJson(Str)])
        \\    parsed = Json.parse(text)
        \\    match parsed {
        \\        Ok(record) => Ok(record.value)
        \\        Err(e) => Err(e)
        \\    }
        \\}
        \\
        \\used : Try(U64, [InvalidJson(Str)])
        \\used = value_of("{\"value\":1}")
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 13007 forwarding: a nested record of a list of records reads one type variable" {
    var env = try TestEnv.init("Test",
        \\nested : a, b -> Str where [a.Json.Encodable([]), b.Json.Encodable([])]
        \\nested = |x, y| Json.to_str({ items: [{ v: x, w: (y, x) }] })
        \\
        \\first = nested(1.U64, "a")
        \\second = nested("b", [2.U8])
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("first", "Str");
    try env.assertDefType("second", "Str");
}

test "issue 13007 forwarding: a type variable without a codec requirement is rejected" {
    var env = try TestEnv.init("Test",
        \\in_record : a -> Str
        \\in_record = |x| Json.to_str({ value: x })
    );
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

test "issue 13007 forwarding: a requirement for another encoding is rejected" {
    var env = try TestEnv.init("Test",
        \\in_record : a -> Str where [a.encoder_for : Str -> (a, Str -> Try(Str, []))]
        \\in_record = |x| Json.to_str({ value: x })
    );
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

test "issue 13007 forwarding: an encoding requirement does not parse" {
    var env = try TestEnv.init("Test",
        \\value_of : Str -> Try(a, [InvalidJson(Str)]) where [a.Json.Encodable([])]
        \\value_of = |text| {
        \\    parsed : Try({ value : a }, [InvalidJson(Str)])
        \\    parsed = Json.parse(text)
        \\    match parsed {
        \\        Ok(record) => Ok(record.value)
        \\        Err(e) => Err(e)
        \\    }
        \\}
    );
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

test "issue 13007 forwarding: an imported generic function encodes at each importer's types" {
    var source = try TestEnv.init("Codecs",
        \\Codecs := {}.{
        \\    in_pair = |_self, x, y| in_pair_of(x, y)
        \\}
        \\
        \\in_pair_of : a, b -> Str where [a.Json.Encodable([]), b.Json.Encodable([])]
        \\in_pair_of = |x, y| Json.to_str({ left: x, right: (y, x) })
    );
    defer source.deinit();
    try source.assertNoErrors();

    var importer = try TestEnv.initWithImport("Importer",
        \\import Codecs
        \\first = Codecs.in_pair(Codecs.{}, 1.U64, "a")
        \\second = Codecs.in_pair(Codecs.{}, "b", [True])
    , "Codecs", &source);
    defer importer.deinit();
    try importer.assertNoErrors();
    try importer.assertDefType("first", "Str");
    try importer.assertDefType("second", "Str");
}
