//! A record decoded or encoded inside a definition and used only through
//! field access closes at that definition's generalization boundary, so the
//! derived codec's error row is part of the published type. The field kinds of
//! a record literal no caller can reach settle at the same boundary.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "derived parser: a record used only through field access closes before generalization" {
    const source =
        \\greet = |json| {
        \\    person = Json.parse(json)?
        \\    Ok(Str.concat("Hello, ", person.name))
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertDefType("greet", "Str -> Try(Str, [InvalidJson(Str), MissingRequiredField(Str)])");
}

test "derived parser: a field-accessed record's field type stays generic" {
    const source =
        \\name_of = |json| {
        \\    person = Json.parse(json)?
        \\    Ok(person.name)
        \\}
        \\
        \\greeting = name_of("{\"name\":\"Ann\"}").map_ok(|name| Str.concat("Hello, ", name))
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "derived parser: callers see the required-field error of a field-accessed record" {
    const source =
        \\greet = |json| {
        \\    person = Json.parse(json)?
        \\    Ok(Str.concat("Hello, ", person.name))
        \\}
        \\
        \\describe : Str -> Str
        \\describe = |json|
        \\    match greet(json) {
        \\        Ok(s) => s
        \\        Err(InvalidJson(_)) => "bad json"
        \\    }
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

test "derived parser: an annotation omitting the required-field error of a field-accessed record is rejected" {
    const source =
        \\greet = |json| {
        \\    person = Json.parse(json)?
        \\    Ok(Str.concat("Hello, ", person.name))
        \\}
        \\
        \\greet_annotated : Str -> Try(Str, [InvalidJson(Str)])
        \\greet_annotated = greet
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Type Mismatch");
}

test "derived parser: a shared value's record keeps collecting fields across definitions" {
    const source =
        \\config = Json.parse("{\"port\":8080,\"host\":\"h\"}")
        \\
        \\port = match config {
        \\    Ok(c) => Str.repeat("*", c.port)
        \\    Err(_) => ""
        \\}
        \\
        \\host = match config {
        \\    Ok(c) => Str.concat("", c.host)
        \\    Err(_) => ""
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "derived parser: nested records used only through field access close before generalization" {
    const source =
        \\city = |json| {
        \\    rec = Json.parse(json)?
        \\    Ok(Str.concat(rec.person.address.city, "!"))
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertDefType("city", "Str -> Try(Str, [InvalidJson(Str), MissingRequiredField(Str)])");
}

test "derived parser: a nested record used only through field access closes outside any function" {
    const source =
        \\city = match Json.parse("{\"person\":{\"city\":\"X\"}}") {
        \\    Ok(rec) => Str.concat(rec.person.city, "!")
        \\    Err(_) => ""
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "derived parser: a returned record used only through field access is rejected" {
    const source =
        \\load = |json| {
        \\    person = Json.parse(json)?
        \\    _ = Str.concat("", person.name)
        \\    Ok(person)
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Record Fields Not Known");
}

test "derived encoder: a parameter record used only through field access is rejected" {
    const source =
        \\show = |rec| {
        \\    _ = Str.concat("", rec.name)
        \\    Json.to_str(rec)
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Record Fields Not Known");
}

test "derived encoder: a nested open record in a parameter is rejected" {
    const source =
        \\show = |rec| {
        \\    _ = Str.concat("", rec.person.name)
        \\    Json.to_str(rec)
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Record Fields Not Known");
}

test "derived encoder: a boundary-local record literal's field kinds settle before generalization" {
    const source =
        \\Doc := { n : U64 }
        \\
        \\encode : Doc -> Try(Str, [Infinity, NaN, NegativeInfinity])
        \\encode = |doc| Json.to_str_try({ n: doc.n })
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    // The codec's error row reaches the signature's implicitly open row, but
    // no caller can change the record's codec, so the scheme needs no
    // codec evidence from its uses.
    try std.testing.expectEqual(0, test_env.module_env.binding_scheme_codec_requirements.items.items.len);
}

test "derived encoder: a record literal the signature reaches keeps its field kinds undetermined" {
    const source =
        \\pair = |n| {
        \\    rec = { v: n }
        \\    (rec, Json.to_str_try(rec))
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expect(test_env.module_env.binding_scheme_codec_requirements.items.items.len > 0);
}

test "derived encoder: a record literal's field kind pinned by an optional access stays optional" {
    const source =
        \\encode_optional = |{}| {
        \\    n : U64
        \\    n = 1
        \\    rec = { v: n }
        \\    _ = rec.?v
        \\    Json.to_str_try(rec)
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}
