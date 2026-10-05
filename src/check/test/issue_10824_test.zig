//! Regression tests for issue 10824.

const TestEnv = @import("./TestEnv.zig");

test "issue 10824: inferred encoder record row closes at quiescence" {
    const source =
        \\encode_count = |json| {
        \\    record = Json.parse(json)?
        \\    _count : U64
        \\    _count = record.count
        \\    Ok(Json.to_str(record))
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertDefType("encode_count", "Str -> Try(Str, [InvalidJson(Str), MissingRequiredField(Str)])");
}

test "issue 10824: derived encoder rejects an inferred open record parameter" {
    const source =
        \\encode_count = |record| {
        \\    _count : U64
        \\    _count = record.count
        \\    Json.to_str(record)
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Record Fields Not Known");
}

test "issue 10824: derived encoder rejects a rigid open record row" {
    const source =
        \\encode_open : { count : U64, ..rest } -> Str
        \\encode_open = |record| Json.to_str(record)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}

test "issue 10824: derived parser rejects a rigid open record row" {
    const source =
        \\parse_open : Str -> Try({ count : U64, ..rest }, _)
        \\parse_open = |json| Json.parse(json)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}
