//! Regression tests for issue 10759.

const TestEnv = @import("./TestEnv.zig");

test "issue 10759: Json.parse wildcard error row can be inspected" {
    // This is the module the REPL builds for:
    //
    //     » a : Try(U64, _)
    //     » a = Json.parse("123")
    //     » a
    const source =
        \\a : Try(U64, _)
        \\a = Json.parse("123")
        \\main = || Str.inspect((a))
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 10759: an open record row can be inspected" {
    const source =
        \\make : {} -> { value: U64, ..others }
        \\make = |_| crash "no value"
        \\value = make({})
        \\main = || Str.inspect(value)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 10759: a value whose whole type is unresolved can be inspected" {
    // Nothing pins `a`'s type and it carries no method requirement, so it is
    // not a polymorphic-value error; no value of that type is ever rendered.
    const source =
        \\make : {} -> a
        \\make = |_| crash "no value"
        \\a = make({})
        \\main = || Str.inspect(a)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 10759: a variable shared by a row tail and a value can be inspected" {
    // `a` occurs both as the tag union's row tail and as the tuple's second
    // element. It carries no method requirement in either position.
    const source =
        \\make : {} -> ([Tag, ..a], a)
        \\make = |_| crash "no value"
        \\value = make({})
        \\main = || Str.inspect(value)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}
