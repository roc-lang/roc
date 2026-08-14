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

test "issue 10759: unresolved ordinary value passed to inspect remains polymorphic" {
    // A row tail has an explicit contextual close-to-empty default, but an
    // ordinary value position does not. Keep rejecting the latter.
    const source =
        \\make : {} -> a
        \\make = |_| crash "no value"
        \\a = make({})
        \\main = || Str.inspect(a)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Polymorphic Value");
}

test "issue 10759: a variable shared by a row tail and value remains polymorphic" {
    // Traversing the tag union sees `a` first as a row tail, where it has a
    // contextual close-to-empty default. The tuple's second element is an
    // ordinary occurrence of the same variable and must still be rejected.
    const source =
        \\make : {} -> ([Tag, ..a], a)
        \\make = |_| crash "no value"
        \\value = make({})
        \\main = || Str.inspect(value)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Polymorphic Value");
}
