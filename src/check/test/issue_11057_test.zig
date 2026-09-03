//! Regression tests for issue 11057: https://github.com/roc-lang/roc/issues/11057

const TestEnv = @import("./TestEnv.zig");

const field_mismatch =
    \\**Type Mismatch**
    \\The type of the field `bar` is incompatible.
    \\```roc
    \\config2 = { ..config, bar }
    \\```
    \\                      ^^^
    \\
    \\You are trying to update the `bar` field to be the type:
    \\
    \\    [True, ..]
    \\
    \\But the `config` record needs it to be
    \\
    \\    U64
    \\
    \\__Note:__ You cannot change the type of a record field with the record update syntax. You can do that by create a new record, copying over the unchanged fields, then transforming `bar` to be the new type.
    \\
    \\
;

test "issue 11057: updating a nominal record with a wrong-typed field reports the field" {
    const source =
        \\main! = |_| {}
        \\
        \\Config := { foo : U64, bar : U64 }
        \\
        \\config : Config
        \\config = Config.{ foo: 1, bar: 2 }
        \\
        \\bar = True
        \\
        \\config2 = { ..config, bar }
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(field_mismatch);
}

test "issue 11057: updating an aliased record with a wrong-typed field reports the field" {
    const source =
        \\main! = |_| {}
        \\
        \\Config : { foo : U64, bar : U64 }
        \\
        \\config : Config
        \\config = { foo: 1, bar: 2 }
        \\
        \\bar = True
        \\
        \\config2 = { ..config, bar }
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(field_mismatch);
}

test "issue 11057: nominal update evidence uses the instantiated backing" {
    const source =
        \\main! = |_| {}
        \\
        \\Config(a) := { foo : U64, bar : a }
        \\
        \\config : Config(U64)
        \\config = Config.{ foo: 1, bar: 2 }
        \\
        \\bar = True
        \\
        \\config2 = { ..config, bar }
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(field_mismatch);
}

test "issue 11057: nominal update evidence keeps roles through an alias" {
    const source =
        \\main! = |_| {}
        \\
        \\NamedConfig := { foo : U64, bar : U64 }
        \\Config : NamedConfig
        \\
        \\config : Config
        \\config = NamedConfig.{ foo: 1, bar: 2 }
        \\
        \\bar = True
        \\
        \\config2 = { ..config, bar }
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(field_mismatch);
}

// spellchecker:off
test "issue 11057: a missing nominal record field remains a field diagnostic" {
    const source =
        \\main! = |_| {}
        \\
        \\Config := { hello : Str }
        \\
        \\config : Config
        \\config = Config.{ hello: "world" }
        \\
        \\config2 = { ..config, hllo: "goodbye" }
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This record does not have a `hllo` field.
        \\```roc
        \\config2 = { ..config, hllo: "goodbye" }
        \\```
        \\              ^^^^^^
        \\
        \\This is often due to a typo. The most similar fields are:
        \\
        \\    - `hello`
        \\
        \\So maybe `hllo` should be `hello`?
        \\
        \\__Note:__ You cannot add new fields to a record with the record update syntax.
        \\
        \\
    );
}

test "issue 11057: nominal record access uses structural evidence" {
    const source =
        \\main! = |_| {}
        \\
        \\Config := { hello : Str }
        \\
        \\config : Config
        \\config = Config.{ hello: "world" }
        \\
        \\value = config.hllo
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This record does not have a `hllo` field.
        \\```roc
        \\value = config.hllo
        \\```
        \\              ^^^^^
        \\
        \\This is often due to a typo. The most similar fields are:
        \\
        \\    - `hello`
        \\
        \\So maybe `hllo` should be `hello`?
        \\
        \\
    );
}

test "issue 11057: an empty nominal backing remains an empty-record diagnostic" {
    const source =
        \\main! = |_| {}
        \\
        \\Config := {}
        \\
        \\config : Config
        \\config = Config.{}
        \\
        \\config2 = { ..config, hello: "world" }
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\The `config` record does not have a `hello` field.
        \\```roc
        \\config2 = { ..config, hello: "world" }
        \\```
        \\              ^^^^^^
        \\
        \\It is actually a record with no fields.
        \\
        \\
    );
}
// spellchecker:on
