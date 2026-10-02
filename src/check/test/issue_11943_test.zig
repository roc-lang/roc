//! Regression tests for issue 11943.

const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/11943
//
// In `Q.U.v`, when `Q` names a type and `Q.U` names none, `Q.U` is a qualified
// tag and `v` is a member accessed on it, exactly as in `(Q.U).v`. A nested type
// named `Q.U` takes precedence. Imported types are covered by
// src/compile/test/issue_11943_test.zig, which checks through the real build.

const color_decl =
    \\Color := [Red, Green].{
    \\    Shade := [Light, Dark].{
    \\        describe : Shade -> Str
    \\        describe = |_| "shade"
    \\    }
    \\
    \\    to_hex : Color -> Str
    \\    to_hex = |color| match color {
    \\        Red => "#f00"
    \\        Green => "#0f0"
    \\    }
    \\
    \\    with_suffix : Color, Str -> Str
    \\    with_suffix = |_, suffix| suffix
    \\}
    \\
;

test "issue 11943: method called directly on a qualified tag" {
    const source = color_decl ++
        \\hex = Color.Red.to_hex()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("hex", "Str");
}

test "issue 11943: method on a qualified tag as an arrow target" {
    const source = color_decl ++
        \\suffixed = "!"->Color.Green.with_suffix()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("suffixed", "Str");
}

test "issue 11943: method on a tag of a nested type" {
    const source = color_decl ++
        \\shade = Color.Shade.Dark.describe()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("shade", "Str");
}

test "issue 11943: method on a qualified builtin tag" {
    const source =
        \\flipped = Bool.True.not()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("flipped", "Bool");
}

test "issue 11943: an uncalled member of a qualified tag is a field access" {
    const source = color_decl ++
        \\hex = Color.Red.to_hex
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

test "issue 11943: a nested type takes precedence over a tag of the same name" {
    const source = color_decl ++
        \\missing = Color.Shade.missing()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneCanErrorMsg(
        \\**Does Not Exist**
        \\`Color.Shade.missing` does not exist.
        \\`Color.Shade` is in scope, but it has no associated `missing`.
        \\
        \\```roc
        \\missing = Color.Shade.missing()
        \\```
        \\          ^^^^^^^^^^^^^^^^^^^
        \\
        \\
        \\
    );
}

test "issue 11943: a nested builtin type takes precedence over a tag of the same name" {
    const source =
        \\missing = Str.Utf8Problem.missing()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneCanErrorMsg(
        \\**Does Not Exist**
        \\`Str.Utf8Problem.missing` does not exist.
        \\`Str.Utf8Problem` is in scope, but it has no associated `missing`.
        \\
        \\```roc
        \\missing = Str.Utf8Problem.missing()
        \\```
        \\          ^^^^^^^^^^^^^^^^^^^^^^^
        \\
        \\
        \\
    );
}
