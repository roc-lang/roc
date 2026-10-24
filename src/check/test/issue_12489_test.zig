//! Regression tests for https://github.com/roc-lang/roc/issues/12489.
//!
//! A tag union alias with a row variable closed with `[]` (`Ab([])` for
//! `Ab(r) : [A, B, ..r]`) is the closed union `[A, B]`, so it has a derived
//! `encoder_for` and `parser_for` when it appears in a function signature,
//! exactly like the written-out `[A, B]`.
const TestEnv = @import("TestEnv.zig");

test "issue 12489: row alias closed with [] in a function argument has encoder_for" {
    const source =
        \\Ab(r) : [A, B, ..r]
        \\
        \\enc : Ab([]) -> Str
        \\enc = |x| Json.to_str(x)
        \\
        \\out = enc(A)
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12489: row alias closed with [] with a payload in a function argument has encoder_for" {
    const source =
        \\Ab(r) : [A(Str), B, ..r]
        \\
        \\enc : Ab([]) -> Str
        \\enc = |x| Json.to_str(x)
        \\
        \\out = enc(A("a"))
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12489: row alias closed with [] in a function return has parser_for" {
    const source =
        \\Ab(r) : [A, B, ..r]
        \\
        \\dec : Str -> Try(Ab([]), _)
        \\dec = |s| Json.parse(s)
        \\
        \\out = dec("\"A\"")
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12489: row alias closed through a nested tagless row alias has encoder_for" {
    const source =
        \\Ab(r) : [A, B, ..r]
        \\Rest(r) : [..r]
        \\
        \\enc : Ab(Rest([])) -> Str
        \\enc = |x| Json.to_str(x)
        \\
        \\out = enc(B)
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12489: tagless row alias closed with [] has no encoder_for" {
    const source =
        \\Rest(r) : [..r]
        \\
        \\enc : Rest([]) -> Str
        \\enc = |x| Json.to_str(x)
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}
