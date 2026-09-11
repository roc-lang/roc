//! Regression tests for https://github.com/roc-lang/roc/issues/11248.
//! A `?` returns an `Err` the body tail never mentions, so the tags a lambda
//! constructs cannot be read off that tail alone: the payload of an inferred
//! `Err` row must stay open for the `?` to widen.

const TestEnv = @import("./TestEnv.zig");

test "issue 11248 `?` before an all-Ok if keeps the inferred error row open" {
    const src =
        \\parse : Str -> Try(Str, [Empty])
        \\parse = |s| if s == "" { Err(Empty) } else { Ok(s) }
        \\
        \\with_suffix : Str, Bool -> Try(Str, _)
        \\with_suffix = |s, add| {
        \\    text = parse(s)?
        \\    if add { Ok(text) } else { Ok("other") }
        \\}
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertDefType("with_suffix", "Str, Bool -> Try(Str, [Empty])");
}

test "issue 11248 `?` before an all-Ok match keeps the inferred error row open" {
    const src =
        \\parse : Str -> Try(Str, [Empty])
        \\parse = |s| if s == "" { Err(Empty) } else { Ok(s) }
        \\
        \\with_suffix : Str, Bool -> Try(Str, _)
        \\with_suffix = |s, add| {
        \\    text = parse(s)?
        \\    match add {
        \\        True => Ok(text)
        \\        False => Ok("other")
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertDefType("with_suffix", "Str, Bool -> Try(Str, [Empty])");
}

test "issue 11248 a `?` inside one branch keeps the inferred error row open" {
    const src =
        \\parse : Str -> Try(Str, [Empty])
        \\parse = |s| if s == "" { Err(Empty) } else { Ok(s) }
        \\
        \\with_suffix : Str, Bool -> Try(Str, _)
        \\with_suffix = |s, add| {
        \\    if add {
        \\        Ok(s)
        \\    } else {
        \\        text = parse(s)?
        \\        Ok(text)
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertDefType("with_suffix", "Str, Bool -> Try(Str, [Empty])");
}

test "issue 11248 a wrapped `?` before an all-Ok if keeps the inferred error row open" {
    const src =
        \\parse : Str -> Try(Str, [Empty])
        \\parse = |s| if s == "" { Err(Empty) } else { Ok(s) }
        \\
        \\with_suffix : Str, Bool -> Try(Str, _)
        \\with_suffix = |s, add| {
        \\    text = parse(s) ? |e| Wrapped(e)
        \\    if add { Ok(text) } else { Ok("other") }
        \\}
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertDefType("with_suffix", "Str, Bool -> Try(Str, [Wrapped([Empty]), ..])");
}

test "issue 11248 an explicit early return keeps the inferred error row open" {
    const src =
        \\with_suffix : Str, Bool -> Try(Str, _)
        \\with_suffix = |s, add| {
        \\    if s == "" {
        \\        return Err(Empty)
        \\    } else {}
        \\
        \\    if add { Ok(s) } else { Ok("other") }
        \\}
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertDefType("with_suffix", "Str, Bool -> Try(Str, [Empty, ..])");
}

test "issue 11248 an all-Ok body with no early return still closes the error payload" {
    const src =
        \\only_ok : Str, Bool -> Try(Str, _)
        \\only_ok = |s, add| if add { Ok(s) } else { Ok("other") }
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertDefType("only_ok", "Str, Bool -> Try(Str, [])");
}
