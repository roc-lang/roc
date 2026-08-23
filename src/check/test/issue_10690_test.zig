//! Regression test for issue 10690.

const TestEnv = @import("./TestEnv.zig");

test "issue 10690: match branch mismatch through wrapped try reports a type mismatch" {
    const source =
        \\wrapped : (Str -> Try(Str, [FetchFailed(Str), ..])), Str -> Try(Str, _)
        \\wrapped = |fetch, key| {
        \\    v = fetch(key) ? Wrap
        \\    Ok(v)
        \\}
        \\
        \\dispatch : (Str -> Try(Str, [FetchFailed(Str), ..])), Str -> Try(Str, _)
        \\dispatch = |fetch, path|
        \\    match path {
        \\        "a" => fetch("x")
        \\        _ => wrapped(fetch, "y")
        \\    }
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("Type Mismatch");
}
