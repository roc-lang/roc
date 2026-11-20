//! Regression test for issue 10690.

const TestEnv = @import("./TestEnv.zig");

test "issue 10690: a closed try beside a wrapped try is a branch mismatch" {
    // `fetch("x")` has the callback's closed error row, and a `Try` is an
    // ordinary nominal type whose arguments no branch join widens, so it does
    // not meet `wrapped(..)`'s open row.
    const source =
        \\wrapped : (Str -> Try(Str, [FetchFailed(Str)])), Str -> Try(Str, _)
        \\wrapped = |fetch, key| {
        \\    v = fetch(key) ? Wrap
        \\    Ok(v)
        \\}
        \\
        \\dispatch : (Str -> Try(Str, [FetchFailed(Str)])), Str -> Try(Str, _)
        \\dispatch = |fetch, path|
        \\    match path {
        \\        "a" => fetch("x")
        \\        _ => wrapped(fetch, "y")
        \\    }
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Type Mismatch");
}

test "issue 10690: a closed try rebuilt through `?` composes with a wrapped try" {
    // `Ok(fetch("x")?)` composes the callback's closed error row into the
    // annotated result (design.md "Try Return-Row Composition"), so the
    // result holds both branches' errors.
    const source =
        \\wrapped : (Str -> Try(Str, [FetchFailed(Str)])), Str -> Try(Str, _)
        \\wrapped = |fetch, key| {
        \\    v = fetch(key) ? Wrap
        \\    Ok(v)
        \\}
        \\
        \\dispatch : (Str -> Try(Str, [FetchFailed(Str)])), Str -> Try(Str, _)
        \\dispatch = |fetch, path|
        \\    match path {
        \\        "a" => Ok(fetch("x")?)
        \\        _ => wrapped(fetch, "y")
        \\    }
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("dispatch", "(Str -> Try(Str, [FetchFailed(Str)])), Str -> Try(Str, [FetchFailed(Str), Wrap([FetchFailed(Str)])])");
}
