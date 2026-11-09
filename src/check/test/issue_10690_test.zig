//! Regression test for issue 10690.

const TestEnv = @import("./TestEnv.zig");

test "issue 10690: match branches returning a closed try and a wrapped try compose into the annotated result" {
    // `fetch("x")` has the callback's closed error row and `wrapped(..)` an
    // open one; each flows into `dispatch`'s annotated result through a
    // widening copy (design.md "Row Subsumption"), so the result holds both.
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

    try test_env.assertNoErrors();
    try test_env.assertDefType("dispatch", "(Str -> Try(Str, [FetchFailed(Str)])), Str -> Try(Str, [FetchFailed(Str), Wrap([FetchFailed(Str)])])");
}
