//! repro for https://github.com/roc-lang/roc/issues/12794
//! A `?` whose error is a nominal tag union contributes that whole nominal to
//! an unannotated lambda's result, like any source with tags. Only an open row
//! variable is a tagless contribution whose row becomes the result's residual
//! tail; a nominal has no residual extension, so a bare tag returned beside it
//! joins the nominal.
const TestEnv = @import("TestEnv.zig");

const check_def =
    \\Error := [Unsupported(Str), Other(Str)]
    \\
    \\check : Str -> Try(Str, Error)
    \\check = |s| if s == "" Err(Error.Other("empty")) else Ok(s)
    \\
    \\
;

test "issue 12794: a bare tag joins the nominal error of a ?" {
    var env = try TestEnv.init("Test", check_def ++
        \\f = |x| {
        \\    if x == "" return Err(Unsupported("empty"))
        \\    _ = check(x)?
        \\    Ok(x)
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("f", "Str -> Try(Str, Error)");
}

test "issue 12794: a bare tag joins the nominal error of a bound ? in a local lambda" {
    var env = try TestEnv.init("Test", check_def ++
        \\run : Str -> Try(Str, Error)
        \\run = |s| {
        \\    f = |x| {
        \\        if x == "" return Err(Unsupported("empty"))
        \\        v = check(x)?
        \\        Ok(v)
        \\    }
        \\    f(s)
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12794: a body whose error is a nominal joins a returned bare tag" {
    var env = try TestEnv.init("Test", check_def ++
        \\f = |x| {
        \\    if x == "" return Err(Unsupported("empty"))
        \\    _ = check(x)?
        \\    check(x)
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("f", "Str -> Try(Str, Error)");
}

test "issue 12794: a bare tag the nominal does not have is a mismatch" {
    var env = try TestEnv.init("Test", check_def ++
        \\f = |x| {
        \\    if x == "" return Err(Missing)
        \\    _ = check(x)?
        \\    Ok(x)
        \\}
    );
    defer env.deinit();
    try env.assertFirstTypeError("Type Mismatch");
}
