//! Regression tests for https://github.com/roc-lang/roc/issues/11621.
//!
//! An unannotated recursive function whose body uses `?` must check cleanly:
//! a recursive reference's result shares the function's own error row, so the
//! inferred type is an ordinary non-recursive function type. This holds for a
//! top-level function whether or not an unannotated function calls it, for a
//! local recursive closure, and across a mutually recursive group, whose
//! members share one error row.

const TestEnv = @import("./TestEnv.zig");

test "issue 11621 - unannotated recursive function using ? called from an unannotated wrapper" {
    const src =
        \\run = |{}| collect(0, [])
        \\
        \\step : U64 -> Try([More, Done], [StepFailed])
        \\step = |_n| Ok(Done)
        \\
        \\collect = |n, out|
        \\    match step(n)? {
        \\        Done => Ok(out)
        \\        More => collect(n, out.append(n))
        \\    }
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("collect", "U64, ok -> Try(ok, [StepFailed]) where [ok.append : ok, U64 -> ok]");
}

test "issue 11621 - unannotated recursive function using ? with no caller" {
    const src =
        \\step : U64 -> Try([More, Done], [StepFailed])
        \\step = |_n| Ok(Done)
        \\
        \\collect = |n, out|
        \\    match step(n)? {
        \\        Done => Ok(out)
        \\        More => collect(n, out.append(n))
        \\    }
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("collect", "U64, ok -> Try(ok, [StepFailed]) where [ok.append : ok, U64 -> ok]");
}

test "issue 11621 - local recursive closure using ? called through an unannotated wrapper" {
    const src =
        \\step : U64 -> Try([More, Done], [StepFailed])
        \\step = |_n| Ok(Done)
        \\
        \\collect = |n| {
        \\    helper = |out|
        \\        match step(n)? {
        \\            Done => Ok(out)
        \\            More => helper(out.append(n))
        \\        }
        \\    helper([])
        \\}
        \\
        \\run = |{}| collect(0)
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("collect", "U64 -> Try(List(U64), [StepFailed])");
}

test "issue 11621 - mutually recursive functions using ?" {
    const src =
        \\step : U64 -> Try([More, Done], [StepFailed])
        \\step = |_n| Ok(Done)
        \\
        \\stop : U64 -> Try([More, Done], [StopFailed])
        \\stop = |_n| Ok(Done)
        \\
        \\ping = |n, out|
        \\    match step(n)? {
        \\        Done => Ok(out)
        \\        More => pong(n, out.append(n))
        \\    }
        \\
        \\pong = |n, out|
        \\    match stop(n)? {
        \\        Done => Ok(out)
        \\        More => ping(n, out.append(n))
        \\    }
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("ping", "U64, ok -> Try(ok, [StepFailed, StopFailed]) where [ok.append : ok, U64 -> ok]");
    try test_env.assertDefType("pong", "U64, ok -> Try(ok, [StepFailed, StopFailed]) where [ok.append : ok, U64 -> ok]");
}

test "issue 11621 - recursion through a generalized higher-order helper" {
    // `apply`'s result row is `[StepFailed, ..e]`; passing `rec` makes `e`
    // the row that extends it, which includes its own tags.
    const src =
        \\step : U64 -> Try(U64, [StepFailed])
        \\step = |n| if n > 3 { Err(StepFailed) } else { Ok(n) }
        \\
        \\apply = |f, n| {
        \\    _ = step(n)?
        \\    f(n + 1)
        \\}
        \\
        \\rec = |n| apply(rec, n)
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("rec", "U64 -> Try(ok, [StepFailed])");
}

test "issue 11621 - a callback failing with the same tag as the helper's own ?" {
    const src =
        \\step : U64 -> Try(U64, [StepFailed])
        \\step = |n| if n > 3 { Err(StepFailed) } else { Ok(n) }
        \\
        \\apply = |f, n| {
        \\    _ = step(n)?
        \\    f(n + 1)
        \\}
        \\
        \\direct = |n| apply(step, n)
        \\
        \\through_lambda = |n| apply(|m| {
        \\    _ = step(m)?
        \\    Ok(m)
        \\}, n)
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("direct", "U64 -> Try(U64, [StepFailed])");
    try test_env.assertDefType("through_lambda", "U64 -> Try(U64, [StepFailed])");
}

test "issue 11621 - a repeated tag with an incompatible payload is rejected" {
    const src =
        \\step : U64 -> Try(U64, [StepFailed])
        \\step = |n| if n > 3 { Err(StepFailed) } else { Ok(n) }
        \\
        \\describe : U64 -> Try(U64, [StepFailed(Str)])
        \\describe = |n| if n > 3 { Err(StepFailed("too big")) } else { Ok(n) }
        \\
        \\apply = |f, n| {
        \\    _ = step(n)?
        \\    f(n + 1)
        \\}
        \\
        \\use = |n| apply(describe, n)
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Conflicting Tag");
}

test "issue 11621 - a method recursing through dispatch on its own receiver" {
    // `go`'s requirement resolves to `go` itself once the receiver is
    // `Counter`; that recursion is rejected as a diagnostic.
    const src =
        \\Counter := [C(U64)].{
        \\    go = |c, n| {
        \\        _ = helper(n)?
        \\        c.go(n)
        \\    }
        \\}
        \\
        \\helper : U64 -> Try(U64, [StepFailed])
        \\helper = |n| if n > 3 { Err(StepFailed) } else { Ok(n) }
        \\
        \\probe = |n| {
        \\    _ = Counter.go(Counter.C(n), n)?
        \\    Ok(n)
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Recursive Dispatch");
}

test "issue 11621 - a transaction helper called from a recursive caller" {
    // Issue 11469's `transaction` keeps its callback row out of the wrapped
    // tag; a caller that recurses through its callback includes its own row.
    const src =
        \\transaction = |execute, operation| {
        \\    _ = execute("BEGIN") ? BeginFailed
        \\    operation({})
        \\}
        \\
        \\exec : Str -> Try({}, [DbErr(Str)])
        \\exec = |sql| Err(DbErr(sql))
        \\
        \\retry = |n| transaction(exec, |{}| if n == 0.U64 { Ok({}) } else { retry(n - 1) })
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("retry", "U64 -> Try({}, [BeginFailed([DbErr(Str)])])");
}
