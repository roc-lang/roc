//! Regression tests for https://github.com/roc-lang/roc/issues/11640.
//!
//! A lambda that uses `?` and whose body ends in a tail call to itself (or to
//! a member of its own recursive binding group) returns that recursive call's
//! `Try` as its body result. That body's error row is the lambda's own result
//! row, so including it in the composed `Try` result adds nothing: these
//! definitions check cleanly and infer the same type as the equivalent
//! definition that propagates the recursive call's result with `?` and
//! rewraps it in `Ok`. The same holds when the recursive result is reached
//! from a nested closure, re-wrapped by a match, or reached through a method
//! whose target is checked only at the caller's boundary.

const TestEnv = @import("./TestEnv.zig");

test "issue 11640 - local recursive closure tail-calls itself after propagating a captured Try" {
    // The shape of basic-cli's `Sqlite.decode_rows!`: a local recursive
    // helper that propagates a captured value's error with `?` and ends in a
    // tail call to itself.
    const src =
        \\f = |r| {
        \\    helper = |_| {
        \\        _a = Err(Bad)?
        \\        _b = r?
        \\        helper({})
        \\    }
        \\    helper({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("f", "Try(ok, [Bad, ..a]) -> Try(ok, [Bad, ..a])");
}

test "issue 11640 - top-level function tail-calls itself after a ? return" {
    const src =
        \\helper = |_| {
        \\    _a = Err(Bad)?
        \\    helper({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("helper", "{} -> Try(ok, [Bad])");
}

test "issue 11640 - mutually recursive functions tail-call each other after ? returns" {
    const src =
        \\f = |n| {
        \\    _a = Err(Bad)?
        \\    g(n)
        \\}
        \\
        \\g = |n| {
        \\    _b = Err(Other)?
        \\    f(n)
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("f", "_arg -> Try(ok, [Bad, Other])");
    try test_env.assertDefType("g", "_arg -> Try(ok, [Bad, Other])");
}

test "issue 11640 - local closure tail-calls its enclosing function after a ? return" {
    const src =
        \\f = |n| {
        \\    h = |x| {
        \\        _a = Err(A)?
        \\        f(x)
        \\    }
        \\    h(n)
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("f", "_arg -> Try(ok, [A])");
}

test "issue 11640 - closure tail-calling its enclosing function adds nothing to that function's errors" {
    // The closure's result includes `f`'s errors, but `f` never returns the
    // closure's result, so `A` is not one of `f`'s errors.
    const src =
        \\f = |n| {
        \\    _ = List.map([n], |x| {
        \\        _a = Err(A)?
        \\        f(x)
        \\    })
        \\    _b = Err(B)?
        \\    Ok(n)
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("f", "ok -> Try(ok, [B])");
}

test "issue 11640 - body re-wraps the error of a recursive call after a ? return" {
    const src =
        \\f = |n| {
        \\    _a = Err(A)?
        \\    match f(n) {
        \\        Ok(v) => Ok(v)
        \\        Err(e) => Err(e)
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("f", "_arg -> Try(ok, [A])");
}

test "issue 11640 - ? return then a tail method call whose target is checked at the caller's boundary" {
    // The shape of basic-cli's `Sqlite.query_many!`: the method's target is
    // checked only when the caller's boundary resolves the dispatch, and its
    // result already carries the tag the caller's own `?` propagates.
    const src =
        \\Stmt := { host : U64 }.{
        \\    query_many! = |stmt, decode| {
        \\        bind!(stmt.host)?
        \\        res = decode_rows!(stmt.host, decode)
        \\        bind!(stmt.host)?
        \\        res
        \\    }
        \\}
        \\
        \\prepare! : Str => Try(Stmt, [SqliteErr(Str)])
        \\prepare! = |path| host_prepare!(path).map_ok(|stmt| Stmt.{ host: stmt })
        \\
        \\query_many! = |path, rows| {
        \\    stmt = prepare!(path)?
        \\    stmt.query_many!(rows)
        \\}
        \\
        \\host_prepare! : Str => Try(U64, [SqliteErr(Str)])
        \\host_prepare! = |_| Ok(0)
        \\
        \\bind! : U64 => Try({}, [SqliteErr(Str)])
        \\bind! = |_| Ok({})
        \\
        \\step! : U64 => Try([Row, Done], [SqliteErr(Str)])
        \\step! = |_| Ok(Done)
        \\
        \\decode_rows! = |stmt, gen_decode| {
        \\    decode_row! = gen_decode(stmt)
        \\    helper! = |out|
        \\        match step!(stmt)? {
        \\            Done => Ok(out)
        \\            Row => {
        \\                row = decode_row!(stmt)?
        \\                helper!(out.append(row))
        \\            }
        \\        }
        \\    helper!([])
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("query_many!", "Str, (U64 -> (U64 -> Try(a, [SqliteErr(Str), ..c]))) => Try(List(a), [SqliteErr(Str), ..c])");
}

test "issue 11640 - an infinite type reaching an enclosing function's parameter is reported" {
    // Generalizing `h` meets a cyclic type that also reaches `f`'s parameter.
    // The cycle is reported by the settled-state occurs check.
    const src =
        \\f = |r| {
        \\    h = |x| {
        \\        _ = [x, Bar(x)]
        \\        _ = [r, Foo(x)]
        \\        x
        \\    }
        \\    h
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("Anonymous Recursion");
}
