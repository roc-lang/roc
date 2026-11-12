//! Regression tests for https://github.com/roc-lang/roc/issues/12951.
//!
//! A lambda checked before its parameter's type is known can call a method on
//! that parameter and propagate the call's error with `?`. The method's error
//! row is unknown until the receiver resolves, so the lambda's `?` error rows
//! compose only once it is known: the method may fail with the very tags
//! another `?` in the same lambda contributes.

const TestEnv = @import("./TestEnv.zig");

const prelude =
    \\Repro := [].{}
    \\
    \\Source := { n : U64 }.{
    \\    read : Source -> Try(U64, [Bad])
    \\    read = |s| Ok(s.n)
    \\}
    \\
    \\run : (input -> output) -> (input -> output)
    \\run = |f| f
    \\
    \\check : U64 -> Try(U64, [Bad])
    \\check = |n| Ok(n)
    \\
;

test "issue 12951 - method call with ? in a lambda passed to a generic function" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a : Source -> Try(U64, [Bad])
        \\a = run(|source| {
        \\    m = source.read()?
        \\    k = check(m)?
        \\    Ok(k)
        \\})
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a", "Source -> Try(U64, [Bad])");
}

test "issue 12951 - the same body as an annotated definition passed by name" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\b : Source -> Try(U64, [Bad])
        \\b = run(b_body)
        \\
        \\b_body : Source -> Try(U64, [Bad])
        \\b_body = |source| {
        \\    m = source.read()?
        \\    k = check(m)?
        \\    Ok(k)
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("b", "Source -> Try(U64, [Bad])");
}

test "issue 12951 - the method's ? after another ? with the same tag" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a : Source -> Try(U64, [Bad])
        \\a = run(|source| {
        \\    k = check(1)?
        \\    m = source.read()?
        \\    Ok(k + m)
        \\})
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a", "Source -> Try(U64, [Bad])");
}

test "issue 12951 - builtin method call with ? on a list parameter" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\non_zero : U64 -> Try(U64, [ListWasEmpty])
        \\non_zero = |n| if n == 0 Err(ListWasEmpty) else Ok(n)
        \\
        \\a : List(U64) -> Try(U64, [ListWasEmpty])
        \\a = run(|xs| {
        \\    m = xs.first()?
        \\    k = non_zero(m)?
        \\    Ok(k)
        \\})
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a", "List(U64) -> Try(U64, [ListWasEmpty])");
}

test "issue 12951 - nested unannotated helper calls the method on a captured receiver" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a : Source -> Try(U64, [Bad])
        \\a = run(|source| {
        \\    helper = |{}| {
        \\        m = source.read()?
        \\        k = check(m)?
        \\        Ok(k)
        \\    }
        \\    helper({})
        \\})
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a", "Source -> Try(U64, [Bad])");
}

test "issue 12951 - nested helper's receiver outlives the helper" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a : Source -> Try(U64, [Bad])
        \\a = run(|source| {
        \\    helper = |s| {
        \\        m = s.read()?
        \\        k = check(m)?
        \\        _same = [s, source]
        \\        Ok(k)
        \\    }
        \\    helper(source)
        \\})
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a", "Source -> Try(U64, [Bad])");
}

test "issue 12951 - a local helper stays polymorphic in its receiver's method errors" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\Other := { n : U64 }.{
        \\    read : Other -> Try(U64, [Oops])
        \\    read = |s| Ok(s.n)
        \\}
        \\
        \\g : Source, Other -> (Try(U64, [Bad]), Try(U64, [Oops]))
        \\g = |source, other| {
        \\    get = |s| {
        \\        v = s.read()?
        \\        Ok(v * 2)
        \\    }
        \\    x : Try(U64, [Bad])
        \\    x = get(source)
        \\    y : Try(U64, [Oops])
        \\    y = get(other)
        \\    (x, y)
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 12951 - receiver type arrives through a later annotated binding" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a : {} -> Try(U64, [Bad])
        \\a = |{}| {
        \\    x = run(|source| {
        \\        m = source.read()?
        \\        k = check(m)?
        \\        Ok(k)
        \\    })
        \\    y : Source -> Try(U64, [Bad])
        \\    y = x
        \\    y({ n: 1 })
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a", "{} -> Try(U64, [Bad])");
}

test "issue 12951 - effectful method call with ? in a lambda passed to a generic function" {
    var test_env = try TestEnv.init("Repro",
        \\Repro := [].{}
        \\
        \\Source := { n : U64 }.{
        \\    read! : Source => Try(U64, [Bad])
        \\    read! = |s| {
        \\        dbg s.n
        \\        Ok(s.n)
        \\    }
        \\}
        \\
        \\run! : (input => output) -> (input => output)
        \\run! = |f!| f!
        \\
        \\check : U64 -> Try(U64, [Bad])
        \\check = |n| Ok(n)
        \\
        \\a! : Source => Try(U64, [Bad])
        \\a! = run!(|source| {
        \\    m = source.read!()?
        \\    k = check(m)?
        \\    Ok(k)
        \\})
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a!", "Source => Try(U64, [Bad])");
}

test "issue 12951 - hosted effect called as a method with ? in a lambda passed to a generic function" {
    var test_env = try TestEnv.initHosted("File",
        \\File := { fd : U64 }.{
        \\    read! : File, { len : U64 } => Try(List(U8), [Bad])
        \\}
        \\
        \\run! : (input => output) -> (input => output)
        \\run! = |f!| f!
        \\
        \\check : List(U8) -> Try(U64, [Bad])
        \\check = |bytes| Ok(List.len(bytes))
        \\
        \\a! : File => Try(U64, [Bad])
        \\a! = run!(|file| {
        \\    bytes = file.read!({ len: 4 })?
        \\    k = check(bytes)?
        \\    Ok(k)
        \\})
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a!", "File => Try(U64, [Bad])");
}

test "issue 12951 - a method error missing from the annotation is still rejected" {
    var test_env = try TestEnv.init("Repro",
        \\Repro := [].{}
        \\
        \\Source := { n : U64 }.{
        \\    read : Source -> Try(U64, [Missing])
        \\    read = |s| Ok(s.n)
        \\}
        \\
        \\run : (input -> output) -> (input -> output)
        \\run = |f| f
        \\
        \\check : U64 -> Try(U64, [Bad])
        \\check = |n| Ok(n)
        \\
        \\a : Source -> Try(U64, [Bad])
        \\a = run(|source| {
        \\    m = source.read()?
        \\    k = check(m)?
        \\    Ok(k)
        \\})
    );
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "Missing");
}
