//! Regression tests for https://github.com/roc-lang/roc/issues/11624.
//!
//! A method whose annotation has a `_` hole, that dispatches with method
//! syntax to a sibling method calling a top-level function declared *below*
//! the type, must be generalized before its uses. Every call of it
//! instantiates its named row variable (`..others`) freshly, so two calls can
//! flow into two different rows of the caller's own annotation.

const TestEnv = @import("./TestEnv.zig");

test "issue 11624 - two calls of a hole-annotated method each instantiate its row variable" {
    const src =
        \\Client := { id : U64 }.{
        \\    command! : Client, Str => Try(Str, [Refused(Str), ReadErr(_), ..others])
        \\    command! = |_client, sql| {
        \\        say!(sql)
        \\        Ok(sql)
        \\    }
        \\
        \\    execute! : Client, Str => Try({}, [Refused(Str), ReadErr(_), ..others])
        \\    execute! = |client, sql| {
        \\        _ = client.command!(sql)?
        \\        Ok({})
        \\    }
        \\
        \\    begin_and_commit! : Client => Try({}, [BeginFailed([Refused(Str), ReadErr(_), ..begin_others]), CommitFailed([Refused(Str), ReadErr(_), ..commit_others]), ..others])
        \\    begin_and_commit! = |client|
        \\        match client.execute!("begin") {
        \\            Err(err) => Err(BeginFailed(err))
        \\            Ok({}) =>
        \\                match client.execute!("commit") {
        \\                    Ok({}) => Ok({})
        \\                    Err(err) => Err(CommitFailed(err))
        \\                }
        \\        }
        \\}
        \\
        \\say! : Str => {}
        \\say! = |_text| {}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "issue 11624 - control: same program with the helper declared above the type" {
    const src =
        \\say! : Str => {}
        \\say! = |_text| {}
        \\
        \\Client := { id : U64 }.{
        \\    command! : Client, Str => Try(Str, [Refused(Str), ReadErr(_), ..others])
        \\    command! = |_client, sql| {
        \\        say!(sql)
        \\        Ok(sql)
        \\    }
        \\
        \\    execute! : Client, Str => Try({}, [Refused(Str), ReadErr(_), ..others])
        \\    execute! = |client, sql| {
        \\        _ = client.command!(sql)?
        \\        Ok({})
        \\    }
        \\
        \\    begin_and_commit! : Client => Try({}, [BeginFailed([Refused(Str), ReadErr(_), ..begin_others]), CommitFailed([Refused(Str), ReadErr(_), ..commit_others]), ..others])
        \\    begin_and_commit! = |client|
        \\        match client.execute!("begin") {
        \\            Err(err) => Err(BeginFailed(err))
        \\            Ok({}) =>
        \\                match client.execute!("commit") {
        \\                    Ok({}) => Ok({})
        \\                    Err(err) => Err(CommitFailed(err))
        \\                }
        \\        }
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}
