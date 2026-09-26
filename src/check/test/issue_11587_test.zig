//! Regression tests for https://github.com/roc-lang/roc/issues/11587.
//!
//! A method of a parameterized nominal type that is dispatched with method
//! syntax on a `Client(_)` receiver from a sibling method, and whose body calls
//! a top-level function declared *below* the type, must check cleanly. The
//! annotated method's declared type is the type every use sees; a use must
//! never report a mismatch between the annotation and itself.

const TestEnv = @import("./TestEnv.zig");

test "issue 11587 - effectful method dispatched on Client(_) receiver calling a later top-level function" {
    const src =
        \\Client(effects) := { effects : effects }.{
        \\    command! : Client(_), Str => Try(Str, [Refused(Str), ..others])
        \\    command! = |_client, sql| {
        \\        say!(sql)
        \\        Ok("done")
        \\    }
        \\
        \\    query! : Client(_), Str => Try(Str, [Refused(Str), ..others])
        \\    query! = |client, sql| client.command!(sql)
        \\}
        \\
        \\say! : Str => {}
        \\say! = |_text| {}
        \\
        \\run! : Str => Try(Str, [Refused(Str)])
        \\run! = |sql| {
        \\    client = Client.{ effects: {} }
        \\    done = Client.query!(client, sql)?
        \\    Ok(done)
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("run!", "Str => Try(Str, [Refused(Str)])");
}

test "issue 11587 - pure method dispatched on Client(_) receiver calling a later top-level function" {
    const src =
        \\Client(effects) := { effects : effects }.{
        \\    command : Client(_), Str -> Try(Str, [Refused(Str), ..others])
        \\    command = |_client, sql| {
        \\        _ = say(sql)
        \\        Ok("done")
        \\    }
        \\
        \\    query : Client(_), Str -> Try(Str, [Refused(Str), ..others])
        \\    query = |client, sql| client.command(sql)
        \\}
        \\
        \\say : Str -> Str
        \\say = |text| text
        \\
        \\run : Str -> Try(Str, [Refused(Str)])
        \\run = |sql| {
        \\    client = Client.{ effects: {} }
        \\    done = Client.query(client, sql)?
        \\    Ok(done)
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("run", "Str -> Try(Str, [Refused(Str)])");
}

test "issue 11587 - mutually recursive methods dispatched on Client(_) receiver calling a later top-level function" {
    // `command` and `retry` form one recursive binding group that is checked
    // nested inside `query`'s generalization boundary. While that group is
    // still open, `query`'s obligation on `command` must keep waiting for the
    // group's scheme instead of merging with the not-yet-generalized member.
    const src =
        \\Client(effects) := { effects : effects }.{
        \\    command = |client, sql, attempts| {
        \\        _ = say(sql)
        \\        if attempts == 0 Ok("done") else retry(client, sql, attempts - 1)
        \\    }
        \\
        \\    retry = |client, sql, attempts| command(client, sql, attempts)
        \\
        \\    query : Client(_), Str -> Try(Str, [Refused(Str), ..others])
        \\    query = |client, sql| client.command(sql, 1)
        \\}
        \\
        \\say : Str -> Str
        \\say = |text| text
        \\
        \\run : Str -> Try(Str, [Refused(Str)])
        \\run = |sql| {
        \\    client = Client.{ effects: {} }
        \\    done = Client.query(client, sql)?
        \\    Ok(done)
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("run", "Str -> Try(Str, [Refused(Str)])");
}

test "issue 11587 - value-typed method dispatched on Client(_) receiver built from a later top-level function" {
    // `command` is a value definition (its right-hand side is a call, not a
    // lambda) whose group is checked nested inside `query`'s boundary. An
    // in-flight value target reached from an enclosing frame's obligation is
    // not a recursive value cycle; the obligation waits for the finished def.
    const src =
        \\Client(effects) := { effects : effects }.{
        \\    command = make_command("done")
        \\
        \\    query : Client(_), Str -> Try(Str, [Refused(Str), ..others])
        \\    query = |client, sql| client.command(sql)
        \\}
        \\
        \\make_command : Str -> (Client(_), Str -> Try(Str, [Refused(Str), ..others]))
        \\make_command = |reply| |_client, _sql| Ok(reply)
        \\
        \\run : Str -> Try(Str, [Refused(Str)])
        \\run = |sql| {
        \\    client = Client.{ effects: {} }
        \\    done = Client.query(client, sql)?
        \\    Ok(done)
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("run", "Str -> Try(Str, [Refused(Str)])");
}

test "issue 11587 - control: same program with the helper declared above the type" {
    const src =
        \\say : Str -> Str
        \\say = |text| text
        \\
        \\Client(effects) := { effects : effects }.{
        \\    command : Client(_), Str -> Try(Str, [Refused(Str), ..others])
        \\    command = |_client, sql| {
        \\        _ = say(sql)
        \\        Ok("done")
        \\    }
        \\
        \\    query : Client(_), Str -> Try(Str, [Refused(Str), ..others])
        \\    query = |client, sql| client.command(sql)
        \\}
        \\
        \\run : Str -> Try(Str, [Refused(Str)])
        \\run = |sql| {
        \\    client = Client.{ effects: {} }
        \\    done = Client.query(client, sql)?
        \\    Ok(done)
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("run", "Str -> Try(Str, [Refused(Str)])");
}
