//! Regression tests for https://github.com/roc-lang/roc/issues/11622.
//!
//! A method whose body uses `?` on a function declared *below* the type, and
//! which is dispatched with method syntax after a `?` whose error union shares
//! a tag with the method's error union, must check cleanly. The caller's error
//! row is the union of both `?` contributions, never the closed empty union.

const TestEnv = @import("./TestEnv.zig");

test "issue 11622 - method using ? on a later function, dispatched after ? with a shared error tag" {
    const src =
        \\Stmt :: { host : U64 }.{
        \\    run = |stmt|
        \\        match step(stmt.host)? {
        \\            Done => Ok(stmt.host)
        \\            Row => Err(TooManyRows)
        \\        }
        \\}
        \\
        \\prepare : Str -> Try(Stmt, [DbErr(Str)])
        \\prepare = |_sql| Ok(Stmt.{ host: 0 })
        \\
        \\query = |sql| {
        \\    stmt = prepare(sql)?
        \\    stmt.run()
        \\}
        \\
        \\step : U64 -> Try([Row, Done], [DbErr(Str)])
        \\step = |_host| Ok(Done)
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("query", "Str -> Try(U64, [DbErr(Str), TooManyRows])");
}
