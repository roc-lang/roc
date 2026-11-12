//! Regression coverage for equality on a tag carried by a composed error row.
//! https://github.com/roc-lang/roc/issues/12883
const TestEnv = @import("TestEnv.zig");

test "issue 12883: equality stays concrete when a record function contributes an open error row" {
    var env = try TestEnv.init("Test",
        \\log = {
        \\    info!: |_message| Ok({}),
        \\}
        \\
        \\main! : _ => Try({}, _)
        \\main! = |args| {
        \\    info! = log.info!
        \\    info!("starting")?
        \\    sold = if args.len() > 1 Present else Missing
        \\    (if sold == Present Ok({}) else Err(NotPresent)) ? |_| Unsold(sold)
        \\    Ok({})
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12883: value-row finalization leaves generalized equality rows polymorphic" {
    var env = try TestEnv.init("Test",
        \\same = |a, b| a == b
        \\first = same(A, B)
        \\second = same(C("c"), D)
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("first", "Bool");
    try env.assertDefType("second", "Bool");
}

test "issue 12883: a stored function's explicit closed error row checks cleanly" {
    var env = try TestEnv.init("Test",
        \\log : { info! : Str -> Try({}, []) }
        \\log = { info!: |_message| Ok({}) }
        \\
        \\main! : List(Str) => Try({}, _)
        \\main! = |args| {
        \\    info! = log.info!
        \\    info!("starting")?
        \\    sold = if args.len() > 1 Present else Missing
        \\    (if sold == Present Ok({}) else Err(NotPresent)) ? |_| Unsold(sold)
        \\    Ok({})
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}
