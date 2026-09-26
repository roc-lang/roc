//! Tests for design.md "Row Union Normalization": a label repeated along one
//! row's extension chain names one tag, so compatible occurrences merge and
//! only conflicting ones are rejected.

const TestEnv = @import("./TestEnv.zig");

test "row union - a callback may raise the tag its wrapper adds" {
    const source =
        \\step! : {} => Try({}, [Oops])
        \\step! = |_| Err(Oops)
        \\call! = |operation| {
        \\    step!({})?
        \\    operation({})
        \\}
        \\outer! = |operation| {
        \\    call!(|{}| {
        \\        step!({})?
        \\        operation({})
        \\    })
        \\}
        \\direct! = |{}| call!(|{}| {
        \\    step!({})?
        \\    Ok({})
        \\})
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("direct!", "{} => Try({}, [Oops])");
}

test "row union - a repeated tag reaching a method dispatcher merges first" {
    const source =
        \\step! : {} => Try({}, [Oops])
        \\step! = |_| Err(Oops)
        \\call! = |operation| {
        \\    step!({})?
        \\    operation({})
        \\}
        \\mapped! = |{}| call!(|{}| {
        \\    step!({})?
        \\    Ok({})
        \\}).map_err(|e| e)
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "row union - a method call types like the direct call it names" {
    const source =
        \\Stmt :: { id : U64 }.{
        \\    run! = |_stmt, {}| {
        \\        bind!({})?
        \\        Err(NoRows)
        \\    }
        \\}
        \\prepare! : {} => Try(Stmt, [DbErr(Str)])
        \\prepare! = |_| Ok({ id: 1 })
        \\bind! : {} => Try({}, [DbErr(Str)])
        \\bind! = |_| Ok({})
        \\by_method! = |{}| {
        \\    stmt = prepare!({})?
        \\    stmt.run!({})
        \\}
        \\by_name! = |{}| {
        \\    stmt = prepare!({})?
        \\    Stmt.run!(stmt, {})
        \\}
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("by_method!", "{} => Try(ok, [DbErr(Str), NoRows])");
    try env.assertDefType("by_name!", "{} => Try(ok, [DbErr(Str), NoRows])");
}

test "row union - a tag repeated two extensions down merges" {
    const source =
        \\step! : {} => Try({}, [Oops])
        \\step! = |_| Err(Oops)
        \\other! : {} => Try({}, [Other])
        \\other! = |_| Err(Other)
        \\call! = |operation| {
        \\    step!({})?
        \\    operation({})
        \\}
        \\nested! = |{}| call!(|{}| {
        \\    other!({})?
        \\    call!(|{}| Ok({}))
        \\})
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("nested!", "{} => Try({}, [Oops, Other])");
}

test "row union - conflicting payloads of one repeated tag are rejected" {
    const source =
        \\step! : {} => Try({}, [Oops(U64)])
        \\step! = |_| Ok({})
        \\call! = |operation| {
        \\    step!({})?
        \\    operation({})
        \\}
        \\outer! : {} => Try({}, [Oops(U64)])
        \\outer! = |{}| call!(|{}| Err(Oops("not a U64")))
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertHasTypeError("Type Mismatch");
}

test "row union - a repeated tag with a different payload count is rejected" {
    const source =
        \\step! : {} => Try({}, [Oops])
        \\step! = |_| Ok({})
        \\call! = |operation| {
        \\    step!({})?
        \\    operation({})
        \\}
        \\outer! = |{}| call!(|{}| Err(Oops(1.U64)))
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertHasTypeError("Conflicting Tag");
}
