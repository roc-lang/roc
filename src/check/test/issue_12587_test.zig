//! repro for https://github.com/roc-lang/roc/issues/12587
const TestEnv = @import("TestEnv.zig");

const row_and_client =
    \\Row := [Row].{
    \\    i32 : Row -> Try(I32, [Missing])
    \\    i32 = |_row| Ok(1)
    \\}
    \\
    \\Client := [Client].{
    \\    query : Client, (Row -> Try(a, err)) -> Try(List(a), err)
    \\    query = |_client, decode| Ok([decode(Row.Row)?])
    \\}
    \\
;

test "issue 12587: same-name method requirements whose merge would build a cyclic type are the method conflict" {
    var env = try TestEnv.init("Test", row_and_client ++
        \\run = |db| {
        \\    _ = db.query(|row| Ok({ id: row.i32()? }))?
        \\    _ = db.query(|row| row.i32())?
        \\    Ok({})
        \\}
        \\
        \\main = run(Client.Client)
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\This is used in a way that requires its type to have two different, incompatible `i32` methods, but no type can have more than one `i32` method.
        \\```roc
        \\    _ = db.query(|row| Ok({ id: row.i32()? }))?
        \\```
        \\                                ^^^^^^^^^
        \\
        \\One part of this use needs `i32` to have this type:
        \\
        \\    b -> Try({ id: ok }, err)
        \\
        \\Another part needs it to have this type:
        \\
        \\    b -> Try(ok, err)
        \\
        \\A type has only one `i32` method, so no type can satisfy both.
        \\
        \\
    });
}

test "issue 12587: the cyclic merge is the method conflict without a call" {
    var env = try TestEnv.init("Test", row_and_client ++
        \\run = |db| {
        \\    _ = db.query(|row| Ok({ id: row.i32()? }))?
        \\    _ = db.query(|row| row.i32())?
        \\    Ok({})
        \\}
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\This is used in a way that requires its type to have two different, incompatible `i32` methods, but no type can have more than one `i32` method.
        \\```roc
        \\    _ = db.query(|row| Ok({ id: row.i32()? }))?
        \\```
        \\                                ^^^^^^^^^
        \\
        \\One part of this use needs `i32` to have this type:
        \\
        \\    b -> Try({ id: ok }, err)
        \\
        \\Another part needs it to have this type:
        \\
        \\    b -> Try(ok, err)
        \\
        \\A type has only one `i32` method, so no type can satisfy both.
        \\
        \\
    });
}

test "issue 12587: requirements copied from helpers merge into the method conflict" {
    var env = try TestEnv.init("Test", row_and_client ++
        \\with_id = |db| db.query(|row| Ok({ id: row.i32()? }))
        \\
        \\plain = |db| db.query(|row| row.i32())
        \\
        \\run = |db| {
        \\    _ = with_id(db)?
        \\    _ = plain(db)?
        \\    Ok({})
        \\}
        \\
        \\main = run(Client.Client)
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\This is used in a way that requires its type to have two different, incompatible `i32` methods, but no type can have more than one `i32` method.
        \\```roc
        \\run = |db| {
        \\```
        \\^^^
        \\
        \\One part of this use needs `i32` to have this type:
        \\
        \\    b -> Try({ id: ok }, err)
        \\
        \\Another part needs it to have this type:
        \\
        \\    b -> Try(ok, err)
        \\
        \\A type has only one `i32` method, so no type can satisfy both.
        \\
        \\
    });
}

test "issue 12587: a cycle reachable only through a method requirement is an infinite type" {
    var env = try TestEnv.init("Test",
        \\g = |x| x.m(|v| v(v))
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Infinite Type**
        \\I am inferring a weird self-referential type.
        \\```roc
        \\g = |x| x.m(|v| v(v))
        \\```
        \\^^^^^^^^^^^^^^^^^^^^^
        \\
        \\Here is my best effort at writing down the type. You will see `<RecursiveType>` for parts of the type that repeat infinitely.
        \\
        \\    <RecursiveType> -> _ret
        \\
        \\
        \\
    });
}

test "issue 12587: a cycle no binding's type reaches is an infinite type" {
    var env = try TestEnv.init("Test",
        \\main : List(Str)
        \\main = List.map([], |v| {
        \\    _ = v(v)
        \\    "x"
        \\})
    );
    defer env.deinit();
    try env.assertTypeErrorMsgs(&.{
        \\**Infinite Type**
        \\I am inferring a weird self-referential type.
        \\```roc
        \\main = List.map([], |v| {
        \\```
        \\                     ^
        \\
        \\Here is my best effort at writing down the type. You will see `<RecursiveType>` for parts of the type that repeat infinitely.
        \\
        \\    <RecursiveType> -> _ret
        \\
        \\
        \\
    });
}

test "issue 12587: a requirement whose type refers back to its receiver is valid" {
    var env = try TestEnv.init("Test",
        \\triple = |x| x + x + x
        \\
        \\chain = |x| x.next().next()
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12587: a declaration cycle without a tag is infinite whichever tuple element comes first" {
    var tag_first = try TestEnv.init("Test",
        \\X := ([C(Y)], Y)
        \\
        \\Y := X
    );
    defer tag_first.deinit();
    try tag_first.assertTypeErrorMsgs(&.{
        \\**Invalid Recursive Type**
        \\The nominal type _X_ refers to itself in a way that would make it infinite.
        \\```roc
        \\X := ([C(Y)], Y)
        \\```
        \\^^^^^^^^^^^^^^^^
        \\
        \\Its definition is:
        \\
        \\    ([C(Y)], Y)
        \\
        \\**Hint:** Recursion in a nominal type is only allowed inside a tag union payload or record field—for example `ConsList(a) := [Nil, Cons(a, ConsList(a))]`.
        \\
        \\
    });

    var tag_last = try TestEnv.init("Test",
        \\X := (Y, [C(Y)])
        \\
        \\Y := X
    );
    defer tag_last.deinit();
    try tag_last.assertTypeErrorMsgs(&.{
        \\**Invalid Recursive Type**
        \\The nominal type _X_ refers to itself in a way that would make it infinite.
        \\```roc
        \\X := (Y, [C(Y)])
        \\```
        \\^^^^^^^^^^^^^^^^
        \\
        \\Its definition is:
        \\
        \\    (Y, [C(Y)])
        \\
        \\**Hint:** Recursion in a nominal type is only allowed inside a tag union payload or record field—for example `ConsList(a) := [Nil, Cons(a, ConsList(a))]`.
        \\
        \\
    });
}
