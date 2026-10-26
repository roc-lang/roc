//! repro for https://github.com/roc-lang/roc/issues/12586
//! A generic function that forwards a callback's error with `?` and adds its
//! own error keeps the callback's error row its own: the callback's row is the
//! result row's residual tail, so a callback with a closed error row, read out
//! of an annotated record or list, is accepted just like the same function
//! passed directly.
const TestEnv = @import("TestEnv.zig");

const send_str =
    \\send_str : Str -> Try(U64, [Empty])
    \\send_str = |url| if url == "" Err(Empty) else Ok(1)
    \\
    \\
;

const fetch =
    \\fetch = |send, url| {
    \\    response = send(url)?
    \\    if response == 0 Err(Bad(url)) else Ok(response)
    \\}
    \\
    \\
;

const generic_fetch =
    \\fetch = |send, url| {
    \\    _ = send(url)?
    \\    Err(Bad(url))
    \\}
    \\
    \\
;

test "issue 12586: a function from an annotated record is accepted by a generic function adding its own error" {
    var env = try TestEnv.init("Test", send_str ++ fetch ++
        \\run = |_| {
        \\    effects : { send : Str -> Try(U64, [Empty]) }
        \\    effects = { send: send_str }
        \\    fetch(effects.send, "x")
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12586: a function from an annotated list is accepted by a generic function adding its own error" {
    var env = try TestEnv.init("Test", send_str ++ fetch ++
        \\run : List((Str -> Try(U64, [Empty]))) -> Try(U64, [Empty, Bad(Str)])
        \\run = |sends|
        \\    match sends.first() {
        \\        Ok(send) => fetch(send, "x")
        \\        Err(_) => Err(Empty)
        \\    }
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12586: a forwarded callback error row is the result row's residual tail" {
    var env = try TestEnv.init("Test", generic_fetch);
    defer env.deinit();
    try env.assertDefType("fetch", "(a -> Try(ok, err)), a -> Try(ok, [Bad(a), ..err])");
}

test "issue 12586: the inferred signature is as general as the annotation that spells it" {
    var env = try TestEnv.init("Test", send_str ++
        \\fetch : (Str -> Try(U64, err)), Str -> Try(U64, [Bad(Str), ..err])
        \\
    ++ fetch ++
        \\run = |_| {
        \\    effects : { send : Str -> Try(U64, [Empty]) }
        \\    effects = { send: send_str }
        \\    fetch(effects.send, "x")
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12586: a callback failing with the same tag at the same payload is accepted" {
    var env = try TestEnv.init("Test", fetch ++
        \\send_bad : Str -> Try(U64, [Bad(Str)])
        \\send_bad = |url| if url == "" Err(Bad(url)) else Ok(1)
        \\
        \\run = |_| {
        \\    effects : { send : Str -> Try(U64, [Bad(Str)]) }
        \\    effects = { send: send_bad }
        \\    fetch(effects.send, "x")
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12586: a callback failing with the same tag at another payload is rejected" {
    var env = try TestEnv.init("Test", fetch ++
        \\send_bad : Str -> Try(U64, [Bad(I64)])
        \\send_bad = |_| Err(Bad(1))
        \\
        \\run = |_| {
        \\    effects : { send : Str -> Try(U64, [Bad(I64)]) }
        \\    effects = { send: send_bad }
        \\    fetch(effects.send, "x")
        \\}
    );
    defer env.deinit();
    try env.assertOneTypeError("Conflicting Tag");
}
