//! Regression tests for https://github.com/roc-lang/roc/issues/12142.
//!
//! Every use of an effectful method is an effectful call, however many
//! identical uses came before it. Once one concrete use has settled a ground
//! instance of a method, later uses of the same shape share that instance
//! (concrete dispatch replay); a shared use must still take the instance's
//! effect, or a function calling it checks as pure and its calls become
//! eligible for compile-time evaluation.
const TestEnv = @import("TestEnv.zig");

test "issue 12142: the third identical use of an effectful method is effectful" {
    const source =
        \\Cmd := [Cmd(Str => U64)].{
        \\  spawn! : Cmd => U64
        \\  spawn! = |Cmd.Cmd(run!)| run!("true")
        \\}
        \\
        \\first! = |run!| Cmd.Cmd(run!).spawn!()
        \\
        \\second! = |run!| Cmd.Cmd(run!).spawn!()
        \\
        \\third! = |run!| Cmd.Cmd(run!).spawn!()
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("first!", "(Str => U64) => U64");
    try env.assertDefType("second!", "(Str => U64) => U64");
    try env.assertDefType("third!", "(Str => U64) => U64");
}

test "issue 12142: the third identical use of a helper calling an effectful method is effectful" {
    const source =
        \\Cmd := [Cmd(Str => U64)].{
        \\  spawn! : Cmd => U64
        \\  spawn! = |Cmd.Cmd(run!)| run!("true")
        \\}
        \\
        \\launch! = |cmd| cmd.spawn!()
        \\
        \\first! = |run!| launch!(Cmd.Cmd(run!))
        \\
        \\second! = |run!| launch!(Cmd.Cmd(run!))
        \\
        \\third! = |run!| launch!(Cmd.Cmd(run!))
        \\
        \\fourth! = |run!| launch!(Cmd.Cmd(run!))
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("first!", "(Str => U64) => U64");
    try env.assertDefType("second!", "(Str => U64) => U64");
    try env.assertDefType("third!", "(Str => U64) => U64");
    try env.assertDefType("fourth!", "(Str => U64) => U64");
}
