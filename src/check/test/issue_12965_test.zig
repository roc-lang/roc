//! Regression tests for issue 12965.

const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/12965
//
// A punned field in a record builder (`{ x, y }.Wrap`) is a use of that name,
// exactly as it is in a plain record literal, so it must not leave the bound
// name reported as an unused variable.

const wrap_decl =
    \\Wrap(a) := [Wrap(a)].{
    \\    map2 : Wrap(a), Wrap(b), (a, b -> c) -> Wrap(c)
    \\    map2 = |Wrap(a), Wrap(b), f| Wrap(f(a, b))
    \\
    \\    get : Wrap(a) -> a
    \\    get = |Wrap(a)| a
    \\}
    \\
;

test "issue 12965: punned record builder fields use block-local definitions" {
    const source = wrap_decl ++
        \\expect {
        \\    x = Wrap.Wrap(1)
        \\    y = Wrap.Wrap(2)
        \\    got = { x, y }.Wrap.get()
        \\    got.x == 1 and got.y == 2
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "issue 12965: punned record builder fields use lambda parameters" {
    const source = wrap_decl ++
        \\both : Wrap(I64), Wrap(I64) -> Wrap({ x : I64, y : I64 })
        \\both = |x, y| { x, y }.Wrap
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "issue 12965: punned record builder fields read top-level definitions that follow" {
    const source = wrap_decl ++
        \\both : Wrap({ x : I64, y : I64 })
        \\both = { x, y }.Wrap
        \\
        \\x : Wrap(I64)
        \\x = Wrap.Wrap(1)
        \\
        \\y : Wrap(I64)
        \\y = Wrap.Wrap(2)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}
