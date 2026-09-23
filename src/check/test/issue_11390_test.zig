//! Regression tests for https://github.com/roc-lang/roc/issues/11390.
//! A def annotated with a non-function type but defined as a lambda is a type
//! mismatch. When a mutually recursive sibling calls that def, the call must be
//! reported against the declared non-function type rather than treated as a
//! call to a function whose effect is still being inferred.

const TestEnv = @import("./TestEnv.zig");

test "issue 11390 recursive call to a lambda annotated with a nominal type reports a type error" {
    const src =
        \\RBMut(k) := [
        \\    Empty,
        \\    Node(RBMut(k)),
        \\].{
        \\    delA : RBMut(k)
        \\    delA = |inner| match inner {
        \\        RBMut.Node(x) => x |> delB
        \\        Empty => Empty
        \\    }
        \\    delB : RBMut(k) -> RBMut(k)
        \\    delB = |t| match t {
        \\        RBMut.Node(inner) => inner |> delA
        \\        _ => t
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init("RBMut", src);
    defer test_env.deinit();
    try test_env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\This expression is used in an unexpected way.
        \\```roc
        \\    delA = |inner| match inner {
        \\        RBMut.Node(x) => x |> delB
        \\        Empty => Empty
        \\    }
        \\```
        \\
        \\It has the type:
        \\
        \\    RBMut(k) -> RBMut(k)
        \\
        \\But the annotation says it should be:
        \\
        \\    RBMut(k)
        \\
        \\
        ,
        \\**Not A Function**
        \\The `RBMut.delA` value is not a function, but it was given 1 argument.
        \\```roc
        \\        RBMut.Node(inner) => inner |> delA
        \\```
        \\                             ^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    RBMut(k)
        \\
        \\
        ,
    });
}

test "issue 11390 recursive call to a lambda annotated with a non-function type reports a type error" {
    const src =
        \\f : Str
        \\f = |x| g(x)
        \\
        \\g : Str -> Str
        \\g = |y| f(y)
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\This expression is used in an unexpected way.
        \\```roc
        \\f = |x| g(x)
        \\```
        \\    ^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    Str -> Str
        \\
        \\But the annotation says it should be:
        \\
        \\    Str
        \\
        \\
        ,
        \\**Not A Function**
        \\The `f` value is not a function, but it was given 1 argument.
        \\```roc
        \\g = |y| f(y)
        \\```
        \\        ^^^^
        \\
        \\It has the type:
        \\
        \\    Str
        \\
        \\
        ,
    });
}
