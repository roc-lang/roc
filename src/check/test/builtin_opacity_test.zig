//! Tests that the Builtin nominal types the checker builds itself (for
//! literals, annotations, and from_numeral signatures) are as opaque as their
//! declarations in Builtin.roc.

const TestEnv = @import("./TestEnv.zig");

test "a from_numeral body cannot open Numeral's backing" {
    const source =
        \\MyNum := [Wrapped(Bool)].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| match numeral {
        \\        Literal(parts) => Ok(Wrapped(parts.is_negative))
        \\    }
        \\}
        \\
        \\x : MyNum
        \\x = 42
    ;

    var test_env = try TestEnv.init("MyNum", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("Type Mismatch");
}

test "a from_numeral body reads Numeral through its accessor methods" {
    const source =
        \\MyNum := [Wrapped(Bool)].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Wrapped(numeral.is_negative()))
        \\}
        \\
        \\x : MyNum
        \\x = 42
    ;

    var test_env = try TestEnv.init("MyNum", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "a list literal is not List's backing type" {
    const source =
        \\f : [ProvidedByCompiler] -> I64
        \\f = |_| 1
        \\
        \\main : I64
        \\main = f([1.I64, 2])
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Type Mismatch");
}

test "an annotated Box is not Box's backing type" {
    const source =
        \\f : [ProvidedByCompiler] -> I64
        \\f = |_| 1
        \\
        \\b : Box(I64)
        \\b = Box.box(1)
        \\
        \\main : I64
        \\main = f(b)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Type Mismatch");
}
