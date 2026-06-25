//! Tests for numeric literal size and type unification logic.

const TestEnv = @import("./TestEnv.zig");

test "U8: 255 fits" {
    const source =
        \\{
        \\  x : U8
        \\  x = 50
        \\
        \\  x + 255
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefType("U8");
}

test "U64: app string interpolation constrains local unsuffixed positive literal" {
    const source =
        \\app [main] { pf: platform "platform.roc" }
        \\
        \\main = {
        \\  x = 42
        \\  y = x
        \\
        \\  "done: ${U64.to_str(y)}"
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("main", "Str");
}

test "I8: -128 fits" {
    const source =
        \\{
        \\  x : I8
        \\  x = 1
        \\
        \\  x + -128
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefType("I8");
}

test "F32: fits" {
    const source =
        \\{
        \\  x : F32
        \\  x = 1
        \\
        \\  x + 10.1
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefType("F32");
}
