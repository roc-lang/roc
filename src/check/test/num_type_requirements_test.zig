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
