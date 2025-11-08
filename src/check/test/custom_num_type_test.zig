//! Tests for custom number types that implement from_int_digits or from_dec_digits

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "Custom number type with from_int_digits: integer literal unifies" {
    const source =
        \\{
        \\  MyNum := [].{
        \\    from_int_digits : List(U8) -> Try(MyNum, [OutOfRange])
        \\    from_int_digits = |_| Err(OutOfRange)
        \\  }
        \\
        \\  x : MyNum
        \\  x = 42
        \\
        \\  x
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();

    // Should type-check successfully - MyNum has from_int_digits so it can accept integer literals
    try test_env.assertNoErrors();
}

test "Custom number type with from_dec_digits: decimal literal unifies" {
    const source =
        \\{
        \\  MyDecimal := [].{
        \\    from_dec_digits : { before_dot : List(U8), after_dot : List(U8) } -> Try(MyDecimal, [OutOfRange])
        \\    from_dec_digits = |_| Err(OutOfRange)
        \\  }
        \\
        \\  x : MyDecimal
        \\  x = 3.14
        \\
        \\  x
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();

    // Should type-check successfully - MyDecimal has from_dec_digits so it can accept decimal literals
    try test_env.assertNoErrors();
}

test "Custom number type without from_int_digits: integer literal does not unify" {
    const source =
        \\{
        \\  MyType := [].{
        \\    some_method : MyType -> Bool
        \\    some_method = |_| True
        \\  }
        \\
        \\  x : MyType
        \\  x = 42
        \\
        \\  x
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();

    // Should fail - MyType doesn't have from_int_digits
    try test_env.assertOneTypeError("TYPE MISMATCH");
}
