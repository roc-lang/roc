//! Tests for custom number types that implement from_numeral

const std = @import("std");
const testing = std.testing;
const TestEnv = @import("./TestEnv.zig");

test "Custom number type with from_numeral: integer literal unifies" {
    const source =
        \\  MyNum := [].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Err(InvalidNumeral("not supported"))
        \\  }
        \\
        \\  x : MyNum
        \\  x = 42
    ;

    var test_env = try TestEnv.init("MyNum", source);
    defer test_env.deinit();

    // Should type-check successfully - MyNum has from_numeral so it can accept integer literals
    try test_env.assertNoErrors();
}

test "Custom number type with from_numeral: decimal literal unifies" {
    const source =
        \\  MyDecimal := [].{
        \\    from_numeral : Numeral -> Try(MyDecimal, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Err(InvalidNumeral("not implemented"))
        \\  }
        \\
        \\  x : MyDecimal
        \\  x = 3.14
    ;

    var test_env = try TestEnv.init("MyDecimal", source);
    defer test_env.deinit();

    // Should type-check successfully - MyDecimal has from_numeral so it can accept decimal literals
    try test_env.assertNoErrors();
}

test "Custom number type without from_numeral: integer literal does not unify" {
    const source =
        \\  MyType := [].{
        \\    some_method : MyType -> Bool
        \\    some_method = |_| True
        \\  }
        \\
        \\  x : MyType
        \\  x = 42
    ;

    var test_env = try TestEnv.init("MyType", source);
    defer test_env.deinit();

    // Should fail - MyType doesn't have from_numeral
    try test_env.assertOneTypeError("MISSING METHOD");
}

test "Custom number type with negate: unary minus works" {
    const source =
        \\  MyNum := [Blah].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Err(InvalidNumeral("not implemented"))
        \\
        \\    negate : MyNum -> MyNum
        \\    negate = |_| Blah
        \\  }
        \\
        \\  x : MyNum
        \\  x = 42
        \\
        \\  y : MyNum
        \\  y = -x
    ;

    var test_env = try TestEnv.init("MyNum", source);
    defer test_env.deinit();

    // Should type-check successfully - MyNum has negate so unary minus works,
    // and Blah is a valid tag in the backing type [Blah]
    try test_env.assertNoErrors();
}

test "Custom number type without negate: unary minus fails" {
    const source =
        \\  MyNum := [].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Err(InvalidNumeral("not implemented"))
        \\  }
        \\
        \\  x : MyNum
        \\  x = 42
        \\
        \\  y : MyNum
        \\  y = -x
    ;

    var test_env = try TestEnv.init("MyNum", source);
    defer test_env.deinit();

    // Should fail - MyNum doesn't have negate method
    try test_env.assertOneTypeError("MISSING METHOD");
}

test "Custom type with negate returning different type" {
    // Skip this test, as currently `negate` requires the return type to be the
    // same type as the arg.
    if (true) return error.SkipZigTest;

    // Tests that forward references between sibling types work.
    // Positive's negate method returns Negative, which is declared after Positive.
    // This requires all type declarations to be introduced into scope before
    // processing associated item signatures.

    const source =
        \\  Positive := [].{
        \\    from_numeral : Numeral -> Try(Positive, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Err(InvalidNumeral("not implemented"))
        \\
        \\    negate : Positive -> Negative
        \\    negate = |_| Negative.Value
        \\  }
        \\
        \\  Negative := [Value]
        \\
        \\  x : Positive
        \\  x = 5
        \\
        \\  y = -x
    ;

    var test_env = try TestEnv.init("Positive", source);
    defer test_env.deinit();

    // Should type-check successfully - Positive can reference Negative in its
    // negate method signature even though Negative is declared after Positive.
    // The result y should have type Negative.
    try test_env.assertNoErrors();
}
