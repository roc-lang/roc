//! Tests for custom number types that implement from_numeral

const std = @import("std");
const testing = std.testing;
const TestEnv = @import("./TestEnv.zig");

test "Custom number type with from_numeral: integer literal unifies" {
    const source =
        \\  MyNum := [].{
        \\    from_numeral : I128 -> Try(MyNum, [InvalidNumeral(Str)])
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
        \\    from_numeral : I128 -> Try(MyDecimal, [OutOfRange])
        \\    from_numeral = |_| Err(OutOfRange)
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
        \\    from_numeral : I128 -> Try(MyNum, [OutOfRange])
        \\    from_numeral = |_| Err(OutOfRange)
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
        \\    from_numeral : I128 -> Try(MyNum, [OutOfRange])
        \\    from_numeral = |_| Err(OutOfRange)
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
    // This test documents a forward reference limitation: Negative is used in Positive's
    // method signature before it's declared. Type checking processes associated items
    // before all top-level types are in scope, causing "UNDECLARED TYPE" error.
    // TODO: Implement two-pass type checking (collect types first, then check bodies)
    // or support forward references in associated item type signatures.
    // Once that's implemented, this test should expect no errors and y should have type Negative.

    const source =
        \\  Positive := [].{
        \\    from_numeral : I128 -> Try(Positive, [OutOfRange])
        \\    from_numeral = |_| Err(OutOfRange)
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

    var test_env = try TestEnv.init("CustomNegate", source);
    defer test_env.deinit();

    // Currently fails with UNDECLARED TYPE because forward references aren't supported yet
    // Just verify there are canonicalization errors
    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);
    try testing.expect(diagnostics.len > 0);
}
