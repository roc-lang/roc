//! Tests for custom number types that implement from_num_literal

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "Custom number type with from_num_literal: integer literal unifies" {
    const source =
        \\  MyNum := [].{
        \\    from_num_literal : I128 -> Try(MyNum, [OutOfRange])
        \\    from_num_literal = |_| Err(OutOfRange)
        \\  }
        \\
        \\  x : MyNum
        \\  x = 42
    ;

    var test_env = try TestEnv.init("MyNum", source);
    defer test_env.deinit();

    // Should type-check successfully - MyNum has from_num_literal so it can accept integer literals
    try test_env.assertNoErrors();
}

test "Custom number type with from_num_literal: decimal literal unifies" {
    const source =
        \\  MyDecimal := [].{
        \\    from_num_literal : I128 -> Try(MyDecimal, [OutOfRange])
        \\    from_num_literal = |_| Err(OutOfRange)
        \\  }
        \\
        \\  x : MyDecimal
        \\  x = 3.14
    ;

    var test_env = try TestEnv.init("MyDecimal", source);
    defer test_env.deinit();

    // Should type-check successfully - MyDecimal has from_num_literal so it can accept decimal literals
    try test_env.assertNoErrors();
}

test "Custom number type without from_num_literal: integer literal does not unify" {
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

    // Should fail - MyType doesn't have from_num_literal
    try test_env.assertOneTypeError("MISSING METHOD");
}

test "Custom number type with negate: unary minus works" {
    // Skipped: This test requires opaque type constructor syntax within associated items
    // which is not yet implemented. The line `negate = |_| MyNum` doesn't work because
    // MyNum is a type, not a value, and the backing type [] has no constructable values.
    // TODO: Implement proper opaque type value construction or update test expectations.

    const source =
        \\  MyNum := [].{
        \\    from_num_literal : I128 -> Try(MyNum, [OutOfRange])
        \\    from_num_literal = |_| Err(OutOfRange)
        \\
        \\    negate : MyNum -> MyNum
        \\    negate = |_| MyNum
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

    // Should type-check successfully - MyNum has negate so unary minus works
    try test_env.assertNoErrors();
}

test "Custom number type without negate: unary minus fails" {
    const source =
        \\  MyNum := [].{
        \\    from_num_literal : I128 -> Try(MyNum, [OutOfRange])
        \\    from_num_literal = |_| Err(OutOfRange)
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
    // Skipped: This test has a forward reference issue - Negative is used in Positive's
    // method signature before it's declared. Type checking processes associated items
    // before all top-level types are in scope, causing "UNDECLARED TYPE" error.
    // TODO: Implement two-pass type checking (collect types first, then check bodies)
    // or support forward references in associated item type signatures.

    const source =
        \\  Positive := [].{
        \\    from_num_literal : I128 -> Try(Positive, [OutOfRange])
        \\    from_num_literal = |_| Err(OutOfRange)
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

    try test_env.assertNoErrors();
    // y should have type Negative
    try test_env.assertLastDefType("Negative");
}
