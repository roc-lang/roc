//! Tests for custom number types that implement from_numeral

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

test "Custom number type with from_numeral: typed integer suffix unifies" {
    const source =
        \\  MyNum := [].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Err(InvalidNumeral("not supported"))
        \\  }
        \\
        \\  x : MyNum
        \\  x = 123.MyNum
    ;

    var test_env = try TestEnv.init("MyNum", source);
    defer test_env.deinit();

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

test "Custom number type with from_numeral: typed decimal suffix unifies" {
    const source =
        \\  MyDecimal := [].{
        \\    from_numeral : Numeral -> Try(MyDecimal, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Err(InvalidNumeral("not implemented"))
        \\  }
        \\
        \\  x : MyDecimal
        \\  x = 3.14.MyDecimal
    ;

    var test_env = try TestEnv.init("MyDecimal", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "Custom number type with from_numeral: exact huge fractional suffix unifies" {
    const source =
        \\  Big := [
        \\      Value({ is_negative: Bool, before: List(U8), after: List(U8), count: U64 }),
        \\  ].{
        \\      from_numeral : Numeral -> Try(Big, [InvalidNumeral(Str)])
        \\      from_numeral = |numeral| Ok(Value({
        \\          is_negative: numeral.is_negative(),
        \\          before: numeral.digits_before_pt(),
        \\          after: numeral.digits_after_pt(),
        \\          count: numeral.digits_after_pt_count(),
        \\      }))
        \\  }
        \\
        \\  main = {
        \\      value = 340282366920938463463374607431768211456.00000000000000000001.Big
        \\
        \\      match value {
        \\          Value(parts) => (parts.is_negative, parts.before, parts.after, parts.count)
        \\      }
        \\  }
    ;

    var test_env = try TestEnv.init("Big", source);
    defer test_env.deinit();

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

    // Should fail - MyType doesn't have from_numeral, so number literal can't be used
    try test_env.assertOneTypeError("Type Mismatch");
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
    try test_env.assertOneTypeError("Missing Method");
}

test "Custom type with from_numeral and heterogeneous plus: literal + literal works" {
    const source =
        \\  MyNum := [Blah].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(Blah)
        \\
        \\    plus : MyNum, I64 -> MyNum
        \\    plus = |_, _| Blah
        \\  }
        \\
        \\  z : MyNum
        \\  z = 1 + 2
    ;

    var test_env = try TestEnv.init("MyNum", source);
    defer test_env.deinit();

    // Type-checks: the annotation pins the lhs literal to MyNum, and the binop
    // dispatch constraint against MyNum.plus pins the rhs literal to I64. Eagerly
    // unifying the two literal operands (assuming literal + literal must be
    // homogeneous) would wrongly force rhs = MyNum.
    try test_env.assertNoErrors();
}

test "Custom type with heterogeneous times: Duration * I64 works" {
    const source =
        \\Duration := { seconds: I64 }.{
        \\    times : Duration, I64 -> Duration
        \\    times = |d, n| { seconds: d.seconds * n }
        \\}
        \\
        \\my_duration : Duration
        \\my_duration = { seconds: 60 }
        \\
        \\result : Duration
        \\result = my_duration * 5
    ;

    var test_env = try TestEnv.init("Duration", source);
    defer test_env.deinit();

    // Should type-check successfully - Duration.times accepts Duration and I64
    try test_env.assertNoErrors();
}

test "Custom type with heterogeneous times: Duration * I64 variable works" {
    const source =
        \\Duration := { seconds: I64 }.{
        \\    times : Duration, I64 -> Duration
        \\    times = |d, n| { seconds: d.seconds * n }
        \\}
        \\
        \\my_duration : Duration
        \\my_duration = { seconds: 60 }
        \\
        \\count : I64
        \\count = 5
        \\
        \\result : Duration
        \\result = my_duration * count
    ;

    var test_env = try TestEnv.init("Duration", source);
    defer test_env.deinit();

    // Type-checks. The rhs is a CONCRETE builtin numeric (I64) but the
    // lhs/dispatcher is a user nominal: eagerly unifying the operands (assuming
    // homogeneity because one side is builtin-numeric) would wrongly force
    // Duration = I64. The dispatch constraint
    // `Duration.times : Duration, I64 -> Duration` must resolve this instead.
    try test_env.assertNoErrors();
}

test "Flex lambda param plus concrete I64 rhs stays polymorphic" {
    const source =
        \\my_i64 : I64
        \\my_i64 = 7
        \\f = |x| x + my_i64
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // The concrete builtin rhs must NOT eagerly pin the flex lhs (the dispatcher
    // could be any nominal with `plus : a, I64 -> a`); `f` keeps the dispatch
    // constraint instead of collapsing to `I64 -> I64`.
    try test_env.assertDefType("f", "a -> a where [a.plus : a, I64 -> a]");
}
