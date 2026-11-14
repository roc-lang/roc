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

const gui_source =
    \\Gui := [].{
    \\    Color := [Default, Rgb(U32)].{
    \\        from_numeral : Numeral -> Try(Color, [InvalidNumeral(Str)])
    \\        from_numeral = |numeral| match U32.from_numeral(numeral) {
    \\            Ok(value) => Ok(Rgb(value))
    \\            Err(_) => Err(InvalidNumeral("invalid color"))
    \\        }
    \\    }
    \\}
;

// repro for https://github.com/roc-lang/roc/issues/11466
test "Custom number type with from_numeral: qualified typed integer suffix unifies" {
    var gui_env = try TestEnv.init("Gui", gui_source);
    defer gui_env.deinit();
    try gui_env.assertNoErrors();

    const source =
        \\import Gui
        \\
        \\well : Gui.Color
        \\well = 0x05080a.Gui.Color
    ;

    var test_env = try TestEnv.initWithImport("Theme", source, "Gui", &gui_env);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

// repro for https://github.com/roc-lang/roc/issues/11466
test "Custom number type with from_numeral: qualified typed decimal suffix unifies" {
    var gui_env = try TestEnv.init("Gui", gui_source);
    defer gui_env.deinit();
    try gui_env.assertNoErrors();

    const source =
        \\import Gui
        \\
        \\well : Gui.Color
        \\well = 5.0.Gui.Color
    ;

    var test_env = try TestEnv.initWithImport("Theme", source, "Gui", &gui_env);
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

test "An operator cannot call a plus whose second parameter is another type" {
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

    // The annotation makes both operands and the result `MyNum`, so `+`
    // needs `plus : MyNum, MyNum -> MyNum`.
    try test_env.assertOneTypeError("Type Mismatch");
}

test "A method with mixed parameter types is called by name" {
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
        \\result = my_duration.times(5)
    ;

    var test_env = try TestEnv.init("Duration", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "An operator whose method mixes types reports the method: Duration * I64" {
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

    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "The `times` method on `Duration` has an incompatible type.");
}

test "An operator whose method mixes types reports once for a literal operand: Duration * 3" {
    const source =
        \\Duration := { seconds: I64 }.{
        \\    times : Duration, I64 -> Duration
        \\    times = |d, n| { seconds: d.seconds * n }
        \\}
        \\
        \\my_duration : Duration
        \\my_duration = { seconds: 60 }
        \\
        \\result = my_duration * 3
    ;

    var test_env = try TestEnv.init("Duration", source);
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "The `times` method on `Duration` has an incompatible type.");
}

test "A comparison whose method does not return Bool reports once for a literal operand" {
    const source =
        \\Rank := [Low, High].{
        \\    is_lt : Rank, Rank -> Str
        \\    is_lt = |_, _| "no"
        \\}
        \\
        \\low : Rank
        \\low = Low
        \\
        \\result = low < 3
    ;

    var test_env = try TestEnv.init("Rank", source);
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "The `is_lt` method on `Rank` has an incompatible type.");
}

test "A mismatching operator use leaves the method's other uses intact" {
    const source =
        \\Duration := { seconds: I64 }.{
        \\    times : Duration, I64 -> Duration
        \\    times = |d, n| { seconds: d.seconds * n }
        \\}
        \\
        \\my_duration : Duration
        \\my_duration = { seconds: 60 }
        \\
        \\longer : Duration
        \\longer = my_duration.times(5)
        \\
        \\result : Duration
        \\result = my_duration * my_duration
    ;

    var test_env = try TestEnv.init("Duration", source);
    defer test_env.deinit();

    // `*` needs `times : Duration, Duration -> Duration`. Its rejection is
    // this use's alone: `longer` still calls the method the type declares.
    try test_env.assertOneTypeError("Type Mismatch");
}

test "An operator's known operand decides the other operand" {
    const source =
        \\my_i64 : I64
        \\my_i64 = 7
        \\f = |x| x + my_i64
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertDefType("f", "I64 -> I64");
}

test "Discarded unpinned arithmetic specialization validates the default method type" {
    const source =
        \\run = || {
        \\    f = |x| x + x.absent()
        \\    _ = (f, |x| Str.inspect(x))
        \\    {}
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // The bare use materializes the default for f's arithmetic operand, which
    // has no `absent` method, so checking owns the diagnostic instead of
    // publishing incompatible dispatch evidence. Nothing determines that
    // operand, so it is reported as undetermined.
    try test_env.assertOneTypeError("Missing Method");
}

test "qualified literal suffix follows a re-exported alias to its declaring module" {
    var gui_env = try TestEnv.init("Gui", gui_source);
    defer gui_env.deinit();
    try gui_env.assertNoErrors();

    const facade_source =
        \\import Gui
        \\Facade := [].{
        \\    Palette : Gui
        \\}
    ;
    var facade_env = try TestEnv.initWithImport("Facade", facade_source, "Gui", &gui_env);
    defer facade_env.deinit();
    try facade_env.assertNoErrors();

    const source =
        \\import Facade
        \\integer = 0x05080a.Facade.Palette.Color
        \\decimal = 5.0.Facade.Palette.Color
    ;
    var test_env = try TestEnv.initWithImport("Theme", source, "Facade", &facade_env);
    defer test_env.deinit();
    try test_env.assertNoErrors();
    try test_env.assertDefType("integer", "Gui.Color");
    try test_env.assertDefType("decimal", "Gui.Color");
}

test "unqualified literal suffix resolves an exposed imported type" {
    var gui_env = try TestEnv.init("Gui", gui_source);
    defer gui_env.deinit();
    try gui_env.assertNoErrors();

    const source =
        \\import Gui exposing [Color]
        \\value = 42.Color
    ;
    var test_env = try TestEnv.initWithImport("Theme", source, "Gui", &gui_env);
    defer test_env.deinit();
    try test_env.assertNoErrors();
    try test_env.assertDefType("value", "Gui.Color");
}

test "qualified literal suffix rejects a type the imported module does not expose" {
    var gui_env = try TestEnv.init("Gui", gui_source);
    defer gui_env.deinit();
    try gui_env.assertNoErrors();

    const source =
        \\import Gui
        \\value = 42.Gui.Missing
    ;
    var test_env = try TestEnv.initWithImport("Theme", source, "Gui", &gui_env);
    defer test_env.deinit();
    try test_env.assertOneCanError("Type Not Exposed");
}
