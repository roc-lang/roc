//! Regression tests for specific bug fixes.

const TestEnv = @import("./TestEnv.zig");

test "check - repro - issue 8764" {
    const src =
        \\main! = |_| {}
        \\         
        \\NumericRangeType : [
        \\    ExcludesEndValue, # ..<
        \\    IncludesEndValue, # ..=
        \\]
        \\
        \\TokenContents : [
        \\    NewlineToken,
        \\    OpenBracketToken, # (
        \\    CloseBracketToken, # )
        \\    OpenSquareBracketToken, # [
        \\    CloseSquareBracketToken, # ]
        \\    OpenBraceToken, # {
        \\    CloseBraceToken, # }
        \\    CommaToken, # ,
        \\    ColonToken, # :
        \\    AssignToken, # =
        \\    EqualToken, # ==
        \\    MinusToken, # -
        \\    ArrowToken, # ->
        \\    DotDotToken(NumericRangeType),
        \\    DotToken, # .
        \\    UIntDigitsToken(U64),
        \\    IntDigitsToken(I64),
        \\    FloatDigitsToken(F64),
        \\    StrDigitsToken(Str),
        \\    ForToken,
        \\    InToken,
        \\    IfToken,
        \\    ElseToken,
        \\    DoToken,
        \\    WhileToken,
        \\    MutToken,
        \\    IdentToken(Str),
        \\    CommentToken(Str),
        \\    StringToken(Str),
        \\    EndOfFileToken,
        \\]
        \\
        \\get_next_token : Try(TokenContents, Str)
        \\get_next_token = {
        \\    contents : TokenContents
        \\    contents = DotDotToken(ExcludesEndValue)
        \\    Ok(contents)
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 8785" {
    // Pattern matching on recursive opaque type element retrieved from List
    const src =
        \\main! = |_| {}
        \\
        \\Node := [
        \\    Element(Str, List(Node)),
        \\    Text(Str),
        \\]
        \\
        \\test_pattern_match = |node| {
        \\    match node {
        \\        Element(tag, _) => tag
        \\        Text(text) => text
        \\    }
        \\}
        \\
        \\result = {
        \\    element_val : Node
        \\    element_val = Element("span", [])
        \\
        \\    element_list : List(Node)
        \\    element_list = [element_val]
        \\
        \\    match List.first(element_list) {
        \\        Ok(v) => test_pattern_match(v)
        \\        Err(_) => "Error"
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 8919" {
    // Instance method syntax should not produce UNUSED VALUE error
    // when the method returns {}. Previously, the return type was not
    // resolved before the unused value check due to deferred constraints.
    // The bug only manifests when the method call is an s_expr statement
    // (not the final expression of a block).
    const src =
        \\main! = |_| {}
        \\
        \\Foo := { x: I64 }.{
        \\    show! = |_self| {}
        \\}
        \\
        \\test_method! = |_| {
        \\    f : Foo
        \\    f = { x: 42 }
        \\    f.show!()
        \\    Ok({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129" {
    // Type variable becomes 'rigid' and won't unify with concrete types when
    // captured in a closure created inside a function body
    const src =
        \\BugRepro(a) := { run : {} -> a }.{
        \\    wrap : a -> BugRepro(a)
        \\    wrap = |value| { run: |{}| value }
        \\
        \\    unwrap : BugRepro(a) -> a
        \\    unwrap = |b| (b.run)({})
        \\}
        \\
        \\expect BugRepro.unwrap(BugRepro.wrap("hi")) == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129 simpler" {
    // Simplified version: just wrap a value without closure
    const src =
        \\BugRepro(a) := { value : a }.{
        \\    wrap : a -> BugRepro(a)
        \\    wrap = |v| { value: v }
        \\}
        \\
        \\expect (BugRepro.wrap("hi")).value == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129 closure capture" {
    // The issue is specifically about closures capturing type variables
    // This test uses a record with a closure field without nominal types
    const src =
        \\main! = |_| {}
        \\
        \\# Simple version that tests just the closure capture
        \\wrap : a -> ({} -> a)
        \\wrap = |value| |{}| value
        \\
        \\# Call the wrapped closure
        \\result = wrap("hi")({})
        \\
        \\expect result == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129 - minimal closure capture" {
    // Minimal test case: a polymorphic function that returns a closure capturing the type variable
    const src =
        \\main! = |_| {}
        \\
        \\wrap : a -> ({} -> a)
        \\wrap = |value| |{}| value
        \\
        \\expect wrap("hi")({}) == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129 - control no closure" {
    // Control test: polymorphic function WITHOUT a closure - this should pass
    const src =
        \\main! = |_| {}
        \\
        \\identity : a -> a
        \\identity = |value| value
        \\
        \\expect identity("hi") == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129 - with explicit block" {
    // Test with explicit block - still fails
    const src =
        \\main! = |_| {}
        \\
        \\wrap : a -> ({} -> a)
        \\wrap = |value| {
        \\    |{}| value
        \\}
        \\
        \\expect wrap("hi")({}) == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 8848" {
    // Pattern matching on recursive opaque type element retrieved from List
    const src =
        \\ValueCombinationMethod := [
        \\  Modulo,
        \\]
        \\
        \\Value := [
        \\    UInt(U64),
        \\    CombinedValue({
        \\        combination_method: ValueCombinationMethod,
        \\        value1: Value,
        \\        value2: Value,
        \\    }),
        \\]
        \\
        \\TokenContents : []
        \\
        \\TokenizerResult : (
        \\    Try(TokenContents, Str),
        \\    U64, # Index of start of token/error
        \\    U64, # New index in file
        \\)
        \\get_next_token : List(U8), U64 -> TokenizerResult
        \\get_next_token = |_file, _index| {
        \\    (Err("todo"), 0, 0)
        \\}
        \\
        \\parse_value : List(U8), TokenizerResult, List(Str) -> Try((Value, U64), Str)
        \\parse_value = |file, result, possibilities| {
        \\    (first_value, var $index) = parse_first_value(file, result, possibilities)?
        \\    (token, token_pos, $index) = get_next_token(file, $index)
        \\    (value1, token2, token2_pos) = match (first_value, token) {
        \\      (VariableReference(name), Ok(OpenBracketToken)) => {
        \\        # TODO
        \\        (t2, t2_pos, $index) = get_next_token(file, $index)
        \\        (FunctionCall({name, args: []}), t2, t2_pos)
        \\      }
        \\      _ => (first_value, token, token_pos)
        \\    }
        \\    combination_method1 : ValueCombinationMethod
        \\    combination_method1 = Modulo
        \\    (value2, $index) = parse_value(file, get_next_token(file, $index), [])
        \\    value : Value
        \\    value = match value2 {
        \\        CombinedValue({combination_method: combination_method2, value1: value2A, value2: value2B}) => {
        \\            # Combination method 1 is ran first
        \\            CombinedValue({
        \\                combination_method: combination_method2,
        \\                value1: CombinedValue({
        \\                    combination_method: combination_method1,
        \\                    value1: value1,
        \\                    value2: value2A,
        \\                }),
        \\                value2: value2B,
        \\            })
        \\        }
        \\        _ => CombinedValue({combination_method: combination_method1, value1, value2})
        \\    }
        \\    Ok((value, $index))
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I32)])
        \\main! = |_args| {
        \\    (value, _index) = parse_value(Str.to_utf8("15 + 4 == 19"), 0, []).map_err!(|e| {
        \\        Stderr.line!(e)
        \\        Exit(1)
        \\    })?
        \\    Stdout.line!(Str.inspect(value))
        \\    Ok({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    // No assertion here, this repro previously panicked, so that's the
    // regression we're guarding against
}

test "check - repro - bad return branch mismatch after utf8 empty guard" {
    const src =
        \\main! = |_| {}
        \\
        \\to_uppercase : U8 -> U8
        \\to_uppercase = |ch| ch - 32
        \\
        \\capitalize_first : Str -> Str
        \\capitalize_first = |s| {
        \\    bytes = Str.to_utf8(s)
        \\    if List.is_empty(bytes) {
        \\        return ""
        \\    }
        \\
        \\    first = match List.first(bytes) {
        \\        Ok(b) => b
        \\        Err(_) => 0
        \\    }
        \\    first_is_lower = first >= 'a' and first <= 'z'
        \\    new_first = if first_is_lower to_uppercase(first) else first
        \\    match Str.from_utf8([new_first]) {
        \\        Ok(str) => str
        \\        Err(_) => s
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - bad inline if branch mismatch in utf8 byte loop" {
    const src =
        \\main! = |_| {}
        \\
        \\to_lowercase : U8 -> U8
        \\to_lowercase = |ch| ch + 32
        \\
        \\to_lower_snake_case : Str -> Str
        \\to_lower_snake_case = |s| {
        \\    bytes = Str.to_utf8(s)
        \\    var $output = []
        \\
        \\    for byte in bytes {
        \\        is_upper = byte >= 'A' and byte <= 'Z'
        \\        new_byte = if is_upper to_lowercase(byte) else byte
        \\        $output = $output.append(new_byte)
        \\    }
        \\
        \\    match Str.from_utf8($output) {
        \\        Ok(str) => str
        \\        Err(_) => s
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - using dbg in a function does not make it effectful" {
    const src =
        \\main! = |_args| {}
        \\
        \\User := [
        \\  LoggedIn(Str),
        \\  Guest
        \\].{
        \\  welcome : User -> {}
        \\  welcome = |self| match (self) {
        \\    Guest => dbg "Welcome Guest"
        \\    LoggedIn(name) => dbg Str.concat("Welcome Guest", name)
        \\  }
        \\}
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "check - repro - issue 9500 - equality on tag-union alias" {
    const src =
        \\Color : [Red, Green, Blue]
        \\
        \\pick : U8 -> Color
        \\pick = |n| match n {
        \\    0 => Red
        \\    1 => Green
        \\    _ => Blue
        \\}
        \\
        \\is_red : Bool
        \\is_red = pick(0) == Red
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "check - repro - issue 9500 - inequality on tag-union alias" {
    const src =
        \\Color : [Red, Green, Blue]
        \\
        \\pick : U8 -> Color
        \\pick = |n| match n {
        \\    0 => Red
        \\    1 => Green
        \\    _ => Blue
        \\}
        \\
        \\not_red : Bool
        \\not_red = pick(0) != Red
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "check - repro - issue 9500 - equality on record alias" {
    const src =
        \\Point : { x : U8, y : U8 }
        \\
        \\origin : Point
        \\origin = { x: 0, y: 0 }
        \\
        \\is_origin : Bool
        \\is_origin = origin == { x: 0, y: 0 }
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "check - repro - issue 9500 - equality on record alias of tag-union aliases" {
    const src =
        \\Palette : [None, Color1, Color2, Color3, Color4]
        \\
        \\DrawColors : {
        \\    primary : Palette,
        \\    secondary : Palette,
        \\}
        \\
        \\from_flags : U8 -> DrawColors
        \\from_flags = |flags| match flags {
        \\    0 => { primary: None, secondary: None }
        \\    _ => { primary: Color2, secondary: Color4 }
        \\}
        \\
        \\is_match : Bool
        \\is_match = from_flags(1) == { primary: Color2, secondary: Color4 }
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}
