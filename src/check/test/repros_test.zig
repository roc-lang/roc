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
