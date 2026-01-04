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
