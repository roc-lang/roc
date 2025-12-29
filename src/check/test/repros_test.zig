//! Regression tests for specific bug fixes.

const TestEnv = @import("./TestEnv.zig");
const testing = @import("std").testing;
const std = @import("std");

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
