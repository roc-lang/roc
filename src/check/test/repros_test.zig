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
