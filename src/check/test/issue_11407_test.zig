//! Tag-union diffing while building a Type Mismatch report (issue 11407).
//! https://github.com/roc-lang/roc/issues/11407
const TestEnv = @import("TestEnv.zig");

test "issue 11407: diffing tag unions that outgrow the diff buffer builds every report" {
    const source =
        \\T := [
        \\    A(Bool),
        \\    B(U64),
        \\    C,
        \\    D(I64),
        \\    E(F32),
        \\    F(F64),
        \\    G(Str),
        \\    H(List(U8)),
        \\    I({ a : U8, b : List(U8) }),
        \\    J(List(T)),
        \\    K(List((T, T))),
        \\].{
        \\    is_eq : _
        \\}
        \\
        \\f : List(U8) -> Try(T, [Err(Str), ..])
        \\f = |x| {
        \\    Ok(T.B(List.get(x, 0).ok_or(0).to_i64()))
        \\}
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();

    try env.assertTypeErrorTitles(&.{ "Invalid Nominal Tag", "Redundant Open Tag Union" });
}
