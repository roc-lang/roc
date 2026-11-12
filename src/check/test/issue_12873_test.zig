//! Regression coverage for equality and parser requirements sharing a returned tag payload.
//! https://github.com/roc-lang/roc/issues/12873
const TestEnv = @import("TestEnv.zig");

test "issue 12873: a parser-constrained Try can be compared and returned in an error tag" {
    var env = try TestEnv.init("Test",
        \\Endpoint(answer) :: { path : Str }
        \\
        \\decode : Endpoint(answer) -> Try(answer, [Failed(Str)]) where [answer.Json.Parseable([InvalidJson(Str)])]
        \\decode = |e| Err(Failed(e.path))
        \\
        \\endpoint : Str -> Endpoint([A, B])
        \\endpoint = |path| { path: path }
        \\
        \\check = |path| {
        \\    answer = decode(endpoint(path))
        \\    if answer == Ok(A) Ok({}) else Err(NotA(answer))
        \\}
        \\
        \\process_string : Str -> Str
        \\process_string = |s|
        \\    match check(s) {
        \\        Ok({}) => "A"
        \\        Err(_) => "not A"
        \\    }
    );
    defer env.deinit();
    try env.assertNoErrors();
}
