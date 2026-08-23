//! Regression test for issue 10695.

const TestEnv = @import("./TestEnv.zig");

test "issue 10695: derived parse of repeated aliased tag union fields checks cleanly" {
    const source =
        \\Profile : { a : Str, b : Str, c : Str, d : Str, images : [Bar({ source : Str, small : Str }), Foo] }
        \\
        \\Model : { group : { primary : [Bar(Profile), Foo], secondary : [Bar(Profile), Foo] } }
        \\
        \\main = |json| {
        \\    parsed : Try({ payload : Model }, _)
        \\    parsed = Json.parse(json)
        \\
        \\    match parsed {
        \\        Ok(_) => {}
        \\        Err(_) => {}
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}
