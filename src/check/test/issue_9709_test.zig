//! Regression test for issue #9709.

const TestEnv = @import("./TestEnv.zig");

test "issue 9709: typed numeric suffix uses resolved external target for from_numeral" {
    const src =
        \\main! = |_args| {
        \\    _ = 12.Str
        \\    Ok({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Missing Method");
}
