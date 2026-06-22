//! Regression test for issue #9710.

const TestEnv = @import("./TestEnv.zig");

test "issue 9710: malformed derived is_eq arity reports an error" {
    const src =
        \\main! = |_args| {
        \\    val = A
        \\    _ = val.is_eq(val, val)
        \\    Ok({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Too Many Args");
}
