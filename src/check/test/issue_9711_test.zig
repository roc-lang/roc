//! Regression test for issue #9711.

const TestEnv = @import("./TestEnv.zig");

test "issue 9711: custom interpolation item type survives instantiated constraint finalization" {
    const src =
        \\MyType(val) := [A(val), B].{
        \\    from_interpolation : Str, Iter((val, Str)) -> MyType(val)
        \\    from_interpolation = |_, _| B
        \\}
        \\
        \\g = |x, y| {
        \\    res = "hello ${x}"
        \\    (res, y)
        \\}
        \\
        \\main! = |_args| {
        \\    val : (MyType(Str), MyType(Str))
        \\    val = g({}, B)
        \\    _ = val
        \\    Ok({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}
