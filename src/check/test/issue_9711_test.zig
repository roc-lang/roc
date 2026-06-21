//! Regression test for issue #9711.

const TestEnv = @import("./TestEnv.zig");

test "issue 9711: custom interpolation against an instantiated annotation is a clean type mismatch, not a postcheck panic" {
    // `g` interpolates its parameter `x`, so `"hello ${x}"` desugars to
    // `MyType.from_interpolation` whose item type is `x`'s type. The call
    // `g({}, B)` makes the item type `{}`, but the annotation pins the result to
    // `MyType(Str)` — so the program is genuinely ill-typed (`MyType({})` vs
    // `MyType(Str)`). Checking must report exactly one TYPE MISMATCH; previously the
    // per-instantiation interpolation re-check unified the item type against the
    // original (un-instantiated) part var instead of the instantiated one, dropping
    // the mismatch so it surfaced as a postcheck invariant panic during monotype
    // lowering instead.
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

    try test_env.assertOneTypeError("TYPE MISMATCH");
}
