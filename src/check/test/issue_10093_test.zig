//! Regression coverage for invalid nominal declaration-template propagation.

const TestEnv = @import("./TestEnv.zig");

test "issue 10093: valid mutual nominal dependencies remain valid" {
    const src =
        \\First := [First(Second)]
        \\Second := [Second(First)]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertNominalDeclValidity("First", true);
    try test_env.assertNominalDeclValidity("Second", true);
}

test "issue 10093: nested malformed nominal backing is invalid" {
    const src =
        \\T(k) := [A(T(t))]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertOneCanError("Undeclared Type Variable");
    try test_env.assertNominalDeclValidity("T", false);
}

test "issue 10093: invalidity propagates through forward declaration dependencies" {
    const src =
        \\Outer := [Outer(Middle)]
        \\Middle : Inner
        \\Inner : [Broken(missing)]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertOneCanError("Undeclared Type Variable");
    try test_env.assertNominalDeclValidity("Outer", false);
}

test "issue 10093: invalidity propagates through mutual nominal dependencies" {
    const src =
        \\First := [First(Second)]
        \\Second := [Second(First), Broken(missing)]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertOneCanError("Undeclared Type Variable");
    try test_env.assertNominalDeclValidity("First", false);
    try test_env.assertNominalDeclValidity("Second", false);
}

test "issue 10093: invalid recursion propagates to dependent nominals" {
    const src =
        \\Bad := Bad
        \\Outer := [Outer(Bad)]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("Invalid Recursive Type");
    try test_env.assertNominalDeclValidity("Bad", false);
    try test_env.assertNominalDeclValidity("Outer", false);
}
