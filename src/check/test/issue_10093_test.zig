//! Regression coverage for invalid type-declaration template propagation.

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

test "issue 10093: imported invalid aliases invalidate dependent nominals" {
    const source_a =
        \\A := [A].{
        \\    Bad : [Broken(missing)]
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertOneCanError("Undeclared Type Variable");

    const source_b =
        \\import A
        \\
        \\Outer := [Outer(A.Bad)]
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();

    try test_env_b.assertNoErrors();
    try test_env_b.assertNominalDeclValidity("Outer", false);
}

test "issue 10093: imported valid aliases preserve dependent nominals" {
    const source_a =
        \\A := [A].{
        \\    Good : [Good]
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertNoErrors();

    const source_b =
        \\import A
        \\
        \\Outer := [Outer(A.Good)]
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();

    try test_env_b.assertNoErrors();
    try test_env_b.assertNominalDeclValidity("Outer", true);
}
