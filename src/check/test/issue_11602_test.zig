//! Regression tests for https://github.com/roc-lang/roc/issues/11602.
//!
//! A where alias is generated once every type declaration's validity is final,
//! so a where-method signature that names an invalid nominal declaration holds
//! the error type, exactly like a where clause written inline. A signature that
//! references the where alias then never carries an application of a
//! declaration the checked module omits.

const TestEnv = @import("./TestEnv.zig");

test "issue 11602 - where alias naming a nominal with an undeclared type variable" {
    const src =
        \\coll.ToThings(a) : where [coll.to_things : coll -> Thing(a)]
        \\
        \\Thing(a) :: b
        \\
        \\map : input -> Str where [input.ToThings(a)]
        \\map = |_input| ""
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNominalDeclValidity("Thing", false);
    try test_env.assertDefTypeOptions("map", "input -> Str where [input.to_things : input -> Error]", .{ .allow_type_errors = true, .allow_can_errors = true });
}

test "issue 11602 - where alias naming a nominal whose recursion grows its arguments" {
    const src =
        \\Thing(a) :: [A(Thing(List(a)))]
        \\
        \\coll.ToThings(a) : where [coll.to_things : coll -> Thing(a)]
        \\
        \\map : input -> Str where [input.ToThings(a)]
        \\map = |_input| ""
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNominalDeclValidity("Thing", false);
    try test_env.assertDefTypeOptions("map", "input -> Str where [input.to_things : input -> Error]", .{ .allow_type_errors = true });
}
