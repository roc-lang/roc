//! Regression tests for issue 11465.

const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/11465
//
// A qualified record-builder suffix names its type explicitly, so `.Gui.Builder`
// must find `map2` on the `Builder` that `Gui` exposes. The consuming module
// imports only `Gui`; no unqualified `Builder` is in scope there.

test "issue 11465: qualified record-builder suffix resolves a type alias exposed by a facade module" {
    const builder_source =
        \\Builder(a) := [Builder(a)].{
        \\    map2 : Builder(a), Builder(b), (a, b -> c) -> Builder(c)
        \\    map2 = |Builder(a), Builder(b), combine| Builder(combine(a, b))
        \\
        \\    one : Builder(U64)
        \\    one = Builder(1)
        \\
        \\    two : Builder(U64)
        \\    two = Builder(2)
        \\}
    ;
    var builder_env = try TestEnv.init("Builder", builder_source);
    defer builder_env.deinit();
    try builder_env.assertNoErrors();

    const gui_source =
        \\import Builder
        \\
        \\Gui :: [].{
        \\    Builder(a) : Builder.Builder(a)
        \\
        \\    one : Builder(U64)
        \\    one = Builder.one
        \\
        \\    two : Builder(U64)
        \\    two = Builder.two
        \\}
    ;
    var gui_env = try TestEnv.initWithImport("Gui", gui_source, "Builder", &builder_env);
    defer gui_env.deinit();
    try gui_env.assertNoErrors();

    const app_source =
        \\import Gui
        \\
        \\App :: [].{
        \\    built = { first: Gui.one, second: Gui.two }.Gui.Builder
        \\}
    ;
    var app_env = try TestEnv.initWithImport("App", app_source, "Gui", &gui_env);
    defer app_env.deinit();
    try app_env.assertNoErrors();
}

test "issue 11465: qualified record-builder suffix resolves a nominal type nested in an imported module" {
    const gui_source =
        \\Gui :: [].{
        \\    Builder(a) := [Builder(a)].{
        \\        map2 : Builder(a), Builder(b), (a, b -> c) -> Builder(c)
        \\        map2 = |Builder(a), Builder(b), combine| Builder(combine(a, b))
        \\
        \\        one : Builder(U64)
        \\        one = Builder(1)
        \\
        \\        two : Builder(U64)
        \\        two = Builder(2)
        \\    }
        \\}
    ;
    var gui_env = try TestEnv.init("Gui", gui_source);
    defer gui_env.deinit();
    try gui_env.assertNoErrors();

    const app_source =
        \\import Gui
        \\
        \\App :: [].{
        \\    built = { first: Gui.Builder.one, second: Gui.Builder.two }.Gui.Builder
        \\}
    ;
    var app_env = try TestEnv.initWithImport("App", app_source, "Gui", &gui_env);
    defer app_env.deinit();
    try app_env.assertNoErrors();
}

test "issue 11465: qualified record-builder suffix resolves a nested type in the same module" {
    const source =
        \\Outer := [].{
        \\    Inner(a) := [Inner(a)].{
        \\        map2 : Inner(a), Inner(b), (a, b -> c) -> Inner(c)
        \\        map2 = |Inner(a), Inner(b), combine| Inner(combine(a, b))
        \\
        \\        one : Inner(U64)
        \\        one = Inner(1)
        \\
        \\        two : Inner(U64)
        \\        two = Inner(2)
        \\    }
        \\}
        \\
        \\built = { first: Outer.Inner.one, second: Outer.Inner.two }.Outer.Inner
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "issue 11465: qualified record-builder suffix without map2 names the full type path" {
    const gui_source =
        \\Gui :: [].{
        \\    Plain := [Plain(U64)].{
        \\        one : Plain
        \\        one = Plain(1)
        \\    }
        \\}
    ;
    var gui_env = try TestEnv.init("Gui", gui_source);
    defer gui_env.deinit();
    try gui_env.assertNoErrors();

    const app_source =
        \\import Gui
        \\
        \\App :: [].{
        \\    built = { first: Gui.Plain.one, second: Gui.Plain.one }.Gui.Plain
        \\}
    ;
    var app_env = try TestEnv.initWithImport("App", app_source, "Gui", &gui_env);
    defer app_env.deinit();
    try app_env.assertOneCanErrorMsg(
        \\**Record Builder Not Supported**
        \\The type `Gui.Plain` is used in a record builder expression, but does not implement `map2`.
        \\```roc
        \\    built = { first: Gui.Plain.one, second: Gui.Plain.one }.Gui.Plain
        \\```
        \\            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        \\
        \\Hint: To use `Gui.Plain` as a record builder, add a `map2` method to its type module.
        \\
        \\
    );
}

test "issue 11465: record-builder suffix naming a type variable alias is reported as not implemented" {
    const source =
        \\pair : f, f -> f where [f.map2 : f, f, (U64, U64 -> { a : U64, b : U64 }) -> f]
        \\pair = |x, y| {
        \\    F : f
        \\    { a: x, b: y }.F
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneCanError("Not Implemented");
}
