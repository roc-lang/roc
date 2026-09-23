//! Regression tests for issue 11465.

const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/11465
//
// A record-builder suffix names a type, and `map2` must come from that type.
// Cross-module suffixes are covered by src/compile/test/issue_11465_test.zig,
// which checks through the real build.

test "issue 11465: qualified record-builder suffix resolves a nested type in the same module" {
    const source =
        \\Outer := [].{
        \\    Inner(a) := [Inner(a)].{
        \\        map2 : Inner(a), Inner(b), (a, b -> c) -> Inner(c)
        \\        map2 = |Inner(a), Inner(b), combine| Inner(combine(a, b))
        \\
        \\        one : Inner(U64)
        \\        one = Inner(1)
        \\    }
        \\}
        \\
        \\built = { first: Outer.Inner.one, second: Outer.Inner.one, third: Outer.Inner.one }.Outer.Inner
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "issue 11465: record-builder suffix uses the innermost type of that name, even before its map2 is declared" {
    // The top-level `Inner.map2` only accepts top-level `Inner` values, so
    // binding it instead of `Outer.Inner.map2` is a type mismatch.
    const source =
        \\Inner(a) := [Top(a)].{
        \\    map2 : Inner(a), Inner(b), (a, b -> c) -> Inner(c)
        \\    map2 = |Top(a), Top(b), combine| Top(combine(a, b))
        \\}
        \\
        \\Outer := [].{
        \\    Inner(a) := [Nested(a)].{
        \\        both : Inner(U64), Inner(U64), Inner(U64) -> Inner({ first : U64, second : U64, third : U64 })
        \\        both = |x, y, z| { first: x, second: y, third: z }.Inner
        \\
        \\        map2 : Inner(a), Inner(b), (a, b -> c) -> Inner(c)
        \\        map2 = |Nested(a), Nested(b), combine| Nested(combine(a, b))
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
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
    try test_env.assertOneCanErrorMsg(
        \\**Not Implemented**
        \\This feature is not yet implemented: record builder on a type variable alias.
        \\```roc
        \\    { a: x, b: y }.F
        \\```
        \\    ^^^^^^^^^^^^^^^^
        \\
        \\This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!
        \\
        \\
        \\
    );
}
