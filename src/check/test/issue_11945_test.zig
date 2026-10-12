//! Regression tests for issue 11945.

const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/11945
//
// A record builder with duplicate field names reports them as duplicate record
// fields, just like a plain record literal does.

const builder_decl =
    \\Builder(a) := { value : a }.{
    \\    pure : a -> Builder(a)
    \\    pure = |x| { value: x }
    \\
    \\    map2 : Builder(a), Builder(b), (a, b -> c) -> Builder(c)
    \\    map2 = |a, b, combine| { value: combine(a.value, b.value) }
    \\}
    \\
;

test "issue 11945: record builder reports a duplicate field" {
    const source = builder_decl ++
        \\settings = {
        \\    retries: Builder.pure(3),
        \\    timeout: Builder.pure(10),
        \\    retries: Builder.pure(5),
        \\}.Builder
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneCanErrorMsg(
        \\**Duplicate Record Field**
        \\The record field `retries` appears more than once in this record.
        \\```roc
        \\    retries: Builder.pure(5),
        \\```
        \\    ^^^^^^^
        \\
        \\The field `retries` was first defined in Test:9:5:
        \\```roc
        \\    retries: Builder.pure(3),
        \\```
        \\    ^^^^^^^
        \\
        \\Record fields must have unique names. Consider renaming one of these fields or removing the duplicate.
        \\
        \\
    );
}

test "issue 11945: record builder whose only fields are duplicates reports just the duplicate" {
    const source = builder_decl ++
        \\settings = {
        \\    retries: Builder.pure(3),
        \\    retries: Builder.pure(5),
        \\}.Builder
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertCanErrors(&.{"Duplicate Record Field"});
}
