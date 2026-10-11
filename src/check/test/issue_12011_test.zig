//! Regression tests for issue 12011.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/12011
//
// A record pattern without `..` is closed, so a nested record pattern that
// leaves out one of its field's fields is a type mismatch. The nested binder's
// relation to the field is judged after the pattern is checked; a rejected
// judgment reports there and retires the construct that owns the pattern
// without poisoning the field type the scrutinee shares.

test "issue 12011: nested closed record pattern in a nominal match reports a mismatch" {
    const source =
        \\Event := { payload : { id : U64, note : Str } }
        \\
        \\summarize : Event -> Str
        \\summarize = |event| match event {
        \\    Event.{ payload: { id } } => id.to_str()
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
    try test_env.assertDefTypeOptions("summarize", "Event -> Str", .{ .allow_type_errors = true });

    const store = &test_env.module_env.store;
    const lambda = store.getExpr(try test_env.defExpr("summarize"));
    try std.testing.expect(lambda == .e_lambda);
    try std.testing.expect(store.getExpr(lambda.e_lambda.body) == .e_runtime_error);
}

test "issue 12011: nested closed record pattern reports the missing field hint" {
    const source =
        \\summarize : { payload : { id : U64, note : Str } } -> Str
        \\summarize = |event| match event {
        \\    { payload: { id } } => id.to_str()
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This expression is used in an unexpected way.
        \\```roc
        \\    { payload: { id } } => id.to_str()
        \\```
        \\      ^^^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    { id: U64, note: Str }
        \\
        \\But you are trying to use it as:
        \\
        \\    { id: U64 }
        \\**Hint:** This pattern doesn't bind the `note` field. Match it explicitly with `note: _`, or add `..` to match all the remaining fields.
        \\
        \\
    );
}

test "issue 12011: nested closed record pattern in a destructuring statement" {
    const source =
        \\Event := { payload : { id : U64, note : Str } }
        \\
        \\summarize : Event -> Str
        \\summarize = |event| {
        \\    Event.{ payload: { id } } = event
        \\    id.to_str()
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

test "issue 12011: nested closed record pattern in a lambda parameter" {
    const source =
        \\first : { payload : { id : U64, note : Str } } -> Str
        \\first = |{ payload: { id } }| id.to_str()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

test "issue 12011: nested record pattern with a rest checks" {
    const source =
        \\Event := { payload : { id : U64, note : Str } }
        \\
        \\summarize : Event -> Str
        \\summarize = |event| match event {
        \\    Event.{ payload: { id, .. } } => id.to_str()
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("summarize", "Event -> Str");
}
