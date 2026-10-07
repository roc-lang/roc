//! Regression tests for https://github.com/roc-lang/roc/issues/11625.
//!
//! A method whose annotation has a `_` hole (`Bound(_)`) dispatches to another
//! type's method, which calls a top-level function declared below both types.
//! A use of the hole-annotated method from a def declared above that function
//! must instantiate the method's annotation, so the method's `a` is fresh at
//! each use instead of being the method's own rigid variable.

const TestEnv = @import("./TestEnv.zig");

test "issue 11625 - hole-annotated method dispatching to a method that calls a later function is instantiated at use" {
    const src =
        \\Pool(tcp) :: { tcp : tcp }.{
        \\    run : Pool(_), (Str -> a) -> a
        \\    run = |_pool, body| {
        \\        _ = release("conn")
        \\        body("conn")
        \\    }
        \\}
        \\
        \\Bound(tcp) :: { pool : Pool(tcp) }.{
        \\    run : Bound(_), (Str -> a) -> a
        \\    run = |bound, body| bound.pool.run(body)
        \\}
        \\
        \\query : Bound(U64) -> Str
        \\query = |bound| bound.run(|conn| conn)
        \\
        \\release : Str -> {}
        \\release = |_conn| {}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("query", "Bound(U64) -> Str");
}
