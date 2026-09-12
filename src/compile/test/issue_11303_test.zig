//! Regression test for issue #11303.

const harness = @import("lower_to_lir_harness.zig");

// repro for https://github.com/roc-lang/roc/issues/11303
//
// `hooks` returns a record whose `render` field is an unannotated lambda, and
// that lambda's parameter is constrained only by the `to_str` method call.
// Nothing in the program ever instantiates the parameter at a concrete type,
// so lowering the call to `hooks` must resolve the field lambda's
// callable-derived evidence from the function request that produced it.
test "issue 11303: a returned record field lambda constrained only by a method call lowers to Monotype" {
    try harness.expectLowersToLirWithOptions(
        \\hooks = |{}| { render: |value| value.to_str() }
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    _ = hooks({})
        \\    Ok({})
        \\}
    , .{ .monotype_only = true });
}

test "issue 11303: an unused returned method-constrained lambda lowers through LIR" {
    try harness.expectLowersToLir(
        \\hooks = |{}| { render: |value| value.to_str() }
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    _ = hooks({})
        \\    Ok({})
        \\}
    );
}

test "issue 11303: nested returned fields retain independent method evidence" {
    try harness.expectLowersToLir(
        \\hooks = |{}| { nested: { render: |value| value.to_str(), size: |value| value.len() } }
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    _ = hooks({})
        \\    Ok({})
        \\}
    );
}

test "issue 11303: returned callable fields retain distinct concrete requests" {
    try harness.expectLowersToLir(
        \\hooks = |{}| { render: |value| value.to_str() }
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    number_render = hooks({}).render
        \\    text_render = hooks({}).render
        \\    _ = number_render(1)
        \\    _ = text_render("hello")
        \\    Ok({})
        \\}
    );
}
