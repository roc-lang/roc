//! Regression tests for issue #11358.
//!
//! A nominal's backing is one shared template whose formals each use
//! substitutes. When a container's backing reaches another use of the same
//! declaration, as `Set(item)`'s backing `Dict(item, {})` does inside
//! `Dict(U8, Set(U8))`, the inner use binds the same formals to different
//! actuals, and Boxy planning must keep each binding in its own scope.

const harness = @import("lower_to_lir_harness.zig");

fn expectLowersInBothStrategies(source: []const u8) harness.LowerToLirHarnessError!void {
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

test "issue 11358: an annotated empty Dict crosses a call boundary" {
    try expectLowersInBothStrategies(
        \\make! : {} => Try(Dict(U8, U8), [Exit(I8)])
        \\make! = |_| {
        \\    d : Dict(U8, U8)
        \\    d = Dict.empty()
        \\    Ok(d)
        \\}
        \\
        \\main! = |_args| {
        \\    _d = make!({})?
        \\    Ok({})
        \\}
    );
}

test "issue 11358: a Set nested in a Dict crosses a call boundary" {
    try expectLowersInBothStrategies(
        \\make! : {} => Try(Dict(U8, Set(U8)), [Exit(I8)])
        \\make! = |_| {
        \\    d : Dict(U8, Set(U8))
        \\    d = Dict.empty()
        \\    Ok(d)
        \\}
        \\
        \\main! = |_args| {
        \\    _d = make!({})?
        \\    Ok({})
        \\}
    );
}

test "issue 11358: a Dict nested in a Dict crosses a call boundary" {
    try expectLowersInBothStrategies(
        \\make! : {} => Try(Dict(U8, Dict(U8, U8)), [Exit(I8)])
        \\make! = |_| {
        \\    d : Dict(U8, Dict(U8, U8))
        \\    d = Dict.empty()
        \\    Ok(d)
        \\}
        \\
        \\main! = |_args| {
        \\    _d = make!({})?
        \\    Ok({})
        \\}
    );
}

test "issue 11358: a nested container inside a List crosses a call boundary" {
    try expectLowersInBothStrategies(
        \\make! : {} => Try(List(Dict(U8, Set(U8))), [Exit(I8)])
        \\make! = |_| {
        \\    d : Dict(U8, Set(U8))
        \\    d = Dict.empty()
        \\    Ok([d])
        \\}
        \\
        \\main! = |_args| {
        \\    _d = make!({})?
        \\    Ok({})
        \\}
    );
}
