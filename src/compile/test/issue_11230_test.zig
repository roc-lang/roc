//! Regression test for issue #11230.

const harness = @import("lower_to_lir_harness.zig");
const lir = @import("lir");

test "issue 11230: a refcounted var reassigned to itself in a for loop lowers to LIR" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |_| {
        \\    var $s = ([], 0)
        \\    for _ in [""] {
        \\        $s = $s
        \\    }
        \\    _ = List.len($s.0)
        \\    Ok({})
        \\}
    , .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
        .include_internal_static_data = true,
    });
}

test "issue 11230: nested static constants survive loop projection and destructuring" {
    for ([_]lir.CheckedPipeline.InlineMode{ .none, .wrappers }) |inline_mode| {
        for ([_]bool{ false, true }) |static_data| {
            try harness.expectLowersToLirWithOptions(
                \\constant = { items: ["alpha", "beta"], pair: ([1.I64, 2.I64], 3.I64), tag: Found(["gamma"]) }
                \\main! = |_| {
                \\    var $state = (constant, 0)
                \\    for _ in ["", ""] {
                \\        $state = $state
                \\    }
                \\    expect $state.0.items == ["alpha", "beta"]
                \\    expect $state.0.pair.0 == [1.I64, 2.I64]
                \\    expect $state.0.pair.1 == 3.I64
                \\    Found(items) = $state.0.tag
                \\    expect items == ["gamma"]
                \\    Ok({})
                \\}
            , .{
                .inline_mode = inline_mode,
                .include_internal_static_data = static_data,
            });
        }
    }
}
