//! Regression test for issue #11290.

const harness = @import("lower_to_lir_harness.zig");

test "issue 11290: a builtin Set plans a Boxy representation" {
    const source =
        \\main! = |_args| {
        \\    letters = Set.single("x")
        \\    if Set.contains(letters, "x") {
        \\        Ok({})
        \\    } else {
        \\        Err(SetLookupFailed)
        \\    }
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

test "issue 11290: a builtin Dict plans a Boxy representation" {
    const source =
        \\main! = |_args| {
        \\    counts = Dict.single("x", 1)
        \\    if Dict.contains(counts, "x") {
        \\        Ok({})
        \\    } else {
        \\        Err(DictLookupFailed)
        \\    }
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}
