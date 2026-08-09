//! Regression test for issue #10617.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "issue 10617: for loop on invalid type does not panic monotype lowering" {
    try expectLowersToLirWithOptions(
        \\l : U
        \\
        \\a = {
        \\    for 0 in {} 0
        \\}
        \\
        \\main! = |_| {
        \\    a
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true });
}
