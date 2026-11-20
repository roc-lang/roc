//! Boxy lowering regression for https://github.com/roc-lang/roc/issues/13249.

const harness = @import("lower_to_lir_harness.zig");

test "issue 13249: Boxy lowers an opaque method call from a local closure capturing a platform Model callback" {
    try harness.expectAppPathLowersToLirWithOptions(
        "test/postcheck/issue_13249_platform_callback/app.roc",
        .{ .specialization_strategy = .boxy },
    );
}
