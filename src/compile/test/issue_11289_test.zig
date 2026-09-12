//! Regression test for issue #11289.

const harness = @import("lower_to_lir_harness.zig");

test "issue 11289: constructing an imported generic nominal substitutes its backing field representation" {
    try harness.expectAppPathLowersToLirWithOptions(
        "test/postcheck/boxy_construct_imported_generic_nominal/app.roc",
        .{ .specialization_strategy = .boxy },
    );
}

test "issue 11289: nested and generic nominal constructors keep distinct substitutions" {
    try harness.expectAppPathLowersToLirWithOptions(
        "test/postcheck/boxy_construct_imported_generic_nominal/runtime.roc",
        .{ .specialization_strategy = .boxy },
    );
}
