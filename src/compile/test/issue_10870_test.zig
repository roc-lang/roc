//! Regression test for issue #10870.

const expectAppPathLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectAppPathLowersToLirWithOptions;

test "issue 10870: mismatched hosted-try argument lowers to a checked crash" {
    // The app binds a hosted `Try` whose Ok payload is a nominal type and then
    // passes that value where a `Str` is required. Checking reports the
    // mismatch, and lowering the rejected program must reach the end instead of
    // reading a stale borrow while it builds the hosted-try adapter.
    try expectAppPathLowersToLirWithOptions(
        "test/postcheck/issue_10870_hosted_try_nominal_arg_mismatch/app.roc",
        .{ .allow_user_errors = true },
    );
}
