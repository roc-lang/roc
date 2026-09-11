//! Regression tests for issue #11301: forwarding an Err-only closure through
//! records permutes the enclosing functions' symbolic method requirements.

const harness = @import("lower_to_lir_harness.zig");
const std = @import("std");
const postcheck = @import("postcheck");

const hooks_app_body =
    \\main! : List(Str) => Try({}, _)
    \\main! = |_args|
    \\    outer({
    \\        run: || Err(CommandNotFound),
    \\        get: |_name| "x",
    \\    }).map_err(|_| Exit(1))
    \\
    \\outer = |hooks| {
    \\    _ = hooks.get
    \\    middle(hooks)
    \\}
    \\
    \\middle = |hooks| {
    \\    get = hooks.get
    \\    _ = get("name")
    \\    inner(hooks)
    \\}
    \\
    \\inner = |hooks| {
    \\    run = hooks.run
    \\    output = run()?
    \\    _ = output.trim()
    \\    Ok({})
    \\}
;

test "issue 11301: a method call pinning a forwarded Err-only closure's Ok payload lowers to Monotype" {
    try harness.expectLowersToLirWithOptions(hooks_app_body, .{ .monotype_only = true });
}

test "issue 11301: a method call pinning a forwarded Err-only closure's Ok payload lowers to LIR" {
    try harness.expectLowersToLir(hooks_app_body);
}

test "issue 11301: unresolved callable evidence reuses its immutable vector" {
    var diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectLowersToLirWithOptions(hooks_app_body, .{
        .monotype_only = true,
        .monotype_diagnostics_out = &diagnostics,
    });
    // The Err-only payload stays open across repeated interface requests.
    // Those requests must not each allocate another identical evidence vector.
    try std.testing.expect(diagnostics.body.callable_evidence_symbolic_requests >
        diagnostics.body.callable_evidence_vector_copies);
}
