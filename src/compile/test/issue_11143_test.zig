//! Regression test for issue #11143.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

// repro for https://github.com/roc-lang/roc/issues/11143
//
// A caller matches on the result of a self-recursive walker whose self-call
// keeps passing along a known constructor. The self-call stays a call when
// the walker is inlined into its caller (the recursion guard declines it),
// and distributing the caller's match over the inlined body must leave that
// retained call opaque instead of inlining it again outside the frame whose
// recursion guard owns it. The post-check program therefore stays within a
// small multiple of the program that passes an opaque argument instead.
const known_argument_app =
    \\main! = |args| {
    \\    match walk(args, {}) {
    \\        Found(x) => echo!(x)
    \\        Missing => echo!("missing")
    \\    }
    \\    Ok({})
    \\}
    \\
    \\walk = |items, deps|
    \\    match items {
    \\        [] => Missing
    \\        [x, .. as rest] => if Str.is_empty(x) walk(rest, deps) else Found(x)
    \\    }
    \\
;

const opaque_argument_app =
    \\main! = |args| {
    \\    match walk(args, args) {
    \\        Found(x) => echo!(x)
    \\        Missing => echo!("missing")
    \\    }
    \\    Ok({})
    \\}
    \\
    \\walk = |items, deps|
    \\    match items {
    \\        [] => Missing
    \\        [x, .. as rest] => if Str.is_empty(x) walk(rest, deps) else Found(x)
    \\    }
    \\
;

/// Lower the app with the post-check settings `roc build` uses at its
/// default `--opt speed` and report the post-check program size.
fn liftedExprCount(app_body: []const u8) !usize {
    var lifted_exprs: usize = 0;
    try harness.expectLowersToLirWithOptions(app_body, .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
        .lifted_expr_count_out = &lifted_exprs,
    });
    return lifted_exprs;
}

test "issue 11143: a recursive walker called with a known constructor lowers at speed with bounded post-check growth" {
    const opaque_count = try liftedExprCount(opaque_argument_app);
    const known_count = try liftedExprCount(known_argument_app);
    // Inlining the walker once into its caller and specializing it for the
    // known argument each cost at most one more copy of this tiny program.
    if (known_count > opaque_count * 4) {
        std.debug.print(
            "post-check program held {d} expressions with a known argument and {d} with an opaque one\n",
            .{ known_count, opaque_count },
        );
        return error.PostCheckProgramGrewUnboundedly;
    }
}
