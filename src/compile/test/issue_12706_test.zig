//! Regression test for https://github.com/roc-lang/roc/issues/12706.
//!
//! At `--opt speed`, a chain of matches whose `Err` arm holds the next match,
//! over an inlined callee that builds `Err` on two paths, must not copy the
//! rest of the chain into both paths. The post-check program grows with the
//! length of the chain, not exponentially in it.
const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

fn chain(comptime depth: usize) []const u8 {
    @setEvalBranchQuota(100_000);
    var body: []const u8 = "Missing";
    for (0..depth) |level| {
        body = std.fmt.comptimePrint(
            "match check(n + {d}) {{ Ok(v) => Found(v + {d}), Err(_) => {s} }}",
            .{ level, level, body },
        );
    }
    return body;
}

fn app(comptime depth: usize) []const u8 {
    return
    \\check : U64 -> Try(U64, [Bad])
    \\check = |n| if n > 100 Err(Bad) else if n % 7 == 0 Err(Bad) else Ok(n)
    \\
    \\first_found : U64 -> [Found(U64), Missing]
    \\first_found = |n|
    \\
    ++ "    " ++ chain(depth) ++ "\n" ++
        \\
        \\main! = |args| {
        \\    match first_found(args.len()) {
        \\        Found(v) => echo!(v.to_str())
        \\        Missing => echo!("missing")
        \\    }
        \\    Ok({})
        \\}
    ;
}

const short_app = app(6);
const long_app = app(12);

/// Lower the app with the post-check settings `roc build` uses at its
/// default `--opt speed` and report the post-check program size.
fn liftedExprCount(app_body: []const u8) harness.LowerToLirHarnessError!usize {
    var lifted_exprs: usize = 0;
    try harness.expectLowersToLirWithOptions(app_body, .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
        .lifted_expr_count_out = &lifted_exprs,
    });
    return lifted_exprs;
}

test "issue 12706: a match chain over a callee with two Err paths stores each arm once" {
    const short_count = try liftedExprCount(short_app);
    const long_count = try liftedExprCount(long_app);
    if (long_count > short_count * 3) {
        std.debug.print(
            "post-check program held {d} expressions for a chain of 6 and {d} for a chain of 12\n",
            .{ short_count, long_count },
        );
        return error.PostCheckProgramGrewExponentiallyInChainLength;
    }
}
