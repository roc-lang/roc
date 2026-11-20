//! Regression test for https://github.com/roc-lang/roc/issues/13026.
//!
//! In optimized builds (`--opt size` and `--opt speed` both use SpecConstr's
//! `.all_calls` clone inlining), a known tag argument that only a helper's
//! final `match` reads must not copy the helper's tag-independent loop into
//! every call site. The same program spelled three ways, with the loop
//! inline in the helper, with the loop factored into a shared function, and
//! with a redundant `return` before the helper's final `match`, lowers to a
//! post-check program of about the same size, and the redundant `return`
//! changes nothing.
const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const loop_prefix =
    \\    var $acc = n % 1000003
    \\    var $i = 0
    \\    while $i < n % 97 {
    \\        $acc = ($acc * 17 + $i) % 1000003
    \\        $acc = ($acc * 19 + $i) % 1000003
    \\        $acc = ($acc * 21 + $i) % 1000003
    \\        $acc = ($acc * 23 + $i) % 1000003
    \\        $acc = ($acc * 25 + $i) % 1000003
    \\        $acc = ($acc * 27 + $i) % 1000003
    \\        $acc = ($acc * 29 + $i) % 1000003
    \\        $acc = ($acc * 31 + $i) % 1000003
    \\        $i = $i + 1
    \\    }
    \\
;

const callers =
    \\main! = |args| {
    \\    n = args.len()
    \\    v0 = helper(A, n + 0)
    \\    v1 = helper(B, n + 1)
    \\    v2 = helper(C, n + 2)
    \\    v3 = helper(A, n + 3)
    \\    v4 = helper(B, n + 4)
    \\    v5 = helper(C, n + 5)
    \\    v6 = helper(A, n + 6)
    \\    v7 = helper(B, n + 7)
    \\    v8 = helper(C, n + 8)
    \\    v9 = helper(A, n + 9)
    \\    v10 = helper(B, n + 10)
    \\    v11 = helper(C, n + 11)
    \\    echo!((v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + v11).to_str())
    \\    Ok({})
    \\}
    \\
;

const combined_helper =
    \\helper : [A, B, C], U64 -> U64
    \\helper = |mode, n| {
    \\
++ loop_prefix ++
    \\    match mode {
    \\        A => $acc + 1
    \\        B => $acc + 2
    \\        C => $acc + 3
    \\    }
    \\}
    \\
;

const combined_app = combined_helper ++ callers;

const redundant_return_app =
    \\helper : [A, B, C], U64 -> U64
    \\helper = |mode, n| {
    \\
++ loop_prefix ++
    \\    return match mode {
    \\        A => $acc + 1
    \\        B => $acc + 2
    \\        C => $acc + 3
    \\    }
    \\}
    \\
++ callers;

const factored_app =
    \\common : U64 -> U64
    \\common = |n| {
    \\
++ loop_prefix ++
    \\    $acc
    \\}
    \\
    \\helper : [A, B, C], U64 -> U64
    \\helper = |mode, n| {
    \\    acc = common(n)
    \\    match mode {
    \\        A => acc + 1
    \\        B => acc + 2
    \\        C => acc + 3
    \\    }
    \\}
    \\
++ callers;

/// Lower the app with the post-check settings `roc build` uses at
/// `--opt size` and `--opt speed`, and report the post-check program size.
fn liftedExprCount(app_body: []const u8) harness.LowerToLirHarnessError!usize {
    var lifted_exprs: usize = 0;
    try harness.expectLowersToLirWithOptions(app_body, .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
        .lifted_expr_count_out = &lifted_exprs,
    });
    return lifted_exprs;
}

test "issue 13026: a known tag read only after a loop does not copy the loop to every call site" {
    const combined_count = try liftedExprCount(combined_app);
    const redundant_return_count = try liftedExprCount(redundant_return_app);
    const factored_count = try liftedExprCount(factored_app);

    // Only the final `match` gains from the known tag; the loop before it is
    // independent of the tag, so it stays shared however the helper is
    // written, and a redundant `return` is the same program as its absence.
    if (combined_count != redundant_return_count or combined_count > factored_count * 2) {
        std.debug.print(
            "post-check program held {d} expressions with the loop inline, {d} with a redundant return, and {d} with the loop factored out\n",
            .{ combined_count, redundant_return_count, factored_count },
        );
        return error.PostCheckProgramCopiedTagIndependentLoop;
    }
}

/// The callers with a helper that has no statements before its `match`:
/// whatever the rest of the program gives SpecConstr to split.
const baseline_app =
    \\helper : [A, B, C], U64 -> U64
    \\helper = |mode, n| match mode {
    \\    A => n + 1
    \\    B => n + 2
    \\    C => n + 3
    \\}
    \\
++ callers;

/// Lower the app as `roc build` does at `--opt size` and `--opt speed` and
/// report how many prefix functions SpecConstr created.
fn prefixFunctionCount(app_body: []const u8) harness.LowerToLirHarnessError!u64 {
    var metrics: lir.CheckedPipeline.SpecConstrParallelMetrics = .{};
    try harness.expectLowersToLirWithOptions(app_body, .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
        .spec_constr_parallel_metrics_out = &metrics,
    });
    return metrics.prefix_functions;
}

test "issue 13026: a loop before the only read of a known tag moves to one shared function" {
    const baseline = try prefixFunctionCount(baseline_app);
    try std.testing.expect(try prefixFunctionCount(combined_app) > baseline);
    try std.testing.expect(try prefixFunctionCount(redundant_return_app) > baseline);
}

test "issue 13026: statements that bind a tag union stay in every copy" {
    const app =
        \\helper : [A, B, C], U64 -> U64
        \\helper = |mode, n| {
        \\    var $state = Small
        \\    var $i = 0
        \\    while $i < n % 97 {
        \\        $state = if ($i * 17) % 5 == 0 Big else Small
        \\        $i = $i + 1
        \\    }
        \\    match mode {
        \\        A => match $state { Big => 1, Small => 2 }
        \\        B => match $state { Big => 3, Small => 4 }
        \\        C => match $state { Big => 5, Small => 6 }
        \\    }
        \\}
        \\
    ++ callers;
    try std.testing.expectEqual(try prefixFunctionCount(baseline_app), try prefixFunctionCount(app));
}

const result_loop =
    \\helper : U64 -> [Big, Small]
    \\helper = |n| {
    \\
++ loop_prefix ++
    \\    if $acc > 500000 Big else Small
    \\}
    \\
;

test "issue 13026: a body copied once into its only caller is not split" {
    const app = result_loop ++
        \\main! = |args| {
        \\    n = args.len()
        \\    r = match helper(n) { Big => 1, Small => 2 }
        \\    echo!(r.to_str())
        \\    Ok({})
        \\}
        \\
    ;
    const baseline =
        \\main! = |args| {
        \\    n = args.len()
        \\    r = if n > 3 1 else 2
        \\    echo!(r.to_str())
        \\    Ok({})
        \\}
        \\
    ;
    try std.testing.expectEqual(try prefixFunctionCount(baseline), try prefixFunctionCount(app));
}

test "issue 13026: a body copied into two callers shares its loop" {
    const app = result_loop ++
        \\main! = |args| {
        \\    n = args.len()
        \\    r = match helper(n) { Big => 1, Small => 2 }
        \\    s = match helper(n + 1) { Big => 3, Small => 4 }
        \\    echo!((r + s).to_str())
        \\    Ok({})
        \\}
        \\
    ;
    const baseline =
        \\main! = |args| {
        \\    n = args.len()
        \\    r = if n > 3 1 else 2
        \\    s = if n > 4 3 else 4
        \\    echo!((r + s).to_str())
        \\    Ok({})
        \\}
        \\
    ;
    try std.testing.expect(try prefixFunctionCount(app) > try prefixFunctionCount(baseline));
}

// An iterator loop is an ordinary loop: its tag-independent prefix is
// shared like any other.
test "issue 13026: a body that drives an iterator shares its loop like any other" {
    const app =
        \\helper : [A, B, C], List(U64) -> U64
        \\helper = |mode, xs| {
        \\    var $acc = 0
        \\    for x in xs.iter() {
        \\        $acc = ($acc * 17 + x) % 1000003
        \\        $acc = ($acc * 19 + x) % 1000003
        \\        $acc = ($acc * 23 + x) % 1000003
        \\    }
        \\    match mode {
        \\        A => $acc + 1
        \\        B => $acc + 2
        \\        C => $acc + 3
        \\    }
        \\}
        \\
        \\main! = |args| {
        \\    xs = [args.len(), 2, 3]
        \\    v0 = helper(A, xs)
        \\    v1 = helper(B, xs)
        \\    v2 = helper(C, xs)
        \\    v3 = helper(A, xs.append(4))
        \\    echo!((v0 + v1 + v2 + v3).to_str())
        \\    Ok({})
        \\}
        \\
    ;
    const baseline =
        \\helper : [A, B, C], List(U64) -> U64
        \\helper = |mode, xs| match mode {
        \\    A => xs.len() + 1
        \\    B => xs.len() + 2
        \\    C => xs.len() + 3
        \\}
        \\
        \\main! = |args| {
        \\    xs = [args.len(), 2, 3]
        \\    v0 = helper(A, xs)
        \\    v1 = helper(B, xs)
        \\    v2 = helper(C, xs)
        \\    v3 = helper(A, xs.append(4))
        \\    echo!((v0 + v1 + v2 + v3).to_str())
        \\    Ok({})
        \\}
        \\
    ;
    try std.testing.expect(try prefixFunctionCount(app) > try prefixFunctionCount(baseline));
}

test "issue 13026: splitting is deterministic across schedules" {
    try harness.expectSpecConstrParallelismDeterministicLir(combined_app, .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
    }, &.{});
}

test "issue 13026: a split first made inside a rejected loop attempt outlives the attempt" {
    // The loop's first attempt assumes `$mode` stays `A`, so both calls copy
    // the helper's body and split it; the reassignment then rejects the
    // attempt, and the retry reads the split body.
    const app = combined_helper ++
        \\main! = |args| {
        \\    n = args.len()
        \\    var $mode = A
        \\    var $total = 0
        \\    var $k = 0
        \\    while $k < n {
        \\        $total = $total + helper($mode, n + $k) + helper($mode, n + $k + 1)
        \\        $mode = if $total % 2 == 0 B else C
        \\        $k = $k + 1
        \\    }
        \\    echo!($total.to_str())
        \\    Ok({})
        \\}
        \\
    ;
    try std.testing.expect(try prefixFunctionCount(app) > try prefixFunctionCount(baseline_app));
}
