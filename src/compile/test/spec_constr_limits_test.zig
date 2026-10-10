//! SpecConstr's per-function worker budget on real source.

const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

fn lowerWithSpecConstrMetrics(app_body: []const u8) !lir.CheckedPipeline.SpecConstrParallelMetrics {
    var metrics: lir.CheckedPipeline.SpecConstrParallelMetrics = .{};
    try harness.expectLowersToLirWithOptions(app_body, .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
        .spec_constr_parallel_metrics_out = &metrics,
    });
    return metrics;
}

test "SpecConstr terminates on a function that rebuilds its argument one constructor deeper" {
    const metrics = try lowerWithSpecConstrMetrics(
        \\Tree := [Leaf, Node(U64, Tree)]
        \\
        \\deepen : Tree, U64 -> Tree
        \\deepen = |t, n| match t {
        \\    Leaf => if n == 0 Leaf else deepen(Node(n, Leaf), n - 1)
        \\    Node(_, _) => if n == 0 t else deepen(Node(n, t), n - 1)
        \\}
        \\
        \\total : Tree -> U64
        \\total = |t| match t {
        \\    Leaf => 0
        \\    Node(v, rest) => v + total(rest)
        \\}
        \\
        \\main! = |args| {
        \\    echo!(total(deepen(Leaf, List.len(args))).to_str())
        \\    Ok({})
        \\}
    );
    // Each recursive call passes a strictly deeper `Node` than the last, so
    // every round of cloning discovers a new distinct pattern. The per-function
    // worker budget is what ends the round: further patterns stay ordinary
    // direct calls.
    try std.testing.expect(metrics.workers_written > 0);
    try std.testing.expect(metrics.spec_count_denials > 0);
}

test "SpecConstr still specializes consumer recursion that only destructures" {
    const metrics = try lowerWithSpecConstrMetrics(
        \\Lst := [Nil, Cons(U64, Lst)]
        \\
        \\sum_list : Lst, U64 -> U64
        \\sum_list = |l, acc| match l {
        \\    Nil => acc
        \\    Cons(x, rest) => sum_list(rest, acc + x)
        \\}
        \\
        \\main! = |args| {
        \\    n = List.len(args)
        \\    echo!(sum_list(Cons(n, Cons(2, Cons(3, Nil))), 0).to_str())
        \\    Ok({})
        \\}
    );
    try std.testing.expect(metrics.workers_written > 0);
}
