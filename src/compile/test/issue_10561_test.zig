//! Regression test for issue #10561.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");

const harness = @import("lower_to_lir_harness.zig");

/// Statement count of the most recently lowered program, published by
/// `recordStmtCount` because `LirInspectFn` carries no caller context.
var lowered_stmt_count: usize = 0;

fn recordStmtCount(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    _ = layouts;
    lowered_stmt_count = store.cfStmtCount();
}

/// A recursive, parameterized nominal whose builder method matches its receiver
/// and rebuilds it, chained `links` times with a distinct closure per link. The
/// result depends on the hosted argument so the chain cannot be constant
/// folded away.
fn lowerBuilderChain(allocator: std.mem.Allocator, links: usize) !usize {
    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(allocator);

    try body.appendSlice(allocator,
        \\V(model) := [
        \\    Node(List(Box(V(model)))),
        \\    Handler(Box((U64, model -> model))),
        \\].{
        \\    leaf : V(model)
        \\    leaf = V.Node([])
        \\
        \\    handler : (U64, model -> model) -> V(model)
        \\    handler = |f| V.Handler(Box.box(f))
        \\
        \\    append : V(model), V(model) -> V(model)
        \\    append = |v, child|
        \\        match v {
        \\            V.Node(kids) => V.Node(List.append(kids, Box.box(child)))
        \\            V.Handler(f) => V.Node([Box.box(V.Handler(f)), Box.box(child)])
        \\        }
        \\
        \\    run_all : V(model), U64, model -> model
        \\    run_all = |v, event, model|
        \\        match v {
        \\            V.Handler(f) => Box.unbox(f)(event, model)
        \\            V.Node(kids) =>
        \\                Iter.fold(List.iter(kids), model, |acc, kid|
        \\                    V.run_all(Box.unbox(kid), event, acc))
        \\        }
        \\}
        \\
        \\main! = |args| {
        \\    tree =
        \\        V.leaf
        \\
    );
    for (0..links) |index| {
        try body.print(allocator, "        .append(V.handler(|e, m| m + e + {d}))\n", .{index + 1});
    }
    try body.appendSlice(allocator,
        \\    start = List.len(args)
        \\    echo!("result: ${V.run_all(tree, 1, start).to_str()}")
        \\    Ok({})
        \\}
    );

    lowered_stmt_count = 0;
    try harness.expectLirInspectionWithOptions(
        body.items,
        .{ .inline_mode = .wrappers },
        recordStmtCount,
    );
    return lowered_stmt_count;
}

test "issue 10561: chaining a builder method grows the lowered program linearly" {
    // Repro for https://github.com/roc-lang/roc/issues/10561.
    // Each added link costs a bounded amount of lowered code, so the code
    // added by links 4..6 must stay in the same range as the code added by
    // links 2..4. Exponential growth makes the later stretch many times the
    // earlier one.
    const allocator = std.testing.allocator;

    const at_two = try lowerBuilderChain(allocator, 2);
    const at_four = try lowerBuilderChain(allocator, 4);
    const at_six = try lowerBuilderChain(allocator, 6);

    try std.testing.expect(at_four > at_two);
    const first_stretch = at_four - at_two;
    const second_stretch = at_six - at_four;
    try std.testing.expect(second_stretch <= first_stretch * 2);
}
