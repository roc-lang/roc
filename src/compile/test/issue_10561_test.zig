//! Regression test for issue #10561.

const std = @import("std");

const harness = @import("lower_to_lir_harness.zig");

/// Lift and specialize a program whose builder method is chained `links` times,
/// and report the size of the lifted program every later post-check stage walks.
///
/// The nominal is recursive and parameterized, `append` matches its receiver and
/// rebuilds it so the receiver is a known constructor at every link, each link
/// carries its own closure, and the result depends on the hosted argument so the
/// chain cannot be folded away at compile time.
fn liftedExprCountForChain(allocator: std.mem.Allocator, links: usize) harness.LowerToLirHarnessError!usize {
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

    var lifted_exprs: usize = 0;
    try harness.expectLowersToLirWithOptions(body.items, .{
        .inline_mode = .wrappers,
        .lifted_expr_count_out = &lifted_exprs,
    });
    return lifted_exprs;
}

test "issue 10561: chaining a builder method grows the post-check program linearly" {
    // Repro for https://github.com/roc-lang/roc/issues/10561.
    // Every added link costs a bounded amount of post-check code, so the code
    // that links 5 and 6 add must stay in the same range as the code that links
    // 3 and 4 add. Exponential growth makes the later pair many times the
    // earlier one.
    const allocator = std.testing.allocator;

    const at_two = try liftedExprCountForChain(allocator, 2);
    const at_four = try liftedExprCountForChain(allocator, 4);
    const at_six = try liftedExprCountForChain(allocator, 6);

    try std.testing.expect(at_four > at_two);
    const first_stretch = at_four - at_two;
    const second_stretch = at_six - at_four;
    if (second_stretch > first_stretch * 2) {
        std.debug.print(
            "post-check program held {d}, {d} and {d} expressions at 2, 4 and 6 links\n",
            .{ at_two, at_four, at_six },
        );
        return error.PostCheckProgramGrewSuperlinearly;
    }
}
