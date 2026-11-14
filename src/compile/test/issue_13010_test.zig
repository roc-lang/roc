//! Regression test for https://github.com/roc-lang/roc/issues/13010.
//!
//! A compile-time constant whose closures capture the same value must be
//! restored with that value built once. The constant here is a chain in which
//! each node's two closures both capture the previous node, so the value grows
//! linearly with the chain's depth, and so must the post-check program that
//! restores it.
const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

fn app(comptime depth: usize) []const u8 {
    return
    \\Node := [Node({ left : (U64 -> U64), right : (U64 -> U64) })]
    \\
    \\leaf : U64 -> Node
    \\leaf = |v| Node.Node({ left: |_| v, right: |_| v })
    \\
    \\pair : Node, Node -> Node
    \\pair = |l, r| Node.Node({
    \\    left: |i| if i % 2 == 0 go_left(l, i // 2) else go_left(r, i // 2),
    \\    right: |i| if i % 2 == 0 go_right(l, i // 2) else go_right(r, i // 2),
    \\})
    \\
    \\go_left : Node, U64 -> U64
    \\go_left = |Node(n), i| (n.left)(i)
    \\
    \\go_right : Node, U64 -> U64
    \\go_right = |Node(n), i| (n.right)(i)
    \\
    \\chain : U64 -> Node
    \\chain = |depth| if depth == 0 leaf(1) else pair(chain(depth - 1), leaf(depth))
    \\
    \\mounted : Node
    \\
    ++ std.fmt.comptimePrint("mounted = chain({d})\n", .{depth}) ++
        \\
        \\main! = |args| {
        \\    echo!(go_left(mounted, args.len()).to_str())
        \\    Ok({})
        \\}
    ;
}

/// Lower the app the way an optimized build does, restoring the constant as
/// internal static data, and report the size of the lifted program every
/// later post-check stage walks.
fn liftedExprCount(app_body: []const u8) harness.LowerToLirHarnessError!usize {
    var lifted_exprs: usize = 0;
    try harness.expectLowersToLirWithOptions(app_body, .{
        .include_internal_static_data = true,
        .lifted_expr_count_out = &lifted_exprs,
    });
    return lifted_exprs;
}

test "issue 13010: a constant whose closures share a captured value restores it once" {
    const at_two = try liftedExprCount(comptime app(2));
    const at_four = try liftedExprCount(comptime app(4));
    const at_six = try liftedExprCount(comptime app(6));

    try std.testing.expect(at_four > at_two);
    const first_stretch = at_four - at_two;
    const second_stretch = at_six - at_four;
    if (second_stretch > first_stretch * 2) {
        std.debug.print(
            "post-check program held {d}, {d} and {d} expressions at depths 2, 4 and 6\n",
            .{ at_two, at_four, at_six },
        );
        return error.PostCheckProgramGrewSuperlinearly;
    }
}
