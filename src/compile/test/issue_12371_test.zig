//! Regression test for https://github.com/roc-lang/roc/issues/12371.
//!
//! A recursive function whose branch filters a list with `keep_if` and then
//! maps the same list with the enclosing function checks cleanly and must pass
//! ARC certification when lowered with the optimized-build options.
const harness = @import("lower_to_lir_harness.zig");

test "issue 12371: filtering a list then mapping it with the enclosing function passes ARC certification" {
    try harness.expectLowersToLirWithOptions(
        \\Tree := [Leaf(Str), Node(List(Tree))]
        \\
        \\walk : Tree -> Tree
        \\walk = |tree|
        \\    match tree {
        \\        Leaf(text) => Leaf(text)
        \\        Node(children) =>
        \\            if children.keep_if(|_| Bool.True).is_empty() {
        \\                Node([])
        \\            } else {
        \\                Node(children.map(walk))
        \\            }
        \\    }
        \\
        \\main! = |args| {
        \\    match walk(Node([Leaf(args.len().to_str())])) {
        \\        Node(children) => echo!(children.len().to_str())
        \\        Leaf(text) => echo!(text)
        \\    }
        \\    Ok({})
        \\}
    ,
        .{
            .inline_mode = .wrappers,
            .spec_constr_clone_inlining = .all_calls,
            .consume_dead_boxes = true,
            .list_in_place_map = true,
            .prove_ranges = true,
        },
    );
}
