//! Regression test for issue #10057.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 10057: recursive binary search tree equality lowers to LIR" {
    // Repro for https://github.com/roc-lang/roc/issues/10057.
    // A well-typed recursive tree and its structural equality must lower and
    // pass ARC certification without crashing or exhausting compiler memory.
    try expectLowersToLir(
        \\BinarySearchTree := [Alpha, Bravo, Charlie, Delta, Node({ value : U64, left : BinarySearchTree, right : BinarySearchTree })].{
        \\    is_eq : _
        \\
        \\    build_tree : {} -> BinarySearchTree
        \\    build_tree = |_| Node({
        \\        value: 4,
        \\        left: Node({
        \\            value: 2,
        \\            left: Alpha,
        \\            right: Bravo,
        \\        }),
        \\        right: Node({
        \\            value: 6,
        \\            left: Charlie,
        \\            right: Delta,
        \\        }),
        \\    })
        \\}
        \\
        \\main! = |_args| {
        \\    result = BinarySearchTree.build_tree({})
        \\    expected = Node({
        \\        value: 4,
        \\        left: Node({
        \\            value: 2,
        \\            left: Alpha,
        \\            right: Bravo,
        \\        }),
        \\        right: Node({
        \\            value: 6,
        \\            left: Charlie,
        \\            right: Delta,
        \\        }),
        \\    })
        \\
        \\    if result == expected {
        \\        Ok({})
        \\    } else {
        \\        Err(Exit(1))
        \\    }
        \\}
    );
}
