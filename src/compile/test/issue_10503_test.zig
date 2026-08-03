//! Regression test for issue #10503.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 10503: container join received records with different field order" {
    try expectLowersToLir(
        \\Node := [
        \\    Leaf,
        \\    NodeGroup({ inner : Node, val : U64, mode : [Cap] }),
        \\].{
        \\    is_eq : _
        \\}
        \\
        \\State : {
        \\    items : List(Op),
        \\    count : U64,
        \\}
        \\
        \\Op := [Step].{ is_eq : _ }
        \\
        \\build : Node -> State
        \\build = |node| {
        \\    match node {
        \\        Leaf => { items: [], count: 0 }
        \\        NodeGroup(g) => build_group({ items: [], count: 0 }, g)
        \\    }
        \\}
        \\
        \\build_group : State, { inner : Node, val : U64, mode : [Cap] } -> State
        \\build_group = |st, { inner, val: _, mode: Cap }| {
        \\    s1 = build(inner)
        \\    { ..s1, items: List.append(st.items, Step) }
        \\}
        \\
        \\main! = |_| {
        \\    g = { inner: Leaf, val: 0, mode: Cap }
        \\    st = build(NodeGroup(g))
        \\    if st.count == 0 { Ok({}) } else { Err(Exit(1)) }
        \\}
    );
}
