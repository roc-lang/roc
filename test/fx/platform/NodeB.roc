import NodeA exposing [NodeA]

NodeB := [
    ConstB(Str),
    FoldB({ initial : Str, event : NodeA }),
].{
    const : Str -> NodeB
    const = |s| ConstB(s)

    fold : Str, NodeA -> NodeB
    fold = |initial, event| FoldB({ initial, event })
}
