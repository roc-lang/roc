V(model) := [
    Node(Box(V(model)), Box(V(model))),
    Click(Box((model -> model))),
].{
    append : V(model), V(model) -> V(model)
    append = |a, b| Node(Box.box(a), Box.box(b))

    click : (model -> model) -> V(model)
    click = |f| Click(Box.box(f))
}
