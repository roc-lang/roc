import Inner exposing [Inner]

Outer := [
    Div(List(Outer)),
    Node(Inner),
    Text(Str),
].{
    div : List(Outer) -> Outer
    div = |children| Div(children)

    text : Str -> Outer
    text = |s| Text(s)
}
