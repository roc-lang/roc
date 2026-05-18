Elem := [
    Div(List(Elem)),
    Text(Str),
].{
    div : List(Elem) -> Elem
    div = |children| Div(children)

    text : Str -> Elem
    text = |content| Text(content)
}
