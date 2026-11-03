Attribute := [Handler(Box(Str -> Str))].{
    on_input : (Str -> Str) -> Attribute
    on_input = |handler| Handler(Box.box(handler))
}
