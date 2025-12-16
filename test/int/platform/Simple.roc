Simple(model) := [Leaf(Str)].{
    leaf : Str -> Simple(model)
    leaf = |s| Leaf(s)
}
