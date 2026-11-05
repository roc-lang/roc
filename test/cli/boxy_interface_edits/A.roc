A := {}.{
    value : {} -> U64
    value = |_| 7

    name : Str
    name = "alpha"

    other : U64 -> U64
    other = |x| x + 1

    keep : Str -> Str
    keep = |s| s

    bump = |x| x + 1
}
