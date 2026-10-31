import A
import Point

B := {}.{
    run : Str -> Str
    run = |s| A.keep(s)

    label : Str
    label = "b-${A.name}"

    runtime : U64 -> U64
    runtime = |n| A.other(n)

    bumped : U64 -> U64
    bumped = |n| A.bump(n)

    point : U64 -> U64
    point = |n| {
        p : Point
        p = { x: n }
        p.sum()
    }
}
