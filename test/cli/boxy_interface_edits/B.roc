import A
import Point
import Adder
import Spaced
import Html

B := {}.{
    run : Str -> Str
    run = |s| A.keep(s)

    label : Str
    label = Html.to_str(html(A.name))

    # A custom interpolation converts in place at runtime, so B's object
    # holds its rejection site, which names B.
    html : Str -> Html
    html = |n| "b-${n}"

    runtime : U64 -> U64
    runtime = |n| A.other(n)

    bumped : U64 -> U64
    bumped = |n| A.bump(n)

    stored : U64 -> U64
    stored = |n| Adder.bumper(n)

    held : U64 -> U64
    held = |n| (Adder.table.f)(n)

    spaced : U64 -> U64
    spaced = |n| {
        s = Spaced.new(n)
        s.a * 10 + s.z
    }

    point : U64 -> U64
    point = |n| {
        p : Point
        p = { x: n }
        p.sum()
    }
}
