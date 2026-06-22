Probe := [].{
    LayoutProbe := [
        Wide({
            label : Str,
            a : Box(U64),
            b : Box(U64),
            c : Box(U64),
            d : Box(U64),
            e : Box(U64),
            f : Box(U64),
            g : Box(U64),
            h : Box(U64),
        }),
        Aligned({
            marker : U64,
            token : Box(U64),
        }),
        Empty,
    ]

    roundtrip! : LayoutProbe => LayoutProbe
}
