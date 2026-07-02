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
        # Mixed alignment classes (align-8, pointer, align-4, align-1): the
        # committed field order must be identical at 32- and 64-bit pointer
        # widths, with the pointer field sorting between the 8- and 4-byte
        # alignment bands.
        Aligned({
            marker : U64,
            token : Box(U64),
            flag : U32,
            tiny : U8,
        }),
        Empty,
    ]

    roundtrip! : LayoutProbe => LayoutProbe
}
