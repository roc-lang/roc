app [main!] { pf: platform "./static-lib-platform/main.roc" }

flip : Bool -> Bool
flip = |value| !value

main! = |seed| {
    # Freeing a box of set bits leaves those bytes behind for the next small
    # allocation, so the one-byte Bool payload below is followed by nonzero
    # bytes that unboxing it must not read.
    dirty : Box(U32)
    dirty = Box.box(0xFFFFFF00 + seed.to_u32_wrap())
    sink = Box.unbox(dirty)

    boxed : Box(Bool)
    boxed = Box.box(seed == 1)
    flipped = flip(Box.unbox(boxed))

    if flipped and sink != 7 "ok" else "bad"
}
