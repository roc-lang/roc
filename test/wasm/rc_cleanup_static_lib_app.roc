app [main!] { pf: platform "./static-lib-platform/main.roc" }

step : Box(U64) -> Box(U64)
step = |boxed| {
    model = Box.unbox(boxed)
    temp = List.concat([1.U8, 2.U8, 3.U8], [4.U8, 5.U8, 6.U8])

    Box.box(model + List.len(temp))
}

main! = |seed| {
    b0 = Box.box(seed)
    b1 = step(b0)
    b2 = step(b1)
    b3 = step(b2)
    b4 = step(b3)
    b5 = step(b4)
    b6 = step(b5)
    b7 = step(b6)
    b8 = step(b7)
    b9 = step(b8)
    b10 = step(b9)
    b11 = step(b10)
    b12 = step(b11)
    b13 = step(b12)
    b14 = step(b13)
    b15 = step(b14)
    b16 = step(b15)

    if Box.unbox(b16) == seed + 96 {
        "ok"
    } else {
        "bad"
    }
}
