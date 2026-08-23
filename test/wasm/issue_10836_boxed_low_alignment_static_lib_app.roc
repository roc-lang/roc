app [main!] { pf: platform "./static-lib-platform/main.roc" }

Pair : { x : U8, y : U8 }

Choice : [Dec(U8), Inc(U8)]

main! = |seed| {
    boxed : Box(Pair)
    boxed = Box.box({ x: 7, y: 9 + seed.to_u8_wrap() })
    pair = Box.unbox(boxed)

    boxed_choice : Box(Choice)
    boxed_choice = if seed == 0 {
        Box.box(Inc(42))
    } else {
        Box.box(Dec(seed.to_u8_wrap()))
    }
    choice = Box.unbox(boxed_choice)
    choice_str = match choice {
        Dec(value) => "Dec(${value.to_str()})"
        Inc(value) => "Inc(${value.to_str()})"
    }

    "${pair.x.to_str()},${pair.y.to_str()};${choice_str}"
}
