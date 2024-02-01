app "expects-test"
    packages { pf: "zig-platform/main.roc" }
    provides [main] to pf

expect
    a = 1
    b = 2

    a == b

polyDbg = \x ->
    dbg x

    x

main =
    str = "this will for sure be a large string so when we split it it will use seamless slices which affect printing"
    words = Str.split str " "
    expect words == []

    x = 42
    dbg x

    dbg "Fjoer en ferdjer frieten oan dyn geve lea"

    dbg "this is line 24"

    r = { x: polyDbg "abc", y: polyDbg 10u8, z: polyDbg (A (B C)) }

    when r is
        _ -> "Program finished!\n"
