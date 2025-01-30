app [main] { pf: platform "../test-platform-simple-zig/main.roc" }

make_a =
    a = 1

    expect a == 2
    expect a == 3

    a

expect
    a = make_a
    b = 2i64

    a == b

poly_dbg = \x ->
    dbg x

    x

main =
    str = "this will for sure be a large string so when we split it it will use seamless slices which affect printing"
    words = Str.split_on(str, " ")
    expect words == []

    x = 42
    dbg x

    dbg "Fjoer en ferdjer frieten oan dyn geve lea"

    dbg "this is line 24"

    r = { x: poly_dbg("abc"), y: poly_dbg(10u8), z: poly_dbg(A(B(C))) }

    when r is
        _ -> "Program finished!\n"
