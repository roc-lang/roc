app "expects"
    packages { pf: "zig-platform/main.roc" }
    imports []
    provides [main] to pf

expect
    a = 1
    b = 2

    a == b

polyDbg = \x ->
    dbg x
    x

main =
    x = 42
    expect x != x
    dbg x
    dbg "Fjoer en ferdjer frieten oan dyn geve lea"

    r = {x : polyDbg "abc", y: polyDbg 10u8, z : polyDbg (A (B C))}

    when r is
        _ -> "Program finished!\n"
