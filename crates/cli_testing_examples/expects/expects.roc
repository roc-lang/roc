app "expects-test"
    packages { pf: "zig-platform/main.roc" }
    imports []
    provides [main] to pf

expect
    a = 1
    b = 2

    a == b

main =
    x = 42
    expect x != x
    dbg x
    dbg "Fjoer en ferdjer frieten oan dyn geve lea"
    "Program finished!\n"
