app "test"
    packages { pf: "../zig-platform/main.roc" }
    imports []
    provides [main] to pf

main =
    Str.withCapacity 42
        |> Str.concat "foobar"
