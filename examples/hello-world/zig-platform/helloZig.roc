app "helloZig"
    packages { pf: "." }
    imports []
    provides [main] to pf


main =
    a = Str.concat "a" "foo"
    b = Str.concat "a" "bar"

    (
        expect a == b
        "Hello, World!\n"
    )
