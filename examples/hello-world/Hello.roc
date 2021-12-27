app "hello-world"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

foo : [ A, B ] -> Result Str [ B ]
foo = \x ->
    when x is
        A -> Ok ""
        other -> Err other


main = "Hello, World!\n"
