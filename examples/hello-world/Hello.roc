app "hello-world"
    packages { base: "platform" }
    imports []
    provides [ main ] to base

foo : [ A, B ] -> Result Str [ B ]
foo = \x ->
    when x is
        A -> Ok ""
        other -> Err other


main = "Hello, World!\n"
