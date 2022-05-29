app "app"
    packages { pf: "." }
    imports []
    provides [main] to pf

main = Concat (String "Hello, ") (String "World!")
