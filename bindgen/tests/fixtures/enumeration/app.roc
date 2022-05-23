app "app"
    packages { pf: "." }
    imports []
    provides [main] to pf

main = Foo
