app "app"
    packages { pf: "." }
    imports []
    provides [main] to pf

main = { a: 1995, b: 42 }
