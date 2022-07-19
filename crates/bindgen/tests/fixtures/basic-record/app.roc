app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main = { a: 1995, b: 42 }
