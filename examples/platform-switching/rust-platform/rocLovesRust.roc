app "rocLovesRust"
    packages { pf: "main.roc" }
    imports []
    provides [main] to pf

main = "Roc <3 Rust!\n"
