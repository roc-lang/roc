app "rocLovesC"
    packages { pf: "c-platform/main.roc" }
    imports []
    provides [main] to pf

main = "Roc <3 C!\n"
