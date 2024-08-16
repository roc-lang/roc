app "stub"
    packages { pf: "main.roc" }
    imports []
    provides [main] to pf

main : Str
main = "STUBBED APP"
