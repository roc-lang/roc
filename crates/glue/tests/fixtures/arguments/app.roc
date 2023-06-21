app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main : I64 -> I64
main = \x -> 2 * x
