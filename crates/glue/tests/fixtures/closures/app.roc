app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main : I64 -> ({} -> I64)
main = \x ->
    capture1 = 2
    capture2 = 8
    \{} -> capture1 * capture2 * x
