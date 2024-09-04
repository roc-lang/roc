app [main] { pf: platform "platform.roc" }

main : I64 -> ({} -> I64)
main = \x ->
    capture1 = 2
    capture2 = 8
    \{} -> capture1 * capture2 * x
