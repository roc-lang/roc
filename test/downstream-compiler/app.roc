app [main] { pf: platform "./platform/main.roc" }

main : () -> U64
main = || 40 + 2
