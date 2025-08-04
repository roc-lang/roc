app [main] { pf: platform "./main.roc" }

main : I64, I64 -> I64
main = |a, b| a * b
