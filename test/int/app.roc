app { pf: "./platform/main.roc" platform [main] }

main : I64, I64 -> I64
main = |a, b| a * b
