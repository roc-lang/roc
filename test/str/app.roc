app { pf: "./platform/main.roc" platform [main] }

main : Str -> Str
main = |input|
    "Got the following from the host: ${input}\n"
