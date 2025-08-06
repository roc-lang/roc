app [main] { pf: platform "./platform/main.roc" }

main : Str -> Str
main = |input|
    "Got the following from the host: ${input}\n"
