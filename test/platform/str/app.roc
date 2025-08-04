app [main] { pf: platform "./test/platform/str/main.roc" }

main : Str -> Str
main = |input|
    "Got the following from the host: ${input}\n"
