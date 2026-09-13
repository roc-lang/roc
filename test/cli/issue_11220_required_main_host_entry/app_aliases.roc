app [main] { pf: platform "./platform/aliases.roc" }

main : Str -> Str
main = |input| "Got the following from the host: ${input}"
