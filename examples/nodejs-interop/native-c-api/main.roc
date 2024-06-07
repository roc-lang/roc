app [main] { pf: platform "platform/main.roc" }

main : Str -> Str
main = \message ->
    "TypeScript said to Roc: $(message)! 🎉"
