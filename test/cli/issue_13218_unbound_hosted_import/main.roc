app [main!] { pf: platform "platform/main.roc" }

main! : List(Str) => Try({}, _)
main! = |_args| Ok({})
