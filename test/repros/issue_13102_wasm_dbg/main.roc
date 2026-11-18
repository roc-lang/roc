app [main!] { pf: platform "./platform/main.roc" }

main! : U64 => U64
main! = |seed| {
    dbg "hi"
    seed + 1
}
