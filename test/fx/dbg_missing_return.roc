app [main!] { pf: platform "./platform/main.roc" }

main! = || {
    dbg "this will break, there return value isn't provided I think"
}
