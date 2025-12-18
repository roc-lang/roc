app [main!] { pf: platform "./platform/main.roc" }

main! = || {
    count : Iter({})
    count = It({})
    _ignore = count.identity()
}

Iter(s) := [It(s)].{
    identity = |It(s_)| It(s_)
}
